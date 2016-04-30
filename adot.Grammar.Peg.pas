unit adot.Grammar.Peg;

{ Parser for grammar based on TGrammarClass:
  - recursive (similar to Recursive descent parser)
  - stops at first successfull choice (similar to "parsing expression grammars")
  - uses memoization of intermediate results (similar to "packrat parsing") }

interface

uses
  adot.Grammar.Types,
  adot.Grammar,
  adot.Tools,
  adot.Collections,
  System.SysUtils;

type

  TPegParser = class(TGrammarParser)
  protected
    type
      PCallStackItem = ^TCallStackItem;
      TCallStackItem = record
        Rule: TGrammarClass;
        Step: integer;
        Start: integer;
        Len: integer;
        ResIndex: integer;      { position of matching result in tree-like structure organized as array of TMatchingResult)}
        ResLastChild: integer;

        procedure SetUp(ARule: TGrammarClass); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetUp(ARule: TGrammarClass; AStart: integer); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
      end;

    var
      Tree: TVector<TMatchingResult>;

  public
    function Accepted: Boolean; override;
  end;

implementation

{ TPegParser.TCallStackItem }

procedure TPegParser.TCallStackItem.SetUp(ARule: TGrammarClass);
begin
  Rule := ARule;
  Step := 0;
end;

procedure TPegParser.TCallStackItem.SetUp(ARule: TGrammarClass; AStart: integer);
begin
  Rule  := ARule;
  Step  := 0;
  Start := AStart;
end;

{ TPegParser }

function TPegParser.Accepted: Boolean;
var

  { stack of calls (to avoid deep recursion) }
  Stack: TVector<TCallStackItem>;

  { result of last operation on stack:
    Accept   - input accepted
    Len      - length of accepted block if Accept=True, undefined if Accept=False
    ResIndex - index of parse tree element }
  Len: integer;
  Accept: boolean;
  ResIndex: integer;

  { pointer to current/new item on the stack (invalid after any modification of the stack) }
  Item: PCallStackItem;
begin
  Stack.Clear;
  Tree.Clear;
  Accept := False;
  Len := 0;
  ResIndex := -1;
  Stack.Items[Stack.Add].SetUp(Grammar);
  repeat
    Item := @Stack.Items[Stack.Count-1];
    case Item.Rule.GrammarType of

      gtUnknown:
        raise Exception.Create('rule is not initialized');

      gtLink:
        Item.SetUp(TGrammarLink(Item.Rule).Op.Data);

      gtString:
        begin
          Len    := TGrammarString(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := Tree.Add;
            Tree.Items[ResIndex].SetUp(Item.Rule.Id, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtChar:
        begin
          Len    := TGrammarChar(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := Tree.Add;
            Tree.Items[ResIndex].SetUp(Item.Rule.Id, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtSequence:
        case Item.Step of
          0: begin
               Inc(Item.Step);
               Stack.Items[Stack.Add].SetUp(TGrammarSequence(Item.Rule).Op1.Data);
             end;
          1: if not Accept then
               Stack.DeleteLast
             else begin
               Inc(Item.Step);
               Item.Len := Len;
               Data.Position := Data.Position + Len;
               Item.ResIndex := ResIndex;
               Stack.Items[Stack.Add].SetUp(TGrammarSequence(Item.Rule).Op2.Data);
             end;
          2: begin
               Data.Position := Data.Position - Item.Len;
               if Accept then
               begin
                 Inc(Len, Item.Len);
                 Tree.Items[Tree.Add].SetUp(Item.Rule.Id, Data.Position, Len, Item.ResIndex);
                 Tree.Items[Item.ResIndex].NextSibling := ResIndex;
                 ResIndex := Tree.Count-1;
               end;
               Stack.DeleteLast;
             end;
        end;

      gtSelection:
        case Item.Step of
          0: begin
               Inc(Item.Step);
               Stack.Items[Stack.Add].SetUp(TGrammarSelection(Item.Rule).Op1.Data);
             end;
          1: if Accept then begin
               Tree.Items[Tree.Add].SetUp(Item.Rule.Id, Data.Position, Len, ResIndex);
               ResIndex := Tree.Count-1;
               Stack.DeleteLast;
             end else begin
               Inc(Item.Step);
               Stack.Items[Stack.Add].SetUp(TGrammarSelection(Item.Rule).Op2.Data);
             end;
          2: begin
               if Accept then
               begin
                 Tree.Items[Tree.Add].SetUp(Item.Rule.Id, Data.Position, Len, ResIndex);
                 ResIndex := Tree.Count-1;
               end;
               Stack.DeleteLast;
             end;
        end;

      gtRepeat:
        if Item.Step=0 then
          if TGrammarGreedyRepeater(Item.Rule).MaxCount <= 0 then begin
            Accept := False;
            Stack.DeleteLast;
          end else begin
            Inc(Item.Step);
            Item.Start := Data.Position;
            Item.ResIndex := Tree.Add;
            Item.Len := 0;
            Stack.Items[Stack.Add].SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
          end
        else
        if Accept and (Item.Step < TGrammarGreedyRepeater(Item.Rule).MaxCount) then begin
          Inc(Item.Step);
          Inc(Item.Len, Len);
          if Item.Step=1 then begin
            Item.ResLastChild := ResIndex;
            Tree.Items[Item.ResIndex].FirstChild := ResIndex;
          end else begin
            Tree.Items[Item.ResLastChild].NextSibling := ResIndex;
            Item.ResLastChild := ResIndex;
          end;
          Data.Position := Data.Position + Len;
          Stack.Items[Stack.Add].SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
        end
        else begin
          if Accept then
            Inc(Item.Len, Len);
          with TGrammarGreedyRepeater(Item.Rule) do
            Accept := (Item.Step >= MinCount) and (Item.Step <= MaxCount);
          if not Accept then
            Tree.Count := Item.ResIndex
          else
          begin
            ResIndex := Item.ResIndex;
            Len := Item.Len;
          end;
          Data.Position := Item.Start;
          Stack.DeleteLast;
        end;

      gtNot:
        if Item.Step=0 then
        begin
          Inc(Item.Step);
          Stack.Items[Stack.Add].SetUp(TGrammarNot(Item.Rule).Op.Data);
        end
        else
        begin
          Accept := not Accept;
          if Accept then
          begin
            Len := 0;
            ResIndex := Tree.Add;
            Tree[ResIndex].SetUp(Item.Rule.Id, Data.Position,0);
          end;
          Stack.DeleteLast;
        end;

      gtEOF:
        begin
          Accept := Data.EOF;
          if Accept then
          begin
            Len := 0;
            ResIndex := Tree.Add;
            Tree[ResIndex].SetUp(Item.Rule.Id, Data.Position,0);
          end;
          Stack.DeleteLast;
        end;

    end; { case Item.Rule.GrammarType of }
  until Stack.Count=0;
  result := Accept;
  if not result then
    Tree.Clear;
end;

end.
