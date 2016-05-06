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
  adot.Strings,
  adot.Log,
  System.SysUtils,
  System.Classes;

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

    procedure LogTextInputParseTree(const ParseTree: TVector<TMatchingResult>; ResIndex, Margin: integer);

  public
    function Accepted: Boolean; override;
    procedure LogResult; override;
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

procedure TPegParser.LogResult;
var
  ParseTree: TVector<TMatchingResult>;
begin
  AppLog.Log('');
  AppLog.Log('Input string:');
  AppLog.Log(TStr.GetPrintable(Data.Text));
  AppLog.Log('Parse tree:');
  //LogTextInputParseTree(Root, 0);
  GetParseTree(ParseTree);
  LogTextInputParseTree(ParseTree, 0,0);
end;

procedure TPegParser.LogTextInputParseTree(const ParseTree: TVector<TMatchingResult>; ResIndex, Margin: integer);

  procedure L(const S: string; const Args: array of const; Margin: integer);
  begin
    AppLog.Log(StringOfChar(' ', Margin) + Format(S, Args));
  end;

var
  R: TMatchingResult;
  S,T: string;
begin
  while ResIndex>=0 do
  begin
    R := ParseTree.Items[ResIndex];

    Assert(R.Position.Len mod SizeOf(Char)=0);
    L('%s Pos: %d Len: %d)', [R.Rule.Info, R.Position.Start, R.Position.Len], Margin);
    Data.Position := R.Position.Start;
    Data.Read(S, R.Position.Len div SizeOf(Char));
    T := Data.Text;
    if T=S then
      L('[EXP] %s', [TStr.GetPrintable(S)], Margin)
      //L('%s', [TStr.GetPrintable(S)], Margin+R.Position.Start div 2)
    else
    begin
      L('[EXP] %s', [StringOfChar(' ', R.Position.Start div 2) + TStr.GetPrintable(S)], Margin);
      L('[INP] %s', [TStr.GetPrintable(T)], Margin);
    end;

    LogTextInputParseTree(ParseTree, R.FirstChild, Margin + 2);
    ResIndex := ParseTree.Items[ResIndex].NextSibling;
  end;
end;

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
  Results.Clear;
  Stack.Clear;
  FullParseTree.Clear;
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
            ResIndex := FullParseTree.Add;
            FullParseTree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtChar:
        begin
          Len    := TGrammarChar(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := FullParseTree.Add;
            FullParseTree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtCharClass:
        begin
          Len    := TGrammarCharClass(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := FullParseTree.Add;
            FullParseTree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
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
                 FullParseTree.Items[FullParseTree.Add].SetUp(Item.Rule, Data.Position, Len, Item.ResIndex);
                 FullParseTree.Items[Item.ResIndex].NextSibling := ResIndex;
                 ResIndex := FullParseTree.Count-1;
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
               FullParseTree.Items[FullParseTree.Add].SetUp(Item.Rule, Data.Position, Len, ResIndex);
               ResIndex := FullParseTree.Count-1;
               Stack.DeleteLast;
             end else begin
               Inc(Item.Step);
               Stack.Items[Stack.Add].SetUp(TGrammarSelection(Item.Rule).Op2.Data);
             end;
          2: begin
               if Accept then
               begin
                 FullParseTree.Items[FullParseTree.Add].SetUp(Item.Rule, Data.Position, Len, ResIndex);
                 ResIndex := FullParseTree.Count-1;
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
            Item.Len := 0;
            Item.ResIndex := FullParseTree.Add;
            FullParseTree.Items[Item.ResIndex].SetUp(Item.Rule, Data.Position, 0);
            Stack.Items[Stack.Add].SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
          end
        else
        if Accept and (Item.Step < TGrammarGreedyRepeater(Item.Rule).MaxCount) then begin
          Inc(Item.Step);
          Inc(Item.Len, Len);
          if Item.Step=2 { we just incremented, 2 means that first accepted iteration } then
            FullParseTree.Items[Item.ResIndex].FirstChild := ResIndex
          else
            FullParseTree.Items[Item.ResLastChild].NextSibling := ResIndex;
          Item.ResLastChild := ResIndex;
          Data.Position := Data.Position + Len;
          Stack.Items[Stack.Add].SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
        end
        else begin
          { add last iteration to the result (if accepted) }
          if Accept then
          begin
            Inc(Item.Step);
            Inc(Item.Len, Len);
            if Item.Step=2 { we just incremented, 2 means that first accepted iteration } then
              FullParseTree.Items[Item.ResIndex].FirstChild := ResIndex
            else
              FullParseTree.Items[Item.ResLastChild].NextSibling := ResIndex;
          end;
          { ">" because Step=AcceptedCount+1. No need to check MaxCount here. }
          Accept := (Item.Step > TGrammarGreedyRepeater(Item.Rule).MinCount);
          { assign or reset result }
          if not Accept then
            FullParseTree.Count := Item.ResIndex
          else
          begin
            ResIndex := Item.ResIndex;
            Len := Item.Len;
            FullParseTree.Items[ResIndex].Position.Len := Len;
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
            ResIndex := FullParseTree.Add;
            FullParseTree.Items[ResIndex].SetUp(Item.Rule, Data.Position,0);
          end;
          Stack.DeleteLast;
        end;

      gtEOF:
        begin
          Accept := Data.EOF;
          if Accept then
          begin
            Len := 0;
            ResIndex := FullParseTree.Add;
            FullParseTree.Items[ResIndex].SetUp(Item.Rule, Data.Position,0);
          end;
          Stack.DeleteLast;
        end;

    end; { case Item.Rule.GrammarType of }
  until Stack.Count=0;
  result := Accept;
  if result then
    FullParseTreeRoot := ResIndex
  else
  begin
    FullParseTreeRoot := -1;
    FullParseTree.Clear;
  end;
end;

end.
