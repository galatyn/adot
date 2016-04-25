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
      TCallStackItem = record
        Rule: TGrammarClass;
        Step: integer;
        Start: integer;
        Len: integer;

        procedure SetUp(ARule: TGrammarClass); {$IFNDEF DEBUG}inline;{$ENDIF}
      end;

  public
    procedure RunSubexpression(Expr,SubExpr: TGrammarClass; Tag: integer); override;
    function Accepted: Boolean; override;
  end;

implementation

{ TPegParser.TCallStackItem }

procedure TPegParser.TCallStackItem.SetUp(ARule: TGrammarClass);
begin
  Rule := ARule;
  Step := 0;
end;

{ TPegParser }

procedure TPegParser.RunSubexpression(Expr, SubExpr: TGrammarClass; Tag: integer);
var
  Accept: Boolean;
  P: TPos;
begin
  Accept := SubExpr.Accepted(Self, P);
  Expr.SubExprResult(Accept, P, Tag);
end;

function TPegParser.Accepted: Boolean;
var
  Res: TPos;
  Accept: boolean;
  Stack: TVector<TCallStackItem>;
  Item,NewItem: TCallStackItem;
begin
  Accept := False;
  Stack.Clear;
  Item.SetUp(Grammar);
  Stack.Add(Item);
  repeat
    Item := Stack.Last;
    case Item.Rule.GrammarType of
      gtUnknown:
        raise Exception.Create('rule is not initialized');
      gtLink:
        Stack.Items[Stack.Count-1].SetUp(TGrammarLink(Item.Rule).Op.Data);
      gtString:
        begin
          Accept := TGrammarString(Item.Rule).Accepted(Self, Res);
          Stack.DeleteLast;
        end;
      gtChar:
        begin
          Accept := TGrammarChar(Item.Rule).Accepted(Self, Res);
          Stack.DeleteLast;
        end;
      gtSequence:
        case Item.Step of
          0: begin
               Inc(Stack.Items[Stack.Count-1].Step);
               NewItem.SetUp(TGrammarSequence(Item.Rule).Op1.Data);
               Stack.Add(NewItem);
             end;
          1: if not Accept then Stack.DeleteLast else
             begin
               with Stack.Items[Stack.Count-1] do
               begin
                 Inc(Step);
                 Start := Res.Start;
               end;
               NewItem.SetUp(TGrammarSequence(Item.Rule).Op2.Data);
               Stack.Add(NewItem);
             end;
          2: begin
               Res.SetPos(Item.Start, Res.Start+Res.Len-Item.Start);
               Stack.DeleteLast;
             end;
        end;
      gtSelection:
        case Item.Step of
          0: begin
               Inc(Stack.Items[Stack.Count-1].Step);
               NewItem.SetUp(TGrammarSelection(Item.Rule).Op1.Data);
               Stack.Add(NewItem);
             end;
          1: if Accept then Stack.DeleteLast else
             begin
               Inc(Stack.Items[Stack.Count-1].Step);
               NewItem.SetUp(TGrammarSelection(Item.Rule).Op2.Data);
               Stack.Add(NewItem);
             end;
          2: Stack.DeleteLast;
        end;
      gtRepeat:
        if Item.Step=0 then
          if TGrammarGreedyRepeater(Item.Rule).MaxCount <= 0 then begin
            Accept := False;
            Stack.DeleteLast;
          end else begin
            with Stack.Items[Stack.Count-1] do begin
              Start := Data.Position;
              Inc(Step);
            end;
            NewItem.SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
            Stack.Add(NewItem);
          end
        else
        if Accept and (NewItem.Step < TGrammarGreedyRepeater(Item.Rule).MaxCount) then begin
          with Stack.Items[Stack.Count-1] do begin
            Inc(Step);
            Len := Res.Start+Res.Len-Start;
          end;
          NewItem.SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
          Stack.Add(NewItem);
        end
        else begin
          with TGrammarGreedyRepeater(Item.Rule) do
            Accept := (Item.Step >= MinCount) and (Item.Step <= MaxCount);
          Res.SetPos(Item.Start, Item.Len);
          Stack.DeleteLast;
        end;
      gtNot:
        if Item.Step=0 then
        begin
          Inc(Stack.Items[Stack.Count-1].Step);
          NewItem.SetUp(TGrammarNot(Item.Rule).Op.Data);
          Stack.Add(NewItem);
        end
        else
        begin
          Accept := not Accept;
          Res.Len := 0;
          Stack.DeleteLast;
        end;
      gtEOF:
        begin
          Accept := Data.EOF;
          Res.Start := Data.Position;
          Res.Len := 0;
          Stack.DeleteLast;
        end;
    end;
  until Stack.Count=0;
  result := Accept;
end;

end.
