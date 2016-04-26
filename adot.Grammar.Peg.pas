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

        procedure SetUp(ARule: TGrammarClass); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetUp(ARule: TGrammarClass; AStart: integer); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
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

procedure TPegParser.TCallStackItem.SetUp(ARule: TGrammarClass; AStart: integer);
begin
  Rule  := ARule;
  Step  := 0;
  Start := AStart;
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

  { stack of calls (to avoid deep recursion) }
  Stack: TVector<TCallStackItem>;

  { result of last operation on stack:
    Accept - input accepted
    Len - length of accepted block if Accept=True, undefined if Accept=False }
  Len: integer;
  Accept: boolean;

  { pointer to current/new item on the stack (invalid after any modification of the stack) }
  Item: PCallStackItem;

  { next rule to be executed/matched against current position in data stream }
  G: TGrammarClass;
begin
  Accept := False;
  Len := 0;
  Stack.Clear;
  Item := @Stack.Items[Stack.Add];
  Item.SetUp(Grammar);
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
          Stack.DeleteLast;
        end;

      gtChar:
        begin
          Len    := TGrammarChar(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          Stack.DeleteLast;
        end;

      gtSequence:
        case Item.Step of
          0: begin
               Inc(Item.Step);
               Item.Start := Data.Position;
               G := TGrammarSequence(Item.Rule).Op1.Data;
               Item := @Stack.Items[Stack.Add];
               Item.SetUp(G);
             end;
          1: if not Accept then Stack.DeleteLast else
             begin
               Inc(Item.Step);
               Item.Len := Len;
               Data.Position := Data.Position + Len;
               G := TGrammarSequence(Item.Rule).Op2.Data;
               Item := @Stack.Items[Stack.Add];
               Item.SetUp(G);
             end;
          2: begin
               Inc(Len, Item.Len);
               Data.Position := Item.Start;
               Stack.DeleteLast;
             end;
        end;

      gtSelection:
        case Item.Step of
          0: begin
               Inc(Item.Step);
               G := TGrammarSelection(Item.Rule).Op1.Data;
               Item := @Stack.Items[Stack.Add];
               Item.SetUp(G);
             end;
          1: if Accept then Stack.DeleteLast else
             begin
               Inc(Item.Step);
               G := TGrammarSelection(Item.Rule).Op2.Data;
               Item := @Stack.Items[Stack.Add];
               Item.SetUp(G);
             end;
          2: Stack.DeleteLast;
        end;

      gtRepeat:
        if Item.Step=0 then
          if TGrammarGreedyRepeater(Item.Rule).MaxCount <= 0 then begin
            Accept := False;
            Stack.DeleteLast;
          end else begin
            Item.Start := Data.Position;
            Item.Len := 0;
            Inc(Item.Step);
            G := TGrammarGreedyRepeater(Item.Rule).Op.Data;
            Item := @Stack.Items[Stack.Add];
            Item.SetUp(G);
          end
        else
        if Accept and (Item.Step < TGrammarGreedyRepeater(Item.Rule).MaxCount) then begin
          Inc(Item.Step);
          Inc(Item.Len, Len);
          Data.Position := Data.Position + Len;
          G := TGrammarGreedyRepeater(Item.Rule).Op.Data;
          Item := @Stack.Items[Stack.Add];
          Item.SetUp(G);
        end
        else begin
          if Accept then
            Inc(Item.Len, Len);
          with TGrammarGreedyRepeater(Item.Rule) do
            Accept := (Item.Step >= MinCount) and (Item.Step <= MaxCount);
          Len := Item.Len;
          Data.Position := Item.Start;
          Stack.DeleteLast;
        end;

      gtNot:
        if Item.Step=0 then
        begin
          Inc(Item.Step);
          G := TGrammarNot(Item.Rule).Op.Data;
          Item := @Stack.Items[Stack.Add];
          Item.SetUp(G);
        end
        else
        begin
          Accept := not Accept;
          Len := 0;
          Stack.DeleteLast;
        end;

      gtEOF:
        begin
          Accept := Data.EOF;
          Len := 0;
          Stack.DeleteLast;
        end;

    end; { case Item.Rule.GrammarType of }
  until Stack.Count=0;
  result := Accept;
end;

end.
