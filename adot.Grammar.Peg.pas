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
      end;
  public
    procedure RunSubexpression(Expr,SubExpr: TGrammarClass; Tag: integer); override;
    function Accepted: Boolean; override;
  end;

implementation

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
  P: TPos;
  A: boolean;
  Stack: TVector<TCallStackItem>;
  Item,NewItem: TCallStackItem;
begin
  Stack.Clear;
  Item.Rule := Grammar;
  Item.Step := 0;
  Stack.Add(Item);
  repeat
    Item := Stack.Last;
    case Item.Rule.GrammarType of
      gtUnknown:
        raise Exception.Create('rule is not initialized');
      gtLink:
        begin
          NewItem.Rule := TGrammarLink(Item.Rule).Op.Data;
          NewItem.Step := 0;
          Stack.Last   := NewItem;
        end;
      gtString:
        begin
          A := TGrammarString(Item.Rule).Accepted(Self, P);
          Stack.DeleteLast;
        end;
      gtChar:
        begin
          A := TGrammarChar(Item.Rule).Accepted(Self, P);
          Stack.DeleteLast;
        end;
      gtSequence:
        case Item.Step of
          0:
            begin
              NewItem.Rule := TGrammarSequence(Item.Rule).Op1.Data;
              NewItem.Step := 1;
              Stack.Add(NewItem);
            end;
          1:
            if not A then
              Stack.DeleteLast
            else
            begin
              NewItem.Rule := TGrammarSequence(Item.Rule).Op2.Data;
              NewItem.Step := 2;
              Stack.Add(NewItem);
            end;
          2:
            if not A then
              Stack.DeleteLast
            else
            begin
              NewItem.Rule := TGrammarSequence(Item.Rule).Op2.Data;
              NewItem.Step := 2;
              Stack.Add(NewItem);
            end;
        end;
      gtSelection: ;
      gtRepeat: ;
      gtNot: ;
      gtEOF: ;
    end;
    Result := Grammar.Accepted(Self, P);

  until Stack.Count=0;

end;

end.
