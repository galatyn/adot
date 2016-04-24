unit adot.Grammar.Peg;

{ Parser for grammar based on TGrammarClass:
  - recursive (similar to Recursive descent parser)
  - stops at first successfull choice (similar to "parsing expression grammars")
  - uses memoization of intermediate results (similar to "packrat parsing") }

interface

uses
  adot.Grammar.Types,
  adot.Tools,
  adot.Collections;

type

  TPegParser = class(TGrammarParser)
  protected
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
begin
  inherited;
  Result := Grammar.Accepted(Self, P);
end;

end.
