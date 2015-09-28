unit adot.CrossPlatform.PEG.Test;

interface

uses
  {$IFDEF PEGLOG}
  adot.Log, adot.VCL.Log,
  {$ENDIF}
  adot.PEG, adot.PEGR;

type
  TTests = class
  private
    class procedure Test_Num; static;
    class procedure Test_LeftRecursion; static;
    class procedure Test_ExpressionParser; static;
    class procedure Test_ExpressionParserExt; static;
    class procedure Test_ExpressionParserExt2; static;
    class procedure Test_ExpressionPrimitive; static;
  end;

procedure Run;

implementation

procedure Run;
begin
//  TTests.Test_Num;
//  TTests.Test_LeftRecursion;
//  TTests.Test_ExpressionPrimitive;
  TTests.Test_ExpressionParser;
//  TTests.Test_ExpressionParserExt;
//  TTests.Test_ExpressionParserExt2;
end;

class procedure TTests.Test_Num;
var
  Num, Digits: TExpr;
begin
  {$IFDEF PEGLOG}
  AppLog.Log('');
  AppLog.Log('Num    ← (''+'' / ''-'') Digits');
  AppLog.Log('Digits ← [0-9]+');
  {$ENDIF}
  {
    Num = ('+' / '-') Digits
    Digits = [0-9]+
  }
  Num :=
    E(@Digits) or
    (E('+') or E('-')) and E(@Digits);
  Digits := E.Rep1( E(['0'..'9']) );
  Assert(ParseAndLog(Num, '12'));
end;

class procedure TTests.Test_ExpressionPrimitive;
var
  Number, Digit: TExpr;
begin
  {$IFDEF PEGLOG}
  AppLog.Log('');
  AppLog.Log('Number  ← Digit Digit*');
  AppLog.Log('Digit   ← "1"');
  {$ENDIF}

  // Friendly names
  Number.Name  := 'Number';
  Digit.Name   := 'Digit';

  // Grammar
  Number  := @Digit and E.Rep0( @Digit );
  Digit   := E('1');

  Assert(ParseAndLog(Number, '11'));

end;

class procedure TTests.Test_ExpressionParser;
var
  Expr, Sum, Product, Value: TExpr;
begin
  {$IFDEF PEGLOG}
  AppLog.Log('');
  AppLog.Log('Expr    ← Sum');
  AppLog.Log('Sum     ← Product ((''+'' / ''-'') Product)*');
  AppLog.Log('Product ← Value ((''*'' / ''/'') Value)*');
  AppLog.Log('Value   ← [0-9]+ / ''('' Expr '')''');
  {$ENDIF}
  {
    https://en.wikipedia.org/wiki/Parsing_expression_grammar
    PEG:

    Expr    ← Sum
    Sum     ← Product (('+' / '-') Product)*
    Product ← Value (('*' / '/') Value)*
    Value   ← [0-9]+ / '(' Expr ')'

  }

  // Friendly names
  Expr.Name    := 'Expr';
  Sum.Name     := 'Sum';
  Product.Name := 'Product';
  Value.Name   := 'Value';

  // Grammar
  Expr    := @Sum;
  Sum     := @Product and E.Rep0( (E('+') or E('-')) and @Product );
  Product := @Value   and E.Rep0( (E('*') or E('/')) and @Value   );
  Value   :=
    E.Rep1( E(['0'..'9']) ) or
    E('(') and @Expr and E(')');

//  Assert(ParseAndLog(Expr, '12'));
//  Assert(ParseAndLog(Expr, '1+2'));
//  Assert(ParseAndLog(Expr, '1*2'));
//  Assert(ParseAndLog(Expr, '(1+2)*3'));
  Assert(ParseAndLog(Expr, '(((1+(2*4+3/7)*12-3)+100)+2+3*4)*27'));
end;

class procedure TTests.Test_ExpressionParserExt;
var
  Expr, Sum, Product, Value, Num, IntNum: TExpr;
begin
  {$IFDEF PEGLOG}
  AppLog.Log('');
  AppLog.Log('Expr    ← Sum');
  AppLog.Log('Sum     ← Product ((''+'' / ''-'') Product)*');
  AppLog.Log('Product ← Value ((''*'' / ''/'') Value)*');
  AppLog.Log('Value   ← [0-9]+ / ''('' Expr '')''');
  {$ENDIF}

  // Grammar
  Expr    := @Sum;
  Sum     := @Product and E.Rep0( (E('+') or E('-')) and @Product );
  Product := @Value   and E.Rep0( (E('*') or E('/')) and @Value   );
  Value   := @Num  or E('(') and @Expr and E(')');
  Num     := @IntNum and E.Optional( E('.') and @IntNum );
  IntNum  := E.Rep1( E(['0'..'9']) );

  // Friendly names
  Expr.Name    := 'Expr';
  Sum.Name     := 'Sum';
  Product.Name := 'Product';
  Value.Name   := 'Value';
  Num.Name     := 'Num';

  //Assert(ParseAndLog(Expr, '1+2'));
  Assert(ParseAndLog(Expr, '12'));
  Assert(ParseAndLog(Expr, '1.2'));
  Assert(ParseAndLog(Expr, '1+2'));
  Assert(ParseAndLog(Expr, '1*2'));
  Assert(ParseAndLog(Expr, '(1+2)*3'));
  Assert(ParseAndLog(Expr, '(((1+(2*4.55+3/7)*12-3)+100.0)+2+3*4)*27'));
end;

{
  We can support recursion without cursor movement, but it is very easy to
  get inefficient parser in this case. For exmaple we will have to define max allowed deepness.
  For example: "(((((((1)+2)+3)+4)+5)+6)+7)"
  With simple recursive grammar we will have to go may require 7 recursive calles without cursor movement
  to parse if.
}
class procedure TTests.Test_LeftRecursion;
var
  FORML,PAR,MUL,MULOP,SUM,SUMOP,NUM: TExpr;
//  psum: TPEGSequence;
  pfrm: TPEGChoice;
  pnum: TPEGString;
//  ppls: TPEGString;
  pmul: TPEGSequence;
  ppar: TPEGSequence;
  pbr1: TPEGString;
  pbr2: TPEGString;
  pmop: TPEGString;
begin
  {
    FORML = PAR / MUL / SUM / NUM
    PAR = '(' FORML ')'
    MUL = FORML MULOP FORML
    MULOP = '*' / '/'
    SUM = FORML SUMOP FORML
    SUMOP = '+' / '-'
    NUM = ('+' / '-')? ['0'-'9']+

    (1)*2 :
    FORML-> MUL -> FORML
                   *
                   FORML
   To enable left recursion we should:
   + detect recursive call [PEG; POS]
   - check recursive path list (for example above it is "MUL")
   - empty path is not allowed (return fail)
   - duplicated path is not allowed (return fail)

   FORML = SUM / NUM
   SUM   = FORML '+' FORML
   NUM   = [0..9]*
  }
  FORML := E(@PAR) or E(@MUL) or E('1');
  PAR   := E('(') and E(@FORML) and E(')');
  MUL   := E(@FORML) and E('*') and E(@FORML);
  //MULOP := E('*') or E('/');
  //SUM   := E(@FORML) and E(@SUMOP) and E(@FORML);
  //SUMOP := E('+') or E('-');
  //NUM   := E.Rep1( E(['0'..'9']) );
  // fromls - sum - forml

 { Assert(ParseAndLog(FORML, '(1)*1'));
  exit;
  Assert(ParseAndLog(FORML, '12+11-13*3/14'));

  Assert(ParseAndLog(FORML, '1'));
  Assert(ParseAndLog(FORML, '12'));
  Assert(ParseAndLog(FORML, '1+2'));
  Assert(ParseAndLog(FORML, '1*2'));
  Assert(ParseAndLog(FORML, '(1)*2'));
  Assert(ParseAndLog(FORML, '(1+2)*3'));
  Assert(ParseAndLog(FORML, '(1+2)*3'));
  Assert(ParseAndLog(FORML, '(((1+(2*4+3/7)*12-3)+100)+2+3*4)*27')); }

  pfrm := TPEGChoice.Create;             pfrm.Name := 'FORML';
  ppar := TPEGSequence.Create;           ppar.Name := 'PAR';
  pmul := TPEGSequence.Create;           pmul.Name := 'MUL';

  pnum := TPEGString.Create('1', false); pnum.Name := '1';
  pbr1 := TPEGString.Create('(', false); pbr1.Name := '(';
  pbr2 := TPEGString.Create(')', false); pbr2.Name := ')';
  pmop := TPEGString.Create('*', false); pnum.Name := '*';

{  psum := TPEGSequence.Create;           psum.Name := 'SUM';
  pnum := TPEGString.Create('1', false); pnum.Name := '1';
  ppls := TPEGString.Create('+', false); ppls.Name := '+';}
  pfrm.Add([ ppar, pmul, pnum ]);
  ppar.Add([ pbr1, pfrm, pbr2 ]);
  pmul.Add([ pfrm, pmop, pfrm ]);


  Assert(Verify(pfrm, '((1)*1)'));

  Assert(ParseAndLog(FORML, '1'));
  Assert(ParseAndLog(FORML, '12'));
  Assert(ParseAndLog(FORML, '1+2'));
  Assert(ParseAndLog(FORML, '1*2'));
  Assert(ParseAndLog(FORML, '(1)*2'));
  Assert(ParseAndLog(FORML, '(1+2)*3'));
  Assert(ParseAndLog(FORML, '(((1+(2*4+3/7)*12-3)+100)+2+3*4)*27'));





  exit;
  FORML := E(@PAR) or E(@MUL) or E(@SUM) or E(@NUM);
  PAR   := E('(')    and E(@FORML) and E(')');
  MUL   := E(@FORML) and E(@MULOP) and E(@FORML);
  MULOP := E('*')     or E('/');
  SUM   := E(@FORML) and E(@SUMOP) and E(@FORML);
  SUMOP := E('+')     or E('-');
  NUM   := E.Rep1( E(['0'..'9']) );
    //E.Optional(E('+') or E('-')) and E.Rep1( E(['0'..'9']) );

  FORML.Name := 'formula';
  PAR.Name   := 'parentheses';
  MUL.Name   := 'mul';
  MULOP.Name := '"*" or "/"';
  SUM.Name   := 'sum';
  SUMOP.Name := '"+" or "-"';
  NUM.Name   := 'number';

  Assert(ParseAndLog(FORML, '1+2'));

  Assert(ParseAndLog(FORML, '1'));
  Assert(ParseAndLog(FORML, '12'));
  Assert(ParseAndLog(FORML, '1+2'));
  Assert(ParseAndLog(FORML, '1*2'));
  Assert(ParseAndLog(FORML, '(1)*2'));
  Assert(ParseAndLog(FORML, '(1+2)*3'));
  Assert(ParseAndLog(FORML, '(((1+(2*4+3/7)*12-3)+100)+2+3*4)*27'));

end;

class procedure TTests.Test_ExpressionParserExt2;
var
  S, Expr, Sum, Product, Value: TExpr;
  Syntax, Rule, OptWhitespace, Expression, LineEnd, List, Term, Literal, EOL, RuleName, NameChar, Digit: TExpr;
begin
  {
    PEG with left-recursion (can not be processed by most of PEG parsers):

    Value   ← [0-9]+ / '(' Expr ')'
    Product ← Expr (('*' / '/') Expr)*
    Sum     ← Expr (('+' / '-') Expr)*
    Expr    ← Product / Sum / Value
  }
  Value   :=
    E.Rep1( E(['0'..'9']) ) or
    E('(') and E(Expr) and E(')');
  Product := E(Expr) and E.Rep0( (E('*') or E('/')) and E(Expr) );
  Sum     := E(Expr) and E.Rep0( (E('+') or E('-')) and E(Expr) );
  Expr    := E(Product) or E(Sum) or E(Value);
  Assert(ParseAndLog(Expr, '12'));
  Assert(ParseAndLog(Expr, '1+2'));
  Assert(ParseAndLog(Expr, '1*2'));
  Assert(ParseAndLog(Expr, '(1+2)*3'));
  Assert(ParseAndLog(Expr, '(((1+(2*4+3/7)*12-3)+100)+2+3*4)*27'));
  Assert(not Verify(S, '(12+2'));
  Assert(not Verify(S, '(12+2))(3'));
  Assert(not Verify(S, '(12+2))*(3'));

  {
    PEG packrat parsers cannot recognize some unambiguous nondeterministic CFG rules, such as the following:
    S ← 'x' S 'x' | 'x'
    Neither LL(k) nor LR(k) parsing algorithms are capable of recognizing this example.
  }
  S := E('x') and E(S) and E('x') or E('x');
  Assert(ParseAndLog(S, 'x'));
  Assert(ParseAndLog(S, 'xx'));
  Assert(ParseAndLog(S, 'xxx'));
  Assert(ParseAndLog(S, 'xxxx'));
  Assert(ParseAndLog(S, 'xxxxx'));
  Assert(ParseAndLog(S, 'xxxxxx'));
  Assert(ParseAndLog(S, 'xxxxxxx'));
  Assert(not Verify(S, 'y'));
  Assert(not Verify(S, 'yy'));
  Assert(not Verify(S, 'xy'));
  Assert(not Verify(S, 'xxy'));
  Assert(not Verify(S, 'xxxy'));
  Assert(not Verify(S, 'xxxxy'));

  {
    https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form
    BNF grammar

    <syntax>         ::= <rule> | <rule> <syntax>
    <rule>           ::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
    <opt-whitespace> ::= " " <opt-whitespace> | ""
    <expression>     ::= <list> | <list> "|" <expression>
    <line-end>       ::= <opt-whitespace> <EOL> | <line-end> <line-end>
    <list>           ::= <term> | <term> <opt-whitespace> <list>
    <term>           ::= <literal> | "<" <rule-name> ">"
    <literal>        ::= '"' <text> '"' | "'" <text> "'"
  }
  Syntax :=
    E(Rule) or
    E(Rule) and E(Syntax);
  Rule :=
    E(OptWhitespace) and E('<')and E(RuleName) and E('>') and E(OptWhitespace) and E('::=') and E(OptWhitespace) and E(Expression) and E(LineEnd);
  OptWhitespace  :=
    E(' ') and E(OptWhitespace) or
    E('');
  Expression :=
    E(List) or
    E(List) and E('|') and E(Expression);
  LineEnd :=
    E(OptWhitespace) and E(EOL) or
    E(LineEnd) and E(LineEnd);
  List :=
    E(Term) or
    E(Term) and E(OptWhitespace) and E(List);
  Term :=
    E(Literal) or
    E('<') and E(RuleName) and E('>');
  Literal :=
    E('"')  and E.Rep0(E([' '..#127]-['"']))  and E('"') or
    E('''') and E.Rep0(E([' '..#127]-[''''])) and E('''');
  EOL :=
    E([13,10]) or
    E([10,13]) or
    E([13]) or
    E([10]);
  RuleName :=
    E(NameChar) and E.Rep0( E(NameChar) or E(Digit) );
  NameChar :=
    E(['a'..'z', 'A'..'Z', '_']);
  Digit :=
    E(['0'..'9']);
end;

end.
