unit adot.Grammar.Demo;

interface

uses
  adot.Collections,
  adot.Strings,
  adot.Tools,
  adot.Tools.IO,
  adot.Grammar,
  adot.Grammar.Types,
  adot.Grammar.Peg,
  System.SysUtils;

type
  THTMLParser = class(TCustomLanguage)
  protected

    { grammar rules }
    Dok, Element,
    EndTag, StartTag,
    Definition, DefContent,
    Comment, CommentBody,
    Attr, AttrName, AttrValue,
    DoubleQuotedAttrValue,
    SingleQuotedAttrValue,
    UnquotedAttrValue,
    TagName, WS, AsciiChar: TGrammar;

    { parser (TPegParser for example) }
    Parser: TGrammarParser;

    function GetTotalSizeBytes: int64;
  public
    type
      TReplaceImageProc = reference to procedure(var ImageSrc: string);

    constructor Create;
    destructor Destroy; override;

    { Parse input text and build ParseTree }
    function Parse(const Html: string): Boolean;

    { StartTagNode - index of StartTag in ParseTree }
    function GetTagName(StartTagNode: integer): string;

    { AttrNode - index of Attr in ParseTree }
    function GetAttrName(AttrNode: integer): string;

    { AttrNode - index of Attr in ParseTree, AttrValueNode - index of AttrValue in ParseTree }
    function FindAttrValue(AttrNode: integer; var AttrValueNode: integer): boolean;

    { StartTagNode - index of StartTag in ParseTree, AttrNode - index of first Attr with given Name }
    function FindAttrByName(StartTagNode: integer; const Name: string; var AttrNode: integer): boolean;

    { Replacing/enumerating of images in HTML }
    function ReplaceImages(AProc: TReplaceImageProc): string; overload;

    { Replace all images in source HTML to ANewImageSrc, for example: ReplaceImages('"NoImage.jpg"').
      Returns new HTML generated. }
    function ReplaceImages(const ANewImageSrc: string): string; overload;

    class function RandomHtml(SizeBytes: int64): string; static;

    property GrammarParser: TGrammarParser read Parser;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
  end;

  TCalc = class(TCustomLanguage)
  protected
    Input, Expr, Sum, Product, Value, SumOp, ProdOp, MulOp: TGrammar;
    Number,FixedNum,IntNum,Digit: TGrammar;
    Parser: TGrammarParser;
    FormatSettings: TFormatSettings;

    function GetResultValue(TreeIndex: integer): Double;
  public
    constructor Create;
    destructor Destroy; override;

    function Evaluate(const Expr: string): Double;

    class function Eval(const Expr: string): Double; static;
  end;

implementation

{ THTMLParser }

constructor THTMLParser.Create;
begin
  { We will try to create robust parser. It should be able to recognize correct HTML
    elements even in non-HTML stream (corrupted files, HTML generating errors etc). }

  { we use small optimization here, we skip all chars until "<", it makes parser 50% faster }
  //Dok        := ( Ex(Element) or Ex(ccAny))*Rep;
  Dok        := ((not Ex('<',True) + Ex(ccAny))*Rep + (Ex(Element) or Ex(ccAny)))*Rep;
  Element    := Ex(EndTag) or Ex(StartTag) or Ex(Definition) or Ex(Comment);

  { definition: <!DOCTYPE html>}
  Definition := Ex('<!') + Ex(DefContent) + Ex('>');
  DefContent := (not Ex('>') + Ex(ccAny))*Rep;

  { Comment: <!-- xxx --> }
  Comment    := Ex('<!--') + Ex(CommentBody) + Ex('-->');
  CommentBody:= (not Ex('-->') + Ex(ccAny))*Rep;

  { Tags }
  EndTag     := Ex('</') + Ex(TagName)*Opt + Ex('>');
  StartTag   := Ex('<' ) + Ex(TagName)*Opt + Ex(Attr)*Rep + Ex(WS) + Ex('/')*Opt + Ex('>');

  { attribute: https://html.spec.whatwg.org/multipage/syntax.html#attributes-2 }
  Attr       := Ex(WS) + Ex(AttrName) + Ex(WS) + (Ex('=') + Ex(WS) + Ex(AttrValue))*Opt;
  AttrName   := (not Ex(ccWhiteSpace) + not Ex(ccControl) + not Ex(['"','''','>','/','=']) + Ex(ccAny))*Rep1;
  AttrValue  :=
    (Ex('"')  + Ex(DoubleQuotedAttrValue) + Ex('"'))  or
    (Ex('''') + Ex(SingleQuotedAttrValue) + Ex('''')) or
    (Ex(UnquotedAttrValue));
  DoubleQuotedAttrValue := (not Ex('"')  + Ex(ccAny))*Rep;
  SingleQuotedAttrValue := (not Ex('''') + Ex(ccAny))*Rep;
  UnquotedAttrValue     := (not Ex(ccWhiteSpace) + not Ex(['"','''','=','<','>','`']) + Ex(ccAny)) * Rep1;

  { additional rules }
  AsciiChar  := Ex('a','z') or Ex('0','9');
  TagName    := Ex(AsciiChar)*Rep1;
  WS         := Ex(ccWhiteSpace)*Rep;

  { readable names (usefull for debugging, logging etc) }
  SetNames(
    [ Dok,  Element,   Definition,   DefContent,   Comment,   CommentBody,   EndTag,  StartTag,  Attr,  AttrName,  AttrValue,
      DoubleQuotedAttrValue,  SingleQuotedAttrValue,  UnquotedAttrValue,   TagName,  WS,  AsciiChar],
    ['Dok','Element', 'Definition', 'DefContent', 'Comment', 'CommentBody', 'EndTag','StartTag','Attr','AttrName','AttrValue',
     'DoubleQuotedAttrValue','SingleQuotedAttrValue','UnquotedAttrValue', 'TagName','WS','AsciiChar']);

  Parser := TPegParser.Create(Dok.Grm);
end;

destructor THTMLParser.Destroy;
begin
  FreeAndNil(Parser);
  inherited;
end;

function THTMLParser.Parse(const Html: string): Boolean;
begin
  Result := Parser.Accepts(Html);
end;

function THTMLParser.GetTagName(StartTagNode: integer): string;
var I: integer;
begin
  for I in Parser.ParseTree.RuleMatches[StartTagNode, TagName.Id] do
    Exit( Parser.DataToken[Parser.ParseTree.Tree.Nodes.Items[I].Data.Position] );
  Result := '';
end;

function THTMLParser.GetTotalSizeBytes: int64;
begin
  result := Parser.TotalSizeBytes;
end;

function THTMLParser.GetAttrName(AttrNode: integer): string;
var I: integer;
begin
  for I in Parser.ParseTree.RuleMatches[AttrNode, AttrName.Id] do
    Exit( Parser.DataToken[Parser.ParseTree.Tree.Nodes.Items[I].Data.Position] );
  Result := '';
end;

function THTMLParser.FindAttrValue(AttrNode: integer; var AttrValueNode: integer): boolean;
var I: integer;
begin
  for I in Parser.ParseTree.RuleMatches[AttrNode, AttrValue.Id] do
  begin
    AttrValueNode := I;
    Exit(True);
  end;
  Result := False;
end;

function THTMLParser.FindAttrByName(StartTagNode: integer; const Name: string; var AttrNode: integer): boolean;
var I: integer;
begin
  for I in Parser.ParseTree.RuleMatches[StartTagNode, Attr.Id] do
    if TStr.SameText(GetAttrName(I),Name) then
    begin
      AttrNode := I;
      Exit(True);
    end;
  Result := False;
end;

function THTMLParser.ReplaceImages(AProc: TReplaceImageProc): string;
var
  StringEditor: TStringEditor;
  StartTagNode,AttrNode,AttrValueNode: integer;
  Src, Dst: string;
begin
  StringEditor.Clear;
  { Hierarchy of HTML elements in our parser (see grammar definition for details):
    Dok
      Element
        StartTag
          TagName?
          Attr*
            AttrName
            AttrValue
        EndTag
          TagName? }
  for StartTagNode in Parser.ParseTree.RuleMatches[0, StartTag.Id] do
    if TStr.SameText(GetTagName(StartTagNode), 'img') then
      if FindAttrByName(StartTagNode, 'src', AttrNode) then
        if FindAttrValue(AttrNode, AttrValueNode) then
        begin
          Src := Parser.DataToken[Parser.ParseTree[AttrValueNode].Position];
          Dst := Src;
          AProc(Dst);
          if Src<>Dst then
            StringEditor.Replace(Parser.ParseTree[AttrValueNode].Position.BytesToChars, Dst);
        end;

  Result := StringEditor.Apply(Parser.Data.Text);
end;

class function THTMLParser.RandomHtml(SizeBytes: int64): string;
var
  Buf: TBuffer;

  { generates random attrobutes with or without values }
  function GenAttrNameAndValue: string;
  begin
    { attr name }
    result := TStr.Random(1+Random(10));
    { attr value }
    case Random(4) of
      0 : ;
      1 : result := result + '='   + TStr.Random(1+Random(10));
      2 : result := result + '="'  + TStr.Random(1+Random(10)) + '"';
      3 : result := result + '=''' + TStr.Random(1+Random(10)) + '''';
    end;
  end;

  { generates nodename with random attributes without '<>') }
  procedure GenNodeName(var NodeName,NodeWithRandomAttrs: string);
  var
    I,J: Integer;
  begin
    NodeName := TStr.Random(1+Random(10));
    J := Random(5);
    NodeWithRandomAttrs := NodeName;
    for I := 0 to J-1 do
      NodeWithRandomAttrs := NodeWithRandomAttrs + ' ' + GenAttrNameAndValue;
  end;

  { generates node with sunbodes recursively }
  procedure GenNode(offset: integer);
  var
    NodeName,NodeWithAttrs: string;
    I,J,N: integer;
  begin
    J := Random(10);
    for I := 0 to J do
    begin
      if Buf.Size >= SizeBytes then Exit;
      GenNodeName(NodeName, NodeWithAttrs);
      Buf.Write( StringOfChar(' ',Offset) );
      N := Random(31);
      case N of
        0..10:
          Buf.Write( format('<%s/>#13#10', [NodeWithAttrs]) );
        11..20:
          Buf.Write( format('%s'#13#10, [NodeName]) ); { content }
        21..30:
          begin
            Buf.Write( format('<%s>'#13#10, [NodeWithAttrs]) );
            GenNode(offset + 2);
            Buf.Write( StringOfChar(' ',Offset) );
            Buf.Write( format('</%s>'#13#10, [NodeName]) );
          end;
      end;
    end;
  end;

begin
  Buf.Clear;
  Buf.Write('<html>');
  while Buf.Size < SizeBytes do
    GenNode(2);
  Buf.Write('</html>');
  result := Buf.Text;
end;

function THTMLParser.ReplaceImages(const ANewImageSrc: string): string;
begin
  Result := ReplaceImages(
    procedure(var ImageSrc: string)
    begin
      ImageSrc := ANewImageSrc;
    end);
end;

{ TCalc }

constructor TCalc.Create;
begin
  { general parser of expressions: 12+3, 4/5, 2*4, (3+1)*2 etc }
  Input   := Ex(Expr) + EOF;
  Expr    := Ex(Sum);
  Sum     := Ex(Product) + (Ex(SumOp) + Ex(Product))*Rep;
  Product := Ex(Value)   + (Ex(MulOp) + Ex(Value))*Rep;
  Value   := Ex(Number) or (Ex('(') + Ex(Expr) + Ex(')'));
  SumOp   := Ex('+') or Ex('-');
  MulOp   := Ex('*') or Ex('/');

  { general parser of numbers: 1, 1.2, -12e-5 etc }
  Number   := Ex(FixedNum) + (Ex('E') + Ex(IntNum))*Opt;             { general number    : +12.3E-10 }
  FixedNum := Ex(IntNum) + (Ex('.') + Ex(IntNum)*Opt)*Opt;           { fixed point number: +12.3     }
  IntNum   := (Ex('+') or Ex('-'))*Opt + Ex(Digit) + Ex(Digit)*Rep;  { integer number    : -12       }
  Digit    := Ex('0','9');

  { Friendly names for expressions parser }
  Input.IncludeIntoParseTree    := False;
  Expr.IncludeIntoParseTree     := False;
  Sum.Name                      := 'Sum';
  SumOp.Name                    := 'SumOp';
  MulOp.Name                    := 'MulOp';
  Product.Name                  := 'Product';
  Value.IncludeIntoParseTree    := False;
  { Friendly names for numbers parser }
  Number.Name                   := 'Number';
  FixedNum.IncludeIntoParseTree := False;
  IntNum.IncludeIntoParseTree   := False;
  Digit.IncludeIntoParseTree    := False;

  Parser := TPegParser.Create(Input.Grm);
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DecimalSeparator := '.';
end;

destructor TCalc.Destroy;
begin
  Input.Release; { Just in case (if Parser is not initialized) }
  FreeAndNil(Parser);
  inherited;
end;

function TCalc.GetResultValue(TreeIndex: integer): Double;
var
  Id: TRuleId;
  I,J: integer;
  ParseTree: TParseTree;
begin
  ParseTree := Parser.ParseTree;
  Id := ParseTree.Tree.Nodes.Items[TreeIndex].Data.Rule.Id;
  { Sum  := Ex(Product) + ((Ex('+') or Ex('-')) + Ex(Product))*Rep; }
  if Id=Sum.Id then
  begin
    I := ParseTree.Tree.Nodes.Items[TreeIndex].FirstChild;
    Assert(I>=0);
    result := GetResultValue(I); { Product }
    repeat
      I := ParseTree.NextSibling[I]; { SumOp }
      if I<0 then Break;
      J := ParseTree.NextSibling[I]; { Product }
      Assert(J>=0);
      Assert(ParseTree.Tree.Nodes.Items[I].Data.Rule.Id=SumOp.Id);
      if Parser.DataToken[ParseTree.Tree.Nodes.Items[I].Data.Position]='+' then
        Result := Result + GetResultValue(J)
      else
        Result := Result - GetResultValue(J);
      I := J;
    until False;
  end
  { Product := Ex(Value) + ((Ex('*') or Ex('/')) + Ex(Value))*Rep; }
  else if Id=Product.Id then
  begin
    I := ParseTree.Tree.Nodes.Items[TreeIndex].FirstChild;
    Assert(I>=0);
    result := GetResultValue(I); { Value }
    repeat
      I := ParseTree.NextSibling[I]; { MulOp }
      if I<0 then Break;
      J := ParseTree.NextSibling[I]; { Value }
      Assert(J>=0);
      Assert(ParseTree.Tree.Nodes.Items[I].Data.Rule.Id=MulOp.Id);
      if Parser.DataToken[ParseTree.Tree.Nodes.Items[I].Data.Position]='*' then
        Result := Result * GetResultValue(J)
      else
        Result := Result / GetResultValue(J);
      I := J;
    until False;
  end
  else if Id=Number.Id then
    Result := StrToFloat(Parser.DataToken[ParseTree.Tree.Nodes.Items[TreeIndex].Data.Position], FormatSettings)
  else
    raise Exception.Create('Internal error');
end;

class function TCalc.Eval(const Expr: string): Double;
var
  C: TCalc;
begin
  C := TCalc.Create;
  try
    Result := C.Evaluate(Expr);
  finally
    C.Free;
  end;
end;

function TCalc.Evaluate(const Expr: string): Double;
begin
  if not Parser.Accepts(Expr) then
    raise Exception.Create('Input rejected');
  Parser.LogParseTree;
  result := GetResultValue(0);
end;


end.
