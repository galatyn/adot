unit adot.Grammar;

{ Uniform grammar presentation classes }

interface

uses
  adot.Types,
  adot.Collections,
  adot.Tools,
  adot.Strings,
  adot.Grammar.Types,
  adot.Tools.RTTI,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.Character,
  System.StrUtils;

type
  { Record type for grammar definition. Construct grammar rules with help of Ex/Rep functions:
    var
      Number,Digit: TGrammar;
    begin
      Number := Ex(Digit) + Ex(Digit)*Rep;
      Digit  := Ex('1') or Ex('2');
    end; }
  PGrammar = ^TGrammar;
  TGrammar = record
  public
    type
      {# repeater range (range of allowed repetitions for expression) }
      TRange = record
        MinCount, MaxCount: integer;
      end;

      {# Delphi doesn't provide any way to catch copy operator, we introduce separate
        type for all expression operations (right side of assigment) and Implicit operator
        to proceed assigment to TGrammar correctly. }
      TMedia = record
      private
        MediaGrm: IInterfacedObject<TGrammarClass>;

      public
        { Repeater: B*, A+, A[2;5] }
        class operator Multiply(A: TMedia; const R: TRange): TMedia;

        { Sequence: A "abc" B}
        class operator Add(A,B: TMedia): TMedia;

        { Selection:  A | "abc" }
        class operator LogicalOr(A,B: TMedia): TMedia;

        { Logical not: A + not "abc"}
        class operator LogicalNot(A: TMedia): TMedia;
      end;

    var
      Grm: IInterfacedObject<TGrammarClass>;

  private
    function GetDef: TGrammarClass; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetId: int64; {$IFNDEF DEBUG}inline;{$ENDIF}

  public
    class operator Implicit(A : TMedia) : TGrammar;

    property Def: TGrammarClass read GetDef;
    property Id: int64 read GetId;
  end;

  { abstract class for expression with no operands (string, char, EOF etc) }
  TGrammarClassOp0 = class abstract(TGrammarClass)
  public
    procedure Release; override;
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); override;
  end;

  { abstract class for expression with one operand (link, repeater, not etc) }
  TGrammarClassOp1 = class abstract(TGrammarClass)
  protected
    FOp: IInterfacedObject<TGrammarClass>;

    function GetInfo: string; override;
  public
    procedure Release; override;
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); override;

    property Op: IInterfacedObject<TGrammarClass> read FOp;
  end;

  { abstract class for expression with two operand (sequence, selection etc) }
  TGrammarClassOp2 = class abstract(TGrammarClass)
  protected
    FOp1: IInterfacedObject<TGrammarClass>;
    FOp2: IInterfacedObject<TGrammarClass>;

    function GetInfo: string; override;
  public
    procedure Release; override;
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); override;

    property Op1: IInterfacedObject<TGrammarClass> read FOp1;
    property Op2: IInterfacedObject<TGrammarClass> read FOp2;
  end;

  { When rule used link, we create TGrammarLink and initialize pointer FLink,
    but Op=nil, because it can be not initialized yet. Later, when all rules
    are initialized, SetMainRule will set Op. }
  TGrammarLink = class(TGrammarClassOp1)
  protected
    FLink: PGrammar;

    function GetInfo: string; override;
  public
    constructor Create(var ALink: TGrammar);
    procedure SetupRule; override;
  end;

  TGrammarString = class(TGrammarClassOp0)
  protected
    FValue: String;
    FCaseSensitive: boolean;

    function GetInfo: string; override;
  public
    constructor Create(Value: String; CaseSensitive: boolean);

    { input accepted: return length of accepted block
      input rejected: -1}
    function GetAcceptedBlock(var Buffer: TBuffer): integer;

    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property Value: string read FValue write FValue;
  end;

  TGrammarChar = class(TGrammarClassOp0)
  protected
    FValueFrom,FValueTo: Char;
    FCaseSensitive: boolean;

    function GetInfo: string; override;
  public
    constructor Create(ValueFrom,ValueTo: Char; CaseSensitive: boolean);

    { input accepted: return length of accepted block
      input rejected: -1}
    function GetAcceptedBlock(var Buffer: TBuffer): integer;

    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property ValueFrom: char read FValueFrom write FValueFrom;
    property ValueTo: char read FValueTo write FValueTo;
  end;

  TCharClass = (ccControl, ccDigit, ccLetter, ccLetterOrDigit, ccLower, ccPunctuation, ccSeparator, ccSymbol, ccUpper, ccWhiteSpace, ccAny);
  TGrammarCharClass = class(TGrammarClassOp0)
  protected
    FCharClass: TCharClass;

    function GetInfo: string; override;
  public
    constructor Create(ACharClass: TCharClass);

    { input accepted: return length of accepted block
      input rejected: -1}
    function GetAcceptedBlock(var Buffer: TBuffer): integer;

    property CharClass: TCharClass read FCharClass write FCharClass;
  end;

  TGrammarCharSetClass = class(TGrammarClassOp0)
  protected
    function GetInfo: string; override;
  public
    CharSet: TSet<Char>;

    constructor Create(const Chars: array of Char; CaseSensitive: boolean = False);

    { input accepted: return length of accepted block
      input rejected: -1}
    function GetAcceptedBlock(var Buffer: TBuffer): integer; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  TGrammarSequence = class(TGrammarClassOp2)
  protected
  public
    constructor Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
  end;

  TGrammarSelection = class(TGrammarClassOp2)
  protected
  public
    constructor Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
  end;

  TGrammarGreedyRepeater = class(TGrammarClassOp1)
  protected
    FMinCount, FMaxCount: integer;

    function GetInfo: string; override;
  public
    constructor Create(AOp: IInterfacedObject<TGrammarClass>; AMinCount,AMaxCount: integer);

    property MinCount: integer read FMinCount;
    property MaxCount: integer read FMaxCount;
  end;

  TGrammarNot = class(TGrammarClassOp1)
  protected
  public
    constructor Create(AOp: IInterfacedObject<TGrammarClass>);
  end;

  TGrammarEOF = class(TGrammarClassOp0)
  protected
  public
    constructor Create;
  end;

{ all possible expressions for TGrammar (used in right side of rule definition) }
function Ex(var AGrammar: TGrammar): TGrammar.TMedia; overload;
function Ex(Value: String; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function Ex(CharRangeFrom,CharRangeTo: Char; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function Ex(ACharClass: TCharClass): TGrammar.TMedia; overload;
function Ex(const Chars: array of Char; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
{function Ex(Value: TArray<Byte>): TGrammar.TMedia; overload;
function Ex(Value: array of Byte): TGrammar.TMedia; overload;
function Ex(Value: TEnumerable<Byte>): TGrammar.TMedia; overload;
function Ex(const ByteSet: TByteSet): TGrammar.TMedia; overload;
function Ex(CharSet: TSet<Char>): TGrammar.TMedia; overload;
function ExAnsi(const Value: AnsiString; ; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function ExAnsi(CharRangeFrom,CharRangeTo: AnsiChar; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function ExAnsi(CharSet: TSet<AnsiChar>): TGrammar.TMedia; overload;
}

procedure SetNames(const Rules: array of TGrammar; const Names: array of string);

function EOF: TGrammar.TMedia; overload;

{ all possible repeaters for TGrammar (used as multiplyer of expression in right side of rule definiton) }
function Rep(AMinCount,AMaxCount: integer): TGrammar.TRange; overload;
function Rep(AExactCount: integer): TGrammar.TRange; overload;
function Rep: TGrammar.TRange; overload;
function Rep1: TGrammar.TRange; overload;
function Opt: TGrammar.TRange; overload;

{ Should be called after initialization of all rules, but before main rule will be used.
  Fixes internal links etc. }
procedure SetMainRule(var Rule: TGrammar);

implementation

procedure SetMainRule(var Rule: TGrammar);
var
  Queue: TVector<TGrammarClass>;
  QueuedIds: TSet<int64>;
  Operands: TVector<IInterfacedObject<TGrammarClass>>;
  Item: TGrammarClass;
  I: integer;
begin
  Assert(Rule.Grm<>nil, 'rule is not initialized');
  Queue.Clear;
  Queue.Add(Rule.Def);
  QueuedIds.Clear;
  QueuedIds.Add(Rule.Def.Id);
  repeat

    { process next rule }
    Item := Queue.ExtractLast;
    if Item is TGrammarLink then
    begin
      Assert(TGrammarLink(Item).FLink.Grm<>nil, 'link is not initialized');
      if TGrammarLink(Item).Op=nil then
        TGrammarLink(Item).FOp := TGrammarLink(Item).FLink.Grm;
    end;

    { process operands of the rule }
    Operands.Clear;
    Item.GetOperands(Operands);
    for I := 0 to Operands.Count-1 do
    begin
      Assert(Operands[I]<>nil, 'Operand is not initialized');
      Item := Operands[I].Data;
      if Item.Id in QueuedIds then
        Continue;
      QueuedIds.Add(Item.Id);
      Queue.Add(Item);
    end;
  until Queue.Count=0;
end;

function Ex(var AGrammar: TGrammar): TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarLink.Create(AGrammar));
end;

function Ex(Value: String; CaseSensitive: boolean): TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarString.Create(Value, CaseSensitive));
end;

function Ex(CharRangeFrom,CharRangeTo: Char; CaseSensitive: boolean): TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarChar.Create(CharRangeFrom,CharRangeTo, CaseSensitive));
end;

function Ex(ACharClass: TCharClass): TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarCharClass.Create(ACharClass));
end;

function Ex(const Chars: array of Char; CaseSensitive: boolean = False): TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarCharSetClass.Create(Chars, CaseSensitive));
end;

{function Ex(Value: TArray<Byte>): TGrammar.TMedia;
begin
end;

function Ex(const ByteSet: TByteSet): TGrammar.TMedia;
begin

end;

function Ex(CharSet: TSet<Char>): TGrammar.TMedia;
begin

end;}

procedure SetNames(const Rules: array of TGrammar; const Names: array of string);
var
  I: Integer;
begin
  Assert(Length(Rules)=Length(Names));
  for I := Low(Rules) to High(Rules) do
    Rules[I].Def.Name := Names[I];
end;

function EOF: TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarEOF.Create);
end;

function Rep(AMinCount,AMaxCount: integer): TGrammar.TRange;
begin
  result.MinCount := AMinCount;
  if AMaxCount=integer(infinite) then
    result.MaxCount := High(AMaxCount)
  else
    result.MaxCount := AMaxCount;
end;

function Rep(AExactCount: integer): TGrammar.TRange;
begin
  result.MinCount := AExactCount;
  result.MaxCount := AExactCount;
end;

function Rep: TGrammar.TRange;
begin
  result.MinCount := 0;
  result.MaxCount := High(result.MaxCount);
end;

function Rep1: TGrammar.TRange;
begin
  result.MinCount := 1;
  result.MaxCount := High(result.MaxCount);
end;

function Opt: TGrammar.TRange;
begin
  result.MinCount := 0;
  result.MaxCount := 1;
end;

{ TGrammar }

function TGrammar.GetDef: TGrammarClass;
begin
  result := Grm.Data;
end;

function TGrammar.GetId: int64;
begin
  result := Grm.Data.Id;
end;

class operator TGrammar.Implicit(A: TMedia): TGrammar;
begin
  Result.Grm := A.MediaGrm;
  A.MediaGrm.Data.IncludeIntoParseTree := True;
end;

{ TMedia }

class operator TGrammar.TMedia.Add(A, B: TMedia): TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarSequence.Create(A.MediaGrm, B.MediaGrm));
end;

class operator TGrammar.TMedia.LogicalNot(A: TMedia): TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarNot.Create(A.MediaGrm));
end;

class operator TGrammar.TMedia.LogicalOr(A, B: TMedia): TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarSelection.Create(A.MediaGrm, B.MediaGrm));
end;

class operator TGrammar.TMedia.Multiply(A: TMedia; const R: TRange): TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarGreedyRepeater.Create(A.MediaGrm, R.MinCount,R.MaxCount));
end;

{ TGrammarClass }

{ TGrammarClassOp0 }

procedure TGrammarClassOp0.GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>);
begin
  { nothing to do here }
end;

procedure TGrammarClassOp0.Release;
begin
  { nothing to do here }
end;

{ TGrammarClassOp1 }

function TGrammarClassOp1.GetInfo: string;
begin
  result := inherited + ' ' + GetOperandInfo(FOp);
end;

procedure TGrammarClassOp1.GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>);
begin
  if FOp<>nil then
    Dst.Add(FOp);
end;

procedure TGrammarClassOp1.Release;
begin
  FOp := nil;
end;

{ TGrammarClassOp2 }

function TGrammarClassOp2.GetInfo: string;
begin
  result := inherited + ' ' + GetOperandInfo(FOp1) + ' ' + GetOperandInfo(FOp2);
end;

procedure TGrammarClassOp2.GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>);
begin
  if FOp1<>nil then
    Dst.Add(FOp1);
  if FOp2<>nil then
    Dst.Add(FOp2);
end;

procedure TGrammarClassOp2.Release;
begin
  FOp1 := nil;
  FOp2 := nil;
end;

{ TGrammarLink }

constructor TGrammarLink.Create(var ALink: TGrammar);
begin
  inherited Create(gtLink);
  FLink := @ALink;
end;

function TGrammarLink.GetInfo: string;
begin
  result := inherited + ' linked:' + FOp.Data.Info;
end;

procedure TGrammarLink.SetupRule;
begin
  inherited;
  Assert(FLink.Grm<>nil, 'link is not initialized');
  if Op=nil then
    FOp := FLink.Grm;
end;

{ TGrammarString }

constructor TGrammarString.Create(Value: String; CaseSensitive: boolean);
begin
  inherited Create(gtString);
  FValue := Value;
  FCaseSensitive := CaseSensitive;
end;

function TGrammarString.GetAcceptedBlock(var Buffer: TBuffer): integer;
begin

  { empty string is always matched }
  if Value='' then
    Exit(0);

  { not enough of data to match }
  Result := Length(Value)*SizeOf(Char);
  if Buffer.Left < Result then
    Exit(-1);

  { we have enough of data, it is safe to compare }
  if CaseSensitive then
    if not CompareMem(@Value[Low(Value)], Buffer.CurrentData, result) then
      result := -1
    else
  else
    if not TStr.SameText(@Value[Low(Value)], Buffer.CurrentData, Length(Value)) then
      result := -1;
end;

function TGrammarString.GetInfo: string;
begin
  result := inherited + Format(' Value:"%s", CaseSensitive:%s', [TStr.GetPrintable(FValue), TValueUtils.BoolToStr(FCaseSensitive)]);
end;

{ TGrammarChar }

constructor TGrammarChar.Create(ValueFrom, ValueTo: Char; CaseSensitive: boolean);
begin
  inherited Create(gtChar);
  FValueFrom := ValueFrom;
  FValueTo := ValueTo;
  FCaseSensitive := CaseSensitive;
end;

function TGrammarChar.GetAcceptedBlock(var Buffer: TBuffer): integer;
var
  C: Char;
begin

  { not enough of data to match }
  if Buffer.Left < SizeOf(Char) then
    Exit(-1);

  if CaseSensitive then
  begin
    C := Char(Buffer.CurrentData^);
    if (C >= FValueFrom) and (C <= FValueTo) then result := SizeOF(Char)
      else result := -1;
  end
  else
  begin
    C := TStr.LowerCaseChar(Char(Buffer.CurrentData^));
    if (C >= TStr.LowerCaseChar(FValueFrom)) and (C <= TStr.LowerCaseChar(FValueTo)) then result := SizeOF(Char)
      else result := -1;
  end;

end;

function TGrammarChar.GetInfo: string;
begin
  result := inherited + Format(' Value:"%s", CaseSensitive:%s', [
    TStr.GetPrintable(IfThen(FValueFrom=FValueTo, FValueFrom, '['+FValueFrom+'..'+FValueFrom+']')),
    TValueUtils.BoolToStr(FCaseSensitive)
  ]);
end;

{ TGrammarCharClass }

constructor TGrammarCharClass.Create(ACharClass: TCharClass);
begin
  inherited Create(gtCharClass);
  FCharClass := ACharClass;
end;

function TGrammarCharClass.GetAcceptedBlock(var Buffer: TBuffer): integer;
var
  C: Char;
begin
  { not enough of data to match }
  if Buffer.Left < SizeOf(Char) then
    Exit(-1);
  C := Char(Buffer.CurrentData^);
  case FCharClass of
    ccControl       : if C.IsControl       then result := SizeOf(Char) else result := -1;
    ccDigit         : if C.IsDigit         then result := SizeOf(Char) else result := -1;
    ccLetter        : if C.IsLetter        then result := SizeOf(Char) else result := -1;
    ccLetterOrDigit : if C.IsLetterOrDigit then result := SizeOf(Char) else result := -1;
    ccLower         : if C.IsLower         then result := SizeOf(Char) else result := -1;
    ccPunctuation   : if C.IsPunctuation   then result := SizeOf(Char) else result := -1;
    ccSeparator     : if C.IsSeparator     then result := SizeOf(Char) else result := -1;
    ccSymbol        : if C.IsSymbol        then result := SizeOf(Char) else result := -1;
    ccUpper         : if C.IsUpper         then result := SizeOf(Char) else result := -1;
    ccWhiteSpace    : if C.IsWhiteSpace    then result := SizeOf(Char) else result := -1;
    ccAny           : result := SizeOf(Char);
    else result := -1;
  end;
end;

function TGrammarCharClass.GetInfo: string;
begin
  result := inherited + Format(' CharClass:"%s"', [TEnumeration<TCharClass>.ToString(FCharClass)]);
end;

{ TGrammarCharSetClass }

constructor TGrammarCharSetClass.Create(const Chars: array of Char; CaseSensitive: boolean);
var C: Char;
begin
  inherited Create(gtCharSet);
  CharSet.Clear;
  if CaseSensitive then
    for C in Chars do
      CharSet.Add(C)
  else
    for C in Chars do
    begin
      CharSet.Add(C.ToLower);
      CharSet.Add(C.ToUpper);
    end;
end;

function TGrammarCharSetClass.GetAcceptedBlock(var Buffer: TBuffer): integer;
begin
  if (Buffer.Left >= SizeOf(Char)) and (Char(Buffer.CurrentData^) in CharSet) then
    Result := SizeOf(Char)
  else
    Result := -1;
end;

function TGrammarCharSetClass.GetInfo: string;
var
  C: Char;
begin
  result := '';
  for C in CharSet do
    if Length(Result)>=20 then begin
      result := result + ',...';
      break;
    end else
    if result='' then
      result := '[' + C
    else
      result := result + ',' + C;
  if result='' then result := '[]' else result := result + ']';
  result := inherited + Format(' CharSet: %s', [TStr.GetPrintable(result)]);
end;

{ TGrammarSequence }

constructor TGrammarSequence.Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
begin
  inherited Create(gtSequence);
  FOp1 := AOp1;
  FOp2 := AOp2;
end;

{ TGrammarSelection }

constructor TGrammarSelection.Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
begin
  inherited Create(gtSelection);
  FOp1 := AOp1;
  FOp2 := AOp2;
end;

{ TGrammarGreedyRepeater }

constructor TGrammarGreedyRepeater.Create(AOp: IInterfacedObject<TGrammarClass>; AMinCount, AMaxCount: integer);
begin
  inherited Create(gtRepeat);
  FOp := AOp;
  FMinCount := AMinCount;
  FMaxCount := AMaxCount;
end;

function TGrammarGreedyRepeater.GetInfo: string;
  function CntToStr(n: Integer): string;
  begin
    if n=High(n) then result := 'infinite' else result := IntToStr(n);
  end;
begin
  result := inherited + Format(' Repeat:%s', [
    IfThen(FMinCount=FMaxCount, IntToStr(FMinCount), '['+CntToStr(FMinCount)+'..'+CntToStr(FMaxCount)+']')
  ]);
end;

{ TGrammarNot }

constructor TGrammarNot.Create(AOp: IInterfacedObject<TGrammarClass>);
begin
  inherited Create(gtNot);
  FOp := AOp;
end;

{ TGrammarEOF }

constructor TGrammarEOF.Create;
begin
  inherited Create(gtEOF);
end;

end.
