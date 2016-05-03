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

  public
    class operator Implicit(A : TMedia) : TGrammar;

    property Def: TGrammarClass read GetDef;
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
{function Ex(Value: TArray<Byte>): TGrammar.TMedia; overload;
function Ex(Value: array of Byte): TGrammar.TMedia; overload;
function Ex(Value: TEnumerable<Byte>): TGrammar.TMedia; overload;
function Ex(const ByteSet: TByteSet): TGrammar.TMedia; overload;
function Ex(CharSet: TSet<Char>): TGrammar.TMedia; overload;
function ExAnsi(const Value: AnsiString; ; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function ExAnsi(CharRangeFrom,CharRangeTo: AnsiChar; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function ExAnsi(CharSet: TSet<AnsiChar>): TGrammar.TMedia; overload;
}

function EOF: TGrammar.TMedia; overload;

{ all possible repeaters for TGrammar (used as multiplyer of expression in right side of rule definiton) }
function Rep(AMinCount,AMaxCount: integer): TGrammar.TRange; overload;
function Rep(AExactCount: integer): TGrammar.TRange; overload;
function Rep: TGrammar.TRange; overload;
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

{function Ex(Value: TArray<Byte>): TGrammar.TMedia;
begin
end;

function Ex(const ByteSet: TByteSet): TGrammar.TMedia;
begin

end;

function Ex(CharSet: TSet<Char>): TGrammar.TMedia;
begin

end;}

function EOF: TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarEOF.Create);
end;

function Rep(AMinCount,AMaxCount: integer): TGrammar.TRange;
begin
  result.MinCount := AMinCount;
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
  result := inherited + Format(' Value:"%s", CaseSensitive:%s', [TStr.GetReadable(FValue), TValueUtils.BoolToStr(FCaseSensitive)]);
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
    TStr.GetReadable(IfThen(FValueFrom=FValueTo, FValueFrom, '['+FValueFrom+'..'+FValueFrom+']')),
    TValueUtils.BoolToStr(FCaseSensitive)
  ]);
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
