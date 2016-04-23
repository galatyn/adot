unit adot.Grammar;

{ Uniform grammar presentation classes }

interface

uses
  adot.Types,
  adot.Collections,
  adot.Tools,
  adot.Strings,
  adot.Grammar.Types,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.Character;

type
  TByteSet = set of byte;

  TGrammarType = (gtUnknown, gtContainer, gtString, gtChar, gtSequence, gtSelection, gtRepeat, gtNot, gtEOF);

  TCustomGrammar = class abstract(TGrammarClass)
  protected
    class var
      FIdCnt: int64;
    var
      FGrammarType: TGrammarType;
      FId: int64;

    function GetId: int64; override;
  public
    constructor Create(AGrammarType: TGrammarType);

    property GrammarType: TGrammarType read FGrammarType;
  end;

  { abstract class for expression with no operands }
  TGrammarClassOp0 = class abstract(TCustomGrammar)
  protected
    FOp: IInterfacedObject<TGrammarClass>;

  public
    procedure Release; override;
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); override;
  end;

  { abstract class for expression with one operand }
  TGrammarClassOp1 = class abstract(TCustomGrammar)
  protected
    FOp: IInterfacedObject<TGrammarClass>;

  public
    procedure Release; override;
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); override;

    property Op: IInterfacedObject<TGrammarClass> read FOp;
  end;

  { abstract class for expression with two operand }
  TGrammarClassOp2 = class abstract(TCustomGrammar)
  protected
    FOp1: IInterfacedObject<TGrammarClass>;
    FOp2: IInterfacedObject<TGrammarClass>;

  public
    procedure Release; override;
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); override;

    property Op1: IInterfacedObject<TGrammarClass> read FOp1;
    property Op2: IInterfacedObject<TGrammarClass> read FOp2;
  end;

  { Used as container for rules in case when link to the rule is initialized before the rule is defined
    (actual grammar rule will be store as inner) }
  TGrammarContainer = class(TGrammarClassOp1)
  public
    constructor Create;
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;
  end;

  TGrammarString = class(TGrammarClassOp0)
  protected
    FValue: String;
    FCaseSensitive: boolean;
  public
    constructor Create(Value: String; CaseSensitive: boolean);
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;

    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property Value: string read FValue write FValue;
  end;

  TGrammarChar = class(TGrammarClassOp0)
  protected
    FValueFrom,FValueTo: Char;
    FCaseSensitive: boolean;
  public
    constructor Create(ValueFrom,ValueTo: Char; CaseSensitive: boolean);
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;

    property CaseSensitive: boolean read FCaseSensitive write FCaseSensitive;
    property ValueFrom: char read FValueFrom write FValueFrom;
    property ValueTo: char read FValueTo write FValueTo;
  end;

  TGrammarSequence = class(TGrammarClassOp2)
  protected
  public
    constructor Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;
  end;

  TGrammarSelection = class(TGrammarClassOp2)
  protected
  public
    constructor Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;
  end;

  TGrammarGreedyRepeater = class(TGrammarClassOp1)
  protected
    FMinCount, FMaxCount: integer;
  public
    constructor Create(AOp: IInterfacedObject<TGrammarClass>; AMinCount,AMaxCount: integer);
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;

    property MinCount: integer read FMinCount;
    property MaxCount: integer read FMaxCount;
  end;

  TGrammarNot = class(TGrammarClassOp1)
  protected
  public
    constructor Create(AOp: IInterfacedObject<TGrammarClass>);
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;
  end;

  TGrammarEOF = class(TGrammarClassOp0)
  protected
  public
    constructor Create;
    function Accepted(var Input: TBuffer; var P: TPos): Boolean; override;
  end;

  { Record type for grammar definition. Construct grammar rules with help of Ex/Rep functions:
    var
      Number,Digit: TGrammar;
    begin
      Number := Ex(Digit) + Ex(Digit)*Rep;
      Digit  := Ex('1') or Ex('2');
    end; }
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
      FGrm: IInterfacedObject<TGrammarClass>;
  public
    class operator Implicit(A : TMedia) : TGrammar;
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

implementation

function Ex(var AGrammar: TGrammar): TGrammar.TMedia;
begin
  { If we use link to undefined (yet) rule, we create proxy
    container which must be set up later. }
  if AGrammar.FGrm=nil then
    AGrammar.FGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarContainer.Create);
  result.MediaGrm := AGrammar.FGrm;
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

class operator TGrammar.Implicit(A: TMedia): TGrammar;
begin
  { Sometime TGrammar is touched before we assign it (by link from definition of another rule),
    in this case there will be TGrammarContainer and we should replace content of container. }
  if (Result.FGrm=nil) or not (Result.FGrm.Data is TGrammarContainer) then
    Result.FGrm := A.MediaGrm
  else
    TGrammarContainer(Result.FGrm.Data).FOp := A.MediaGrm;
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

constructor TCustomGrammar.Create(AGrammarType: TGrammarType);
begin
  FGrammarType := AGrammarType;
  inc(FIdCnt);
  FId := FIdCnt;
end;

function TCustomGrammar.GetId: int64;
begin
  result := FId;
end;

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

{ TGrammarContainer }

function TGrammarContainer.Accepted(var Input: TBuffer; var P: TPos): Boolean;
begin
  result := Op.Data.Accepted(Input, P);
end;

constructor TGrammarContainer.Create;
begin
  inherited Create(gtContainer);
end;

{ TGrammarString }

constructor TGrammarString.Create(Value: String; CaseSensitive: boolean);
begin
  inherited Create(gtString);
  FValue := Value;
  FCaseSensitive := CaseSensitive;
end;

function TGrammarString.Accepted(var Input: TBuffer; var P: TPos): Boolean;
var
  LenBytes: integer;
begin

  { empty string is always matched }
  if Value='' then
  begin
    P.SetPos(Input.Position, 0);
    Exit(True);
  end;

  { not enough of data to match }
  LenBytes := Length(Value)*SizeOf(Char);
  if Input.Left < LenBytes then
    Exit(False);

  { we have enough of data, it is safe to compare }
  if CaseSensitive then
    result := CompareMem(@Value[Low(Value)], Input.CurrentData, LenBytes)
  else
    result := TStr.SameText(@Value[Low(Value)], Input.CurrentData, Length(Value));
  if result then
    P.SetPos(Input.Position, LenBytes);
end;

{ TGrammarChar }

constructor TGrammarChar.Create(ValueFrom, ValueTo: Char; CaseSensitive: boolean);
begin
  inherited Create(gtChar);
  FValueFrom := ValueFrom;
  FValueTo := ValueTo;
  FCaseSensitive := CaseSensitive;
end;

function TGrammarChar.Accepted(var Input: TBuffer; var P: TPos): Boolean;
var
  C: Char;
begin

  { not enough of data to match }
  if Input.Left < SizeOf(Char) then
    Exit(False);

  if CaseSensitive then
  begin
    C := Char(Input.CurrentData^);
    result := (C >= FValueFrom) and (C <= FValueTo);
  end
  else
  begin
    C := TStr.LowerCaseChar(Char(Input.CurrentData^));
    result := (C >= TStr.LowerCaseChar(FValueFrom)) and (C <= TStr.LowerCaseChar(FValueTo));
  end;
  if result then
    P.SetPos(Input.Position, SizeOf(Char));

end;

{ TGrammarSequence }

constructor TGrammarSequence.Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
begin
  inherited Create(gtSequence);
  FOp1 := AOp1;
  FOp2 := AOp2;
end;

function TGrammarSequence.Accepted(var Input: TBuffer; var P: TPos): Boolean;
var
  P1, P2: TPos;
  Position: integer;
begin
  result := Op1.Data.Accepted(Input, P1);
  if not result then
    Exit;
  Position := Input.Position;
  Input.Position := Input.Position + P1.Len;
  result := Op2.Data.Accepted(Input, P2);
  Input.Position := Position;
  if result then
    P.SetPos(P1.Start, P1.Len + P2.Len);
end;

{ TGrammarSelection }

constructor TGrammarSelection.Create(AOp1, AOp2: IInterfacedObject<TGrammarClass>);
begin
  inherited Create(gtSelection);
  FOp1 := AOp1;
  FOp2 := AOp2;
end;

function TGrammarSelection.Accepted(var Input: TBuffer; var P: TPos): Boolean;
begin
  result := Op1.Data.Accepted(Input, P) or Op2.Data.Accepted(Input, P);
end;

{ TGrammarGreedyRepeater }

function TGrammarGreedyRepeater.Accepted(var Input: TBuffer; var P: TPos): Boolean;
var
  I,J,Position: Integer;
  CurPos: TPos;
begin
  Position := Input.Position;
  CurPos.SetPos(Position, 0);
  J := 0;
  for I := 0 to MaxCount-1 do
    if not Op.Data.Accepted(Input, CurPos) then
      Break
    else
    begin
      inc(J);
      Input.Position := CurPos.Start + CurPos.Len;
    end;
  Input.Position := Position;
  result := (J >= MinCount) and (J <= MaxCount);
  { CurPos keeps last matched position or [Position;0] if there is no matches }
  if result then
    P.SetPos(Position, CurPos.Start + CurPos.Len - P.Start);
end;

constructor TGrammarGreedyRepeater.Create(AOp: IInterfacedObject<TGrammarClass>; AMinCount, AMaxCount: integer);
begin
  inherited Create(gtRepeat);
  FOp := AOp;
  FMinCount := AMinCount;
  FMaxCount := AMaxCount;
end;

{ TGrammarNot }

constructor TGrammarNot.Create(AOp: IInterfacedObject<TGrammarClass>);
begin
  inherited Create(gtNot);
  FOp := AOp;
end;

function TGrammarNot.Accepted(var Input: TBuffer; var P: TPos): Boolean;
var CurPos: TPos;
begin
  result := not Op.Data.Accepted(Input, CurPos);
  if result then
    P.SetPos(Input.Position, 0);
end;

{ TGrammarEOF }

constructor TGrammarEOF.Create;
begin
  inherited Create(gtEOF);
end;

function TGrammarEOF.Accepted(var Input: TBuffer; var P: TPos): Boolean;
begin
  result := Input.EOF;
  if result then
    P.SetPos(Input.Position, 0);
end;

end.
