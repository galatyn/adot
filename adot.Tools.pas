unit adot.Tools;
// compatible with all delphi platforms (Win x32/x64, Android, iOS)

interface

uses
  {$IFDEF CompilerVersion>=30} System.Hash, {$ENDIF}
  IdGlobal, System.Classes, IdHashMessageDigest, System.SysUtils,
  System.Variants, System.Generics.Collections, System.Generics.Defaults,
  System.StrUtils, System.Math, System.UITypes, System.Diagnostics,
  System.TimeSpan, System.Character, System.Types, System.SyncObjs,
  System.TypInfo, System.Rtti;

type
  TDelegatedOnComponentWithBreak = reference to procedure(AComponent: TComponent; var ABreak: boolean);
  TDelegatedOnComponent = reference to procedure(AComponent: TComponent);

procedure ForEachComponent(AStart: TComponent; ACallback: TDelegatedOnComponentWithBreak); overload;
procedure ForEachComponent(AStart: TComponent; ACallback: TDelegatedOnComponent); overload;

type
  TDelegatedMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(var Buf; Size: NativeInt);
  end;

  THex = class
  public
    const
      B2HConvert = '0123456789ABCDEF';
      // System.SysUtils.TwoHexLookup is hidden behind implementation section,
      // so we have to reintroduce it.
      TwoHexLookup : packed array[0..255] of array[0..1] of Char =
      ('00','01','02','03','04','05','06','07','08','09','0A','0B','0C','0D','0E','0F',
       '10','11','12','13','14','15','16','17','18','19','1A','1B','1C','1D','1E','1F',
       '20','21','22','23','24','25','26','27','28','29','2A','2B','2C','2D','2E','2F',
       '30','31','32','33','34','35','36','37','38','39','3A','3B','3C','3D','3E','3F',
       '40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
       '50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
       '60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
       '70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
       '80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
       '90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
       'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
       'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
       'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
       'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
       'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
       'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','FA','FB','FC','FD','FE','FF');
      H2B: packed array['0'..'f'] of SmallInt =
        ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
         -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
         -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
         -1,10,11,12,13,14,15);

    class procedure Encode(const Buf; ByteBufSize: integer; Dst: PChar); overload; static;
    class function Encode(const Buf; ByteBufSize: integer): String; overload; static;
    class function Encode<T: Record>(const Value: T): String; overload; static;
    class function Encode(const s: TIdBytes):String; overload; static;
    class function Encode(const s: TBytes):String; overload; static;
    class function Encode(const s: string):String; overload; static;

    class procedure Decode(const HexEncodedStr: String; var Buf); overload; static;
    class function Decode<T: Record>(const HexEncodedStr: String): T; overload; static;
    class function DecodeIdBytes(const HexEncodedStr: String):TIdBytes; static;
    class function DecodeBytes(const HexEncodedStr: String):TBytes; static;
    class function DecodeString(const HexEncodedStr: String):String; static;

    class function Valid(const HexEncodedStr: String):Boolean; static;

    // Int64ToHex(Value) <> Encode(Value, SizeOf(Value)) for x86-compatible CPU family,
    // because lower bytes of integers are stored by lower addresses. When we translate
    // integer/pointer to hex we would like to use regular notation, when higher digits
    // are shown first.
    class function Int64ToHex(s: Int64): string; static;
    class function UInt64ToHex(s: UInt64): string; static; inline;
    class function NativeIntToHex(s: NativeInt): string; static;
    class function NativeUIntToHex(s: NativeUInt): string; static; inline;
    class function PointerToHex(s: Pointer): string; static;

    class function HexToInt64(const HexEncodedInt: String):Int64; static;
    class function HexToUInt64(const HexEncodedInt: String):UInt64; static; inline;
    class function HexToNativeInt(const HexEncodedInt: String):NativeInt; static;
    class function HexToNativeUInt(const HexEncodedInt: String):NativeUInt; static; inline;
    class function HexToPointer(const HexEncodedPointer: String):Pointer; static; inline;
  end;

  TCryptoHash = class
  public
    type
      TValue = TIdBytes;

    class function Encode(const Buf; ByteBufSize: integer): TValue; overload; static;
    class function Encode<T: Record>(const Value: T): TValue; overload; static; inline;
    class function Encode(const s: TIdBytes): TValue; overload; static;
    class function Encode(const s: TBytes): TValue; overload; static; inline;
    class function Encode(const s: string): TValue; overload; static;
    class function Encode(s: TStream; AOwnsStream: boolean = False): TValue; overload; static;
  end;

  TFastHash = class
  public
    type
      TValue = Integer;

    class function Encode(const Buf; ByteBufSize: integer): TValue; overload; static; inline;
    class function Encode<T: Record>(const Value: T): TValue; overload; static; inline;
    class function Encode(const s: TIdBytes): TValue; overload; static; inline;
    class function Encode(const s: TBytes): TValue; overload; static; inline;
    class function Encode(const s: string): TValue; overload; static; inline;
    class function Encode(s: TStream; AOwnsStream: boolean = False): TValue; overload; static;
  end;

  TVar = class
  public
    class function AsString(const v: Variant): String; static;
  end;

  TNumbers = class
  public
    class function IntToStr(const Value: int64; const Digits: integer): String; static;
  end;

  TColors = class
  public
    type
      TDistanceType = (dtMaxComponentDeviation, dtStandardDeviation);
      TColorClass = (
        ccBlack, ccGray,      ccSilver,  ccWhite,
        ccRed,   ccOrange,    ccYellow,  ccSpringGreen,
        ccGreen, ccTurquoise, ccCyan,    ccOcean,
        ccBlue,  ccViolet,    ccMagenta, ccRaspberry);
    const
      TDefDist = dtMaxComponentDeviation;
      ColorValues: array[TColorClass] of TColor = (
        TColorRec.Black, TColorRec.Gray,   TColorRec.Silver,  TColorRec.White,
        TColorRec.Red,   TColor($007DFF),  TColorRec.Yellow,  TColor($00FF7D),
        TColorRec.Green, TColor($7DFF00),  TColorRec.Cyan,    TColor($FF7D00),
        TColorRec.Blue,  TColorRec.Violet, TColorRec.Magenta, TColor($7D00FF)
      );
      ColorNames: array[TColorClass] of String = (
        'Black', 'Gray',      'Silver',  'White',
        'Red',   'Orange',    'Yellow',  'SpringGreen',
        'Green', 'Turquoise', 'Cyan',    'Ocean',
        'Blue',  'Violet',    'Magenta', 'Raspberry'
      );
  public
    class function Distance(A,B: TColor; ADistType: TDistanceType = TDefDist):Integer; static;
    class function Spot(ASample: TColor; const AColors: array of TColor;
      ADistType: TDistanceType = TDefDist): Integer; overload; static;
    class function Spot(ASample: TColor; ADistType: TDistanceType = TDefDist): TColorClass; overload; static;
    class function GetName(C: TColor; ADistType: TDistanceType = TDefDist): String; static;
    class function GetBaseColorName(C: TColor; ADistType: TDistanceType = TDefDist): String; static;
  end;

  // assign Value:=nil to free content immediately.
  TAutoFree<T: class> = record
  private
    type
      TAutoFreeImpl = class(TInterfacedObject, IUnknown)
      protected
        FObject: TObject;
      public
        constructor Create(AObject: TObject);
        destructor Destroy; override;
      end;

    var
      FValue: T;
      FGuard: IUnknown;

    procedure SetValue(const Value: T);
    function GetIsLink: boolean;
    procedure SetAsLink(const Value: T);
  public
    class function Create: TAutoFree<T>; overload; static;
    class function Create(const AValue: T): TAutoFree<T>; overload; static;
    class operator Equal(const ALeft, ARight: TAutoFree<T>): Boolean;
    class operator Equal(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
    class operator NotEqual(const ALeft, ARight: TAutoFree<T>): Boolean;
    class operator NotEqual(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
    class operator Implicit(const AValue: TAutoFree<T>): T;
    class operator Implicit(const AValue: T): TAutoFree<T>;

    property Value: T read FValue write SetValue;
    property AsLink: T read FValue write SetAsLink;
    property IsLink: boolean read GetIsLink;
  end;

  TNullable<T> = record
  private
    type
      PT = ^T;
    var
      FValue: T;
      FHasValue: string;

    function GetValue: T;
    procedure SetValue(const AValue: T);
    function GetIsNull: boolean;
    procedure SetIsNull(const AIsNull: boolean);
    function GetHasValue: boolean;
    procedure SetHasValue(const AHasValue: boolean);
    function GetPointer: pointer;
  public
    class function Create: TNullable<T>; overload; static;
    class function Create(const AValue: T): TNullable<T>; overload; static;
    class operator Equal(const ALeft, ARight: TNullable<T>): Boolean;
    class operator Equal(const ALeft: TNullable<T>; const ARight: T): Boolean;
    class operator NotEqual(const ALeft, ARight: TNullable<T>): Boolean;
    class operator NotEqual(const ALeft: TNullable<T>; const ARight: T): Boolean;
    class operator Implicit(const AValue: TNullable<T>): T;
    class operator Implicit(const AValue: T): TNullable<T>;
    class operator Implicit(const AValue: PT): TNullable<T>;
    class operator Implicit(const AValue: Variant): TNullable<T>;

    property Value: T read GetValue write SetValue;
    property IsNull: boolean read GetIsNull write SetIsNull;
    property HasValue: boolean read GetHasValue write SetHasValue; // not IsNull
    property Ptr: pointer read GetPointer;
  end;

  TThreadSafe<T: record> = record
  public
    type
      TCopyProc = reference to procedure(const ASrc: T; var Dst: T);
  private
    var
      FValue: T;
      FCriticalSection: TAutoFree<TCriticalSection>;
      FComparer: IEqualityComparer<T>;
      FCopyProc: TCopyProc;

    function GetCriticalSection: TCriticalSection;
    function GetComparer: IEqualityComparer<T>;
    function GetValue: T;
    procedure SetValue(const AValue: T);
    procedure SetComparer(const Value: IEqualityComparer<T>);
    procedure SetCopyProc(const Value: TCopyProc);
  public
    class function Create: TThreadSafe<T>; overload; static;
    class function Create(const AValue: T): TThreadSafe<T>; overload; static;
    class operator Equal(const ALeft, ARight: TThreadSafe<T>): Boolean;
    class operator Equal(const ALeft: TThreadSafe<T>; const ARight: T): Boolean;
    class operator NotEqual(const ALeft, ARight: TThreadSafe<T>): Boolean;
    class operator NotEqual(const ALeft: TThreadSafe<T>; const ARight: T): Boolean;
    class operator Implicit(const AValue: TThreadSafe<T>): T;
    class operator Implicit(const AValue: T): TThreadSafe<T>;
    class operator Implicit(const AValue: Variant): TThreadSafe<T>;

    property Value: T read GetValue write SetValue;
    property Comparer: IEqualityComparer<T> read GetComparer write SetComparer;
    property CopyProc: TCopyProc read FCopyProc write SetCopyProc;
    property CriticalSection: TCriticalSection read GetCriticalSection;
  end;

  TTiming = class
  protected
    class var
      FTimeStack: TStack<TStopwatch>;

    class function GetTimeStack: TStack<TStopwatch>; static;
    class procedure Finilaze; static;

    class property TimeStack: TStack<TStopwatch> read GetTimeStack;
  public
    class procedure Start; static;
    class function Stop: TTimeSpan; static;
  end;

  TDelegatedOnFile = reference to procedure(const APath: string; const AFile: TSearchRec; var ABreak: Boolean);
  TFileTools = class
  public
    class procedure EnumFiles(const AFileNamePattern: string; AOnfile: TDelegatedOnFile); static;
    class procedure CleanUpOldFiles(const AFileNamePattern: string; const AMaxAllowedTotalSize, AMaxAllowedCount: int64;
      AChanceToRun: Double = 1); static;
  end;

  // PDF-compatible RLE codec
  TRLE = class
  public
    class function MaxEncodedSize(ASrcSize: cardinal): cardinal;
    class function Encode(const ASrc; ASrcSize: cardinal; var ADest): cardinal;

    class function DecodedSize(const ASrc; APackedSize: cardinal): cardinal;
    class procedure Decode(const ASrc; APackedSize: cardinal; var ADest; AUnpackedSize: cardinal);
  end;

  // Simple convertion EnumType->string->EnumType etc.
  // http://stackoverflow.com/questions/31601707/generic-functions-for-converting-an-enumeration-to-string-and-back#31604647
  TEnumeration<T: record> = record
  strict private
    class function TypeInfo: PTypeInfo; inline; static;
    class function TypeData: PTypeData; inline; static;
  public
    class function IsEnumeration: Boolean; static;
    class function ToOrdinal(Enum: T): Integer; inline; static;
    class function FromOrdinal(Value: Integer): T; inline; static;
    class function ToString(Enum: T): string; inline; static;
    class function FromString(const S: string): T; inline; static;
    class function MinValue: Integer; inline; static;
    class function MaxValue: Integer; inline; static;
    class function InRange(Value: Integer): Boolean; inline; static;
    class function EnsureRange(Value: Integer): Integer; inline; static;
  end;

implementation

function ForEachComponentBrk(AStart: TComponent; ACallback: TDelegatedOnComponentWithBreak):Boolean;
var
  i: Integer;
begin
  result := False; // break
  ACallback(AStart, result);
  if not result then
    for i := AStart.ComponentCount-1 downto 0 do
      if ForEachComponentBrk(AStart.Components[i], ACallback) then
      begin
        result := True;
        break;
      end;
end;

procedure ForEachComponent(AStart: TComponent; ACallback: TDelegatedOnComponentWithBreak);
begin
  ForEachComponentBrk(AStart, ACallback);
end;

procedure ForEachComponent(AStart: TComponent; ACallback: TDelegatedOnComponent);
var
  i: Integer;
begin
  if AStart=nil then
    exit;
  ACallback(AStart);
  for i := AStart.ComponentCount-1 downto 0 do
    ForEachComponent(AStart.Components[i], ACallback);
end;

{ TDelegatedMemoryStream }

constructor TDelegatedMemoryStream.Create(var Buf; Size: NativeInt);
begin
  SetPointer(@Buf, Size);
end;

{ THex }

class function THex.Encode(const Buf; ByteBufSize: integer): String;
var
  I: Integer;
begin
  setlength(result, ByteBufSize*2);
  for I := 0  to ByteBufSize - 1 do
  begin
    Result[Low(Result) + I * 2]     := B2HConvert[Low(B2HConvert) + PByteArray(@Buf)[I] shr 4];
    Result[Low(Result) + I * 2 + 1] := B2HConvert[Low(B2HConvert) + PByteArray(@Buf)[I] and $0F];
  end;
end;

class procedure THex.Encode(const Buf; ByteBufSize: integer; Dst: PChar);
var
  I: Integer;
begin
  for I := 0  to ByteBufSize - 1 do
  begin
    Dst[I * 2]     := B2HConvert[Low(B2HConvert) + PByteArray(@Buf)[I] shr 4];
    Dst[I * 2 + 1] := B2HConvert[Low(B2HConvert) + PByteArray(@Buf)[I] and $0F];
  end;
end;

class function THex.Encode<T>(const Value: T): String;
begin
  Result := Encode(Value, SizeOf(Value));
end;

class function THex.HexToInt64(const HexEncodedInt: String):Int64;
var
  i: Integer;
begin
  assert(Valid(HexEncodedInt));
  result := 0;
  for i := Low(HexEncodedInt) to High(HexEncodedInt) do
    result := (result shl 4) or H2B[HexEncodedInt[i]];
end;

class function THex.HexToUInt64(const HexEncodedInt: String):UInt64;
begin
  result := UInt64(HexToInt64(HexEncodedInt));
end;

class function THex.HexToNativeInt(const HexEncodedInt: String):NativeInt;
var
  i: Integer;
begin
  assert(Valid(HexEncodedInt));
  result := 0;
  for i := Low(HexEncodedInt) to High(HexEncodedInt) do
    result := (result shl 4) or H2B[HexEncodedInt[i]];
end;

class function THex.HexToNativeUInt(const HexEncodedInt: String):NativeUInt;
begin
  result := NativeUInt(HexToNativeInt(HexEncodedInt));
end;

class function THex.HexToPointer(const HexEncodedPointer: String): Pointer;
begin
  result := Pointer(HexToNativeUInt(HexEncodedPointer));
end;

class function THex.Int64ToHex(s: Int64): string;
var
  i,j: Integer;
begin
  setlength(result, SizeOf(s)*2);
  for i := SizeOf(s)-1 downto 0 do
  begin
    J := S and $FF;
    S := S shr 8;
    Result[Low(Result) + i*2    ] := TwoHexLookup[J][0];
    Result[Low(Result) + i*2 + 1] := TwoHexLookup[J][1];
  end;
end;

class function THex.UInt64ToHex(s: UInt64): string;
begin
  result := Int64ToHex(Int64(s));
end;

class function THex.NativeIntToHex(s: NativeInt): string;
var
  i,j: Integer;
begin
  setlength(result, SizeOf(s)*2);
  for i := SizeOf(s)-1 downto 0 do
  begin
    J := S and $FF;
    S := S shr 8;
    Result[Low(Result) + i*2    ] := TwoHexLookup[J][0];
    Result[Low(Result) + i*2 + 1] := TwoHexLookup[J][1];
  end;
end;

class function THex.NativeUIntToHex(s: NativeUInt): string;
begin
  result := NativeIntToHex(NativeInt(s));
end;

class function THex.PointerToHex(s: Pointer): string;
begin
  result := NativeIntToHex(NativeInt(s));
end;

class function THex.Encode(const s: TIdBytes): String;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function THex.Encode(const s: TBytes): String;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function THex.Encode(const s: string): String;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function THex.Valid(const HexEncodedStr: String): Boolean;
var
  i: Integer;
  c: Char;
begin
  for i := Low(HexEncodedStr) to High(HexEncodedStr) do
  begin
    c := HexEncodedStr[i];
    if (c<Low(H2B)) or (c>High(H2B)) or (H2B[c]<0) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

class procedure THex.Decode(const HexEncodedStr: String; var Buf);
var
  I: Integer;
  C1,C2: Char;
begin
  Assert(length(HexEncodedStr) and 1=0);
  for I := 0 to Length(HexEncodedStr) shr 1 - 1 do
  begin
    C1 := HexEncodedStr[Low(HexEncodedStr) + I * 2];
    C2 := HexEncodedStr[Low(HexEncodedStr) + I * 2 + 1];
    if (C1>=Low(H2B)) and (C1<=High(H2B)) and (H2B[C1]>=0) and
       (C2>=Low(H2B)) and (C2<=High(H2B)) and (H2B[C2]>=0)
    then
      PByteArray(@Buf)[I] := (H2B[C1] shl 4) or H2B[C2]
    else
      Abort;
  end;
end;

class function THex.Decode<T>(const HexEncodedStr: String): T;
begin
  Decode(HexEncodedStr, Result);
end;

class function THex.DecodeIdBytes(const HexEncodedStr: String): TIdBytes;
begin
  Assert(length(HexEncodedStr) mod 2=0);
  setlength(result, length(HexEncodedStr) div 2);
  if Length(Result)>0 then
    Decode(HexEncodedStr, Result[Low(Result)]);
end;

class function THex.DecodeBytes(const HexEncodedStr: String): TBytes;
begin
  Assert(length(HexEncodedStr) mod 2=0);
  setlength(result, length(HexEncodedStr) div 2);
  if Length(Result)>0 then
    Decode(HexEncodedStr, Result[Low(Result)]);
end;

class function THex.DecodeString(const HexEncodedStr: String): String;
begin
  Assert(length(HexEncodedStr) mod 2=0);
  setlength(result, (length(HexEncodedStr) div 2) div SizeOf(result[Low(result)]));
  if Length(Result)>0 then
    Decode(HexEncodedStr, result[Low(result)]);
end;

{ TVar }

class function TVar.AsString(const v: Variant): String;
begin
  if VarIsClear(v) or VarIsNull(v) then
    Result := ''
  else
    Result := v;
end;

{ THash }

class function TCryptoHash.Encode(const Buf; ByteBufSize: integer): TValue;
var
  h: TIdHashMessageDigest5;
  s: TDelegatedMemoryStream;
begin
  h := nil;
  s := nil;
  try
    h := TIdHashMessageDigest5.Create;
    s := TDelegatedMemoryStream.Create((@Buf)^, ByteBufSize);
    result := h.HashStream(s);
  finally
    FreeAndNil(s);
    FreeAndNil(h);
  end;
end;

class function TCryptoHash.Encode(const s: TIdBytes): TValue;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function TCryptoHash.Encode(const s: TBytes): TValue;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function TCryptoHash.Encode(s: TStream; AOwnsStream: boolean = False): TValue;
var
  h: TIdHashMessageDigest5;
  p: Int64;
begin
  p := s.Position;
  try
    s.Position := 0;
    h := TIdHashMessageDigest5.Create;
    try
      result := h.HashStream(s);
    finally
      FreeAndNil(h);
    end;
  finally
    if AOwnsStream then
      s.Free
    else
      s.Position := p;
  end;
end;

class function TCryptoHash.Encode(const s: string): TValue;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function TCryptoHash.Encode<T>(const Value: T): TValue;
begin
  Result := Encode(Value, SizeOf(Value));
end;

{ TFastHash }

class function TFastHash.Encode(const Buf; ByteBufSize: integer): TValue;
begin
  {$IFDEF CompilerVersion>=30}
  result := System.Hash.THashBobJenkins.GetHashValue(Buf, ByteBufSize, 0);
  {$ELSE}
  result := BobJenkinsHash(Buf, ByteBufSize, 0);
  {$ENDIF}
end;

class function TFastHash.Encode(const s: TIdBytes): TValue;
begin
  result := Encode(s[Low(s)], Length(s));
end;

class function TFastHash.Encode(const s: TBytes): TValue;
begin
  result := Encode(s[Low(s)], Length(s));
end;

class function TFastHash.Encode(const s: string): TValue;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function TFastHash.Encode<T>(const Value: T): TValue;
begin
  Result := Encode(Value, SizeOf(Value));
end;

class function TFastHash.Encode(s: TStream; AOwnsStream: boolean): TValue;
var
  b: TBytes;
  p: Int64;
begin
  result := 0;
  p := s.Position;
  try
    s.Position := 0;
    setlength(b, s.Size);
    if s.Read(b[low(b)], length(b))<>length(b) then
      raise Exception.Create('Error');
    result := Encode(b);
  finally
    if AOwnsStream then
      s.Free
    else
      s.Position := p;
  end;
end;

{ TColors }

class function TColors.Distance(A, B: TColor; ADistType: TDistanceType = TDefDist): Integer;
var
  CA,CB: Longint;
begin
  CA := System.UITypes.TColors.ColorToRGB(A);
  CB := System.UITypes.TColors.ColorToRGB(B);
  case ADistType of
    dtMaxComponentDeviation:
      Result := Max(
        Max(
          Abs(((CA shr  8) and $FF)-((CB shr  8) and $FF)),
          Abs(((CA shr 16) and $FF)-((CB shr 16) and $FF))
        ),
        Abs((CA and $FF)-(CB and $FF))
      );
    dtStandardDeviation:
      Result := (
        Sqr((CA and $FF)-(CB and $FF)) +
        Sqr(((CA shr  8) and $FF)-((CB shr  8) and $FF)) +
        Sqr(((CA shr 16) and $FF)-((CB shr 16) and $FF))
      ) div 3;
    else
      result := High(Result);
  end;
end;

class function TColors.Spot(ASample: TColor; const AColors: array of TColor;
  ADistType: TDistanceType = TDefDist): Integer;
var
  Dist: Integer;
  i,j: Integer;
begin
  Dist := High(Integer);
  Result := -1;
  for i := Low(AColors) to High(AColors) do
  begin
    j := Distance(ASample, AColors[i], ADistType);
    if j<Dist then
    begin
      Dist := j;
      Result := i;
    end;
  end;
end;

class function TColors.Spot(ASample: TColor; ADistType: TDistanceType = TDefDist): TColorClass;
begin
  Result := TColorClass(Spot(ASample, ColorValues, ADistType));
end;

class function TColors.GetName(C: TColor; ADistType: TDistanceType = TDefDist): String;
begin
  Result := ColorNames[Spot(C, ADistType)];
end;

class function TColors.GetBaseColorName(C: TColor;
  ADistType: TDistanceType): String;
begin
  case Spot(C, [
    TColorRec.Red,    TColorRec.Green, TColorRec.Blue,  TColorRec.SkyBlue,
    TColorRec.Yellow, TColorRec.Black, TColorRec.White, TColorRec.Gray,
    TColorRec.Silver
  ]) of
    0: Result := ColorNames[ccRed];
    1: Result := ColorNames[ccGreen];
    2: Result := ColorNames[ccBlue];
    3: Result := ColorNames[ccBlue];
    4: Result := ColorNames[ccYellow];
    5: Result := ColorNames[ccBlack];
    6: Result := ColorNames[ccWhite];
    7: Result := ColorNames[ccGray];
    8: Result := ColorNames[ccSilver];
    else result := '';
  end;
end;

{ TAutofree<T>.TAutoFreeImpl }

constructor TAutofree<T>.TAutoFreeImpl.Create(AObject: TObject);
begin
  FObject := AObject;
end;

destructor TAutofree<T>.TAutoFreeImpl.Destroy;
begin
  FreeAndNil(FObject);
  inherited;
end;

{ TAutofree<T> }

class function TAutoFree<T>.Create: TAutoFree<T>;
begin
  result.FValue := nil;
  result.FGuard := nil;
end;

class function TAutoFree<T>.Create(const AValue: T): TAutoFree<T>;
begin
  result.Value := AValue;
end;

procedure TAutofree<T>.SetValue(const Value: T);
begin
  if FValue = Value then
    Exit;
  FValue := Value;
  FGuard := TAutoFreeImpl.Create(FValue);
end;

procedure TAutoFree<T>.SetAsLink(const Value: T);
begin
  FGuard := nil;
  FValue := Value;
end;

class operator TAutoFree<T>.Equal(const ALeft, ARight: TAutoFree<T>): Boolean;
begin
  result := ALeft.Value=ARight.Value;
end;

class operator TAutoFree<T>.Equal(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
begin
  result := ALeft.Value=ARight;
end;

function TAutoFree<T>.GetIsLink: boolean;
begin
  result := (FGuard=nil) and (FValue<>nil);
end;

class operator TAutoFree<T>.Implicit(const AValue: TAutoFree<T>): T;
begin
  result := AValue.Value;
end;

class operator TAutoFree<T>.Implicit(const AValue: T): TAutoFree<T>;
begin
  result.Value := AValue;
end;

class operator TAutoFree<T>.NotEqual(const ALeft, ARight: TAutoFree<T>): Boolean;
begin
  result := ALeft.Value<>ARight.Value;
end;

class operator TAutoFree<T>.NotEqual(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
begin
  result := ALeft.Value<>ARight;
end;

{ TTiming }

class procedure TTiming.Finilaze;
begin
  FreeAndNil(FTimeStack);
end;

class function TTiming.GetTimeStack: TStack<TStopwatch>;
begin
  if FTimeStack=nil then
    FTimeStack := TStack<TStopwatch>.Create;
  result := FTimeStack;
end;

class procedure TTiming.Start;
begin
  TimeStack.Push(TStopwatch.StartNew);
end;

class function TTiming.Stop: TTimeSpan;
begin
  Result := TimeStack.Pop.Elapsed;
end;

{ TFileTools }

class procedure TFileTools.CleanUpOldFiles(const AFileNamePattern: string;
  const AMaxAllowedTotalSize, AMaxAllowedCount: int64; AChanceToRun: Double = 1);
type
  TFileInfo = record
    Name: string;
    Size: int64;
    Age: TDateTime;
  end;
var
  List: TList<TFileInfo>;
  TotalSize: Int64;
  TotalCount: Integer;
  FilePath: string;
  i: Integer;
begin
  if (Random>AChanceToRun) or (AMaxAllowedTotalSize<0) and (AMaxAllowedCount<0) then
    Exit;
  List := TList<TFileInfo>.Create;
  try
    FilePath := ExtractFilePath(AFileNamePattern);
    EnumFiles(AFileNamePattern,
      procedure(const APath: string; const AFile: TSearchRec; var ABreak: Boolean)
      var R: TFileInfo;
      begin
        R.Name := AFile.Name;
        R.Size := AFile.Size;
        R.Age  := AFile.TimeStamp;
        List.Add(R);
      end);
    List.Sort(TDelegatedComparer<TFileInfo>.Create(
      function(const A,B: TFileInfo): Integer
      begin
        result := Sign(A.Age-B.Age);
      end));
    TotalCount := List.Count;
    TotalSize := 0;
    for i := 0 to List.Count-1 do
      inc(TotalSize, List[i].Size);
    for i := 0 to List.Count-1 do
      if not (
        (AMaxAllowedTotalSize>=0) and (TotalSize>AMaxAllowedTotalSize) or
        (AMaxAllowedCount>=0) and (TotalCount>AMaxAllowedCount)
      ) then
        Break
      else
        if System.SysUtils.DeleteFile(FilePath+List[i].Name) then
        begin
          Dec(TotalSize, List[i].Size);
          Dec(TotalCount);
        end;
  finally
    FreeAndNil(List);
  end;
end;

class procedure TFileTools.EnumFiles(const AFileNamePattern: string;
  AOnfile: TDelegatedOnFile);
var
  F: System.SysUtils.TSearchRec;
  B: Boolean;
  P: string;
begin
  if System.SysUtils.FindFirst(AFileNamePattern, faAnyfile, F)=0 then
    try
      P := ExtractFilePath(AFileNamePattern);
      B := False;
      repeat
        if (F.Attr and faDirectory<>0) then
          Continue;
        AOnFile(P, F, B);
      until B or (System.SysUtils.FindNext(F)<>0);
    finally
      System.SysUtils.FindClose(F);
    end;
end;

{ TNumbers }

class function TNumbers.IntToStr(const Value: int64; const Digits: integer): String;
begin
  result := System.SysUtils.IntToStr(Value);
  if length(result)<Digits then
    result := StringOfChar('0',Digits-length(result)) + result;
end;

(*
function murmur3_32(key: PByte; Len: Longword; k: Longword {seed}): Longword;
type
  TLongword = array[0..$1FFFFFF0] of Longword;
  PLongword = ^TLongword;
const
	c1: Longword = $cc9e2d51;
	c2: Longword = $1b873593;
	r1: Longword = 15;
	r2: Longword = 13;
	m : Longword = 5;
	n : Longword = $e6546b64;
var
  i: integer;
begin
  Result := k;

  // main part
	for i := 0 to len div 4-1 do
  begin
		k := PLongword(Key)[i];
		k := k*c1;
		k := (k shl r1) or (k shr (32 - r1));
		k := k*c2;
		Result := Result xor k;
		Result := ((Result shl r2) or (Result shr (32 - r2))) * m + n;
	end;

  // tail
	key := (key + (len and not 3));
	k := 0;
	case (len and 3) of
	  1:
      begin
        k := k xor key[0];
      end;
	  2:
      begin
        k := k xor (key[1] shl 8);
        k := k xor key[0];
      end;
	  3:
      begin
        k := k xor (key[2] shl 16);
        k := k xor (key[1] shl 8);
        k := k xor key[0];
      end;
	end;
  k := k * c1;
  k := (k shl r1) or (k shr (32 - r1));
  k := k * c2;
  Result := Result xor k;

  // final mix
	Result := Result xor len;
	Result := Result xor (Result shr 16);
	Result := Result * Longword($85ebca6b);
	Result := Result xor (Result shr 13);
	Result := Result * Longword($c2b2ae35);
	Result := Result xor (Result shr 16);
End;
*)

{ TRLE }

class function TRLE.MaxEncodedSize(ASrcSize: cardinal): cardinal;
begin
  result := ASrcSize + ASrcSize div 64 + 16;
end;

(*
  RunLengthEncoding according to PDF specification. Stream of bytes encoded
  as stream of packets. The packet contains ocntrol byte (length) and
  optional string of bytes. Allowed values for control byte:
    0..127: next N+1 bytes (1..128) should be writtens as is
       128: EOF (decoding finished)
  129..255: next byte should be repeated 257-N times (2..128)

  At every step we have current state described in following variables:
  S[1.2...N][1.2..M]
  N-number of uncompressable bytes
  M-number of items in sequance of equal (potentially compressable)
*)
class function TRLE.Encode(const ASrc; ASrcSize: cardinal; var ADest): cardinal;
var
  Src, Dst: PByte;
  M,N: cardinal;
  PrevChar: byte;

  // 0 1 [2 3 4] [5 6] {7}
  //       N=3    M=2 CurPos
  procedure COPY_N;
  begin
    assert((N>=1) and (N<=128));
    inc(result,N+1);
    Dst^ := N-1; // copy N (1..128) bytes as is
    inc(Dst);
    system.move((Src-M-N)^, Dst^, N);
    inc(Dst, N);
  end;

  // M=0!
  procedure COPY_128;
  begin
    assert(N>=128);
    inc(result,129);
    Dst^ := 127; // copy N (1..128) bytes as is
    inc(Dst);
    system.move((Src-N)^, Dst^, 128);
    inc(Dst, 128);
    dec(N, 128);
  end;

  // 257-x=M -> x=257-M
  procedure PACK_M;
  begin
    assert((M>=2) and (M<=128));
    inc(result,2);
    dst^ := 257-M; // copy folowing byte 257-x times
    inc(dst);
    dst^ := PrevChar;
    inc(dst);
  end;

  function Init:longword;
  begin
    //ADest := AllocMem(ASrcSize + ASrcSize div 64 + 16);
    Src := @ASrc;
    Dst := @ADest;
    result := 0;

    // too short chain
    if ASrcSize<3 then
    begin
      result := ASrcSize;
      if result>0 then
      begin
        inc(result);              // + 1 byte command
        dst^ := ASrcSize-1;   // command (copy ASourceCount bytes)
        inc(dst);
        system.move(Src^, Dst^, ASrcSize);
        inc(dst, ASrcSize);
      end;

      // end of data
      inc(result);
      dst^ := 128;
      inc(dst);
      exit;
    end;
  end;

  procedure Finish;
  begin

    // short repeated chain we join to "as is" chain (M must be greater than 1)
    if M<2 then
    begin
      inc(N, M);
      if N>128 then
        COPY_128;
      M := 0;
    end;

    // out "as is" chain
    if N>0 then
    begin
      COPY_N;
      N := 0;
    end;

    // out "repeated" chain
    if M>0 then
    begin
      PACK_M;
      M := 0;
    end;

    // out "end of data" marker
    inc(result);
    dst^ := 128;
    inc(dst);
  end;

begin

  // initialize
  result := Init;
  if result>0 then
    exit;

  // read src
  N := 0;
  M := 0;
  repeat
    if M=0 then
    begin
      PrevChar := Src^;
      inc(M);
    end
    else
    begin
      if Src^=PrevChar then
        inc(M);

      // two folowing conditions never met at same time
      if (Src^<>PrevChar) or (M=128) then
        if N>0 then
          if M>=4 then
            if M=128 then
            begin // N>0, M=128, Src^=PrevChar
              inc(Src);
              COPY_N;
              PACK_M;
              dec(Src);
              N := 0;
              M := 0;
            end
            else
            begin // N>0, M=[4..127], Src^<>PrevChar
              COPY_N;
              PACK_M;
              N := 0;
              M := 1;
              PrevChar := Src^;
            end
          else
          begin // N>0, M<4, Src^<>PrevChar
            inc(N,M);
            if N>128 then
              COPY_128;
            M := 1;
            PrevChar := Src^;
          end
        else
          if M>=3 then
          begin
            if M=128 then
            begin // N=0, M=128, Src^=PrevChar
              inc(Src);
              PACK_M;
              dec(Src);
              M := 0;
            end
            else
            begin // N=0, M=[3..127], Src^<>PrevChar
              PACK_M;
              M := 1;
              PrevChar := Src^;
            end
          end
          else
          begin // N=0, M=[1..2], Src^<>PrevChar
            N := M;
            M := 1;
            PrevChar := Src^;
          end
    end;
    dec(ASrcSize);
    inc(Src);
  until ASrcSize=0;

  // finish uncompleted chain
  finish;
//  assert(result<(ASrcSize + ASrcSize div 64 + 16));
end;

class function TRLE.DecodedSize(const ASrc; APackedSize: cardinal): cardinal;
var
  src: PByte;
  n: byte;
begin
  Src := @ASrc;
  result := 0;
  while APackedSize>0 do
  begin
    n := Src^;
    inc(Src);
    dec(APackedSize);

    // copy
    if n<128 then
    begin
      inc(n);
      if APackedSize<n then
        raise exception.create('RLDecode error');
      inc(result, n);
      inc(Src, n);
      dec(APackedSize, n);
      continue;
    end;

    // stop
    if n=128 then
      break;

    // repeat
    n := 257-n;
    if APackedSize<1 then
      raise exception.create('RLDecode error');
    inc(result, n);
    inc(Src);
    dec(APackedSize);
  end;
end;

class procedure TRLE.Decode(const ASrc; APackedSize: cardinal; var ADest; AUnpackedSize: cardinal);
var
  src,dst: PByte;
  n: byte;
begin
  Src := @ASrc;
  Dst := @ADest;
  while APackedSize>0 do
  begin
    n := Src^;
    inc(Src);
    dec(APackedSize);

    // copy
    if n<128 then
    begin
      inc(n);
      if (APackedSize<n) or (AUnpackedSize<n) then
        raise exception.create('RLDecode error');
      system.move(Src^, dst^, n);
      dec(AUnpackedSize, n);
      inc(Src, n);
      dec(APackedSize, n);
      inc(dst, n);
      continue;
    end;

    // stop
    if n=128 then
      break;

    // repeat
    n := 257-n;
    if (APackedSize<1) or (AUnpackedSize<1) then
      raise exception.create('RLDecode error');
    FillChar(dst^, n, Src^);
    dec(AUnpackedSize, n);
    inc(Src);
    dec(APackedSize);
    inc(dst, n);
  end;
  if (AUnpackedSize<>0) then
    raise exception.create('RLDecode error');
end;

{ TEnumeration<T> }

class function TEnumeration<T>.TypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(T);
end;

class function TEnumeration<T>.TypeData: PTypeData;
begin
  Result := System.TypInfo.GetTypeData(TypeInfo);
end;

class function TEnumeration<T>.IsEnumeration: Boolean;
begin
  Result := TypeInfo.Kind=tkEnumeration;
end;

class function TEnumeration<T>.ToOrdinal(Enum: T): Integer;
begin
  Assert(IsEnumeration);
  Assert(SizeOf(Enum)<=SizeOf(Result));
  Result := 0; // needed when SizeOf(Enum) < SizeOf(Result)
  Move(Enum, Result, SizeOf(Enum));
  Assert(InRange(Result));
end;

class function TEnumeration<T>.FromOrdinal(Value: Integer): T;
begin
  Assert(IsEnumeration);
  Assert(InRange(Value));
  Assert(SizeOf(Result)<=SizeOf(Value));
  Move(Value, Result, SizeOf(Result));
end;

class function TEnumeration<T>.ToString(Enum: T): string;
begin
  Result := GetEnumName(TypeInfo, ToOrdinal(Enum));
end;

class function TEnumeration<T>.FromString(const S: string): T;
begin
  Result := FromOrdinal(GetEnumValue(TypeInfo, S));
end;

class function TEnumeration<T>.MinValue: Integer;
begin
  Assert(IsEnumeration);
  Result := TypeData.MinValue;
end;

class function TEnumeration<T>.MaxValue: Integer;
begin
  Assert(IsEnumeration);
  Result := TypeData.MaxValue;
end;

class function TEnumeration<T>.InRange(Value: Integer): Boolean;
var
  ptd: PTypeData;
begin
  Assert(IsEnumeration);
  ptd := TypeData;
  Result := System.Math.InRange(Value, ptd.MinValue, ptd.MaxValue);
end;

class function TEnumeration<T>.EnsureRange(Value: Integer): Integer;
var
  ptd: PTypeData;
begin
  Assert(IsEnumeration);
  ptd := TypeData;
  Result := System.Math.EnsureRange(Value, ptd.MinValue, ptd.MaxValue);
end;

{ TNullable<T> }

class function TNullable<T>.Create: TNullable<T>;
begin
  result.FValue := Default(T);
  result.FHasValue := '';
end;

class function TNullable<T>.Create(const AValue: T): TNullable<T>;
begin
  result.FValue := AValue;
  result.FHasValue := '1';
end;

function TNullable<T>.GetValue: T;
begin
  if IsNull then
    raise EInvalidOperation.Create('Var is NULL');
  result := FValue;
end;

procedure TNullable<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
  IsNull := False;
end;

function TNullable<T>.GetIsNull: boolean;
begin
  result := FHasValue='';
end;

function TNullable<T>.GetPointer: pointer;
begin
  result := @FValue;
end;

procedure TNullable<T>.SetIsNull(const AIsNull: boolean);
begin
  if AIsNull then
  begin
    FHasValue := '';
    FValue := Default(T);
  end
  else
    FHasValue := '1';
end;

function TNullable<T>.GetHasValue: boolean;
begin
  result := not IsNull;
end;

procedure TNullable<T>.SetHasValue(const AHasValue: boolean);
begin
  IsNull := not AHasValue;
end;

class operator TNullable<T>.Equal(const ALeft, ARight: TNullable<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.HasValue and ARight.HasValue then
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := Comparer.Equals(ALeft.Value, ARight.Value);
  end
  else
    Result := ALeft.HasValue = ARight.HasValue;
end;

class operator TNullable<T>.Equal(const ALeft: TNullable<T>; const ARight: T): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if ALeft.IsNull then
    result := False
  else
  begin
    Comparer := TEqualityComparer<T>.Default;
    Result := Comparer.Equals(ALeft.Value, ARight);
  end
end;

class operator TNullable<T>.NotEqual(const ALeft, ARight: TNullable<T>): Boolean;
begin
  result := not (ALeft=ARight);
end;

class operator TNullable<T>.NotEqual(const ALeft: TNullable<T>; const ARight: T): Boolean;
begin
  result := not (ALeft=ARight);
end;

class operator TNullable<T>.Implicit(const AValue: T): TNullable<T>;
begin
  result := TNullable<T>.Create(AValue);
end;

class operator TNullable<T>.Implicit(const AValue: TNullable<T>): T;
begin
  result := AValue.Value;
end;

class operator TNullable<T>.Implicit(const AValue: Variant): TNullable<T>;
begin
  if VarIsClear(AValue) then
    result := TNullable<T>.Create
  else
    Result := TNullable<T>.Create( TValue.FromVariant(AValue).AsType<T> );
end;

class operator TNullable<T>.Implicit(const AValue: PT): TNullable<T>;
begin
  if AValue=nil then
    result := TNullable<T>.Create
  else
    result := TNullable<T>.Create(AValue^);
end;

{ TThreadSafe<T> }

class function TThreadSafe<T>.Create: TThreadSafe<T>;
begin
  result := Create(Default(T));
end;

class function TThreadSafe<T>.Create(const AValue: T): TThreadSafe<T>;
begin
  result.FValue := AValue;
  result.FCriticalSection.Value := TCriticalSection.Create;
end;

function TThreadSafe<T>.GetComparer: IEqualityComparer<T>;
begin
  CriticalSection.Acquire;
  try
    if FComparer=nil then
      FComparer := TEqualityComparer<T>.Default;
    result := FComparer;
  finally
    CriticalSection.Leave;
  end;
end;

function TThreadSafe<T>.GetCriticalSection: TCriticalSection;
begin
  result := FCriticalSection.Value;
end;

class operator TThreadSafe<T>.Equal(const ALeft,
  ARight: TThreadSafe<T>): Boolean;
begin
  Result := ALeft=ARight.Value;
end;

class operator TThreadSafe<T>.Equal(const ALeft: TThreadSafe<T>;
  const ARight: T): Boolean;
begin
  ALeft.CriticalSection.Acquire;
  try
    Result := ALeft.Comparer.Equals(ALeft.Value, ARight);
  finally
    ALeft.CriticalSection.Leave;
  end;
end;

function TThreadSafe<T>.GetValue: T;
begin
  CriticalSection.Acquire;
  try
    result := FValue;
  finally
    CriticalSection.Leave;
  end;
end;

procedure TThreadSafe<T>.SetComparer(const Value: IEqualityComparer<T>);
begin
  CriticalSection.Acquire;
  try
    FComparer := Value;
  finally
    CriticalSection.Leave;
  end;
end;

procedure TThreadSafe<T>.SetCopyProc(const Value: TCopyProc);
begin
  CriticalSection.Acquire;
  try
    FCopyProc := Value;
  finally
    CriticalSection.Leave;
  end;
end;

procedure TThreadSafe<T>.SetValue(const AValue: T);
begin
  CriticalSection.Acquire;
  try
    FValue := AValue;
  finally
    CriticalSection.Leave;
  end;
end;

class operator TThreadSafe<T>.Implicit(const AValue: TThreadSafe<T>): T;
begin
  result := AValue.Value;
end;

class operator TThreadSafe<T>.Implicit(const AValue: Variant): TThreadSafe<T>;
begin
  if VarIsClear(AValue) then
    result := TThreadSafe<T>.Create
  else
    Result := TThreadSafe<T>.Create( TValue.FromVariant(AValue).AsType<T> );
end;

class operator TThreadSafe<T>.Implicit(const AValue: T): TThreadSafe<T>;
begin
  result.Value := AValue;
end;

class operator TThreadSafe<T>.NotEqual(const ALeft,
  ARight: TThreadSafe<T>): Boolean;
begin
  result := not (ALeft=ARight);
end;

class operator TThreadSafe<T>.NotEqual(const ALeft: TThreadSafe<T>;
  const ARight: T): Boolean;
begin
  result := not (ALeft=ARight);
end;

initialization

finalization
  TTiming.Finilaze;

end.

