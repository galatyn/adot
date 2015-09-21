unit CrossPlatform.Tools;
// compatible with all delphi platforms (Win x32/x64, Android, iOS)

interface

uses
  IdGlobal, System.Classes, IdHashMessageDigest, System.SysUtils,
  System.Variants, System.Generics.Collections, System.Generics.Defaults,
  System.StrUtils, System.Math, System.UITypes, System.Diagnostics,
  System.TimeSpan, System.Character, System.Types, System.SyncObjs,
  System.TypInfo;

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

  THash = class
  public
    class function Encode(const Buf; ByteBufSize: integer): TIdBytes; overload; static;
    class function Encode<T: Record>(const Value: T): TIdBytes; overload; static;
    class function Encode(s: TIdBytes): TIdBytes; overload; static;
    class function Encode(s: TBytes): TIdBytes; overload; static;
    class function Encode(const s: string): TIdBytes; overload; static;
    class function Encode(s: TStream): TIdBytes; overload; static;
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
    procedure SetAsLink(const Value: T);
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
  public
    property Value: T read FValue write SetValue;
    property AsLink: T read FValue write SetAsLink;
    property IsLink: boolean read GetIsLink;
  end;

  TEmptyRec = record end;

  TSet<TValue> = class
  private
  protected
    FSet: TDictionary<TValue, TEmptyRec>;

    function GetCount: integer; inline;
  public
    constructor Create; overload;
    constructor Create(const AValues: array of TValue); overload;
    constructor Create(const AValues: TEnumerable<TValue>); overload;
    destructor Destroy; override;
    procedure Add(const AValue: TValue); overload; inline;
    procedure Add(const ASet: TSet<TValue>); overload;
    procedure Add(const ASet: array of TValue); overload;
    procedure Add(const AValues: TEnumerable<TValue>); overload;
    procedure Include(const AValue: TValue); overload; inline;
    procedure Include(const ASet: TSet<TValue>); overload;
    procedure Include(const ASet: array of TValue); overload;
    procedure Include(const AValues: TEnumerable<TValue>); overload;
    procedure Remove(const AValue: TValue); overload; inline;
    procedure Remove(const ASet: TSet<TValue>); overload;
    procedure Remove(const ASet: array of TValue); overload;
    procedure Remove(const AValues: TEnumerable<TValue>); overload;
    function Contains(const AValue: TValue): boolean; overload; inline;
    function Contains(const ASet: array of TValue): boolean; overload;
    function Contains(const AValues: TEnumerable<TValue>): boolean; overload;
    procedure Clear; inline;
    property Count: integer read GetCount;
  end;

  TMultimap<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  protected
    type
      TMultimapKey = record
        Key: TKey;
        Number: integer;
      end;

      // TKey can be String for example, so we can't use default comparer for
      // TMultimapKey record type, we have to implement specific one.
      TMultimapKeyEqualityComparer = class(TEqualityComparer<TMultimapKey>)
      private
        FKeyComparer: IEqualityComparer<TKey>;
      public
        constructor Create(AKeyComparer: IEqualityComparer<TKey>);
        function Equals(const Left, Right: TMultimapKey): Boolean; overload; override;
        function GetHashCode(const Value: TMultimapKey): Integer; overload; override;
      end;

  public
    type
      TValueEnumerator = record
      private
        FMultimap: TMultimap<TKey,TValue>;
        FMultimapKey: TMultimapKey;

        constructor Create(AMultimap: TMultimap<TKey,TValue>; const AKey: TKey);
        function GetCurrent: TValue;
        function GetKey: TKey;
      public
        function MoveNext: Boolean;
        property Current: TValue read GetCurrent;
        property Key: TKey read GetKey;
      end;

      TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
      private
        FMultimap: TMultimap<TKey,TValue>;
        FCurrentKey: TDictionary<TKey, integer>.TKeyEnumerator;
        FInEnumKey: boolean;
        FCurrentValue: TValueEnumerator;

        function GetCurrent: TPair<TKey,TValue>;
      protected
        function DoGetCurrent: TPair<TKey,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMultimap: TMultimap<TKey,TValue>);
        destructor Destroy; override;
        property Current: TPair<TKey,TValue> read GetCurrent;
        function MoveNext: Boolean;
      end;

      TKeyEnumerator = TDictionary<TKey, integer>.TKeyEnumerator;
      TKeyCollection = TDictionary<TKey, integer>.TKeyCollection;

  private
    function GetKeys: TKeyCollection;

  protected
    FCount: TDictionary<TKey, integer>;
    FValues: TDictionary<TMultimapKey, TValue>;
    FKeyCollection: TKeyCollection;

    function GetTotalValuesCount: integer;
    function GetValuesCount(const AKey: TKey):Integer;
    function GetValuesEnumerator(const AKey: TKey): TValueEnumerator;
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;

  public

    constructor Create; overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const AKey: TKey; const AValues: array of TValue); overload;
    procedure Add(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    function Remove(const AKey: TKey):Boolean;
    procedure RemoveValue(const AEnum: TValueEnumerator);
    procedure RemoveValues(const AKey: TKey; const AValues: TSet<TValue>); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: array of TValue); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsKeys(const AKeys: array of TKey): Boolean;
    function GetEnumerator: TPairEnumerator; reintroduce;

    property TotalValuesCount: integer read GetTotalValuesCount;
    property ValuesCount[const AKey: TKey]:integer read GetValuesCount;
    property Values[const AKey: TKey]: TValueEnumerator read GetValuesEnumerator; default;
    property Keys: TKeyCollection read GetKeys;
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

  // Search words inside of text (to extract words/numbers etc)
  PTextWords = ^TTextWords;
  TTextWords = record
  public
    type
      TIsAlphaPredicate = reference to function(const C: Char):Boolean;
      TWordPosRec = record
        Start, Len: Integer;
      end;
    var
      Text: String;
      Position: integer;
      IsAlphaChar: TIsAlphaPredicate;

    // set Text and IsAlpha predicate (default is IsLetterOrDigit)
    function Prepare(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate = nil): PTextWords; overload;
    // reset current position (to start search from begin of the text)
    procedure Reset; inline;
    // reset and get number of words in the text
    function Count:Integer;
    // find next word in the text
    function FindNext(var AStart,ALen: integer):Boolean; overload;
    function FindNext(var AWord: String):Boolean; overload;
    function FindNext: String; overload; inline;
    // reset and get all words from the text
    procedure Get(ADst: TStrings); overload;
    procedure Get(var ADst: TArray<String>); overload;
    procedure Get(var ADst: TArray<TWordPosRec>); overload;
    // get substring from the text (usually APos is result of FindNext or Get)
    function GetSubStr(const APos: TWordPosRec): String;
    // find sequence of strings in the list of words:
    //    r.Prepare('it is test').Get(SearchStrArr);
    //    r.Prepare('word0 word1 it is test xxx xxx xxx').Get(WordList);
    //    Assert(r.Find(SearchStrArr, WordList) = 2);
    //    Assert(r.Find(['it', 'is', 'test'], WordList) = 2);
    function Find(const ASubSequence: array of string; const ASequence: TArray<TWordPosRec>): integer;

    // most common TIsAlphaPredicate functions
    class function IsNonSpace(const C: Char): Boolean; static;
    class function IsLetter(const C: Char): Boolean; static;
    class function IsDigit(const C: Char): Boolean; static;
    class function IsLetterOrDigit(const C: Char): Boolean; static;

    class function StrToPrintable(const S: string): string; static;
  end;

  // PDF-compatible RLE codec
  TRLE = class
  public
    class function MaxEncodedSize(ASrcSize: cardinal): cardinal;
    class function Encode(const ASrc; ASrcSize: cardinal; var ADest): cardinal;

    class function DecodedSize(const ASrc; APackedSize: cardinal): cardinal;
    class procedure Decode(const ASrc; APackedSize: cardinal; var ADest; AUnpackedSize: cardinal);
  end;

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

{$IFDEF QuickTest}
uses
  CrossPlatform.Tools_Test;
{$ENDIF}

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

class function THash.Encode(const Buf; ByteBufSize: integer): TIdBytes;
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

class function THash.Encode(s: TIdBytes): TIdBytes;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function THash.Encode(s: TBytes): TIdBytes;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function THash.Encode(s: TStream): TIdBytes;
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
    s.Position := p;
  end;
end;

class function THash.Encode(const s: string): TIdBytes;
begin
  result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function THash.Encode<T>(const Value: T): TIdBytes;
begin
  Result := Encode(Value, SizeOf(Value));
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

procedure TAutofree<T>.SetValue(const Value: T);
begin
  if FValue = Value then
    Exit;
  FValue := Value;
  FGuard := TAutoFreeImpl.Create(FValue);
end;

procedure TAutoFree<T>.SetAsLink(const Value: T);
begin
  FValue := Value;
  FGuard := nil;
end;

function TAutoFree<T>.GetIsLink: boolean;
begin
  result := (FGuard=nil) and (FValue<>nil);
end;

{ TSet<TValue> }

constructor TSet<TValue>.Create;
begin
  FSet := TDictionary<TValue, TEmptyRec>.Create;
end;

destructor TSet<TValue>.Destroy;
begin
  FreeAndNil(FSet);
  inherited;
end;

function TSet<TValue>.GetCount: integer;
begin
  result := FSet.Count;
end;

procedure TSet<TValue>.Remove(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Remove(ASet[i]);
end;

procedure TSet<TValue>.Remove(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Remove(Item);
end;

procedure TSet<TValue>.Remove(const ASet: TSet<TValue>);
var
  Value: TValue;
begin
  for Value in ASet.FSet.Keys do
    Remove(Value);
end;

procedure TSet<TValue>.Add(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.Add(AValue, R);
end;

procedure TSet<TValue>.Add(const ASet: TSet<TValue>);
var
  Value: TValue;
  R: TEmptyRec;
begin
  for Value in ASet.FSet.Keys do
    FSet.AddOrSetValue(Value, R);
end;

procedure TSet<TValue>.Add(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Add(ASet[i]);
end;

procedure TSet<TValue>.Add(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Add(Item);
end;

procedure TSet<TValue>.Include(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.AddOrSetValue(AValue, R);
end;

procedure TSet<TValue>.Include(const ASet: TSet<TValue>);
var
  Value: TValue;
begin
  for Value in ASet.FSet.Keys do
    Include(Value);
end;

procedure TSet<TValue>.Include(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Include(ASet[i]);
end;

procedure TSet<TValue>.Include(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Include(Item);
end;

procedure TSet<TValue>.Clear;
begin
  FSet.Clear;
end;

function TSet<TValue>.Contains(const AValue: TValue): boolean;
begin
  result := FSet.ContainsKey(AValue);
end;

function TSet<TValue>.Contains(const ASet: array of TValue): boolean;
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    if not Contains(ASet[i]) then
      Exit(False);
  result := True;
end;

function TSet<TValue>.Contains(const AValues: TEnumerable<TValue>): boolean;
var
  Item: TValue;
begin
  for Item in AValues do
    if not Contains(Item) then
      Exit(False);
  result := True;
end;

constructor TSet<TValue>.Create(const AValues: array of TValue);
begin
  Create;
  Add(AValues);
end;

constructor TSet<TValue>.Create(const AValues: TEnumerable<TValue>);
begin
  Create;
  Add(AValues);
end;

procedure TSet<TValue>.Remove(const AValue: TValue);
begin
  FSet.Remove(AValue);
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

{ TTextWords }

function TTextWords.Prepare(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate = nil): PTextWords;
begin
  result := @Self;
  Text := AText;
  if Assigned(AIsAlphaPredicate) then
    IsAlphaChar := AIsAlphaPredicate
  else
    IsAlphaChar := IsLetterOrDigit;
  Reset;
end;

procedure TTextWords.Reset;
begin
  Position := Low(Text);
end;

class function TTextWords.StrToPrintable(const S: string): string;
var
  i: Integer;
begin
  for i := Low(s) to High(s) do
    if (s[i]=' ') or IsLetterOrDigit(s[i]) then
      result := result + s[i]
    else
      result := result + '#$'+THex.Encode(s[i], SizeOf(Char));
end;

function TTextWords.Count: Integer;
var
  Start, Len: Integer;
begin
  Reset;
  Result := 0;
  while FindNext(Start, Len) do
    Inc(Result);
end;

function TTextWords.FindNext(var AStart, ALen: integer): Boolean;
var
  i, Finish: integer;
begin
  AStart := -1;
  for i := Position to Length(Text) do
    if IsAlphaChar(Text[i]) then
    begin
      AStart := i;
      Break;
    end;
  Result := AStart>=Low(Text);
  if not Result then
  begin
    ALen := 0;
    Position := High(Text)+1;
    Exit;
  end;
  Finish := High(Text);
  for i := AStart+1 to High(Text) do
    if not IsAlphaChar(Text[i]) then
    begin
      Finish := i-1;
      Break;
    end;
  Position := Finish+1;
  ALen := Finish-AStart+1;
end;

function TTextWords.FindNext(var AWord: String): Boolean;
var
  Start, Len: integer;
begin
  Result := FindNext(Start, Len);
  if Result then
    AWord := Copy(Text, Start, Len)
  else
    AWord := '';
end;

function TTextWords.FindNext: String;
begin
  FindNext(Result);
end;

function TTextWords.Find(const ASubSequence: array of string;
  const ASequence: TArray<TWordPosRec>): integer;
var
  i,j: Integer;
  b: Boolean;
begin
  for i := 0 to High(ASequence)-Length(ASubSequence)+1 do
  begin
    b := True;
    for j := 0 to High(ASubSequence) do
      if not AnsiSameText(GetSubStr(ASequence[i+j]), ASubSequence[j]) then
      begin
        b := False;
        break;
      end;
    if b then
    begin
      result := i;
      Exit;
    end;
  end;
  result := -1;
end;

procedure TTextWords.Get(ADst: TStrings);
var
  w: string;
begin
  Reset;
  while FindNext(w) do
    ADst.Add(w);
end;

procedure TTextWords.Get(var ADst: TArray<String>);
var
  i: Integer;
begin
  SetLength(ADst, Count);
  Reset;
  for i := 0 to High(ADst) do
    FindNext(ADst[i]);
end;

procedure TTextWords.Get(var ADst: TArray<TWordPosRec>);
var
  i: Integer;
begin
  SetLength(ADst, Count);
  Reset;
  for i := 0 to High(ADst) do
    FindNext(ADst[i].Start, ADst[i].Len);
end;

class function TTextWords.IsNonSpace(const C: Char): Boolean;
begin
  result := C>' ';
end;

class function TTextWords.IsLetter(const C: Char): Boolean;
begin
  Result := C.IsLetter;
end;

class function TTextWords.IsDigit(const C: Char): Boolean;
begin
  Result := C.IsDigit;
end;

class function TTextWords.IsLetterOrDigit(const C: Char): Boolean;
begin
  Result := C.IsLetter or C.IsDigit;
end;

function TTextWords.GetSubStr(const APos: TWordPosRec): String;
begin
  Result := Copy(Text, APos.Start, APos.Len);
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

{ TMultimap<TKey, TValue> }

constructor TMultimap<TKey, TValue>.Create;
begin
  Create(IEqualityComparer<TKey>(nil));
end;

constructor TMultimap<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FCount := TDictionary<TKey, integer>.Create(AComparer);
  FValues := TDictionary<TMultimapKey, TValue>.Create(TMultimapKeyEqualityComparer.Create(AComparer));
end;

constructor TMultimap<TKey, TValue>.Create(
  const ACollection: TEnumerable<TPair<TKey, TValue>>);
begin
  Create(IEqualityComparer<TKey>(nil));
end;

destructor TMultimap<TKey, TValue>.Destroy;
begin
  FreeAndNil(FCount);
  FreeAndNil(FValues);
  FreeAndNil(FKeyCollection);
  inherited;
end;

function TMultimap<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  result := GetEnumerator;
end;

function TMultimap<TKey, TValue>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TMultimap<TKey, TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(FCount);
  Result := FKeyCollection;
end;

function TMultimap<TKey, TValue>.GetTotalValuesCount: integer;
begin
  result := FValues.Count;
end;

procedure TMultimap<TKey, TValue>.Clear;
begin
  FCount.Clear;
  FValues.Clear;
end;

function TMultimap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  result := GetValuesCount(AKey)>0;
end;

function TMultimap<TKey, TValue>.ContainsKeys(
  const AKeys: array of TKey): Boolean;
var
  i: Integer;
begin
  for i := Low(AKeys) to High(AKeys) do
    if not ContainsKey(AKeys[i]) then
      Exit(False);
  result := True;
end;

function TMultimap<TKey, TValue>.GetValuesCount(const AKey: TKey): Integer;
begin
  if not FCount.TryGetValue(AKey, result) then
    result := 0;
end;

procedure TMultimap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  MKey: TMultimapKey;
begin
  MKey.Key := AKey;
  if not FCount.TryGetValue(AKey, MKey.Number) then
    MKey.Number := 0;
  FValues.Add(MKey, AValue);
  inc(MKey.Number);
  FCount.AddOrSetValue(AKey, MKey.Number);
end;

procedure TMultimap<TKey, TValue>.Add(const AKey: TKey;
  const AValues: array of TValue);
var
  MKey: TMultimapKey;
  i: Integer;
begin
  MKey.Key := AKey;
  if not FCount.TryGetValue(AKey, MKey.Number) then
    MKey.Number := 0;
  for i := Low(AValues) to High(AValues) do
  begin
    FValues.Add(MKey, AValues[i]);
    inc(MKey.Number);
  end;
  FCount.AddOrSetValue(AKey, MKey.Number);
end;

procedure TMultimap<TKey, TValue>.Add(const AKey: TKey; const AValues: TEnumerable<TValue>);
var
  item: TValue;
begin
  for item in AValues do
    Add(AKey, item);
end;

procedure TMultimap<TKey, TValue>.Add(
  const ACollection: TEnumerable<TPair<TKey, TValue>>);
var
  item: TPair<TKey,TValue>;
begin
  for item in ACollection do
    Add(item.Key, item.Value);
end;

function TMultimap<TKey, TValue>.Remove(const AKey: TKey): Boolean;
var
  MKey: TMultimapKey;
begin
  result := FCount.TryGetValue(AKey, MKey.Number);
  if not result then
    Exit;
  FCount.Remove(AKey);
  MKey.Key := AKey;
  while MKey.Number>0 do
  begin
    dec(MKey.Number);
    FValues.Remove(MKey);
  end;
end;

procedure TMultimap<TKey, TValue>.RemoveValue(const AEnum: TValueEnumerator);
var
  LastKey: TMultimapKey;
  LastValue: TValue;
begin
  if not FValues.ContainsKey(AEnum.FMultimapKey) or not FCount.TryGetValue(AEnum.Key, LastKey.Number) then
    raise Exception.Create('Error');
  dec(LastKey.Number);
  LastKey.Key := AEnum.Key;
  if not FValues.TryGetValue(LastKey, LastValue) then
    raise Exception.Create('Error');
  FValues.AddOrSetValue(AEnum.FMultimapKey, LastValue);
  FValues.Remove(LastKey);
  if LastKey.Number=0 then
    FCount.Remove(AEnum.Key)
  else
    FCount.AddOrSetValue(AEnum.Key, LastKey.Number);
end;

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TSet<TValue>);
var
  Enum: TValueEnumerator;
begin
  Enum := Values[AKey];
  while Enum.MoveNext do
    if AValues.Contains(Enum.Current) then
      RemoveValue(Enum);
end;

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: array of TValue);
var
  s: TSet<TValue>;
begin
  s := TSet<TValue>.Create(AValues);
  try
    RemoveValues(AKey, s);
  finally
    FReeAndNil(s);
  end;
end;

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>);
var
  s: TSet<TValue>;
begin
  s := TSet<TValue>.Create(AValues);
  try
    RemoveValues(AKey, s);
  finally
    FReeAndNil(s);
  end;
end;

function TMultimap<TKey, TValue>.GetValuesEnumerator(const AKey: TKey): TValueEnumerator;
begin
  result := TValueEnumerator.Create(Self, AKey);
end;

{ TMultimap<TKey, TValue>.TValueEnumerator }

constructor TMultimap<TKey, TValue>.TValueEnumerator.Create(AMultimap: TMultimap<TKey, TValue>; const AKey: TKey);
begin
  FMultimap := AMultimap;
  FMultimapKey.Key := AKey;
  if not FMultimap.FCount.TryGetValue(AKey, FMultimapKey.Number) then
    FMultimapKey.Number := -1;
end;

function TMultimap<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  result := FMultimapKey.Number>0;
  if result then
    dec(FMultimapKey.Number);
end;

function TMultimap<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if not FMultimap.FValues.TryGetValue(FMultimapKey, result) then
    raise Exception.Create('Error');
end;

function TMultimap<TKey, TValue>.TValueEnumerator.GetKey: TKey;
begin
  result := FMultimapKey.Key;
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

{ TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer }

constructor TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer.Create(AKeyComparer: IEqualityComparer<TKey>);
begin
  FKeyComparer := AKeyComparer;
  if FKeyComparer=nil then
    FKeyComparer := TEqualityComparer<TKey>.Default;
end;

function TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer.Equals(const Left,
  Right: TMultimapKey): Boolean;
begin
  result := (Left.Number=Right.Number) and FKeyComparer.Equals(Left.Key, Right.Key);
end;

function TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer.GetHashCode(
  const Value: TMultimapKey): Integer;
begin
  result := FKeyComparer.GetHashCode(Value.Key) xor Value.Number;
end;

{ TMultimap<TKey, TValue>.TPairEnumerator }

constructor TMultimap<TKey, TValue>.TPairEnumerator.Create(
  const AMultimap: TMultimap<TKey, TValue>);
begin
  inherited Create;
  FMultimap := AMultimap;
  FCurrentKey := FMultimap.FCount.Keys.GetEnumerator;
end;

destructor TMultimap<TKey, TValue>.TPairEnumerator.Destroy;
begin
  FMultimap := nil;
  FreeAndNil(FCurrentKey);
  inherited;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  Result := GetCurrent;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if not FInEnumKey then
  begin
    result := FCurrentKey.MoveNext;
    if not result then
      Exit;
    FCurrentValue := FMultimap.Values[FCurrentKey.Current];
    result := FCurrentValue.MoveNext;
    Assert(result); // if key is here, then there must be at least one value
    FInEnumKey := True;
    Exit;
  end;
  result := FCurrentValue.MoveNext;
  if not result then
  begin
    FInEnumKey := False;
    result := MoveNext;
  end;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  result.Key := FCurrentKey.Current;
  result.Value := FCurrentValue.Current;
end;

initialization
  {$IFDEF QuickTest}
  CrossPlatform.Tools_Test.TTests.QuickTest;
  {$ENDIF}

finalization
  TTiming.Finilaze;

end.

