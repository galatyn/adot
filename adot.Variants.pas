unit adot.Variants;

{ Definition of classes/record types:

  TUnifiedVarAccess = record
    Unified access to content of variable of any type:
    - array of byte/integer/extended/... type can be accessed as array of "double".
    - operations on variant array of any type can be done in unified style.

  TVar = class
    Utils/helpers for variant type (set of functions ToType/ToTypeDef/TryToType etc)

  TVarArray = record
    Helper for variant array (unified access to numeric values as "double" etc).

  TVarArrayTypified<ElementType> = record
    Typed access to elements of variant array (should be instantiated with correct type).

}
interface

uses
  adot.Types,
  System.Variants,
  System.Math,
  System.SysUtils,
  System.Classes;

type

  { Utils/helpers for variant type (set of functions ToType/ToTypeDef/TryToType etc) }
  TVar = class
  public
    class function IsArray(const v: variant): boolean; static;
    class function IsRef(const v: variant): boolean;
    class function IsArrayOrRef(const v: variant): boolean;

    class function IsStr(const v: variant): boolean;
    class function IsDateTime(const v: variant): boolean;
    class function IsNumeric(const v: variant): boolean;
    class function IsInteger(const v: variant): boolean;
    class function IsBoolean(const v: variant): boolean;

    class function IsNull(const V: variant): Boolean; overload;
    class function IsNull(const V: array of variant): Boolean; overload;
    class function IsEmpty(const V: variant): Boolean; { Null, Unassigned }

    { simple conversion (raises exception in case of errors) }
    class function ToInteger(const Src: variant): int64; static;
    class function ToFloat(const Src: variant): double; static;
    class function ToCurrency(const Src: variant): Currency; static;
    class function ToChar(const Src: variant): char; static;
    class function ToGUID(const Src: variant): TGUID; overload; static;
    class function ToBoolean(const Src: variant): boolean; static;
    class function ToString(const Src: variant): string; reintroduce; static;
    class function ToDateTime(const Src: variant): TDateTime; static;

    { same conversion but returns boolean instead of exceptions }
    class function TryToInteger(const Src: variant; out Dst: integer): boolean; overload; static;
    class function TryToInteger(const Src: variant; out Dst: int64): boolean; overload; static;
    class function TryToFloat(const Src: variant; out Dst: double): boolean; static;
    class function TryToCurrency(const Src: variant; out Dst: currency): boolean; static;
    class function TryToChar(const Src: variant; out Dst: char): boolean; static;
    class function TryToGUID(const Src: variant; out Dst: TGUID): boolean; static;
    class function TryToBoolean(const Src: variant; out Dst: boolean): boolean; static;
    class function TryToString(const Src: variant; out Dst: string): boolean; static;
    class function TryToDateTime(const Src: variant; out Dst: TDateTime): boolean; static;

    { same conversion with default value instead of exception }
    class function ToIntegerDef(const Src: variant; const Default: int64 = 0): int64; static;
    class function ToFloatDef(const Src: variant; const Default: double = 0): double; static;
    class function ToCurrencyDef(const Src: variant; const Default: currency = 0): currency; static;
    class function ToCharDef(const Src: variant; const Default: char = #0): char; static;
    class function ToGUIDDef(const Src: variant; const Default: TGUID): TGUID; overload; static;
    class function ToGUIDDef(const Src: variant): TGUID; overload; static;
    class function ToBooleanDef(const Src: variant; const Default: boolean = False): boolean; static;
    class function ToStringDef(const Src: variant; const Default: string = ''): string; reintroduce; static;
    class function ToDateTimeDef(const Src: variant; const Default: TDateTime = 0): TDateTime; static;

    class function VarOrArrayToStr(const V: variant; const ArrElemDelimeter: string = #13#10): string; static;
    class function GetArrayBounds(const DataArray: Variant; var ALow, AHigh: Integer): boolean; overload;
    class function GetArrayBounds(const DataArray: Variant; var ALow1,AHigh1,ALow2,AHigh2: Integer): boolean; overload;

    class function VarRecAsVariant(Value: TVarRec): Variant; static;
    class function VarRecAsString(Value: TVarRec): String; static;
  end;

  TEnumFloatProc = reference to procedure(var AValue: Double; const ADim: TArray<integer>);

  { Helper for variant array (unified access to numeric values as "double" etc). }
  TVarArray = record
  public
    type
      TDimEnumerator = record
      private
        FDimCount: integer;
        FDimCurrent: TArray<integer>;
        FDimInc: TArray<integer>;
        FDimLow: TArray<integer>;
        FDimHigh: TArray<integer>;

      public
        constructor Create(const Arr: TVarArray);
        procedure Next;
        property Current: TArray<integer> read FDimCurrent;
      end;

  private
    FVArray: variant;

    function GetDimCount: integer;
    function GetDimHighBound(ADim: integer): integer;
    function GetDimLowBound(ADim: integer): integer;
    function GetCount: integer;

  public
    constructor Create(VArray: variant);

    function Lock: pointer;
    procedure Unlock;

    procedure Clear;

    { default enumerator for indices of all elements }
    function GetEnumerator: TDimEnumerator;

    { Enumerates all compatible types (array of integer/string/...) as "float" for unified access.
      If type of array elements is not compatible, then returns False. }
    function EnumElementsAsFloat(const ACurProc: TEnumFloatProc): boolean;

    property Count: integer read GetCount; { Total number of elements (in all dimensions) }
    property DimCount: integer read GetDimCount;
    property DimLowBound[ADim: integer]: integer read GetDimLowBound; { Indexing starts from 0 }
    property DimHighBound[ADim: integer]: integer read GetDimHighBound; { Indexing starts from 0 }
    property VArray: variant read FVArray;
  end;

  { Typed access to elements of variant array (should be instantiated with correct type). }
  TVarArrayTypified<ElementType> = record
  private
    type
      TVarArrayEnumerator = record
      private
        FVarArray: variant;
        FTotalItemsLeft: integer;
        FCurrentPtr: ^ElementType;

        function GetCurrent: ElementType;
      public
        constructor Create(const Arr: TVarArrayTypified<ElementType>);
        function MoveNext: Boolean;
        property Current: ElementType read GetCurrent;
      end;

    var
      FVarArray: TVarArray;

    function GetDimCount: integer;
    function GetDimHighBound(ADim: integer): integer;
    function GetDimLowBound(ADim: integer): integer;
    function GetCount: integer;
    function GetVArray: variant;

  public
    type
      TEnumProc = reference to procedure(var AValue: ElementType);
      TEnumProcDim = reference to procedure(var AValue: ElementType; const ADim: TArray<integer>);

    constructor Create(const VArray: variant);

    { Default enumerator for all elements of the array:
      arr := TVarArray<variant>.Create(v);
      for i in arr do
        <...> }
    function GetEnumerator: TVarArrayEnumerator;

    function GetDimEnumerator: TVarArray.TDimEnumerator;

    { Enumerate all items for all dimensions (there is overloaded function
      without "indices" parameter):
        arr := TVarArray<variant>.Create(Value);
        arr.Enumerate(
          procedure(var Value: variant; const indices: TArray<integer>)
          begin
            value := -value;
          end); }
    procedure Enumerate(const AProc: TEnumProc); overload;
    procedure Enumerate(const AProc: TEnumProcDim); overload;

    function Lock: pointer;
    procedure Unlock;

    { Total number of elements (in all dimensions) }
    property Count: integer read GetCount;

    { Dimensions info }
    property DimCount: integer read GetDimCount;
    property DimLowBound[ADim: integer]: integer read GetDimLowBound; { Indexing starts from 0 }
    property DimHighBound[ADim: integer]: integer read GetDimHighBound; { Indexing starts from 0 }
    property VArray: variant read GetVArray;
  end;

  { Access variant array of bytes as read/write stream (resize operation is not allowed) }
  TVarArrayStream = class(TCustomMemoryStream)
  protected
    FValue: Variant;

    procedure SetValue(AValue: Variant);
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(AValue: Variant);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function ExtractValue: variant;

    class function IsAcceptable(const AValue: Variant):Boolean; static;

    property Value: Variant read FValue write SetValue;
  end;

implementation

uses
  adot.Tools;

type
  { Unified access to content of variable of any type:
    - array of byte/integer/extended/... type can be accessed as array of "double".
    - operations on variant array of any type can be done in unified style. }
  TUnifiedVarAccess = record
  private
    type
      TFloatReader = function(var Src): Double;
      TFloatSetter = procedure(const Src: Double; var Dst);
    var
      Reader: TFloatReader;
      Setter: TFloatSetter;
      ElementSizeInArray: integer;

    function GetAsFloat(var Src): Double;
    procedure SetAsFloat(var Dst; const Value: Double);
    procedure SetAccessProc(const AReader: TFloatReader; const ASetter: TFloatSetter;
      AElementSize: integer);

    class function GetByteAsFloat(var Value): Double; static;
    class function GetWordAsFloat(var Value): Double; static;
    class function GetLongwordAsFloat(var Value): Double; static;
    class function GetShortIntAsFloat(var Value): Double; static;
    class function GetSmallIntAsFloat(var Value): Double; static;
    class function GetIntegerAsFloat(var Value): Double; static;
    class function GetInt64AsFloat(var Value): Double; static;
    class function GetUInt64AsFloat(var Value): Double; static;
//    class function GetNativeIntAsFloat(var Value): Double; static;
//    class function GetNativeUIntAsFloat(var Value): Double; static;
    class function GetSingleAsFloat(var Value): Double; static;
    class function GetDoubleAsFloat(var Value): Double; static;
//    class function GetExtendedAsFloat(var Value): Double; static;
    class function GetCurrencyAsFloat(var Value): Double; static;
    class function GetStringAsFloat(var Value): Double; static;
//    class function GetUnicodeStringAsFloat(var Value): Double; static;
    class function GetWideStringAsFloat(var Value): Double; static;
    class function GetVariantAsFloat(var Value): Double; static;

    class procedure SetByteAsFloat(const Src: Double; var Value); static;
    class procedure SetWordAsFloat(const Src: Double; var Value); static;
    class procedure SetLongwordAsFloat(const Src: Double; var Value); static;
    class procedure SetShortIntAsFloat(const Src: Double; var Value); static;
    class procedure SetSmallIntAsFloat(const Src: Double; var Value); static;
    class procedure SetIntegerAsFloat(const Src: Double; var Value); static;
    class procedure SetInt64AsFloat(const Src: Double; var Value); static;
    class procedure SetUInt64AsFloat(const Src: Double; var Value); static;
//    class procedure SetNativeIntAsFloat(const Src: Double; var Value); static;
//    class procedure SetNativeUIntAsFloat(const Src: Double; var Value); static;
    class procedure SetSingleAsFloat(const Src: Double; var Value); static;
    class procedure SetDoubleAsFloat(const Src: Double; var Value); static;
//    class procedure SetExtendedAsFloat(const Src: Double; var Value); static;
    class procedure SetCurrencyAsFloat(const Src: Double; var Value); static;
    class procedure SetStringAsFloat(const Src: Double; var Value); static;
//    class procedure SetUnicodeStringAsFloat(const Src: Double; var Value); static;
    class procedure SetWideStringAsFloat(const Src: Double; var Value); static;
    class procedure SetVariantAsFloat(const Src: Double; var Value); static;

  public
    class function Init(VarType: integer; var AHelper: TUnifiedVarAccess): boolean; static;

    property AsFloat[var Src]: double read GetAsFloat write SetAsFloat;
    property ElementSize: integer read ElementSizeInArray;
  end;

{ TUnifiedVarAccess }

class function TUnifiedVarAccess.Init(VarType: integer; var AHelper: TUnifiedVarAccess): boolean;
begin
  AHelper.Reader := nil;
  AHelper.Setter := nil;

  { constants from System.Variants.pas }
  case VarType of
//    varEmpty    = $0000; { vt_empty        0 }
//    varNull     = $0001; { vt_null         1 }
    varSmallint : AHelper.SetAccessProc(GetSmallintAsFloat, SetSmallintAsFloat, SizeOf(SmallInt));
    varInteger  : AHelper.SetAccessProc(GetIntegerAsFloat, SetIntegerAsFloat, SizeOf(Integer));
    varSingle   : AHelper.SetAccessProc(GetSingleAsFloat, SetSingleAsFloat, SizeOf(Single));
    varDouble   : AHelper.SetAccessProc(GetDoubleAsFloat, SetDoubleAsFloat, SizeOf(Double));
    varCurrency : AHelper.SetAccessProc(GetCurrencyAsFloat, SetCurrencyAsFloat, SizeOf(Currency));
//    varDate     = $0007; { vt_date         7 }
    varOleStr   : AHelper.SetAccessProc(GetWideStringAsFloat, SetWideStringAsFloat, SizeOf(WideString));
//    varDispatch = $0009; { vt_dispatch     9 }
//    varError    = $000A; { vt_error       10 }
//    varBoolean  = $000B; { vt_bool        11 }
    varVariant  : AHelper.SetAccessProc(GetVariantAsFloat, SetVariantAsFloat, SizeOf(Variant));
//    varUnknown  = $000D; { vt_unknown     13 }
//  //varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
//  //varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
    varShortInt : AHelper.SetAccessProc(GetShortIntAsFloat, SetShortIntAsFloat, SizeOf(ShortInt));
    varByte     : AHelper.SetAccessProc(GetByteAsFloat, SetByteAsFloat, SizeOf(Byte));
    varWord     : AHelper.SetAccessProc(GetWordAsFloat, SetWordAsFloat, SizeOf(Word));
    varLongWord : AHelper.SetAccessProc(GetLongWordAsFloat, SetLongWordAsFloat, SizeOf(LongWord));
    varInt64    : AHelper.SetAccessProc(GetInt64AsFloat, SetInt64AsFloat, SizeOf(Int64));
    varUInt64   : AHelper.SetAccessProc(GetUInt64AsFloat, SetUInt64AsFloat, SizeOf(UInt64));
//    varRecord   = $0024; { VT_RECORD      36 }
  {  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

//    varStrArg   = $0048; { vt_clsid        72 }
//    varObject   = $0049; {                 73 }
//    varUStrArg  = $004A; {                 74 }
    varString   : AHelper.SetAccessProc(GetStringAsFloat, SetStringAsFloat, SizeOf(String));
//    varAny      = $0101; { Corba any      257 } {not OLE compatible }
    //varUString  : AHelper.SetAccessProc(GetUnicodeStringAsFloat, SetUnicodeStringAsFloat, SizeOf(UnicodeString));
  end;
  result := Assigned(AHelper.Reader) and Assigned(AHelper.Setter);
end;

procedure TUnifiedVarAccess.SetAccessProc(const AReader: TFloatReader; const ASetter: TFloatSetter;
  AElementSize: integer);
begin
  Reader := AReader;
  Setter := ASetter;
  ElementSizeInArray := AElementSize;
end;

function TUnifiedVarAccess.GetAsFloat(var Src): Double;
begin
  result := Reader(Src);
end;

procedure TUnifiedVarAccess.SetAsFloat(var Dst; const Value: Double);
begin
  Setter(Value, Dst);
end;

class function TUnifiedVarAccess.GetByteAsFloat(var Value): Double;
begin
  result := Byte(Value);
end;

class function TUnifiedVarAccess.GetCurrencyAsFloat(var Value): Double;
begin
  result := Currency(Value);
end;

class function TUnifiedVarAccess.GetDoubleAsFloat(var Value): Double;
begin
  result := Double(Value);
end;

//class function TUnifiedVarAccess.GetExtendedAsFloat(var Value): Double;
//begin
//  result := Extended(Value);
//end;

class function TUnifiedVarAccess.GetInt64AsFloat(var Value): Double;
begin
  result := Int64(Value);
end;

class function TUnifiedVarAccess.GetIntegerAsFloat(var Value): Double;
begin
  result := Integer(Value);
end;

class function TUnifiedVarAccess.GetLongwordAsFloat(var Value): Double;
begin
  result := Longword(Value);
end;

//class function TUnifiedVarAccess.GetNativeIntAsFloat(var Value): Double;
//begin
//  result := NativeInt(Value);
//end;

class function TUnifiedVarAccess.GetShortIntAsFloat(var Value): Double;
begin
  result := ShortInt(Value);
end;

class function TUnifiedVarAccess.GetSingleAsFloat(var Value): Double;
begin
  result := Single(Value);
end;

class function TUnifiedVarAccess.GetSmallIntAsFloat(var Value): Double;
begin
  result := SmallInt(Value);
end;

class function TUnifiedVarAccess.GetStringAsFloat(var Value): Double;
begin
  if not TryStrToFloat(String(Value), result) then
    result := 0;
end;

class function TUnifiedVarAccess.GetUInt64AsFloat(var Value): Double;
begin
  result := UInt64(Value);
end;

//class function TUnifiedVarAccess.GetNativeUIntAsFloat(var Value): Double;
//begin
//  result := NativeUInt(Value);
//end;

//class function TUnifiedVarAccess.GetUnicodeStringAsFloat(var Value): Double;
//begin
//  if not TryStrToFloat(UnicodeString(Value), result) then
//    result := 0;
//end;

class function TUnifiedVarAccess.GetVariantAsFloat(var Value): Double;
begin
  result := TVar.ToFloatDef(Variant(Value), 0);
end;

class function TUnifiedVarAccess.GetWideStringAsFloat(var Value): Double;
begin
  if not TryStrToFloat(WideString(Value), result) then
    result := 0;
end;

class function TUnifiedVarAccess.GetWordAsFloat(var Value): Double;
begin
  result := Word(Value);
end;

class procedure TUnifiedVarAccess.SetByteAsFloat(const Src: Double; var Value);
begin
  Byte(Value) := Round(Src);
end;

class procedure TUnifiedVarAccess.SetCurrencyAsFloat(const Src: Double; var Value);
begin
  Currency(Value) := Src;
end;

class procedure TUnifiedVarAccess.SetDoubleAsFloat(const Src: Double; var Value);
begin
  Double(Value) := Src;
end;

//class procedure TUnifiedVarAccess.SetExtendedAsFloat(const Src: Double; var Value);
//begin
//  Extended(Value) := Src;
//end;

class procedure TUnifiedVarAccess.SetInt64AsFloat(const Src: Double; var Value);
begin
  Int64(Value) := Round(Src);
end;

class procedure TUnifiedVarAccess.SetIntegerAsFloat(const Src: Double; var Value);
begin
  Integer(Value) := Round(Src);
end;

class procedure TUnifiedVarAccess.SetLongwordAsFloat(const Src: Double; var Value);
begin
  LongWord(Value) := Round(Src);
end;

//class procedure TUnifiedVarAccess.SetNativeIntAsFloat(const Src: Double; var Value);
//begin
//  NativeInt(Value) := Round(Src);
//end;

class procedure TUnifiedVarAccess.SetShortIntAsFloat(const Src: Double; var Value);
begin
  ShortInt(Value) := Round(Src);
end;

class procedure TUnifiedVarAccess.SetSingleAsFloat(const Src: Double; var Value);
begin
  NativeInt(Value) := Round(Src);
end;

class procedure TUnifiedVarAccess.SetSmallIntAsFloat(const Src: Double; var Value);
begin
  SmallInt(Value) := Round(Src);
end;

class procedure TUnifiedVarAccess.SetStringAsFloat(const Src: Double; var Value);
begin
  String(Value) := FloatToStr(Src);
end;

class procedure TUnifiedVarAccess.SetUInt64AsFloat(const Src: Double; var Value);
begin
  UInt64(Value) := Round(Src);
end;

//class procedure TUnifiedVarAccess.SetNativeUIntAsFloat(const Src: Double; var Value);
//begin
//  NativeUInt(Value) := Round(Src);
//end;
//
//class procedure TUnifiedVarAccess.SetUnicodeStringAsFloat(const Src: Double; var Value);
//begin
//  UnicodeString(Value) := FloatToStr(Src);
//end;

class procedure TUnifiedVarAccess.SetVariantAsFloat(const Src: Double; var Value);
begin
  Variant(Value) := Src;
end;

class procedure TUnifiedVarAccess.SetWideStringAsFloat(const Src: Double; var Value);
begin
  WideString(Value) := FloatToStr(Src);
end;

class procedure TUnifiedVarAccess.SetWordAsFloat(const Src: Double; var Value);
begin
  Word(Value) := Round(Src);
end;

{ TVarArray<ElementType>.TVarArrayEnumerator<ElementType> }

constructor TVarArrayTypified<ElementType>.TVarArrayEnumerator.Create(const Arr: TVarArrayTypified<ElementType>);
begin
  FVarArray := Arr.VArray;
  FTotalItemsLeft  := Arr.Count;
  if FTotalItemsLeft<=0 then
    FCurrentPtr := nil
  else
  begin
    FCurrentPtr := VarArrayLock(FVarArray);
    dec(FCurrentPtr);
  end;
end;

function TVarArrayTypified<ElementType>.TVarArrayEnumerator.MoveNext: Boolean;
begin
  result := FTotalItemsLeft>0;
  if result then
  begin
    dec(FTotalItemsLeft);
    inc(FCurrentPtr);
  end
  else
    if FCurrentPtr<>nil then
    begin
      FCurrentPtr := nil;
      VarArrayUnlock(FVarArray);
    end;
end;

function TVarArrayTypified<ElementType>.TVarArrayEnumerator.GetCurrent: ElementType;
begin
  result := FCurrentPtr^;
end;

{ TVarArrayTypified<ElementType> }

constructor TVarArrayTypified<ElementType>.Create(const VArray: variant);
begin
  FVarArray := TVarArray.Create(VArray);
end;

function TVarArrayTypified<ElementType>.GetDimEnumerator: TVarArray.TDimEnumerator;
begin
  result := FVarArray.GetEnumerator;
end;

function TVarArrayTypified<ElementType>.Lock: pointer;
begin
  result := FVarArray.Lock;
end;

procedure TVarArrayTypified<ElementType>.Unlock;
begin
  FVarArray.UnLock;
end;

function TVarArrayTypified<ElementType>.GetCount: integer;
begin
  result := FVarArray.Count;
end;

function TVarArrayTypified<ElementType>.GetVArray: variant;
begin
  result := FVarArray.VArray;
end;

function TVarArrayTypified<ElementType>.GetDimCount: integer;
begin
  result := FVarArray.DimCount;
end;

function TVarArrayTypified<ElementType>.GetDimHighBound(ADim: integer): integer;
begin
  result := FVarArray.DimHighBound[ADim];
end;

function TVarArrayTypified<ElementType>.GetDimLowBound(ADim: integer): integer;
begin
  result := FVarArray.DimLowBound[ADim];
end;

function TVarArrayTypified<ElementType>.GetEnumerator: TVarArrayEnumerator;
begin
  result := TVarArrayEnumerator.Create(Self);
end;

procedure TVarArrayTypified<ElementType>.Enumerate(const AProc: TEnumProc);
var
  p: ^ElementType;
  i: Integer;
begin
  p := Lock;
  try
    for i := 0 to Count-1 do
    begin
      AProc(p^);
      inc(p);
    end;
  finally
    Unlock;
  end;
end;

procedure TVarArrayTypified<ElementType>.Enumerate(const AProc: TEnumProcDim);
var
  p: ^ElementType;
  i: Integer;
  DimEnum: TVarArray.TDimEnumerator;
begin
  DimEnum := GetDimEnumerator;
  p := Lock;
  try
    for i := 0 to Count-1 do
    begin
      AProc(p^, DimEnum.Current);
      inc(p);
      DimEnum.Next;
    end;
  finally
    Unlock;
  end;
end;

{ TVar }

class function TVar.ToBoolean(const Src: variant): boolean;
var
  S: string;
begin
  if IsBoolean(Src) then { VarIsNumeric returns True for varBoolean type, so we check it first }
    result := Src
  else
  if VarIsStr(Src) then
  begin
    S := Src;
    result := AnsiSameText(S, 'True') or AnsiSameText(S, 'Ja') or AnsiSameText(S, '1');
  end
  else
  if VarIsNumeric(Src) then
    result := Integer(Src)=1
  else
    result := Src; { give a try }
end;

class function TVar.ToBooleanDef(const Src: variant; const Default: boolean): boolean;
begin
  if not TryToBoolean(Src, result) then
    result := Default;
end;

class function TVar.ToChar(const Src: variant): char;
var
  S: string;
begin
  S := Src;
  if Length(S)>0 then
    result := S[Low(S)]
  else
    raise Exception.Create('Error');
end;

class function TVar.ToCharDef(const Src: variant; const Default: char): char;
begin
  if not TryToChar(Src, result) then
    result := Default;
end;

class function TVar.ToDateTime(const Src: variant): TDateTime;
begin
  result := VarToDateTime(Src); { Supports string/DateTime etc }
end;

class function TVar.ToDateTimeDef(const Src: variant; const Default: TDateTime): TDateTime;
begin
  if not TryToDateTime(Src, result) then
    result := Default;
end;

class function TVar.ToFloat(const Src: variant): double;
begin
  result := Src;
end;

class function TVar.ToCurrency(const Src: variant): currency;
begin
  result := Src;
end;

class function TVar.ToFloatDef(const Src: variant; const Default: double): double;
begin
  if not TryToFloat(Src, result) then
    result := Default;
end;

class function TVar.ToCurrencyDef(const Src: variant; const Default: currency): currency;
begin
  if not TryToCurrency(Src, result) then
    result := Default;
end;

class function TVar.ToGUIDDef(const Src: variant; const Default: TGUID): TGUID;
begin
  if not TryToGUID(Src, result) then
    result := Default;
end;

class function TVar.ToGUID(const Src: variant): TGUID;
begin
  result := StringToGUID(Src);
end;

class function TVar.ToGUIDDef(const Src: variant): TGUID;
begin
  if not TryToGUID(Src, result) then
    result := NullGUID;
end;

class function TVar.ToInteger(const Src: variant): int64;
begin
  Result := Src;
end;

class function TVar.ToIntegerDef(const Src: variant; const Default: int64): int64;
begin
  if not TryToInteger(Src, result) then
    result := Default;
end;

class function TVar.ToString(const Src: variant): string;
begin
  result := Src;
end;

class function TVar.ToStringDef(const Src: variant; const Default: string): string;
begin
  if not TryToString(Src, result) then
    result := Default;
end;

class function TVar.IsBoolean(const v: variant): boolean;
begin
  result := (VarType(v) and varTypeMask)=varBoolean;
end;

class function TVar.TryToBoolean(const Src: variant; out Dst: boolean): boolean;
var
  S: string;
begin
  try
    result := not IsEmpty(Src);
    if result then
      if IsBoolean(Src) then { VarIsNumeric returns True for varBoolean type, so we check it first }
        Dst := Src
      else
      if VarIsStr(Src) then
      begin
        S := Src;
        Dst := AnsiSameText(S, 'True') or AnsiSameText(S, 'Ja') or AnsiSameText(S, '1');
      end
      else
      if VarIsNumeric(Src) then
        Dst := Integer(Src)=1
      else
        Dst := Src; { give a try }
  except
    result := False; { type conversion to integer/boolean may raise exceptions }
  end;
end;

class function TVar.TryToChar(const Src: variant; out Dst: char): boolean;
var
  S: string;
begin
  result := IsStr(Src);
  if result then
  begin
    S := Src;
    if Length(S)>0 then
      Dst := S[Low(S)]
    else
      result := False;
  end;
end;

class function TVar.TryToDateTime(const Src: variant; out Dst: TDateTime): boolean;
begin
  result := not IsEmpty(Src); { To avoid of exceptions in simple situations }
  if result then
    try
      Dst := VarToDateTime(Src); { Supports string/DateTime etc }
    except
      result := False;
    end;
end;

class function TVar.TryToFloat(const Src: variant; out Dst: double): boolean;
begin
  Result := not IsEmpty(Src); { To avoid of exceptions in simple situations }
  if Result then
    try
      { "Dst := Src" does same job, but we use extra check for
        regular strings to avoid of exceptions (annoying in debug) }
      if VarIsStr(Src) then
        result := TryStrToFloat(Src, Dst)
      else
        Dst := Src; { Supports numeric/str/datetime etc }
    except
      result := False;
    end;
end;

class function TVar.TryToCurrency(const Src: variant; out Dst: currency): boolean;
begin
  Result := not IsEmpty(Src); { To avoid of exceptions in simple situations }
  if Result then
    try
      { "Dst := Src" does same job, but we use extra check for
        regular strings to avoid of exceptions (annoying in debug) }
      if VarIsStr(Src) then
        result := TryStrToCurr(Src, Dst)
      else
        Dst := Src; { Supports numeric/str/datetime etc }
    except
      result := False;
    end;
end;

class function TVar.TryToGUID(const Src: variant; out Dst: TGUID): boolean;
var
  S: string;
begin
  result := VarIsStr(Src);  { To avoid of exceptions in simple situations }
  if result then
  begin
    S := Src;
    { Simple check (copy-pasted from StringToGUID) helps to avoid of exceptions in many cases }
    if ((Length(S) <> 38) or (S[Low(string)] <> '{')) then
      result := False
    else
      try
        Dst := StringToGUID(Src);
      except
        result := False;
      end;
  end;
end;

class function TVar.TryToInteger(const Src: variant; out Dst: integer): boolean;
begin
  Result := not IsEmpty(Src); { to avoid of exceptions in simple situations }
  if Result then
    try
      Dst := Src; { Supports numeric/str/datetime etc, no need for manual check  }
    except
      result := False;
    end;
end;

class function TVar.TryToInteger(const Src: variant; out Dst: int64): boolean;
begin
  Result := not IsEmpty(Src); { to avoid of exceptions in simple situations }
  if Result then
    try
      Dst := Src; { Supports numeric/str/datetime etc, no need for manual check  }
    except
      result := False;
    end;
end;

class function TVar.TryToString(const Src: variant; out Dst: string): boolean;
begin
  result := not IsEmpty(Src); { to avoid of exceptions in simple situations }
  if result then
    try
      Dst := Src; { supports all string-compatible formats }
    except
      result := False;
    end;
end;

class function TVar.VarOrArrayToStr(const V: variant; const ArrElemDelimeter: string): string;
var
  i,l,r: Integer;
  Arr: TVarArray;
begin
  if not IsArray(v) then
    if IsEmpty(v) then
      result := ''
    else
      result := v
  else
  begin
    Arr := TVarArray.Create(v);
    Assert(Arr.DimCount=1);
    l := Arr.DimLowBound[0];
    r := Arr.DimHighBound[0];
    result := VarOrArrayToStr(v[l], ArrElemDelimeter);
    for i := l+1 to r do
      result := result + ArrElemDelimeter + VarOrArrayToStr(v[i], ArrElemDelimeter);
  end;
end;

class function TVar.VarRecAsString(Value: TVarRec): String;
begin
  case Value.VType of
    vtInteger       : result := Value.VInteger.ToString;
    vtBoolean       : result := Value.VBoolean.ToString;
    vtChar          : result := Char(Value.VChar);
    vtExtended      : result := Value.VExtended^.ToString;
    {$IFNDEF NEXTGEN}
    vtString        : result := String(Value.VString^);
    {$ENDIF NEXTGEN}
    vtPointer       : result := '$' + THex.PointerToHex(Value.VPointer);
    vtPChar         : result := Char(Value.VPChar^);
    vtObject        :
      if Value.VObject = nil
        then result := 'TObject(nil)'
        else result := Value.VObject.ClassName + '($' + THex.PointerToHex(Value.VObject) + ')';
    vtClass         : result := Value.VClass.ClassName;
    vtWideChar      : result := Value.VWideChar;
    vtPWideChar     : result := Value.VPWideChar^;
    vtAnsiString    : result := UnicodeString(AnsiString(Value.VAnsiString));
    vtCurrency      : result := CurrToStr(Value.VCurrency^);
    vtVariant       : result := TVar.ToStringDef(Value.VVariant^);
    vtInterface     : result := 'Interface($' + THex.PointerToHex(Value.VInterface) + ')';
    vtWideString    : result := WideString(Value.VWideString);
    vtInt64         : result := Value.VInt64^.ToString;
    vtUnicodeString : result := String(Value.VUnicodeString);
  end;
end;

class function TVar.VarRecAsVariant(Value: TVarRec): Variant;
begin
  case Value.VType of
    vtInteger       : result := Value.VInteger;
    vtBoolean       : result := Value.VBoolean;
    vtChar          : result := Char(Value.VChar);
    vtExtended      : result := Value.VExtended^;
    {$IFNDEF NEXTGEN}
    vtString        : result := String(Value.VString^);
    {$ENDIF NEXTGEN}
    vtPointer       : result := VarRecAsString(Value);
    vtPChar         : result := Char(Value.VPChar^);
    vtObject        : result := VarRecAsString(Value);
    vtClass         : result := VarRecAsString(Value);
    vtWideChar      : result := Value.VWideChar;
    vtPWideChar     : result := Value.VPWideChar^;
    vtAnsiString    : result := UnicodeString(AnsiString(Value.VAnsiString));
    vtCurrency      : result := Value.VCurrency^;
    vtVariant       : result := Value.VVariant^;
    vtInterface     : result := VarRecAsString(Value);
    vtWideString    : result := WideString(Value.VWideString);
    vtInt64         : result := Value.VInt64^;
    vtUnicodeString : result := String(Value.VUnicodeString);
  end;
end;

function VarTypeIsStr(const AVarType: TVarType): Boolean;
begin
  Result := (AVarType = varOleStr) or (AVarType = varString) or (AVarType = varUString);
end;

class function TVar.IsStr(const v: variant): boolean;
begin
  Result := VarTypeIsStr(FindVarData(V)^.VType);
end;

{ TIntegerTypesHelper<T> }

class function TVar.IsArray(const v: variant): boolean;
begin
  result := VarType(v) and varArray<>0;
end;

class function TVar.IsRef(const v: variant): boolean;
begin
  result := VarType(v) and varByRef<>0;
end;

class function TVar.IsArrayOrRef(const v: variant): boolean;
begin
  result := VarType(v) and (varArray or varByRef)<>0;
end;

class function TVar.IsDateTime(const v: variant): boolean;
begin
  result := VarType(v) and varTypeMask = varDate;
end;

class function TVar.IsNull(const V: variant): Boolean;
begin
  result := VarIsNull(v);
end;

class function TVar.IsEmpty(const V: variant): Boolean;
begin
  result := VarIsNull(v) or VarIsEmpty(v);
end;

class function TVar.IsNull(const V: array of variant): Boolean;
var
  I: Integer;
begin
  for I := Low(V) to High(V) do
    if not VarIsNull(v[I]) then
      Exit(False);
  Result := True;
end;

class function TVar.IsNumeric(const v: variant): boolean;
var
  vt: Word;
begin
  vt := VarType(v) and varTypeMask;
  result := (vt=varSmallint) or (vt=varInteger) or (vt=varSingle) or (vt=varDouble) or (vt=varCurrency) or
    (vt=varShortInt) or (vt=varByte) or (vt=varWord) or (vt=varLongWord) or (vt=varInt64) or (vt=varUInt64);
end;

class function TVar.IsInteger(const v: variant): boolean;
var
  vt: Word;
begin
  vt := VarType(v) and varTypeMask;
  result := (vt=varSmallint) or (vt=varInteger) or (vt=varShortInt) or (vt=varByte) or
    (vt=varWord) or (vt=varLongWord) or (vt=varInt64) or (vt=varUInt64);
end;

class function TVar.GetArrayBounds(const DataArray: Variant; var ALow, AHigh: Integer): boolean;
begin
  result := IsArray(DataArray);
  if result then
    try
      ALow  := VarArrayLowBound (DataArray, 1);
      AHigh := VarArrayHighBound(DataArray, 1);
    except
      result := false;
      ALow := 0;
      AHigh := -1;
    end;
end;

class function TVar.GetArrayBounds(const DataArray: Variant; var ALow1, AHigh1, ALow2, AHigh2: Integer): boolean;
begin
  result := IsArray(DataArray);
  if result then
    try
      ALow1  := VarArrayLowBound (DataArray, 1);
      AHigh1 := VarArrayHighBound(DataArray, 1);
      ALow2  := VarArrayLowBound (DataArray, 2);
      AHigh2 := VarArrayHighBound(DataArray, 2);
    except
      result := false;
      ALow1  := 0;
      AHigh1 := -1;
      ALow2  := 0;
      AHigh2 := -1;
    end;
end;

{ TVarArray.TDimEnumerator }

constructor TVarArray.TDimEnumerator.Create(const Arr: TVarArray);
var
  i: Integer;
begin
  FDimCount := Arr.DimCount;
  SetLength(FDimLow, FDimCount);
  SetLength(FDimHigh, FDimCount);
  SetLength(FDimInc, FDimCount);
  SetLength(FDimCurrent, FDimCount);
  for i := 0 to FDimCount-1 do
  begin
    FDimLow[i]     := Arr.DimLowBound[i];
    FDimHigh[i]    := Arr.DimHighBound[i];
    FDimInc[i]     := Sign(FDimHigh[i]-FDimLow[i]);
    FDimCurrent[i] := FDimLow[i];
  end;
end;

procedure TVarArray.TDimEnumerator.Next;
var
  i: Integer;
begin
  for i := 0 to FDimCount-1 do
    if FDimCurrent[i]=FDimHigh[i] then
      FDimCurrent[i] := FDimLow[i]
    else
    begin
      inc(FDimCurrent[i], FDimInc[i]);
      Break;
    end;
end;

{ TVarArray }

constructor TVarArray.Create(VArray: variant);
begin
  Assert(TVar.IsArray(VArray));
  FVArray := VArray;
end;

procedure TVarArray.Clear;
begin
  Self := Default(TVarArray);
end;

function TVarArray.EnumElementsAsFloat(const ACurProc: TEnumFloatProc): boolean;
var
  Dim: TDimEnumerator; { item indices }
  Value: TUnifiedVarAccess; { access to value }
  p: PByte; { pointer to current element of the array }
  i,j,n: integer;
  a,b: double;
begin
  result := False;
  if not TVar.IsArray(FVArray) or TVar.IsRef(FVArray) or not TUnifiedVarAccess.Init(VarType(FVArray) and varTypeMask, Value) then
    Exit;
  Dim := GetEnumerator;
  p := Lock;
  if p<>nil then
    try
      j := Count;
      n := Value.ElementSize;
      for i := 0 to j-1 do
      begin
        a := Value.AsFloat[p^];
        b := a;
        ACurProc(a, Dim.Current);
        if a<>b then
          Value.AsFloat[p^] := a;
        inc(p, n);
        Dim.Next;
      end;
    finally
      Unlock;
    end;
end;

function TVarArray.GetCount: integer;
var
  d,i: Integer;
begin
  result := 0;
  d := DimCount;
  if d>0 then
  begin
    result := Abs(DimLowBound[0]-DimHighBound[0]) + 1;
    for i := 1 to d-1 do
      result := result * (Abs(DimLowBound[i]-DimHighBound[i]) + 1);
  end;
end;

function TVarArray.GetDimCount: integer;
begin
  result := VarArrayDimCount(FVArray);
end;

function TVarArray.GetDimHighBound(ADim: integer): integer;
begin
  result := VarArrayHighBound(FVArray, ADim + 1); { VarArrayHighBound has 1-based index }
end;

function TVarArray.GetDimLowBound(ADim: integer): integer;
begin
  result := VarArrayLowBound(FVArray, ADim + 1); { VarArrayLowBound has 1-based index }
end;

function TVarArray.GetEnumerator: TDimEnumerator;
begin
  result := TDimEnumerator.Create(Self);
end;

function TVarArray.Lock: pointer;
begin
  result := VarArrayLock(FVArray);
end;

procedure TVarArray.Unlock;
begin
  VarArrayUnLock(FVArray)
end;

{ TVarArrayStream }

constructor TVarArrayStream.Create(AValue: Variant);
begin
  Value := AValue;
end;

destructor TVarArrayStream.Destroy;
begin
  Value := Null;
  inherited;
end;

procedure TVarArrayStream.SetSize(NewSize: Integer);
begin
  raise Exception.Create('Error');
end;

procedure TVarArrayStream.SetValue(AValue: Variant);
begin
  { release old value }
  if Memory<>nil then
  begin
    VarArrayUnLock(FValue);
    SetPointer(nil, 0);
  end;

  { assign new one }
  Assert(IsAcceptable(AValue));
  FValue := AValue;
  Position := 0;
  if VarIsNull(AValue) or VarIsClear(AValue) then
    Exit;

  { FValue is 1-dimensional array with ElementSize=1 }
  SetPointer(VarArrayLock(FValue), VarArrayHighBound(FValue, 1) - VarArrayLowBound(FValue, 1) + 1);
end;

function TVarArrayStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := Min(Count, Size-Position);
  if Result > 0 then
  begin
    System.Move(Buffer, (PByte(Memory) + Position)^, Result);
    Position := Position + Result;
  end;
end;

function TVarArrayStream.ExtractValue: variant;
begin
  if Memory<>nil then
  begin
    VarArrayUnLock(FValue);
    SetPointer(nil, 0);
  end;
  result := FValue;
  FValue := null;
end;

class function TVarArrayStream.IsAcceptable(const AValue: Variant): Boolean;
begin
  Result :=
    VarIsNull(AValue) or
    VarIsClear(AValue) or
    VarIsArray(AValue) and (VarArrayDimCount(AValue)=1) and (TVarData(AValue).VArray.ElementSize=1);
end;

end.
