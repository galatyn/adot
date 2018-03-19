unit adot.Tools;
{$OVERFLOWCHECKS OFF}
{$IFNDEF Debug}
  { $Define UseInline}
{$ENDIF}

{ Definition of classes/record types:

  TArrayUtils = record
    Fill/fillRandom/Randomize and other tools for arrays.

  TComponentUtils = class
    Enumeration and other component-specific tools.

  TCurrencyUtils = class
    Currency type utils.

  TDateTimeRec = record
    Record type to define TDateTime compatible constants in readable way.

  TDateTimeUtils = class
    Check TDateTime correctness, convert to string etc.

  TGUIDUtils = class
    IsValid and other utils.

  THex = class
    Set of functions for HEX conversion.

  TIfThen = class
    Generic implementation of IfThen (to accept virtually any type). Example:
     A := TIfThen.Get(Visible, fsMDIChild, fsNormal);

  TInterfacedObject<T: class> = class
    Wrapper class to make any class type available through interface (that means that
    lifetime of the class will be controlled by reference counter)

  TNullable<T> = record
    Extends any type by IsNull property.

}
interface

uses
  adot.Types,
  adot.Collections,
  adot.Collections.Maps,
  System.DateUtils,
  System.Types,
  System.Rtti,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.TimeSpan;

type

  { Set of functions for HEX conversion }
  THex = class
  protected
    const
      { System.SysUtils.TwoHexLookup is hidden behind implementation section,
        so we have to reintroduce it. }
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
        ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1, { 0..9 }
         -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1, { a..f }
         -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
         -1,10,11,12,13,14,15);                           { A..F }
  public

    class function EncodedSizeChars(SourceSizeBytes: integer): integer; static;
    class function DecodedSizeBytes(EncodedSizeChars: integer): integer; static;

    class function Encode(const Buf; ByteBufSize: integer): String; overload; static;
    class function Encode<T: Record>(const Value: T): String; overload; static;
    class function Encode(const s: TBytes):String; overload; static;
    class function Encode(const s: string): string; overload; static;
    class function Encode(const s: string; utf8: boolean): string; overload; static;
    {$If Defined(MSWindows)}
      class function EncodeAnsiString(const s: AnsiString):String; static;
    {$EndIf}
    class function EncodeByteH(Src: byte): char; static;
    class function EncodeByteL(Src: byte): char; static;

    class procedure Decode(const HexEncodedStr: String; var Buf); overload; static;
    class function Decode<T: Record>(const HexEncodedStr: String): T; overload; static;
    class function DecodeBytes(const HexEncodedStr: String):TBytes; static;
    class function DecodeString(const HexEncodedStr: string): string; overload; static;
    class function DecodeString(const HexEncodedStr: string; utf8: boolean): string; overload; static;
    {$If Defined(MSWindows)}
      class function DecodeAnsiString(const HexEncodedStr: String):AnsiString; static;
    {$EndIf}
    class function DecodeByte(H,L: Char): byte; static;
    class function DecodeHexChar(HexChar: Char): byte; static;

    class function IsValid(const HexEncodedStr: String):Boolean; overload; static;
    class function IsValid(const HexEncodedStr: String; ZeroBasedStartIdx,Len: integer):Boolean; overload; static;
    class function IsValid(const C: Char):Boolean; overload; static;

    { Int64ToHex(Value) <> Encode(Value, SizeOf(Value)) for x86-compatible CPU family,
      because lower bytes of integers are stored by lower addresses. When we translate
      integer/pointer to hex we would like to use regular notation, when higher digits
      are shown first. }
    class function Int64ToHex(s: Int64): string; static;
    class function UInt64ToHex(s: UInt64): string; static;
    class function NativeIntToHex(s: NativeInt): string; static;
    class function NativeUIntToHex(s: NativeUInt): string; static;
    class function PointerToHex(s: Pointer): string; static;
    class function CardinalToHex(s: cardinal): string; static;
    class function WordToHex(s: word): string; static;

    class function HexToInt64(const HexEncodedInt: String):Int64; static;
    class function HexToUInt64(const HexEncodedInt: String):UInt64; static;
    class function HexToNativeInt(const HexEncodedInt: String):NativeInt; static;
    class function HexToNativeUInt(const HexEncodedInt: String):NativeUInt; static;
    class function HexToPointer(const HexEncodedPointer: String):Pointer; static;
    class function HexToCardinal(const HexEncodedCardinal: String):Cardinal; static;
    class function HexToWord(const HexEncodedWord: String):Word; static;
  end;

  TInvertedComparer<TValueType> = class(TInterfacedObject, IComparer<TValueType>)
  protected
    FExtComparer: IComparer<TValueType>;
  public
    constructor Create(AExtComparer: IComparer<TValueType>);
    function Compare(const Left, Right: TValueType): Integer;
  end;

  { Examples:
      const
        Date1 : TDateTimeRec = (Year:2009; Month:05; Day:11);
        Date2 : TDateTimeRec = (Year:2009; Month:05; Day:11; Hour:05); }
  { Record type to define TDateTime compatible constants in readable way }		
  TDateTimeRec = record
    Year, Month, Day, Hour, Minute, Second, Millisecond : Word;

    class operator Implicit(const ADateTime : TDateTimeRec): TDateTime;
    class operator Implicit(const ADateTime : TDateTime): TDateTimeRec;
    class operator Implicit(const ADateTime : TDateTimeRec): String;
    class operator Implicit(const ADateTime : String): TDateTimeRec;
  end;

  { Check TDateTime correctness, convert to string etc }
  TDateTimeUtils = class
  public
    const
      NoDateStr = '';

    class function StdEuFormatSettings: TFormatSettings; static;

    class function IsCorrectDate(const t: TDateTime): boolean; static;
    class function ToStr(const t: TDateTime; ANoDateStr: string = NoDateStr): string; static;

    class function ToStringStd(const t: TDateTime): string; static;
    class function FromStringStd(const t: string; def: TDateTime = 0): TDateTime; static;
  end;

  TFuncConst<T,TResult> = reference to function (const Arg1: T): TResult;
  TFuncConst<T1,T2,TResult> = reference to function (const Arg1: T1; const Arg2: T2): TResult;
  TFuncConst<T1,T2,T3,TResult> = reference to function (const Arg1: T1; const Arg2: T2; const Arg3: T3): TResult;
  { Fill/fillRandom/Randomize and other tools for arrays }
  TArrayUtils = record
  public
    class procedure SaveToFileAsText<T>(const Arr: TArray<T>; const AFileName: string); static;
    class procedure SaveToFileAsBin<T>(const Arr: TArray<T>; const AFileName: string); static;
    class function Get<T>(const Arr: array of T):TArray<T>; overload; static;
    class function Get(const Arr: TStringDynArray):TArray<string>; overload; static;
    class procedure Randomize<T>(var Arr: TArray<T>); static;
    class procedure Inverse<T>(var Arr: TArray<T>; AStartIndex: integer = 0; ACount: integer = -1); static;
    class procedure Delete<T>(var Arr: TArray<T>; AFilter: TFuncConst<T,Boolean>); overload; static;
    class procedure Delete<T>(var Arr: TArray<T>; Index: integer); overload; static;
    class function Add<T>(const A,B: TArray<T>): TArray<T>; overload; static;
    class function Copy<T>(const Src: TArray<T>): TArray<T>; overload; static;
    class function Copy<T>(const Src: TArray<T>; StartIndex,Count: integer): TArray<T>; overload; static;
    class function Copy<T>(const Src: TArray<T>; ACopyFilter: TFuncConst<T,Boolean>): TArray<T>; overload; static;
    class function Equal<T>(const A,B: TArray<T>; AComparer: IEqualityComparer<T> = nil): Boolean; overload; static;
    class function Equal<T>(const A,B: TArray<T>; IndexA,IndexB,Count: integer; AComparer: IEqualityComparer<T> = nil): Boolean; overload; static;
    class procedure Append<T>(var Dst: TArray<T>; const Src: T); overload; static;
    class procedure Append<T>(var Dst: TArray<T>; const Src: TArray<T>); overload; static;
    class procedure Append<T>(var Dst: TArray<T>; const Src: TEnumerable<T>); overload; static;
    class procedure FillRandom(var Dst: TArray<byte>; Count: integer; AValRangeFrom,AValRangeTo: byte); overload; static;
    class procedure FillRandom(var Dst: TArray<integer>; Count: integer; AValRangeFrom,AValRangeTo: integer); overload; static;
    class procedure FillRandom(var Dst: TArray<double>; Count: integer; AValRangeFrom,AValRangeTo: double); overload; static;
    class procedure FillRandom(var Dst: TArray<string>; Count,ValMaxLen: integer); overload; static;
    class procedure Fill(var Dst: TArray<byte>; Count: integer; AValueStart,AValueInc: integer); overload; static;
    class procedure Fill(var Dst: TArray<integer>; Count: integer; AValueStart,AValueInc: integer); overload; static;
    class procedure Fill(var Dst: TArray<double>; Count: integer; AValueStart,AValueInc: double); overload; static;
    class procedure StableSort<T>(var Dst: TArray<T>; StartIndex,Count: integer; Comparer: IComparer<T>); static;
    class function Cut<T>(var Dst: TArray<T>; Capacity,StartIndex,Count: integer): integer; overload; static;
    class function Cut<T>(var Dst: TArray<T>; StartIndex,Count: integer): integer; overload; static;
    class function Slice<T>(const Src: TArray<T>; Capacity,StartIndex,Count: integer): TArray<T>; overload; static;
    class function Slice<T>(const Src: TArray<T>; StartIndex,Count: integer): TArray<T>; overload; static;
    class function Slice<T>(const Src: TArray<T>; CopyValue: TFunc<T,boolean>): TArray<T>; overload; static;
    { 9 1 7 2 5 8 -> [1-2] [5] [7-9] }
    class function Ranges(Src: TArray<integer>): TRangeEnumerable; overload; static;
    class function Ranges(Src: TEnumerable<integer>): TRangeEnumerable; overload; static;
    class function IndexOf<T>(const Item: T; const Src: TEnumerable<T>; AComparer: IEqualityComparer<T> = nil): integer; overload; static;
    class function IndexOf<T>(const Item: T; const Src: TArray<T>; AComparer: IEqualityComparer<T> = nil): integer; overload; static;
    class function IndexOf<T>(const Template, Data: TArray<T>; AComparer: IEqualityComparer<T> = nil): integer; overload; static;
    class function GetPtr<T>(const Src: TArray<T>): pointer; static;
    class function GetFromDynArray(const Src: TStringDynArray): TArray<string>; static;
    class function Sum(var Arr: double; Count: integer): double; overload; static;
    class function Sum(var Arr: integer; Count: integer): int64; overload; static;
    class function Sum(var Arr: int64; Count: integer): int64; overload; static;
    class function StartWith<T>(const Data,Template: TArray<T>; AComparer: IEqualityComparer<T> = nil): boolean; overload; static;
    class function EndsWith<T>(const Data,Template: TArray<T>; AComparer: IEqualityComparer<T> = nil): boolean; overload; static;
    class function Contains<T>(const Template: T; const Data: TArray<T>; AComparer: IEqualityComparer<T> = nil): boolean; overload; static;
    class function Contains<T>(const Template,Data: TArray<T>; AComparer: IEqualityComparer<T> = nil): boolean; overload; static;
    class function Sorted<T>(const Arr: TArray<T>; AComparer: IComparer<T> = nil): boolean; overload; static;
    class function Sorted<T>(const Arr: TArray<T>; AStartIndex,ACount: integer; AComparer: IComparer<T> = nil): boolean; overload; static;
    class procedure Sort<T>(var Arr: TArray<T>; AComparer: TComparison<T>); static;
  end;

  { Wrapper class to make any class type available through interface (that means that
    lifetime of the class will be controlled by reference counter) }
  TInterfacedObject<T: class> = class(TInterfacedObject, IInterfacedObject<T>)
  protected
    FData: T;

    function GetData: T;
    procedure SetData(const AData: T);
    function GetRefCount: integer;
  public
    constructor Create(AData: T);
    destructor Destroy; override;

    function Extract: T;

    property Data: T read GetData write SetData;
  end;

  { TInterfacedType provides interfaced access to any type.
    Unlike TInterfacedObject it will not destroy inner object (if T is object type). }
  TInterfacedType<T> = class(TInterfacedObject, IInterfacedObject<T>)
  protected
    FData: T;

    function GetData: T;
    procedure SetData(const AData: T);
    function GetRefCount: integer;
  public
    constructor Create(AData: T);

    function Extract: T;
  end;

  { IsValid and other utils }
  TGUIDUtils = class
  public
    const
      NullGuid: TGUID = '{00000000-0000-0000-0000-000000000000}';

    class function IsValid(const S: string): Boolean; static;
    class function TryStrToGuid(const S: string; out Dst: TGUID): boolean; static;
    class function StrToGuid(const S: string): TGUID; static;
    class function StrToGuidDef(const S: string; const Def: TGUID): TGUID; overload; static;
    class function StrToGuidDef(const S: string): TGUID; overload; static;

    class function IntToGuid(N: integer): TGUID; static;
    class function GuidToInt(const Src: TGUID): integer; static;

    class function GetNew: TGUID; static;
    class function GetNewAsString: string; static;
  end;

  {
    Collections of system functions.
    We call it Sys (not TSys), because
      Sys.FreeAndNil looks more natural than
      TSys.FreeAndNil
  }
  Sys = class
  public

    { Generic IfThen, compatible with any type.
      Example: Rect := Sys.IfThen(Condition, Rect1, Rect2); }
    class function IfThen<T>(ACondition: Boolean; AValueTrue,AValueFalse: T):T; static;

    { Generic swap function, compatible with any type.
      Example: Sys.Exchange(Rect1,Rect2); }
    class procedure Exchange<T>(var A,B: T); static; {$IFDEF UseInline}inline;{$ENDIF}

    { Generic version of FreeAndNil. Much safe than regular FreeAndNil, it accepts classes
      only and wrong use with other type will be reported as error in compile time.
      Example: Sys.FreeAndNil(Obj); }
    class procedure FreeAndNil<T: class>(var Obj: T); static;

    { Safe generic implementation of FillChar(A,SizeOf(A),0) :
      - Strictly typified, size is calculated by compiler.
      - Supports managed types (strings, interfaces, dynamic arrays and other managed
        types will be freed correctly) }
    class procedure Clear<T>(var R: T); static;

    { Generic function to check if value is withing range }
    class function ValueInRange<T>(AValue, AValueFrom, AValueTo: T): boolean; static;

    { Type specific functions to check if value is withing range (more efficient than generic ValueInRange) }
    class function InRange(const AValue, AValueFrom, AValueTo: integer): boolean; overload; static;
    class function InRange(const AValue, AValueFrom, AValueTo: double): boolean; overload; static;

    { Type specific functions to check if two ranges are overlapped }
    class function Overlapped(const AFrom,ATo, BFrom,BTo: integer): boolean; overload; static;
    class function Overlapped(const AFrom,ATo, BFrom,BTo: double): boolean; overload; static;

    class function Min(const A,B: integer): integer; overload; static;
    class function Min(const A,B,C: integer): integer; overload; static;
    class function Min(const Values: array of integer): integer; overload; static;
    class function Min(const Values: TArray<integer>): integer; overload; static;
    class function Max(const A,B: integer): integer; overload; static;
    class function Max(const A,B,C: integer): integer; overload; static;
    class function Max(const Values: array of integer): integer; overload; static;
    class function Max(const Values: TArray<integer>): integer; overload; static;

    class function Min(const A,B: double): double; overload; static;
    class function Min(const A,B,C: double): double; overload; static;
    class function Min(const Values: array of double): double; overload; static;
    class function Min(const Values: TArray<double>): double; overload; static;
    class function Max(const A,B: double): double; overload; static;
    class function Max(const A,B,C: double): double; overload; static;
    class function Max(const Values: array of double): double; overload; static;
    class function Max(const Values: TArray<double>): double; overload; static;

    class function GetPtr(const Values: TArray<byte>): pointer; overload; static;
  end;

  { Currency type utils }
  TCurrencyUtils = class
  public
    class function ToString(const Value: Currency; FractionalPart: boolean = False): string; reintroduce; static;
  end;

  { Extends any type by IsNull property. Compare operator will use case insensitive comparer for strings
    (unlike default comparer for strings in Delphi). }
  TBox<T> = record
  private
    type
      PT = ^T;
    var
      FValue: T;
      FHasValue: string;

    { AH: all "inline" directives are commented, because Delphi failed to compile it in
          release configuration (internal error). Reproduced in Delphi 10.1 }
    function GetValue: T;
    procedure SetValue(const AValue: T);
    function GetEmpty: boolean;
    function GetPointer: PT;

  public
    procedure Init; overload;
    procedure Init(const AValue: T); overload;

    procedure Clear;
    function Extract: T;

    { assign operators }
    class operator Implicit(const AValue: TBox<T>): T;
    class operator Implicit(const AValue: T): TBox<T>;
    { We can't disable assigning of variant to TBox and some wrong assignments will not be checked in compile time.
      Delphi has different rules for conversion of Null (exception generated) and Unassigned (converted to default value).
      We rely on Delphi in such conversion to keep default behaviour.
    //class operator Implicit(const AValue: Variant): TBox<T>;

    { compare operators }
    class operator Equal(const Left, Right: TBox<T>): Boolean;
    class operator Equal(const Left: TBox<T>; const Right: T): Boolean;
    class operator Equal(const Left: T; const Right: TBox<T>): Boolean;
    class operator NotEqual(const Left, Right: TBox<T>): Boolean;
    class operator LessThan(const Left,Right: TBox<T>): Boolean;
    class operator LessThanOrEqual(const Left,Right: TBox<T>): Boolean;
    class operator GreaterThan(const Left,Right: TBox<T>): Boolean;
    class operator GreaterThanOrEqual(const Left,Right: TBox<T>): Boolean;

    { basic math operators (available for numeric types) }
    class operator Add(Left: TBox<T>; Right: TBox<T>): TBox<T>;
    class operator Subtract(Left: TBox<T>; Right: TBox<T>): TBox<T>;
    class operator Multiply(Left: TBox<T>; Right: TBox<T>): TBox<T>;
    class operator Divide(Left: TBox<T>; Right: TBox<T>): TBox<T>;
    class operator IntDivide(Left: TBox<T>; Right: TBox<T>): TBox<T>;
    class operator Negative(Value: TBox<T>): TBox<T>;

    property Value: T read GetValue write SetValue;
    property Empty: boolean read GetEmpty;
    property ValuePtr: PT read GetPointer;
  end;

  { Simple generic class to keep record as object }
  TEnvelop<T> = class
  public
    Value: T;

    constructor Create; overload;
    constructor Create(AValue: T); overload;
  end;

  TForEachComponentBreakProc = reference to procedure(AComponent: TComponent; var ABreak: boolean);

  { Enumeration and other component-specific tools }
  TComponentUtils = class
  public

    { Enumerates all child components recursively. Returns True if canceled (ACancel is set True by ACallback) }
    class function ForEach(AStart: TComponent; ACallback: TProcVar1<TComponent, Boolean>): Boolean; overload; static;
    { Enumerates child components of type T recursively. Returns True if canceled (ACancel is set True by ACallback) }
    class function ForEach<T: class>(AStart: TComponent; ACallback: TProcVar1<T, Boolean>): Boolean; overload; static;
    { Enumerates all child components recursively }
    class procedure ForEach(AStart: TComponent; ACallback: TProc<TComponent>); overload; static;
    { Enumerates all child components of type T recursively }
    class procedure ForEach<T: class>(AStart: TComponent; ACallback: TProc<T>); overload; static;
    { Get all child components recursively }
    class function GetAll(AStart: TComponent): TArray<TComponent>; overload; static;
    { Get all child components of type T recursively }
    class function GetAll<T: class>(AStart: TComponent): TArray<T>; overload; static;
    { Generates unique component name }
    class function GetUniqueName: string; static;
    { Creates copy of the component }
    class function Copy<T: TComponent>(Src: T): T; static;
  end;

  { Executes custom action (procedure/method) when last instance goes out of scope (automatic finalization etc). }
  TOutOfScopeAction = record
  private
    FProc: IInterfacedObject<TObject>;

  public
    procedure Init(AProc: TProc);
  end;

  { Executes custom action (procedure/method) with specific parameter when last instance goes out of scope. }
  TOutOfScopeAction<T> = record
  private
    type
      TRunOnDestroy = class
      private
        FProc: TProc<T>;
        FValue: T;

      public
        constructor Create(AProc: TProc<T>; AValue: T);
        destructor Destroy; override;
      end;

    var
      FProc: IInterfacedObject<TRunOnDestroy>;

  public
    procedure Init(AProc: TProc<T>; AValue: T);
  end;

  TEventUtils = class
  public
    class function IsSameHandler(const A,B: TNotifyEvent): Boolean; overload; static;
    class function IsSameHandler(const A,B: TActionEvent): Boolean; overload; static;
    class function Equal<T>(const A,B: T): boolean; static;
  end;

  TDataSize = record
  private
    FSize: int64;

    function GetGb: double;
    function GetKb: double;
    function GetMb: double;
    function GetTb: double;
    procedure SetGb(const Value: double);
    procedure SetKb(const Value: double);
    procedure SetMb(const Value: double);
    procedure SetTb(const Value: double);
    function GetAsString: string;

  public
    const
      Kb = int64(1024);
      Mb = int64(1024)*Kb;
      Gb = int64(1024)*Mb;
      Tb = int64(1024)*Gb;

    procedure Init(const AValue: int64);
    procedure InitMb(const AValue: double);

    class function SizeToString(const AValue: int64): string; static;

    class operator Implicit(const AValue: TDataSize): int64;
    class operator Implicit(const AValue: int64): TDataSize;

    class operator Equal(const Left, Right: TDataSize): Boolean;
    class operator NotEqual(const Left, Right: TDataSize): Boolean;
    class operator LessThan(const Left,Right: TDataSize): Boolean;
    class operator LessThanOrEqual(const Left,Right: TDataSize): Boolean;
    class operator GreaterThan(const Left,Right: TDataSize): Boolean;
    class operator GreaterThanOrEqual(const Left,Right: TDataSize): Boolean;

    class operator Add(Left: TDataSize; Right: TDataSize): TDataSize;
    class operator Subtract(Left: TDataSize; Right: TDataSize): TDataSize;
    class operator Multiply(Left: TDataSize; Right: TDataSize): TDataSize;
    class operator Divide(Left: TDataSize; Right: TDataSize): TDataSize;
    class operator IntDivide(Left: TDataSize; Right: TDataSize): TDataSize;
    class operator Negative(Value: TDataSize): TDataSize;

    property SizeBytes: int64 read FSize write FSize;
    property SizeKb: double read GetKb write SetKb;
    property SizeMb: double read GetMb write SetMb;
    property SizeGb: double read GetGb write SetGb;
    property SizeTb: double read GetTb write SetTb;

    property AsString: string read GetAsString;
  end;

  TDebugUtils = class
  public

    { returns True if application is started from IDE for example }
    class function DebuggerIsAttached: boolean; static;
  end;

implementation

Uses
  adot.Arithmetic,
  adot.Tools.Rtti,
  adot.Tools.IO,
  adot.Strings,
  adot.Collections.Vectors;

{ THex }

class function THex.EncodedSizeChars(SourceSizeBytes: integer): integer;
begin
  Assert(SourceSizeBytes>=0);
  result := SourceSizeBytes shl 1;
end;

class function THex.EncodeByteH(Src: byte): char;
begin
  result := TwoHexLookup[Src][0];
end;

class function THex.EncodeByteL(Src: byte): char;
begin
  result := TwoHexLookup[Src][1];
end;

class function THex.DecodedSizeBytes(EncodedSizeChars: integer): integer;
begin
  Assert(EncodedSizeChars and 1=0);
  result := EncodedSizeChars shr 1;
end;

class function THex.DecodeHexChar(HexChar: Char): byte;
begin
  Assert(IsValid(HexChar));
  result := H2B[HexChar];
end;

{$IF Defined(NEXTGEN)}
  class function THex.Encode(const Buf; ByteBufSize: integer): String;
  const
    B2HConvert: array[0..15] of Byte = ($30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $41, $42, $43, $44, $45, $46);
  var
    I: Integer;
  begin
    SetLength(result, EncodedSizeChars(ByteBufSize));
    for I := 0 to ByteBufSize - 1 do
    begin
      Result[Low(Result) + I*2    ] := Char(B2HConvert[PByte(@Buf)[I] shr 4]);
      Result[Low(Result) + I*2 + 1] := Char(B2HConvert[PByte(@Buf)[I] and $0F]);
    end;
  end;
{$Else}
  class function THex.Encode(const Buf; ByteBufSize: integer): String;
  begin
    SetLength(result, EncodedSizeChars(ByteBufSize));
    BinToHex(@Buf, PChar(result), ByteBufSize);
  end;
{$EndIF}

{$If Defined(MSWindows)}
class function THex.EncodeAnsiString(const s: AnsiString): String;
begin
  { need this check only to range check error (when "check range check" is on) }
  if s='' then result := '' else
    result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;
{$EndIf}

class function THex.Encode<T>(const Value: T): String;
begin
  Result := Encode(Value, SizeOf(Value));
end;

class function THex.Encode(const s: TBytes): String;
begin
  { need this check only to range check error (when "check range check" is on) }
  if Length(s)=0 then result := '' else
    result := Encode(s[0], length(s));
end;

class function THex.Encode(const s: string): string;
begin
  { need this check only to range check error (when "check range check" is on) }
  if s='' then result := '' else
    result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

class function THex.Encode(const s: string; utf8: boolean): string;
begin
  if utf8 then
    result := Encode(TEncoding.UTF8.GetBytes(s))
  else
    result := Encode(s);
end;

{$IF Defined(NEXTGEN)}
  class procedure THex.Decode(const HexEncodedStr: String; var Buf);
  const
    H2BValidSet = ['0'..'9','A'..'F','a'..'f'];
    H2BConvert: array['0'..'f'] of SmallInt =
      ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
       -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
       -1,10,11,12,13,14,15);
  var
    I: Integer;
    C1,C2: Char;
  begin
    for I := 0 to DecodedSizeBytes(length(HexEncodedStr)) - 1 do
    begin
      C1 := HexEncodedStr[Low(String) + I * 2    ];
      C2 := HexEncodedStr[Low(String) + I * 2 + 1];
      if
        (C1 >= Low(H2BConvert)) and (C1 <= High(H2BConvert)) and (H2BConvert[C1] >=0 ) and
        (C2 >= Low(H2BConvert)) and (C2 <= High(H2BConvert)) and (H2BConvert[C2] >=0 )
      then
        PByte(@Buf)[I] := (H2BConvert[C1] shl 4) or H2BConvert[C2]
      else
        Break;
    end;
  end;
{$Else}
  class procedure THex.Decode(const HexEncodedStr: String; var Buf);
  begin
    HexToBin(PChar(HexEncodedStr), Buf, DecodedSizeBytes(length(HexEncodedStr)));
  end;
{$EndIf}

class function THex.Decode<T>(const HexEncodedStr: String): T;
begin
  Decode(HexEncodedStr, Result);
end;

{$If Defined(MSWindows)}
class function THex.DecodeAnsiString(const HexEncodedStr: String): AnsiString;
begin
  { need this check only to range check error (when "check range check" is on) }
  if HexEncodedStr='' then Result := '' else
  begin
    SetLength(Result, DecodedSizeBytes(length(HexEncodedStr))); { AnsiString is always 1 byte per char }
    Decode(HexEncodedStr, result[Low(result)]);
  end;
end;
{$EndIf}

class function THex.DecodeByte(H, L: Char): byte;
begin
  result := (DecodeHexChar(H) shl 4) or DecodeHexChar(L);
end;

class function THex.DecodeBytes(const HexEncodedStr: String): TBytes;
begin
  { need this check only to range check error (when "check range check" is on) }
  if HexEncodedStr='' then SetLength(result, 0) else
  begin
    SetLength(Result, DecodedSizeBytes(length(HexEncodedStr)));
    Decode(HexEncodedStr, Result[Low(Result)]);
  end;
end;

{$IF SizeOf(Char)=2}
class function THex.DecodeString(const HexEncodedStr: string): string;
begin
  { need this check only to range check error (when "check range check" is on) }
  if HexEncodedStr='' then result := '' else
  begin
    Assert(length(HexEncodedStr) and 3=0);
    SetLength(Result, length(HexEncodedStr) shr 2);
    Decode(HexEncodedStr, Result[Low(Result)]);
  end;
end;
{$ELSE}
class function THex.DecodeString(const HexEncodedStr: string): string;
var
  SizeInBytes: Integer;
begin
  { need this check only to range check error (when "check range check" is on) }
  if HexEncodedStr='' then result := '' else
  begin
    SizeInBytes := DecodedSizeBytes(length(HexEncodedStr));
    Assert(SizeInBytes mod SizeOf(Result[Low(Result)]) = 0);
    SetLength(Result, SizeInBytes div SizeOf(Result[Low(Result)]));
    Decode(HexEncodedStr, Result[Low(Result)]);
  end;
end;
{$IFEND}

class function THex.DecodeString(const HexEncodedStr: string; utf8: boolean): string;
begin
  if utf8 then
    result := TEncoding.UTF8.GetString( DecodeBytes(HexEncodedStr) )
  else
    result := DecodeString(HexEncodedStr);
end;

class function THex.IsValid(const C: Char): Boolean;
begin
  result := (c>=Low(H2B)) and (c<=High(H2B)) and (H2B[c]>=0);
end;

class function THex.IsValid(const HexEncodedStr: String; ZeroBasedStartIdx, Len: integer): Boolean;
var
  i: Integer;
begin
  if Len and 1<>0 then
    Exit(False);
  for i := ZeroBasedStartIdx to ZeroBasedStartIdx+Len-1 do
    if not IsValid(HexEncodedStr.Chars[i]) then
      Exit(False);
  Result := True;
end;

class function THex.IsValid(const HexEncodedStr: String): Boolean;
begin
  if Length(HexEncodedStr) and 1<>0 then
    Exit(False);
  Result := IsValid(HexEncodedStr, 0,Length(HexEncodedStr));
end;

class function THex.HexToInt64(const HexEncodedInt: String):Int64;
var
  i: Integer;
begin
  assert(IsValid(HexEncodedInt));
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
  assert(IsValid(HexEncodedInt));
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

class function THex.HexToCardinal(const HexEncodedCardinal: String):Cardinal;
var
  i: Integer;
begin
  assert(IsValid(HexEncodedCardinal));
  result := 0;
  for i := Low(HexEncodedCardinal) to High(HexEncodedCardinal) do
    result := (result shl 4) or Cardinal(H2B[HexEncodedCardinal[i]]);
end;

class function THex.HexToWord(const HexEncodedWord: String):Word;
begin
  result := Word(HexToCardinal(HexEncodedWord));
end;

class function THex.Int64ToHex(s: Int64): string;
var
  i,j: Integer;
begin
  SetLength(result, SizeOf(s)*2);
  for i := High(result) shr 1 downto 0 do
  begin
    J := S and $FF;
    S := S shr 8;
    Result[i*2 + Low(result)    ] := TwoHexLookup[J][0];
    Result[i*2 + Low(result) + 1] := TwoHexLookup[J][1];
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
  SetLength(result, SizeOf(s)*2);
  for i := SizeOf(s)-1 downto 0 do
  begin
    J := S and $FF;
    S := S shr 8;
    Result[i*2 + Low(result)    ] := TwoHexLookup[J][0];
    Result[i*2 + Low(result) + 1] := TwoHexLookup[J][1];
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

class function THex.CardinalToHex(s: cardinal): string;
var
  i,j: Integer;
begin
  SetLength(result, SizeOf(s)*2);
  for i := SizeOf(s)-1 downto 0 do
  begin
    J := S and $FF;
    S := S shr 8;
    Result[i*2 + Low(result)    ] := TwoHexLookup[J][0];
    Result[i*2 + Low(result) + 1] := TwoHexLookup[J][1];
  end;
end;

class function THex.WordToHex(s: word): string;
var
  i,j: Integer;
begin
  SetLength(result, SizeOf(s)*2);
  for i := SizeOf(s)-1 downto 0 do
  begin
    J := S and $FF;
    S := S shr 8;
    Result[i*2 + Low(result)    ] := TwoHexLookup[J][0];
    Result[i*2 + Low(result) + 1] := TwoHexLookup[J][1];
  end;
end;

{ TInvertedComparer<TValue> }

constructor TInvertedComparer<TValueType>.Create(AExtComparer: IComparer<TValueType>);
begin
  inherited Create;
  FExtComparer := AExtComparer;
  if FExtComparer=nil then
    FExtComparer := TComparer<TValueType>.Default;
end;

function TInvertedComparer<TValueType>.Compare(const Left, Right: TValueType): Integer;
begin
  result := -FExtComparer.Compare(Left, Right);
end;

{ TDateTimeRec }

class operator TDateTimeRec.Implicit(const ADateTime: TDateTime): TDateTimeRec;
begin
  with Result do
    DecodeDateTime(ADateTime, Year, Month, Day, Hour, Minute, Second, Millisecond);
end;

class operator TDateTimeRec.Implicit(const ADateTime: TDateTimeRec): TDateTime;
begin
  with ADateTime do
    Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);
end;

class operator TDateTimeRec.Implicit(const ADateTime: String): TDateTimeRec;
begin
  Result := StrToDateTime(ADateTime);
end;

class operator TDateTimeRec.Implicit(const ADateTime: TDateTimeRec): String;
begin
  Result := DateTimeToStr(ADateTime);
end;

{ TArrayUtils }

class function TArrayUtils.Copy<T>(const Src: TArray<T>): TArray<T>;
begin
  result := Copy<T>(Src, 0, Length(Src));
end;

class function TArrayUtils.Copy<T>(const Src: TArray<T>; StartIndex, Count: integer): TArray<T>;
var
  i: Integer;
begin
  SetLength(result, Count);
  if Count > 0 then
    if TRttiUtils.IsOrdinal<T> then
      System.Move(Src[StartIndex], Result[0], Count*SizeOf(T))
    else
      for i := 0 to Count-1 do
        result[i] := Src[i + StartIndex];
end;

class function TArrayUtils.Copy<T>(const Src: TArray<T>; ACopyFilter: TFuncConst<T, Boolean>): TArray<T>;
var
  i,j: Integer;
begin
  SetLength(result, Length(Src));
  j := 0;
  for i := 0 to High(Src) do
    if ACopyFilter(Src[i]) then
    begin
      result[j] := Src[i];
      inc(j);
    end;
  SetLength(result, j);
end;

class procedure TArrayUtils.Append<T>(var Dst: TArray<T>; const Src: T);
var i: integer;
begin
  i := Length(Dst);
  SetLength(Dst, i+1);
  Dst[i] := Src;
end;

class procedure TArrayUtils.Append<T>(var Dst: TArray<T>; const Src: TArray<T>);
var i,j: integer;
begin
  j := Length(Dst);
  SetLength(Dst, j + Length(Src));
  for I := 0 to High(Src) do
    Dst[I+J] := Src[I];
end;

class function TArrayUtils.Add<T>(const A, B: TArray<T>): TArray<T>;
var
  I,J: Integer;
begin
  SetLength(Result, Length(A) + Length(B));
  for I := 0 to Length(A)-1 do
    Result[I] := A[I];
  J := Length(A);
  for I := 0 to Length(B)-1 do
    Result[I+J] := B[I];
end;

class procedure TArrayUtils.Append<T>(var Dst: TArray<T>; const Src: TEnumerable<T>);
var
  I: integer;
  Value: T;
begin
  I := 0;
  for Value in Src do
    inc(I);
  SetLength(dst, Length(dst) + I);
  I := Length(dst) - I;
  for Value in Src do
  begin
    Dst[I] := Value;
    inc(I);
  end;
  Assert(I=Length(Dst));
end;

class function TArrayUtils.Contains<T>(const Template: T; const Data: TArray<T>; AComparer: IEqualityComparer<T>): boolean;
begin
  result := IndexOf<T>(Template, Data, AComparer) >= 0;
end;

class function TArrayUtils.Contains<T>(const Template, Data: TArray<T>; AComparer: IEqualityComparer<T>): boolean;
begin
  result := IndexOf<T>(Template, Data, AComparer) >= 0;
end;

class function TArrayUtils.Sorted<T>(const Arr: TArray<T>; AComparer: IComparer<T> = nil): boolean;
begin
  result := Sorted<T>(Arr, 0, Length(Arr), AComparer);
end;

class procedure TArrayUtils.Sort<T>(var Arr: TArray<T>; AComparer: TComparison<T>);
var
  C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(AComparer);
  TArray.Sort<T>(Arr, C);
end;

class function TArrayUtils.Sorted<T>(const Arr: TArray<T>; AStartIndex,ACount: integer; AComparer: IComparer<T> = nil): boolean;
var
  I: Integer;
begin
  if ACount <= 0 then
    Exit(True);
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  Assert((AStartIndex >= 0) and (AStartIndex + ACount - 1 < Length(Arr)));
  for I := AStartIndex to AStartIndex + ACount - 2 do
    if AComparer.Compare(Arr[I], Arr[I+1]) > 0 then
      Exit(False);
  result := True;
end;

class procedure TArrayUtils.Delete<T>(var Arr: TArray<T>; AFilter: TFuncConst<T, Boolean>);
var
  i,j: Integer;
begin
  j := -1;
  for i := 0 to High(Arr) do
    if AFilter(Arr[i]) then
    begin
      j := i;
      Break;
    end;
  if j<0 then
    Exit;
  for i := j+1 to High(Arr) do
    if not AFilter(Arr[i]) then
    begin
      Arr[j] := Arr[i];
      inc(j);
    end;
  SetLength(Arr, j);
end;

class procedure TArrayUtils.Delete<T>(var Arr: TArray<T>; Index: integer);
var
  I: Integer;
begin
  if (Index >= Low(Arr)) and (Index <= High(Arr)) then
  begin
    for I := Index to High(Arr)-1 do
      Arr[I] := Arr[I+1];
    SetLength(Arr, Length(Arr)-1);
  end;
end;

class function TArrayUtils.Equal<T>(const A, B: TArray<T>; IndexA, IndexB, Count: integer; AComparer: IEqualityComparer<T>): Boolean;
var
  I,J: Integer;
begin
  if (Length(A)-IndexA < Count) or (Length(B)-IndexB < Count) then
    Exit(False);
  if Count <= 0 then
    Exit(True);
  if TRttiUtils.IsOrdinal<T> and (AComparer=nil) then
    { A & B are not empty -> it is safe to use @A[0] / @B[0] }
    result := CompareMem(@A[IndexA], @B[IndexB], Count*SizeOF(T))
  else
  begin
    if AComparer=nil then
      AComparer := TEqualityComparer<T>.Default;
    for i := 0 to Count-1 do
      if not AComparer.Equals(A[i+IndexA], B[i+IndexB]) then
        Exit(False);
    result := True;
  end;
end;

class function TArrayUtils.Equal<T>(const A, B: TArray<T>; AComparer: IEqualityComparer<T>): Boolean;
var
  i: Integer;
begin
  result := Length(A)=Length(B);
  if result and (Length(A) > 0) then
    if TRttiUtils.IsOrdinal<T> and (AComparer=nil) then
      { A & B are not empty -> it is safe to use @A[0] / @B[0] }
      result := CompareMem(@A[0], @B[0], Length(A)*SizeOF(T))
    else
    begin
      if AComparer=nil then
        AComparer := TEqualityComparer<T>.Default;
      for i := 0 to High(A) do
        if not AComparer.Equals(A[i], B[i]) then
          Exit(False);
    end;
end;

class procedure TArrayUtils.Fill(var Dst: TArray<byte>; Count, AValueStart, AValueInc: integer);
var
  I: Integer;
begin
  if Count >= 0 then
    SetLength(Dst, Count);
  for I := 0 to High(Dst) do
  begin
    Dst[I] := AValueStart;
    inc(AValueStart, AValueInc);
  end;
end;

class procedure TArrayUtils.Fill(var Dst: TArray<integer>; Count, AValueStart, AValueInc: integer);
var
  I: Integer;
begin
  if Count >= 0 then
    SetLength(Dst, Count);
  for I := 0 to High(Dst) do
  begin
    Dst[I] := AValueStart;
    inc(AValueStart, AValueInc);
  end;
end;

class procedure TArrayUtils.Fill(var Dst: TArray<double>; Count: integer; AValueStart, AValueInc: double);
var
  I: Integer;
begin
  if Count >= 0 then
    SetLength(Dst, Count);
  for I := 0 to High(Dst) do
  begin
    Dst[I] := AValueStart;
    AValueStart := AValueStart + AValueInc;
  end;
end;

class procedure TArrayUtils.FillRandom(var Dst: TArray<byte>; Count: integer; AValRangeFrom, AValRangeTo: byte);
var
  I: Integer;
begin
  if Count >= 0 then
    SetLength(Dst, Count);
  for I := 0 to High(Dst) do
    Dst[I] := AValRangeFrom + Random(integer(AValRangeTo)-integer(AValRangeFrom)+1);
end;

class procedure TArrayUtils.FillRandom(var Dst: TArray<integer>; Count, AValRangeFrom, AValRangeTo: integer);
var
  I: Integer;
begin
  if Count >= 0 then
    SetLength(Dst, Count);
  for I := 0 to High(Dst) do
    Dst[I] := AValRangeFrom + Random(AValRangeTo-AValRangeFrom+1);
end;

class procedure TArrayUtils.FillRandom(var Dst: TArray<string>; Count, ValMaxLen: integer);
var
  i: Integer;
begin
  if Count >=0 then
    SetLength(Dst, Count);
  if ValMaxLen <= 0 then
    ValMaxLen := 10;
  for i := 0 to High(Dst) do
    Dst[i] := TStr.Random(System.Random(ValMaxLen), 'a','z');
end;

class procedure TArrayUtils.FillRandom(var Dst: TArray<double>; Count: integer; AValRangeFrom, AValRangeTo: double);
var
  i: Integer;
begin
  if Count >=0 then
    SetLength(Dst, Count);
  for I := 0 to High(Dst) do
    Dst[I] := AValRangeFrom + Random*(AValRangeTo-AValRangeFrom);
end;

class function TArrayUtils.Get(const Arr: TStringDynArray): TArray<string>;
var
  I: Integer;
begin
  SetLength(result, Length(Arr));
  for I := Low(Arr) to High(Arr) do
    result[I] := Arr[I];
end;

class function TArrayUtils.Get<T>(const Arr: array of T): TArray<T>;
var
  i: Integer;
begin
  SetLength(result, Length(Arr));
  for i := 0 to High(result) do
    result[i] := Arr[i];
end;

class function TArrayUtils.GetFromDynArray(const Src: TStringDynArray): TArray<string>;
var
  I: Integer;
begin
  SetLEngth(result, Length(Src));
  for I := Low(Src) to High(Src) do
    result[I] := Src[I];
end;

class function TArrayUtils.GetPtr<T>(const Src: TArray<T>): pointer;
begin
  if Length(Src)=0
    then result := nil
    else result := @Src[0];
end;

class function TArrayUtils.IndexOf<T>(const Item: T; const Src: TEnumerable<T>; AComparer: IEqualityComparer<T> = nil): integer;
var
  V: T;
begin
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultEqualityComparer<T>;
  result := 0;
  for V in Src do
    if AComparer.Equals(V, Item) then
      Exit
    else
      inc(result);
  result := -1;
end;

class function TArrayUtils.IndexOf<T>(const Item: T; const Src: TArray<T>; AComparer: IEqualityComparer<T> = nil): integer;
var
  I: integer;
begin
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultEqualityComparer<T>;
  for I := 0 to High(Src) do
    if AComparer.Equals(Src[I], Item) then
      Exit(I);
  result := -1;
end;

class function TArrayUtils.IndexOf<T>(const Template, Data: TArray<T>; AComparer: IEqualityComparer<T>): integer;
var
  I,J: integer;
begin
  result := -1;
  if (Length(Data)=0) or (Length(Data) < Length(Template)) then
    Exit;
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultEqualityComparer<T>;
  for I := 0 to Length(Data)-Length(Template) do
    if AComparer.Equals(Data[I], Template[0]) then
    begin
      result := I;
      for J := 1 to High(Template) do
        if not AComparer.Equals(Data[I+J], Template[J]) then
        begin
          result := -1;
          break;
        end;
      if result >= 0 then
        break;
    end;
end;

class procedure TArrayUtils.Inverse<T>(var Arr: TArray<T>; AStartIndex, ACount: integer);
var
  i,EndIndex: Integer;
  Value: T;
begin
  if ACount < 0 then
    ACount := Length(Arr);
  EndIndex := AStartIndex + ACount - 1;
  for i := 0 to ACount div 2-1 do
  begin
    Value := Arr[AStartIndex];
    Arr[AStartIndex] := Arr[EndIndex];
    Arr[EndIndex] := Value;
    inc(AStartIndex);
    dec(EndIndex);
  end;
end;

class procedure TArrayUtils.Randomize<T>(var Arr: TArray<T>);
var
  I,J,N: Integer;
  V: T;
begin
  N := Length(Arr);
  for I := 0 to N-1 do
  begin
    J      := Random(N);
    V      := Arr[I];
    Arr[I] := Arr[J];
    Arr[J] := V;
  end;
end;

class function TArrayUtils.Ranges(Src: TEnumerable<integer>): TRangeEnumerable;
begin
  result := Ranges(Src.ToArray);
end;

class function TArrayUtils.Ranges(Src: TArray<integer>): TRangeEnumerable;
begin
  result.Init(Src);
end;

class procedure TArrayUtils.SaveToFileAsBin<T>(const Arr: TArray<T>; const AFileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    Stream.Write(Arr[Low(Arr)], SizeOf(T)*Length(Arr))
  finally
    Stream.Free;
  end;
end;

class procedure TArrayUtils.SaveToFileAsText<T>(const Arr: TArray<T>; const AFileName: string);
var
  Stream: TFileStream;
  Writer: TStreamWriter;
  Value: T;
begin
  Stream := nil;
  Writer := nil;
  try
    Stream := TFileStream.Create(AFileName, fmCreate);
    Writer := TStreamWriter.Create(Stream);
    for Value in Arr do
      Writer.Write( TValue.From(Value).ToString );
  finally
    Writer.Free;
    Stream.Free;
  end;
end;

class procedure TArrayUtils.StableSort<T>(var Dst: TArray<T>; StartIndex, Count: integer; Comparer: IComparer<T>);
var
  Idx: TArray<integer>;
  Src,Tmp: TArray<T>;
  Cmp: IComparer<integer>;
  I: Integer;
begin
  SetLength(Idx, Count);
  for I := 0 to High(Idx) do
    Idx[I] := I + StartIndex;
  Src := Dst;
  Cmp := TDelegatedComparer<integer>.Create(
    function(const A,B: integer): integer
    begin
      result := Comparer.Compare(Src[Idx[A]], Src[Idx[B]]);
      if result=0 then
        result := Idx[B]-Idx[A];
    end);
  TArray.Sort<integer>(Idx, Cmp);
  SetLength(Tmp, Count);
  for I := 0 to High(Tmp) do
    Tmp[I] := Dst[Idx[I]];
  for I := 0 to High(Tmp) do
    Dst[I+StartIndex] := Tmp[I];
end;

class function TArrayUtils.StartWith<T>(const Data, Template: TArray<T>; AComparer: IEqualityComparer<T>): boolean;
begin
  result := (Length(Data) >= Length(Template)) and
    TArrayUtils.Equal<T>(Data, Template, 0, 0, Length(Template), AComparer);
end;

class function TArrayUtils.EndsWith<T>(const Data, Template: TArray<T>; AComparer: IEqualityComparer<T>): boolean;
begin
  result := (Length(Data) >= Length(Template)) and
    TArrayUtils.Equal<T>(Data, Template, Length(Data)-Length(Template), 0, Length(Template), AComparer);
end;

class function TArrayUtils.Sum(var Arr: integer; Count: integer): int64;
var
  p: ^integer;
begin
  p := @Arr;
  result := 0;
  while Count > 0 do
  begin
    dec(Count);
    inc(result, p^);
    inc(p);
  end;
end;

class function TArrayUtils.Sum(var Arr: int64; Count: integer): int64;
var
  p: ^int64;
begin
  p := @Arr;
  result := 0;
  while Count > 0 do
  begin
    dec(Count);
    inc(result, p^);
    inc(p);
  end;
end;

class function TArrayUtils.Sum(var Arr: double; Count: integer): double;
var
  p: ^double;
begin
  p := @Arr;
  result := 0;
  while Count > 0 do
  begin
    dec(Count);
    result := result + p^;
    inc(p);
  end;
end;

class function TArrayUtils.Cut<T>(var Dst: TArray<T>; Capacity,StartIndex,Count: integer): integer;
var
  I: Integer;
begin
  if StartIndex >= Capacity then
    Exit(0);
  result := Count;
  if StartIndex < 0 then
  begin
    inc(result, StartIndex);
    StartIndex := 0;
  end;
  result := Max(Min(result, Capacity-StartIndex), 0);
  if result = 0 then
    Exit;
  if TRttiUtils.IsOrdinal<T> then
    System.Move(Dst[StartIndex+result], Dst[StartIndex], (Capacity-StartIndex-result)*SizeOf(T))
  else
    for I := 0 to result-1 do
      Dst[I+StartIndex] := Dst[I+StartIndex+result];
end;

class function TArrayUtils.Cut<T>(var Dst: TArray<T>; StartIndex,Count: integer): integer;
begin
  result := Cut<T>(Dst, Length(Dst), StartIndex, Count);
end;

class function TArrayUtils.Slice<T>(const Src: TArray<T>; Capacity,StartIndex,Count: integer): TArray<T>;
var
  I: Integer;
begin
  if StartIndex >= Capacity then
    Count := 0
  else
  begin
    if StartIndex < 0 then
    begin
      inc(Count, StartIndex);
      StartIndex := 0;
    end;
    Count := Min(Count, Capacity-StartIndex);
  end;
  if Count <= 0 then
    SetLength(result, 0)
  else
  begin
    SetLength(result, Count);
    if TRttiUtils.IsOrdinal<T> then
      System.Move(Src[StartIndex], result[0], Count*SizeOf(T))
    else
      for I := 0 to Count-1 do
        result[I] := Src[I+StartIndex];
  end;
end;

class function TArrayUtils.Slice<T>(const Src: TArray<T>; StartIndex,Count: integer): TArray<T>;
begin
  result := Slice<T>(Src, Length(Src), StartIndex, Count);
end;

class function TArrayUtils.Slice<T>(const Src: TArray<T>; CopyValue: TFunc<T, boolean>): TArray<T>;
var
  V: TArr<T>;
  I: Integer;
begin
  V.Clear;
  for I := Low(Src) to High(Src) do
    if CopyValue(Src[I]) then
      V.Add(Src[I]);
  V.TrimExcess;
  result := V.Items;
end;

{ TDateTimeUtils }

class function TDateTimeUtils.ToStringStd(const t: TDateTime): string;
var
  F: TFormatSettings;
begin
  F := StdEuFormatSettings;
  result := DateTimeToStr(t, F);
end;

class function TDateTimeUtils.FromStringStd(const t: string; def: TDateTime = 0): TDateTime;
var
  F: TFormatSettings;
begin
  F := StdEuFormatSettings;
  if not TryStrToDateTime(t, result, F) then
    result := Def;
end;

class function TDateTimeUtils.IsCorrectDate(const t: TDateTime): boolean;
begin
  result :=
    (Trunc(t)<>0) and  {       0 (30.12.1899) as empty value }
    (YearOf(t)>0);     { -700000 (00.00.0000) as empty value + Delphi supports only "01.01.01" and later }
end;

class function TDateTimeUtils.StdEuFormatSettings: TFormatSettings;
begin
  result := TFormatSettings.Create;
  result.CurrencyString            := 'eur';
  result.CurrencyFormat            := 2;
  result.CurrencyDecimals          := 2;
  result.DateSeparator             := '.';
  result.TimeSeparator             := '.';
  result.ListSeparator             := ';';
  result.ShortDateFormat           := 'dd/MM/yyyy';
  result.LongDateFormat            := 'dddd d/ MMMM yyyy';
  result.TimeAMString              := 'a.m.';
  result.TimePMString              := 'p.m.';
  result.ShortTimeFormat           := 'hh:mm';
  result.LongTimeFormat            := 'hh:mm:ss';
  result.ThousandSeparator         := ' ';
  result.DecimalSeparator          := '.'; { not EU, but std in math etc }
  result.TwoDigitYearCenturyWindow := 50; {h32}
  result.NegCurrFormat             := 9;
  result.NormalizedLocaleName      := '';
end;

class function TDateTimeUtils.ToStr(const t: TDateTime; ANoDateStr: string): string;
begin
  if IsCorrectDate(t) then
    result := DateToStr(t)
  else
    result := ANoDateStr;
end;

{ TInterfacedObject<T> }

constructor TInterfacedObject<T>.Create(AData: T);
begin
  inherited Create;
  FData := AData;
end;

destructor TInterfacedObject<T>.Destroy;
begin
  Sys.FreeAndNil(FData);
  inherited;
end;

function TInterfacedObject<T>.Extract: T;
begin
  result := FData;
  FData := nil;
end;

function TInterfacedObject<T>.GetData: T;
begin
  result := FData;
end;

procedure TInterfacedObject<T>.SetData(const AData: T);
begin
  if (FData<>nil) and (FData<>AData) then
    Sys.FreeAndNil(FData);
  FData := AData;
end;

function TInterfacedObject<T>.GetRefCount: integer;
begin
  result := RefCount;
end;

{ TGUIDUtils }

class function TGUIDUtils.IntToGuid(N: integer): TGUID;
begin
  {$If SizeOf(integer)<>4}
    {$MESSAGE ERROR 'unexpected size of integer type' }
  {$EndIf}
  result := Default(TGUID);
  result.D1 := cardinal(N);
end;

class function TGUIDUtils.GuidToInt(const Src: TGUID): integer;
begin
  {$If SizeOf(integer)<>4}
    {$MESSAGE ERROR 'unexpected size of integer type' }
  {$EndIf}
  result := integer(Src.D1);
end;

class function TGUIDUtils.IsValid(const S: string): Boolean;
var
  i: Integer;
begin
  (*  {41D3CDB4-1249-41CF-91E8-52D2C2EDC314}  *)
  i := Length(S);
  result := (i = 38) and
    (S.Chars[  0] = '{') and
    (S.Chars[i-1] = '}') and
    (S.Chars[  9] = '-') and
    (S.Chars[ 14] = '-') and
    (S.Chars[ 19] = '-') and
    (S.Chars[ 24] = '-') and
    THex.IsValid(S,  1, 8) and
    THex.IsValid(S, 10, 4) and
    THex.IsValid(S, 15, 4) and
    THex.IsValid(S, 20, 4) and
    THex.IsValid(S, 25,12);
end;

class function TGUIDUtils.GetNew: TGUID;
begin
  CreateGUID(Result);
end;

class function TGUIDUtils.GetNewAsString: string;
var G: TGUID;
begin
  CreateGUID(G);
  result := GuidToString(G);
end;

class function TGUIDUtils.TryStrToGuid(const S: string; out Dst: TGUID): boolean;
var V: string;
begin
  V := Trim(S);
  result := IsValid(V);
  if result then
    Dst := StringToGuid(V);
end;

class function TGUIDUtils.StrToGuid(const S: string): TGUID;
begin
  result := StringToGuid(Trim(S));
end;

class function TGUIDUtils.StrToGuidDef(const S: string): TGUID;
begin
  result := StrToGuidDef(S, NullGuid);
end;

class function TGUIDUtils.StrToGuidDef(const S: string; const Def: TGUID): TGUID;
var V: string;
begin
  V := Trim(S);
  if IsValid(V)
    then result := StringToGuid(V)
    else result := Def;
end;

{ TCurrencyUtils }

class function TCurrencyUtils.ToString(const Value: currency; FractionalPart: boolean): string;
begin
  result := FormatCurr( IfThen(FractionalPart, '#,##', '#,'), Value);
end;

{ TBox<T> }

procedure TBox<T>.Init;
begin
  Self := Default(TBox<T>);
end;

procedure TBox<T>.Init(const AValue: T);
begin
  Self := Default(TBox<T>);
  Value := AValue;
end;

procedure TBox<T>.Clear;
begin
  Self := Default(TBox<T>);
end;

function TBox<T>.GetValue: T;
begin
  if Empty then
    raise EInvalidOperation.Create('No value to read');
  result := FValue;
end;

procedure TBox<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
  FHasValue := '1';
end;

function TBox<T>.GetEmpty: boolean;
begin
  result := FHasValue='';
end;

function TBox<T>.GetPointer: PT;
begin
  result := @FValue;
end;

class operator TBox<T>.GreaterThan(const Left, Right: TBox<T>): Boolean;
begin
  { we have LessThan implementation already }
  Result := Right < Left;
end;

class operator TBox<T>.GreaterThanOrEqual(const Left, Right: TBox<T>): Boolean;
begin
  { we have LessThanOrEqual implementation already }
  result := Right <= Left;
end;

class operator TBox<T>.Equal(const Left, Right: TBox<T>): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if Left.Empty or Right.Empty then
    Result := Left.Empty = Right.Empty
  else
  begin
    Comparer := TComparerUtils.DefaultEqualityComparer<T>;
    Result := Comparer.Equals(Left.Value, Right.Value);
  end;
end;

class operator TBox<T>.Equal(const Left: TBox<T>; const Right: T): Boolean;
var
  Comparer: IEqualityComparer<T>;
begin
  if Left.Empty then
    result := False
  else
  begin
    Comparer := TComparerUtils.DefaultEqualityComparer<T>;
    Result := Comparer.Equals(Left.Value, Right);
  end
end;

class operator TBox<T>.Equal(const Left: T; const Right: TBox<T>): Boolean;
begin
  { We have implementation for (Left: TBox<T>; Right: T) already. }
  Result := Right=Left;
end;

function TBox<T>.Extract: T;
begin
  Assert(not Empty);
  result := Value;
  Clear;
end;

class operator TBox<T>.NotEqual(const Left, Right: TBox<T>): Boolean;
begin
  result := not (Left=Right);
end;

class operator TBox<T>.Implicit(const AValue: T): TBox<T>;
begin
  result.Init(AValue);
end;

class operator TBox<T>.Implicit(const AValue: TBox<T>): T;
begin
  result := AValue.Value;
end;

class operator TBox<T>.LessThan(const Left, Right: TBox<T>): Boolean;
var
  C: IComparer<T>;
begin
  { Null < any real value }
  if Left.Empty then
    result := not Right.Empty
  else
  if Right.Empty then
    Result := False
  else
  begin
    C := TComparerUtils.DefaultComparer<T>;
    result := C.Compare(Left.Value, Right.Value) < 0;
  end;
end;

class operator TBox<T>.LessThanOrEqual(const Left, Right: TBox<T>): Boolean;
var
  C: IComparer<T>;
begin
  { Null < any real value }
  if Left.Empty then
    Result := True
  else
  if Right.Empty then
    Result := False
  else
  begin
    C := TComparerUtils.DefaultComparer<T>;
    result := C.Compare(Left.Value, Right.Value) <= 0;
  end;
end;

class operator TBox<T>.Add(Left, Right: TBox<T>): TBox<T>;
begin
  if Left.Empty or Right.Empty then
    raise Exception.Create('Bad operation');
  result := TArithmeticUtils<T>.DefaultArithmetic.Add(Left, Right);
end;

class operator TBox<T>.Divide(Left, Right: TBox<T>): TBox<T>;
begin
  if Left.Empty or Right.Empty then
    raise Exception.Create('Bad operation');
  result := TArithmeticUtils<T>.DefaultArithmetic.Divide(Left, Right);
end;

class operator TBox<T>.IntDivide(Left, Right: TBox<T>): TBox<T>;
begin
  result := Left / Right;
end;

class operator TBox<T>.Subtract(Left, Right: TBox<T>): TBox<T>;
begin
  if Left.Empty or Right.Empty then
    raise Exception.Create('Bad operation');
  result := TArithmeticUtils<T>.DefaultArithmetic.Subtract(Left, Right);
end;

class operator TBox<T>.Multiply(Left, Right: TBox<T>): TBox<T>;
begin
  if Left.Empty or Right.Empty then
    raise Exception.Create('Bad operation');
  result := TArithmeticUtils<T>.DefaultArithmetic.Multiply(Left, Right);
end;

class operator TBox<T>.Negative(Value: TBox<T>): TBox<T>;
begin
  if Value.Empty then
    raise Exception.Create('Bad operation');
  result := TArithmeticUtils<T>.DefaultArithmetic.Negative(Value);
end;

{ TComponentUtils }

class function TComponentUtils.ForEach(AStart: TComponent; ACallback: TProcVar1<TComponent, Boolean>): Boolean;
var
  i: Integer;
begin
  if AStart<>nil then
  begin

    { check if canceled by callback function }
    result := False;
    ACallback(AStart, result);
    if result then
      Exit(False);

    { check if any subsearch canceled }
    for i := AStart.ComponentCount-1 downto 0 do
      if not ForEach(AStart.Components[i], ACallback) then
        Exit(False);
  end;
  result := True;
end;

class function TComponentUtils.Copy<T>(Src: T): T;
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.WriteComponent(Src);
    MemoryStream.Seek(0, TSeekOrigin.soBeginning);
    Result := MemoryStream.ReadComponent(nil) as T;
  finally
    MemoryStream.free;
  end;
end;

class procedure TComponentUtils.ForEach(AStart: TComponent; ACallback: TProc<TComponent>);
var
  i: Integer;
begin
  if AStart=nil then
    Exit;
  ACallback(AStart);
  for i := AStart.ComponentCount-1 downto 0 do
    ForEach(AStart.Components[i], ACallback);
end;

class function TComponentUtils.ForEach<T>(AStart: TComponent; ACallback: TProcVar1<T, Boolean>): Boolean;
begin
  result := ForEach(AStart,
    procedure(C: TComponent; var Break: boolean)
    begin
      if C is T then
        ACallback(T(C), Break);
    end);
end;

class procedure TComponentUtils.ForEach<T>(AStart: TComponent; ACallback: TProc<T>);
begin
  ForEach(AStart,
    procedure(C: TComponent)
    begin
      if C is T then
        ACallback(T(C));
    end);
end;

class function TComponentUtils.GetAll(AStart: TComponent): TArray<TComponent>;
var
  V: TArr<TComponent>;
begin
  V.Clear;
  V.Capacity := AStart.ComponentCount;
  ForEach(AStart,
    procedure(C: TComponent)
    begin
      V.Add(C);
    end);
  V.TrimExcess;
  Result := V.Items;
end;

class function TComponentUtils.GetAll<T>(AStart: TComponent): TArray<T>;
var
  V: TArr<T>;
begin
  V.Clear;
  V.Capacity := AStart.ComponentCount;
  ForEach(AStart,
    procedure(C: TComponent)
    begin
      if C is T then
        V.Add(T(C));
    end);
  V.TrimExcess;
  result := V.Items;
end;

class function TComponentUtils.GetUniqueName: string;
begin
  result := GuidToString(TGUIDUtils.GetNew).Replace('-','', [rfReplaceAll]);
  result := 'G' + result.Substring(1, Length(result)-2);
end;

{ TOutOfScopeAction.TOnDestroyRunner }

type
  TOnDestroyRunner = class
  private
    FProc: TProc;

    constructor Create(AProc: TProc);
    destructor Destroy; override;
  end;

constructor TOnDestroyRunner.Create(AProc: TProc);
begin
  FProc := AProc;
end;

destructor TOnDestroyRunner.Destroy;
begin
  if Assigned(FProc) then
    FProc;
  inherited;
end;

{ TOutOfScopeAction }

procedure TOutOfScopeAction.Init(AProc: TProc);
begin
  Self := Default(TOutOfScopeAction);
  FProc := TInterfacedObject<TObject>.Create(TOnDestroyRunner.Create(AProc));
end;

{ TOutOfScopeAction<T> }

procedure TOutOfScopeAction<T>.Init(AProc: TProc<T>; AValue: T);
begin
  Self := Default(TOutOfScopeAction<T>);
  FProc := TInterfacedObject<TRunOnDestroy>.Create(TRunOnDestroy.Create(AProc, AValue));
end;

{ TOutOfScopeAction<T>.TRunOnDestroy }

constructor TOutOfScopeAction<T>.TRunOnDestroy.Create(AProc: TProc<T>; AValue: T);
begin
  FProc := AProc;
  FValue := AValue;
end;

destructor TOutOfScopeAction<T>.TRunOnDestroy.Destroy;
begin
  FProc(FValue);
  inherited;
end;

{ TEventUtils }

class function TEventUtils.IsSameHandler(const A, B: TNotifyEvent): Boolean;
begin
  result := CompareMem(@A, @B, SizeOF(TNotifyEvent));
end;

class function TEventUtils.Equal<T>(const A, B: T): boolean;
begin
  result := CompareMem(@A, @B, SizeOF(T));
end;

class function TEventUtils.IsSameHandler(const A, B: TActionEvent): Boolean;
begin
  result := CompareMem(@A, @B, SizeOF(TActionEvent));
end;

{ TFun }

class procedure Sys.Clear<T>(var R: T);
begin
  R := Default(T);
end;

class procedure Sys.Exchange<T>(var A, B: T);
var C: T;
begin
  C := A;
  A := B;
  B := C;
end;

class procedure Sys.FreeAndNil<T>(var Obj: T);
begin
  {$IF Defined(AUTOREFCOUNT)}
    Obj := nil;
  {$ELSE}
    { unlike SysUtils.FreeAndNil we call destructor first and only after set to nil }
    if Obj<>nil then
    begin
      Obj.Destroy;
      Obj := nil;
    end;
  {$ENDIF}
end;

class function Sys.GetPtr(const Values: TArray<byte>): pointer;
begin
  if Length(Values)=0
    then result := nil
    else result := @Values[0];
end;

class function Sys.IfThen<T>(ACondition: Boolean; AValueTrue, AValueFalse: T): T;
begin
  if ACondition then result := AValueTrue else result := AValueFalse;
end;

class function Sys.InRange(const AValue, AValueFrom, AValueTo: integer): boolean;
begin
  if AValueFrom <= AValueTo then
    result := (AValue >= AValueFrom) and (AValue <= AValueTo)
  else
    result := (AValue >= AValueTo) and (AValue <= AValueFrom);
end;

class function Sys.InRange(const AValue, AValueFrom, AValueTo: double): boolean;
begin
  if AValueFrom <= AValueTo then
    result := (AValue >= AValueFrom) and (AValue <= AValueTo)
  else
    result := (AValue >= AValueTo) and (AValue <= AValueFrom);
end;

class function Sys.ValueInRange<T>(AValue, AValueFrom, AValueTo: T): boolean;
var
  Comparer: IComparer<T>;
begin
  Comparer := TComparerUtils.DefaultComparer<T>;
  if Comparer.Compare(AValueFrom, AValueTo) <= 0 then
    Result := (Comparer.Compare(AValue, AValueFrom) >= 0) and (Comparer.Compare(AValue, AValueTo) <= 0)
  else
    Result := (Comparer.Compare(AValue, AValueTo) >= 0) and (Comparer.Compare(AValue, AValueFrom) <= 0);
end;

class function Sys.Max(const A, B, C: double): double;
begin
  Result := A;
  if B > Result then
    Result := B;
  if C > Result then
    Result := C;
end;

class function Sys.Max(const A, B: double): double;
begin
  if A >= B then
    result := A
  else
    result := B;
end;

class function Sys.Max(const A, B: integer): integer;
begin
  if A >= B then
    result := A
  else
    result := B;
end;

class function Sys.Min(const Values: array of integer): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function Sys.Min(const Values: TArray<integer>): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function Sys.Max(const Values: array of integer): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

class function Sys.Max(const Values: TArray<integer>): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

class function Sys.Max(const A, B, C: integer): integer;
begin
  Result := A;
  if B > Result then
    Result := B;
  if C > Result then
    Result := C;
end;

class function Sys.Min(const A, B, C: double): double;
begin
  Result := A;
  if B < Result then
    Result := B;
  if C < Result then
    Result := C;
end;

class function Sys.Min(const A, B: double): double;
begin
  if A <= B then
    result := A
  else
    result := B;
end;

class function Sys.Min(const A, B: integer): integer;
begin
  if A <= B then
    result := A
  else
    result := B;
end;

class function Sys.Min(const A, B, C: integer): integer;
begin
  Result := A;
  if B < Result then
    Result := B;
  if C < Result then
    Result := C;
end;

class function Sys.Overlapped(const AFrom, ATo, BFrom, BTo: integer): boolean;
begin
  if AFrom <= ATo then
    if BFrom <= BTo then
      result := (AFrom <= BTo) and (BFrom <= ATo)
    else
      result := (AFrom <= BFrom) and (BTo <= ATo)
  else
    if BFrom <= BTo then
      result := (ATo <= BTo) and (BFrom <= AFrom)
    else
      result := (ATo <= BFrom) and (BTo <= AFrom);
end;

class function Sys.Overlapped(const AFrom, ATo, BFrom, BTo: double): boolean;
begin
  if AFrom <= ATo then
    if BFrom <= BTo then
      result := (AFrom <= BTo) and (BFrom <= ATo)
    else
      result := (AFrom <= BFrom) and (BTo <= ATo)
  else
    if BFrom <= BTo then
      result := (ATo <= BTo) and (BFrom <= AFrom)
    else
      result := (ATo <= BFrom) and (BTo <= AFrom);
end;

class function Sys.Min(const Values: array of double): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function Sys.Min(const Values: TArray<double>): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function Sys.Max(const Values: array of double): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

class function Sys.Max(const Values: TArray<double>): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

{ TDataSize }

procedure TDataSize.Init(const AValue: int64);
begin
  Self := Default(TDataSize);
  FSize := AValue;
end;

procedure TDataSize.InitMb(const AValue: double);
begin
  Self := Default(TDataSize);
  SizeMb := AValue;
end;

class operator TDataSize.Add(Left, Right: TDataSize): TDataSize;
begin
  result := Left.FSize+right.FSize;
end;

class operator TDataSize.Divide(Left, Right: TDataSize): TDataSize;
begin
  result := Left.FSize div Right.FSize;
end;

class operator TDataSize.Equal(const Left, Right: TDataSize): Boolean;
begin
  result := Left.FSize=Right.FSize;
end;

function TDataSize.GetAsString: string;
begin

  {case FSize of
     0 .. Kb-1 : result := Format('%d', [FSize]);
    Kb .. Mb-1 : result := Format('%.1f Kb', [FSize/Kb]);
    Mb .. Gb-1 : result := Format('%.1f Mb', [FSize/Mb]);
    Gb .. Tb-1 : result := Format('%.1f Gb', [FSize/Gb]);
    else result := Format('%.1f Tb', [FSize/Tb]);
  end;}
  if FSize<Mb then
    if FSize<Kb then
      result := Format('%d', [FSize])
    else
      result := Format('%.2f Kb', [FSize/Kb])
  else
    if FSize<Gb then
      result := Format('%.2f Mb', [FSize/Mb])
    else
      if FSize<Tb then
        result := Format('%.2f Gb', [FSize/Gb])
      else
        result := Format('%.2f Tb', [FSize/Tb]);
end;

function TDataSize.GetGb: double;
begin
  result := FSIze / Gb;
end;

function TDataSize.GetKb: double;
begin
  result := FSIze / Kb;
end;

function TDataSize.GetMb: double;
begin
  result := FSIze / Mb;
end;

function TDataSize.GetTb: double;
begin
  result := FSIze / Tb;
end;

class operator TDataSize.GreaterThan(const Left, Right: TDataSize): Boolean;
begin
  result := Left.FSize>Right.FSize;
end;

class operator TDataSize.GreaterThanOrEqual(const Left, Right: TDataSize): Boolean;
begin
  result := Left.FSize>=Right.FSize;
end;

class operator TDataSize.Implicit(const AValue: int64): TDataSize;
begin
  result.FSize := AValue;
end;

class operator TDataSize.IntDivide(Left, Right: TDataSize): TDataSize;
begin
  result := Left.FSize div Right.FSize;
end;

class operator TDataSize.LessThan(const Left, Right: TDataSize): Boolean;
begin
  result := Left.FSize<Right.FSize;
end;

class operator TDataSize.LessThanOrEqual(const Left, Right: TDataSize): Boolean;
begin
  result := Left.FSize<=Right.FSize;
end;

class operator TDataSize.Multiply(Left, Right: TDataSize): TDataSize;
begin
  result := Left.FSize*Right.FSize;
end;

class operator TDataSize.Negative(Value: TDataSize): TDataSize;
begin
  result := -Value.FSize;
end;

class operator TDataSize.NotEqual(const Left, Right: TDataSize): Boolean;
begin
  result := Left.FSize<>Right.FSize;
end;

class operator TDataSize.Implicit(const AValue: TDataSize): int64;
begin
  result := AValue.FSize;
end;

procedure TDataSize.SetGb(const Value: double);
begin
  FSize := Trunc(Value*Gb);
end;

procedure TDataSize.SetKb(const Value: double);
begin
  FSize := Trunc(Value*Kb);
end;

procedure TDataSize.SetMb(const Value: double);
begin
  FSize := Trunc(Value*Mb);
end;

procedure TDataSize.SetTb(const Value: double);
begin
  FSize := Trunc(Value*Tb);
end;

class function TDataSize.SizeToString(const AValue: int64): string;
var
  S: TDataSize;
begin
  S.Init(AValue);
  result := S.AsString;
end;

class operator TDataSize.Subtract(Left, Right: TDataSize): TDataSize;
begin
  result := Left.FSize-Right.FSize;
end;

{ TDebugUtils }

class function TDebugUtils.DebuggerIsAttached: boolean;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  result := DebugHook <> 0;
  {$WARN SYMBOL_PLATFORM DEFAULT}
end;

{ TInterfacedType<T> }

constructor TInterfacedType<T>.Create(AData: T);
begin
  inherited Create;
  FData := AData;
end;

function TInterfacedType<T>.Extract: T;
begin
  result := FData;
  FData := Default(T);
end;

function TInterfacedType<T>.GetData: T;
begin
  result := FData;
end;

function TInterfacedType<T>.GetRefCount: integer;
begin
  result := RefCount;
end;

procedure TInterfacedType<T>.SetData(const AData: T);
begin
  FData := AData;
end;

{ TEnvelop<T> }

constructor TEnvelop<T>.Create;
begin
end;

constructor TEnvelop<T>.Create(AValue: T);
begin
  Value := AValue;
end;

end.
