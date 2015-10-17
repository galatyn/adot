unit adot.dip;

interface

{
  Delphi Interchange Protocol (binary interchange format for Delphi).
  DIP 1.0 (Febrary 2015).
  Highlights:
  + Compact. Compressed presentation of integers, no overhead or any kind
    convertion for binary data etc.
  + Fast. Parsing of 16mb test file is more than 10x faster than with JSON
    (test environment: IDE="XE7", OS="Win 7 x64", CPU="i7 2600",
    build configuration="Release").
  + Self-describing and strictly tipified (unlike XML/JSON, DIP doesn't keep
    abstract "numbers", but numbers of known/specified types instead).
  + Safe to parse data from untrusted sources (or potentially damaged data).
  + Native support for all basic Delphi types.
  + Native support for arrays and dictionaries with unlimited nesting and binary
    keys (similar to JSON, but key of dictionary is sequence of bytes, string
    can be used as key aswell).
  + Import/export to JSON (System.JSON in Delphi). 
  + Import from XML (Xml.* in Delphi).
  + Supports streaming of all basic types (arrays, dictionaries, integers etc).
  + Sequence of DIP-objects can be streamed with zero overhead (stream with
    DIP-objects can be extended by streaming of new objects to the end, usefull
    for logs etc).
  - Supports partial parsing, unneccesary objects can be skiped without
    parsing/loading. For example if you need to read only "header" item from
    dictionary, all other items can be ignored. NOT IMPLEMENTED YET.
  - NULL variable (dtNull)
  - ZLIB compression (dtZLIB)

  Compatibility:
  + Supports all Delphi platforms (Windows X32/X64, OSX, iOS, Android).
  + Supports Delphi XE3 and later (earlier versions are not tested).

  Licensing:
  + The library is distributed under terms of MIT license.
}

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Generics.Defaults, System.Math,

  {$IFNDEF DisableJSON}
  System.JSON, System.JSONConsts,
  {$ENDIF}

  {$IFNDEF DisableXML}
  Xml.XMLDoc, Xml.XMLIntf, Xml.xmldom, Xml.adomxmldom,
  {$ENDIF}

  Soap.EncdDecd, System.NetEncoding;

const
  GB = 1024*1024*1024;
  DIPFileExt = '.dip';

type
  {$IFDEF NEXTGEN}
  TUTF8String = TBytes;
  {$ELSE}
  TUTF8String = RawByteString;
  {$ENDIF}
  TDataType = (
    dtInteger,       // TDIPInteger (Int64)
    dtSingle,        // TDIPSingle
    dtDouble,        // TDIPSingle
    dtCurrency,      // TDIPCurrency
    dtDateTime,      // TDIPDateTime (TDateTime)
    dtBoolean,       // TDIPBoolean
    dtBytes,         // TDIPBytes
    dtUnicodeString, // TDIPUnicodeString (can be accessed as UTF8String)
    dtDictionary,    // TDIPDictionary (contains pairs [dtBytes, dt*])
    dtArray          // TDIPArray (contains array of dt*)
  );

  TDIPDictionary = class;
  TDIPArray = class;
  TDIPInteger = class;
  TDIPSingle = class;
  TDIPDouble = class;
  TDIPCurrency = class;
  TDIPDateTime = class;
  TDIPUnicodeString = class;
  TDIPBoolean = class;
  TDIPBytes = class;

  // abstract streaming classes
  TDIPEncoder = class;
  TDIPDecoder = class;
  CDIPEncoder = class of TDIPEncoder;
  CDIPDecoder = class of TDIPDecoder;
  // default implementation of streaming
  TDIPBinaryWriter = class;
  TDIPBinaryReader = class;

  TDIPValue = class
  protected
    // Full size of the item in the binary presentation
    // (including subitems, type header, ...).
    FEstimatedByteSize: UInt64;
    FDataType: TDataType;

    function GetInt64: Int64; virtual;
    procedure SetInt64(const AValue: Int64); virtual;
    function GetUInt64: UInt64; virtual;
    procedure SetUInt64(const AValue: UInt64); virtual;

    function GetSingle: Single; virtual;
    procedure SetSingle(const AValue: Single); virtual;
    function GetDouble: Double; virtual;
    procedure SetDouble(const AValue: Double); virtual;
    function GetCurrency: Currency; virtual;
    procedure SetCurrency(const AValue: Currency); virtual;

    function GetDateTime: TDateTime; virtual;
    procedure SetDateTime(const AValue: TDateTime); virtual;

    function GetBoolean: Boolean; virtual;
    procedure SetBoolean(const AValue: Boolean); virtual;

    function GetBytes: TBytes; virtual;
    procedure SetBytes(const AValue: TBytes); virtual;
    function GetUnicodeString: UnicodeString; virtual;
    procedure SetUnicodeString(const AValue: UnicodeString); virtual;

    function GetDictionary: TDIPDictionary; virtual;
    function GetArray: TDIPArray; virtual;

    class procedure RaiseError; static;
    procedure SetSize; virtual;
    constructor Create(ADataType: TDataType);
    property EstimatedByteSize: UInt64 read FEstimatedByteSize write FEstimatedByteSize;

    class function CompareBytes(const A, B: TBytes): Boolean; static;

    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; virtual;
    {$ENDIF}

    {$IFNDEF DisableXML}
    class function NewXMLDocument: IXMLDocument; static;
    {$ENDIF}

  public
    procedure SaveToStream(ADst: TStream); overload; inline; // save with default encoder (TDIPBinaryWriter)
    procedure SaveToStream(ADst: TStream; AEncoder: TDIPEncoder); overload; inline;
    procedure SaveToStream(ADst: TStream; AEncoderClass: CDIPEncoder); overload;
    procedure SaveToFile(AFileName: string); overload;

    {$IFNDEF DisableJSON}
    function SaveToJSON: TJSONValue;
    {$ENDIF}

    class function LoadFromStream(ASrc: TStream): TDIPValue; overload; static; // load with deault decoder (TDIPBinaryReader)
    class function LoadFromStream(ASrc: TStream; ADecoder: TDIPDecoder): TDIPValue; overload; static;
    class function LoadFromStream(ASrc: TStream; ADecoderClass: CDIPDecoder): TDIPValue; overload; static;

    {$IFNDEF DisableJSON}
    class function LoadFromJSON(ASrc: TJSONValue): TDIPValue; overload; static;
    {$ENDIF}

    {$IFNDEF DisableXML}
    class function LoadFromXML(ASrc: IXMLNode): TDIPArray; overload; static;
    class function LoadFromXML(ASrc: IXMLDocument): TDIPArray; overload; static;
    class function LoadFromXML(const AXML: string): TDIPArray; overload; static;
    class function LoadFromXML(AXML: TStream): TDIPArray; overload; static;
    class function LoadFromXMLFile(const AXMLfileName: string): TDIPArray; overload; static;
    {$ENDIF}

    class function LoadFromFile(AFileName: string): TDIPValue; static; // .dip, .json, .xml
    function EqualTo(AValue: TDIPValue): Boolean; virtual; abstract;
    function IsConsistent: Boolean; virtual;

    class function NewValue(const ADataType: TDataType):TDIPValue; overload; static;
    class function NewValue(const AValue: Int64):TDIPInteger; overload; static; inline;
    class function NewValue(const AValue: Single):TDIPSingle; overload; static; inline;
    class function NewValue(const AValue: Double):TDIPDouble; overload; static; inline;
    class function NewValue(const AValue: Currency):TDIPCurrency; overload; static; inline;
    class function NewValue(const AValue: TDateTime):TDIPDateTime; overload; static; inline;
    class function NewValue(const AValue: UnicodeString):TDIPUnicodeString; overload; static; inline;
    class function NewValue(const AValue: Boolean):TDIPBoolean; overload; static; inline;
    class function NewValue(const AValue: TBytes):TDIPBytes; overload; static; inline;

    // integer
    property AsInteger: Int64 read GetInt64 write SetInt64;
    property AsInt64: Int64 read GetInt64 write SetInt64;
    property AsUInt64: UInt64 read GetUInt64 write SetUInt64;

    // float
    property AsSingle: Single read GetSingle write SetSingle;
    property AsDouble: Double read GetDouble write SetDouble;
    property AsCurrency: Currency read GetCurrency write SetCurrency;

    // date&time
    property AsDateTime: TDateTime read GetDateTime write SetDateTime;

    // boolean
    property AsBoolean: Boolean read GetBoolean write SetBoolean;

    // strings
    property AsBytes: TBytes read GetBytes write SetBytes;
    property AsUnicodeString: UnicodeString read GetUnicodeString write SetUnicodeString;
    property AsString: UnicodeString read GetUnicodeString write SetUnicodeString;

    // structures
    property AsDictionary:TDIPDictionary read GetDictionary;
    property AsArray:TDIPArray read GetArray;

    {$IFNDEF DisableJSON}
    property AsJSON: TJSONValue read GetAsJSON;
    {$ENDIF}
    property DataType: TDataType read FDataType;
  end;

  TDIPInteger = class(TDIPValue)
  protected
    function GetInt64: Int64; override;
    procedure SetInt64(const AValue: Int64); override;
    function GetUInt64: UInt64; override;
    procedure SetUInt64(const AValue: UInt64); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: Int64;

    constructor Create; overload;
    constructor Create(AValue: Int64); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
  end;

  TDIPSingle = class(TDIPValue)
  protected
    function GetSingle: Single; override;
    procedure SetSingle(const AValue: Single); override;
    function GetDouble: Double; override;
    procedure SetDouble(const AValue: Double); override;
    function GetCurrency: Currency; override;
    procedure SetCurrency(const AValue: Currency); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: Single;

    constructor Create; overload;
    constructor Create(AValue: Single); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
  end;

  TDIPDouble = class(TDIPValue)
  protected
    function GetSingle: Single; override;
    procedure SetSingle(const AValue: Single); override;
    function GetDouble: Double; override;
    procedure SetDouble(const AValue: Double); override;
    function GetCurrency: Currency; override;
    procedure SetCurrency(const AValue: Currency); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: Double;

    constructor Create; overload;
    constructor Create(AValue: Double); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
  end;

  TDIPCurrency = class(TDIPValue)
  protected
    function GetSingle: Single; override;
    procedure SetSingle(const AValue: Single); override;
    function GetDouble: Double; override;
    procedure SetDouble(const AValue: Double); override;
    function GetCurrency: Currency; override;
    procedure SetCurrency(const AValue: Currency); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: Currency;

    constructor Create; overload;
    constructor Create(AValue: Currency); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
  end;

  TDIPDateTime = class(TDIPValue)
  protected
    function GetDateTime: TDateTime; override;
    procedure SetDateTime(const AValue: TDateTime); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: TDateTime;

    constructor Create; overload;
    constructor Create(AValue: TDateTime); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
  end;

  TDIPBoolean = class(TDIPValue)
  protected
    function GetBoolean: Boolean; override;
    procedure SetBoolean(const AValue: Boolean); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: Boolean;

    constructor Create; overload;
    constructor Create(AValue: Boolean); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
  end;

  TDIPBytes = class(TDIPValue)
  protected
    function GetBytes: TBytes; override;
    procedure SetBytes(const AValue: TBytes); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: TBytes;

    constructor Create; overload;
    constructor Create(const AValue: TBytes); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
    procedure SetValue<T: record>(const AValue: T);
    function GetValue<T: record>: T;
  end;

  TDIPUnicodeString = class(TDIPValue)
  protected
    function GetUnicodeString: UnicodeString; override;
    procedure SetUnicodeString(const AValue: UnicodeString); override;
    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}
  public
    Value: UnicodeString;

    constructor Create; overload;
    constructor Create(AValue: UnicodeString); overload;
    function EqualTo(AValue: TDIPValue): Boolean; override;
  end;
  TDIPString = TDIPUnicodeString;

  // Set of ORDERED pairs <TBytes, AnyType> with fast search/add and relatively
  // slow delete operations (for data interchange format it is not a problem).
  TDIPDictionary = class(TDIPValue)
  private
  protected
    type
      TPair = class
      protected
        FKey: TBytes;
        FValue: TDIPValue;

        procedure SetValue(AValue: TDIPValue); inline;
      public
        constructor Create(AKey: TBytes; AValue: TDIPValue);
        destructor Destroy; override;

        property Value: TDIPValue read FValue write SetValue;
        property Key: TBytes read FKey write FKey;
      end;

      TDIPDictionaryKeysEnumerator = record
        FIndex, FMaxIndex: Integer;
        FPairs: TObjectList<TPair>;

        procedure Init(APairs: TObjectList<TPair>); inline;
        function MoveNext: Boolean; inline;
        function GetCurrent: TBytes; inline;
        property Current: TBytes read GetCurrent;
      end;

    var
      Pairs: TObjectList<TPair>;               // indexed access (order for output)
      Dictionary: TDictionary<TBytes, Integer>;// search index key->Pairs[n]
      ContentSize: Int64;                      // EstimatedByteSize = 1 + CalsIntSize(ContentSize) + ContentSize
      CaseIns: Boolean;

    function GetDictionary: TDIPDictionary; override;
    function GetValue(const AKey: TBytes): TDIPValue; inline;
    procedure SetValue(const AKey: TBytes; const Value: TDIPValue); inline;
    function GetValueByIndex(AIndex: integer): TDIPValue;
    procedure SetValueByIndex(AIndex: integer; const Value: TDIPValue);
    function GetValueByName(const AKey: UnicodeString): TDIPValue; inline;
    procedure SetValueByName(const AKey: UnicodeString; const Value: TDIPValue); inline;
    function GetKeyByIndex(AIndex: integer): TBytes;
    procedure SetKeyByIndex(AIndex: integer; const Value: TBytes);

    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}

  public
    constructor Create;
    destructor Destroy; override;
    function EqualTo(AValue: TDIPValue): Boolean; override;
    class function KeyToJSONString(const AKey: TBytes): String;
    class function AsKey(const AKey: UnicodeString):TBytes; overload; static;
    class function AsKey<T: record>(const AKey: T):TBytes; overload; static;
    function IsConsistent: Boolean; override;
    function GetEnumerator: TDIPDictionaryKeysEnumerator;

    procedure AddOrSetValue      (const AKey: TBytes; AValue: TDIPValue); overload; inline;
    procedure AddOrSetValue      (const AKey: UnicodeString; AValue: TDIPValue); overload; inline;

    function AddOrSetInteger     (const AKey: TBytes; const AValue: Int64):TDIPInteger; overload;
    function AddOrSetSingle      (const AKey: TBytes; const AValue: Single):TDIPSingle; overload;
    function AddOrSetDouble      (const AKey: TBytes; const AValue: Double):TDIPDouble; overload;
    function AddOrSetCurrency    (const AKey: TBytes; const AValue: Currency):TDIPCurrency; overload;
    function AddOrSetDateTime    (const AKey: TBytes; const AValue: TDateTime):TDIPDateTime; overload;
    function AddOrSetString      (const AKey: TBytes; const AValue: UnicodeString):TDIPUnicodeString; overload;
    function AddOrSetBoolean     (const AKey: TBytes; const AValue: Boolean):TDIPBoolean; overload;
    function AddOrSetBytes       (const AKey: TBytes; const AValue: TBytes):TDIPBytes; overload;

    function AddOrSetInteger     (const AKey: UnicodeString; const AValue: Int64):TDIPInteger; overload; inline;
    function AddOrSetSingle      (const AKey: UnicodeString; const AValue: Single):TDIPSingle; overload; inline;
    function AddOrSetDouble      (const AKey: UnicodeString; const AValue: Double):TDIPDouble; overload; inline;
    function AddOrSetCurrency    (const AKey: UnicodeString; const AValue: Currency):TDIPCurrency; overload; inline;
    function AddOrSetDateTime    (const AKey: UnicodeString; const AValue: TDateTime):TDIPDateTime; overload; inline;
    function AddOrSetString      (const AKey: UnicodeString; const AValue: UnicodeString):TDIPUnicodeString; overload; inline;
    function AddOrSetBoolean     (const AKey: UnicodeString; const AValue: Boolean):TDIPBoolean; overload; inline;
    function AddOrSetBytes       (const AKey: UnicodeString; const AValue: TBytes):TDIPBytes; overload; inline;

    function TryGetValue(const AKey: TBytes; var AValue: TDIPValue): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: TDIPValue): Boolean; overload; inline;

    function TryGetValue(const AKey: TBytes; var AValue: Int64): Boolean; overload;
    function TryGetValue(const AKey: TBytes; var AValue: Single): Boolean; overload;
    function TryGetValue(const AKey: TBytes; var AValue: Double): Boolean; overload;
    function TryGetValue(const AKey: TBytes; var AValue: Currency): Boolean; overload;
    function TryGetValue(const AKey: TBytes; var AValue: TDateTime): Boolean; overload;
    function TryGetValue(const AKey: TBytes; var AValue: UnicodeString): Boolean; overload;
    function TryGetValue(const AKey: TBytes; var AValue: Boolean): Boolean; overload;
    function TryGetValue(const AKey: TBytes; var AValue: TBytes): Boolean; overload;

    function TryGetValue(const AKey: UnicodeString; var AValue: Int64): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: Single): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: Double): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: Currency): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: TDateTime): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: UnicodeString): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: Boolean): Boolean; overload; inline;
    function TryGetValue(const AKey: UnicodeString; var AValue: TBytes): Boolean; overload; inline;

    function Exists(const AKey: TBytes):Boolean; overload; inline;
    function Exists(const AKey: UnicodeString):Boolean; overload; inline;
    function IndexOf(const AKey: TBytes): integer; overload; inline;
    function IndexOf(const AKey: UnicodeString): integer; overload; inline;
    procedure Clear; inline;
    function Count: Integer; inline;
    procedure Exchange(AIndex1, AIndex2: integer);
    procedure ReplaceKey(const AOldKey, ANewKey: TBytes); overload;
    procedure ReplaceKey(const AOldKey, ANewKey: UnicodeString); overload;

    // relatively slow operation when AKeepOrder = True!
    procedure Delete(const AKey: TBytes; AKeepOrder: Boolean = False); overload;
    procedure Delete(const AKey: UnicodeString; AKeepOrder: Boolean = False); overload; inline;

    property Values[const AKey: TBytes]:TDIPValue read GetValue write SetValue;
    property ValueByName[const AKey: UnicodeString]:TDIPValue read GetValueByName write SetValueByName; default;
    property ValueByIndex[AIndex: integer]:TDIPValue read GetValueByIndex write SetValueByIndex;
    property KeyByIndex[AIndex: integer]:TBytes read GetKeyByIndex write SetKeyByIndex;
    property CaseInsensitive: boolean read CaseIns write CaseIns;
  end;

  // array of items of any type
  TDIPArray = class(TDIPValue)
  protected
    type
      TDIPArrayEnumerator = record
        FIndex, FMaxIndex: Integer;
        FList: TObjectList<TDIPValue>;

        procedure Init(AList: TObjectList<TDIPValue>); inline;
        function MoveNext: Boolean; inline;
        function GetCurrent: TDIPValue; inline;
        property Current: TDIPValue read GetCurrent;
      end;

  protected
    FList: TObjectList<TDIPValue>;
    ContentSize: Int64; // EstimatedByteSize = 1 + CalsIntSize(ContentSize) + ContentSize

    function GetArray: TDIPArray; override;

    function GetCount: integer; inline;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);

    function GetValue       (AIndex: integer): TDIPValue; inline;
    procedure SetValue      (AIndex: integer; const Value: TDIPValue); inline;

    {$IFNDEF DisableJSON}
    function GetAsJSON: TJSONValue; override;
    {$ENDIF}

    property List: TObjectList<TDIPValue> read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    function EqualTo(AValue: TDIPValue): Boolean; override;
    function GetEnumerator: TDIPArrayEnumerator;

    function  AddValue<T: TDIPValue>(const AValue: T):T; inline;
    function  AddInteger    (const AValue: Int64): TDIPInteger; inline;
    function  AddSingle     (const AValue: Single): TDIPSingle; inline;
    function  AddDouble     (const AValue: Double): TDIPDouble; inline;
    function  AddCurrency   (const AValue: Currency): TDIPCurrency; inline;
    function  AddDateTime   (const AValue: TDateTime): TDIPDateTime; inline;
    function  AddString     (const AValue: UnicodeString): TDIPUnicodeString; inline;
    function  AddBoolean    (const AValue: Boolean): TDIPBoolean; inline;
    function  AddBytes      (const AValue: TBytes): TDIPBytes; inline;

    function  Add<T: TDIPValue>(const AValue: T): T; overload; inline;
    function  Add(const AValue: Int64): TDIPInteger; overload; inline;
    function  Add(const AValue: Double): TDIPDouble; overload; inline;
    function  Add(const AValue: UnicodeString): TDIPUnicodeString; overload; inline;
    function  Add(const AValue: Boolean): TDIPBoolean; overload; inline;
    function  Add(const AValue: TBytes): TDIPBytes; overload; inline;

    procedure Add(AValues: TEnumerable<byte>); overload;
    procedure Add(AValues: TEnumerable<integer>); overload;
    procedure Add(AValues: TEnumerable<cardinal>); overload;
    procedure Add(AValues: TEnumerable<int64>); overload;
    procedure Add(AValues: TEnumerable<string>); overload;
    procedure Add(AValues: TEnumerable<single>); overload;
    procedure Add(AValues: TEnumerable<double>); overload;
    procedure Add(AValues: TEnumerable<currency>); overload;
    procedure Add(AValues: TEnumerable<TDateTime>); overload;
    procedure Add(const AValues: array of byte); overload;
    procedure Add(const AValues: array of integer); overload;
    procedure Add(const AValues: array of cardinal); overload;
    procedure Add(const AValues: array of int64); overload;
    procedure Add(const AValues: array of string); overload;
    procedure Add(const AValues: array of single); overload;
    procedure Add(const AValues: array of double); overload;
    procedure Add(const AValues: array of currency); overload;
    procedure Add(const AValues: array of TDateTime); overload;

    procedure Delete(AIndex: integer); inline;
    function Extract(AIndex: integer; AShiftArray: Boolean): TDIPValue; inline; // delete without freing
    procedure Exchange(AIndex1,AIndex2: integer); inline;
    function IndexOf(const Value: TDIPValue): Integer; inline;
    procedure Clear; inline;
    procedure Sort(const AComparer: IComparer<TDIPValue>); overload;
    procedure Sort(const AComparer: TComparison<TDIPValue>); overload;

    property Values[AIndex: integer]: TDIPValue read GetValue write SetValue; default;

    property Count: integer read GetCount;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  TCustomStreamer = class
  protected
    Stream: TStream;
    OwnsStream: Boolean;
    Buffer: array[0..4095] of byte;
    Buffered: integer;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
  end;

  // buffered stream writer
  TStreamWriter = class(TCustomStreamer)
  protected
  public
    destructor Destroy; override;
    procedure Flush;
    procedure Write(const AValue; ASize: longword); overload;
    procedure Write<T>(const AValue: T); overload; inline;
  end;

  // buffered stream reader + stream framing
  TStreamReader = class(TCustomStreamer)
  private
  protected
    ReadableFrameSize: int64;   // S.Size-S.Position (or AReadableSize for frame)
    ReadableFrameSrcPos: int64; // where readable frame starts in stream
    ReadableFramePos: int64;    // current position (within the frame)
    BufReadPos: integer;        // current position to read from the buffer

    function GetAvailToRead: Int64; inline;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    procedure SetReadFrame(const APos, AReadableSize: int64); // frame within stream
    function HasData(const ASize: int64): int64; inline; // Assert(ASize<=AvailToRead)

    procedure Read(out AValue; ASize: longword); overload;
    function Read<T>: T; overload; inline;

    property Position: Int64 read ReadableFramePos;
    property Size: Int64 read ReadableFrameSize;
    property AvailToRead: Int64 read GetAvailToRead; // [ReadableFrameSize..0]
  end;

  TDIPEncoder = class abstract
  public
    constructor Create; virtual; abstract;
    procedure SaveToStream(ASrc: TDIPValue; ADst: TStream); virtual; abstract;
  end;

  TDIPDecoder = class abstract
  public
    constructor Create; virtual; abstract;
    function LoadFromStream(ASrc: TStream; AMaxSize:UInt64 = UInt64(1000)*1024*1024*1024):TDIPValue; virtual; abstract;
  end;

  {
    Stored value format (mostly structure of the first byte):
    [7] - short integer (1..128)
    True:
      [6-0] value-1 (0 must be encoded as "byte(dtInteger)+STD_DEF"!)
    False:

    [6] short string
    True:
      [5] TShortStringType (0:dtBytes, 1:dtUnicodeString)
      [4-0] string length-1 (1..32)
    False:

    [5] variable size integer (1,2 or 4 bytes + sign)
    True:
      [4] negative stored as -(value+1), so -1,-2,-3,... -> 0,1,2,...
      [3-2] RESERVED
      [1-0] size:
        Positive values:
          00 - 1 byte
            to save 129..255+129 values only, we keep Value-129
            0 must be encoded as byte(dtInteger)+STD_DEF,
            1..128 must be encoded as "short int"
          01 - 2 bytes
            to save 255+130..65535+255+130 values only, we keep Value-255-130
            0..255+129 must be encoded as described before
          10 - 4 bytes
            to save 65535+255+131..High(Longword)+65535+255+131 values only,
            we keep Value-65535-255-131
            0..65535+255+130 must be encoded as described before
          11 - RESERVED (8 bytes integer must be encoded as dtInteger!)
        For negative values instead of N<0 we keep positive -(N+1):
          00: 1 byte  0..255
          01: 2 bytes 256..65535+256
          10: 4 bytes 65535+257..high(Longword)+65535+257
          11 - RESERVED
    False:

    [4] default value (empty/zero/...)
    [3-0] TDataType (max 16 types)

    Short integer, Short string and Variable size integer are encoded in
    special way (see description above), all other types are encoded in
    general way and have corresponding TDataType.
    TDataType = (
      dtInteger,       // 1+8 bytes (1 byte for zero)
      dtSingle,        // 1+4 bytes (1 byte for zero)
      dtDouble,        // 1+8 bytes (1 byte for zero)
      dtCurrency,      // 1+8 bytes (1 byte for zero)
      dtDateTime,      // 1+8 bytes (1 byte for zero)
      dtBoolean,       // 1 byte (Def=1 -> False, Def=0 -> True)
      dtBytes,         // full size of data in bytes: Int + Data
      dtUnicodeString, // full size of str in bytes: Int + Data (stored as UTF8!)
      dtDictionary,    // Full size of pairs in bytes:Int + pairs
                       //   Key:dtBytes (but stored as int + data!),
                       //   Value:dt*
      dtArray          // FullSizeBytes:*Int + list of dt*
      *Int: integer is most important type and has several ways to store:
        - 1 byte  for [0]                                            (dtInteger + Def=1)
        - 1 byte  for [1             ..128]                          (short integer, SHINT_BIT=1)
        - 2 bytes for [129           ..255+129]                      (variable size integer, type 00)
        - 3 bytes for [255+130       ..65535+255+130]                (variable size integer, type 01)
        - 5 bytes for [65535+255+131 ..High(Longword)+65535+255+131] (variable size integer, type 10)
        - 9 bytes for [Low(int64)    ..High(int64)]                  (dtInteger)
        Check implementation to see details about negative values.
        So, integers usually are stored in compressed form.
    );
  }
  TDIPConst = class
  public
    const
      SHINT_BIT      = 128;
      SHINT_VAL      = 255-SHINT_BIT;

      SHSTR_BIT      = 64;
      SHSTR_TYPE     = 32;
      SHSTR_BYTES    = 0;
      SHSTR_STR      = SHSTR_TYPE;
      SHSTR_VAL      = 16+8+4+2+1;

      CINT_BIT       = 32;
      CINT_NEG       = 16;
      CINT_SIZE      = 2+1;
      CINT_SIZE1     = 0;
      CINT_SIZE2     = 1;
      CINT_SIZE4     = 2;

      STD_DEF        = 16;
      STD_TYPE       = 8+4+2+1;
  end;

  TDIPBinaryWriter = class(TDIPEncoder)
  protected
    Dst: TStreamWriter;

    class function CalcIntegerValueSize(AValue: INT64): Longword; static;
    class procedure CalcValueSize(ASrc: TDIPValue); static;
    procedure SaveInteger(AValue: INT64);
    procedure SaveValue(ASrc: TDIPValue);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SaveToStream(ASrc: TDIPValue; ADst: TStream); override;
  end;

  TDIPBinaryReader = class(TDIPDecoder)
  protected
    Src: TStreamReader;

    function ReadFirstValueSize: Int64;
    function LoadValue: TDIPValue;
    function ReadInt(b: byte): Int64; overload;
    function ReadInt: Int64; overload; inline;
    function ReadShortBytes(ALen: Integer): TDIPBytes;
    function ReadShortString(ALen: Integer): TDIPUnicodeString;
    function ReadBytes: TDIPBytes;
    function ReadUnicodeString: TDIPUnicodeString;
    function ReadDictionary: TDIPDictionary;
    function ReadArray: TDIPArray;
  public
    MaxDictionaryKeyLen: int64;

    constructor Create; override;
    function LoadFromStream(ASrcStream: TStream; AMaxSize:UInt64 = 1*GB):TDIPValue; override;
  end;

function Str2UTF8(const S: UnicodeString): TUTF8String;
function UTF82Str(const S: TUTF8String): UnicodeString;

implementation

function Str2UTF8(const S: UnicodeString): TUTF8String;
begin
  {$IFDEF NEXTGEN}
  Result := TEncoding.UTF8.GetBytes(S);
  {$ELSE}
  Result := UTF8Encode(S);
  {$ENDIF}
end;

function UTF82Str(const S: TUTF8String): UnicodeString;
begin
  {$IFDEF NEXTGEN}
  Result := TEncoding.UTF8.GetString(S);
  {$ELSE}
  Result := UTF8ToString(S);
  {$ENDIF}
end;

{ TDIPValue }

constructor TDIPValue.Create(ADataType: TDataType);
begin
  FDataType := ADataType;
end;

class function TDIPValue.NewValue(const ADataType: TDataType): TDIPValue;
begin
  case ADataType of
    dtInteger       : Result := TDIPInteger.Create;
    dtSingle        : Result := TDIPSingle.Create;
    dtDouble        : Result := TDIPDouble.Create;
    dtCurrency      : Result := TDIPCurrency.Create;
    dtDateTime      : Result := TDIPDateTime.Create;
    dtBoolean       : Result := TDIPBoolean.Create;
    dtBytes         : Result := TDIPBytes.Create;
    dtUnicodeString : Result := TDIPUnicodeString.Create;
    dtDictionary    : Result := TDIPDictionary.Create;
    dtArray         : Result := TDIPArray.Create;
    else
    begin
      RaiseError;
      result := nil;
    end;
  end;
end;

class function TDIPValue.NewValue(const AValue: Single): TDIPSingle;
begin
  Result := TDIPSingle.Create(AValue);
end;

class function TDIPValue.NewValue(const AValue: Double): TDIPDouble;
begin
  Result := TDIPDouble.Create(AValue);
end;

class function TDIPValue.NewValue(const AValue: Currency): TDIPCurrency;
begin
  Result := TDIPCurrency.Create(AValue);
end;

class function TDIPValue.NewValue(const AValue: TDateTime): TDIPDateTime;
begin
  Result := TDIPDateTime.Create(AValue);
end;

class function TDIPValue.NewValue(const AValue: Int64): TDIPInteger;
begin
  Result := TDIPInteger.Create(AValue);
end;

class function TDIPValue.NewValue(const AValue: UnicodeString): TDIPUnicodeString;
begin
  Result := TDIPUnicodeString.Create(AValue);
end;

class function TDIPValue.NewValue(const AValue: Boolean): TDIPBoolean;
begin
  Result := TDIPBoolean.Create(AValue);
end;

class function TDIPValue.NewValue(const AValue: TBytes):TDIPBytes;
begin
  Result := TDIPBytes.Create(AValue);
end;

class procedure TDIPValue.RaiseError;
begin
  raise Exception.Create('Error');
end;

function TDIPValue.GetArray: TDIPArray;
begin
  RaiseError;
  result := nil;
end;

{$IFNDEF DisableJSON}
function TDIPValue.GetAsJSON: TJSONValue;
begin
  RaiseError;
  result := nil;
end;
{$ENDIF}

function TDIPValue.GetBoolean: Boolean;
begin
  RaiseError;
  result := False;
end;

function TDIPValue.GetBytes: TBytes;
begin
  RaiseError;
end;

function TDIPValue.GetCurrency: Currency;
begin
  RaiseError;
  result := 0;
end;

function TDIPValue.GetDateTime: TDateTime;
begin
  RaiseError;
  result := 0;
end;

function TDIPValue.GetDouble: Double;
begin
  RaiseError;
  result := 0;
end;

function TDIPValue.GetInt64: Int64;
begin
  RaiseError;
  result := 0;
end;

function TDIPValue.GetDictionary: TDIPDictionary;
begin
  RaiseError;
  result := nil;
end;

function TDIPValue.GetSingle: Single;
begin
  RaiseError;
  result := 0;
end;

function TDIPValue.GetUInt64: UInt64;
begin
  RaiseError;
  result := 0;
end;

function TDIPValue.GetUnicodeString: UnicodeString;
begin
  RaiseError;
  result := '';
end;

function TDIPValue.IsConsistent: Boolean;
begin
  Result := true;
end;

{$IFNDEF DisableXML}
class function TDIPValue.NewXMLDocument: IXMLDocument;
var
  XMLDoc: TXMLDocument;
begin
  XMLDoc := TXMLDocument.Create(nil);
  result := XMLDoc;
  // Delphi's XE7 help: To use TXMLDocument in Mac OS X or Mobile
  // applications, set the DOMVendor property to ADOM XML v4.
  XMLDoc.DOMVendor := GetDOMVendor(sAdom4XmlVendor);
end;
{$ENDIF}

class function TDIPValue.LoadFromFile(AFileName: string): TDIPValue;
var
  Ext: string;
  UTF8Str: TUTF8String;
  S: TStream;
{$IFNDEF DisableJSON}
  J: TJSONValue;
{$ENDIF}
  T: UnicodeString;
begin
  AFileName := Trim(AFileName);
  Ext := AnsiLowerCase(ExtractfileExt(AfileName));
  if Ext=DIPFileExt then
  begin
    S := TFileStream.Create(AFileName, fmOpenRead);
    try
      Result := LoadFromStream(S);
    finally
      FreeAndNil(S);
    end;
  end

  {$IFNDEF DisableJSON}
  else if Ext='.json' then
  begin
    S := TFileStream.Create(AFileName, fmOpenRead);
    try
      setlength(UTF8Str, S.Size);
      S.Read(UTF8Str[Low(UTF8Str)], Length(UTF8Str));
    finally
      FreeAndNil(S);
    end;
    T := UTF82Str(UTF8Str);
    setlength(UTF8Str, 0);
    J := TJSONObject.ParseJSONValue(T);
    try
      T := '';
      Result := LoadFromJSON(J);
    finally
      FreeAndNil(J);
    end;
  end
  {$ENDIF}

  {$IFNDEF DisableXML}
  else if Ext='.xml' then
    result := LoadFromXMLFile(AFileName)
  {$ENDIF}

  else
    raise Exception.Create('Error');
end;

{$IFNDEF DisableXML}
class function TDIPValue.LoadFromXML(const AXML: string): TDIPArray;
var
  XMLDoc: IXMLDocument;
begin
  XMLDoc := NewXMLDocument;
  XMLDoc.LoadFromXML(AXML);
  Result := LoadFromXML(XMLDoc);
end;

class function TDIPValue.LoadFromXML(AXML: TStream): TDIPArray;
var
  XMLDoc: IXMLDocument;
begin
  XMLDoc := NewXMLDocument;
  XMLDoc.LoadFromStream(AXML);
  Result := LoadFromXML(XMLDoc);
end;

class function TDIPValue.LoadFromXMLFile(const AXMLfileName: string): TDIPArray;
var
  XMLDoc: IXMLDocument;
begin
  XMLDoc := NewXMLDocument;
  XMLDoc.LoadFromFile(AXMLfileName);
  Result := LoadFromXML(XMLDoc);
end;
{$ENDIF}

{$IFNDEF DisableJSON}
class function TDIPValue.LoadFromJSON(
  ASrc: TJSONValue): TDIPValue;
var
  i: Integer;
  I64: Int64;
  D: Double;
begin
  if ASrc is TJSONTrue then
    result := TDIPBoolean.Create(True)
  else if ASrc is TJSONFalse then
    result := TDIPBoolean.Create(False)
  else if ASrc is TJSONString then
    result := TDIPString.Create(TJSONString(ASrc).Value)
  else if ASrc is TJSONNumber then
  begin
    if TryStrToInt64(TJSONNumber(ASrc).Value, I64) then
      Result := TDIPInteger.Create(I64)
    else if TryStrToFloat(TJSONNumber(ASrc).Value, D) then
      Result := TDIPDouble.Create(D)
    else
      Result := TDIPString.Create(TJSONNumber(ASrc).Value);
  end
  else if ASrc is TJSONObject then
  begin
    Result := TDIPDictionary.Create;
    for i := 0 to TJSONObject(ASrc).Count-1 do
      with TJSONObject(ASrc).Pairs[i] do
        TDIPDictionary(Result).AddOrSetValue(JsonString.Value, LoadFromJSON(JsonValue));
  end
  else if ASrc is TJSONNull then
    // we don't use NULL/NIL etc in DIP, so we use empty array instead
    Result := TDIPArray.Create
  else if ASrc is TJSONArray then
  begin
    Result := TDIPArray.Create;
    for i := 0 to TJSONArray(ASrc).Count-1 do
      TDIPArray(Result).Add(LoadFromJSON(TJSONArray(ASrc).Items[i]));
  end
  else
    raise Exception.Create('Error');
end;
{$ENDIF}

(*
  <app v1="1" v2="2"> text <\app>

  ["app",    "text",    {"v1": "1", "v2": "2"}]

  <rss version="2.0">
	  <title>RAD Studio/title>
  </rss>

  ["rss", "", {"version": "2.0"}, [
    "title", "RAD Studio",
  ]]
*)
{$IFNDEF DisableXML}
class function TDIPValue.LoadFromXML(ASrc: IXMLNode): TDIPArray;
var
  i: Integer;
  XMLAttributes: IXMLNodeList;
  XMLChilds: IXMLNodeList;
  XMLAttr: IXMLNode;
  Attributes: TDIPDictionary;
  Childs: TDIPArray;
  ChildAsValue: Boolean;
  ChildCount: Integer;
begin
  Result := TDIPArray.Create;
  try

    // 0 - name (NodeType=ntElement)
    if ASrc.NodeType=ntElement then
      result.AddValue<TDIPString>(TDIPString.Create(ASrc.NodeName))
    else
      result.AddValue<TDIPString>(TDIPString.Create);

    // 1 - value
    ChildAsValue := (ASrc.NodeType=ntElement) and (ASrc.ChildNodes.Count=1) and (ASrc.ChildNodes[0].NodeType=ntText);
    if ChildAsValue then
      result.AddValue<TDIPString>(TDIPString.Create(ASrc.ChildNodes[0].Text))
    else
    if ASrc.NodeType<>ntElement then
      result.AddValue<TDIPString>(TDIPString.Create(ASrc.Text))
    else
      result.AddValue<TDIPString>(TDIPString.Create);

    // 2 - attributes (only if AttributeNodescount>0 or ChildCount>0)
    if ChildAsValue then ChildCount := 0 else ChildCount := ASrc.ChildNodes.Count;
    XMLAttributes := ASrc.AttributeNodes;
    if (XMLAttributes.Count>0) or (ChildCount>0) then
    begin
      Attributes := Result.AddValue<TDIPDictionary>(TDIPDictionary.Create);
      for i := 0 to XMLAttributes.Count-1 do
      begin
        XMLAttr := XMLAttributes.Get(i);
        Attributes.AddOrSetString(Attributes.AsKey(XMLAttr.NodeName), XMLAttr.Text);
      end;
    end;

    // 3 - childs
    if ChildCount>0 then
    begin
      Childs := Result.AddValue<TDIPArray>(TDIPArray.Create);
      XMLChilds := ASrc.ChildNodes;
      for i := 0 to XMLChilds.Count-1 do
        Childs.AddValue<TDIPArray>(LoadFromXML(XMLChilds.Get(i)));
    end;

  except
    result.Free;
    raise;
  end;
end;

class function TDIPValue.LoadFromXML(ASrc: IXMLDocument): TDIPArray;
begin
  result := LoadFromXML(ASrc.DocumentElement);
end;
{$ENDIF}

class function TDIPValue.LoadFromStream(ASrc: TStream): TDIPValue;
begin
  Result := LoadFromStream(ASrc, TDIPBinaryReader);
end;

class function TDIPValue.LoadFromStream(ASrc: TStream;
  ADecoder: TDIPDecoder): TDIPValue;
begin
  Result := ADecoder.LoadFromStream(ASrc);
end;

class function TDIPValue.LoadFromStream(ASrc: TStream;
  ADecoderClass: CDIPDecoder): TDIPValue;
var
  Decoder: TDIPDecoder;
begin
  Decoder := ADecoderClass.Create;
  try
    Result := Decoder.LoadFromStream(ASrc);
  finally
    FreeAndNil(Decoder);
  end;
end;

procedure TDIPValue.SaveToStream(ADst: TStream);
begin
  SaveToStream(ADst, TDIPBinaryWriter);
end;

class function TDIPValue.CompareBytes(const A, B: TBytes): Boolean;
begin
  result := (Length(A)=Length(B)) and CompareMem(@A[0], @B[0], Length(A));
end;

procedure TDIPValue.SaveToFile(AFileName: string);
var
  Ext: string;
  S: TStream;
  {$IFNDEF DisableJSON}
  J: TJSONValue;
  {$ENDIF}
  UTF8Str: TUTF8String;
  T: UnicodeString;
begin
  AFileName := Trim(AFileName);
  Ext := AnsiLowerCase(ExtractfileExt(AfileName));
  if Ext=DIPFileExt then
  begin
    S := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
    try
      SaveToStream(S);
    finally
      FreeAndNil(S);
    end;
  end
  {$IFNDEF DisableJSON}
  else if Ext='.json' then
  begin
    J := SaveToJSON;
    try
      T := J.ToString;
    finally
      FreeAndNil(J);
    end;
    UTF8Str := Str2UTF8(T);
    T := '';
    S := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
    try
      S.Write(UTF8Str[low(UTF8Str)], length(UTF8Str)*sizeof(UTF8Str[low(UTF8Str)]));
    finally
      FreeAndNil(S);
    end;
  end
  {$ENDIF}
  else
    raise Exception.Create('Error');
end;

{$IFNDEF DisableJSON}
function TDIPValue.SaveToJSON: TJSONValue;
begin
  Result := AsJSON;
end;
{$ENDIF}

procedure TDIPValue.SaveToStream(ADst: TStream; AEncoder: TDIPEncoder);
begin
  AEncoder.SaveToStream(Self, ADst);
end;

procedure TDIPValue.SaveToStream(ADst: TStream;
  AEncoderClass: CDIPEncoder);
var
  Encoder: TDIPEncoder;
begin
  Encoder := AEncoderClass.Create;
  try
    Encoder.SaveToStream(Self, ADst);
  finally
    FreeAndNil(Encoder);
  end;
end;

procedure TDIPValue.SetBoolean(const AValue: Boolean);
begin
  RaiseError;
end;

procedure TDIPValue.SetBytes(const AValue: TBytes);
begin
  RaiseError;
end;

procedure TDIPValue.SetCurrency(const AValue: Currency);
begin
  RaiseError;
end;

procedure TDIPValue.SetDateTime(const AValue: TDateTime);
begin
  RaiseError;
end;

procedure TDIPValue.SetDouble(const AValue: Double);
begin
  RaiseError;
end;

procedure TDIPValue.SetInt64(const AValue: Int64);
begin
  RaiseError;
end;

procedure TDIPValue.SetSingle(const AValue: Single);
begin
  RaiseError;
end;

procedure TDIPValue.SetSize;
begin
  EstimatedByteSize := 0;
end;

procedure TDIPValue.SetUInt64(const AValue: UInt64);
begin
  RaiseError;
end;

procedure TDIPValue.SetUnicodeString(const AValue: UnicodeString);
begin
  RaiseError;
end;

{ TDIPDictionary }

constructor TDIPDictionary.Create;
begin
  inherited Create(dtDictionary);
  Pairs := TObjectList<TPair>.Create;
  Dictionary := TDictionary<TBytes, Integer>.Create;
end;

procedure TDIPDictionary.Delete(const AKey: UnicodeString; AKeepOrder: Boolean);
begin
  Delete(AsKey(AKey), AKeepOrder);
end;

destructor TDIPDictionary.Destroy;
begin
  FreeAndNil(Dictionary);
  FreeAndNil(Pairs);
  inherited;
end;

procedure TDIPDictionary.Clear;
begin
  Dictionary.Clear;
  Pairs.Clear;
end;

function TDIPDictionary.Count: Integer;
begin
  result := Pairs.Count;
end;

class function TDIPDictionary.AsKey(const AKey: UnicodeString): TBytes;
var
  UTF8Str: TUTF8String;
begin
  UTF8Str := Str2UTF8(AKey);
  SetLength(Result, Length(UTF8Str)*SizeOf(UTF8Str[Low(UTF8Str)]));
  System.Move(UTF8Str[Low(UTF8Str)], Result[0], Length(Result));
end;

class function TDIPDictionary.AsKey<T>(const AKey: T): TBytes;
begin
  setlength(Result, SizeOf(T));
  System.Move(AKey, Result[0], SizeOf(T));
end;

procedure TDIPDictionary.AddOrSetValue(const AKey: TBytes; AValue: TDIPValue);
var
  Index: Integer;
begin
  if Dictionary.TryGetValue(AKey, Index) then
    Pairs[Index].Value := AValue
  else
    Dictionary.Add(AKey, Pairs.Add(TPair.Create(AKey, AValue)));
end;

procedure TDIPDictionary.AddOrSetValue(const AKey: UnicodeString; AValue: TDIPValue);
begin
  AddOrSetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetString(const AKey: TBytes;
  const AValue: UnicodeString): TDIPUnicodeString;
begin
  Result := TDIPUnicodeString.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.AddOrSetSingle(const AKey: TBytes; const AValue: Single): TDIPSingle;
begin
  Result := TDIPSingle.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.AddOrSetDouble(const AKey: TBytes; const AValue: Double): TDIPDouble;
begin
  Result := TDIPDouble.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.AddOrSetCurrency(const AKey: TBytes; const AValue: Currency): TDIPCurrency;
begin
  Result := TDIPCurrency.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.AddOrSetDateTime(const AKey: TBytes; const AValue: TDateTime): TDIPDateTime;
begin
  Result := TDIPDateTime.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.AddOrSetInteger(const AKey: TBytes; const AValue: Int64): TDIPInteger;
begin
  Result := TDIPInteger.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.AddOrSetBytes(const AKey, AValue: TBytes): TDIPBytes;
begin
  Result := TDIPBytes.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.AddOrSetBoolean(const AKey: TBytes; const AValue: Boolean): TDIPBoolean;
begin
  Result := TDIPBoolean.Create(AValue);
  AddOrSetValue(AKey, Result);
end;

function TDIPDictionary.EqualTo(AValue: TDIPValue): Boolean;
var
  i: Integer;
  p: TPair;
begin
  Result := False;
  if (AValue.DataType<>DataType) or (TDIPDictionary(AValue).Count<>Count) then
    Exit;
  for i := 0 to Pairs.Count-1 do
  begin
    p := TDIPDictionary(AValue).Pairs[i];
    with Pairs[i] do
      if not CompareBytes(Key, P.Key) or not Value.EqualTo(P.Value) then
        Exit;
  end;
  Result := True;
end;

procedure TDIPDictionary.Exchange(AIndex1, AIndex2: integer);
begin
  Pairs.Exchange(AIndex1, AIndex2);
  Dictionary.AddOrSetValue(Pairs[AIndex1].Key, AIndex1);
  Dictionary.AddOrSetValue(Pairs[AIndex2].Key, AIndex2);
end;

function TDIPDictionary.Exists(const AKey: UnicodeString): Boolean;
begin
  Result := Dictionary.ContainsKey(AsKey(AKey));
end;

function TDIPDictionary.Exists(const AKey: TBytes): Boolean;
begin
  Result := Dictionary.ContainsKey(AKey);
end;

procedure TDIPDictionary.Delete(const AKey: TBytes; AKeepOrder: Boolean = False);
var
  i, Index: Integer;
begin
  if not Dictionary.TryGetValue(AKey, Index) then
    Exit;
  Dictionary.Remove(AKey);
  if AKeepOrder then
  begin
    // slow part
    Pairs.Delete(Index);
    for i := Index to Pairs.Count-1 do
      Dictionary.AddOrSetValue(Pairs[i].Key, i);
  end
  else
    // If we don't have to keep order of items, then we can move last
    // item of the list (Pairs) to Index position and avoid of mem shift with
    // recalculation of all (potentially) Dictionary values.
    if Index=Pairs.Count-1 then
      Pairs.Delete(Index)
    else
    begin
      Pairs.Exchange(Index, Pairs.Count-1);
      Pairs.Delete(Pairs.Count-1);
      Dictionary.AddOrSetValue(Pairs[Index].Key, Index);
    end;
end;

{$IFNDEF DisableJSON}
function TDIPDictionary.GetAsJSON: TJSONValue;
var
  p: TJSONObject;
  i: Integer;
begin
  p := TJSONObject.Create;
  result := p;
  for i := 0 to Pairs.Count-1 do
    with Pairs[i] do
      p.AddPair(KeyToJSONString(Key), Value.AsJSON);
end;
{$ENDIF}

function TDIPDictionary.GetDictionary: TDIPDictionary;
begin
  result := Self;
end;

function TDIPDictionary.GetEnumerator: TDIPDictionaryKeysEnumerator;
begin
  Result.Init(Pairs);
end;

function TDIPDictionary.GetKeyByIndex(AIndex: integer): TBytes;
begin
  Result := Pairs[AIndex].Key;
end;

function TDIPDictionary.GetValue(const AKey: TBytes): TDIPValue;
begin
  TryGetValue(AKey, Result);
end;

function TDIPDictionary.GetValueByName(const AKey: UnicodeString): TDIPValue;
begin
  Result := Values[AsKey(AKey)];
end;

procedure TDIPDictionary.ReplaceKey(const AOldKey, ANewKey: TBytes);
var
  PairIndex: Integer;
begin
  if Dictionary.TryGetValue(ANewKey, PairIndex) then
    raise Exception.Create('Duplication of keys is not allowed');
  if not Dictionary.TryGetValue(AOldKey, PairIndex) then
    raise Exception.Create('Key is not found');
  Dictionary.Remove(AOldKey);
  Dictionary.Add(ANewKey, PairIndex);
  Pairs[PairIndex].Key := ANewKey;
end;

procedure TDIPDictionary.ReplaceKey(const AOldKey, ANewKey: UnicodeString);
begin
  ReplaceKey(AsKey(AOldKey), AsKey(ANewKey));
end;

procedure TDIPDictionary.SetKeyByIndex(AIndex: integer; const Value: TBytes);
begin
  ReplaceKey(Pairs[AIndex].Key, Value);
end;

procedure TDIPDictionary.SetValue(const AKey: TBytes; const Value: TDIPValue);
begin
  AddOrSetValue(AKey, Value);
end;

function TDIPDictionary.GetValueByIndex(AIndex: integer): TDIPValue;
begin
  Result := Pairs[AIndex].Value;
end;

function TDIPDictionary.IndexOf(const AKey: UnicodeString): integer;
begin
  if not Dictionary.TryGetValue(AsKey(AKey), Result) then
    Result := -1;
end;

function TDIPDictionary.IsConsistent: Boolean;
var
  i,j: Integer;
begin
  result := False;
  if Dictionary.Count<>Pairs.Count then
    Exit;
  for i := 0 to Pairs.Count-1 do
    if not Dictionary.TryGetValue(Pairs[i].Key, j) or (i<>j) then
      Exit;
  result := True;
end;

class function TDIPDictionary.KeyToJSONString(const AKey: TBytes): String;
var
  I: Integer;
begin
  for I := 0 to High(AKey) do
    if (AKey[i]<=31) or (AKey[i]>=127) then
    begin
      Result := String(EncodeBase64(@AKey[0], Length(AKey)));
      Exit;
    end;
  setlength(Result, Length(AKey));
  for I := 0 to High(AKey) do
    Result[I+Low(Result)] := Char(AKey[I]);
end;

function TDIPDictionary.IndexOf(const AKey: TBytes): integer;
begin
  if not Dictionary.TryGetValue(AKey, Result) then
    Result := -1;
end;

procedure TDIPDictionary.SetValueByIndex(AIndex: integer;
  const Value: TDIPValue);
begin
  Pairs[AIndex].Value := Value;
end;

procedure TDIPDictionary.SetValueByName(const AKey: UnicodeString;
  const Value: TDIPValue);
begin
  Values[AsKey(AKey)] := Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes;
  var AValue: TDIPValue): Boolean;
var
  Index: Integer;
begin
  Result := Dictionary.TryGetValue(AKey, Index);
  if Result then
    AValue := Pairs[Index].Value
  else
    AValue := nil;
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString;
  var AValue: TDIPValue): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: TBytes): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtBytes);
  if Result then
    AValue := TDIPBytes(V).Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: Single): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtSingle);
  if Result then
    AValue := TDIPSingle(V).Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: Double): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtDouble);
  if Result then
    AValue := TDIPDouble(V).Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: Currency): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtCurrency);
  if Result then
    AValue := TDIPCurrency(V).Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: TDateTime): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtDateTime);
  if Result then
    AValue := TDIPDateTime(V).Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: Int64): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtInteger);
  if Result then
    AValue := TDIPInteger(V).Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: UnicodeString): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtUnicodeString);
  if Result then
    AValue := TDIPUnicodeString(V).Value;
end;

function TDIPDictionary.TryGetValue(const AKey: TBytes; var AValue: Boolean): Boolean;
var
  V: TDIPValue;
begin
  Result := TryGetValue(AKey, V) and (v.DataType=dtBoolean);
  if Result then
    AValue := TDIPBoolean(V).Value;
end;

function TDIPDictionary.AddOrSetBoolean(const AKey: UnicodeString;
  const AValue: Boolean): TDIPBoolean;
begin
  Result := AddOrSetBoolean(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetBytes(const AKey: UnicodeString; const AValue: TBytes): TDIPBytes;
begin
  Result := AddOrSetBytes(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetCurrency(const AKey: UnicodeString;
  const AValue: Currency): TDIPCurrency;
begin
  Result := AddOrSetCurrency(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetDateTime(const AKey: UnicodeString;
  const AValue: TDateTime): TDIPDateTime;
begin
  Result := AddOrSetDateTime(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetDouble(const AKey: UnicodeString;
  const AValue: Double): TDIPDouble;
begin
  Result := AddOrSetDouble(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetInteger(const AKey: UnicodeString;
  const AValue: Int64): TDIPInteger;
begin
  Result := AddOrSetInteger(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetSingle(const AKey: UnicodeString;
  const AValue: Single): TDIPSingle;
begin
  Result := AddOrSetSingle(AsKey(AKey), AValue);
end;

function TDIPDictionary.AddOrSetString(const AKey,
  AValue: UnicodeString): TDIPUnicodeString;
begin
  Result := AddOrSetString(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString;
  var AValue: Currency): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString;
  var AValue: TDateTime): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString; var AValue: Double): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString; var AValue: Int64): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString; var AValue: Single): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString; var AValue: Boolean): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString; var AValue: TBytes): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

function TDIPDictionary.TryGetValue(const AKey: UnicodeString; var AValue: UnicodeString): Boolean;
begin
  Result := TryGetValue(AsKey(AKey), AValue);
end;

{ TDIPArray }

function TDIPArray.Add(const AValue: UnicodeString): TDIPUnicodeString;
begin
  result := AddString(AValue);
end;

function TDIPArray.Add(const AValue: Double): TDIPDouble;
begin
  result := AddDouble(AValue);
end;

function TDIPArray.Add(const AValue: Int64): TDIPInteger;
begin
  result := AddInteger(AValue);
end;

function TDIPArray.Add(const AValue: TBytes): TDIPBytes;
begin
  result := AddBytes(AValue);
end;

function TDIPArray.Add<T>(const AValue: T): T;
begin
  result := AddValue(AValue);
end;

function TDIPArray.Add(const AValue: Boolean): TDIPBoolean;
begin
  result := AddBoolean(AValue);
end;

function TDIPArray.AddBoolean(const AValue: Boolean): TDIPBoolean;
begin
  Result := TDIPBoolean.Create(AValue);
  FList.Add(Result);
end;

function TDIPArray.AddBytes(const AValue: TBytes): TDIPBytes;
begin
  Result := TDIPBytes.Create(AValue);
  FList.Add(Result);
end;

function TDIPArray.AddCurrency(const AValue: Currency): TDIPCurrency;
begin
  Result := TDIPCurrency.Create(AValue);
  FList.Add(Result);
end;

function TDIPArray.AddDateTime(const AValue: TDateTime): TDIPDateTime;
begin
  Result := TDIPDateTime.Create(AValue);
  FList.Add(Result);
end;

function TDIPArray.AddDouble(const AValue: Double): TDIPDouble;
begin
  Result := TDIPDouble.Create(AValue);
  FList.Add(Result);
end;

function TDIPArray.AddInteger(const AValue: Int64): TDIPInteger;
begin
  Result := TDIPInteger.Create(AValue);
  FList.Add(Result);
end;

function TDIPArray.AddSingle(const AValue: Single): TDIPSingle;
begin
  Result := TDIPSingle.Create(AValue);
  FList.Add(Result);
end;

function TDIPArray.AddString(const AValue: UnicodeString): TDIPUnicodeString;
begin
  Result := TDIPUnicodeString.Create(AValue);
  FList.Add(Result);
end;

procedure TDIPArray.Clear;
begin
  FList.Clear;
end;

constructor TDIPArray.Create;
begin
  inherited Create(dtArray);
  FList := TObjectList<TDIPValue>.Create(True);
end;

destructor TDIPArray.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TDIPArray.AddValue<T>(const AValue: T): T;
begin
  result := AValue;
  List.Add(AValue)
end;

procedure TDIPArray.Delete(AIndex: integer);
begin
  List.Delete(AIndex);
end;

function TDIPArray.GetArray: TDIPArray;
begin
  Result := Self;
end;

{$IFNDEF DisableJSON}
function TDIPArray.GetAsJSON: TJSONValue;
var
  p: TJSONArray;
  i: Integer;
begin
  p := TJSONArray.Create;
  result := p;
  for i := 0 to Count-1 do
    p.AddElement(Values[i].AsJSON);
end;
{$ENDIF}

function TDIPArray.GetCount: integer;
begin
  result := FList.Count;
end;

function TDIPArray.GetEnumerator: TDIPArrayEnumerator;
begin
  Result.Init(FList);
end;

function TDIPArray.GetOwnsObjects: Boolean;
begin
  Result := List.OwnsObjects;
end;

function TDIPArray.GetValue(AIndex: integer): TDIPValue;
begin
  Result := TDIPValue(List[AIndex]);
end;

procedure TDIPArray.SetValue(AIndex: integer; const Value: TDIPValue);
begin
  List[AIndex] := Value;
end;

procedure TDIPArray.SetOwnsObjects(const Value: Boolean);
begin
  List.OwnsObjects := Value;
end;

procedure TDIPArray.Sort(const AComparer: TComparison<TDIPValue>);
begin
  FList.Sort(TDelegatedComparer<TDIPValue>.Create(AComparer));
end;

procedure TDIPArray.Sort(const AComparer: IComparer<TDIPValue>);
begin
  FList.Sort(AComparer);
end;

function TDIPArray.EqualTo(AValue: TDIPValue): Boolean;
var
  I: Integer;
begin
  Result := (AValue.DataType=DataType) and (TDIPArray(AValue).Count=Count);
  if Result then
    for I := 0 to Count-1 do
      if not Values[i].EqualTo(TDIPArray(AValue)[i]) then
      begin
        Result := False;
        Exit;
      end;
end;

procedure TDIPArray.Exchange(AIndex1, AIndex2: integer);
begin
  List.Exchange(AIndex1, AIndex2);
end;

function TDIPArray.Extract(AIndex: integer; AShiftArray: Boolean): TDIPValue;
var
  b: Boolean;
begin
  b := FList.OwnsObjects;
  FList.OwnsObjects := False;
  Result := FList[AIndex];
  if AShiftArray then
    FList.Delete(AIndex)
  else
    FList[AIndex] := nil;
  FList.OwnsObjects := b;
end;

function TDIPArray.IndexOf(const Value: TDIPValue): Integer;
begin
  Result := List.IndexOf(Value);
end;

procedure TDIPArray.Add(AValues: TEnumerable<double>);
var
  v: double;
begin
  for v in AValues do
    AddDouble(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<single>);
var
  v: single;
begin
  for v in AValues do
    AddSingle(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<TDateTime>);
var
  v: TDateTime;
begin
  for v in AValues do
    AddDateTime(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<currency>);
var
  v: Currency;
begin
  for v in AValues do
    AddCurrency(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<string>);
var
  v: String;
begin
  for v in AValues do
    AddString(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<integer>);
var
  v: Integer;
begin
  for v in AValues do
    AddInteger(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<byte>);
var
  v: Byte;
begin
  for v in AValues do
    AddInteger(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<int64>);
var
  v: Int64;
begin
  for v in AValues do
    AddInteger(v);
end;

procedure TDIPArray.Add(AValues: TEnumerable<cardinal>);
var
  v: cardinal;
begin
  for v in AValues do
    AddInteger(v);
end;

procedure TDIPArray.Add(const AValues: array of double);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddDouble(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of single);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddSingle(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of TDateTime);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddDateTime(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of currency);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddCurrency(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of string);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddString(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of integer);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddInteger(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of byte);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddInteger(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of int64);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddInteger(AValues[i]);
end;

procedure TDIPArray.Add(const AValues: array of cardinal);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddInteger(AValues[i]);
end;

{ TDIPInteger }

constructor TDIPInteger.Create;
begin
  Create(0);
end;

constructor TDIPInteger.Create(AValue: Int64);
begin
  inherited Create(dtInteger);
  Value := AValue;
end;

function TDIPInteger.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and (TDIPInteger(AValue).Value=Value);
end;

{$IFNDEF DisableJSON}
function TDIPInteger.GetAsJSON: TJSONValue;
begin
  result := TJSONNumber.Create(Value);
end;
{$ENDIF}

function TDIPInteger.GetInt64: Int64;
begin
  result := Value;
end;

function TDIPInteger.GetUInt64: UInt64;
begin
  result := UInt64(Value);
end;

procedure TDIPInteger.SetInt64(const AValue: Int64);
begin
  Value := AValue;
end;

procedure TDIPInteger.SetUInt64(const AValue: UInt64);
begin
  Value := Int64(AValue);
end;

{ TDIPSingle }

constructor TDIPSingle.Create;
begin
  Create(0);
end;

constructor TDIPSingle.Create(AValue: Single);
begin
  inherited Create(dtSingle);
  Value := AValue;
end;

function TDIPSingle.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and (TDIPSingle(AValue).Value=Value);
end;

{$IFNDEF DisableJSON}
function TDIPSingle.GetAsJSON: TJSONValue;
begin
  result := TJSONNumber.Create(Value);
end;
{$ENDIF}

function TDIPSingle.GetCurrency: Currency;
begin
  result := Value;
end;

function TDIPSingle.GetDouble: Double;
begin
  result := Value;
end;

function TDIPSingle.GetSingle: Single;
begin
  result := Value;
end;

procedure TDIPSingle.SetCurrency(const AValue: Currency);
begin
  value := AValue;
end;

procedure TDIPSingle.SetDouble(const AValue: Double);
begin
  value := AValue;
end;

procedure TDIPSingle.SetSingle(const AValue: Single);
begin
  value := AValue;
end;

{ TDIPDouble }

constructor TDIPDouble.Create;
begin
  Create(0);
end;

constructor TDIPDouble.Create(AValue: Double);
begin
  inherited Create(dtDouble);
  Value := AValue;
end;

function TDIPDouble.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and (TDIPDouble(AValue).Value=Value);
end;

{$IFNDEF DisableJSON}
function TDIPDouble.GetAsJSON: TJSONValue;
begin
  result := TJSONNumber.Create(Value);
end;
{$ENDIF}

function TDIPDouble.GetCurrency: Currency;
begin
  result := Value;
end;

function TDIPDouble.GetDouble: Double;
begin
  result := Value;
end;

function TDIPDouble.GetSingle: Single;
begin
  result := Value;
end;

procedure TDIPDouble.SetCurrency(const AValue: Currency);
begin
  Value := AValue;
end;

procedure TDIPDouble.SetDouble(const AValue: Double);
begin
  Value := AValue;
end;

procedure TDIPDouble.SetSingle(const AValue: Single);
begin
  Value := AValue;
end;

{ TDIPCurrency }

constructor TDIPCurrency.Create;
begin
  Create(0);
end;

constructor TDIPCurrency.Create(AValue: Currency);
begin
  inherited Create(dtCurrency);
  Value := AValue;
end;

function TDIPCurrency.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and (TDIPCurrency(AValue).Value=Value);
end;

{$IFNDEF DisableJSON}
function TDIPCurrency.GetAsJSON: TJSONValue;
begin
  result := TJSONNumber.Create(Value);
end;
{$ENDIF}

function TDIPCurrency.GetCurrency: Currency;
begin
  result := Value;
end;

function TDIPCurrency.GetDouble: Double;
begin
  result := Value;
end;

function TDIPCurrency.GetSingle: Single;
begin
  result := Value;
end;

procedure TDIPCurrency.SetCurrency(const AValue: Currency);
begin
  Value := AValue;
end;

procedure TDIPCurrency.SetDouble(const AValue: Double);
begin
  Value := AValue;
end;

procedure TDIPCurrency.SetSingle(const AValue: Single);
begin
  Value := AValue;
end;

{ TDIPDateTime }

constructor TDIPDateTime.Create;
begin
  Create(0);
end;

constructor TDIPDateTime.Create(AValue: TDateTime);
begin
  inherited Create(dtDateTime);
  Value := AValue;
end;

function TDIPDateTime.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and (TDIPDateTime(AValue).Value=Value);
end;

{$IFNDEF DisableJSON}
function TDIPDateTime.GetAsJSON: TJSONValue;
begin
  result := TJSONNumber.Create(Value);
end;
{$ENDIF}

function TDIPDateTime.GetDateTime: TDateTime;
begin
  result := Value;
end;

procedure TDIPDateTime.SetDateTime(const AValue: TDateTime);
begin
  Value := AValue;
end;

{ TDIPBoolean }

constructor TDIPBoolean.Create;
begin
  Create(False);
end;

constructor TDIPBoolean.Create(AValue: Boolean);
begin
  inherited Create(dtBoolean);
  Value := AValue;
end;

function TDIPBoolean.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and (TDIPBoolean(AValue).Value=Value);
end;

{$IFNDEF DisableJSON}
function TDIPBoolean.GetAsJSON: TJSONValue;
begin
  if Value then
    result := TJSONTrue.Create
  else
    result := TJSONFalse.Create;
end;
{$ENDIF}

function TDIPBoolean.GetBoolean: Boolean;
begin
  result := Value;
end;

procedure TDIPBoolean.SetBoolean(const AValue: Boolean);
begin
  Value := AValue;
end;

{ TDIPBytes }

constructor TDIPBytes.Create;
begin
  inherited Create(dtBytes);
end;

constructor TDIPBytes.Create(const AValue: TBytes);
begin
  inherited Create(dtBytes);
  Value := AValue;
end;

function TDIPBytes.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and CompareBytes(TDIPBytes(AValue).Value, Value);
end;

function TDIPBytes.GetBytes: TBytes;
begin
  result := Value;
end;

function TDIPBytes.GetValue<T>: T;
begin
  assert(Length(Value)=SizeOf(T));
  System.Move(Value[0], Result, SizeOf(T));
end;

{$IFNDEF DisableJSON}
function TDIPBytes.GetAsJSON: TJSONValue;
begin
  result := TJSONString.Create(String(EncodeBase64(@Value[0], Length(Value))));
end;
{$ENDIF}

procedure TDIPBytes.SetBytes(const AValue: TBytes);
begin
  Value := AValue;
end;

procedure TDIPBytes.SetValue<T>(const AValue: T);
begin
  SetLength(Value, SizeOf(T));
  System.Move(AValue, Value[0], SizeOf(T));
end;

{ TDIPUnicodeString }

constructor TDIPUnicodeString.Create;
begin
  Create('');
end;

constructor TDIPUnicodeString.Create(AValue: UnicodeString);
begin
  inherited Create(dtUnicodeString);
  Value := AValue;
end;

function TDIPUnicodeString.EqualTo(AValue: TDIPValue): Boolean;
begin
  Result := (AValue.DataType=DataType) and (TDIPUnicodeString(AValue).Value=Value);
end;

{$IFNDEF DisableJSON}
function TDIPUnicodeString.GetAsJSON: TJSONValue;
begin
  result := TJSONString.Create(Value);
end;
{$ENDIF}

function TDIPUnicodeString.GetUnicodeString: UnicodeString;
begin
  Result := Value;
end;

procedure TDIPUnicodeString.SetUnicodeString(const AValue: UnicodeString);
begin
  Value := AValue;
end;

{ TCustomStreamer }

constructor TCustomStreamer.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  Stream := AStream;
  OwnsStream := AOwnsStream;
end;

destructor TCustomStreamer.Destroy;
begin
  if OwnsStream then
    FreeAndNil(Stream)
  else
    Stream := nil;
  inherited;
end;

{ TStreamWriter }

destructor TStreamWriter.Destroy;
begin
  Flush;
  inherited;
end;

procedure TStreamWriter.Flush;
begin
  if (Buffered>0) and (Stream.Write(Buffer, Buffered)<>Buffered) then
    raise Exception.Create('Error');
  Buffered := 0;
end;

procedure TStreamWriter.Write(const AValue; ASize: longword);
begin
  if ASize>Longword(SizeOf(Buffer)-Buffered) then
  begin
    Flush;
    if ASize>=SizeOf(Buffer) then
    begin
      if Longword(Stream.Write(AValue, ASize))<>ASize then
        raise Exception.Create('Error');
      Exit;
    end;
  end;
  System.Move(AValue, Buffer[Buffered], ASize);
  inc(Buffered, ASize);
end;

procedure TStreamWriter.Write<T>(const AValue: T);
begin
  Write(AValue, SizeOf(AValue));
end;

{ TStreamReader }

constructor TStreamReader.Create(AStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStream, AOwnsStream);
  ReadableFrameSrcPos := AStream.Position;
  ReadableFrameSize := AStream.Size-ReadableFramePos;
  ReadableFramePos := 0;
  BufReadPos := 0;
  Buffered := 0;
end;

function TStreamReader.HasData(const ASize: int64): int64;
begin
  if ASize>ReadableFrameSize - ReadableFramePos then
    raise Exception.Create('Error');
  result := ASize;
end;

function TStreamReader.GetAvailToRead: Int64;
begin
  Result := ReadableFrameSize - ReadableFramePos;
end;

procedure TStreamReader.Read(out AValue; ASize: longword);
var
  DstOffset: longword;
begin
  if ASize>AvailToRead then
    raise Exception.Create('Error');
  DstOffset := 0;
  if ASize>longword(Buffered) then
  begin

    // read buffered part
    if Buffered>0 then
    begin
      System.Move(Buffer[BufReadPos], AValue, Buffered);
      DstOffset := Buffered;
      Dec(ASize, Buffered);
      Inc(ReadableFramePos, Buffered);
      BufReadPos := 0;
      Buffered := 0;
    end;

    // if we still need large dataset, then read it directly (no buffering)
    if ASize>=SizeOf(Buffer) then
    begin
      if Longword(Stream.Read(PByteArray(@AValue)[DstOffset], ASize))<>ASize then
        raise Exception.Create('Error');
      Inc(ReadableFramePos, ASize);
      Exit;
    end;

    // read next part of the stream to the buffer
    BufReadPos := 0;
    if AvailToRead<SizeOf(Buffer) then
      Buffered := AvailToRead
    else
      Buffered := SizeOf(Buffer);
    if Stream.Read(Buffer, Buffered)<>Buffered then
      raise Exception.Create('Error');
  end;

  // read byffered part of the data
  System.Move(Buffer[BufReadPos], PByteArray(@AValue)[DstOffset], ASize);
  Inc(ReadableFramePos, ASize);
  Inc(BufReadPos, ASize);
  Dec(Buffered, ASize);
end;

function TStreamReader.Read<T>: T;
begin
  Read(Result, SizeOf(Result));
end;

procedure TStreamReader.SetReadFrame(const APos, AReadableSize: int64);
begin
  if APos+AReadableSize>Stream.Size then
    raise Exception.Create('Error');
  Stream.Position := APos;
  ReadableFrameSrcPos := APos;
  ReadableFrameSize := AReadableSize;
  ReadableFramePos := 0;
  BufReadPos := 0;
  Buffered := 0;
end;

{ TDIPBinaryWriter }

procedure RaiseError;
begin
  raise Exception.Create('Error');
end;

constructor TDIPBinaryWriter.Create;
begin
end;

destructor TDIPBinaryWriter.Destroy;
begin
  inherited;
end;

class procedure TDIPBinaryWriter.CalcValueSize(ASrc: TDIPValue);
var
  i,j: integer;
  m: UInt64;
  p: TDIPDictionary.TPair;
  v: TDIPValue;
begin
  case ASrc.DataType of
    dtInteger     : ASrc.EstimatedByteSize := CalcIntegerValueSize(TDIPInteger(ASrc).Value);
    dtSingle      : ASrc.EstimatedByteSize := 1 + IfThen(TDIPSingle(ASrc).Value=0, 0, SizeOf(Single));
    dtDouble      : ASrc.EstimatedByteSize := 1 + IfThen(TDIPDouble(ASrc).Value=0, 0, SizeOf(Double));
    dtCurrency    : ASrc.EstimatedByteSize := 1 + IfThen(TDIPCurrency(ASrc).Value=0, 0, SizeOf(Currency));
    dtDateTime    : ASrc.EstimatedByteSize := 1 + IfThen(TDIPDateTime(ASrc).Value=0, 0, SizeOf(TDateTime));
    dtBoolean     : ASrc.EstimatedByteSize := 1;
    dtBytes:
      begin
        m := Length(TDIPBytes(ASrc).Value);
        if m<=32 then
          ASrc.EstimatedByteSize := 1 + m
        else
          ASrc.EstimatedByteSize := 1 + CalcIntegerValueSize(m) + m;
      end;
    dtUnicodeString:
      begin
        m := Length(UTF8Encode(TDIPUnicodeString(ASrc).Value));
        if m<=32 then
          ASrc.EstimatedByteSize := 1 + m
        else
          ASrc.EstimatedByteSize := 1 + CalcIntegerValueSize(m) + m;
      end;
    dtDictionary:
      if TDIPDictionary(ASrc).Dictionary.Count=0 then
        ASrc.EstimatedByteSize := 1
      else
      begin
        m := 0;
        for j := 0 to TDIPDictionary(ASrc).Count-1 do
        begin
          p := TDIPDictionary(ASrc).Pairs[j];
          CalcValueSize(p.Value);
          i := Length(p.Key);
          inc(m, CalcIntegerValueSize(i) + Longword(i) + p.Value.EstimatedByteSize);
        end;
        TDIPDictionary(ASrc).ContentSize := m;
        ASrc.EstimatedByteSize := 1 + CalcIntegerValueSize(m) + m;
      end;
    dtArray:
      if TDIPArray(ASrc).Count=0 then
        ASrc.EstimatedByteSize := 1
      else
      begin
        m := 0;
        for i := 0 to TDIPArray(ASrc).Count-1 do
        begin
          v := TDIPArray(ASrc)[i];
          CalcValueSize(v);
          inc(m, v.EstimatedByteSize);
        end;
        TDIPArray(ASrc).ContentSize := m;
        ASrc.EstimatedByteSize := 1 + CalcIntegerValueSize(m) + m;
      end;
    else raise Exception.Create('Error');
  end;
end;

class function TDIPBinaryWriter.CalcIntegerValueSize(AValue: INT64): Longword;
begin
  // based on implementation of SaveInteger
  if AValue>=0 then
  begin
    if AValue<=128 then
      Result := 1
    else
    if AValue<=High(Word)+255+130 then
      if AValue<=High(Byte)+129 then
        Result := 2
      else
        Result := 3
    else
      if AValue<=Int64(High(Longword))+65535+255+131 then
        Result := 5
      else
        Result := 9;
    Exit;
  end;

  AValue := -(AValue+1);
  if AValue<=High(Word)+256 then
    if AValue<=High(Byte) then
      Result := 2
    else
      Result := 3
  else
    if AValue<=Int64(High(Longword))+65535+257 then
      Result := 5
    else
      Result := 9;
end;

procedure TDIPBinaryWriter.SaveInteger(AValue: INT64);
begin
  if AValue>=0 then
  begin
    // dtInteger+STD_DEF: 0..0
    // short integer: 1..128
    // 1: 129..high(byte)+129
    // 2: 255+130..high(word)+255+130
    // 4: 65535+255+131..high(longword)+65535+255+131
    if AValue=0 then
      Dst.Write<Byte>(Byte(dtInteger) or TDIPConst.STD_DEF)
    else
    if AValue<=128 then
      Dst.Write<Byte>((Byte(AValue)-1) or TDIPConst.SHINT_BIT)
    else
    if AValue<=High(Word)+255+130 then
      if AValue<=High(Byte)+129 then
      begin
        Dst.Write<Byte>(TDIPConst.CINT_BIT or TDIPConst.CINT_SIZE1);
        Dst.Write<Byte>(Byte(AValue-129));
      end
      else
      begin
        Dst.Write<Byte>(TDIPConst.CINT_BIT or TDIPConst.CINT_SIZE2);
        Dst.Write<Word>(Word(AValue-(255+130)));
      end
    else
      if AValue<=Int64(High(Longword))+65535+255+131 then
      begin
        Dst.Write<Byte>(TDIPConst.CINT_BIT or TDIPConst.CINT_SIZE4);
        Dst.Write<Longword>(Longword(AValue-(65535+255+131)));
      end
      else
      begin
        Dst.Write<Byte>(Byte(dtInteger));
        Dst.Write<Int64>(AValue);
      end;
    Exit;
  end;

  // AValue<0 we transform to -(AValue+1) and save as compressed int with CINT_NEG marker
  // 1: 0..high(Byte)
  // 2: 256..high(Word)+256
  // 4: 65535+257..high(Longword)+65535+257
  AValue := -(AValue+1);
  if AValue<=High(Word)+256 then
    if AValue<=High(Byte) then
    begin
      Dst.Write<Byte>(TDIPConst.CINT_BIT or TDIPConst.CINT_NEG or TDIPConst.CINT_SIZE1);
      Dst.Write<Byte>(Byte(AValue));
    end
    else
    begin
      Dst.Write<Byte>(TDIPConst.CINT_BIT or TDIPConst.CINT_NEG or TDIPConst.CINT_SIZE2);
      Dst.Write<Word>(Word(AValue-256));
    end
  else
    if AValue<=Int64(High(Longword))+65535+257 then
    begin
      Dst.Write<Byte>(TDIPConst.CINT_BIT or TDIPConst.CINT_NEG or TDIPConst.CINT_SIZE4);
      Dst.Write<Longword>(Longword(AValue-(65535+257)));
    end
    else
    begin
      Dst.Write<Byte>(Byte(dtInteger));
      Dst.Write<Int64>((-AValue)-1);
    end;
end;

procedure TDIPBinaryWriter.SaveValue(ASrc: TDIPValue);
var
  i,l: Integer;
  p: TDIPDictionary.TPair;
  u8: TUTF8String;
begin
  case ASrc.DataType of

    // integer
    dtInteger:
      SaveInteger(TDIPInteger(ASrc).Value);

    // float
    dtSingle:
      if TDIPSingle(ASrc).Value=0 then
        Dst.Write<Byte>(Byte(dtSingle) or TDIPConst.STD_DEF)
      else
      begin
        Dst.Write<Byte>(Byte(dtSingle));
        Dst.Write<Single>(TDIPSingle(ASrc).Value);
      end;
    dtDouble:
      if TDIPDouble(ASrc).Value=0 then
        Dst.Write<Byte>(Byte(dtDouble) or TDIPConst.STD_DEF)
      else
      begin
        Dst.Write<Byte>(Byte(dtDouble));
        Dst.Write<Double>(TDIPDouble(ASrc).Value);
      end;
    dtCurrency:
      if TDIPCurrency(ASrc).Value=0 then
        Dst.Write<Byte>(Byte(dtCurrency) or TDIPConst.STD_DEF)
      else
      begin
        Dst.Write<Byte>(Byte(dtCurrency));
        Dst.Write<Currency>(TDIPCurrency(ASrc).Value);
      end;
    dtDateTime:
      if TDIPDateTime(ASrc).Value=0 then
        Dst.Write<Byte>(Byte(dtDateTime) or TDIPConst.STD_DEF)
      else
      begin
        Dst.Write<Byte>(Byte(dtDateTime));
        Dst.Write<TDateTime>(TDIPDateTime(ASrc).Value);
      end;

    // boolean
    dtBoolean:
      if TDIPBoolean(ASrc).Value then
        Dst.Write<Byte>(Byte(dtBoolean))
      else
        Dst.Write<Byte>(Byte(dtBoolean) or TDIPConst.STD_DEF);

    // strings
    dtBytes:
      begin
        L := Length(TDIPBytes(ASrc).Value);
        if L=0 then
          Dst.Write<Byte>(Byte(dtBytes) or TDIPConst.STD_DEF)
        else
        begin
          if L<=32 then
            Dst.Write<Byte>(TDIPConst.SHSTR_BIT or TDIPConst.SHSTR_BYTES or (L-1))
          else
          begin
            Dst.Write<Byte>(Byte(dtBytes));
            SaveInteger(L);
          end;
          Dst.Write(TDIPBytes(ASrc).Value[0], L);
        end;
      end;
    dtUnicodeString:
      begin
        u8 := Str2UTF8(TDIPUnicodeString(ASrc).Value);
        L := Length(u8);
        if L=0 then
          Dst.Write<Byte>(Byte(dtUnicodeString) or TDIPConst.STD_DEF)
        else
        begin
          if L<=32 then
            Dst.Write<Byte>(TDIPConst.SHSTR_BIT or TDIPConst.SHSTR_STR or (L-1))
          else
          begin
            Dst.Write<Byte>(Byte(dtUnicodeString));
            SaveInteger(L);
          end;
          Dst.Write(u8[Low(u8)], L);
        end;
      end;

    // structure
    dtDictionary:
      if TDIPDictionary(ASrc).Dictionary.Count=0 then
        Dst.Write<Byte>(Byte(dtDictionary) or TDIPConst.STD_DEF)
      else
      begin
        Dst.Write<Byte>(Byte(dtDictionary));
        SaveInteger(TDIPDictionary(ASrc).ContentSize);
        for i := 0 to TDIPDictionary(ASrc).Count-1 do
        begin
          p := TDIPDictionary(ASrc).Pairs[i];
          // save Key (len:int + data)
          L := Length(p.Key);
          SaveInteger(L);
          Dst.Write(p.Key[0], L);
          // save Value (any type)
          SaveValue(p.Value);
        end;
      end;
    dtArray:
      if TDIPArray(ASrc).Count=0 then
        Dst.Write<Byte>(Byte(dtArray) or TDIPConst.STD_DEF)
      else
      begin
        Dst.Write<Byte>(Byte(dtArray));
        SaveInteger(TDIPArray(ASrc).ContentSize);
        for L := 0 to TDIPArray(ASrc).Count-1 do
          SaveValue(TDIPArray(ASrc)[L]);
      end;

    else raise Exception.Create('Error');
  end; // case
end; // method

procedure TDIPBinaryWriter.SaveToStream(ASrc: TDIPValue; ADst: TStream);
begin
  CalcValueSize(ASrc);

  Dst := TStreamWriter.Create(ADst);
  try
    // protocol marker
    //Dst.Write(TDIPConst.DIPID[Low(TDIPConst.DIPID)], Length(TDIPConst.DIPID));
    // contents
    SaveValue(ASrc);
  finally
    FreeAndNil(Dst);
  end;
end;

{ TDIPBinaryReader }

function TDIPBinaryReader.ReadInt(b: Byte): Int64;
begin
  if b and TDIPConst.SHINT_BIT<>0 then
    Result := (b and TDIPConst.SHINT_VAL)+1
  else
  if b and TDIPConst.SHSTR_BIT<>0 then
    Raise Exception.Create('Error')
  else
  if b and TDIPConst.CINT_BIT<>0 then
    if b and TDIPConst.CINT_NEG=0 then
      // 1: 129..high(byte)+129
      // 2: 255+130..high(word)+255+130
      // 4: 65535+255+131..high(longword)+65535+255+131
      case b and TDIPConst.CINT_SIZE of
        0: Result := Longword(Src.Read<Byte>)  + 129;
        1: Result := Longword(Src.Read<Word>)  + 255+130;
        2: Result := Int64(Src.Read<Longword>) + 65535+255+131;
        else Raise Exception.Create('Error');
      end
    else
      // -(AValue+1) sored as:
      // 1: 0..high(Byte)
      // 2: 256..high(Word)+256
      // 4: 65535+257..high(Longword)+65535+257
      case b and TDIPConst.CINT_SIZE of
        0: Result := -(Integer(Src.Read<Byte>)+1);
        1: Result := -(Integer(Src.Read<Word>)+256+1);
        2: Result := -(Int64(Src.Read<Longword>)+65535+257+1);
        else Raise Exception.Create('Error');
      end
  else
  if b and TDIPConst.STD_TYPE<>Byte(dtInteger) then
    Raise Exception.Create('Error')
  else
    if b and TDIPConst.STD_DEF<>0 then
      result := 0
    else
      result := Src.Read<Int64>;
end;

function TDIPBinaryReader.ReadInt: Int64;
begin
  Result := ReadInt(Src.Read<Byte>);
end;

function TDIPBinaryReader.ReadFirstValueSize: Int64;
var
  b: Byte;
  p: Int64;
begin
  b := Src.Read<Byte>;
  if b and TDIPConst.SHINT_BIT<>0 then
    Result := 1
  else
  if b and TDIPConst.SHSTR_BIT<>0 then
    Result := 1 + (b and TDIPConst.SHSTR_VAL)
  else
  if b and TDIPConst.CINT_BIT<>0 then
    case b and TDIPConst.CINT_SIZE of
      0: Result := 2;
      1: Result := 3;
      2: Result := 5;
      else Raise Exception.Create('Error');
    end
  else
  if b and TDIPConst.STD_DEF<>0 then
    result := 1
  else
    case b and TDIPConst.STD_TYPE of
      Byte(dtInteger)       : Result := 1+SizeOf(Int64);
      Byte(dtSingle)        : Result := 1+SizeOf(Single);
      Byte(dtDouble)        : Result := 1+SizeOf(Double);
      Byte(dtCurrency)      : Result := 1+SizeOf(Currency);
      Byte(dtDateTime)      : Result := 1+SizeOf(TDateTime);
      Byte(dtBoolean)       : Result := 1;
      Byte(dtBytes)         : Result := 1 + ReadInt;
      Byte(dtUnicodeString) : Result := 1 + ReadInt;
      Byte(dtDictionary), Byte(dtArray):
        begin
          p := Src.Position;
          Result := 1 + ReadInt;
          inc(Result, Src.Position-p);
        end;
      else Raise Exception.Create('Error');
    end;
end;

function TDIPBinaryReader.ReadShortBytes(ALen: Integer): TDIPBytes;
begin
  Result := TDIPBytes.Create;
  SetLength(Result.Value, ALen);
  Src.Read(Result.Value[0], ALen);
end;

function TDIPBinaryReader.ReadShortString(ALen: Integer): TDIPUnicodeString;
var
  UTFStr: TUTF8String;
begin
  setlength(UTFStr, ALen);
  Src.Read(UTFStr[Low(UTFStr)], ALen);
  Result := TDIPUnicodeString.Create(UTF82Str(UTFStr));
end;

function TDIPBinaryReader.ReadBytes: TDIPBytes;
var
  i: Int64;
  b: TBytes;
begin
  i := ReadInt;
  SetLength(b, i);
  Src.Read(b[0], i);
  Result := TDIPBytes.Create(b);
end;

function TDIPBinaryReader.ReadUnicodeString: TDIPUnicodeString;
var
  i: Int64;
  UTFStr: TUTF8String;
begin
  i := ReadInt;
  setlength(UTFStr, i);
  Src.Read(UTFStr[Low(UTFStr)], i);
  Result := TDIPUnicodeString.Create(UTF82Str(UTFStr));
end;

function CheckRangePosOrZero(const AValue, AMax: int64): Int64; inline;
begin
  if (AValue<0) or (AValue>AMax) then
    RaiseError;
  result := AValue;
end;

function TDIPBinaryReader.ReadDictionary: TDIPDictionary;
var
  L, EndPos: Int64;
  Key: TBytes;
  Value: TDIPValue;
begin
  // we take "Position-1" because we should take in acoount 1 more byte (cmd)
  L := Src.HasData(ReadInt); // content size
  EndPos := Src.Position + L;  // max pos
  Result := TDIPDictionary.Create;
  repeat

    // read Key
    L := ReadInt; // KeyLen
    SetLength(Key, CheckRangePosOrZero(Src.HasData(L), MaxDictionaryKeyLen));
    Src.Read(Key[0], L);

    // read Value
    Value := LoadValue;
    Result.AddOrSetValue(Key, Value);

    // check consistency
    CheckRangePosOrZero(Src.Position, EndPos);
  until Src.Position=EndPos;
end;

function TDIPBinaryReader.ReadArray: TDIPArray;
var
  L, EndPos: Int64;
begin
  // we take "Position-1" because we should take in acoount 1 more byte (cmd)
  L := Src.HasData(ReadInt); // content size
  EndPos := Src.Position + L;  // max pos
  Result := TDIPArray.Create;
  repeat
    Result.Add(LoadValue);
    CheckRangePosOrZero(Src.Position, EndPos);
  until Src.Position=EndPos;
end;

function TDIPBinaryReader.LoadValue: TDIPValue;
var
  b: byte;
begin
  b := Src.Read<Byte>;
  if b and TDIPConst.SHINT_BIT<>0 then
    Result := TDIPInteger.Create((b and TDIPConst.SHINT_VAL)+1)
  else
  if b and TDIPConst.SHSTR_BIT<>0 then
    if b and TDIPConst.SHSTR_TYPE=TDIPConst.SHSTR_BYTES then
      Result := ReadShortBytes((b and TDIPConst.SHSTR_VAL)+1)
    else
      Result := ReadShortString((b and TDIPConst.SHSTR_VAL)+1)
  else
  if b and TDIPConst.CINT_BIT<>0 then
    Result := TDIPInteger.Create(ReadInt(b))
  else
  if b and TDIPConst.STD_DEF<>0 then
    if (b and TDIPConst.STD_TYPE) in [Byte(Low(TDataType))..Byte(High(TDataType))] then
      result := TDIPValue.NewValue(TDataType(b and TDIPConst.STD_TYPE))
    else
      Raise Exception.Create('Error')
  else
    case b and TDIPConst.STD_TYPE of
      Byte(dtInteger):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPInteger.Create(0)
        else
          Result := TDIPInteger.Create(Src.Read<Int64>);
      Byte(dtSingle):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPSingle.Create(0)
        else
          Result := TDIPSingle.Create(Src.Read<Single>);
      Byte(dtDouble):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPDouble.Create(0)
        else
          Result := TDIPDouble.Create(Src.Read<Double>);
      Byte(dtCurrency):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPCurrency.Create(0)
        else
          Result := TDIPCurrency.Create(Src.Read<Currency>);
      Byte(dtDateTime):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPDateTime.Create(0)
        else
          Result := TDIPDateTime.Create(Src.Read<TDateTime>);
      Byte(dtBoolean):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPBoolean.Create(False)
        else
          Result := TDIPBoolean.Create(True);
      Byte(dtBytes):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPBytes.Create
        else
          Result := ReadBytes;
      Byte(dtUnicodeString):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPUnicodeString.Create('')
        else
          Result := ReadUnicodeString;
      Byte(dtDictionary):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPDictionary.Create
        else
          Result := ReadDictionary;
      Byte(dtArray):
        if b and TDIPConst.STD_DEF<>0 then
          Result := TDIPArray.Create
        else
          Result := ReadArray;
      else Raise Exception.Create('Error');
    end;
end;

constructor TDIPBinaryReader.Create;
begin
  MaxDictionaryKeyLen := 1*GB;
end;

function TDIPBinaryReader.LoadFromStream(ASrcStream: TStream; AMaxSize: UInt64 = 1*GB): TDIPValue;
var
  Size, Position: Int64;
begin
  Src := TStreamReader.Create(ASrcStream);
  try
    Position := Src.Position;
    Size := ReadFirstValueSize;
    if (Size>AMaxSize) or (Size>Src.Size-Position) then
      RaiseError;
    Src.SetReadFrame(Position, Size);
    Result := LoadValue;
  finally
    FreeAndNil(Src);
  end;
end;

{ TDIPDictionary.TPair }

constructor TDIPDictionary.TPair.Create(AKey: TBytes; AValue: TDIPValue);
begin
  FKey := AKey;
  FValue := AValue;
end;

destructor TDIPDictionary.TPair.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TDIPDictionary.TPair.SetValue(AValue: TDIPValue);
begin
  if AValue<>FValue then
  begin
    FValue.Free;
    FValue := AValue;
  end;
end;

{ TDIPArray.TDIPArrayEnumerator }

procedure TDIPArray.TDIPArrayEnumerator.Init(AList: TObjectList<TDIPValue>);
begin
  FList := AList;
  FIndex := -1;
  FMaxIndex := AList.Count-1;
end;

function TDIPArray.TDIPArrayEnumerator.MoveNext: Boolean;
begin
  result := FIndex<FMaxIndex;
  if result then
    inc(FIndex);
end;

function TDIPArray.TDIPArrayEnumerator.GetCurrent: TDIPValue;
begin
  result := FList[FIndex];
end;

{ TDIPDictionary.TDIPDictionaryKeysEnumerator }

procedure TDIPDictionary.TDIPDictionaryKeysEnumerator.Init(
  APairs: TObjectList<TPair>);
begin
  FPairs := APairs;
  FIndex := -1;
  FMaxIndex := APairs.Count-1;
end;

function TDIPDictionary.TDIPDictionaryKeysEnumerator.MoveNext: Boolean;
begin
  result := FIndex<FMaxIndex;
  if result then
    inc(FIndex);
end;

function TDIPDictionary.TDIPDictionaryKeysEnumerator.GetCurrent: TBytes;
begin
  result := FPairs[FIndex].Key;
end;

end.
