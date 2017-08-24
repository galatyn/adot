unit adot.Tools;
{$OVERFLOWCHECKS OFF}
{$IFNDEF Debug}
  { $Define UseInline}
{$ENDIF}

{ Definition of classes/record types:

  TArrayStream<T: record> = class
    Makes any array of ordinal type to be accessable as stream of bytes.

  TArrayUtils = record
    Fill/fillRandom/Randomize and other tools for arrays.

  TBuffer = record
    Simple and fast managed analog of TMemoryStream.

  TCachable = class
    Basic class for objects with data caching/other read optimizations.

  TComponentUtils = class
    Enumeration and other component-specific tools.

  TCurrencyUtils = class
    Currency type utils.

  TCustomHash = class
    Abstract class for hashes.

  TCustomStreamExt = class
    Extensions of TStream.

  TDateTimeRec = record
    Record type to define TDateTime compatible constants in readable way.

  TDateTimeUtils = class
    Check TDateTime correctness, convert to string etc.

  TDelegatedMemoryStream = class
    Readonly stream for specified memory block.

  TFileUtils = class
    File manipulation utils.

  TGUIDUtils = class
    IsValid and other utils.

  THashes = class
    Simple API for hashing functions (including CRC32/Adler32)

  THex = class
    Set of functions for HEX conversion.

  TIfThen = class
    Generic implementation of IfThen (to accept virtually any type). Example:
     A := TIfThen.Get(Visible, fsMDIChild, fsNormal);

  TIndex = class
    Utils for index arrays (RandomSelection, Direct, Inverse etc)

  TIndexBackEnumerator = record
    Index enumerator "from last to first".

  TInterfacedObject<T: class> = class
    Wrapper class to make any class type available through interface (that means that
    lifetime of the class will be controlled by reference counter)

  TNullable<T> = record
    Extends any type by IsNull property.

  TPIReader = class
    Platform independent stream reader.

  TPIWriter = class
    Platform independent stream writer.

  TRLE = class
    PDF-compatible RLE codec.

  TReader = record
    Stream block reader (ReadNext to get next block of data from stream as array of byte)

  TStreamUtils = class
    Block reader and other utils.

  TTimeOut = record
    Allows to avoid of some operations to be executed too often.

  TTiming = class
    Time measuring functions (recursive Start/Stop etc)

}
interface

uses
  adot.Types,
  adot.Collections,
  adot.Arithmetic,
  adot.Tools.Rtti,
{$IFDEF MSWINDOWS}
  { Preventing from "_Inline has not been expanded" }
  Winapi.Windows,
{$ENDIF}
  System.DateUtils,
  System.Types,
  System.Rtti,
  System.Math,
  System.IOUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Hash,
  System.Classes,
  System.SysUtils,
  System.StrUtils,
  System.Character,
  System.TimeSpan,
  System.Diagnostics,  { TStopwatch }
  System.SyncObjs,     { TInterlocked.* }
  System.Variants,
  System.ZLib,
  System.SysConst;

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
    class function EncodeAnsiString(const s: AnsiString):String; static;
    class function EncodeByteH(Src: byte): char; static;
    class function EncodeByteL(Src: byte): char; static;

    class procedure Decode(const HexEncodedStr: String; var Buf); overload; static;
    class function Decode<T: Record>(const HexEncodedStr: String): T; overload; static;
    class function DecodeBytes(const HexEncodedStr: String):TBytes; static;
    class function DecodeString(const HexEncodedStr: string): string; overload; static;
    class function DecodeString(const HexEncodedStr: string; utf8: boolean): string; overload; static;
    class function DecodeAnsiString(const HexEncodedStr: String):AnsiString; static;
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

  THashData = TArray<byte>;

  { Abstract class for hashes }
  TCustomHash = class abstract
  protected

    { used by CRC/Adler to transform 32bit hash to TBytes }
    class function Hash32ToBytes(Hash: Cardinal): TBytes; static;

    { single call }
    class function DoEncode(const Buf; ByteBufSize: integer): TBytes; overload; virtual; abstract;
    class function DoEncode(S: TStream): TBytes; overload; virtual; abstract;

    { streaming }
    class procedure DoInit(out Hash: THashData); virtual; abstract;
    class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); virtual; abstract;
    class function DoDone(var Hash: THashData): TBytes; virtual; abstract;
  public

    { single call functions }
    class function Encode(const Buf; ByteBufSize: integer): TBytes; overload;
    class function Encode(S: TStream): TBytes; overload;
    class function Encode(const S: TBytes; StartIndex,Count: integer): TBytes; overload;
    class function Encode(const S: TBytes): TBytes; overload;
    class function Encode(const S: string): TBytes; overload;
    class function EncodeAnsiString(const S: AnsiString): TBytes;
    class function EncodeFile(const AFileName: string): TBytes; overload;

    { streaming functions }
    class procedure Init(out Hash: THashData);

    { for values with fixed length, hash will be generated from data only }
    class procedure Update(const Value; ValueByteSize: integer; var Hash: THashData); overload;
    class procedure Update(const Value: integer;      var Hash: THashData); overload;
    class procedure Update(const Value: double;       var Hash: THashData); overload;

    { for values with variable length, hash will be generated from length and data }
    class procedure Update(const Value: TArray<byte>; var Hash: THashData); overload;
    class procedure Update(const Value: string;       var Hash: THashData); overload;

    class function Done(var Hash: THashData): TBytes;

  end;

  THashClass = class of TCustomHash;

  { AH: Don't use THashMD5/THashSHA1 directly, implementation in XE8 has serious bugs:
    http://qc.embarcadero.com/wc/qcmain.aspx?d=132100
    AH (update from 05.04.2016): The issue is fixed, in Delphi 10 Seattle it works correctly.
    Why we still keep THashes class:
    - For now Delphi doesn't have CRC/Adler (usefull for files, but can be replaced by THashBobJenkins)
    - In object model it is much easier to introduce new functions (like hash file/stream etc)
    - If other issues will be discovered in Delphi lib, we can fix it without changes in the code

   Simple API for hashing functions (including CRC32/Adler32). Example:

    function GetHash(L: TList<TRec>): string;
    var
      H: THashClass;
      D: THashData;
      I: Integer;
    begin
      H := THashUtils.Strong;
      H.Init(D);
      for I := 0 to List.Count-1 do
        H.Update(L[I], SizeOf(L[I]), D);
      result := THashUtils.HashToString(H.Done(D));
    end;

    Same example without THashClass:

    function GetHash(L: TList<TRec>): string;
    var
      D: THashData;
      I: Integer;
    begin
      THashUtils.Strong.Init(D);
      for I := 0 to List.Count-1 do
        THashUtils.Strong.Update(L[I], SizeOf(L[I]), D);
      result := THashUtils.HashToString(THashUtils.Strong.Done(D));
    end; }
  THashUtils = class
  public
    const
      StreamingBufSize = 64*1024;

    type
      MD5 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      SHA1 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      { we use default 256bit hash (use THashSHA2 directly for other options - 224bit,384bit,...) }
      SHA2 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      CRC32 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      Adler32 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      BobJenkins32 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      { Strong hash for critical parts (password checksum etc).
        MD5 is outdated for use in cryptography, but for other tasks it's still good enough }
      Strong = MD5;

      { Fast hash with good avalanche effect (hash tables etc). }
      Fast = BobJenkins32;

      { Fastest hash for detection of modifications in massive data arrays (file checksum etc).
        We use Adler32, it's two times faster than Crc32 and still quite good. }
      Fastest = Adler32;

    { general }
    class function Mix(const HashA,HashB: integer): integer; overload; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function Mix(const HashA,HashB,HashC: integer): integer; overload; static;
    class function Mix(const HashA,HashB: TBytes): TBytes; overload; static;
    class function Mix(const Hashes: array of integer): integer; overload; static;
    class function Mix(const Hashes: array of TBytes): TBytes; overload; static;

    class function HashToString(const AHash: TBytes): string; static;

    class function GetHash32(const Hash: TBytes): integer; static;
    class function GetHash24(const Hash: TBytes): integer; static;
    class function GetHash16(const Hash: TBytes): integer; static;
  end;

  THashes = THashUtils;

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

  { Extensions of TStream }
  TCustomStreamExt = class(TStream)
  public
    procedure Clear;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
  end;

  { Readonly stream for specified memory block }
  TDelegatedMemoryStream = class(TCustomMemoryStream)
  public
    constructor Create(Buf: pointer; Size: integer); overload;
    procedure SetMemory(Buf: pointer; Size: integer);
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
  end;

  { Makes any array of ordinal type to be accessable as stream of bytes }
  TArrayStream<T: record> = class(TCustomStreamExt)
  protected
    FItems: TArray<T>;
    FPos: integer;
    FSize: integer;
    FCapacity: integer;

    procedure SetItems(const Value: TArray<T>);
    procedure SetCapacity(NewByteCapacity: integer);
  public
    constructor Create(const AItems: TArray<T>); overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure SetSize(const NewSize: Int64); override;
    procedure SetSize(NewSize: Longint); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;

    property Items: TArray<T> read FItems write SetItems;
  end;

  { Basic class for readonly streams. All Write/Seek methods will generate exception.}
  TCustomReadOnlyStream = class(TStream)
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    function Write(const Buffer; Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    { methods to be implemented in descendants }

    //function Read(var Buffer; Count: Longint): Longint; override;
    //function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
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
  end;

  { Check TDateTime correctness, convert to string etc }
  TDateTimeUtils = class
  public
    const
      NoDateStr = '';

    class function IsCorrectDate(const t: TDateTime): boolean; static;
    class function ToStr(const t: TDateTime; ANoDateStr: string = NoDateStr): string; static;
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

  { Block reader and other utils }
  TStreamUtils = class
  public
    type

      { Stream block reader (ReadNext to get next block of data from stream as array of byte) }
      TReader = record
      private
        AutoFreeCollection: TAutoFreeCollection;
        Stream: TStream;
        BytesToRead: int64;
      public
        Bytes: TArray<Byte>;
        Count: integer;

        constructor Create(Src: TStream; AOwnsStream: Boolean; BufSize: integer; FromBeginning: boolean = True);
        function ReadNext: Boolean;
      end;

    class procedure StringToStream(const S: string; Dst: TStream; Encoding: TEncoding = nil); static;

    { Copy stream functions (from simple to feature-rich):
      1. TStreamUtils.Copy:
           uses standard Delphi streams, UI will freeze until operation is complete.
      2. TVCLStreamUtils.Copy:
           uses standard Delphi streams, UI will not freeze. }
    class function Copy(Src,Dst: TStream; Count,BufSize: integer; ProgressProc: TCopyStreamProgressProc): int64; overload; static;
    class function Copy(Src,Dst: TStream; ProgressProc: TCopyStreamProgressProc): int64; overload; static;
    class function Copy(Src,Dst: TStream): int64; overload; static;
  end;

  TDelegatedOnFile = reference to procedure(const APath: string; const AFile: TSearchRec; var ABreak: Boolean);
  { File manipulation utils }
  TFileUtils = class
  public

    { remove incorrect chars }
    class function RemoveInvalidChars(const AFileName: string): string; static;

    class function FileModeToString(AMode: Integer): string; static;
    class function AccessAllowed(const AFileName: string; ADesiredAccess: word = fmOpenReadWrite or fmShareExclusive): boolean; static;
    class function GetOpenFiles(const AMasks,Exceptions: array of string; Recursive: boolean): TArray<string>; overload; static;
    class function GetOpenFiles(const AMasks,Exceptions: array of string; Recursive: boolean; Delim: string): string; overload; static;
    class function GetSize(const AFileName: string): int64; static;

    { enumerate files with callback function and posibility to break/stop }
    class procedure EnumFiles(const AFileNamePattern: string; AOnfile: TDelegatedOnFile); static;

    { simple API to clean up old files and keep used space in some ranges (log files, temp files etc) }
    class procedure CleanUpOldFiles(
      const AFileNamePattern                       : string;
      const AMaxAllowedTotalSize, AMaxAllowedCount : int64;
            AChanceToRun                           : Double = 1 { 1 = 100%, 0.3=30% etc }); static;

    class function DeleteFile(const AFileName: string; out AErrorMessage: string): boolean; static;
    class function RenameFile(const ASrcFileName,ADstFileName: string; out AErrorMessage: string): boolean; static;

    { Copy file functions (from simple to feature-rich):
      1. TFileUtils.CopyFile:
           uses standard Delphi streams, UI will freeze until operation is complete.
      2. TWinFileUtils.CopyFile:
           uses Windows function CopyFileEx, UI will freeze until operation is complete.
      3. TVCLFileUtils.Copyfile:
           uses standard Delphi streams, UI will not freeze.
      4. TCopyFileProgressDlg.CopyFile:
           uses either Delphi streams or CopyFileEx, UI will not freeze,
           progress bar with cancel command will be available for long operations). }
    class function CopyFile(const SrcFileName,DstFileName: string; out ErrorMessage: string; ProgressProc: TCopyFileProgressProc): boolean; overload;
    class function CopyFile(const SrcFileName,DstFileName: string; out ErrorMessage: string): boolean; overload;

    class procedure Load<T: record>(const FileName: string; var Dst: TArray<T>); overload;
    class procedure Save<T: record>(const FileName: string; const Src: TArray<T>; Index,Count: integer); overload;
    class procedure Save<T: record>(const FileName: string; const Src: TArray<T>); overload;

    { Encode disabled chars (hex), check disabled names ("COM1", "PRN", "NUL" etc). }
    class function StringToFilename(const AStr: string): string; static;

    { When we check all/lot of files for existance in some folder, it is faster to list files than call FileExists.
      It is especially true for network drives. Example of timings for network folder with many files:
        FileExists for every file : 31.65 sec
        Enumerate all files       :  4.19 sec }
    { Preload list of files into cache }
    class procedure ExistsBuildCache(const CacheFolder: string; var Cache: TSet<string>; Recursive: boolean = True); static;
    { Will use preloaded Cache when possible and FileExists function otherwise }
    class function Exists(const FullFileName,CacheFolder: string; var Cache: TSet<string>): boolean; static;
  end;

  { Deprecated, use Sys.IfThen instead }
  { Generic implementation of IfThen (to accept virtually any type). Example:
     A := TIfThen.Get(Visible, fsMDIChild, fsNormal); }
  TIfThen = class
  public
    class function Get<T>(ACondition: Boolean; AValueTrue,AValueFalse: T):T; static;
  end;

  TFun = class
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

  { Shortcut to TFun ("Sys.FreeAndNil" looks more natural than "TFun.FreeAndNil"). }
  Sys = TFun;

  {  Can be used as default enumerator in indexable containers (to implement "for I in XXX do" syntax), example:

     function TListExt.GetEnumerator: TIndexBackEnumerator;
     begin
       result := TIndexBackEnumerator.Create(LastIndex, StartIndex);
     end; }
  { Index enumerator "from last to first". }	 
  TIndexBackEnumerator = record
  private
    FCurrentIndex, FToIndex: integer;
  public
    constructor Create(AIndexFrom, AIndexTo: integer);
    function GetCurrent: Integer;
    function MoveNext:   Boolean;

    property Current:    Integer read GetCurrent;
  end;

  {  Simple API for time measuring (supports recursion and calculates sum time). Example:
     TTiming.Start;
       <do something>
     OpTime := TTiming.Stop;
     Caption := Format('Execution time: %.2f seconds, [OpTime.TotalSeconds]);
       or
     OpTime := TTiming.Stop('TMyForm.LoadData', TotalTime);
     Caption := Format('Execution time: %.2f seconds (total: %.2f), [OpTime.TotalSeconds, TotalTime.TotalSeconds]); }
  { Time measuring functions (recursive Start/Stop etc) }	 
  TTiming = class
  protected
    type
      TTotalStat = record
      private
        Span: TTimeSpan;
        Calls: int64;
      public
        constructor Create(const ASpan: TTimeSpan; const ACalls: int64);
      end;

    class var
      FTimeStack: TStack<TStopwatch>;
      FTotalTimes: TDictionary<string, TTotalStat>;

    class function GetTimeStack: TStack<TStopwatch>; static;
    class function GetTotalTimes: TDictionary<string, TTotalStat>; static;
    class destructor DestroyClass; static;

    class property TimeStack: TStack<TStopwatch> read GetTimeStack;
    class property TotalTimes: TDictionary<string, TTotalStat> read GetTotalTimes;
  public
    class procedure Start; static;
    class function Stop: TTimeSpan; overload; static;
    class function Stop(const OpId: string; out ATotalStat: TTotalStat): TTimeSpan; overload; static;
    class function StopAndGetCaption(const OpId: string): string; overload; static;
    class function StopAndGetCaption(const OpId,Msg: string): string; overload; static;
  end;

  {  Simple API to perform periodical actions. Example:
       T.StartSec(1, 10);    Timeout is 1 sec, check for timeout every 10th iteration.
       for i := 0 to Count-1 do
         if T.Timedout then
           Break
         else
           ProcessItem(I); }
  { Allows to avoid of some operations to be executed too often }	 
  TTimeOut = record
  private
    FOpTimedOut: Boolean;
    FCounter: integer;
    FCheckPeriod: integer;
    FStartTime: TDateTime;
    FMaxTimeForOp: TDateTime;

    function GetTimedOut: Boolean;

  public
    { If we check for timeout every iteration, usually (when iterations are short)
      it is too often and our checks may consume more time then usefull work itself.
      To check only 1 time for N iterations set ACheckPeriod=N. }
    procedure Start(AMaxTimeForOp: TDateTime; ACheckPeriod: integer = 0);
    procedure StartSec(AMaxTimeForOpSec: double; ACheckPeriod: integer = 0);
    procedure StartInfinite;
    procedure Restart;

    { If set True manually, then it will be constantly True until next Start* }
    property TimedOut: Boolean read GetTimedOut write FOpTimedOut;
    property StartTime: TDateTime read FStartTime;
  end;

  { Currency type utils }
  TCurrencyUtils = class
  public
    class function ToString(const Value: Currency; FractionalPart: boolean = False): string; reintroduce; static;
  end;

  { Delphi 10 Seattle doesn't have 100% platform independent reader/writer (inherited from TFiler or any other).
    But TWriter class has set of platform independent functions WriteVar. We write wrapper around TWriter
    in order to expose only platform independent functionality (and extend it for other basic types). }
  { Platform independent stream writer }	
  TPIWriter = class
  protected
    Writer: TWriter;
  public
    constructor Create(ADst: TStream);
    destructor Destroy; override;

    { mapped to TWriter }
    procedure Write(const Value: Char); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: Int8); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: UInt8); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: Int16); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: UInt16); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: Int32); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: UInt32); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: Int64); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: UInt64); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: Single); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: Double); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: Extended); overload; {$IFDEF UseInline}inline;{$ENDIF}

    { extentions }
    procedure Write(const Buf; Count: integer); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Write(const Value: TBytes); overload;
    procedure Write(const Value: string); overload;
  end;

  { Platform independent stream reader }
  TPIReader = class
  protected
    Reader: TReader;
  public
    constructor Create(ASrc: TStream);
    destructor Destroy; override;

    { mapped to TReader }
    procedure Read(out Value: Char); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: Int8); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: UInt8); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: Int16); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: UInt16); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: Int32); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: UInt32); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: Int64); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: UInt64); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: Single); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: Double); overload; {$IFDEF UseInline}inline;{$ENDIF}
    procedure Read(out Value: Extended); overload; {$IFDEF UseInline}inline;{$ENDIF}

    { extentions }
    procedure Read(var Buf; Count: integer); overload;
    procedure Read(out Value: TBytes); overload;
    procedure Read(out Value: string); overload;
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

    { we use class functions because parameterless constructors are not allowed here }
    class function Create: TBox<T>; overload; static;
    class function Create(const AValue: T): TBox<T>; overload; static;

    procedure Clear;

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

  { PDF-compatible RLE codec }
  TRLE = class
  public
    class function MaxEncodedSize(ASrcSize: cardinal): cardinal;
    class function Encode(const ASrc; ASrcSize: cardinal; var ADest): cardinal;

    class function DecodedSize(const ASrc; APackedSize: cardinal): cardinal;
    class procedure Decode(const ASrc; APackedSize: cardinal; var ADest; AUnpackedSize: cardinal);
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
  end;

  { Executes custom action (procedure/method) when last instance goes out of scope (automatic finalization etc). }
  TOutOfScopeAction = record
  private
    FProc: IInterfacedObject<TObject>;
  public
    constructor Create(AProc: TProc);
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
    constructor Create(AProc: TProc<T>; AValue: T);
  end;

  { Lightweight and managed analog of TBytesStream.
    Has mixed set of methods - byte and string-compatible.
    Check also TStringBuffer from *.Strings unit. }
  TBuffer = record
  private
    FData: TArray<Byte>;
    FSize: integer;
    FPosition: integer;

    procedure SetSize(Value: integer);
    function GetCapacity: integer; {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetCapacity(Value: integer); {$IFDEF UseInline}inline;{$ENDIF}
    procedure CheckCapacity(MinCapacity: integer); {$IFDEF UseInline}inline;{$ENDIF}
    function GetLeft: integer; {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetLeft(Value: integer); {$IFDEF UseInline}inline;{$ENDIF}
    function GetCurrentData: pointer; {$IFDEF UseInline}inline;{$ENDIF}
    function GetEOF: boolean; {$IFDEF UseInline}inline;{$ENDIF}
    function GetText: string;
    procedure SetText(const Value: string);
    function GetEmpty: Boolean; {$IFDEF UseInline}inline;{$ENDIF}
    procedure SetData(AData: TArray<Byte>);
  public

    { writes bytes from Src to current position. }
    procedure Write(const Src; ByteCount: integer); overload;
    { writes characters from Src to the buffer (usually 1 char = 2 bytes). }
    procedure Write(const Src: string; CharOffset,CharCount: integer); overload;
    { writes all characters from Src to the buffer (usually 1 char = 2 bytes). }
    procedure Write(const Src: string); overload;

    { Reads bytes from the buffer to Dst. }
    procedure Read(var Dst; ByteCount: integer); overload;
    { Reads characters from the buffer to specific position of Dst (usually 1 char = 2 bytes). }
    procedure Read(var Dst: string; DstCharOffset,CharCount: integer); overload;
    { Reads characters from the buffer to Dst (usually 1 char = 2 bytes). }
    procedure Read(var Dst: string; CharCount: integer); overload;

    { Removes part of data from the buffer. }
    procedure Cut(Start,Len: integer);
    { Returns range of the buffer as array of bytes. }
    function Slice(Start,Len: integer): TArray<byte>;

    { Makes sure that Capacity is equal to Size. After that Data field can be used directly. }
    procedure TrimExcess;
    procedure Clear;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    property Size: integer read FSize write SetSize;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Position: integer read FPosition write FPosition;
    property Left: integer read GetLeft write SetLeft;
    property CurrentData: pointer read GetCurrentData;
    property EOF: boolean read GetEOF;
    property Empty: boolean read GetEmpty;
    property Text: string read GetText write SetText;
    property Data: TArray<Byte> read FData write SetData;
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

    constructor Create(const AValue: int64);
    constructor CreateMb(const AValue: double);

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

  { Summed area table. Precalculates sums for very fast calculation of SUM of any area [x1,y1,x2,y2].
    https://en.wikipedia.org/wiki/Summed_area_table}
  TIntegralImageInt64 = record
  private
    procedure BuildLine(ADst: PInt64Array);
    function GetLine(y: integer): PInt64Array;
    function GetSum(x1, y1, x2, y2: integer): int64;
    function GetAvg(x1, y1, x2, y2: integer): int64;
  public
    Image: array of int64;
    Width,Height: integer;

    procedure SetSize(AWidth,AHeight: integer);

    procedure Build;

    { release memory etc }
    procedure Clear;

    { usefull to fill with initial values }
    property Lines[y: integer]: PInt64Array read GetLine;

    { Fastest, no range checks etc }
    property Sum[x1,y1,x2,y2: integer]:int64 read GetSum;

    { positions are adjusted to be inside of the image }
    property Avg[x1,y1,x2,y2: integer]:int64 read GetAvg; default;
  end;

  TInterpolation_Int64Custom = class
  public
    type
      TPt = record
        X,Y: int64;
      end;

  protected
    FPoints: TArray<TPt>;
    FUpdateCnt: integer;
    FComparer: IComparer<TPt>;

    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
    function DoGetValue(const X: int64):int64; virtual; abstract;

    function GetPointCount: integer;
    function GetPoint(i: integer): TPt;
    procedure SetPoint(i: integer; const Value: TPt);

  public
    constructor Create(PointCount: integer); virtual;

    procedure BeginUpdate;
    procedure EndUpdate;

    property PointCount: integer read GetPointCount;
    property Points[i: integer]: TPt read GetPoint write SetPoint;

    property Values[const x: int64]: int64 read DoGetValue; default;
  end;

  TLinearInterpolation_Int64 = class(TInterpolation_Int64Custom)
  protected
    function DoGetValue(const X: int64):int64; override;
  end;

  TDebugUtils = class
  public

    { returns True if application is started from IDE for example }
    class function DebuggerIsAttached: boolean; static;
  end;

  { Used internally by TSmartComponentPtr/TWeakComponentPtr.
    They are simpler to use, but sometimes class type is prefered over record type. }
  TSharedComponentPtrClass<T: TComponent> = class(TComponent)
  protected
    FComponent: T;
    FWeak: boolean;

    procedure SetComponent(AComponent: T);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AComponent: T; AWeak: boolean = False); reintroduce;
    destructor Destroy; override;

    property Component: T read FComponent write SetComponent;
  end;

  { Smart pointer features (~shared_ptr in C++):
    - Maintains reference counting ownership of its contained component in cooperation with all copies of the TSharedComponent.
      An component referenced by the contained pointer will be destroyed when all copies of the TSharedComponent have been destroyed.
    - Contained component can be also destroyed explicitly, then all copies of the TSharedComponent became empty.
    - If an new component is assigned, then the old one is destroyed and all copies of the TSharedComponent get the new component.
    }
  TSharedComponentPtr<T: TComponent> = record
  private
    FComponentPtr: IInterfacedObject<TSharedComponentPtrClass<T>>;

    function GetValue: T;
    procedure SetValue(const Value: T);
    function GetIsEmpty: boolean;
  public

    { TSharedComponentPtr should be initialized with Create/Init/Copy, otherwise its content is undefined.
      Create and Init does the same, but have different syntax. Next two lines are equivalent:
        A := TSharedComponentPtr<TLabel>.Create(Lbl);
        A.Init(Lbl);
      TSharedComponentPtr can be initialized as empty:
        "A := TSharedComponentPtr<TLabel>.Create(nil)" or "A.Init(nil)".  }
    constructor Create(AValue: T);
    procedure Init(AValue: T);

    { Makes the TSharedComponentPtr empty and uninitialized. Connection with other copies of the TSharedComponentPtr
      will be lost. If it is the last copy of TSharedComponentPtr, then the contained object will be destroyed. }
    procedure Clear;

    { TSharedComponentPtr must be initialized before "Copy" call is made.
      Otherwise content of the created copy of the TSharedComponentPtr is undefined too.
      It is allowed to create copies of empty container, after assigning of component to
      any instance of TSharedComponentPtr, the all copies will get that value. }
    function Copy: TSharedComponentPtr<T>;

    { Access to the contained component. It can be destroyed explicitly and then all copies of the TSharedComponentPtr
      became empty: "SharedComponent.Value.Free;" }
    property Value: T read GetValue write SetValue;

    { Returns True if the TSharedComponentPtr contains nil.
      If the TSharedComponentPtr is not initialized by Create/Init/Copy and not cleared by Clear,
      then result of IsEmpty is undefined. }
    property IsEmpty: boolean read GetIsEmpty;
  end;

  { Smart pointer features (~weak_ptr in C++):
    - Does not take ownership of its contained component.
    - If contained component is destroyed, then all copies of the TWeakComponent became empty.
    - If an new component is assigned, then all copies of the TSharedComponentPtr get the new component.
    }
  TWeakComponentPtr<T: TComponent> = record
  private
    FComponentPtr: IInterfacedObject<TSharedComponentPtrClass<T>>;

    function GetValue: T;
    procedure SetValue(const Value: T);
    function GetIsEmpty: boolean;
  public

    { TWeakComponentPtr should be initialized with Create/Init/Copy, otherwise its content is undefined.
      Create and Init does the same, but have different syntax. Next two lines are equivalent:
        A := TWeakComponentPtr<TLabel>.Create(Lbl);
        A.Init(Lbl);
      TWeakComponentPtr can be initialized as empty:
        "A := TWeakComponentPtr<TLabel>.Create(nil)" or "A.Init(nil)".  }
    constructor Create(AValue: T); overload;
    //constructor Create(ASharedValue: TSharedComponentPtr<T>); overload;
    procedure Init(AValue: T); overload;
    //procedure Init(ASharedValue: TSharedComponentPtr<T>); overload;

    { Makes the TWeakComponentPtr empty and uninitialized. Connection with other copies
      of the TWeakComponentPtr will be lost. }
    procedure Clear;

    { TWeakComponentPtr must be initialized before "Copy" call is made.
      Otherwise content of the created copy of the TWeakComponentPtr is undefined too.
      It is allowed to create copies of empty container, after assigning of component to
      any instance of TWeakComponentPtr, the all copies will get that value. }
    function Copy: TWeakComponentPtr<T>;

    { Access to the contained component. It can be destroyed explicitly and then all copies of the TWeakComponentPtr
      became empty: "WeakComponent.Value.Free;" }
    property Value: T read GetValue write SetValue;

    { Returns True if the TWeakComponentPtr contains nil.
      If the TWeakComponentPtr is not initialized by Create/Init/Copy and not cleared by Clear,
      then result of IsEmpty is undefined. }
    property IsEmpty: boolean read GetIsEmpty;
  end;

  TEventStat = class
  private
    FEvents: TMap<string, int64>;

  public
    procedure Reg(const EventCategory: string); overload;
    procedure Reg(const EventCategory: string; Count: int64); overload;
    procedure UnReg(const EventCategory: string); overload;
    procedure UnReg(const EventCategory: string; Count: int64); overload;
    procedure Add(const Src: TArray<TPair<string, int64>>);
    procedure Clear;
    function GetStat: TArray<TPair<string, int64>>;
  end;

implementation

Uses
  adot.Strings;

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

class function THex.Encode(const Buf; ByteBufSize: integer): String;
begin
  SetLength(result, EncodedSizeChars(ByteBufSize));
  BinToHex(@Buf, PChar(result), ByteBufSize);
end;

class function THex.EncodeAnsiString(const s: AnsiString): String;
begin
  { need this check only to range check error (when "check range check" is on) }
  if s='' then result := '' else
    result := Encode(s[Low(s)], length(s)*SizeOf(s[Low(s)]));
end;

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

class procedure THex.Decode(const HexEncodedStr: String; var Buf);
begin
  HexToBin(PChar(HexEncodedStr), Buf, DecodedSizeBytes(length(HexEncodedStr)));
end;

class function THex.Decode<T>(const HexEncodedStr: String): T;
begin
  Decode(HexEncodedStr, Result);
end;

class function THex.DecodeAnsiString(const HexEncodedStr: String): AnsiString;
begin
  { need this check only to range check error (when "check range check" is on) }
  if HexEncodedStr='' then Result := '' else
  begin
    SetLength(Result, DecodedSizeBytes(length(HexEncodedStr))); { AnsiString is always 1 byte per char }
    Decode(HexEncodedStr, result[Low(result)]);
  end;
end;

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
  for i := ZeroBasedStartIdx to ZeroBasedStartIdx+Len-1 do
    if not IsValid(HexEncodedStr.Chars[i]) then
      Exit(False);
  Result := True;
end;

class function THex.IsValid(const HexEncodedStr: String): Boolean;
begin
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

{ TDelegatedMemoryStream }

constructor TDelegatedMemoryStream.Create(Buf: pointer; Size: integer);
begin
  inherited Create;
  SetPointer(Buf, Size);
end;

procedure TDelegatedMemoryStream.SetMemory(Buf: pointer; Size: integer);
begin
  SetPointer(Buf, Size);
  Position := 0;
end;

procedure TDelegatedMemoryStream.SetSize(NewSize: Integer);
begin
  raise Exception.Create('Error');
end;

procedure TDelegatedMemoryStream.SetSize(const NewSize: Int64);
begin
  raise Exception.Create('Error');
end;

function TDelegatedMemoryStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('Error');
end;

function TDelegatedMemoryStream.Write(const Buffer: TBytes; Offset, Count: Integer): Longint;
begin
  raise Exception.Create('Error');
end;

{ TCustomStreamExt }

procedure TCustomStreamExt.Clear;
begin
  Size := 0;
end;

procedure TCustomStreamExt.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomStreamExt.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomStreamExt.LoadFromStream(Stream: TStream);
var
  P: Int64;
begin
  Size := Stream.Size;
  P := Stream.Position;
  try
    Stream.Position := 0;
    CopyFrom(Stream, Stream.Size);
  finally
    Stream.Position := P;
  end;
end;

procedure TCustomStreamExt.SaveToStream(Stream: TStream);
var
  P: Int64;
begin
  P := Position;
  try
    Position := 0;
    Stream.CopyFrom(Self, Size);
  finally
    Position := P;
  end;
end;

{ TArrayStream<T> }

constructor TArrayStream<T>.Create(const AItems: TArray<T>);
begin
  inherited Create;
  Items := AItems;
end;

procedure TArrayStream<T>.SetItems(const Value: TArray<T>);
begin
  FItems := Value;
  FPos := 0;
  FCapacity := Length(FItems)*SizeOf(T);
  FSize := FCapacity;
end;

procedure TArrayStream<T>.SetCapacity(NewByteCapacity: integer);
begin
  if NewByteCapacity mod SizeOf(T)<>0 then
    raise Exception.Create('Error');
  SetLength(FItems, NewByteCapacity div SizeOf(T));
  FCapacity := NewByteCapacity;
  if FSize>FCapacity then
    FSize := FCapacity;
  if FPos>FCapacity then
    FPos := FCapacity;
end;

function TArrayStream<T>.Read(var Buffer; Count: Integer): Longint;
begin
  if (FPos >= 0) and (Count >= 0) then
  begin
    Result := FSize - FPos;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      Move((PByte(FItems) + FPos)^, Buffer, Result);
      Inc(FPos, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function TArrayStream<T>.Read(Buffer: TBytes; Offset, Count: Integer): Longint;
begin
  Result := Read(Buffer[Offset], Count);
end;

procedure TArrayStream<T>.SetSize(const NewSize: Int64);
var
  OldPosition: Longint;
begin
  OldPosition := FPos;
  SetCapacity(NewSize); { Will check that NewSize mod SizeOf(T)=0 }
  FSize := NewSize;
  if OldPosition > NewSize then Seek(0, soEnd);
end;

procedure TArrayStream<T>.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

function TArrayStream<T>.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning : FPos := Offset;
    soCurrent   : Inc(FPos, Offset);
    soEnd       : FPos := FSize + Offset;
  end;
  if FPos<0 then
    FPos := 0;
  Result := FPos;
end;

function TArrayStream<T>.Write(const Buffer; Count: Integer): Longint;
var
  P: integer;
begin
  if (FPos >= 0) and (Count >= 0) then
  begin
    P := FPos + Count;
    if P > 0 then
    begin
      if P > FSize then
      begin
        if P > FCapacity then
          SetCapacity(P);
        FSize := P;
      end;
      System.Move(Buffer, (PByte(FItems) + FPos)^, Count);
      FPos := P;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

function TArrayStream<T>.Write(const Buffer: TBytes; Offset, Count: Integer): Longint;
begin
  Result := Write(Buffer[Offset], Count);
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
  result := TRangeEnumerable.Create(Src);
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

class function TDateTimeUtils.IsCorrectDate(const t: TDateTime): boolean;
begin
  result :=
    (Trunc(t)<>0) and  {       0 (30.12.1899) as empty value }
    (YearOf(t)>0);     { -700000 (00.00.0000) as empty value + Delphi supports only "01.01.01" and later }
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
begin
  result := IsValid(S);
  if result then
    Dst := StringToGuid(S);
end;

class function TGUIDUtils.StrToGuid(const S: string): TGUID;
begin
  result := StringToGuid(S);
end;

class function TGUIDUtils.StrToGuidDef(const S: string): TGUID;
begin
  result := StrToGuidDef(S, NullGuid);
end;

class function TGUIDUtils.StrToGuidDef(const S: string; const Def: TGUID): TGUID;
begin
  if IsValid(S)
    then result := StringToGuid(S)
    else result := Def;
end;

{ TStreamUtils.TReader }

constructor TStreamUtils.TReader.Create(Src: TStream; AOwnsStream: Boolean; BufSize: integer; FromBeginning: boolean = True);
begin
  Stream := Src;
  if AOwnsStream then
    AutoFreeCollection.Add(Stream);
  SetLength(Bytes, BufSize);
  if not FromBeginning then
    BytesToRead := Stream.Size - Stream.Position
  else
  begin
    Stream.Position := 0;
    BytesToRead := Stream.Size;
  end;
end;

function TStreamUtils.TReader.ReadNext: Boolean;
begin
  Result := BytesToRead > 0;
  if Result then
  begin
    Count := Min(BytesToRead, Length(Bytes));
    Stream.ReadBuffer(Bytes,  Count);
    Dec(BytesToRead, Count);
  end
  else
  begin
    SetLength(Bytes, 0);
    Count := 0;
  end;
end;

{ TStreamUtils }

class function TStreamUtils.Copy(Src, Dst: TStream; Count,BufSize: integer; ProgressProc: TCopyStreamProgressProc): int64;
var
  N: Integer;
  Buffer: TBytes;
  LastOnProgressTime: TDateTime;
  Cancel: Boolean;
begin
  { check TVCLStreamUtils.Copy if you don't want UI to freeze until operation is complete }
  { based on TStream.CopyFrom, but provides time-based callback ProgressProc }
  Result := 0;
  LastOnProgressTime := 0;
  if Count <= 0 then
  begin
    Src.Position := 0;
    Count := Src.Size;
  end;
  if BufSize=0 then
    BufSize := 65536
  else
    BufSize := Max(BufSize, 4096);
  SetLength(Buffer, BufSize);
  Cancel := False;
  if Assigned(ProgressProc) then
  begin
    ProgressProc(0, Cancel);
    LastOnProgressTime := Now;
  end;
  while (Count <> 0) and not Cancel do
  begin
    N := Min(Count, BufSize);
    Src.ReadBuffer(Buffer, N);
    Dst.WriteBuffer(Buffer, N);
    Dec(Count, N);
    Inc(Result, N);
    if Assigned(ProgressProc) and (MilliSecondsBetween(Now, LastOnProgressTime)>=50) then
    begin
      ProgressProc(Result, Cancel);
      LastOnProgressTime := Now;
    end;
  end;
  if Assigned(ProgressProc) then
    ProgressProc(Result, Cancel);
end;

class function TStreamUtils.Copy(Src, Dst: TStream; ProgressProc: TCopyStreamProgressProc): int64;
begin
  { check TVCLStreamUtils.Copy if you don't want UI to freeze until operation is complete }
  result := Copy(Src, Dst, 0,0,ProgressProc);
end;

class function TStreamUtils.Copy(Src, Dst: TStream): int64;
begin
  { check TVCLStreamUtils.Copy if you don't want UI to freeze until operation is complete }
  result := Copy(Src, Dst, 0,0,nil);
end;

class procedure TStreamUtils.StringToStream(const S: string; Dst: TStream; Encoding: TEncoding);
var
  B: TArray<Byte>;
begin
  if Encoding=nil then
    Encoding := TEncoding.UTF8;
  B := Encoding.GetBytes(S);
  if Length(B) > 0 then
    Dst.WriteBuffer(B[0], Length(B));
end;

{ TFileUtils }

class procedure TFileUtils.CleanUpOldFiles(const AFileNamePattern: string; const AMaxAllowedTotalSize, AMaxAllowedCount: int64;
  AChanceToRun: Double);
type
  TFileInfo = record
    Name: string;
    Size: int64;
    Age: TDateTime;
  end;
var
  List: TList<TFileInfo>;
  Comparer: IComparer<TFileInfo>;
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
    Comparer := TDelegatedComparer<TFileInfo>.Create(
      function(const A,B: TFileInfo): Integer
      begin
        result := Sign(A.Age-B.Age);
      end);
    List.Sort(Comparer);
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
    Sys.FreeAndNil(List);
  end;
end;

class function TFileUtils.CopyFile(const SrcFileName, DstFileName: string; out ErrorMessage: string; ProgressProc: TCopyFileProgressProc): boolean;
var
  AutoFreeCollection: TAutoFreeCollection;
  CI: TCopyFileInfo;
  Src,Dst: TStream;
begin
  { check TVCLFileUtils.CopyFile if you don''t want UI to freeze until operation is complete }
  try
    Result := True;
    Src := AutoFreeCollection.Add(TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite));
    Dst := AutoFreeCollection.Add(TFileStream.Create(DstFileName, fmCreate));
    if not Assigned(ProgressProc) then
      TStreamUtils.Copy(Src, Dst)
    else
    begin
      CI.FileSize    := Src.Size;
      CI.SrcFileName := SrcFileName;
      CI.DstFileName := DstFileName;
      TStreamUtils.Copy(Src, Dst,
        procedure(const Transferred: int64; var Cancel: boolean)
        begin
          CI.Copied := Transferred;
          ProgressProc(CI, Cancel);
        end);
    end;
  except
    on e: exception do
    begin
      Result := True;
      ErrorMessage := Format('Can''t copy file "%s" to "%s": %s', [SrcFileName, DstFileName, SysErrorMessage(GetLastError)]);
    end;
  end;
end;

class function TFileUtils.CopyFile(const SrcFileName, DstFileName: string; out ErrorMessage: string): boolean;
begin
  { check TVCLFileUtils.CopyFile if you don''t want UI to freeze until operation is complete }
  result := CopyFile(SrcFileName, DstFileName, ErrorMessage, nil);
end;

class function TFileUtils.DeleteFile(const AFileName: string; out AErrorMessage: string): boolean;
begin
  result := System.SysUtils.DeleteFile(AFileName);
  if not result then
    AErrorMessage := Format('Can''t delete file "%s" (%s)', [AFileName, SysErrorMessage(GetLastError)]);
end;

class function TFileUtils.RenameFile(const ASrcFileName, ADstFileName: string; out AErrorMessage: string): boolean;
begin
  result := System.SysUtils.RenameFile(ASrcFileName, ADstFileName);
  if not result then
    AErrorMessage := Format('Can''t rename file "%s" to "%s" (%s)', [ASrcFileName, ADstFileName, SysErrorMessage(GetLastError)]);
end;

class procedure TFileUtils.EnumFiles(const AFileNamePattern: string; AOnfile: TDelegatedOnFile);
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

class function TFileUtils.FileModeToString(AMode: Integer): string;
const
  OpenMode : array[0..3] of string = ('fmOpenRead', 'fmOpenWrite', 'fmOpenReadWrite', 'unknown_open_mode');
begin
  {$WARN SYMBOL_PLATFORM OFF}
  result := '{' + OpenMode[AMode and 3];
  if AMode and fmShareExclusive = fmShareExclusive then result := result + ', fmShareExclusive';
  if AMode and fmShareDenyWrite = fmShareDenyWrite then result := result + ', fmShareDenyWrite';
  if AMode and fmShareDenyRead  = fmShareDenyRead  then result := result + ', fmShareDenyRead';
  if AMode and fmShareDenyNone  = fmShareDenyNone  then result := result + ', fmShareDenyNone';
  result := result + '}';
  {$WARN SYMBOL_PLATFORM ON}
end;

class function TFileUtils.AccessAllowed(const AFileName: string; ADesiredAccess: word): boolean;
begin
  try
    TFileStream.Create(AFileName, ADesiredAccess).Free;
    result := True;
  except
    result := False;
  end;
end;

class procedure TFileUtils.Load<T>(const FileName: string; var Dst: TArray<T>);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Dst, Stream.Size div SizeOf(T));
    if Length(Dst)>0 then
      Stream.ReadBuffer(Dst[0], Length(Dst)*SizeOf(T));
  finally
    Stream.Free;
  end;
end;

class procedure TFileUtils.Save<T>(const FileName: string; const Src: TArray<T>; Index,Count: integer);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    Stream.WriteBuffer(Src[Index], Count*SizeOf(T));
  finally
    Stream.Free;
  end;
end;

class procedure TFileUtils.Save<T>(const FileName: string; const Src: TArray<T>);
begin
  Save<T>(FileName, Src, 0, Length(Src));
end;

class function TFileUtils.StringToFilename(const AStr: string): string;
const
  DisabledNames: array[0..21] of string = (
    'CON', 'PRN', 'AUX', 'NUL',
    'COM1','COM2','COM3','COM4','COM5','COM6','COM7','COM8','COM9',
    'LPT1','LPT2','LPT3','LPT4','LPT5','LPT6','LPT7','LPT8','LPT9'
  );
  DisabledChars = [#0..#31, '<', '>', ':', '"', '/', '\', '|', '?', '*'];
var
  i: Integer;
  c: Char;
  Path,Name: string;
begin

  { Trailing spaces.
    Windows allows filenames with spaces: " 1.txt", " 1 .txt", " 1 . txt" and even " .txt"
    but does not allow "1.txt ", so only trailing spaces are not allowed
    http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247%28v=vs.85%29.aspx
    "Do not end a file or directory name with a space or a period." }
  Path := ExtractFilePath(AStr);
  Name := TrimRight(ExtractFileName(AStr));

  { Disabled chars: http://stackoverflow.com/questions/960772/how-can-i-sanitize-a-string-for-use-as-a-filename }
  result := '';
  for i := Low(Name) to High(Name) do
  begin
    c := Name[i];
    if (c<#127) and (AnsiChar(Byte(c)) in DisabledChars) then
      result := result + '%' + THex.Encode(c, SizeOf(c))
    else
      result := result + c;
  end;

  { Disabled names }
  for i := Low(DisabledNames) to High(DisabledNames) do
    if AnsiSameText(result, DisabledNames[i]) then
    begin
      result := result + '_';
      Break;
    end;

  result := Path + Name;
end;

procedure FindOpenFiles(
  const AFilesMask  : string;
        AExceptions : TSet<string>;
        ARecursive  : boolean;
        ADst        : TList<string>);
var
  Files,Subdirs: TStringDynArray;
  Dir,FileName,Subdir: string;
begin
  try
    Dir := ExtractFilePath(AFilesMask);
    if not TDirectory.Exists(Dir) then
      Exit;
    Files := TDirectory.GetFiles(Dir, ExtractFileName(AFilesMask));
    for FileName in Files do
      if not (FileName in AExceptions) and not TFileUtils.AccessAllowed(FileName) then
        ADst.Add(FileName);
    if ARecursive then
    begin
      Subdirs := TDirectory.GetDirectories(Dir);
      for Subdir in Subdirs do
        FindOpenFiles(Subdir + '\' + ExtractFileName(AFilesMask), AExceptions, True, ADst);
    end;
  except
  end;
end;

class function TFileUtils.GetOpenFiles(const AMasks,Exceptions: array of string; Recursive: boolean): TArray<string>;
var
  FilesMask: string;
  Exc: TSet<string>;
  FoundFiles: TList<string>;
begin
  FoundFiles := TList<string>.Create;
  try
    Exc.Add(Exceptions);
    for FilesMask in AMasks do
      FindOpenFiles(FilesMask, Exc, Recursive, FoundFiles);
    result := FoundFiles.ToArray;
  finally
    Sys.FreeAndNil(FoundFiles);
  end;
end;

class function TFileUtils.GetOpenFiles(const AMasks, Exceptions: array of string; Recursive: boolean; Delim: string): string;
var
  FoundFiles: TArray<string>;
  i: Integer;
begin
  FoundFiles := GetOpenFiles(AMasks, Exceptions, Recursive);
  if Length(FoundFiles)=0 then result := '' else result := FoundFiles[0];
  for i := 1 to High(FoundFiles) do
    result := result + Delim + FoundFiles[i];
end;

class function TFileUtils.GetSize(const AFileName: string): int64;
var
  F: TSearchRec;
begin
  if FindFirst(AFileName, faAnyfile, F)<>0 then
    result := 0
  else
  begin
    result := F.Size;
    FindClose(F);
  end;
end;

class function TFileUtils.RemoveInvalidChars(const AFileName: string): string;
var
  I: Integer;
  vPath, vFile: string;
begin
  Result := '';
  vPath := ExtractFilePath(AFileName);
  vFile := ExtractFileName(AFileName);

  for I := Low(vPath) to High(vPath) do
    if TPath.IsValidPathChar(vPath[I]) then
      Result := Result + vPath[I];
  Result := IncludeTrailingPathDelimiter(Result);

  for I := Low(vFile) to High(vFile) do
    if TPath.IsValidFileNameChar(vFile[I]) then
      Result := Result + vFile[I];
end;

class procedure TFileUtils.ExistsBuildCache(const CacheFolder: string; var Cache: TSet<string>; Recursive: boolean = True);
var
  SearchOption: TSearchOption;
begin
  Cache.Clear;
  if Recursive then SearchOption := TSearchOption.soAllDirectories
    else SearchOption := TSearchOption.soTopDirectoryOnly;
  if TDirectory.Exists(CacheFolder) then
    Cache.Add(TDirectory.GetFiles(IncludeTrailingPathDelimiter(TrimRight(CacheFolder)), '*.*', SearchOption));
end;

class function TFileUtils.Exists(const FullFileName,CacheFolder: string; var Cache: TSet<string>): boolean;
begin
  if not AnsiLowerCase(FullFileName).StartsWith(AnsiLowerCase(IncludeTrailingPathDelimiter(TrimRight(CacheFolder)))) then
    result := FileExists(FullFileName)
  else
  begin
    { We check files when they should exist, non existing file is very rare case.
      To make check robust/reliable we use two different aproaches in different time:
      - We check file in preloaded list of files. It is very fast, if file is here, then check is done.
      - If file is not in the list, then we check with FixeExists function.
      We report non existing file after double check only. }
    result := FullFileName in Cache;
    if not result then
      result := FileExists(FullFileName);
  end;
end;

{ TIfThen }

class function TIfThen.Get<T>(ACondition: Boolean; AValueTrue, AValueFalse: T): T;
begin
  if ACondition then result := AValueTrue else result := AValueFalse;
end;

{ TIndexBackEnumerator }

constructor TIndexBackEnumerator.Create(AIndexFrom, AIndexTo: integer);
begin
  FCurrentIndex := AIndexFrom;
  FToIndex := AIndexTo;
end;

function TIndexBackEnumerator.MoveNext: Boolean;
begin
  result := FCurrentIndex>=FToIndex;
  if result then
    dec(FCurrentIndex);
end;

function TIndexBackEnumerator.GetCurrent: Integer;
begin
  result := FCurrentIndex+1;
end;

{ TTiming.TTotalStat }

constructor TTiming.TTotalStat.Create(const ASpan: TTimeSpan; const ACalls: int64);
begin
  Span := ASpan;
  Calls := ACalls;
end;

{ TTiming }

class destructor TTiming.DestroyClass;
begin
  Sys.FreeAndNil(FTimeStack);
  Sys.FreeAndNil(FTotalTimes);
end;

class function TTiming.GetTimeStack: TStack<TStopwatch>;
begin
  if FTimeStack=nil then
    FTimeStack := TStack<TStopwatch>.Create;
  result := FTimeStack;
end;

class function TTiming.GetTotalTimes: TDictionary<string, TTotalStat>;
begin
  if FTotalTimes=nil then
    FTotalTimes := TDictionary<string, TTotalStat>.Create(TOrdinalIStringComparer.Ordinal);
  result := FTotalTimes;
end;

class procedure TTiming.Start;
begin
  TimeStack.Push(TStopwatch.StartNew);
end;

class function TTiming.Stop: TTimeSpan;
begin
  Result := TimeStack.Pop.Elapsed;
end;

class function TTiming.Stop(const OpId: string; out ATotalStat: TTotalStat): TTimeSpan;
begin
  result := Stop;
  if not TotalTimes.TryGetValue(OpId, ATotalStat) then
    ATotalStat := TTotalStat.Create(TTimeSpan.Create(0), 0);
  ATotalStat.Span := ATotalStat.Span + result;
  inc(ATotalStat.Calls);
  TotalTimes.AddOrSetValue(OpId, ATotalStat);
end;

class function TTiming.StopAndGetCaption(const OpId, Msg: string): string;
var
  OpTime: TTimeSpan;
  TotalStat: TTotalStat;
begin
  OpTime := Stop(OpId, TotalStat);
  result := Format('%s (%s): %.2f sec (total: %.2f sec; calls: %d)', [
    OpId, Msg, OpTime.TotalSeconds, TotalStat.Span.TotalSeconds, TotalStat.Calls]);
end;

class function TTiming.StopAndGetCaption(const OpId: string): string;
var
  OpTime: TTimeSpan;
  TotalStat: TTotalStat;
begin
  OpTime := Stop(OpId, TotalStat);
  result := Format('%s: %.2f sec (total: %.2f sec; calls: %d)', [
    OpId, OpTime.TotalSeconds, TotalStat.Span.TotalSeconds, TotalStat.Calls]);
end;

{ TTimeOut }

procedure TTimeOut.Start(AMaxTimeForOp: TDateTime; ACheckPeriod: integer = 0);
begin
  FMaxTimeForOp:= AMaxTimeForOp;
  FCheckPeriod := ACheckPeriod;
  Restart;
end;

procedure TTimeOut.StartSec(AMaxTimeForOpSec: double; ACheckPeriod: integer = 0);
begin
  Start(AMaxTimeForOpSec/SecsPerDay, ACheckPeriod);
end;

procedure TTimeOut.StartInfinite;
begin
  Start(-1); { negative MaxTimeForOp means that .TimedOut is constantly False }
end;

procedure TTimeOut.Restart;
begin
  FStartTime   := Now;
  FCounter     := FCheckPeriod;
  FOpTimedOut  := FMaxTimeForOp = 0;
end;

function TTimeOut.GetTimedOut: Boolean;
begin
  { negative value = infinite (never timed out) }
  if FMaxTimeForOp < 0 then Result := False else
  { already timed out }
  if FOpTimedOut then Result := True else
  { check for timeout }
  begin
    Dec(FCounter);
    if FCounter > 0 then Result := False else
    begin
      FCounter := FCheckPeriod;
      Result := Now-FStartTime>FMaxTimeForOp;
      FOpTimedOut := Result;
    end;
  end;
end;

{ TCurrencyUtils }

class function TCurrencyUtils.ToString(const Value: currency; FractionalPart: boolean): string;
begin
  result := FormatCurr( IfThen(FractionalPart, '#,##', '#,'), Value);
end;

{ TPIWriter }

constructor TPIWriter.Create(ADst: TStream);
begin
  inherited Create;
  Writer := TWriter.Create(ADst, 4096);
end;

destructor TPIWriter.Destroy;
begin
  Sys.FreeAndNil(Writer);
  inherited;
end;

procedure TPIWriter.Write(const Value: Int16);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: UInt16);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: Int32);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: Char);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: Int8);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: UInt8);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: Extended);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: Double);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: Int64);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: UInt32);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: Single);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Value: UInt64);
begin
  Writer.WriteVar(Value, SizeOf(Value));
end;

procedure TPIWriter.Write(const Buf; Count: integer);
begin
  Writer.Write(Buf, Count);
end;

procedure TPIWriter.Write(const Value: TBytes);
var
  i: Integer;
begin
  i := Length(Value);
  Write(i);
  if i>0 then
    Write(Value[0], i);
end;

procedure TPIWriter.Write(const Value: string);
begin
  Write(TEncoding.UTF8.GetBytes(Value));
end;

{ TPIReader }

constructor TPIReader.Create(ASrc: TStream);
begin
  inherited Create;
  Reader := TReader.Create(ASrc, 4096);
end;

destructor TPIReader.Destroy;
begin
  Sys.FreeAndNil(Reader);
  inherited;
end;

procedure TPIReader.Read(out Value: Int16);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: UInt16);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: Int32);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: Char);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: Int8);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: UInt8);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: Single);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: Double);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: Extended);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: UInt32);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: Int64);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(out Value: UInt64);
begin
  Reader.ReadVar(Value, SizeOf(Value));
end;

procedure TPIReader.Read(var Buf; Count: integer);
begin
  Reader.Read(Buf, Count);
end;

procedure TPIReader.Read(out Value: TBytes);
var
  i: integer;
begin
  Read(i);
  SetLength(Value, i);
  if i>0 then
    Read(Value[0], i);
end;

procedure TPIReader.Read(out Value: string);
var
  Bytes: TBytes;
begin
  Read(Bytes);
  Value := TEncoding.UTF8.GetString(Bytes);
end;

{ TBox<T> }

class function TBox<T>.Create: TBox<T>;
begin
  Result.Clear;
end;

class function TBox<T>.Create(const AValue: T): TBox<T>;
begin
  Result.Clear;
  result.Value := AValue;
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

class operator TBox<T>.NotEqual(const Left, Right: TBox<T>): Boolean;
begin
  result := not (Left=Right);
end;

class operator TBox<T>.Implicit(const AValue: T): TBox<T>;
begin
  result := TBox<T>.Create(AValue);
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

  { 0 1 [2 3 4] [5 6] (7)
           N=3    M=2 CurPos }
  procedure COPY_N;
  begin
    assert((N>=1) and (N<=128));
    inc(result,N+1);
    Dst^ := N-1; { copy N (1..128) bytes as is }
    inc(Dst);
    system.move((Src-M-N)^, Dst^, N);
    inc(Dst, N);
  end;

  { M=0! }
  procedure COPY_128;
  begin
    assert(N>=128);
    inc(result,129);
    Dst^ := 127; { copy N (1..128) bytes as is }
    inc(Dst);
    system.move((Src-N)^, Dst^, 128);
    inc(Dst, 128);
    dec(N, 128);
  end;

  { 257-x=M -> x=257-M }
  procedure PACK_M;
  begin
    assert((M>=2) and (M<=128));
    inc(result,2);
    dst^ := 257-M; { copy folowing byte 257-x times }
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

{ TCustomHash }

class function TCustomHash.Encode(const Buf; ByteBufSize: integer): TBytes;
begin
  result := DoEncode(Buf, ByteBufSize);
end;

class function TCustomHash.Encode(S: TStream): TBytes;
begin
  result := DoEncode(S);
end;

class function TCustomHash.Encode(const S: TBytes; StartIndex,Count: integer): TBytes;
begin
  Assert((StartIndex >= 0) and (StartIndex + Count <= Length(S)));
  if Count <= 0 then
    result := DoEncode(nil^, 0)
  else
    result := DoEncode(S[StartIndex], Count);
end;

class function TCustomHash.Encode(const S: TBytes): TBytes;
begin
  result := Encode(S, 0, Length(S));
end;

class function TCustomHash.Encode(const S: string): TBytes;
begin
  if Length(S)=0 then
    result := DoEncode(nil^, 0)
  else
    result := DoEncode(S[Low(S)], length(S)*SizeOf(S[Low(S)]));
end;

class function TCustomHash.EncodeAnsiString(const S: AnsiString): TBytes;
begin
  if Length(S)=0 then
    result := DoEncode(nil^, 0)
  else
    result := DoEncode(S[Low(S)], length(S)*SizeOf(S[Low(S)]));
end;

class function TCustomHash.EncodeFile(const AFileName: string): TBytes;
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := DoEncode(S);
  finally
    S.Free;
  end;
end;

class function TCustomHash.Hash32ToBytes(Hash: Cardinal): TBytes;
begin
  SetLength(Result, 4);
  PCardinal(@Result[0])^ := System.Hash.THash.ToBigEndian(Hash);
end;

class procedure TCustomHash.Init(out Hash: TArray<byte>);
begin
  DoInit(Hash);
end;

class procedure TCustomHash.Update(const Value; ValueByteSize: integer; var Hash: THashData);
begin
  DoUpdate(Value, ValueByteSize, Hash);
end;

class procedure TCustomHash.Update(const Value: integer; var Hash: THashData);
begin
  DoUpdate(Value, SizeOf(Value), Hash);
end;

class procedure TCustomHash.Update(const Value: double; var Hash: THashData);
begin
  DoUpdate(Value, SizeOf(Value), Hash);
end;

class procedure TCustomHash.Update(const Value: TArray<byte>; var Hash: THashData);
begin
  { Length must be included in hash, otherwise arrays
    [a, ab] and [aa, b] will produce same hash }
  Update(Length(Value), Hash);
  if Length(Value) > 0 then
    DoUpdate(Value[0], Length(Value), Hash);
end;

class procedure TCustomHash.Update(const Value: string; var Hash: THashData);
begin
  { Length must be included in hash, otherwise arrays
    ["a", "ab"] and ["aa", "b"] will produce same hash }
  Update(Length(Value), Hash);
  if Value <> '' then
    DoUpdate(Value[Low(Value)], Length(Value)*SizeOf(Char), Hash);
end;

class function TCustomHash.Done(var Hash: TArray<byte>): TBytes;
begin
  result := DoDone(Hash);
end;

{ TMD5 }

class function THashUtils.MD5.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Hash: THashMD5;
begin
  Hash := THashMD5.Create; { Create may have params and is not equal to Reset }
  Hash.Update(Buf, ByteBufSize);
  Result := Hash.HashAsBytes;
end;

class function THashUtils.MD5.DoEncode(S: TStream): TBytes;
var
  Reader: TStreamUtils.TReader;
  Hash: THashMD5;
begin
  Reader := TStreamUtils.TReader.Create(S, False, StreamingBufSize, True);
  Hash := THashMD5.Create; { Create may have params and is not equal to Reset }
  while Reader.ReadNext do
    Hash.Update(Reader.Bytes, Reader.Count);
  Result := Hash.HashAsBytes;
end;

class procedure THashUtils.MD5.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashMD5));
  THashMD5((@Hash[0])^) := THashMD5.Create;
end;

class procedure THashUtils.MD5.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashMD5));
  THashMD5((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.MD5.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashMD5));
  result := THashMD5((@Hash[0])^).HashAsBytes;
  THashMD5((@Hash[0])^) := Default(THashMD5);
  SetLength(Hash, 0);
end;

{ THashUtils.SHA1 }

class function THashUtils.SHA1.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Hash: THashSHA1;
begin
  Hash := THashSHA1.Create;
  Hash.Update(Buf, ByteBufSize);
  Result := Hash.HashAsBytes;
end;

class function THashUtils.SHA1.DoEncode(S: TStream): TBytes;
var
  Reader: TStreamUtils.TReader;
  Hash: THashSHA1;
begin
  Reader := TStreamUtils.TReader.Create(S, False, StreamingBufSize, True);
  Hash := THashSHA1.Create;
  while Reader.ReadNext do
    Hash.Update(Reader.Bytes, Reader.Count);
  Result := Hash.HashAsBytes;
end;

class procedure THashUtils.SHA1.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashSHA1));
  THashSHA1((@Hash[0])^) := THashSHA1.Create;
end;

class procedure THashUtils.SHA1.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashSHA1));
  THashSHA1((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.SHA1.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashSHA1));
  result := THashSHA1((@Hash[0])^).HashAsBytes;
  THashSHA1((@Hash[0])^) := Default(THashSHA1);
  SetLength(Hash, 0);
end;

{ THashUtils.SHA2 }

class function THashUtils.SHA2.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Hash: THashSHA2;
begin
  Hash := THashSHA2.Create;
  Hash.Update(Buf, ByteBufSize);
  Result := Hash.HashAsBytes;
end;

class function THashUtils.SHA2.DoEncode(S: TStream): TBytes;
var
  Reader: TStreamUtils.TReader;
  Hash: THashSHA2;
begin
  Reader := TStreamUtils.TReader.Create(S, False, StreamingBufSize, True);
  Hash := THashSHA2.Create;
  while Reader.ReadNext do
    Hash.Update(Reader.Bytes, Reader.Count);
  Result := Hash.HashAsBytes;
end;

class procedure THashUtils.SHA2.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashSHA2));
  THashSHA2((@Hash[0])^) := THashSHA2.Create;
end;

class procedure THashUtils.SHA2.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashSHA2));
  THashSHA2((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.SHA2.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashSHA2));
  result := THashSHA2((@Hash[0])^).HashAsBytes;
  THashSHA2((@Hash[0])^) := Default(THashSHA2);
  SetLength(Hash, 0);
end;

{ THashUtils.CRC32 }

class function THashUtils.CRC32.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Crc: Cardinal;
begin
  Crc := System.ZLib.crc32(0, nil, 0);
  Crc := System.ZLib.crc32(Crc, @Buf, ByteBufSize);
  Result := Hash32ToBytes(Crc);
end;

class function THashUtils.CRC32.DoEncode(S: TStream): TBytes;
var
  Reader: TStreamUtils.TReader;
  Crc: Cardinal;
begin
  Reader := TStreamUtils.TReader.Create(S, False, StreamingBufSize, True);
  Crc := System.ZLib.crc32(0, nil, 0);
  while Reader.ReadNext do
    Crc := System.ZLib.crc32(Crc, @Reader.Bytes[0], Reader.Count);
  Result := Hash32ToBytes(Crc);
end;

class procedure THashUtils.CRC32.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.crc32(0, nil, 0);
end;

class procedure THashUtils.CRC32.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.crc32(cardinal((@Hash[0])^), @Buf, ByteBufSize);
end;

class function THashUtils.CRC32.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  Result := Hash32ToBytes(cardinal((@Hash[0])^));
  cardinal((@Hash[0])^) := Default(cardinal);
  SetLength(Hash, 0);
end;

{ THashUtils.Adler32 }

class function THashUtils.Adler32.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Crc: Cardinal;
begin
  Crc := System.ZLib.adler32(0, nil, 0);
  Crc := System.ZLib.adler32(Crc, @Buf, ByteBufSize);
  Result := Hash32ToBytes(Crc);
end;

class function THashUtils.Adler32.DoEncode(S: TStream): TBytes;
var
  Reader: TStreamUtils.TReader;
  Crc: Cardinal;
begin
  Reader := TStreamUtils.TReader.Create(S, False, StreamingBufSize, True);
  Crc := System.ZLib.adler32(0, nil, 0);
  while Reader.ReadNext do
    Crc := System.ZLib.adler32(Crc, @Reader.Bytes[0], Reader.Count);
  Result := Hash32ToBytes(Crc);
end;

class procedure THashUtils.Adler32.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.adler32(0, nil, 0);
end;

class procedure THashUtils.Adler32.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.adler32(cardinal((@Hash[0])^), @Buf, ByteBufSize);
end;

class function THashUtils.Adler32.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  Result := Hash32ToBytes(cardinal((@Hash[0])^));
  cardinal((@Hash[0])^) := Default(cardinal);
  SetLength(Hash, 0);
end;

{ THashUtils.BobJenkins32 }

class function THashUtils.BobJenkins32.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  h: THashBobJenkins;
begin
  h := THashBobJenkins.Create;
  h.Update(Buf, ByteBufSize);
  Result := h.HashAsBytes;
end;

class function THashUtils.BobJenkins32.DoEncode(S: TStream): TBytes;
var
  Reader: TStreamUtils.TReader;
  Hash: THashBobJenkins;
begin
  Reader := TStreamUtils.TReader.Create(S, False, StreamingBufSize, True);
  Hash := THashBobJenkins.Create;
  while Reader.ReadNext do
    Hash.Update(Reader.Bytes, Reader.Count);
  Result := Hash.HashAsBytes;
end;

class procedure THashUtils.BobJenkins32.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashBobJenkins));
  THashBobJenkins((@Hash[0])^) := THashBobJenkins.Create;
end;

class procedure THashUtils.BobJenkins32.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashBobJenkins));
  THashBobJenkins((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.BobJenkins32.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashBobJenkins));
  Result := THashBobJenkins((@Hash[0])^).HashAsBytes;
  THashBobJenkins((@Hash[0])^) := Default(THashBobJenkins);
  SetLength(Hash, 0);
end;

{ THashUtils }

class function THashUtils.Mix(const HashA, HashB, HashC: integer): integer;
begin
  result := Mix(Mix(HashA, HashB), HashC);
end;

class function THashUtils.Mix(const HashA, HashB: integer): integer;
begin
  result := (HashA*1103515245 + 12345) xor HashB;
end;

class function THashUtils.Mix(const HashA, HashB: TBytes): TBytes;
var
  V,L1,L2,LR: Integer;
  Src1,Src2,Dst: pointer;
begin
  L1 := Length(HashA);
  L2 := Length(HashB);
  LR := Max(L1, L2);
  SetLength(result, LR);
  if LR = 0 then
    Exit;
  Dst := @result[0];
  if L1>0 then Src1 := @HashA[0] else Src1 := nil;
  if L2>0 then Src2 := @HashB[0] else Src2 := nil;
  V := 0;

  while LR >= SizeOf(integer) do
  begin

    { read from HashA }
    if L1 >= SizeOf(integer) then
    begin
      V := (V*1103515245 + 12345) xor integer(Src1^);
      inc(PByte(Src1), SizeOf(integer));
      dec(L1, SizeOf(integer));
    end
    else
      while L1 > 0 do
      begin
        V := (V*1103515245 + 12345) xor PByte(Src1)^;
        inc(PByte(Src1));
        dec(L1);
      end;

    { read from HashB }
    if L2 >= SizeOf(integer) then
    begin
      V := (V*1103515245 + 12345) xor integer(Src2^);
      inc(PByte(Src2), SizeOf(integer));
      dec(L2, SizeOf(integer));
    end
    else
      while L2 > 0 do
      begin
        V := (V*1103515245 + 12345) xor PByte(Src2)^;
        inc(PByte(Src2));
        dec(L2);
      end;

    integer(Dst^) := V;
    dec(LR, SizeOf(integer));
  end;

  while LR > 0 do
  begin
    if L1 > 0 then
    begin
      V := (V*1103515245 + 12345) xor byte(Src1^);
      inc(PByte(Src1));
      dec(L1);
    end;
    if L2 > 0 then
    begin
      V := (V*1103515245 + 12345) xor byte(Src2^);
      inc(PByte(Src2));
      dec(L2);
    end;
    Byte(Dst^) := V;
    dec(LR);
  end;
end;

class function THashUtils.HashToString(const AHash: TBytes): string;
begin
  Result := THex.Encode(AHash);
end;

class function THashUtils.GetHash32(const Hash: TBytes): integer;
begin
  if Length(Hash) = 0 then
    result := 0
  else
  if Length(Hash) = 4 then
    result :=
      (integer(Hash[0]) shl 24) or
      (integer(Hash[1]) shl 16) or
      (integer(Hash[2]) shl  8) or
      (integer(Hash[3]))
  else
    result := GetHash32(CRC32.Encode(Hash,0,Length(Hash)));
end;

class function THashUtils.GetHash24(const Hash: TBytes): integer;
begin
  if Length(Hash) = 0 then
    result := 0
  else
  if Length(Hash) = 4 then
    result :=
      (integer(Hash[0]) shl 16) or
      (integer(Hash[1]) shl  8) or
      (integer(Hash[2]) xor integer(Hash[3]))
  else
    result := GetHash24(CRC32.Encode(Hash,0,Length(Hash)));
end;

class function THashUtils.GetHash16(const Hash: TBytes): integer;
begin
  if Length(Hash) = 0 then
    result := 0
  else
  if Length(Hash) = 4 then
    result :=
      ((integer(Hash[0]) xor integer(Hash[1])) shl 8) or
      (integer(Hash[2]) xor integer(Hash[3]))
  else
    result := GetHash16(CRC32.Encode(Hash,0,Length(Hash)));
end;

class function THashUtils.Mix(const Hashes: array of integer): integer;
var
  I: Integer;
begin
  result := 0;
  for I := Low(Hashes) to High(Hashes) do
    result := Mix(result, Hashes[I]);
end;

class function THashUtils.Mix(const Hashes: array of TBytes): TBytes;
var
  I: Integer;
begin
  SetLength(result, 0);
  for I := Low(Hashes) to High(Hashes) do
    result := Mix(result, Hashes[I]);
end;

{ TBuffer }

procedure TBuffer.Clear;
begin
  Size := 0;
  Capacity := Size;
end;

function TBuffer.GetCapacity: integer;
begin
  result := Length(FData);
end;

function TBuffer.GetCurrentData: pointer;
begin
  result := @FData[Position];
end;

function TBuffer.GetEmpty: Boolean;
begin
  result := Size=0;
end;

function TBuffer.GetEOF: boolean;
begin
  result := Position >= Size;
end;

function TBuffer.GetLeft: integer;
begin
  result := Size-Position;
end;

procedure TBuffer.SetText(const Value: string);
begin
  Clear;
  Write(Value);
  Position := 0;
end;

function TBuffer.GetText: string;
var
  P: Integer;
begin
  P := Position;
  Position := 0;
  Read(Result, Size div SizeOf(Char));
  Position := P;
end;

procedure TBuffer.LoadFromFile(const FileName: string);
begin
  TFileUtils.Load<Byte>(FileName, FData);
  FSize := Length(FData);
  FPosition := 0;
end;

procedure TBuffer.SaveToFile(const FileName: string);
begin
  TFileUtils.Save<Byte>(FileName, FData, 0,Size);
end;

procedure TBuffer.SetLeft(Value: integer);
begin
  Size := Position + Value;
end;

procedure TBuffer.SetCapacity(Value: integer);
begin
  Assert(Value >= Size);
  SetLength(FData, Value);
end;

procedure TBuffer.CheckCapacity(MinCapacity: integer);
begin
  if Capacity < MinCapacity then
    Capacity := TFun.Max(MinCapacity, Capacity shl 1, 16);
end;

procedure TBuffer.SetSize(Value: integer);
begin
  CheckCapacity(Value);
  FSize := Value;
  FPosition := Max(Min(FPosition, FSize), 0);
end;

procedure TBuffer.TrimExcess;
begin
  Capacity := Size;
end;

procedure TBuffer.Read(var Dst; ByteCount: integer);
begin
  Assert(Position + ByteCount <= Size);
  System.Move(FData[Position], Dst, ByteCount);
  inc(FPosition, ByteCount);
end;

procedure TBuffer.Read(var Dst: string; DstCharOffset, CharCount: integer);
begin
  Read(Dst[DstCharOffset+Low(Dst)], CharCount*SizeOf(Char));
end;

procedure TBuffer.Read(var Dst: string; CharCount: integer);
begin
  SetLength(Dst, CharCount);
  Read(Dst[Low(Dst)], CharCount*SizeOf(Char));
end;

procedure TBuffer.Write(const Src; ByteCount: integer);
begin
  CheckCapacity(Position + ByteCount);
  System.Move(Src, FData[Position], ByteCount);
  inc(FPosition, ByteCount);
  if FPosition > FSize then
    FSize := FPosition;
end;

procedure TBuffer.Write(const Src: string; CharOffset,CharCount: integer);
begin
  if CharCount<>0 then
    Write(Src[CharOffset+Low(Src)], CharCount*SizeOf(Char));
end;

procedure TBuffer.Write(const Src: string);
begin
  if Src<>'' then
    Write(Src[Low(Src)], Length(Src)*SizeOf(Char));
end;

procedure TBuffer.Cut(Start, Len: integer);
begin
  Len := TArrayUtils.Cut<byte>(FData, Size, Start, Len);
  dec(FSize, Len);
  if Position > Size then
    Position := Size;
end;

function TBuffer.Slice(Start, Len: integer): TArray<byte>;
begin
  result := TArrayUtils.Slice<byte>(FData, Size, Start, Len);
end;

procedure TBuffer.SetData(AData: TArray<Byte>);
begin
  FData := AData;
  FSize := Length(FData);
  FPosition := 0;
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

constructor TOutOfScopeAction.Create(AProc: TProc);
begin
  FProc := TInterfacedObject<TObject>.Create(TOnDestroyRunner.Create(AProc));
end;

{ TOutOfScopeAction<T> }

constructor TOutOfScopeAction<T>.Create(AProc: TProc<T>; AValue: T);
begin
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

class procedure TFun.Clear<T>(var R: T);
begin
  R := Default(T);
end;

class procedure TFun.Exchange<T>(var A, B: T);
var C: T;
begin
  C := A;
  A := B;
  B := C;
end;

class procedure TFun.FreeAndNil<T>(var Obj: T);
begin
  Obj.Free;
  Obj := nil;
end;

class function TFun.GetPtr(const Values: TArray<byte>): pointer;
begin
  if Length(Values)=0
    then result := nil
    else result := @Values[0];
end;

class function TFun.IfThen<T>(ACondition: Boolean; AValueTrue, AValueFalse: T): T;
begin
  if ACondition then result := AValueTrue else result := AValueFalse;
end;

class function TFun.InRange(const AValue, AValueFrom, AValueTo: integer): boolean;
begin
  if AValueFrom <= AValueTo then
    result := (AValue >= AValueFrom) and (AValue <= AValueTo)
  else
    result := (AValue >= AValueTo) and (AValue <= AValueFrom);
end;

class function TFun.InRange(const AValue, AValueFrom, AValueTo: double): boolean;
begin
  if AValueFrom <= AValueTo then
    result := (AValue >= AValueFrom) and (AValue <= AValueTo)
  else
    result := (AValue >= AValueTo) and (AValue <= AValueFrom);
end;

class function TFun.ValueInRange<T>(AValue, AValueFrom, AValueTo: T): boolean;
var
  Comparer: IComparer<T>;
begin
  Comparer := TComparerUtils.DefaultComparer<T>;
  if Comparer.Compare(AValueFrom, AValueTo) <= 0 then
    Result := (Comparer.Compare(AValue, AValueFrom) >= 0) and (Comparer.Compare(AValue, AValueTo) <= 0)
  else
    Result := (Comparer.Compare(AValue, AValueTo) >= 0) and (Comparer.Compare(AValue, AValueFrom) <= 0);
end;

class function TFun.Max(const A, B, C: double): double;
begin
  Result := A;
  if B > Result then
    Result := B;
  if C > Result then
    Result := C;
end;

class function TFun.Max(const A, B: double): double;
begin
  if A >= B then
    result := A
  else
    result := B;
end;

class function TFun.Max(const A, B: integer): integer;
begin
  if A >= B then
    result := A
  else
    result := B;
end;

class function TFun.Min(const Values: array of integer): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function TFun.Min(const Values: TArray<integer>): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function TFun.Max(const Values: array of integer): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

class function TFun.Max(const Values: TArray<integer>): integer;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

class function TFun.Max(const A, B, C: integer): integer;
begin
  Result := A;
  if B > Result then
    Result := B;
  if C > Result then
    Result := C;
end;

class function TFun.Min(const A, B, C: double): double;
begin
  Result := A;
  if B < Result then
    Result := B;
  if C < Result then
    Result := C;
end;

class function TFun.Min(const A, B: double): double;
begin
  if A <= B then
    result := A
  else
    result := B;
end;

class function TFun.Min(const A, B: integer): integer;
begin
  if A <= B then
    result := A
  else
    result := B;
end;

class function TFun.Min(const A, B, C: integer): integer;
begin
  Result := A;
  if B < Result then
    Result := B;
  if C < Result then
    Result := C;
end;

class function TFun.Overlapped(const AFrom, ATo, BFrom, BTo: integer): boolean;
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

class function TFun.Overlapped(const AFrom, ATo, BFrom, BTo: double): boolean;
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

class function TFun.Min(const Values: array of double): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function TFun.Min(const Values: TArray<double>): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] < result then
      result := Values[I];
end;

class function TFun.Max(const Values: array of double): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

class function TFun.Max(const Values: TArray<double>): double;
var I: integer;
begin
  Assert(Length(Values)>0);
  Result := Values[Low(Values)];
  for I := Low(Values)+1 to High(Values) do
    if Values[I] > result then
      result := Values[I];
end;

{ TDataSize }

class operator TDataSize.Add(Left, Right: TDataSize): TDataSize;
begin
  result := Left.FSize+right.FSize;
end;

constructor TDataSize.Create(const AValue: int64);
begin
  FSize := AValue;
end;

constructor TDataSize.CreateMb(const AValue: double);
begin
  SizeMb := AValue;
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
begin
  result := TDataSize.Create(AValue).AsString;
end;

class operator TDataSize.Subtract(Left, Right: TDataSize): TDataSize;
begin
  result := Left.FSize-Right.FSize;
end;

{ TIntegralImageInt64 }

procedure TIntegralImageInt64.SetSize(AWidth, AHeight: integer);
begin
  Width := AWidth;
  Height := AHeight;
  SetLength(Image, 0);
  SetLength(Image, Width*Height);
end;

procedure TIntegralImageInt64.Build;
var
  x,y: Integer;
begin
  for x := 1 to Width-1 do
    Inc(Image[x],Image[x-1]);
  for y := 1 to Height-1 do
    Inc(Image[y*Width],Image[(y-1)*Width]);
  for y := 1 to Height-1 do
    BuildLine(@Image[y*Width+1]);
end;

(*
  #####
  #   #
  ####?
*)
procedure TIntegralImageInt64.BuildLine(ADst: PInt64Array);
var
  x: Integer;
begin
  for x := 0 to Width-2 do
    inc(ADst[x], ADst[x-1]+ADst[x-Width]-ADst[x-Width-1]);
end;

procedure TIntegralImageInt64.Clear;
begin
  SetSize(0,0);
end;

function TIntegralImageInt64.GetLine(y: integer): PInt64Array;
begin
  result := @Image[y*Width];
end;

function TIntegralImageInt64.GetSum(x1, y1, x2, y2: integer): int64;
begin
  Result := Image[x2+y2*Width];
  if x1>0 then
  begin
    if y1>0 then
      Inc(Result, Image[x1-1+(y1-1)*Width]);
    dec(Result, Image[x1-1+y2*Width]);
  end;
  if y1>0 then
    dec(Result, Image[x2+(y1-1)*Width]);
end;

function TIntegralImageInt64.GetAvg(x1, y1, x2, y2: integer): int64;
begin
  assert((x2>=x1) and (y2>=y1));
  if x2-x1+1>width then
    x2 := x1+width-1;
  if y2-y1+1>height then
    y2 := y1+height-1;
  if x1<0 then
  begin
    dec(x2, x1);
    x1 := 0;
  end;
  if y1<0 then
  begin
    dec(y2, y1);
    y1 := 0;
  end;
  if x2>=width then
  begin
    dec(x1,x2-width+1);
    x2 := width-1;
  end;
  if y2>=height then
  begin
    dec(y1,y2-height+1);
    y2 := height-1;
  end;
  result := Sum[x1,y1,x2,y2] div ((x2-x1+1)*(y2-y1+1));
end;

{ TInterpolation_Int64Custom }

constructor TInterpolation_Int64Custom.Create(PointCount: integer);
begin
  SetLength(FPoints, PointCount);
  FComparer := TDelegatedComparer<TPt>.Create(
    function (const A, B: TPt): integer
    begin
      if A.X < B.X then result := -1 else
        if A.X = B.X then result := 0 else
          result := 1;
    end);
end;

procedure TInterpolation_Int64Custom.BeginUpdate;
begin
  inc(FUpdateCnt);
  if FUpdateCnt=1 then
    DoBeginUpdate;
end;

procedure TInterpolation_Int64Custom.EndUpdate;
begin
  dec(FUpdateCnt);
  if FUpdateCnt=0 then
    DoEndUpdate;
end;

procedure TInterpolation_Int64Custom.DoBeginUpdate;
begin
end;

procedure TInterpolation_Int64Custom.DoEndUpdate;
var
  I: Integer;
begin

  { reorder if necessary }
  for I := 0 to High(FPoints)-1 do
    if FPoints[I].X > FPoints[I+1].X then
    begin
      TArray.Sort<TPt>(FPoints, FComparer);
      Break;
    end;

  { It is not allowed to have Xi=Xj for any i<>j,
    items are ordered, so we can check it eficiently. }
  for I := 0 to High(FPoints)-1 do
    if FPoints[I].X=FPoints[I+1].X then
      raise Exception.Create('Error');
end;

function TInterpolation_Int64Custom.GetPoint(i: integer): TPt;
begin
  result := FPoints[i];
end;

procedure TInterpolation_Int64Custom.SetPoint(i: integer; const Value: TPt);
begin
  Assert((FUpdateCnt>0) and (i>=0) and (i<=High(FPoints)));
  FPoints[i] := Value;
end;

function TInterpolation_Int64Custom.GetPointCount: integer;
begin
  result := Length(FPoints);
end;

{ TLinearInterpolation_Int64 }

function TLinearInterpolation_Int64.DoGetValue(const X: int64): int64;
var
  Item: TPt;
  FoundIndex: Integer;
begin
  Assert(Length(FPoints)>0);
  Item.X := X;
  Item.Y := 0;
  if TArray.BinarySearch<TPt>(FPoints, Item, FoundIndex, FComparer) then
    result := FPoints[FoundIndex].Y
  else
    { If not found, FoundIndex contains the index of the first entry larger than Item }
    if FoundIndex = 0 then result := FPoints[0].Y else
      if FoundIndex > High(FPoints) then result := FPoints[High(FPoints)].Y else
        { Xi<>Xj for any i<>j (check DoEndUpdate), so div by zero is not possible here. }
        result := FPoints[FoundIndex-1].Y +
          (X-FPoints[FoundIndex-1].X) *
          (FPoints[FoundIndex].Y-FPoints[FoundIndex-1].Y) div
          (FPoints[FoundIndex].X-FPoints[FoundIndex-1].X);
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

{ TSharedComponentPtrClass }

constructor TSharedComponentPtrClass<T>.Create(AComponent: T; AWeak: boolean = False);
begin
  Component := AComponent;
  FWeak := AWeak;
end;

destructor TSharedComponentPtrClass<T>.Destroy;
begin
  Component := nil;
  inherited;
end;

procedure TSharedComponentPtrClass<T>.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = pointer(FComponent)) then
    Component := nil;
end;

procedure TSharedComponentPtrClass<T>.SetComponent(AComponent: T);
var
  C: TComponent;
begin
  if AComponent = FComponent then
    exit;

  if FComponent <> nil then
  begin
    C := FComponent;
    FComponent := nil;
    C.RemoveFreeNotification(Self);
    if not FWeak and not (csDestroying in C.ComponentState) then
      C.Destroy;
  end;

  if AComponent <> nil then
    AComponent.FreeNotification(Self);
  FComponent := AComponent;
end;

{ TSharedComponentPtr }

constructor TSharedComponentPtr<T>.Create(AValue: T);
begin
  Init(AValue);
end;

procedure TSharedComponentPtr<T>.Init(AValue: T);
begin
  FComponentPtr := TInterfacedObject<TSharedComponentPtrClass<T>>.Create(
    TSharedComponentPtrClass<T>.Create(AValue)
  );
end;

function TSharedComponentPtr<T>.Copy: TSharedComponentPtr<T>;
begin
  if FComponentPtr = nil then
    Init(nil);
  result := Self;
end;

procedure TSharedComponentPtr<T>.Clear;
begin
  FComponentPtr := nil;
end;

function TSharedComponentPtr<T>.GetValue: T;
begin
  if FComponentPtr = nil then
    result := nil
  else
    result := FComponentPtr.Data.Component;
end;

function TSharedComponentPtr<T>.GetIsEmpty: boolean;
begin
  result := Value = nil;
end;

procedure TSharedComponentPtr<T>.SetValue(const Value: T);
begin
  if FComponentPtr = nil then
    Init(Value)
  else
    FComponentPtr.Data.Component := Value;
end;

{ TWeakComponentPtr<T> }

constructor TWeakComponentPtr<T>.Create(AValue: T);
begin
  Init(AValue);
end;

procedure TWeakComponentPtr<T>.Init(AValue: T);
begin
  FComponentPtr := TInterfacedObject<TSharedComponentPtrClass<T>>.Create(
    TSharedComponentPtrClass<T>.Create(AValue, True)
  );
end;

function TWeakComponentPtr<T>.Copy: TWeakComponentPtr<T>;
begin
  if FComponentPtr = nil then
    Init(nil);
  result := Self;
end;

procedure TWeakComponentPtr<T>.Clear;
begin
  FComponentPtr := nil;
end;

function TWeakComponentPtr<T>.GetValue: T;
begin
  if FComponentPtr = nil then
    result := nil
  else
    result := FComponentPtr.Data.Component;
end;

function TWeakComponentPtr<T>.GetIsEmpty: boolean;
begin
  result := Value = nil;
end;

procedure TWeakComponentPtr<T>.SetValue(const Value: T);
begin
  if FComponentPtr = nil then
    Init(Value)
  else
    FComponentPtr.Data.Component := Value;
end;

{ TCustomReadOnlyStream }

function TCustomReadOnlyStream.GetSize: Int64;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

function TCustomReadOnlyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

function TCustomReadOnlyStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

procedure TCustomReadOnlyStream.SetSize(const NewSize: Int64);
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

procedure TCustomReadOnlyStream.SetSize(NewSize: Integer);
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

function TCustomReadOnlyStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

function TCustomReadOnlyStream.Write(const Buffer: TBytes; Offset, Count: Integer): Longint;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

{ TEventStat }

procedure TEventStat.Reg(const EventCategory: string);
begin
  Reg(EventCategory, 1);
end;

procedure TEventStat.Reg(const EventCategory: string; Count: int64);
var
  C: int64;
begin
  if not FEvents.TryGetValue(EventCategory, C) then
    C:= 0;
  inc(C, Count);
  FEvents.AddOrSetValue(EventCategory, C);
end;

procedure TEventStat.UnReg(const EventCategory: string);
begin
  UnReg(EventCategory, 1);
end;

procedure TEventStat.UnReg(const EventCategory: string; Count: int64);
var
  C: int64;
begin
  if FEvents.TryGetValue(EventCategory, C) then
  begin
    dec(C, Count);
    if C <=0
      then FEvents.Remove(EventCategory)
      else FEvents.AddOrSetValue(EventCategory, C);
  end;
end;

procedure TEventStat.Add(const Src: TArray<TPair<string, int64>>);
var
  I: Integer;
begin
  for I := Low(Src) to High(Src) do
    Reg(Src[I].Key, Src[I].Value);
end;

function TEventStat.GetStat: TArray<TPair<string, int64>>;
var
  C: IComparer<TPair<string, int64>>;
  S: IComparer<string>;
begin
  result := FEvents.ToArray;
  S := TIStringComparer.Ordinal;
  C := TDelegatedComparer<TPair<string, int64>>.Create(
    function (const A,B: TPair<string, int64>): integer
    begin
      result := S.Compare(A.Key, B.Key);
      if result = 0 then
        if A.Key < B.Key then result := -1 else
          if A.Key = B.Key then result := 0 else
            result := 1 else
    end);
  TArray.Sort<TPair<string, int64>>(result, C);
end;

procedure TEventStat.Clear;
begin
  FEvents.Clear;
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
