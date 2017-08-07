unit adot.Strings;

{ Definition of classes/record types:

  TReplace = record
    we translate all comands to sequence of "replace"

  TStr = class
    string utils.

  TStringEditor = record
    List of string edit commands (insert/delete/replace). Can be applied to any string.
    Any operation is applied to initial text (without changes made by other commands).

  TTokAbstract = class
    Abstract class for tokenizer (parsing text and extraction some parts called tokens).
    Methods should be implemented by descendants of TTokCustom.

  TTokCharDelimitedLines = class
    Extract words separated by specific char.

  TTokComments = class
    Extract comments in Delphi style (line starting from "//", chars between "(*" and "*)" etc.

  TTokCompound = class
    Organize several tokenizers to chain.

  TTokCustom = class
    Extends abstract class with basic functionality. All desdendants should be inherited
    from TTokCustom, not from TTokAbstract.

  TTokCustomText = class
    Basic class for simple string tokenizers. Implements all required methods of TTokCustom.
    Inherited classes must implement one function for processing of custom tokenizers.

  TTokDelegated = class
    Custom function to extract specific tokens from the text.

  TTokDigits = class
    Extract words of digits.

  TTokIdentifier = class
    Extract identifiers as Delphi defines them.

  TTokLetters= class
    Extract words of letters.

  TTokLettersOrDigits = class
    Extract words of letters/digits.

  TTokLines = class
    Extract lines.

  TTokLiterals = class
    Extract string literanl as Delphi defines them ('a', 'test literal' etc).

  TTokNonSpace = class
    Extract words of non-space chars.

  TTokNot = class
    Reverses result of inner tokenizer. For example in pair with TTokNumbers will extract all text except numbers.
    Important: TTokNot will skip all empty results, only non-empty tokens are extracted.

  TTokNumbers = class
    Extract numbers ("12", "+2", "-1.3" etc).

  TTokPascal = class
    Lexical analyzer for Pascal code (check TPasTokenType for list of supported lexems).

  TTokWhitespaces = class
    Extract whitespaces.

  TTokenPos = record
    Position of token in the text. Starts from zero.

}
interface

uses
  adot.Types,
  adot.Tools, { min3 etc }
  adot.Collections, { TArr etc }
  System.Character,
  System.Math,
  System.SysUtils,
  System.StrUtils,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TTextDistance = (tdLevenstein);
  TSimilarityOption = (
    soStrictNumMatching, // [soStrictNumMatching] -> "Beløp 1200.40"<>"Beløp 7200.40" (1 char diff, but numbers are different)
    soStrictIntMatching, // [soStrictIntMatching] -> "År 2014"<>"År 2013" (1 char diff, but int numbers are different)
    soIgnoreNums,        // [soIgnoreNums]        -> "Beløp 999000.50"="Beløp 12" (9 chars diff, but all numbers ignored)
    soIgnoreInts,        // [soIgnoreInts]        -> "År 1999"="År 2014" (4 chars diff, but all int numbers ignored)
    soMatchSubst         // [soMatchSubst]        -> "hap"="New year happens every year" (big diff, but "hap" is substring)
  );
  TSimilarityOptions = set of TSimilarityOption;
  TAnsiChars = set of AnsiChar;

  TStrCharsPos = (scAll, scFirst, scLast);
  TTextEncoding = (teUnknown, teAnsi,  teUTF8,  teUTF16LE,  teUTF16BE,  teUTF32LE,  teUTF32BE);

  { Lightweight and managed analog of TStringStream:
    - write operations are faster on large input in comparing with regular string concatenation
    - supports stream-like read/write operations
    - lighter/faster than TStringStream }
  TStringBuffer = record
  public
  private
    FData: string;
    FSize: integer;
    FPosition: integer;

    procedure SetSize(Value: integer);
    function GetCapacity: integer;
    procedure SetCapacity(Value: integer);
    procedure CheckCapacity(MinCapacity: integer);
    function GetLeft: integer;
    procedure SetLeft(Value: integer);
    function GetEOF: boolean;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetEmpty: Boolean;
  public

    procedure Clear;

    procedure Write(const Src: string; CharOffset,CharCount: integer); overload;
    procedure Write(const Src: string); overload;
    procedure Write(const Src: char); overload;

    { Reads CharCount chars from current position of the buffer to Dst starting from DstCharOffset.
      Dst should be preallocated to fit all requested characters. }
    procedure Read(var Dst: string; DstCharOffset,CharCount: integer); overload;
    { Reads CharCount chars from current position of the buffer to Dst. Length of Dst will be set equal to Size. }
    procedure Read(var Dst: string; CharCount: integer); overload;
    { Reads one character from current position of the buffer to Dst. }
    procedure Read(var Dst: char); overload;

    procedure TrimExcess;

    { default encoding is UTF8 }
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding = nil);
    procedure SaveToFile(const FileName: string; Encoding: TEncoding = nil);

    { assign as string }
    class operator Implicit(const Buffer: TStringBuffer): String; static;
    class operator Implicit(const Data: string): TStringBuffer; static;
    class operator Implicit(const Data: integer): TStringBuffer; static;
    class operator Implicit(const Data: double): TStringBuffer; static;

    { case insensitive compare: AnsiSameText(ALeft.Text,ARight.Text) }
    class operator Equal(const ALeft, ARight: TStringBuffer): Boolean; overload;
    class operator Equal(const ALeft: TStringBuffer; const ARight: string): Boolean; overload;
    class operator Equal(const ALeft: string; const ARight: TStringBuffer): Boolean; overload;
    class operator NotEqual(const Left, Right: TStringBuffer): Boolean;

    { Concatenation. Position will be set behind last char of ARight. }
    class operator Add(const ALeft, ARight: TStringBuffer): TStringBuffer;
    class operator Add(const ALeft: TStringBuffer; const ARight: string): TStringBuffer;
    class operator Add(const ALeft: string; const ARight: TStringBuffer): TStringBuffer;

    { 'tests'-'s'='test', 'c:\1\2\'-'\'='c:\1\2', ...}
    class operator Subtract(const ALeft, ARight: TStringBuffer): TStringBuffer;

    { -'123'='321' }
    class operator Negative(Value: TStringBuffer): TStringBuffer;

    { 'test'*3='testtesttest'}
    class operator Multiply(const ALeft: TStringBuffer; ARight: integer): TStringBuffer;
    class operator Multiply(ALeft: integer; const ARight: TStringBuffer): TStringBuffer;

    class operator In(const Left,Right: TStringBuffer): Boolean; overload;
    class operator In(const Left: TStringBuffer; const Right: string): Boolean; overload;

    property Size: integer read FSize write SetSize;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Position: integer read FPosition write FPosition;
    property Left: integer read GetLeft write SetLeft;
    property EOF: boolean read GetEOF;
    property Empty: boolean read GetEmpty;
    property Text: string read GetText write SetText;
    { Returns dirty buffer (capacity can be larger than size). }
    property Data: string read FData;
  end;

  { Position of token in the text. Starts from zero. }
  TTokenPos = record
  public
    Start, Len: Integer;

    constructor Create(AStart,ALen: integer);
    procedure SetPos(AStart,ALen: integer);

    { [StartBytes; LenBytes] -> [StartChars; LenChars] }
    function BytesToChars: TTokenPos;

    class operator Subtract(const A,B: TTokenPos): TTokenPos; { find "space" between   }
    class operator Add(const A,B: TTokenPos): TTokenPos;      { "merge" into one token }
    class operator In(const A: integer; const B: TTokenPos): Boolean;
  end;

  { List of string edit commands (insert/delete/replace). Can be applied to any string.
    Any operation is applied to initial text (without changes made by other commands). }
  TStringEditor = record
  private
    type
      { we translate all comands to sequence of "replace" }
      TReplace = record
        SrcText : string;
        SrcPos  : TTokenPos;
        DstPos  : TTokenPos;
        Order   : integer;

        procedure Clear;
      end;
      PReplace = ^TReplace;

    var
      Instructions: TArr<TReplace>;

    procedure Add(const Instr: TReplace);
    procedure Sort;

  public

    procedure Clear;

    procedure Delete(const Start,Len: integer); overload;
    procedure Delete(const Pos: TTokenPos); overload;
    procedure Delete(const Pos: integer); overload;

    procedure Insert(Pos: integer; const Substr: string; const SubStrOffset,SubStrLen: integer); overload;
    procedure Insert(Pos: integer; const Substr: string; const SubStrPos: TTokenPos); overload;
    procedure Insert(Pos: integer; const Substr: string); overload;

    procedure Replace(const Start,Len : integer;   const Substr: string; const SubStrOffset,SubStrLen : integer); overload;
    procedure Replace(const Pos       : TTokenPos; const Substr: string; const SubStrPos : TTokenPos); overload;
    procedure Replace(const Start,Len : integer;   const Substr: string); overload;
    procedure Replace(const Pos       : TTokenPos; const Substr: string); overload;

    { Apply all commands to string Src and get result }
    function Apply(const Src: string): string;
  end;

  { string utils }
  TStr = class
  public
    type
      TExtractType = (
        etNumbers,     { keep numbers only   : 'a5.7b2012c 12,9' -> '5.7 2012 12,9' }
        etNotNumbers,  { remove numbers      : 'a5.7b2012c' -> 'abc'                }
        etDigits,      { keep digits only    : 'a5.7b2012c' -> '572012'             }
        etNotDigits,   { remove digits       : 'a5.7b2012c' -> 'a.bc'               }
        etDigitsToEnd  { group digits at end : 'a5.7b2012c' -> 'a.bc572012'         }
      );

      TSplitOptions = record
      public
        MaxStrLen: integer;
        AllowWordToExceedMaxLen: boolean;

        procedure Init;
        class function Create: TSplitOptions; static;
      end;

  private
    {$IFNDEF NoCaseMap}
    class var
      FLoCaseMap: array[Char] of Char;
      FHiCaseMap: array[Char] of Char;
    {$ENDIF}
    class var
      FOrdinalComparerTrimText: IComparer<String>;
      FOrdinalComparerAsInt: IComparer<String>;

    class function LevensteinDistance(s, t: string): integer; static;
    class function SubstringDistance(const a, b: String; var Dist: integer): Boolean; static;

    class function GetNumbers(const s: string): String; static;
    class function RemoveNumbers(const s: string): String; static;
    class function GetDigits(const s: string): String; static;
    class function RemoveDigits(const s: string): String; static;
    class function MoveDigitsToEnd(const s: string): String; static;

    class procedure InitializeVars; static;
    class procedure FinalizeVars; static;

  public

    { concatanate not empty values from Src }
    class function Concat(const Src: array of string; Delimeter: string = ' '): string; overload; static;
    class function Concat(Src: TArray<string>; Delimeter: string = ' '): string; overload; static;
    { concatanate used values from Src (including empty strings) }
    class function Concat(const Src: array of string; const InUse: array of boolean; Delimeter: string = ' '): string; overload; static;

    class function Reverse(const S: string): string; static;

    { returns new string where all specified chars replaced by string }
    class function Replace(const Src: string; CharsToReplace: TSet<Char>; const CharReplacement: string): string; static;

    { split text into lines of fixed length with transfering words and punctuation }
    class function Split(const Src: string; MaxStrLen: integer = 80): TArray<string>; overload; static;
    class function Split(const Src: string; const Options: TSplitOptions): TArray<string>; overload; static;
    { Returns max possible encoded substring for specified bufer. }
    class function GetMaxEncodedBytes(const S: string; BufSize: integer; Encoding: TEncoding): TBytes; static;
    { Trancates string without breakig a words. }
    class function TruncToWord(const Src: string; MaxCharLen: integer; AddDots: boolean = True): string; static;
    { Replaces any sequence of any space/control chars by single space. }
    class function TruncSpaces(const Src: string; TrimRes: boolean = True): string; static;
    { xxx -> 'xxx'
      Delphi (up to Delphi 10 Seattle at least) doesn't allow string literals to be longer than 255 chars.
      MakeStringLiteral will add quote char and split to several lines if necessary }
    class function MakeValidStringLiteral(const s: string): string; static;
    { General function for escaping special characters. To be more precise it is allowed to escape
      any char except digits and latin chars in range 'A'..'F'. Escaped chars are converted to HEX:
        Escape( 'key=value', '=' ) = 'key\3D00value' }
    class function HexEscape(const Value,CharsToEscape: string; const EscapeChar: Char = '\'): string; static;
    class function HexUnescape(const Value: string; const EscapeChar: Char = '\'): string; static;
    { Can be used to encode strings as Delphi-compatible string literals.
      TTokPascal tokenizer can be used to extract and decode strings (or DecodeStringLiterals).
      "abc" -> "'abc'"
      " abc" -> "' abc'"
      "a'bc" -> "a'''bc"
      " a'bc " -> "' a'''bc '"}
    class function EncodeStringLiteral(const Value: string): string; overload; static;
    class procedure EncodeStringLiteral(const Value: string; var Dst: TStringBuffer); overload; static;
    class function EncodeStringLiterals(const Values: TArray<string>): string; static;
    class function DecodeStringLiterals(const Values: string): TArray<string>; static;

    { TTestApp -> "Test app" }
    class function ClassNameToCaption(const AClassName: string): string; static;

    { returns new string where all specified chars are deleted }
    class function Remove(const Src: string; CharsToDelete: TSet<Char>): string; static;

    { case insensitive with support of internation chars }
    class function SameText(const A,B: string): Boolean; overload;
    class function SameText(const A: string; const B: array of string): Boolean; overload;
    class function SameText(A,B: PChar; Len: integer): Boolean; overload;
    class function SameTrimText(const A,B: string): Boolean; overload;

    class function CompareStrings(const A,B: string): integer; overload;
    class function CompareText(const A,B: string): integer; overload;
    class function CompareTrimText(const A,B: string): integer; overload;
    class function CompareAsInt(const A,B: string): integer; overload;

    class function ComparerStrings: IComparer<String>; overload;
    class function ComparerText: IComparer<String>; overload;
    class function ComparerTrimText: IComparer<String>; overload;
    class function ComparerAsInt: IComparer<String>; overload;

    class function LowerCaseChar(C: Char): Char; static;
    class function UpperCaseChar(C: Char): Char; static;
    class function LowerCase(const S: string; Chars: TStrCharsPos = scAll): string; static;
    class function UpperCase(const S: string; Chars: TStrCharsPos = scAll): string; static;

    { based on TTokLines }
    class procedure TextToLines(const Text: string; Dst: TStrings; AClear: boolean = True); overload; static;
    class procedure TextToLines(const Text: string; out Dst: TArray<string>); overload; static;
    class procedure TextToLines(const Text: string; Delim: Char; out Dst: TArray<string>); overload; static;

    { similarity functions }
    class function TextDistance(const a,b: string; StrDistType: TTextDistance = tdLevenstein): integer; static;
    class function SimilarStrings(A,B: String; var dist: integer; AOptions: TSimilarityOptions = [soStrictIntMatching]): boolean; overload; static;
    class function SimilarStrings(A,B: String; AOptions: TSimilarityOptions = [soStrictIntMatching]): boolean; overload; static;
    { unlike SimilarStrings this function will try to match AQuery with every word of AText (min dist selected) }
    class function SimilarWordInText(const AWord, AText: String; var dist: integer; AOptions: TSimilarityOptions = [soStrictIntMatching]): boolean; static;

    { extract numbers/digits etc }
    class function Extract(InfoType: TExtractType; const s: string): String; static;

    { System.SysUtils.TextPos doesn't work with international chars "Æ","Å" etc (XE5 at least). }
    class function TextPosition(const ASubStr, AText: string; AOffset: integer = 0): Integer; static;
    class function Contains(const ASubStr, AText: string): boolean; static;

    { file-string }
    class function Load(Src: TStream; Encoding: TEncoding = nil): string; overload; static;
    class function Load(const FileName: string; Encoding: TEncoding = nil): string; overload; static;
    class procedure Save(Dst: TStream; const S: string; Encoding: TEncoding = nil); overload; static;
    class procedure Save(const FileName: string; const S: string; Encoding: TEncoding = nil); overload; static;

    class function LoadFileToArray(const Filename: string; Encoding: TEncoding = nil): TArray<string>; static;
    class function LoadStreamToArray(Src: TStream; Encoding: TEncoding = nil): TArray<string>; static;
    class procedure SaveArrayToFile(const Src: TArray<string>; const Filename: string; Encoding: TEncoding = nil); static;
    class procedure SaveArrayToStream(const Src: TArray<string>; const Dst: TStream; Encoding: TEncoding = nil); static;

    { set-string, number-string etc }
    class function CharsCount(const AChars: TAnsiChars): integer;
    class function SetToString(const AChars: TAnsiChars): string;
    class function StringToSet(const s: string): TAnsiChars;
    class function IntToString(const N: int64; MinResLen: integer = -1; LeadingSpaceChar: char = '0'): string; static;

    { make string printable (replace all unprintable/control chars):
        GetPrintable( 'line1' + #13#10 + 'line2' + #8 + 'qqq' ) = 'line1  line2?qqq' }
    class function GetPrintable(const S: string; ReplChar: Char = '?'): string; overload; static;
    class function GetPrintable(S: PChar; Count: integer; ReplChar: Char = '?'): string; overload; static;

    { randomization }
    class function Random(ALen: integer; const AChars: TAnsiChars): string; overload;
    class function Random(ALen: integer; const AChars: string): string; overload;
    class function Random(ALen: integer): string; overload;
    class function Random(ALen: integer; AFrom,ATo: Char): string; overload;
    class function Random: string; overload;

    class function FixDecimalSeparator(const Src: string): string; static;

    class function ToInteger(const Src: string): int64; static;
    class function ToFloat(const Src: string): double; static;
    class function ToBoolean(const Src: string): boolean; static;
    class function ToDateTime(const Src: string): TDateTime; static;

    class function ToIntegerDef(const Src: string; DefValue: int64 = 0): int64; static;
    class function ToFloatDef(const Src: string; DefValue: int64 = 0): double; static;
    class function ToBooleanDef(const Src: string; DefValue: boolean = False): boolean; static;
    class function ToDateTimeDef(const Src: string; DefValue: TDateTime = 0): TDateTime; static;

    class function TryToInteger(const Src: string; var Value: int64): boolean; overload; static;
    class function TryToInteger(const Src: string; var Value: integer): boolean; overload; static;
    class function TryToFloat(const Src: string; var Value: double): boolean; overload; static;
    class function TryToFloat(const Src: string; var Value: single): boolean; overload; static;
    class function TryToFloat(const Src: string; var Value: extended): boolean; overload; static;
    class function TryToBoolean(const Src: string; var Value: boolean): boolean; static;
    class function TryToDateTime(const Src: string; var Value: TDateTime): boolean; static;

    { Count - size of Text used to keep text (allocated storage can be larger that data)
      TextPos - where text starts in Text array
      TextIsFragment - True if Text is (possibly) cut at the end, for example when we test first bytes of the file }
    class function IsValidUtf8(const Text: TArray<Byte>; Count,TextPos: integer; TextIsFragment: boolean = True): boolean; overload; static;
    class function IsValidUtf8(const Text: TArray<Byte>): boolean; overload; static;

    { Analyze fragment of text & detect correct encoding:
        Count - size of Text used to keep text (allocated storage can be larger that data)
        TextStartPos - position where text starts excluding BOM
        TextIsFragment - True if Text is (possibly) cut at the end, for example when we test first bytes of the file }
    class function DetectEncoding(const Text: TArray<Byte>; Count: integer;
      out TextStartPos: integer; TextIsFragment: boolean): TTextEncoding; static;

    { Will try to detect code page of input. Advantages over TEncoding.GetEncoding (Delphi 10.2):
        - supports UTF32 (little endian / big endian)
        - analyzes data for detection (TTEncoding.GetEncoding analyzes BOM only, "preamble" in TEncoding terminology).
        - 100% detection of UTF family if encoded data is build according to RFC }
    class function DetectCodepage(const AText: TArray<byte>; Count: integer; out CodePage: Cardinal): boolean; overload; static;
    class function DetectCodepage(const AText: TArray<byte>; out CodePage: Cardinal): boolean; overload; static;
    class function DetectCodepage(const AText: TArray<byte>; out Encoding: TEncoding): boolean; overload; static;

    { False if AText is 100% not encoded with code page ACodePage.
      True if AText may (or may not!) be encoded with code page ACodePage.
      Can be used to disable UI elements/functionality specific for some code pages only:
         1200 UTF16 little endian
         1201 UTF16 big endian
        12000 UTF32 little endian
        12001 UTF32 big endian
        65001 UTF8 }
    class function IsPossibleEncoding(const AText: TArray<byte>; ACodePage: Cardinal): boolean;

    { Extends TEncoding.GetEncoding with some extra code pages (UTF32 is not supported by TEncoding in Delphi 10.2) }
    class function GetEncoding(CodePage: Cardinal): TEncoding;
  end;

  { implementation of UTF32 big endian }
  TBigEndianUTF32Encoding = class(TUnicodeEncoding)
  strict protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetCodePage: Cardinal; override;
    function GetEncodingName: string; override;
  public
    function Clone: TEncoding; override;
    function GetPreamble: TBytes; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
  end;

  { implementation of UTF32 little endian }
  TLittleEndianUTF32Encoding = class(TBigEndianUTF32Encoding)
  strict protected
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetCodePage: Cardinal; override;
    function GetEncodingName: string; override;
  public
    function Clone: TEncoding; override;
    function GetPreamble: TBytes; override;
  end;

  { Usually all chars of text belong to one code page.
    It is more efficient to save such text in single byte encoding with code page id. }
  TCodePageId = (
    cpiCyr,
    cpiEur
  );
  TCodePages = class
  private
    type
      TStrCyr = type AnsiString(1251);
      TStrEur = type AnsiString(1252);

    class var
      CyrToUnicode: array[Byte] of Char;
      UnicodeToCyr: TDictionary<Char, Byte>;
      EurToUnicode: array[Byte] of Char;
      UnicodeToEur: TDictionary<Char, Byte>;

    class destructor Destroy;
    class procedure InitCyr; static;
    class procedure InitEur; static;
    class function EncodeTextEur(const Text: string; Len: integer; var Dst): Boolean; static;
    class function EncodeTextCyr(const Text: string; Len: integer; var Dst): Boolean; static;
    class function DecodeTextCyr(const Src; Size: integer): string; static;
    class function DecodeTextEur(const Src; Size: integer): string; static;
  public
    class function EncodeText(const Text: string; var CodePageId: TCodePageId; var Bytes: TArray<byte>): Boolean; static;
    class function DecodeText(const Bytes: TArray<byte>; CodePageId: TCodePageId): string; static;
    class function StringToBuf(const Text: string; BufSize: integer; var CodePageId: TCodePageId; var Buf; var Len: integer): boolean; static;
    class function BufToString(const Buf; BufSize: integer; CodePageId: TCodePageId): string; static;
  end;

  {# Internal structure to describe the token just found by tokenizer.
    Any or even all fields can be zero. }
  TTokenInfo = record
    Start            : integer; { start position in the text             }
    DelimitersPrefix : integer; { number of preceding delimiters or zero }
    Len              : integer; { length of the token or zero            }
    DelimitersSuffix : integer; { number of trailing delimiters or zero  }

    procedure Clear;
  end;

  {# Used to save/restore state of TCustomTokenizer class and descendants. }
  TTokenizerState = record
  private
    State: IInterfacedObject<TMemoryStream>;
  end;

  { Abstract class for tokenizer (parsing text and extraction some parts called tokens).
    Methods should be implemented by descendants of TTokCustom }
  TTokAbstract = class abstract
  protected
    procedure SetText(AText: PChar; ATextLen: integer); virtual; abstract;
    function GetText: PChar; virtual; abstract;
    function GetAsString: string; virtual; abstract;
    function GetTextLen: integer; virtual; abstract;
    procedure SetPosition(const AValue: integer); virtual; abstract;
    function GetPosition: integer; virtual; abstract;
    procedure ResetTokenizer; virtual; abstract;
    procedure SaveTokenizerPos(W: TWriter); virtual; abstract; { save .Position and other fields if necessary }
    procedure LoadTokenizerPos(R: TReader); virtual; abstract;
    function NextToken(out AToken: TTokenInfo): Boolean;  virtual; abstract;
  public
    property Text: PChar read GetText;
    property TextLen: integer read GetTextLen;
    property Position: integer read GetPosition write SetPosition;
    property AsString: string read GetAsString;
  end;

  { Extends abstract class with basic functionality. All desdendants should be inherited
    from TTokCustom, not from TTokAbstract }
  TTokCustom = class abstract (TTokAbstract)
  protected
    FOmmitEmptyTokens: boolean;

    { get token as string }
    function GetTokenAsString(const APos: TTokenPos): string;

    { Get number of words in the text (doesn't change current position) }
    function GetTokenCount: integer;

    procedure SaveTokenizerPos(W: TWriter); override; { save .Position and other fields if necessary }
    procedure LoadTokenizerPos(R: TReader); override;
    function GetAsString: string; override;

    function GetState: TTokenizerState;
    procedure SetState(AState: TTokenizerState);

    { always called by any constructor }
    procedure TokenizerCreated; virtual;

  public
    constructor Create(AText: PChar; ATextLen: integer); overload; virtual;
    constructor Create(const AText: string); overload; virtual;
    constructor Create; overload; virtual;

    { Find next token (word) in the text }
    function Next(out AToken: TTokenPos): Boolean; overload;
    function Next(out AToken: String): Boolean; overload;
    function Next: String; overload;

    { Reset parser to initial state (Position:=0 etc ) }
    procedure Reset; overload;
    procedure Reset(ANewText: PChar; ATextLen: integer); overload;

    { high level methods }
    procedure GetTokens(ADst: TStrings); overload;
    procedure GetTokens(var ADst: TArray<String>); overload;
    procedure GetTokens(var ADst: TArray<TTokenPos>); overload;

    class procedure Parse(const AText: string; ADst: TStrings); overload;
    class procedure Parse(const AText: string; var ADst: TArray<String>); overload;
    class procedure Parse(const AText: string; var ADst: TArray<TTokenPos>); overload;
    class procedure Parse(const AText: string; const ADelimiter: string; var ADst: string); overload;
    class function  Parse(const AText: string): string; overload;

    { Parse text and find number of tokens }
    property TokenCount: integer read GetTokenCount;

    { Get token (defined as TTokenPos) as string }
    property Tokens[const APos: TTokenPos]: string read GetTokenAsString; default;

    { Allows to save current parsing state of tokenizer and restore it later to
      proceed text processing from same position. Doesn't save text, only
      position and internal state required to proceed from the position. }
    property Bookmark: TTokenizerState read GetState write SetState;
  end;

  { Example:
    - First tokenizer removes comments (extracts parts of text excluding comments).
    - Second tokenizer extracting identifiers (Delphi-style ids).
    Such TTokenizerCompound can be used to lookup identifiers with correct
    processing of comments and string literals. }
  { Organize several tokenizers to chain }
  TTokCompound = class(TTokCustom)
  protected
    FTokenizers: TObjectList<TTokCustom>;
    FOffsets: TArray<integer>;
    FCurrentTokenizer: integer;

    { Implementation of TTokAbstract }
    procedure SetText(AText: PChar; ATextLen: integer); override;
    function GetText: PChar; override;
    function GetTextLen: integer; override;
    function GetAsString: string; override;
    procedure SetPosition(const AValue: integer); override;
    function GetPosition: integer; override;
    procedure ResetTokenizer; override;
    procedure SaveTokenizerPos(W: TWriter); override; { save .Position and other fields if necessary }
    procedure LoadTokenizerPos(R: TReader); override;
    function NextToken(out AToken: TTokenInfo): Boolean;  override;

    { Additional methods, specific for this class}
    function GetTokenizersCount: integer;
    function GetTokenizer(n: integer): TTokCustom;
    function GetFirst: TTokCustom;
    function GetLast: TTokCustom;

  public
    constructor Create(const ATokenizers: array of TTokCustom); reintroduce;
    destructor Destroy; override;

    { chain of inner tokenizers }
    property TokenizersCount: integer read GetTokenizersCount;
    property Tokenizers[n: integer]: TTokCustom read GetTokenizer;
    property First: TTokCustom read GetFirst;
    property Last: TTokCustom read GetLast;
  end;

  { Reverses result of inner tokenizer. For example in pair with TTokNumbers will extract all text except numbers.
    Important: TTokNot will skip all empty results, only non-empty tokens are extracted. }
  TTokNot = class(TTokCustom)
  protected
    type
      TState = (Starting, Running, Finished);
    var
      FTokenizer: TTokCustom;
      FOwnsInnerTokenizer: boolean;
      FFinished: boolean;
      FValueStart: integer;
      FValueLen: integer;

    procedure SetText(AText: PChar; ATextLen: integer); override;
    function GetText: PChar; override;
    function GetTextLen: integer; override;
    function GetAsString: string; override;
    procedure SetPosition(const AValue: integer); override;
    function GetPosition: integer; override;
    procedure ResetTokenizer; override;
    procedure SaveTokenizerPos(W: TWriter); override; { save .Position and other fields if necessary }
    procedure LoadTokenizerPos(R: TReader); override;
    function NextToken(out AToken: TTokenInfo): Boolean;  override;
  public
    constructor Create(ATokenizer: TTokCustom; AOwnsInnerTokenizer: boolean);
    destructor Destroy; override;
  end;

  { Basic class for simple string tokenizers. Implements all required methods of TTokCustom.
    Inherited classes must implement one function for processing of custom tokenizers. }
  TTokCustomText = class abstract (TTokCustom)
  protected
    FTextStorage : String;         { to keep text if provided as string }
    FText        : PChar;          { full text }
    FTextLen     : integer;        { size }
    FPosition    : integer;

    { implementation of TTokAbstract methods }
    procedure SetText(AText: PChar; ATextLen: integer); override;
    function GetText: PChar; override;
    function GetTextLen: integer; override;
    function GetAsString: string; override;
    procedure SetPosition(const AValue: integer); override;
    function GetPosition: integer; override;
    procedure ResetTokenizer; override;
    procedure SaveTokenizerPos(W: TWriter); override; { save .Position and other fields if necessary }
    procedure LoadTokenizerPos(R: TReader); override;
    function NextToken(out AToken: TTokenInfo): Boolean;  override;

    { Usually we need to override this only method in descendant class to implement
      specific text tokenizer (extract lines /comments/numbers etc):
      - Parameter "Res" preinitialized to zero.
      - Returns False if parsing is finished (no token found), Res is undefined.
      - Returns True if next token is found, Res is placement of the token in the Text. }
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; virtual; abstract;

  public
    constructor Create(AText: PChar; ATextLen: integer); override;
    constructor Create(const AText: string); override;

    procedure Reset(const AText: string); overload;
    procedure Reset(const AText: string; AStart,ALen: integer); overload;
    procedure Reset(const AText: PChar; ALen: integer); overload;
  end;

  TTokText = class(TTokCustomText)
  public
    type
      TTextTokenType = (tttWord, tttPunctuation, tttWhitespace);

  protected
    FLastTokenType: TTextTokenType;

    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;

  public

    property LastTokenType: TTextTokenType read FLastTokenType;
  end;

  { Custom function to extract specific tokens from the text }
  TTokDelegated = class(TTokCustomText)
  public
    type
      TDelegatedTokenizer = reference to function(Text: PChar; Len: integer; var Res: TTokenInfo): boolean;

  protected
    FProc: TDelegatedTokenizer;

    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;

  public
    constructor Create(AText: PChar; ATextLen: integer; AProc: TDelegatedTokenizer); reintroduce; overload;
    constructor Create(const AText: string; AProc: TDelegatedTokenizer); reintroduce; overload;
  end;

  { Extract words of non-space chars. }
  TTokNonSpace = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  end;

  { Extract words of letters. }
  TTokLetters= class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  end;

  { Extract words of digits. }
  TTokDigits = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    class function IsStartOfDigits(C: Char): boolean; static;
    class function NextDigits(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; static;
  end;

  { Extract words of letters/digits. }
  TTokLettersOrDigits = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  end;

  { Extract numbers ("12", "+2", "-1.3" etc). }
  TTokNumbers = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  end;

  { Extract lines from text (strings separated by new line markers).
    1. If file is empty then returns 0 lines. This has important consequences.
    2. If file contains only EOF char, then it contains 1 line finished by that char. If we read it as two lines,
       then we can't encode one empty line (empty file - 0 line, EOL char - 2 lines, no way to encode 1 line).
    3. As result of 1. and 2.:
      - Every EOL char finishes line, not introducing new one.
      - Last EOL can be ommited for not empty line.
    With such algorithm conversion array->text and text->array is simmetric, TStringList.Load works same way.
    Text editors usually use another logic. If we open empty file in NotePad++, it will show 1 line
    (editors never show zero lines). Such aproach doesn't provide simmetric conversion array-text. }
  { Extract lines. }
  TTokLines = class(TTokCustomText)
  protected
    procedure TokenizerCreated; override;
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    class function GetLine(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; static;
    class function GetLineLength(const Text: PChar; Len: integer; IncludeEOL: boolean): Integer; static;
  end;

  { Extract words separated by specific char. }
  TTokCharDelimitedLines = class(TTokCustomText)
  protected
    FDelimiter: Char;
    FCaseSensitive: boolean;

    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    constructor Create(AText: PChar; ATextLen: integer; ADelimiter: Char; ACaseSensitive: Boolean = False); overload;
    constructor Create(const AText: string; ADelimiter: Char; ACaseSensitive: Boolean = False); overload;
  end;

  { Extract identifiers as Delphi defines them. }
  TTokIdentifier = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    class function IsStartOfIdentifier(C: Char): Boolean; static;
    class function NextIdentifier(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; static;
  end;

  { Search for literals starting/ending by "'". Doesn't support #n style, for example:
    - #13#10 will not be recognized as literal
    - 'a'#13#10 will be racognized as literal 'a' }
  { Extract string literanl as Delphi defines them ('a', 'test literal' etc). }
  TTokLiterals = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    class function IsStartOfLiteral(C: Char): boolean; static;
    class function NextLiteral(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; static;
    class function GetLiteralLen(Text: PChar; Len: integer): integer;
  end;

  { Extract whitespaces. }
  TTokWhitespaces = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    class function IsStartOfWhitespace(C: Char): Boolean; static;
    class function NextWhitespace(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; static;
  end;

  { Extract comments in Delphi style (line starting from "//", chars between "(*" and "*)" etc. }
  TTokComments = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    class function IsStartOfComment(Text: PChar; Len: integer): Boolean; static;
    class function NextComment(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; static;
  end;

  TPasTokenType = (Unknown, Whitespace, Literal, Digits, Comment, Identifier, Delimiter);
  TPasTokenTypes = set of TPasTokenType;
const
  PasAllTokenTypes = [Low(TPasTokenType)..High(TPasTokenType)];

type
  { Lexical analyzer for Pascal code (check TPasTokenType for list of supported lexems). }
  TTokPascal = class(TTokCustomText)
  protected
    FTokenType: TPasTokenType;

    const
      Delimiters = [
        ',', '.', ';', '#', '@', '&', '^','$',
        '[','(','{', ']',')','}',
        '+','-','/','*','<','>','=',':'
      ];

    procedure SaveTokenizerPos(W: TWriter); override; { save .Position and other fields if necessary }
    procedure LoadTokenizerPos(R: TReader); override;
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;

  public
    type
      TToken = record
        FPos: TTokenPos;
        FType: TPasTokenType;
      end;

    function Next(ATokenTypes: TPasTokenTypes; out AToken: TTokenPos): Boolean; overload;
    function Next(ATokenTypes: TPasTokenTypes; out AToken: String): Boolean; overload;
    function Next(ATokenTypes: TPasTokenTypes): String; overload;
    class function IsDelimiterChar(C: Char): Boolean; static;

    function GetAllTokens: TArray<TToken>;

    property LastTokenType: TPasTokenType read FTokenType;
  end;

implementation

{ TStr.TSplitOptions }

class function TStr.TSplitOptions.Create: TSplitOptions;
begin
  result.Init;
end;

procedure TStr.TSplitOptions.Init;
begin
  Self := Default(TStr.TSplitOptions);
  MaxStrLen := 80;
  AllowWordToExceedMaxLen := False;
end;

{ TStr }

class function TStr.Concat(const Src: array of string; Delimeter: string): string;
var
  i: Integer;
begin
  result := '';
  for i := Low(Src) to High(Src) do
    if Src[i]<>'' then
      if result='' then
        result := Src[i]
      else
        result := result + Delimeter + Src[i];
end;

class function TStr.Concat(Src: TArray<string>; Delimeter: string): string;
var
  i: Integer;
begin
  result := '';
  for i := Low(Src) to High(Src) do
    if Src[i]<>'' then
      if result=''
        then result := Src[i]
        else result := result + Delimeter + Src[i];
end;

class function TStr.Concat(const Src: array of string; const InUse: array of boolean; Delimeter: string = ' '): string;
var
  i: Integer;
  empty: Boolean;
begin
  result := '';
  empty := True;
  for i := Low(Src) to High(Src) do
    if InUse[i] then
      if Empty then
      begin
        result := Src[i];
        Empty := False;
      end
      else
        result := result + Delimeter + Src[i];
end;

class function TStr.ClassNameToCaption(const AClassName: string): string;
var
  i: Integer;
begin
  if result.StartsWith('T') then
    result := result.Substring(1);
  for i := Length(result)-1 downto 1 do
    if result.Chars[i].IsUpper and result.Chars[i-1].IsLower then
      result := result.Insert(i, ' ');
  result := result.UpperCase(result.Substring(0,1)) + result.LowerCase(result.Substring(1));
end;

class function TStr.CompareAsInt(const A, B: string): integer;
var
  AI,BI: int64;
begin
  if TryStrToInt64(A, AI) and TryStrToInt64(B, BI) then
  begin
    if AI < BI then Result := -1 else
    if AI > BI then Result := 1 else
    Result := 0;
  end
  else
    result := CompareText(A, B);
end;

class function TStr.ComparerAsInt: IComparer<String>;
begin
  if FOrdinalComparerAsInt = nil then
    FOrdinalComparerAsInt := TDelegatedComparer<String>.Create(
      function (const A,B: string): integer
      begin
        result := CompareAsInt(A, B);
      end);
  result := FOrdinalComparerAsInt;
end;

class function TStr.ComparerStrings: IComparer<String>;
begin
  result := TStringComparer.Ordinal;
end;

class function TStr.ComparerText: IComparer<String>;
begin
  result := TIStringComparer.Ordinal;
end;

class function TStr.ComparerTrimText: IComparer<String>;
begin
  if FOrdinalComparerTrimText = nil then
    FOrdinalComparerTrimText := TDelegatedComparer<String>.Create(
      function (const A,B: string): integer
      begin
        result := CompareTrimText(A, B);
      end);
  result := FOrdinalComparerTrimText;
end;

class function TStr.CompareStrings(const A,B: string): integer;
begin
  result := AnsiCompareStr(A, B);
end;

class function TStr.CompareText(const A, B: string): integer;
begin
  result := AnsiCompareText(A, B);
end;

class function TStr.CompareTrimText(const A, B: string): integer;
begin
  result := CompareText(Trim(A), Trim(B));
end;

class function TStr.HexEscape(const Value, CharsToEscape: string; const EscapeChar: Char): string;
var
  S: TSet<Char>;
  I: Integer;
begin
  S.Clear;
  for I := Low(CharsToEscape) to High(CharsToEscape) do
  begin
    Assert( not (((CharsToEscape[I]>='0') and (CharsToEscape[I]<='9')) or ((CharsToEscape[I]>='A') and (CharsToEscape[I]<='F'))) );
    S.Add(CharsToEscape[I]);
  end;
  S.Add(EscapeChar);
  result := '';
  for I := 0 to Length(Value)-1 do
    if not (Value.Chars[I] in S) then
      result := result + Value.Chars[I]
    else
      result := result + EscapeChar + THex.Encode(Value.Chars[I]);
end;

class function TStr.HexUnescape(const Value: string; const EscapeChar: Char): string;
var
  I: Integer;
begin
  result := '';
  I := 0;
  while I < Length(Value) do
  begin
    if Value.Chars[I] <> EscapeChar then
      result := result + Value.Chars[I]
    else
    begin
      result := result + THex.DecodeString(Value.SubString(I+1, SizeOf(Char)*2));
      Inc(I, SizeOf(Char)*2);
    end;
    inc(I);
  end;
end;

class function TStr.UpperCase(const S: string; Chars: TStrCharsPos): string;
begin
  case Chars of
    scAll:
      result := S.ToUpper;
    scFirst:
      begin
        result := S;
        if result<>'' then
          result[Low(result)] := result.Chars[0].ToUpper;
      end;
    scLast:
      begin
        result := S;
        if result<>'' then
          result[High(result)] := result.Chars[Length(result)-1].ToUpper;
      end;
  end;
end;

class function TStr.LowerCase(const S: string; Chars: TStrCharsPos): string;
begin
  case Chars of
    scAll:
      result := S.ToLower;
    scFirst:
      begin
        result := S;
        if result<>'' then
          result[Low(result)] := LowerCaseChar(result[Low(result)]);
      end;
    scLast:
      begin
        result := S;
        if result<>'' then
          result[High(result)] := LowerCaseChar(result[High(result)]);
      end;
  end;
end;

class function TStr.Extract(InfoType: TExtractType; const s: string): String;
begin
  case InfoType of
    etNumbers     : result := GetNumbers(s);
    etNotNumbers  : result := RemoveNumbers(s);
    etDigits      : result := GetDigits(s);
    etNotDigits   : result := RemoveDigits(s);
    etDigitsToEnd : result := MoveDigitsToEnd(s);
    else raise Exception.Create('Error');
  end;
end;

class function TStr.SameText(const A, B: string): Boolean;
begin
  result := AnsiSameText(A,B);
end;

class function TStr.CharsCount(const AChars: TAnsiChars): integer;
var
  C: AnsiChar;
begin
  result := 0;
  for C in AChars do
    inc(result);
end;

class function TStr.IntToString(const N: int64; MinResLen: integer = -1; LeadingSpaceChar: char = '0'): string;
begin
  result := IntToStr(N);
  if Length(result) < MinResLen then
    if (LeadingSpaceChar = '0') and (N < 0) then
      result := result.Substring(0, 1) + StringOfChar('0', MinResLen-Length(result)) + result.Substring(1)
    else
      result := StringOfChar(LeadingSpaceChar, MinResLen-Length(result)) + result;
end;

class function TStr.StringToSet(const s: string): TAnsiChars;
var
  I: Integer;
begin
  result := [];
  for I := Low(s) to High(s) do
    include(result, AnsiChar(s[i]));
end;

class function TStr.SetToString(const AChars: TAnsiChars): string;
var
  c: AnsiChar;
  i: Integer;
begin
  SetLength(result, CharsCount(AChars));
  i := Low(result);
  for c in AChars do
  begin
    result[i] := Char(c);
    inc(i);
  end;
end;

class function TStr.Random(ALen: integer; const AChars: string): string;
var
  I,J: Integer;
begin
  J := Length(AChars);
  setlength(result, ALen);
  for I := Low(result) to High(result) do
    result[I] := AChars[Low(AChars)+System.Random(J)];
end;

class function TStr.Random(ALen: integer; const AChars: TAnsiChars): string;
begin
  result := Random(ALen, SetToString(AChars));
end;

class function TStr.Random(ALen: integer): string;
begin
  result := Random(ALen, ['a'..'z','0'..'9']);
end;

class function TStr.RemoveDigits(const s: string): String;
var
  i,j: Integer;
begin
  j := 0;
  for i := 1 to length(s) do
    if (s[i]<'0') or (s[i]>'9') then
      inc(j);
  setlength(result, j);
  j := 0;
  for i := 1 to length(s) do
    if (s[i]<'0') or (s[i]>'9') then
    begin
      inc(j);
      result[j] := s[i];
    end;
end;

class function TStr.RemoveNumbers(const s: string): String;
var
  i,j: Integer;
begin
  j := 0;
  for i := 1 to length(s) do
    if (s[i]>='0') and (s[i]<='9') or
      ((s[i]='.') or (s[i]=',')) and (
        (i>1) and (s[i-1]>='0') and (s[i-1]<='9') or
        (i<length(s)) and (s[i+1]>='0') and (s[i+1]<='9')
      )
    then
      { part of number }
    else
      inc(j);
  setlength(result, j);
  j := 0;
  for i := 1 to length(s) do
    if (s[i]>='0') and (s[i]<='9') or
      ((s[i]='.') or (s[i]=',')) and (
        (i>1) and (s[i-1]>='0') and (s[i-1]<='9') or
        (i<length(s)) and (s[i+1]>='0') and (s[i+1]<='9')
      )
    then
      { part of number }
    else
    begin
      inc(j);
      result[j] := s[i];
    end;
end;

class function TStr.Replace(const Src: string; CharsToReplace: TSet<Char>; const CharReplacement: string): string;
var
  Buf: TStringBuffer;
  I: Integer;
begin
  Buf.Clear;
  for I := 0 to Src.Length-1 do
    if Src.Chars[I] in CharsToReplace then
      Buf.Write(CharReplacement)
    else
      Buf.Write(Src.Chars[I]);
  result := Buf.Text;
end;

class function TStr.Remove(const Src: string; CharsToDelete: TSet<Char>): string;
var
  Buf: TStringEditor;
  I: Integer;
begin
  Buf.Clear;
  for I := 0 to Src.Length-1 do
    if Src.Chars[I] in CharsToDelete then
      Buf.Delete(I);
  result := Buf.Apply(Src);
end;

class function TStr.Reverse(const S: string): string;
var
  L,R,I: Integer;
  C: Char;
begin
  result := S;
  L := Low(result);
  R := High(result);
  for I := 0 to Length(result) div 2-1 do
  begin
    C         := result[L];
    result[L] := result[R];
    result[R] := C;
    inc(L);
    dec(R);
  end;
end;

class function TStr.GetNumbers(const s: string): String;
type
  TState = (stEmpty, stNum, stSpace);
var
  i,j: Integer;
  State: TState;
begin
  j := 0;
  State := stEmpty;
  for i := 1 to length(s) do
    if (s[i]>='0') and (s[i]<='9') or
      ((s[i]='.') or (s[i]=',')) and (
        (i>1) and (s[i-1]>='0') and (s[i-1]<='9') or
        (i<length(s)) and (s[i+1]>='0') and (s[i+1]<='9')
      )
    then { part of number }
      begin
        if State=stSpace then
          inc(j);
        inc(j);
        State := stNum;
      end
    else { not part of number }
      if State=stNum then
        State := stSpace; { space is required before next number }

  setlength(result, j);
  j := 0;
  State := stEmpty;
  for i := 1 to length(s) do
    if (s[i]>='0') and (s[i]<='9') or
      ((s[i]='.') or (s[i]=',')) and (
        (i>1) and (s[i-1]>='0') and (s[i-1]<='9') or
        (i<length(s)) and (s[i+1]>='0') and (s[i+1]<='9')
      )
    then { part of number }
      begin
        if State=stSpace then
        begin
          inc(j);
          result[j] := ' ';
        end;
        inc(j);
        result[j] := s[i];
        State := stNum;
      end
    else { not part of number }
      if State=stNum then
        State := stSpace; { space is required before next number }
end;

class function TStr.GetPrintable(const S: string; ReplChar: Char = '?'): string;
begin
  result := GetPrintable(PChar(S), Length(S), ReplChar);
end;

class function TStr.GetPrintable(S: PChar; Count: integer; ReplChar: Char = '?'): string;
var
  I: Integer;
begin
  SetLength(Result, Count);
  if Count<=0 then
    Exit;
  System.Move(S^, result[Low(result)], Count*SizeOf(Char));
  for I := Low(result) to High(result) do
    if result[I].IsWhiteSpace then
      result[I] := ' '
    else
    if result[I].IsControl then
      result[I] := ReplChar;
end;

class procedure TStr.InitializeVars;
{$IFNDEF NoCaseMap}
var
  C: Char;
{$ENDIF}
begin
  {$IFNDEF NoCaseMap}
  for C := Low(C) to High(C) do
  begin
    FLoCaseMap[C] := C.ToLower;
    FHiCaseMap[C] := C.ToUpper;
  end;
  {$ENDIF}
end;

class procedure TStr.FinalizeVars;
begin
end;

class function TStr.MakeValidStringLiteral(const s: string): string;
var
  i: Integer;
begin
  result := '';
  i := Low(s);
  while High(s)-i+1>250 do
  begin
    result := result + '''' + System.Copy(s,i,250) + ''' + ';
    inc(i, 250);
  end;
  result := result + '''' + System.Copy(s,i,high(integer)) + '''';
end;

class function TStr.MoveDigitsToEnd(const s: string): String;
var
  i,j: Integer;
begin
  setlength(result, length(s));
  j := 0;
  for i := 1 to length(s) do
    if (s[i]<'0') or (s[i]>'9') then
    begin
      inc(j);
      result[j] := s[i];
    end;
  for i := 1 to length(s) do
    if (s[i]>='0') and (s[i]<='9') then
    begin
      inc(j);
      result[j] := s[i];
    end;
end;

class function TStr.GetDigits(const s: string): String;
var
  i,j: Integer;
begin
  j := 0;
  for i := 1 to length(s) do
    if (s[i]>='0') and (s[i]<='9') then
      inc(j);
  setlength(result, j);
  j := 0;
  for i := 1 to length(s) do
    if (s[i]>='0') and (s[i]<='9') then
    begin
      inc(j);
      result[j] := s[i];
    end;
end;

class function TStr.GetEncoding(CodePage: Cardinal): TEncoding;
begin
  case CodePage of
     1200           : result := TUnicodeEncoding.Create;            { UTF16 little endian }
     1201           : result := TBigEndianUnicodeEncoding.Create;   { UTF16 big endian }
    65000 {CP_UTF7} : result := TUTF7Encoding.Create;               { UTF7 }
    65001 {CP_UTF8} : result := TUTF8Encoding.Create;               { UTF8 }
    12000           : result := TLittleEndianUTF32Encoding.Create;  { UTF32 little endian }
    12001           : result := TBigEndianUTF32Encoding.Create;     { UTF32 big endian }
    else result := TEncoding.GetEncoding(CodePage);
  end;
end;

class function TStr.GetMaxEncodedBytes(const S: string; BufSize: integer; Encoding: TEncoding): TBytes;
var
  M,N,I,J: Integer;
begin
  { usually string should fit the buffer, in such case we process it in most efficient way }
  if Length(S) <= BufSize then
  begin
    result := Encoding.GetBytes(S);
    if Length(result) <= BufSize then
      Exit;
  end;

  { find N=number of chars to be encoded and M=required buffer size (<=BufSize) }
  M := 0;
  N := 0;
  for I := Low(S) to High(S) do
  begin
    J := Encoding.GetByteCount(S[I]);
    if M+J > BufSize then
      Break;
    inc(M, J);
    inc(N);
  end;
  Result := Encoding.GetBytes(S.Substring(0, N));
  Assert(Length(Result) = M);
end;

class function TStr.SubstringDistance(const a,b: String; var Dist: integer):Boolean;
var
  Substr,Str: string;
begin
  if Length(a)<=length(b) then
  begin
    Substr := a;
    Str := b;
  end
  else
  begin
    Substr := b;
    Str := a;
  end;
  Result := TextPos(PChar(Str), PChar(SubStr))<>nil;
  if Result then
    Dist := Length(Str)-Length(SubStr);
end;

class function TStr.SimilarStrings(A, B: String; var dist: integer; AOptions: TSimilarityOptions): boolean;
var
  maxerror: Integer;
begin
  Result := (soMatchSubst in AOptions) and (Pos(AnsiLowerCase(a), AnsiLowerCase(b))>0);
  if Result then
  begin
    dist := length(b)-length(a);
    Exit;
  end;

  { numbers must be the same without any errors (2012<>2011 even in long strings) }
  if (soStrictNumMatching in AOptions) and (GetNumbers(a)<>GetNumbers(b)) or
    (soStrictIntMatching in AOptions) and (GetDigits(a)<>GetDigits(b))
  then
    Exit;

  { ignore numbers, so "Beløp 12.6 NOK" = "Beløp 5 NOK" }
  if soIgnoreNums in AOptions then
  begin
    a := RemoveNumbers(a);
    b := RemoveNumbers(b);
  end
  else
  if soIgnoreInts in AOptions then
  begin
    a := RemoveDigits(a);
    b := RemoveDigits(b);
  end;

  { for longer strings we allow more mistakes }
  case max(length(a), length(b)) of
    0..2: maxerror := 0;
    3..4: maxerror := 1;
    5..9: maxerror := 2;
    10..15: maxerror := 3;
    16..33: maxerror := 4;
    else maxerror := 0;
  end;

  { if length is too different then we do not have to spend time for similar search }
  Dist := Abs(length(a)-length(b));
  if (Dist>maxerror) then
    exit;

  { special case - substring matching (it is faster than calculation of distance) }
  Result := SubstringDistance(a,b, Dist);

  { For very short and very long strings we do not use similar search.
    (for short strings it is useless, for long strings it is time expensive). }
  if not Result and (maxerror>0) then
  begin
    dist := LevensteinDistance(a,b);
    result := dist<=maxerror;
  end;
end;

class function TStr.SameText(const A: string; const B: array of string): Boolean;
var
  I: Integer;
begin
  for I := Low(B) to High(B) do
    if SameText(B[I], A) then
      Exit(True);
  result := False;
end;

class function TStr.SameText(A, B: PChar; Len: integer): Boolean;
var
  I: integer;
begin
  {$IFNDEF NoCaseMap}
  for I := 0 to Len-1 do
    if FLoCaseMap[A[I]] <> FLoCaseMap[B[I]] then
      Exit(False);
  {$ELSE}
  for I := 0 to Len-1 do
    if Char(A[I]).ToLower <> Char(B[I]).ToLower then
      Exit(False);
  {$ENDIF}
  result := True;
end;

class function TStr.LowerCaseChar(C: Char): Char;
begin
  {$IFNDEF NoCaseMap}
  result := FLoCaseMap[C];
  {$ELSE}
  result := C.ToLower;
  {$ENDIF}
end;

class function TStr.UpperCaseChar(C: Char): Char;
begin
  {$IFNDEF NoCaseMap}
  result := FHiCaseMap[C];
  {$ELSE}
  result := C.ToUpper;
  {$ENDIF}
end;

class function TStr.SameTrimText(const A, B: string): Boolean;
begin
  result := SameText(Trim(A), Trim(B));
end;

class function TStr.SimilarStrings(A,B: String; AOptions: TSimilarityOptions = [soStrictIntMatching]): boolean;
var
  Distance: Integer;
begin
  result := SimilarStrings(A,B, Distance, AOptions);
end;

class function TStr.SimilarWordInText(const AWord, AText: String; var dist: integer; AOptions: TSimilarityOptions): boolean;
var
  Tok: TTokLettersOrDigits;
  W: String;
  D: integer;
begin
  Result := False;
  Tok := TTokLettersOrDigits.Create(AText);
  try
    while Tok.Next(W) do
    begin
      if not SimilarStrings(AWord, W, D, AOptions) then
        Continue;
      if not Result then
        Dist := D
      else
        Dist := Min(Dist, D);
      Result := True;
    end;
  finally
    FreeAndNil(Tok);
  end;
end;

class function TStr.Split(const Src: string; MaxStrLen: integer): TArray<string>;
var
  Options: TSplitOptions;
begin
  Options.Init;
  Options.MaxStrLen := MaxStrLen;
  result := Split(Src, Options);
end;

class function TStr.Split(const Src: string; const Options: TSplitOptions): TArray<string>;
var
  Parser: TTokText;
  Tokens: TArr<TCompound<TTokenPos, TTokText.TTextTokenType>>;
  Pair: TCompound<TTokenPos, TTokText.TTextTokenType>;
  Buf: TStringBuffer;
  Res: TArr<string>;
  I,J,N,L: integer;
begin
  if Options.MaxStrLen <= 0 then
  begin
    if Options.AllowWordToExceedMaxLen then
    begin
      SetLength(result, 1);
      result[0] := Src
    end
    else
      SetLength(result, 0);
    Exit;
  end;

  Parser := TTokText.Create(Src);
  try
    Pair := Default(TCompound<TTokenPos, TTokText.TTextTokenType>);
    Tokens.Clear;
    while Parser.Next(Pair.A) do
      if Parser.LastTokenType <> tttWhitespace then
      begin
        Pair.B := Parser.LastTokenType;
        Tokens.Add(Pair);
      end;
  finally
    Sys.FreeAndNil(Parser);
  end;

  Buf.Clear;
  Res.Clear;
  I := 0;
  while I < Tokens.Count do
  begin
    Pair := Tokens[I];

    if Pair.B = tttWord then
    begin

      { number of punctuation chars following the word }
      N := 0;
      L := 0;
      for J := I+1 to Tokens.Count-1 do
        if Tokens[J].B <> tttPunctuation then
          break
        else
        begin
          inc(N);
          inc(L, Tokens[J].A.Len);
        end;

      { if word + punctuation is too long for single line }
      if Pair.A.Len + L > Options.MaxStrLen then
      begin
        if Buf.Size > 0 then
          begin Res.Add(Buf.Text); Buf.Clear; end;
        if Options.AllowWordToExceedMaxLen then
        begin
          for J := I to I+N do
            Buf.Write(Parser[Tokens[J].A]);
          inc(I, N);
        end
        else
        begin
          while Pair.A.Len > Options.MaxStrLen do
          begin
            Res.Add(Src.Substring(Pair.A.Start, Options.MaxStrLen));
            inc(Pair.A.Start, Options.MaxStrLen);
            dec(Pair.A.Len, Options.MaxStrLen);
          end;
          Buf.Write(Src, Pair.A.Start, Pair.A.Len);
        end;
      end
      else
      { if word + punctuation is small enough to fit a line }
      begin
        if Buf.Size + Ifthen(Buf.Size=0,0,1) + Pair.A.Len + N > Options.MaxStrLen then
          begin Res.Add(Buf.Text); Buf.Clear; end;
        if Buf.Size > 0 then
          Buf.Write(' ');
        for J := I to I+N do
        begin
          Pair := Tokens[J];
          Buf.Write(Src, Pair.A.Start, Pair.A.Len);
        end;
        inc(I, N);
      end;
    end
    else
    begin
      { tttPunctuation }
      if Buf.Size + Pair.A.Len > Options.MaxStrLen then
        begin Res.Add(Buf.Text); Buf.Clear; end;
      Buf.Write(Src, Pair.A.Start, Pair.A.Len);
    end;

    inc(I);
  end;

  if Buf.Size > 0 then
    Res.Add(Buf.Text);
  result := Res.ToArray;
end;

class procedure TStr.TextToLines(const Text: string; Dst: TStrings; AClear: boolean = True);
begin
  if AClear then
    Dst.Clear;
  TTokLines.Parse(Text, Dst);
end;

class procedure TStr.TextToLines(const Text: string; out Dst: TArray<string>);
begin
  TTokLines.Parse(Text, Dst);
end;

class procedure TStr.TextToLines(const Text: string; Delim: Char; out Dst: TArray<string>);
var
  T: TTokCharDelimitedLines;
begin
  T := TTokCharDelimitedLines.Create(Text, Delim);
  try
    T.GetTokens(Dst);
  finally
    T.Free;
  end;
end;

class function TStr.FixDecimalSeparator(const Src: string): string;
begin
  Result := Src;
  if FormatSettings.DecimalSeparator<>'.' then
    Result := Result.Replace('.', FormatSettings.DecimalSeparator);
  if FormatSettings.DecimalSeparator<>',' then
    Result := Result.Replace(',', FormatSettings.DecimalSeparator);
end;

class function TStr.ToFloatDef(const Src: string; DefValue: int64): double;
begin
  if not TryToFloat(Src, result) then
    result := DefValue;
end;

class function TStr.ToInteger(const Src: string): int64;
begin
  result := StrToInt64(Src);
end;

class function TStr.ToFloat(const Src: string): double;
begin
  result := StrToFloat(FixDecimalSeparator(Src));
end;

class function TStr.ToBoolean(const Src: string): boolean;
begin
  if not TryToBoolean(Src, Result) then
    raise Exception.Create('Error');
end;

class function TStr.ToBooleanDef(const Src: string; DefValue: boolean): boolean;
begin
  if not TryToBoolean(Src, Result) then
    Result := DefValue;
end;

class function TStr.ToDateTime(const Src: string): TDateTime;
begin
  result := StrToDateTime(Src);
end;

class function TStr.ToDateTimeDef(const Src: string; DefValue: TDateTime): TDateTime;
begin
  if not TryToDateTime(Src, result) then
    result := DefValue;
end;

class function TStr.ToIntegerDef(const Src: string; DefValue: int64): int64;
begin
  if not TryToInteger(Src, result) then
    result := DefValue;
end;

class function TStr.TryToFloat(const Src: string; var Value: double): boolean;
begin
  result := TryStrToFloat(FixDecimalSeparator(Src), Value);
end;

class function TStr.TryToFloat(const Src: string; var Value: single): boolean;
begin
  result := TryStrToFloat(FixDecimalSeparator(Src), Value);
end;

class function TStr.TryToBoolean(const Src: string; var Value: boolean): boolean;
var
  S: string;
begin
  S := Trim(Src).ToLower;
  if (S='1') or (S='y') or (S='yes') or (S='j') or (S='ja') or (s='t') or (S='true') then
  begin
    Value := True;
    Result := True;
  end
  else
  if (S='0') or (S='n') or (S='no') or (S='nei') or (s='f') or (S='false') then
  begin
    Value := False;
    Result := True;
  end
  else
    result := False;
end;

class function TStr.TryToDateTime(const Src: string; var Value: TDateTime): boolean;
begin
  result := TryStrToDateTime(Src, Value);
end;

class function TStr.TryToFloat(const Src: string; var Value: extended): boolean;
begin
  result := TryStrToFloat(FixDecimalSeparator(Src), Value);
end;

class function TStr.TryToInteger(const Src: string; var Value: int64): boolean;
begin
  result := TryStrToInt64(Src, Value);
end;

class function TStr.TryToInteger(const Src: string; var Value: integer): boolean;
begin
  result := TryStrToInt(Src, Value);
end;

class function TStr.TruncSpaces(const Src: string; TrimRes: boolean = True): string;
type
  TState = (sStart, sWord, sSpace);
var
  I,WordStart: Integer;
  State: TState;
begin
  result := '';
  State := sStart;
  WordStart := 0;
  for I := 0 to Length(Src)-1 do
    if Src.Chars[I].IsWhiteSpace or Src.Chars[I].IsControl then
    begin
      case State of
        sStart:
          begin
            result := result + ' ';
            State := sSpace;
          end;
        sWord:
          begin
            result := result + Src.Substring(WordStart, I-WordStart) + ' ';
            State := sSpace;
          end;
        sSpace:;
      end;
    end
    else
      if State<>sWord then
      begin
        WordStart := I;
        State := sWord;
      end;
  if State=sWord then
    result := result + Src.Substring(WordStart, Length(Src)-WordStart);
  if TrimRes then
    result := Trim(result);
end;

class function TStr.TruncToWord(const Src: string; MaxCharLen: integer; AddDots: boolean = True): string;
var
  I: Integer;
begin
  result := Trim(Src);
  if (MaxCharLen < Length(Src)) and (Src <> '') then
  begin
    result := '';
    for I := MaxCharLen downto 0 do
      if (Src.Chars[I] <= ' ') or (Src.Chars[I]={Non-breaking space}#160) then
      begin
        result := Src.Substring(0,I);
        break;
      end;
    if AddDots then
      result := result + '...';
  end;
end;

class function TStr.LevensteinDistance(s, t: string): integer;
const
  cuthalf = 100;
var
  i, j, m, n: integer;
  cost: integer;
  flip: boolean;
  buf: array[0..((cuthalf * 2) - 1)] of integer;
begin
  s := copy(s, 1, cuthalf - 1);
  t := copy(t, 1, cuthalf - 1);
  m := length(s);
  n := length(t);
  if m = 0 then
    Result := n
  else if n = 0 then
    Result := m
  else
  begin
    flip := false;
    for i := 0 to n do
      buf[i] := i;
    for i := 1 to m do
    begin
      if flip then
        buf[0] := i
      else
        buf[cuthalf] := i;
      for j := 1 to n do
      begin
        if s[i] = t[j] then
          cost := 0
        else
          cost := 1;
        if flip then
          buf[j] := TFun.min((buf[cuthalf + j] + 1),
            (buf[j - 1] + 1),
            (buf[cuthalf + j - 1] + cost))
        else
          buf[cuthalf + j] := TFun.min((buf[j] + 1),
            (buf[cuthalf + j - 1] + 1),
            (buf[j - 1] + cost));
      end;
      flip := not flip;
    end;
    if flip then
      Result := buf[cuthalf + n]
    else
      Result := buf[n];
  end;
end;

class function TStr.Load(Src: TStream; Encoding: TEncoding): string;
var
  B: TArray<System.Byte>;
begin
  Src.Position := 0;
  SetLength(B, Src.Size);
  Src.ReadBuffer(B, Length(B));
  if Encoding <> nil then
    Result := Encoding.GetString(B)
  else
    try
      TEncoding.GetBufferEncoding(B, Encoding, Encoding.UTF8);
      Result := Encoding.GetString(B);
    except
      Result := Encoding.ANSI.GetString(B);
    end;
end;

class function TStr.Load(const FileName: string; Encoding: TEncoding): string;
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    result := Load(F, Encoding);
  finally
    F.Free;
  end;
end;

class function TStr.LoadFileToArray(const Filename: string; Encoding: TEncoding = nil): TArray<string>;
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    S.LoadFromFile(Filename);
    S.Position := 0;
    result := LoadStreamToArray(S, Encoding);
  finally
    Sys.FreeAndNil(S);
  end;
end;

class function TStr.LoadStreamToArray(Src: TStream; Encoding: TEncoding = nil): TArray<string>;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    if Encoding = nil then
      Encoding := TEncoding.UTF8;
    L.LoadFromStream(Src, Encoding);
    Result := L.ToStringArray;
  finally
    Sys.FreeAndNil(L);
  end;
end;

class procedure TStr.SaveArrayToFile(const Src: TArray<string>; const Filename: string; Encoding: TEncoding);
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    SaveArrayToStream(Src, S, Encoding);
    S.SaveToFile(Filename);
  finally
    Sys.FreeAndNil(S);
  end;
end;

class procedure TStr.SaveArrayToStream(const Src: TArray<string>; const Dst: TStream; Encoding: TEncoding);
var
  L: TStringList;
  I: Integer;
begin
  L := TStringList.Create;
  try
    for I := Low(Src) to High(Src) do
      L.Add(Src[I]);
    if Encoding = nil then
      Encoding := TEncoding.UTF8;
    L.SaveToStream(Dst, Encoding);
  finally
    Sys.FreeAndNil(L);
  end;
end;

class procedure TStr.Save(Dst: TStream; const S: string; Encoding: TEncoding);
var
  B: TArray<System.Byte>;
begin
  if Encoding=nil then
    Encoding := TEncoding.UTF8;
  B := Encoding.GetBytes(S);
  Dst.Write(B, Length(B));
end;

class procedure TStr.Save(const FileName: string; const S: string; Encoding: TEncoding);
var
  F: TFileStream;
begin
  F := TFileStream.Create(FileName, fmCreate);
  try
    Save(F, S, Encoding);
  finally
    F.Free;
  end;
end;

class function TStr.TextDistance(const a, b: string; StrDistType: TTextDistance): integer;
begin
  case StrDistType of
    tdLevenstein: result := LevensteinDistance(a,b);
    else raise Exception.Create('Error');
  end;
end;

class function TStr.TextPosition(const ASubStr, AText: string; AOffset: integer): Integer;
begin
  Result := AText.ToUpper.IndexOf(ASubStr.ToUpper, AOffset);
end;

class function TStr.Contains(const ASubStr, AText: string): boolean;
begin
  result := TextPosition(ASubStr, AText) >= 0;
end;

class function TStr.IsValidUtf8(const Text: TArray<Byte>): boolean;
begin
  result := IsValidUtf8(Text, Length(Text), 0, False);
end;

class function TStr.IsValidUtf8(const Text: TArray<Byte>; Count,TextPos: integer; TextIsFragment: boolean = True): boolean;
const
  b2  = 128 + 64 +  0;
  b2m = 128 + 64 + 32;
  b3  = 128 + 64 + 32 +  0;
  b3m = 128 + 64 + 32 + 16;
  b4  = 128 + 64 + 32 + 16 + 0;
  b4m = 128 + 64 + 32 + 16 + 8;
  b5  = 128 + 64 + 32 + 16 + 8 + 0;
  b5m = 128 + 64 + 32 + 16 + 8 + 4;
  b6  = 128 + 64 + 32 + 16 + 8 + 4 + 0;
  b6m = 128 + 64 + 32 + 16 + 8 + 4 + 2;
  v   = 128 +  0;
  vm  = 128 + 64;
begin
  Result := False;

  { we will kep number of remain bytes in Count }
  Dec(Count, TextPos);

  { skip BOM }
  if (Count >= 3) and (Text[TextPos]=$EF) and (Text[TextPos+1]=$BB) and (Text[TextPos+2]=$BF) then
  begin
    inc(TextPos, 3);
    dec(Count, 3);
  end;

  { we can check faster when we know that all 6 bytes are here }
  while Count >= 6 do
    { 1 byte }
    if Text[TextPos] < 128 then
    begin
      inc(TextPos);
      dec(Count);
    end
    else
    { 2 bytes }
    if (Text[TextPos] and b2m = b2) then
      if Text[TextPos+1] and vm <> v then
        Exit
      else
      begin
        inc(TextPos, 2);
        dec(Count, 2);
      end
    else
    { 3 bytes }
    if (Text[TextPos] and b3m = b3) then
      if (Text[TextPos+1] and vm <> v) or (Text[TextPos+2] and vm <> v) then
        Exit
      else
      begin
        inc(TextPos, 3);
        dec(Count, 3);
      end
    else
    { 4 bytes }
    if (Text[TextPos] and b4m = b4) then
      if (Text[TextPos+1] and vm <> v) or
        (Text[TextPos+2] and vm <> v) or
        (Text[TextPos+3] and vm <> v)
      then
        Exit
      else
      begin
        inc(TextPos, 4);
        dec(Count, 4);
      end
    else
    { 5 bytes }
    if (Text[TextPos] and b5m = b5) then
      if (Text[TextPos+1] and vm <> v) or
        (Text[TextPos+2] and vm <> v) or
        (Text[TextPos+3] and vm <> v) or
        (Text[TextPos+4] and vm <> v)
      then
        Exit
      else
      begin
        inc(TextPos, 5);
        dec(Count, 5);
      end
    else
    { 6 bytes }
    if (Text[TextPos] and b6m = b6) then
      if (Text[TextPos+1] and vm <> v) or
        (Text[TextPos+2] and vm <> v) or
        (Text[TextPos+3] and vm <> v) or
        (Text[TextPos+4] and vm <> v) or
        (Text[TextPos+5] and vm <> v)
      then
        Exit
      else
      begin
        inc(TextPos, 6);
        dec(Count, 6);
      end
    else
      Exit;

  { we have 0..5 bytes left }
  while Count > 0 do
    { 1 byte }
    if Text[TextPos] < 128 then
    begin
      inc(TextPos);
      dec(Count);
    end
    else
    { 2 bytes }
    if (Text[TextPos] and b2m = b2) then
      if Count < 2 then
        Exit(TextIsFragment)
      else
        if Text[TextPos+1] and vm <> v then
          Exit
        else
        begin
          inc(TextPos, 2);
          dec(Count, 2);
        end
    else
    { 3 bytes }
    if (Text[TextPos] and b3m = b3) then
      if Count < 3 then
        Exit(TextIsFragment)
      else
        if (Text[TextPos+1] and vm <> v) or
          (Text[TextPos+2] and vm <> v)
        then
          Exit
        else
        begin
          inc(TextPos, 3);
          dec(Count, 3);
        end
    else
    { 4 bytes }
    if (Text[TextPos] and b4m = b4) then
      if Count < 4 then
        Exit(TextIsFragment)
      else
        if (Text[TextPos+1] and vm <> v) or
          (Text[TextPos+2] and vm <> v) or
          (Text[TextPos+3] and vm <> v)
        then
          Exit
        else
        begin
          inc(TextPos, 4);
          dec(Count, 4);
        end
    else
    { 5 bytes }
    if (Text[TextPos] and b5m = b5) then
      if Count < 5 then
        Exit(TextIsFragment)
      else
        if (Text[TextPos+1] and vm <> v) or
          (Text[TextPos+2] and vm <> v) or
          (Text[TextPos+3] and vm <> v) or
          (Text[TextPos+4] and vm <> v)
        then
          Exit
        else
        begin
          inc(TextPos, 5);
          dec(Count, 5);
        end
    else
    { 6 bytes }
    if (Text[TextPos] and b6m = b6) then
      if Count < 6 then
        Exit(TextIsFragment)
      else
        if (Text[TextPos+1] and vm <> v) or
          (Text[TextPos+2] and vm <> v) or
          (Text[TextPos+3] and vm <> v) or
          (Text[TextPos+4] and vm <> v) or
          (Text[TextPos+5] and vm <> v)
        then
          Exit
        else
        begin
          inc(TextPos, 6);
          dec(Count, 6);
        end
    else
      Exit; { wrong sequence }

  { No wrong sequences detected }
  Result := True;

end;

class function TStr.DetectEncoding(const Text: TArray<Byte>; Count: integer;
  out TextStartPos: integer; TextIsFragment: boolean): TTextEncoding;
type
  TRec = record
    z: array[0..3] of byte;
    e: TTextEncoding;
  end;

const
  Encodings: array[0..4] of TRec = (
    (z:(1,1,1,1); e: teUTF8),
    (z:(1,0,1,0); e: teUTF16LE),
    (z:(0,1,0,1); e: teUTF16BE),
    (z:(1,0,0,0); e: teUTF32LE),
    (z:(0,0,0,1); e: teUTF32BE)
  );

var
  i: integer;
begin

  TextStartPos := 0;

  if Count < 4 then
    if not IsValidUtf8(Text, Count, TextStartPos, TextIsFragment) then
      Exit(teAnsi)
    else
    begin
      if (Count >= 3) and (Text[TextStartPos]=$EF) and (Text[TextStartPos+1]=$BB) and (Text[TextStartPos+2]=$BF) then
        inc(TextStartPos, 3);
      Exit(teUTF8)
    end;

  { check byte order mark (BOM) }
  if (Text[TextStartPos]=$EF) and (Text[TextStartPos+1]=$BB) and (Text[TextStartPos+2]=$BF) then
  begin
    if not IsValidUtf8(Text, Count, TextStartPos, TextIsFragment) then
      Exit(teAnsi);
    inc(TextStartPos, 3);
    Exit(teUTF8);
  end;
  if (Text[TextStartPos]=$FF) and (Text[TextStartPos+1]=$FE) and (Text[TextStartPos+2]=0) and (Text[TextStartPos+3]=0) then
  begin
    inc(TextStartPos, 4);
    Exit(teUTF32LE);
  end;
  if (Text[TextStartPos]=0) and (Text[TextStartPos+1]=0) and (Text[TextStartPos+2]=$FE) and (Text[TextStartPos+3]=$FF) then
  begin
    inc(TextStartPos, 4);
    Exit(teUTF32BE);
  end;
  if (Text[TextStartPos]=$FF) and (Text[TextStartPos+1]=$FE) then
  begin
    inc(TextStartPos, 2);
    Exit(teUTF16LE);
  end;
  if (Text[TextStartPos]=$FE) and (Text[TextStartPos+1]=$FF) then
  begin
    inc(TextStartPos, 2);
    Exit(teUTF16BE);
  end;

  (*
    check first characters (according to RFC they must be ASCII)
    00 00 00 xx  UTF-32BE
    00 xx 00 xx  UTF-16BE
    xx 00 00 00  UTF-32LE
    xx 00 xx 00  UTF-16LE
    xx xx xx xx  UTF-8
  *)
  for i := low(Encodings) to high(Encodings) do
    with Encodings[i] do
      if
        ( (Z[0]=0) = (Text[TextStartPos]=0) ) and
        ( (Z[1]=0) = (Text[TextStartPos+1]=0) ) and
        ( (Z[2]=0) = (Text[TextStartPos+2]=0) ) and
        ( (Z[3]=0) = (Text[TextStartPos+3]=0) )
      then
        if E <> teUTF8 then
          Exit(E)
        else
          if IsValidUtf8(Text, Count, TextStartPos, TextIsFragment) then
            Exit(teUTF8)
          else
            Exit(teANSI);

  result := teANSI;
end;

class function TStr.Random(ALen: integer; AFrom, ATo: Char): string;
var
  i: Integer;
begin
  setlength(result, ALen);
  for i := Low(result) to High(result) do
    result[i] := Char(
      Integer(AFrom) +
      System.Random(Integer(ATo)-Integer(AFrom)+1)
    );
end;

class function TStr.IsPossibleEncoding(const AText: TArray<byte>; ACodePage: Cardinal): boolean;
var
  TextEncoding : TTextEncoding;
  TextStartPos: Integer;
begin

  { any encoding is allowed for empty src }
  if Length(AText) = 0 then
    Exit(True);

  TextEncoding := DetectEncoding(AText, Length(AText), TextStartPos, False);
  case TextEncoding of
    teUnknown,teAnsi:
      { it can't be UTF family }
      result := not TArrayUtils.Contains<cardinal>(ACodePage, [1200,1201,12000,12001,65001]);
    teUTF8:
      { it can't be UTF16/UTF32, but maybe it is UTF8 or single byte encoding }
      result := not TArrayUtils.Contains<cardinal>(ACodePage, [1200,1201,12000,12001]);
    teUTF16LE:
      result := ACodePage = 1200;
    teUTF16BE:
      result := ACodePage = 1201;
    teUTF32LE:
      result := ACodePage = 12000;
    teUTF32BE:
      result := ACodePage = 12001;
    else Result := True; { we don't know }
  end;
end;

class function TStr.DetectCodepage(const AText: TArray<byte>; out Encoding: TEncoding): boolean;
var
  CodePage: cardinal;
begin
  result := DetectCodepage(AText, CodePage);
  if result then
    Encoding := TStr.GetEncoding(CodePage);
end;

class function TStr.DetectCodepage(const AText: TArray<byte>; Count: integer; out CodePage: Cardinal): boolean;
var
  TextEncoding : TTextEncoding;
  TextStartPos: Integer;
begin
  TextEncoding := DetectEncoding(AText, Length(AText), TextStartPos, False);
  result := TextEncoding <> teUnknown;
  if result then
    case TextEncoding of
      teAnsi    : CodePage := TEncoding.ANSI.CodePage;
      teUTF8    : CodePage := 65001;
      teUTF16LE : CodePage := 1200;
      teUTF16BE : CodePage := 1201;
      teUTF32LE : CodePage := 12000;
      teUTF32BE : CodePage := 12001;
      else result := False;
    end;
end;

class function TStr.DetectCodepage(const AText: TArray<byte>; out CodePage: Cardinal): boolean;
begin
  result := DetectCodepage(AText, Length(AText), CodePage);
end;

class function TStr.Random: string;
begin
  result := TStr.Random(System.Random(20));
end;

class function TStr.EncodeStringLiteral(const Value: string): string;
var
  Buf: TStringBuffer;
begin
  Buf.Clear;
  EncodeStringLiteral(Value, Buf);
  Result := Buf.Text;
end;

class procedure TStr.EncodeStringLiteral(const Value: string; var Dst: TStringBuffer);
var
  I: integer;
begin
  for I := Low(Value) to High(Value) do
    if Value[I]=''''
      then Dst.Write('''''')
      else Dst.Write(Value[I]);
end;

class function TStr.EncodeStringLiterals(const Values: TArray<string>): string;
begin

end;

class function TStr.DecodeStringLiterals(const Values: string): TArray<string>;
begin

end;

{ TTokenPos }

class operator TTokenPos.Add(const A, B: TTokenPos): TTokenPos;
begin
  result.SetPos(
    Min(A.Start, B.Start),
    Max(A.Start+A.Len, B.Start+B.Len) - result.Start
  );
end;

function TTokenPos.BytesToChars: TTokenPos;
begin
  Assert((Start mod SizeOf(Char)=0) and (Len mod SizeOf(Char)=0));
  result.SetPos(Start div SizeOf(Char), Len div SizeOf(Char));
end;

constructor TTokenPos.Create(AStart, ALen: integer);
begin
  {$IF SizeOf(TTokenPos)<>SizeOf(Start)+SizeOf(Len)}
    Self := Default(TTokenPos);
  {$ENDIF}
  Start := AStart;
  Len := ALen;
end;

procedure TTokenPos.SetPos(AStart, ALen: integer);
begin
  {$IF SizeOf(TTokenPos)<>SizeOf(Start)+SizeOf(Len)}
    Self := Default(TTokenPos);
  {$ENDIF}
  Start := AStart;
  Len := ALen;
end;

class operator TTokenPos.In(const A: integer; const B: TTokenPos): Boolean;
begin
  result := (A >= B.Start) and (A < B.Start + B.Len);
end;

class operator TTokenPos.Subtract(const A, B: TTokenPos): TTokenPos;
begin
  if A.Start>B.Start then
    result := B - A
  else
    result.SetPos(A.Start + A.Len, B.Start-result.Start);
end;

{ TTokLines }

procedure TTokLines.TokenizerCreated;
begin
  inherited;
  FOmmitEmptyTokens := False;
end;

{ Find strings separated by one of possible end-of-line marker. Check
  section "Unicode" for details here: https://en.wikipedia.org/wiki/Newline
    LF:    Line Feed, U+000A
    VT:    Vertical Tab, U+000B
    FF:    Form Feed, U+000C
    CR:    Carriage Return, U+000D
    CR+LF: CR (U+000D) followed by LF (U+000A)
    NEL:   Next Line, U+0085
    LS:    Line Separator, U+2028
    PS:    Paragraph Separator, U+2029
  Additionally we support:
    LF+CR  U+000A + U+000D }
class function TTokLines.GetLine(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
const
  LF  : Char = #$000A; { Line Feed }
  VT  : Char = #$000B; { Vertical Tab }
  FF  : Char = #$000C; { Form Feed }
  CR  : Char = #$000D; { Carriage Return }
  NEL : Char = #$0085; { Next Line }
  LS  : Char = #$2028; { Line Separator }
  PS  : Char = #$2029; { Paragraph Separator }
  { Multichar markers of new line: LF+CR and CR+LF }
var
  N: Integer;
begin
  Result := Len > 0;
  if not result then
    Exit;

  { empty string terminated by EndOfLine }
  if (Text^>=LF) and (Text^<=CR) or (Text^=NEL) or (Text^=LS) or (Text^=PS) then
  begin
    Res.DelimitersPrefix := 0;
    Res.Len              := 0;
    if (Len>1) and ((Text[0]=LF) and (Text[1]=CR) or (Text[0]=CR) and (Text[1]=LF)) then
      Res.DelimitersSuffix := 2
    else
      Res.DelimitersSuffix := 1;
    Exit;
  end;

  { not empty string terminated by EndOfLine or EndOfFile }
  N := 0;
  while (Len > 0) and not ((Text^>=LF) and (Text^<=CR) or (Text^=NEL) or (Text^=LS) or (Text^=PS)) do
  begin
    Inc(Text);
    Dec(Len);
    inc(N);
  end;
  Res.DelimitersPrefix := 0;
  Res.Len              := N;
  if Len <= 0 then
    Res.DelimitersSuffix := 0
  else
    if (Len>1) and ((Text[0]=LF) and (Text[1]=CR) or (Text[0]=CR) and (Text[1]=LF)) then
      Res.DelimitersSuffix := 2
    else
      Res.DelimitersSuffix := 1;
end;

class function TTokLines.GetLineLength(const Text: PChar; Len: integer; IncludeEOL: boolean): Integer;
var
  Token: TTokenInfo;
begin
  if not GetLine(Text, Len, Token) then
    result := 0
  else
    if IncludeEOL then
      result := Token.DelimitersPrefix + Token.Len + Token.DelimitersSuffix
    else
      result := Token.Len;
end;

function TTokLines.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := GetLine(Text, Len, Res);
end;

(* // implementation where "#13#10" is two empty strings (very similar to NotePad++), seem to be not correct
function TTokLines.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
const
  LF  : Char = #$000A; { Line Feed }
  VT  : Char = #$000B; { Vertical Tab }
  FF  : Char = #$000C; { Form Feed }
  CR  : Char = #$000D; { Carriage Return }
  NEL : Char = #$0085; { Next Line }
  LS  : Char = #$2028; { Line Separator }
  PS  : Char = #$2029; { Paragraph Separator }
  { Multichar markers of new line: LF+CR and CR+LF }
var
  D,T: Integer;
begin
  if Len <= 0 then
    Exit(False);
  D := 0;
  if (Text^>=LF) and (Text^<=CR) or (Text^=NEL) or (Text^=LS) or (Text^=PS) then
  begin
    if (Len>1) and ((Text[0]=LF) and (Text[1]=CR) or (Text[0]=CR) and (Text[1]=LF)) then
    begin
      inc(Text, 2);
      dec(Len, 2);
      inc(D, 2);
    end
    else
    begin
      inc(Text);
      dec(Len);
      inc(D);
    end;
  end;
  T := 0;
  if (D=0) or FSkipLineFeed then
  begin
    while (Len > 0) and not ((Text^>=LF) and (Text^<=CR) or (Text^=NEL) or (Text^=LS) or (Text^=PS)) do
    begin
      Inc(Text);
      Dec(Len);
      inc(T);
    end;
    FSkipLineFeed := Len>0;
  end;
  Res.Delimiters := D;
  Res.Len        := T;
  result         := Res.Delimiters+Res.Len>0;
end; *)

{ TTokText }

function TTokText.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  T: Integer;
begin
  T := 0;
  if Len > 0 then
  begin
    if Text[0].IsWhiteSpace then
    begin
      repeat
        Inc(Text);
        Dec(Len);
        inc(T);
      until (Len=0) or not Text[0].IsWhiteSpace;
      FLastTokenType := tttWhitespace;
    end
    else
    if Text[0].IsPunctuation then
    begin
      repeat
        Inc(Text);
        Dec(Len);
        inc(T);
      until (Len=0) or not Text[0].IsPunctuation;
      FLastTokenType := tttPunctuation;
    end
    else
    begin
      repeat
        Inc(Text);
        Dec(Len);
        inc(T);
      until (Len=0) or Text[0].IsPunctuation or Text[0].IsWhiteSpace;
      FLastTokenType := tttWord;
    end;
  end;
  result := T > 0;
  if result then
  begin
    Res.DelimitersPrefix := 0;
    Res.Len := T;
  end;
end;

{ TTokDelegated }

constructor TTokDelegated.Create(AText: PChar; ATextLen: integer; AProc: TDelegatedTokenizer);
begin
  inherited Create(AText, ATextLen);
  FProc := AProc;
end;

constructor TTokDelegated.Create(const AText: string; AProc: TDelegatedTokenizer);
begin
  inherited Create(AText);
  FProc := AProc;
end;

function TTokDelegated.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := FProc(Text, Len, Res);
end;

{ TTokNot }

constructor TTokNot.Create(ATokenizer: TTokCustom; AOwnsInnerTokenizer: boolean);
begin
  inherited Create;
  FTokenizer := ATokenizer;
  FOwnsInnerTokenizer := AOwnsInnerTokenizer;
end;

destructor TTokNot.Destroy;
begin
  FreeAndNil(FTokenizer);
  inherited;
end;

procedure TTokNot.SetText(AText: PChar; ATextLen: integer);
begin
  FTokenizer.SetText(AText, ATextLen);
end;

function TTokNot.GetText: PChar;
begin
  result := FTokenizer.GetText;
end;

function TTokNot.GetTextLen: integer;
begin
  result := FTokenizer.GetTextLen;
end;

procedure TTokNot.SetPosition(const AValue: integer);
begin
  FTokenizer.SetPosition(AValue);
end;

function TTokNot.GetAsString: string;
begin
  result := FTokenizer.AsString;
end;

function TTokNot.GetPosition: integer;
begin
  result := FTokenizer.GetPosition;
end;

procedure TTokNot.ResetTokenizer;
begin
  FTokenizer.ResetTokenizer;
  FFinished := False;
  FValueLen := 0;
end;

procedure TTokNot.SaveTokenizerPos(W: TWriter);
begin
  FTokenizer.SaveTokenizerPos(W);
end;

procedure TTokNot.LoadTokenizerPos(R: TReader);
begin
  FTokenizer.LoadTokenizerPos(R);
end;

function TTokNot.NextToken(out AToken: TTokenInfo): Boolean;
var
  T: TTokenInfo;
  S: integer;
begin
  if FFinished then
    Exit(False);

  S := FTokenizer.Position;
  repeat
    if not FTokenizer.NextToken(T) then
    begin
      FFinished := True;
      result    := FValueLen>0;
      if result then
      begin
        AToken.Start            := FValueStart;
        AToken.DelimitersPrefix := 0;
        AToken.Len              := FValueLen;
        AToken.DelimitersSuffix := 0;
      end;
    end
    else
    begin
      AToken.DelimitersPrefix := 0;
      if FValueLen>0 then
      begin
        AToken.Start := FValueStart;
        AToken.Len   := FValueLen + T.DelimitersPrefix;
      end
      else
      begin
        AToken.Start := T.Start;
        AToken.Len   := T.DelimitersPrefix;
      end;
      AToken.DelimitersSuffix := T.Len;
      FValueLen               := T.DelimitersSuffix;
      if FValueLen>0 then
        FValueStart := AToken.Start + AToken.DelimitersPrefix + AToken.Len;
      result := AToken.Len>0;
    end;
  until result or FFinished;

  { if we skiped some empty values, then AToken.Start can be greater than S }
  if result and (AToken.Start > S) then
  begin
    Inc(AToken.DelimitersPrefix, AToken.Start - S);
    AToken.Start := S;
  end;
end;

{ TTokCustom }

constructor TTokCustom.Create;
begin
  TokenizerCreated;
end;

constructor TTokCustom.Create(AText: PChar; ATextLen: integer);
begin
  TokenizerCreated;
end;

constructor TTokCustom.Create(const AText: string);
begin
  TokenizerCreated;
end;

procedure TTokCustom.TokenizerCreated;
begin
  FOmmitEmptyTokens := True;
end;

function TTokCustom.GetAsString: string;
begin
  SetLength(result, TextLen);
  if Length(result)>0 then
    System.Move(Text^, result[Low(result)], length(result)*SizeOf(char));
end;

function TTokCustom.GetState: TTokenizerState;
var
  W: TWriter;
begin
  result.State := TInterfacedObject<TMemoryStream>.Create(TMemoryStream.Create);
  W := TWriter.Create(result.State.Data, 256);
  try
    SaveTokenizerPos(W);
  finally
    W.Free;
  end;
end;

procedure TTokCustom.SetState(AState: TTokenizerState);
var
  R: TReader;
begin
  AState.State.Data.Position := 0;
  R := TReader.Create(AState.State.Data, 256);
  try
    LoadTokenizerPos(R);
  finally
    R.Free;
  end;
end;

function TTokCustom.GetTokenAsString(const APos: TTokenPos): string;
var
  L: Integer;
begin
  L := Max(0, Min(APos.Len, GetTextLen-APos.Start));
  SetLength(result, L);
  if L>0 then
    System.Move(Text[APos.Start], Result[Low(Result)], L*SizeOf(Char));
end;

function TTokCustom.GetTokenCount: integer;
var
  P: Integer;
  T: TTokenPos;
begin
  Result := 0;
  P := Position;
  Reset;
  while Next(T) do
    Inc(Result);
  Position := P;
end;

procedure TTokCustom.GetTokens(var ADst: TArray<TTokenPos>);
var
  I: Integer;
begin
  Reset;
  SetLength(ADst, TokenCount);
  Reset;
  for I := 0 to High(ADst) do
    Next(ADst[I]);
end;

procedure TTokCustom.GetTokens(var ADst: TArray<String>);
var
  I: Integer;
begin
  Reset;
  SetLength(ADst, TokenCount);
  Reset;
  for I := 0 to High(ADst) do
    Next(ADst[I]);
end;

procedure TTokCustom.GetTokens(ADst: TStrings);
var
  S: string;
begin
  Reset;
  while Next(S) do
    ADst.Add(S);
end;

procedure TTokCustom.SaveTokenizerPos(W: TWriter);
begin
  {$IFDEF DEBUG}
  W.WriteString(ClassName);
  {$ENDIF}
  W.WriteInteger(1); { state format version }
end;

procedure TTokCustom.LoadTokenizerPos(R: TReader);
begin
  {$IFDEF DEBUG}
  if not AnsiSameText(R.ReadString, ClassName) then
    raise Exception.Create('Error');
  {$ENDIF}
  if (R.ReadInteger<>1) then
    raise Exception.Create('Error');
end;

procedure TTokCustom.Reset;
begin
  ResetTokenizer;
end;

procedure TTokCustom.Reset(ANewText: PChar; ATextLen: integer);
begin
  SetText(ANewText, ATextLen);
  ResetTokenizer;
end;

function TTokCustom.Next(out AToken: TTokenPos): Boolean;
var
  TokenInfo: TTokenInfo;
begin
  if not FOmmitEmptyTokens then
    result := NextToken(TokenInfo)
  else
    repeat
      result := NextToken(TokenInfo);
    until not result or (TokenInfo.Len>0);
  if result then
  begin
    AToken.Start := TokenInfo.Start + TokenInfo.DelimitersPrefix;
    AToken.Len   := TokenInfo.Len;
  end;
end;

function TTokCustom.Next(out AToken: String): Boolean;
var
  TokenPos: TTokenPos;
begin
  result := Next(TokenPos);
  if result then
    AToken := Tokens[TokenPos];
end;

function TTokCustom.Next: String;
var
  TokenPos: TTokenPos;
begin
  if Next(TokenPos) then
    result := Tokens[TokenPos]
  else
    result := '';
end;

class procedure TTokCustom.Parse(const AText: string; var ADst: TArray<TTokenPos>);
var
  T: TTokCustom;
begin
  T := Self.Create;
  try
    T.SetText(PChar(AText), Length(AText));
    T.GetTokens(ADst);
  finally
    T.Free;
  end;
end;

class procedure TTokCustom.Parse(const AText: string; var ADst: TArray<String>);
var
  T: TTokCustom;
begin
  T := Self.Create;
  try
    T.SetText(PChar(AText), Length(AText));
    T.GetTokens(ADst);
  finally
    T.Free;
  end;
end;

class procedure TTokCustom.Parse(const AText: string; ADst: TStrings);
var
  T: TTokCustom;
begin
  T := Self.Create;
  try
    T.SetText(PChar(AText), Length(AText));
    T.GetTokens(ADst);
  finally
    T.Free;
  end;
end;

class procedure TTokCustom.Parse(const AText, ADelimiter: string; var ADst: string);
var
  T: TTokCustom;
  S: string;
begin
  T := Self.Create;
  try
    T.SetText(PChar(AText), Length(AText));
    if not T.Next(ADst) then
      ADst := ''
    else
      while T.Next(S) do
        ADst := ADst + ADelimiter + S;
  finally
    T.Free;
  end;
end;

class function TTokCustom.Parse(const AText: string): string;
begin
  Parse(AText, '', result);
end;

{ TTokCompound }

constructor TTokCompound.Create(const ATokenizers: array of TTokCustom);
var
  i: Integer;
begin
  inherited Create;
  FTokenizers := TObjectList<TTokCustom>.Create;
  FTokenizers.Capacity := Length(ATokenizers);
  for i := 0 to High(ATokenizers) do
    FTokenizers.Add(ATokenizers[i]);
  SetLength(FOffsets, Length(ATokenizers));
end;

destructor TTokCompound.Destroy;
begin
  FreeAndNil(FTokenizers);
  inherited;
end;

procedure TTokCompound.SetText(AText: PChar; ATextLen: integer);
begin
  First.SetText(AText, ATextLen);
end;

function TTokCompound.GetText: PChar;
begin
  result := First.Text;
end;

function TTokCompound.GetTextLen: integer;
begin
  result := First.TextLen;
end;

procedure TTokCompound.SetPosition(const AValue: integer);
begin
  { compound tokenizer can not change position randomly }
  raise Exception.Create('invalid op');
end;

function TTokCompound.GetPosition: integer;
var
  i: Integer;
begin
  i := TokenizersCount-1;
  result := FTokenizers[i].Position + FOffsets[i];
end;

procedure TTokCompound.ResetTokenizer;
begin
  inherited;
  First.Reset;
  FCurrentTokenizer := 0;
end;

procedure TTokCompound.SaveTokenizerPos(W: TWriter);
var
  i: Integer;
begin
  inherited;
  W.WriteInteger(1); { state format version }
  W.WriteInteger(FTokenizers.Count);
  W.WriteInteger(FCurrentTokenizer);
  for i := 0 to FTokenizers.Count-1 do
    FTokenizers[i].SaveTokenizerPos(W);
end;

procedure TTokCompound.LoadTokenizerPos(R: TReader);
var
  i: Integer;
begin
  inherited;
  if R.ReadInteger<>1 then
    raise Exception.Create('Error'); { protocol version - must be 1 }
  if R.ReadInteger<>FTokenizers.Count then
    raise Exception.Create('Error'); { stored with another set of tokenizers }
  FCurrentTokenizer := R.ReadInteger;
  if (FCurrentTokenizer<0) or (FCurrentTokenizer>=FTokenizers.Count) then
    raise Exception.Create('Error'); { bad index }
  for i := 0 to FTokenizers.Count-1 do
    FTokenizers[i].LoadTokenizerPos(R);
end;

function TTokCompound.NextToken(out AToken: TTokenInfo): Boolean;
begin
  result := False;
  while (FCurrentTokenizer >= 0) and (FCurrentTokenizer < FTokenizers.Count) do
  begin
    result := FTokenizers[FCurrentTokenizer].NextToken(AToken);

    { no more token on this level - go up }
    if not result then
    begin
      dec(FCurrentTokenizer);
      Continue;
    end;

    { token on last level - match found, we should translate result into coordinates of first tokenizer }
    if FCurrentTokenizer>=FTokenizers.Count-1 then
    begin
      Inc(AToken.Start, FOffsets[FCurrentTokenizer]);
      Exit;
    end;

    { initialize next level }
    inc(FCurrentTokenizer);
    FTokenizers[FCurrentTokenizer].SetText(@FTokenizers[FCurrentTokenizer-1].Text[AToken.Start+AToken.DelimitersPrefix], AToken.Len);
    FTokenizers[FCurrentTokenizer].Reset;
    FOffsets[FCurrentTokenizer] := FOffsets[FCurrentTokenizer-1] + AToken.Start+AToken.DelimitersPrefix;
  end;
end;

function TTokCompound.GetAsString: string;
begin
  result := First.AsString;
end;

function TTokCompound.GetFirst: TTokCustom;
begin
  result := FTokenizers[0];
end;

function TTokCompound.GetLast: TTokCustom;
begin
  result := FTokenizers[FTokenizers.Count-1];
end;

function TTokCompound.GetTokenizer(n: integer): TTokCustom;
begin
  result := FTokenizers[n];
end;

function TTokCompound.GetTokenizersCount: integer;
begin
  result := FTokenizers.Count;
end;

{ TTokCustomText }

constructor TTokCustomText.Create(AText: PChar; ATextLen: integer);
begin
  inherited;
  FTextStorage := '';
  FText        := AText;
  FTextLen     := ATextLen;
  Reset;
end;

constructor TTokCustomText.Create(const AText: string);
begin
  inherited;
  Reset(AText);
end;

procedure TTokCustomText.Reset(const AText: string);
begin
  FTextStorage := AText;
  FText        := PChar(FTextStorage);
  FTextLen     := Length(FTextStorage);
  Reset;
end;

procedure TTokCustomText.Reset(const AText: string; AStart, ALen: integer);
begin
  FTextStorage := AText;
  FText        := @FTextStorage[AStart + Low(FTextStorage)];
  FTextLen     := ALen;
  Reset;
end;

procedure TTokCustomText.Reset(const AText: PChar; ALen: integer);
begin
  FTextStorage := '';
  FText        := AText;
  FTextLen     := ALen;
  Reset;
end;

procedure TTokCustomText.SetText(AText: PChar; ATextLen: integer);
begin
  FTextStorage := '';
  FText        := AText;
  FTextLen     := ATextLen;
end;

function TTokCustomText.GetText: PChar;
begin
  result := FText;
end;

function TTokCustomText.GetTextLen: integer;
begin
  result := FTextLen;
end;

procedure TTokCustomText.SetPosition(const AValue: integer);
begin
  FPosition := AValue;
end;

function TTokCustomText.GetAsString: string;
begin
  if FTextStorage<>'' then
    result := FTextStorage
  else
    result := inherited;
end;

function TTokCustomText.GetPosition: integer;
begin
  result := FPosition;
end;

procedure TTokCustomText.ResetTokenizer;
begin
  Position := 0;
end;

procedure TTokCustomText.SaveTokenizerPos(W: TWriter);
begin
  {inherited;}
  W.WriteInteger(1); { state format version }
  W.WriteInteger(Position);
end;

procedure TTokCustomText.LoadTokenizerPos(R: TReader);
begin
  {inherited;}
  if R.ReadInteger<>1 then
    raise Exception.Create('Error'); { state format version }
  Position := R.ReadInteger;
end;

function TTokCustomText.NextToken(out AToken: TTokenInfo): Boolean;
var
  L: Integer;
begin
  L := FTextLen-FPosition;
  if L<=0 then
    result := False
  else
  begin
    AToken.Clear;
    result := FindNextToken(pointer(FText+FPosition), L, AToken);
    if result then
    begin
      Assert(AToken.Start = 0);
      AToken.Start := FPosition;
      inc(FPosition, AToken.DelimitersPrefix+AToken.Len+AToken.DelimitersSuffix);
    end;
  end;
end;

{ TTokNonSpace }

function TTokNonSpace.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  D,T: Integer;
begin
  D := 0;
  while (Len > 0) and (Text^ <= ' ') do
  begin
    Inc(Text);
    Dec(Len);
    inc(D);
  end;
  T := 0;
  while (Len > 0) and (Text^ > ' ') do
  begin
    Inc(Text);
    Dec(Len);
    inc(T);
  end;
  result := T > 0;
  if result then
  begin
    Res.DelimitersPrefix := D;
    Res.Len := T;
  end;
end;

{ TTokLetters }

function TTokLetters.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  D,T: Integer;
begin
  D := 0;
  while (Len > 0) and not Text^.IsLetter do
  begin
    Inc(Text);
    Dec(Len);
    inc(D);
  end;
  T := 0;
  while (Len > 0) and Text^.IsLetter do
  begin
    Inc(Text);
    Dec(Len);
    inc(T);
  end;
  result := T > 0;
  if result then
  begin
    Res.DelimitersPrefix := D;
    Res.Len := T;
  end;
end;

{ TTokDigits }

function TTokDigits.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := NextDigits(Text, Len, Res);
end;

class function TTokDigits.IsStartOfDigits(C: Char): boolean;
begin
  result := C.IsDigit;
end;

class function TTokDigits.NextDigits(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  I,J: Integer;
begin
  J := Len;
  for I := 0 to Len-1 do
    if IsStartOfDigits(Text[I]) then
    begin
      J := I;
      Break;
    end;
  inc(Text, J);
  dec(Len, J);
  Res.DelimitersPrefix := J;

  J := Len;
  for I := 0 to Len-1 do
    if not Char(Text[I]).IsDigit then
    begin
      J := I;
      Break;
    end;
  Res.Len := J;

  result := Res.DelimitersPrefix+Res.Len > 0;
end;

{ TTokLettersOrDigits }

function TTokLettersOrDigits.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  D,T: Integer;
begin
  D := 0;
  while (Len > 0) and not Text^.IsLetterOrDigit do
  begin
    Inc(Text);
    Dec(Len);
    inc(D);
  end;
  T := 0;
  while (Len > 0) and Text^.IsLetterOrDigit do
  begin
    Inc(Text);
    Dec(Len);
    inc(T);
  end;
  result := T > 0;
  if result then
  begin
    Res.DelimitersPrefix := D;
    Res.Len := T;
  end;
end;

{ TTokNumbers }

{ Find number in string. Only general notation is supported.
  Correct: "12" "12." "1.2" "-134.5" "+2"
  "1E5" -> "1" "5"
  "Date: 12.12.2016" -> "12.12" "2016"
  "P12" -> "12" }
function TTokNumbers.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  D,T: Integer;
begin
  { RE: Num = ("+" | "-") Digit Digit* ("." Digit*)? }
  D := 0;
  repeat
    while (Len > 0) and (Text^ <> '+') and (Text^ <> '-') and not Text^.IsDigit do
    begin
      Inc(Text);
      Dec(Len);
      inc(D);
    end;
    { Len=0 or "+" or "-" or Digit }
    if Len <= 0  then
      Exit(False);
    { "+" or "-" or Digit }
    if Text^.IsDigit then
      Break;
    { "+" or "-" }
    if Len < 2  then
      Exit(False);
    if (Text+1)^.IsDigit then
      Break;
    inc(Text);
    Dec(Len);
    inc(D);
  until false;
  if Len<=0 then
    Exit(False);

  { we found start pos of number, now we should find whole number }
  T := 0;
  if (Text^='+') or (Text^='-') then
  begin
    inc(Text);
    dec(Len);
    inc(T);
  end;
  while (Len>0) do
    if Text^.IsDigit then
    begin
      Inc(Text);
      Dec(Len);
      Inc(T);
    end
    else
    if (Text^=FormatSettings.DecimalSeparator) or (Text^='.') then
    begin
      repeat
        Inc(Text);
        Dec(Len);
        Inc(T);
      until (Len<=0) or not Text^.IsDigit;
      Break;
    end
    else
      Break;

  result := T > 0;
  if result then
  begin
    Res.DelimitersPrefix := D;
    Res.Len := T;
  end;
end;

{ TTokIdentifier }

class function TTokIdentifier.IsStartOfIdentifier(C: Char): Boolean;
begin
  result := C.IsLetter or (C='_');
end;

class function TTokIdentifier.NextIdentifier(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  I,J: Integer;
begin

  { find first char of identifier }
  J := Len;
  for I := 0 to Len-1 do
    if IsStartOfIdentifier(Text[I]) then
    begin
      J := I;
      Break;
    end;
  inc(Text, J);
  dec(Len, J);
  Res.DelimitersPrefix := J;

  { find all other chars of identifier }
  J := Len;
  for I := 1 to Len-1 do
    if not Char(Text[I]).IsLetter and (Text[I]<>'_') and not Char(Text[I]).IsDigit then
    begin
      J := I;
      Break;
    end;
  Res.Len := J;

  result := Res.DelimitersPrefix + Res.Len > 0;
end;

function TTokIdentifier.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := NextIdentifier(Text, Len, Res);
end;

{ TTokCharDelimitedLines }

constructor TTokCharDelimitedLines.Create(AText: PChar; ATextLen: integer; ADelimiter: Char; ACaseSensitive: Boolean = False);
begin
  inherited Create(AText, ATextLen);
  FDelimiter := ADelimiter;
  FCaseSensitive := ACaseSensitive;
end;

constructor TTokCharDelimitedLines.Create(const AText: string; ADelimiter: Char; ACaseSensitive: Boolean = False);
begin
  inherited Create(AText);
  FDelimiter := ADelimiter;
  FCaseSensitive := ACaseSensitive;
end;

function TTokCharDelimitedLines.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  T: Integer;
  L,U: Char;
begin
  if FCaseSensitive then
  begin
    L := FDelimiter;
    T := 0;
    while (Len > 0) and (Text^ <> L) do
    begin
      Inc(Text);
      Dec(Len);
      inc(T);
    end;
  end
  else
  begin
    L := FDelimiter.ToLower;
    U := FDelimiter.ToUpper;
    T := 0;
    while (Len > 0) and (Text^<>L) and (Text^<>U) do
    begin
      Inc(Text);
      Dec(Len);
      inc(T);
    end;
  end;
  result := T > 0;
  if result then
  begin
    Res.Len := T;
    Res.DelimitersSuffix := IfThen(Len > 0, 1, 0);
  end;
end;

{ TTokenInfo }

procedure TTokenInfo.Clear;
begin
  { Start            := 0;   start position in the text
    DelimitersPrefix := 0;   number of preceding delimiters or zero
    Len              := 0;   length of the token or zero
    DelimitersSuffix := 0;   number of trailing delimiters or zero  }
  Self := Default(TTokenInfo);
end;

{ TTokLiterals }

class function TTokLiterals.GetLiteralLen(Text: PChar; Len: integer): integer;
var
  Res: TTokenInfo;
begin
  if (Len<=0) or not IsStartOfLiteral(Text^) then
    result := 0
  else
  begin
    Res.Clear;
    if not NextLiteral(Text, Len, Res) then
      raise Exception.Create('Error');
    Assert((Res.DelimitersPrefix+Res.DelimitersSuffix=0) and (Res.Len>0));
    result := Res.Len;
  end;
end;

function TTokLiterals.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := NextLiteral(Text, Len, Res);
end;

class function TTokLiterals.IsStartOfLiteral(C: Char): boolean;
begin
  result := C='''';
end;

class function TTokLiterals.NextLiteral(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  I,J: Integer;
begin

  { find first char of identifier }
  J := Len;
  for I := 0 to Len-1 do
    if IsStartOfLiteral(Text[I]) then
    begin
      J := I;
      Break;
    end;
  inc(Text, J);
  dec(Len, J);
  Res.DelimitersPrefix := J;

  { find all other chars of identifier }
  if Len>0 then
  begin
    J := 1;
    inc(Text);
    dec(Len);
    while Len>0 do
      if Text^<>'''' then
      begin
        inc(Text);
        dec(Len);
        inc(J);
      end
      else
        if (Len>1) and (Text[1]='''') then
        begin
          inc(Text,2);
          dec(Len,2);
          inc(J, 2);
        end
        else
        begin
          inc(J);
          Break;
        end;
    res.Len := J;
  end;

  result := Res.DelimitersPrefix + Res.Len > 0;
end;

{ TTokComments }

class function TTokComments.IsStartOfComment(Text: PChar; Len: integer): Boolean;
begin
  result := (Text^='{') or
    (Len>1) and
    (
      (Text^='(') and (Text[1]='*') or
      (Text^='/') and (Text[1]='/')
    );
end;

class function TTokComments.NextComment(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  D,T,I: Integer;
begin
  {   {  (*  //  '   }
  D := 0;
  T := 0;
  while Len>0 do

    if IsStartOfComment(Text, Len) then
      case Text^ of
        '{':
          begin
            T := Len;
            for I := 1 to Len-1 do
              if Text[I]='}' then
              begin
                T := I+1;
                Break;
              end;
            Break;
          end;

        '(':
          begin
            T := Len;
            for I := 2 to Len-2 do
              if (Text[I]='*') and (Text[I+1]=')') then
              begin
                T := I+2;
                Break;
              end;
            Break;
          end;

        '/':
        begin
          T := TTokLines.GetLineLength(Text, Len, False);
          Break;
        end;

        else
          raise Exception.Create('Error');
      end

    else
    if TTokLiterals.IsStartOfLiteral(Text^) then
    begin
      I := TTokLiterals.GetLiteralLen(Text, Len);
      Inc(D, I);
      Inc(Text, I);
      Dec(Len, I);
    end

    else
    begin
      inc(D);
      inc(Text);
      dec(Len);
    end;

  Res.DelimitersPrefix := D;
  Res.Len := T;
  result := D+T > 0;
end;

function TTokComments.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := NextComment(Text, Len, Res);
end;

{ TTokWhitespaces }

function TTokWhitespaces.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := NextWhitespace(Text, Len, Res);
end;

class function TTokWhitespaces.IsStartOfWhitespace(C: Char): Boolean;
begin
  result := C.IsWhiteSpace;
end;

class function TTokWhitespaces.NextWhitespace(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
var
  I,J: Integer;
begin

  { find first char }
  J := Len;
  for I := 0 to Len-1 do
    if IsStartOfWhitespace(Text[I]) then
    begin
      J := I;
      Break;
    end;
  inc(Text, J);
  dec(Len, J);
  Res.DelimitersPrefix := J;

  { find all other chars }
  J := Len;
  for I := 1 to Len-1 do
    if not Char(Text[I]).IsWhitespace then
    begin
      J := I;
      Break;
    end;
  Res.Len := J;

  result := Res.DelimitersPrefix + Res.Len > 0;
end;

{ TTokPascal }

function TTokPascal.GetAllTokens: TArray<TToken>;
var
  List: TList<TToken>;
  Token: TToken;
begin
  List := TList<TToken>.Create;
  try
    List.Capacity := Max(1000, TextLen div 2);
    while Next(Token.FPos) do
    begin
      Token.FType := LastTokenType;
      List.Add(Token);
    end;
    result := List.ToArray;
  finally
    List.Free;
  end;
end;

procedure TTokPascal.LoadTokenizerPos(R: TReader);
begin
  inherited;
  R.Read(FTokenType, SizeOF(FTokenType));
end;

function TTokPascal.Next(ATokenTypes: TPasTokenTypes; out AToken: TTokenPos): Boolean;
begin
  while Next(AToken) do
    if LastTokenType in ATokenTypes then
      Exit(True);
  result := False;
end;

function TTokPascal.Next(ATokenTypes: TPasTokenTypes; out AToken: String): Boolean;
begin
  while Next(AToken) do
    if LastTokenType in ATokenTypes then
      Exit(True);
  result := False;
end;

function TTokPascal.Next(ATokenTypes: TPasTokenTypes): String;
begin
  while Next(result) do
    if LastTokenType in ATokenTypes then
      Exit;
  result := '';
end;

procedure TTokPascal.SaveTokenizerPos(W: TWriter);
begin
  inherited;
  W.Write(FTokenType, SizeOF(FTokenType));
end;

class function TTokPascal.IsDelimiterChar(C: Char): Boolean;
begin
  result := (Word(C)<127) and (AnsiChar(Byte(C)) in Delimiters);
end;

function TTokPascal.FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean;
begin
  result := Len>0;
  if not result then
    Exit;

  if TTokWhitespaces.IsStartOfWhitespace(Text^) then
  begin
    result := TTokWhitespaces.NextWhitespace(Text, Len, Res);
    Assert(result and (Res.DelimitersPrefix+Res.DelimitersSuffix=0) and (Res.Len>0));
    FTokenType := TPasTokenType.Whitespace;
    Exit;
  end;

  if TTokLiterals.IsStartOfLiteral(Text^) then
  begin
    result := TTokLiterals.NextLiteral(Text, Len, Res);
    Assert(result and (Res.DelimitersPrefix+Res.DelimitersSuffix=0) and (Res.Len>0));
    FTokenType := TPasTokenType.Literal;
    Exit;
  end;

  if TTokDigits.IsStartOfDigits(Text^) then
  begin
    result := TTokDigits.NextDigits(Text, Len, Res);
    Assert(result and (Res.DelimitersPrefix+Res.DelimitersSuffix=0) and (Res.Len>0));
    FTokenType := TPasTokenType.Digits;
    Exit;
  end;

  if TTokComments.IsStartOfComment(Text, Len) then
  begin
    result := TTokComments.NextComment(Text, Len, Res);
    Assert(result and (Res.DelimitersPrefix+Res.DelimitersSuffix=0) and (Res.Len>0));
    FTokenType := TPasTokenType.Comment;
    Exit;
  end;

  if TTokIdentifier.IsStartOfIdentifier(Text^) then
  begin
    result := TTokIdentifier.NextIdentifier(Text, Len, Res);
    Assert(result and (Res.DelimitersPrefix+Res.DelimitersSuffix=0) and (Res.Len>0));
    FTokenType := TPasTokenType.Identifier;
    Exit;
  end;

  if IsDelimiterChar(Text^) then
  begin
    Res.Len := 1;
    FTokenType := TPasTokenType.Delimiter;
    Exit;
  end;

  FTokenType := TPasTokenType.Unknown;
  Res.Len := 1;
end;

{ TStringEditor.TInstr }

procedure TStringEditor.TReplace.Clear;
begin
  Self := Default(TReplace);
end;

{ TStringEditor }

procedure TStringEditor.Clear;
begin
  Instructions.Clear;
end;

procedure TStringEditor.Add(const Instr: TReplace);
var
  I: Integer;
begin
  I := Instructions.Add(Instr);
  Instructions.Items[I].Order := I;
end;

procedure TStringEditor.Delete(const Pos: TTokenPos);
var
  r: TReplace;
begin
  r.Clear;
  r.DstPos := Pos;
  Add(r);
end;

procedure TStringEditor.Delete(const Start, Len: integer);
var
  r: TReplace;
begin
  r.Clear;
  r.DstPos.Start := Start;
  r.DstPos.Len   := Len;
  Add(r);
end;

procedure TStringEditor.Delete(const Pos: integer);
var
  r: TReplace;
begin
  r.Clear;
  r.DstPos.Start := Pos;
  r.DstPos.Len   := 1;
  Add(r);
end;

procedure TStringEditor.Insert(Pos: integer; const Substr: string; const SubStrPos: TTokenPos);
var
  r: TReplace;
begin
  r.Clear;
  r.SrcText      := Substr;
  r.SrcPos       := SubStrPos;
  r.DstPos.Start := Pos;
  Add(r);
end;

procedure TStringEditor.Insert(Pos: integer; const Substr: string; const SubStrOffset, SubStrLen: integer);
var
  r: TReplace;
begin
  r.Clear;
  r.SrcText      := Substr;
  r.SrcPos.Start := SubStrOffset;
  r.SrcPos.Len   := SubStrLen;
  r.DstPos.Start := Pos;
  Add(r);
end;

procedure TStringEditor.Insert(Pos: integer; const Substr: string);
var
  r: TReplace;
begin
  r.Clear;
  r.SrcText      := Substr;
  r.SrcPos.Start := 0;
  r.SrcPos.Len   := Length(Substr);
  r.DstPos.Start := Pos;
  Add(r);
end;

procedure TStringEditor.Replace(const Pos: TTokenPos; const Substr: string);
var
  r: TReplace;
begin
  r.Clear;
  r.SrcText      := Substr;
  r.SrcPos.Start := 0;
  r.SrcPos.Len   := Length(Substr);
  r.DstPos       := Pos;
  Add(r);
end;

procedure TStringEditor.Replace(const Start, Len: integer; const Substr: string; const SubStrOffset, SubStrLen: integer);
var
  r: TReplace;
begin
  r.Clear;
  r.SrcText      := Substr;
  r.SrcPos.Start := SubStrOffset;
  r.SrcPos.Len   := SubStrLen;
  r.DstPos.Start := Start;
  r.DstPos.Len   := Len;
  Add(r);
end;

procedure TStringEditor.Replace(const Pos: TTokenPos; const Substr: string; const SubStrPos: TTokenPos);
var
  r: TReplace;
begin
  r.Clear;
  r.SrcText := Substr;
  r.SrcPos  := SubStrPos;
  r.DstPos  := Pos;
  Add(r);
end;

procedure TStringEditor.Replace(const Start, Len: integer; const Substr: string);
var
  r: TReplace;
begin
  r.Clear;
  r.SrcText      := Substr;
  r.SrcPos.Start := 0;
  r.SrcPos.Len   := Length(Substr);
  r.DstPos.Start := Start;
  r.DstPos.Len   := Len;
  Add(r);
end;

procedure TStringEditor.Sort;
var
  Comparer: IComparer<TReplace>;
begin
  Comparer := TDelegatedComparer<TReplace>.Create(
    function(const A,B: TReplace): integer
    begin
      result := A.DstPos.Start-B.DstPos.Start;
      if result=0 then
        result := A.Order-B.Order;
    end);
  TArray.Sort<TReplace>(Instructions.Items, Comparer, 0,Instructions.Count);
end;

function TStringEditor.Apply(const Src: string): string;
var
  L,I,SrcPos: Integer;
  P: PReplace;
  Buffer: TStringBuffer;
begin
  Buffer.Clear;
  Sort;
  SrcPos := 0;
  L := Length(Src);
  for I := 0 to Instructions.Count-1 do
  begin
    P := @Instructions.Items[I];
    if P.DstPos.Start > L then
      Break;
    if P.DstPos.Start > SrcPos then
      Buffer.Write(Src, SrcPos, P.DstPos.Start-SrcPos);
    if P.SrcPos.Len > 0 then
      Buffer.Write(P.SrcText, P.SrcPos.Start, P.SrcPos.Len);
    SrcPos := Min(Max(P.DstPos.Start + P.DstPos.Len, SrcPos), L);
  end;
  if SrcPos < L then
    Buffer.Write(Src, SrcPos, L-SrcPos);
  result := Buffer.Text;
end;

{ TStringBuffer }

class operator TStringBuffer.Add(const ALeft: TStringBuffer; const ARight: string): TStringBuffer;
begin
  result.Text := ALeft.Text + ARight;
  result.Position := result.Size;
end;

class operator TStringBuffer.Add(const ALeft, ARight: TStringBuffer): TStringBuffer;
begin
  result.Text := ALeft.Text + ARight.Text;
  result.Position := result.Size;
end;

class operator TStringBuffer.In(const Left, Right: TStringBuffer): Boolean;
begin
  result := TStr.Contains(Left.Text, Right.Text);
end;

class operator TStringBuffer.In(const Left: TStringBuffer; const Right: string): Boolean;
begin
  result := TStr.Contains(Left.Text, Right);
end;

class operator TStringBuffer.Add(const ALeft: string; const ARight: TStringBuffer): TStringBuffer;
begin
  result.Text := ALeft + ARight.Text;
  result.Position := result.Size;
end;

procedure TStringBuffer.CheckCapacity(MinCapacity: integer);
begin
  if Capacity < MinCapacity then
    Capacity := TFun.Max(MinCapacity, Capacity shl 1, 32);
end;

procedure TStringBuffer.Clear;
begin
  Self := Default(TStringBuffer);
end;

class operator TStringBuffer.Equal(const ALeft: string; const ARight: TStringBuffer): Boolean;
begin
  result := AnsiSameText(ALeft, ARight.Text);
end;

class operator TStringBuffer.Equal(const ALeft: TStringBuffer; const ARight: string): Boolean;
begin
  result := AnsiSameText(ALeft.Text, ARight);
end;

class operator TStringBuffer.Equal(const ALeft, ARight: TStringBuffer): Boolean;
begin
  result := AnsiSameText(ALeft.Text, ARight.Text);
end;

function TStringBuffer.GetCapacity: integer;
begin
  result := Length(FData);
end;

function TStringBuffer.GetEmpty: Boolean;
begin
  result := Size=0;
end;

function TStringBuffer.GetEOF: boolean;
begin
  result := Position >= Size;
end;

function TStringBuffer.GetLeft: integer;
begin
  result := Size-Position;
end;

function TStringBuffer.GetText: string;
begin
  if Size=Capacity then
    result := FData
  else
  begin
    SetLength(result, Size);
    System.Move(FData[Low(FData)], result[Low(result)], Size*SizeOf(Char));
  end;
end;

class operator TStringBuffer.Implicit(const Buffer: TStringBuffer): String;
begin
  result := Buffer.Text;
end;

class operator TStringBuffer.Implicit(const Data: string): TStringBuffer;
begin
  result.Text := Data;
end;

class operator TStringBuffer.Implicit(const Data: integer): TStringBuffer;
begin
  result.Text := Data.ToString;
end;

class operator TStringBuffer.Implicit(const Data: double): TStringBuffer;
begin
  result.Text := Data.ToString;
end;

procedure TStringBuffer.SaveToFile(const FileName: string; Encoding: TEncoding = nil);
var
  Bytes: TArray<Byte>;
begin
  if Encoding=nil then
    Encoding := TEncoding.UTF8;
  SetLength(Bytes, Encoding.GetByteCount(FData, 0,Size));
  Encoding.GetBytes(FData, 0,Size, Bytes,0);
  TFileUtils.Save<Byte>(FileName, Bytes, 0,Length(Bytes));
end;

procedure TStringBuffer.LoadFromFile(const FileName: string; Encoding: TEncoding = nil);
var
  Bytes: TArray<byte>;
begin
  if Encoding=nil then
    Encoding := TEncoding.UTF8;
  TFileUtils.Load<Byte>(FileName, Bytes);
  FData := Encoding.GetString(Bytes);
  FSize := Length(FData);
  FPosition := 0;
end;

class operator TStringBuffer.Multiply(const ALeft: TStringBuffer; ARight: integer): TStringBuffer;
var
  S: string;
  I: Integer;
begin
  result.Clear;
  S := ALeft.Text;
  for I := 0 to ARight-1 do
    result.Write(S);
end;

class operator TStringBuffer.Multiply(ALeft: integer; const ARight: TStringBuffer): TStringBuffer;
begin
  result := ARight*ALeft;
end;

class operator TStringBuffer.Negative(Value: TStringBuffer): TStringBuffer;
begin
  result.Text := TStr.Reverse(Value.Text);
end;

class operator TStringBuffer.NotEqual(const Left, Right: TStringBuffer): Boolean;
begin
  result := not (Left=Right);
end;

procedure TStringBuffer.Read(var Dst: char);
begin
  Assert(Position + 1 <= Size);
  Dst := FData.Chars[Position];
  inc(FPosition);
end;

procedure TStringBuffer.Read(var Dst: string; DstCharOffset, CharCount: integer);
begin
  Assert(Position + CharCount <= Size);
  System.Move(FData[Position+Low(FData)], Dst[DstCharOffset+Low(FData)], CharCount*SizeOf(Char));
  inc(FPosition, CharCount);
end;

procedure TStringBuffer.Read(var Dst: string; CharCount: integer);
begin
  SetLength(Dst, CharCount);
  Read(Dst, 0, CharCount);
end;

procedure TStringBuffer.SetCapacity(Value: integer);
begin
  Assert(Value >= Size);
  SetLength(FData, Value);
end;

procedure TStringBuffer.SetLeft(Value: integer);
begin
  Size := Position + Value;
end;

procedure TStringBuffer.SetSize(Value: integer);
begin
  CheckCapacity(Value);
  FSize := Value;
  FPosition := Max(Min(FPosition, FSize), 0);
end;

procedure TStringBuffer.SetText(const Value: string);
begin
  Clear;
  Write(Value);
  Position := 0;
end;

class operator TStringBuffer.Subtract(const ALeft, ARight: TStringBuffer): TStringBuffer;
var
  L,R: string;
begin
  L := ALeft.Text;
  R := ARight.Text;
  if L.EndsWith(R, True) then
    result.Text := L.Substring(0, Length(L)-Length(R))
  else
    result.Text := L;
end;

procedure TStringBuffer.TrimExcess;
begin
  Capacity := Size;
end;

procedure TStringBuffer.Write(const Src: char);
begin
  CheckCapacity(Position + 1);
  FData[Position+Low(FData)] := Src;
  inc(FPosition);
  if FPosition > FSize then
    FSize := FPosition;
end;

procedure TStringBuffer.Write(const Src: string);
begin
  Write(Src, 0, Length(Src));
end;

procedure TStringBuffer.Write(const Src: string; CharOffset, CharCount: integer);
begin
  {$IFOPT R+}
  Assert((CharOffset>=0) and (CharOffset+CharCount<=Length(Src)));
  {$ENDIF}
  if CharCount>0 then
  begin
    CheckCapacity(Position + CharCount);
    System.Move(Src[CharOffset+Low(Src)], FData[Position+Low(Src)], CharCount*SizeOf(Char));
    inc(FPosition, CharCount);
    if FPosition > FSize then
      FSize := FPosition;
  end;
end;

{ TCodePages }

class procedure TCodePages.InitCyr;
var
  I: Integer;
  S: TStrCyr;
  T: string;
begin
  if UnicodeToCyr<>nil then
    Exit;
  UnicodeToCyr := TDictionary<Char, Byte>.Create;
  SetLength(S, 256);
  for I := 0 to 255 do
    Byte(S[I+Low(S)]) := I;
  T := String(S);
  for I := 0 to 255 do
  begin
    CyrToUnicode[I] := T.Chars[I];
    UnicodeToCyr.Add(T.Chars[I], I);
  end;
end;

class procedure TCodePages.InitEur;
var
  I: Integer;
  S: TStrEur;
  T: string;
begin
  if UnicodeToEur<>nil then
    Exit;
  UnicodeToEur := TDictionary<Char, Byte>.Create;
  SetLength(S, 256);
  for I := 0 to 255 do
    Byte(S[I+Low(S)]) := I;
  T := String(S);
  for I := 0 to 255 do
  begin
    EurToUnicode[I] := T.Chars[I];
    UnicodeToEur.Add(T.Chars[I], I);
  end;
end;

class function TCodePages.EncodeTextCyr(const Text: string; Len: integer; var Dst): Boolean;
var
  I: Integer;
begin
  InitCyr;
  for I := 0 to Len-1 do
    if not UnicodeToCyr.TryGetValue(Text.Chars[I], (PByte(@Dst) + I)^) then
      Exit(False);
  Result := True;
end;

class function TCodePages.EncodeTextEur(const Text: string; Len: integer; var Dst): Boolean;
var
  I: Integer;
begin
  InitEur;
  for I := 0 to Len-1 do
    if not UnicodeToEur.TryGetValue(Text.Chars[I], (PByte(@Dst) + I)^) then
      Exit(False);
  Result := True;
end;

class function TCodePages.DecodeTextCyr(const Src; Size: integer): string;
var
  I: Integer;
begin
  SetLength(Result, Size);
  for I := 0 to Size-1 do
    Result[I+Low(Result)] := CyrToUnicode[(PByte(@Src)+I)^];
end;

class function TCodePages.DecodeTextEur(const Src; Size: integer): string;
var
  I: Integer;
begin
  SetLength(Result, Size);
  for I := 0 to Size-1 do
    Result[I+Low(Result)] := EurToUnicode[(PByte(@Src)+I)^];
end;

class destructor TCodePages.Destroy;
begin
  FreeAndNil(UnicodeToCyr);
  FreeAndNil(UnicodeToEur);
end;

class function TCodePages.EncodeText(const Text: string; var CodePageId: TCodePageId; var Bytes: TArray<byte>): Boolean;
begin
  SetLength(Bytes, Length(Text));
  if EncodeTextEur(Text, Length(Text), Bytes[0]) then CodePageId := cpiEur else
    if EncodeTextCyr(Text, Length(Text), Bytes[0]) then CodePageId := cpiCyr else
      Exit(False);
  result := True;
end;

class function TCodePages.DecodeText(const Bytes: TArray<byte>; CodePageId: TCodePageId): string;
begin
  if Length(Bytes) = 0 then
    result := ''
  else
  case CodePageId of
    cpiCyr: result := DecodeTextCyr(Bytes[0], Length(Bytes));
    cpiEur: result := DecodeTextEur(Bytes[0], Length(Bytes));
    else result := '';
  end;
end;

class function TCodePages.StringToBuf(const Text: string; BufSize: integer; var CodePageId: TCodePageId; var Buf; var Len: integer): boolean;
begin
  Len := Min(Length(Text), BufSize);
  if EncodeTextEur(Text, Len, Buf) then CodePageId := cpiEur else
    if EncodeTextCyr(Text, Len, Buf) then CodePageId := cpiCyr else
      Exit(False);
  result := True;
end;

class function TCodePages.BufToString(const Buf; BufSize: integer; CodePageId: TCodePageId): string;
begin
  case CodePageId of
    cpiCyr: result := DecodeTextCyr(Buf, BufSize);
    cpiEur: result := DecodeTextEur(Buf, BufSize);
    else result := '';
  end;
end;

{ TBigEndianUTF32Encoding }

function TBigEndianUTF32Encoding.Clone: TEncoding;
begin
  Result := TBigEndianUTF32Encoding.Create;
end;

function TBigEndianUTF32Encoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
begin
  Result := CharCount * 4;
end;

function TBigEndianUTF32Encoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to CharCount - 1 do
  begin
    Word(Pointer(Bytes)^) := 0;
    Inc(Bytes, 2);
    Bytes^ := Hi(Word(Chars^));
    Inc(Bytes);
    Bytes^ := Lo(Word(Chars^));
    Inc(Bytes);
    Inc(Chars);
  end;
  Result := CharCount * 4;
end;

function TBigEndianUTF32Encoding.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := ByteCount div 4;
end;

function TBigEndianUTF32Encoding.GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
begin
  Result := CharCount;
  while CharCount > 0 do
  begin
    Assert(ByteCount >= 4);
    if Word(Pointer(Bytes)^) <> 0
      then Chars^ := '?'
      else Chars^ := Char(Pointer(@Bytes[2])^);
    Chars^ := Char( (Word(Chars^) shr 8) or (Word(Chars^) shl 8) );
    inc(Bytes,4);
    dec(ByteCount,4);
    inc(Chars);
    dec(CharCount);
  end;
end;

function TBigEndianUTF32Encoding.GetCodePage: Cardinal;
begin
  Result := 12001; // UTF-32BE
end;

function TBigEndianUTF32Encoding.GetEncodingName: string;
begin
  {$IFDEF MSWINDOWS}
    Result := '12001  (Unicode - Big-Endian)'; // do not localize
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    Result := 'Unicode (UTF-32BE)'; // do not localize
  {$ENDIF POSIX}
end;

function TBigEndianUTF32Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 4;
end;

function TBigEndianUTF32Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := (ByteCount div 4) + (ByteCount and 1) + 1;
end;

function TBigEndianUTF32Encoding.GetPreamble: TBytes;
begin
  Result := TBytes.Create(0, 0, $FE, $FF);
end;

{ TLittleEndianUTF32Encoding }

function TLittleEndianUTF32Encoding.Clone: TEncoding;
begin
  Result := TLittleEndianUTF32Encoding.Create;
end;

function TLittleEndianUTF32Encoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to CharCount - 1 do
  begin
    Bytes^ := Lo(Word(Chars^));
    Inc(Bytes);
    Bytes^ := Hi(Word(Chars^));
    Inc(Bytes);
    Word(Pointer(Bytes)^) := 0;
    Inc(Bytes, 2);
    Inc(Chars);
  end;
  Result := CharCount * 4;
end;

function TLittleEndianUTF32Encoding.GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
begin
  Result := CharCount;
  while CharCount > 0 do
  begin
    Assert(ByteCount >= 4);
    if Word((@Bytes[2])^) <> 0
      then Chars^ := '?'
      else Chars^ := Char(Pointer(Bytes)^);
    inc(Bytes,4);
    dec(ByteCount,4);
    inc(Chars);
    dec(CharCount);
  end;
end;

function TLittleEndianUTF32Encoding.GetCodePage: Cardinal;
begin
  Result := 12000; // UTF-32LE
end;

function TLittleEndianUTF32Encoding.GetEncodingName: string;
begin
  {$IFDEF MSWINDOWS}
    Result := '12000  (Unicode - Little-Endian)'; // do not localize
  {$ENDIF MSWINDOWS}
  {$IFDEF POSIX}
    Result := 'Unicode (UTF-32LE)'; // do not localize
  {$ENDIF POSIX}
end;

function TLittleEndianUTF32Encoding.GetPreamble: TBytes;
begin
  Result := TBytes.Create($FF, $FE, 0, 0);
end;

initialization
  TStr.InitializeVars;

finalization
  TStr.FinalizeVars;

end.

