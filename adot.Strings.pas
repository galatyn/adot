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
  adot.Collections, { TVector etc }
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

  private
    {$IFNDEF NoLoCaseMap}
    class var
      FLowerCaseMap: array[Char] of Char;
    {$ENDIF}

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
    { concatanate used values from Src (including empty strings) }
    class function Concat(const Src: array of string; const InUse: array of boolean; Delimeter: string = ' '): string; overload; static;

    { case insensitive with support of internation chars }
    class function SameText(const A,B: string): Boolean; overload;
    class function SameText(const A: string; const B: array of string): Boolean; overload;
    class function SameTrimText(const A,B: string): Boolean; overload;
    class function CompareText(const A,B: string): integer; overload;
    class function CompareTrimText(const A,B: string): integer; overload;
    class function SameText(A,B: PChar; Len: integer): Boolean; overload;

    class function LowerCaseChar(C: Char): Char; static;

    { based on TTokLines }
    class procedure TextToLines(const Text: string; Dst: TStrings; AClear: boolean = True); overload; static;
    class procedure TextToLines(const Text: string; out Dst: TArray<string>); overload; static;
    class procedure TextToLines(const Text: string; Delim: Char; out Dst: TArray<string>); overload; static;

    { similarity }
    class function TextDistance(const a,b: string; StrDistType: TTextDistance = tdLevenstein): integer; static;
    class function SimilarStrings(A,B: String; var dist: integer; AOptions: TSimilarityOptions = [soStrictIntMatching]): boolean; overload; static;
    class function SimilarStrings(A,B: String; AOptions: TSimilarityOptions = [soStrictIntMatching]): boolean; overload; static;
    { unlike SimilarStrings this function will try to match AQuery with every word of AText (min dist selected) }
    class function SimilarWordInText(const AWord, AText: String; var dist: integer; AOptions: TSimilarityOptions = [soStrictIntMatching]): boolean; static;

    { extract numbers/digits etc }
    class function Extract(InfoType: TExtractType; const s: string): String; static;

    { System.SysUtils.TextPos doesn't work with international chars "Æ","Å" etc (XE5 at least). }
    class function TextPosition(const ASubStr, AText: string; AOffset: integer = 0): Integer; static;

    { file-string }
    class function Load(Src: TStream; Encoding: TEncoding = nil): string; overload; static;
    class function Load(const FileName: string; Encoding: TEncoding = nil): string; overload; static;
    class procedure Save(Dst: TStream; const S: string; Encoding: TEncoding = nil); overload; static;
    class procedure Save(const FileName: string; const S: string; Encoding: TEncoding = nil); overload; static;

    { set-string, number-string etc }
    class function CharsCount(const AChars: TAnsiChars): integer;
    class function SetToString(const AChars: TAnsiChars): string;
    class function StringToSet(const s: string): TAnsiChars;
    class function IntToString(const N: int64; MinResLen: integer = -1): string; static;

    class function GetReadable(const S: string): string; overload; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function GetReadable(S: PChar; Count: integer): string; overload; static;

    { randomization }
    class function Random(ALen: integer; const AChars: TAnsiChars): string; overload;
    class function Random(ALen: integer; const AChars: string): string; overload;
    class function Random(ALen: integer): string; overload;
    class function Random(ALen: integer; AFrom,ATo: Char): string; overload;
  end;

  { Position of token in the text. Starts from zero. }
  TTokenPos = record
  public
    Start, Len: Integer;

    class operator Subtract(const A,B: TTokenPos): TTokenPos; { find "space" between   }
    class operator Add(const A,B: TTokenPos): TTokenPos;      { "merge" into one token }
    class operator In(const A: integer; const B: TTokenPos): Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
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
    function Next: String; overload; {$IFNDEF DEBUG}inline;{$ENDIF}

    { Reset parser to initial state (Position:=0 etc ) }
    procedure Reset; overload;{$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Reset(ANewText: PChar; ATextLen: integer); overload;

    { high level methods }
    procedure GetTokens(ADst: TStrings); overload;
    procedure GetTokens(var ADst: TArray<String>); overload;
    procedure GetTokens(var ADst: TArray<TTokenPos>); overload;

    class procedure Parse(const AText: string; ADst: TStrings); overload;
    class procedure Parse(const AText: string; var ADst: TArray<String>); overload;
    class procedure Parse(const AText: string; var ADst: TArray<TTokenPos>); overload;
    class procedure Parse(const AText: string; const ADelimiter: string; var ADst: string); overload;

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
    function GetTokenizersCount: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetTokenizer(n: integer): TTokCustom; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetFirst: TTokCustom; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetLast: TTokCustom; {$IFNDEF DEBUG}inline;{$ENDIF}

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
    class function IsStartOfDigits(C: Char): boolean; static;  {$IFNDEF DEBUG}inline;{$ENDIF}
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
    class function IsStartOfIdentifier(C: Char): Boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
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
    class function IsStartOfLiteral(C: Char): boolean; static;  {$IFNDEF DEBUG}inline;{$ENDIF}
    class function NextLiteral(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; static;
    class function GetLiteralLen(Text: PChar; Len: integer): integer;
  end;

  { Extract whitespaces. }
  TTokWhitespaces = class(TTokCustomText)
  protected
    function FindNextToken(Text: PChar; Len: integer; var Res: TTokenInfo): Boolean; override;
  public
    class function IsStartOfWhitespace(C: Char): Boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}
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
    class function IsDelimiterChar(C: Char): Boolean; static; {$IFNDEF DEBUG}inline;{$ENDIF}

    function GetAllTokens: TArray<TToken>;

    property LastTokenType: TPasTokenType read FTokenType;
  end;

  {!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  DEPRECATED! USE TTok* classes INSTEAD.
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
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

    { Set Text and IsAlpha predicate (default is IsLetterOrDigit). }
    constructor Create(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate = nil);

    { Find next word in the text. }
    function FindNext(var AStart,ALen: integer):Boolean; overload;
    function FindNext(var AWord: String):Boolean; overload;
    function FindNext: String; overload; {$IFNDEF DEBUG}inline;{$ENDIF}

    { Find all words in the text (from begin to end). }
    procedure Get(ADst: TStrings); overload;
    procedure Get(var ADst: TArray<String>); overload;
    procedure Get(var ADst: TArray<TWordPosRec>); overload;

    { Helpers }
    class procedure Get(const AText: string; ADst: TStrings; AIsAlphaPredicate: TIsAlphaPredicate = nil); overload; static;
    class procedure Get(const AText: string; var ADst: TArray<String>; AIsAlphaPredicate: TIsAlphaPredicate = nil); overload; static;
    class procedure Get(const AText: string; var ADst: TArray<TWordPosRec>; AIsAlphaPredicate: TIsAlphaPredicate = nil); overload; static;

    { Start search from begin of the text }
    procedure Reset; {$IFNDEF DEBUG}inline;{$ENDIF}

    { Get number of words in the text (doesn't change current position) }
    function Count:Integer;

    { Get substring from the assigned text (usually APos is result of FindNext or Get) }
    function GetSubStr(const APos: TWordPosRec): String;

    { Find subsequence in the sequence:
       r := TTextWord.Create('word0 word1 it is test xxx xxx xxx');
       r.Get( WordList );
       Assert(r.Find(['it', 'is', 'test'], WordList) = 2); }
    function Find(const ASubSequence: array of string; const ASequence: TArray<TWordPosRec>): integer;

    { Most common TIsAlphaPredicate functions }
    class function IsNonSpace(const C: Char): Boolean; static;
    class function IsLetter(const C: Char): Boolean; static;
    class function IsLetterOrDigit(const C: Char): Boolean; static;
    class function IsDigit(const C: Char): Boolean; static;
    class function IsPartOfNumber(const C: Char): Boolean; static; { digit or DecimalSeparator }

    property Words[const APos: TWordPosRec]: string read GetSubStr; default;
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
      Instructions: TVector<TReplace>;

    procedure Add(const Instr: TReplace); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Sort;

  public

    procedure Clear; {$IFNDEF DEBUG}inline;{$ENDIF}

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

implementation

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

class function TStr.CompareText(const A, B: string): integer;
begin
  result := AnsiCompareText(A, B);
end;

class function TStr.CompareTrimText(const A, B: string): integer;
begin
  result := CompareText(Trim(A), Trim(B));
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

class function TStr.IntToString(const N: int64; MinResLen: integer): string;
begin
  result := IntToStr(N);
  if Length(result) < MinResLen then
    if N < 0 then
      result := result.Substring(0, 1) + StringOfChar('0', MinResLen-Length(result)) + result.Substring(1)
    else
      result := StringOfChar('0', MinResLen-Length(result)) + result;
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

class function TStr.GetReadable(const S: string): string;
begin
  result := GetReadable(PChar(S), Length(S));
end;

class function TStr.GetReadable(S: PChar; Count: integer): string;
var
  I: Integer;
begin
  SetLength(Result, Count);
  if Count<=0 then
    Exit;
  System.Move(S^, result[Low(result)], Count*SizeOf(Char));
  for I := Low(result) to High(result) do
    if result[I]<' ' then
      result[I] := '_';
end;

class procedure TStr.InitializeVars;
{$IFNDEF NoLoCaseMap}
var
  C: Char;
{$ENDIF}
begin
  {$IFNDEF NoLoCaseMap}
  for C := Low(C) to High(C) do
    FLowerCaseMap[C] := C.ToLower;
  {$ENDIF}
end;

class procedure TStr.FinalizeVars;
begin
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
  {$IFNDEF NoLoCaseMap}
  for I := 0 to Len-1 do
    if FLowerCaseMap[A[I]] <> FLowerCaseMap[B[I]] then
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
  {$IFNDEF NoLoCaseMap}
  result := FLowerCaseMap[C];
  {$ELSE}
  result := C.ToLower;
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
  S: TTextWords;
  W: String;
  D: integer;
begin
  Result := False;
  S := TTextWords.Create(AText);
  while S.FindNext(W) do
  begin
    if not SimilarStrings(AWord, W, D, AOptions) then
      Continue;
    if not Result then
      Dist := D
    else
      Dist := Min(Dist, D);
    Result := True;
  end;
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
          buf[j] := min3((buf[cuthalf + j] + 1),
            (buf[j - 1] + 1),
            (buf[cuthalf + j - 1] + cost))
        else
          buf[cuthalf + j] := min3((buf[j] + 1),
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
  if Encoding=nil then
    Encoding := TEncoding.UTF8;
  Src.Position := 0;
  SetLength(B, Src.Size);
  Src.ReadBuffer(B, Length(B));
  Result := Encoding.GetString(B);
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

{ TTextWords }

procedure TTextWords.Reset;
begin
  Position := Low(Text);
end;

function TTextWords.Count: Integer;
var
  OldPosition, Start, Len: Integer;
begin
  OldPosition := Position;
  Reset;
  Result := 0;
  while FindNext(Start, Len) do
    Inc(Result);
  Position := OldPosition;
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

constructor TTextWords.Create(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate);
begin
  Text := AText;
  if Assigned(AIsAlphaPredicate) then
    IsAlphaChar := AIsAlphaPredicate
  else
    IsAlphaChar := IsLetterOrDigit;
  Reset;
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

class function TTextWords.IsPartOfNumber(const C: Char): Boolean;
begin
  result := C.IsDigit or (C=FormatSettings.DecimalSeparator);
end;

class function TTextWords.IsLetter(const C: Char): Boolean;
begin
  Result := C.IsLetter;
end;

class function TTextWords.IsLetterOrDigit(const C: Char): Boolean;
begin
  Result := C.IsLetter or C.IsDigit;
end;

class function TTextWords.IsDigit(const C: Char): Boolean;
begin
  Result := C.IsDigit;
end;

function TTextWords.GetSubStr(const APos: TWordPosRec): String;
begin
  Result := Copy(Text, APos.Start, APos.Len);
end;

class procedure TTextWords.Get(const AText: string; ADst: TStrings; AIsAlphaPredicate: TIsAlphaPredicate);
var
  w: TTextWords;
begin
  w := TTextWords.Create(AText, AIsAlphaPredicate);
  w.Get(ADst);
end;

class procedure TTextWords.Get(const AText: string; var ADst: TArray<String>; AIsAlphaPredicate: TIsAlphaPredicate);
var
  w: TTextWords;
begin
  w := TTextWords.Create(AText, AIsAlphaPredicate);
  w.Get(ADst);
end;

class procedure TTextWords.Get(const AText: string; var ADst: TArray<TWordPosRec>; AIsAlphaPredicate: TIsAlphaPredicate);
var
  w: TTextWords;
begin
  w := TTextWords.Create(AText, AIsAlphaPredicate);
  w.Get(ADst);
end;

{ TTokenPos }

class operator TTokenPos.Add(const A, B: TTokenPos): TTokenPos;
begin
  result.Start := Min(A.Start, B.Start);
  result.Len   := Max(A.Start+A.Len, B.Start+B.Len) - result.Start;
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
  begin
    result.Start := A.Start + A.Len;
    result.Len   := B.Start-result.Start;
  end;
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
  Start            := 0; { start position in the text             }
  DelimitersPrefix := 0; { number of preceding delimiters or zero }
  Len              := 0; { length of the token or zero            }
  DelimitersSuffix := 0; { number of trailing delimiters or zero  }
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
  SrcText      := '';
  SrcPos.Start := 0;
  SrcPos.Len   := 0;
  DstPos.Start := 0;
  DstPos.Len   := 0;
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
begin
  TArray.Sort<TReplace>(Instructions.Items, TDelegatedComparer<TReplace>.Create(
    function(const A,B: TReplace): integer
    begin
      result := A.DstPos.Start-B.DstPos.Start;
      if result=0 then
        result := A.Order-B.Order;
    end));
end;

function TStringEditor.Apply(const Src: string): string;
var
  L,I,SrcPos: Integer;
  P: PReplace;
  Buffer: TBuffer;
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
    Buffer.Write(Src[SrcPos+Low(Src)], (L-SrcPos)*SizeOf(Char));
  Buffer.ReadAllData(result);
end;

initialization
  TStr.InitializeVars;

finalization
  TStr.FinalizeVars;

end.
