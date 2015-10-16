unit adot.Win.Micro.Classes;

// Very few classes for basic manipulations with files/texts.
// Can be used to build small utilities with reasonable size of EXE.

interface

uses
  System.SysUtils, Winapi.Windows, System.Character, System.Types;

const
{ TFileStream create mode }

  fmCreate = $FF00;

type
  TSeekOrigin = (soBeginning, soCurrent, soEnd);

  TMicroStream = class(TObject)
  protected
    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    function GetSize: Int64; virtual;
    procedure SetSize(const NewSize: Int64); overload; virtual;
  public
    function Skip(Amount: Integer): Integer;

    function Read(var Buffer; Count: Longint): Longint; overload; virtual;
    function Write(const Buffer; Count: Longint): Longint; overload; virtual;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; overload; virtual;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; overload; virtual;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; virtual;
    function Seek(const Offset: Int64; Origin: Word): Int64; overload; deprecated; inline;

    procedure ReadBuffer(var Buffer; Count: Longint); overload;
    procedure WriteBuffer(const Buffer; Count: Longint); overload;

    function CopyFrom(const Source: TMicroStream; Count: Int64): Int64;

    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
  end;

{ THandleStream class }

  TMicroHandleStream = class(TMicroStream)
  protected
    FHandle: THandle;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: THandle);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read FHandle;
  end;

{ TFileStream class }

  TMicroFileStream = class(TMicroHandleStream)
  strict private
    FFileName: string;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

  EFCreateError = class(Exception);
  EFOpenError = class(Exception);
  ESetSizeError = class(Exception);

  TMicroStringsEnumerator = class;

  TStringsDefined = set of (sdDelimiter, sdQuoteChar, sdNameValueSeparator,
    sdLineBreak, sdStrictDelimiter);

  TMicroStrings = class
  private
    FEncoding: TEncoding;
    FDefined: TStringsDefined;
    FDefaultEncoding: TEncoding;
    FDelimiter: Char;
    FLineBreak: string;
    FQuoteChar: Char;
    FNameValueSeparator: Char;
    FStrictDelimiter: Boolean;
    FUpdateCount: Integer;
    FWriteBOM: Boolean;
    function GetCommaText: string;
    function GetDelimitedText: string;
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure SetCommaText(const Value: string);
    procedure SetDelimitedText(const Value: string);
    procedure SetValue(const Name, Value: string);
    function GetDelimiter: Char;
    procedure SetDelimiter(const Value: Char);
    function GetLineBreak: string;
    procedure SetLineBreak(const Value: string);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const Value: Char);
    function GetNameValueSeparator: Char;
    procedure SetNameValueSeparator(const Value: Char);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const Value: Boolean);
    function GetValueFromIndex(Index: Integer): string;
    procedure SetValueFromIndex(Index: Integer; const Value: string);
    procedure SetDefaultEncoding(const Value: TEncoding);
  protected
    procedure Error(const Msg: string; Data: Integer); overload;
    procedure Error(Msg: PResStringRec; Data: Integer); overload;
    function ExtractName(const S: string): string;
    function Get(Index: Integer): string; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetEncoding(const Value: TEncoding); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    property UpdateCount: Integer read FUpdateCount;
    function CompareStrings(const S1, S2: string): Integer; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): Integer; virtual;
    function AddObject(const S: string; AObject: TObject): Integer; virtual;
    procedure Append(const S: string);
    procedure AddStrings(Strings: TMicroStrings); overload; virtual;
    procedure AddStrings(const Strings: TArray<string>); overload;
    procedure AddStrings(const Strings: TArray<string>; const Objects: TArray<TObject>); overload;
    procedure BeginUpdate;
    procedure Clear; virtual; abstract;
    procedure Delete(Index: Integer); virtual; abstract;
    procedure EndUpdate;
    function Equals(Strings: TMicroStrings): Boolean; reintroduce;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function GetEnumerator: TMicroStringsEnumerator;
    function GetText: PChar; virtual;
    function IndexOf(const S: string): Integer; virtual;
    function IndexOfName(const Name: string): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer; virtual;
    procedure Insert(Index: Integer; const S: string); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject); virtual;
    procedure LoadFromFile(const FileName: string); overload; virtual;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure LoadFromStream(Stream: TMicroStream); overload; virtual;
    procedure LoadFromStream(Stream: TMicroStream; Encoding: TEncoding); overload; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); overload; virtual;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload; virtual;
    procedure SaveToStream(Stream: TMicroStream); overload; virtual;
    procedure SaveToStream(Stream: TMicroStream; Encoding: TEncoding); overload; virtual;
    procedure SetText(Text: PChar); virtual;
    function ToStringArray: TArray<string>;
    function ToObjectArray: TArray<TObject>;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: Integer read GetCount;
    property DefaultEncoding: TEncoding read FDefaultEncoding write SetDefaultEncoding;
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property Encoding: TEncoding read FEncoding;
    property LineBreak: string read GetLineBreak write SetLineBreak;
    property Names[Index: Integer]: string read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    property Values[const Name: string]: string read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: Integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
  end;

  TMicroStringsEnumerator = class
  private
    FIndex: Integer;
    FStrings: TMicroStrings;
  public
    constructor Create(AStrings: TMicroStrings);
    function GetCurrent: string; inline;
    function MoveNext: Boolean;
    property Current: string read GetCurrent;
  end;

  TMicroStringList = class;

  PMicroStringItem = ^TMicroStringItem;
  TMicroStringItem = record
    FString: string;
    FObject: TObject;
  end;

  PMicroStringItemList = ^TMicroStringItemList;
  TMicroStringItemList = array of TMicroStringItem;
  TStringListSortCompare = function(List: TMicroStringList; Index1, Index2: Integer): Integer;
  TNotifyEvent = procedure(Sender: TObject) of object;

  TMicroStringList = class(TMicroStrings)
  private
    FList: TMicroStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FCaseSensitive: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FOwnsObject: Boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function Get(Index: Integer): string; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function CompareStrings(const S1, S2: string): Integer; override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); virtual;
  public
    constructor Create; overload;
    constructor Create(OwnsObjects: Boolean); overload;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: string; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure InsertObject(Index: Integer; const S: string;
      AObject: TObject); override;
    procedure Sort; virtual;
    procedure CustomSort(Compare: TStringListSortCompare); virtual;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property Sorted: Boolean read FSorted write SetSorted;
    property CaseSensitive: Boolean read FCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OwnsObjects: Boolean read FOwnsObject write FOwnsObject;
  end;

implementation

procedure RaiseError;
begin
  raise Exception.Create('Error');
end;

{ TMicroStream }

function TMicroStream.GetPosition: Int64;
begin
  Result := Seek(0, soCurrent);
end;

function TMicroStream.GetSize: Int64;
var
  Pos: Int64;
begin
  Pos := Seek(0, soCurrent);
  Result := Seek(0, soEnd);
  Seek(Pos, soBeginning);
end;

function TMicroStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

function TMicroStream.CopyFrom(const Source: TMicroStream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: Pointer;
begin
  if Count <= 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  ReallocMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      WriteBuffer(Buffer^, N);
      Dec(Count, N);
    end;
  finally
    ReallocMem(Buffer, 0);
  end;
end;

function TMicroStream.Read(Buffer: TBytes; Offset, Count: Integer): Longint;
begin
  Result := Read(Buffer[Offset], Count);
end;

procedure TMicroStream.ReadBuffer(var Buffer; Count: Integer);
var
  LTotalCount,
  LReadCount: Longint;
begin
  { Perform a read directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Read(Buffer, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    RaiseError;

  while (LTotalCount < Count) do
  begin
    { Try to read a contiguous block of <Count> size }
    LReadCount := Read(PByte(PByte(@Buffer) + LTotalCount)^,
      (Count - LTotalCount));

    { Check if we read something and decrease the number of bytes left to read }
    if LReadCount <= 0 then
      RaiseError
    else
      Inc(LTotalCount, LReadCount);
  end
end;

function TMicroStream.Write(const Buffer: TBytes; Offset,
  Count: Integer): Longint;
begin
  Result := Write(Buffer[Offset], Count);
end;

procedure TMicroStream.WriteBuffer(const Buffer; Count: Integer);
var
  LTotalCount,
  LWrittenCount: Longint;
begin
  { Perform a write directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := Write(Buffer, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    RaiseError;

  while (LTotalCount < Count) do
  begin
    { Try to write a contiguous block of <Count> size }
    LWrittenCount := Write(PByte(PByte(@Buffer) + LTotalCount)^,
      (Count - LTotalCount));

    { Check if we written something and decrease the number of bytes left to write }
    if LWrittenCount <= 0 then
      RaiseError
    else
      Inc(LTotalCount, LWrittenCount);
  end
end;

function TMicroStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result := 0;
  RaiseError;
end;

function TMicroStream.Seek(const Offset: Int64; Origin: Word): Int64;
begin
  Result := Seek(Offset, TSeekOrigin(Origin));
end;

procedure TMicroStream.SetPosition(const Pos: Int64);
begin
  Seek(Pos, soBeginning);
end;

procedure TMicroStream.SetSize(const NewSize: Int64);
begin
  // default = do nothing  (read-only streams, etc)
  // descendents should implement this method to call the Int64 sibling
end;

function TMicroStream.Skip(Amount: Integer): Integer;
var
  P: Integer;
begin
  P := Position;
  Result := Seek(Amount, soCurrent) - P;
end;

function TMicroStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

{ TMicroHandleStream }

constructor TMicroHandleStream.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TMicroHandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function TMicroHandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function TMicroHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FileSeek(FHandle, Offset, Ord(Origin));
end;

procedure TMicroHandleStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
  if not SetEndOfFile(FHandle) then
    raise ESetSizeError.Create('Error');
end;

{ TMicroFileStream }

constructor TMicroFileStream.Create(const AFileName: string; Mode: Word);
begin
  Create(AFilename, Mode, 0);
end;

constructor TMicroFileStream.Create(const AFileName: string; Mode: Word; Rights: Cardinal);
var
  LShareMode: Word;
begin
  if (Mode and fmCreate = fmCreate) then
  begin
    LShareMode := Mode and $FF;
    if LShareMode = $FF then
      LShareMode := fmShareExclusive; // For compat in case $FFFF passed as Mode
    inherited Create(FileCreate(AFileName, LShareMode, Rights));
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFCreateError.Create('Error');
  end
  else
  begin
    inherited Create(FileOpen(AFileName, Mode));
    if FHandle = INVALID_HANDLE_VALUE then
      raise EFOpenError.Create('error');
  end;
  FFileName := AFileName;
end;

destructor TMicroFileStream.Destroy;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    FileClose(FHandle);
  inherited Destroy;
end;

{ TMicroStrings }

constructor TMicroStrings.Create;
begin
  inherited Create;
  FDefaultEncoding := TEncoding.Default;
  FEncoding := nil;
  FWriteBOM := True;
end;

destructor TMicroStrings.Destroy;
begin
  if (FEncoding <> nil) and not TEncoding.IsStandardEncoding(FEncoding) then
    FreeAndNil(FEncoding);
  if not TEncoding.IsStandardEncoding(FDefaultEncoding) then
    FreeAndNil(FDefaultEncoding);
  inherited Destroy;
end;

function TMicroStrings.Add(const S: string): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TMicroStrings.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TMicroStrings.Append(const S: string);
begin
  Add(S);
end;

procedure TMicroStrings.AddStrings(Strings: TMicroStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do
      AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TMicroStrings.AddStrings(const Strings: TArray<string>);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      Add(Strings[I]);
  finally
    EndUpdate;
  end;
end;

procedure TMicroStrings.AddStrings(const Strings: TArray<string>; const Objects: TArray<TObject>);
var
  I: Integer;
begin
  if Length(Strings) <> Length(Objects) then
    raise EArgumentOutOfRangeException.Create('Error');
  BeginUpdate;
  try
    for I := Low(Strings) to High(Strings) do
      AddObject(Strings[I], Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TMicroStrings.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TMicroStrings.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;

function TMicroStrings.Equals(Strings: TMicroStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place params on the stack.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF O+}
procedure TMicroStrings.Error(const Msg: string; Data: Integer);
begin
  raise exception.Create(Msg);
end;

procedure TMicroStrings.Error(Msg: PResStringRec; Data: Integer);
begin
  raise exception.Create('Error');
end;
{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF OPTIMIZATIONSON}

procedure TMicroStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;

function TMicroStrings.ExtractName(const S: string): string;
var
  P: Integer;
begin
  Result := S;
  P := AnsiPos(NameValueSeparator, Result);
  if P <> 0 then
    SetLength(Result, P-1) else
    SetLength(Result, 0);
end;

function TMicroStrings.GetCapacity: Integer;
begin  // descendents may optionally override/replace this default implementation
  Result := Count;
end;

function TMicroStrings.GetCommaText: string;
var
  LOldDefined: TStringsDefined;
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    Result := GetDelimitedText;
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

function NextChar(P: PChar): PChar;
begin
  Result := P;
  if (Result <> nil) and (Result^ <> #0) then
  begin
    Inc(Result);
    if Result^.IsLowSurrogate then
      Inc(Result);
    while Result^.GetUnicodeCategory = TUnicodeCategory.ucNonSpacingMark do
      Inc(Result);
  end;
end;

function TMicroStrings.GetDelimitedText: string;
var
  S: string;
  P: PChar;
  I, Count: Integer;
  LDelimiters: set of AnsiChar;
  SB: TStringBuilder;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    if QuoteChar = #0 then
      Result := ''
    else
      Result := QuoteChar + QuoteChar
  else
  begin
    Result := '';
    if QuoteChar <> #0 then
    begin
      LDelimiters := [Char(#0), Char(QuoteChar), Char(Delimiter)];
      if not StrictDelimiter then
        LDelimiters := LDelimiters + [Char(#1)..Char(' ')];
    end;
    SB := TStringBuilder.Create;
    try
      for I := 0 to Count - 1 do
      begin
        S := Get(I);
        if QuoteChar <> #0 then
        begin
          P := PChar(S);
          while not (AnsiChar(P^) in LDelimiters) do
            P := NextChar(P);
          if (P^ <> #0) then S := AnsiQuotedStr(S, QuoteChar);
        end;
        SB.Append(S);
        SB.Append(Delimiter);
      end;
      if SB.Length > 0 then
        Result := SB.ToString(0, SB.Length - 1);
    finally
      SB.Free;
    end;
  end;
end;

function TMicroStrings.GetEnumerator: TMicroStringsEnumerator;
begin
  Result := TMicroStringsEnumerator.Create(Self);
end;

function TMicroStrings.GetName(Index: Integer): string;
begin
  Result := ExtractName(Get(Index));
end;

function TMicroStrings.GetObject(Index: Integer): TObject;
begin
  Result := nil;
end;

function TMicroStrings.GetText: PChar;
begin
  Result := StrNew(PChar(GetTextStr));
end;

function TMicroStrings.GetTextStr: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S, LB: string;
begin
  Count := GetCount;
  Size := 0;
  LB := LineBreak;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    L := Length(LB);
    if L <> 0 then
    begin
      System.Move(Pointer(LB)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
  end;
end;

function TMicroStrings.GetValue(const Name: string): string;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I >= 0 then
    Result := Copy(Get(I), Length(Name) + 2, MaxInt) else
    Result := '';
end;

function TMicroStrings.IndexOf(const S: string): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then Exit;
  Result := -1;
end;

function TMicroStrings.IndexOfName(const Name: string): Integer;
var
  P: Integer;
  S: string;
begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := AnsiPos(NameValueSeparator, S);
    if (P <> 0) and (CompareStrings(Copy(S, 1, P - 1), Name) = 0) then Exit;
  end;
  Result := -1;
end;

function TMicroStrings.IndexOfObject(AObject: TObject): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;

procedure TMicroStrings.InsertObject(Index: Integer; const S: string; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TMicroStrings.LoadFromFile(const FileName: string);
var
  Stream: TMicroStream;
begin
  Stream := TMicroFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TMicroStrings.LoadFromFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TMicroStream;
begin
  Stream := TMicroFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TMicroStrings.LoadFromStream(Stream: TMicroStream);
begin
  LoadFromStream(Stream, nil);
end;

procedure TMicroStrings.LoadFromStream(Stream: TMicroStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer, 0, Size);
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, FDefaultEncoding);
    SetEncoding(Encoding); // Keep Encoding in case the stream is saved
    SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
  finally
    EndUpdate;
  end;
end;

procedure TMicroStrings.Move(CurIndex, NewIndex: Integer);
var
  TempObject: TObject;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      PutObject(CurIndex, nil);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TMicroStrings.Put(Index: Integer; const S: string);
var
  TempObject: TObject;
begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;

procedure TMicroStrings.PutObject(Index: Integer; AObject: TObject);
begin
end;

procedure TMicroStrings.SaveToFile(const FileName: string);
begin
  SaveToFile(FileName, FEncoding);
end;

procedure TMicroStrings.SaveToFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TMicroStream;
begin
  Stream := TMicroFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TMicroStrings.SaveToStream(Stream: TMicroStream);
begin
  SaveToStream(Stream, FEncoding);
end;

procedure TMicroStrings.SaveToStream(Stream: TMicroStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
begin
  if Encoding = nil then
    Encoding := FDefaultEncoding;
  Buffer := Encoding.GetBytes(GetTextStr);
  if FWriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble, Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer, Length(Buffer));
end;

procedure TMicroStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendents may optionally implement this method
end;

procedure TMicroStrings.SetCommaText(const Value: string);
var
  LOldDefined: TStringsDefined;
  LOldDelimiter: Char;
  LOldQuoteChar: Char;
begin
  LOldDefined := FDefined;
  LOldDelimiter := FDelimiter;
  LOldQuoteChar := FQuoteChar;
  Delimiter := ',';
  QuoteChar := '"';
  try
    SetDelimitedText(Value);
  finally
    FDelimiter := LOldDelimiter;
    FQuoteChar := LOldQuoteChar;
    FDefined := LOldDefined;
  end;
end;

procedure TMicroStrings.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

procedure TMicroStrings.SetTextStr(const Value: string);
var
  P, Start, LB: PChar;
  S: string;
  LineBreakLen: Integer;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      if CompareStr(LineBreak, sLineBreak) = 0 then
      begin
        // This is a lot faster than using StrPos/AnsiStrPos when
        // LineBreak is the default (#13#10)
        while P^ <> #0 do
        begin
          Start := P;
          while not (AnsiChar(P^) in [#0, #10, #13]) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P^ = #13 then Inc(P);
          if P^ = #10 then Inc(P);
        end;
      end
      else
      begin
        LineBreakLen := Length(LineBreak);
        while P^ <> #0 do
        begin
          Start := P;
          LB := AnsiStrPos(P, PChar(LineBreak));
          while (P^ <> #0) and (P <> LB) do Inc(P);
          SetString(S, Start, P - Start);
          Add(S);
          if P = LB then
            Inc(P, LineBreakLen);
        end;
      end;
  finally
    EndUpdate;
  end;
end;

procedure TMicroStrings.SetUpdateState(Updating: Boolean);
begin
end;

procedure TMicroStrings.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then I := Add('');
    Put(I, Name + NameValueSeparator + Value);
  end else
  begin
    if I >= 0 then Delete(I);
  end;
end;

procedure TMicroStrings.SetDelimitedText(const Value: string);
var
  P, P1: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    if not StrictDelimiter then
      while (AnsiChar(P^) in [#1..' ']) do
        P := NextChar(P);
    while P^ <> #0 do
    begin
      if (P^ = QuoteChar) and (QuoteChar <> #0) then
        S := AnsiExtractQuotedStr(P, QuoteChar)
      else
      begin
        P1 := P;
        while ((not FStrictDelimiter and (P^ > ' ')) or
              (FStrictDelimiter and (P^ <> #0))) and (P^ <> Delimiter) do
          P := NextChar(P);
        SetString(S, P1, P - P1);
      end;
      Add(S);
      if not FStrictDelimiter then
        while (AnsiChar(P^) in [#1..' ']) do
          P := NextChar(P);

      if P^ = Delimiter then
      begin
        P1 := P;
        if NextChar(P1)^ = #0 then
          Add('');
        repeat
          P := NextChar(P);
        until not (not FStrictDelimiter and (AnsiChar(P^) in [#1..' ']));
      end;
    end;
  finally
    EndUpdate;
  end;
end;

function TMicroStrings.GetDelimiter: Char;
begin
  if not (sdDelimiter in FDefined) then
    Delimiter := ',';
  Result := FDelimiter;
end;

function TMicroStrings.GetLineBreak: string;
begin
  if not (sdLineBreak in FDefined) then
    LineBreak := sLineBreak;
  Result := FLineBreak;
end;

function TMicroStrings.GetQuoteChar: Char;
begin
  if not (sdQuoteChar in FDefined) then
    QuoteChar := '"';
  Result := FQuoteChar;
end;

function TMicroStrings.GetStrictDelimiter: Boolean;
begin
  if not (sdStrictDelimiter in FDefined) then
    StrictDelimiter := False;
  Result := FStrictDelimiter;
end;

procedure TMicroStrings.SetDefaultEncoding(const Value: TEncoding);
begin
  if not TEncoding.IsStandardEncoding(FDefaultEncoding) then
    FDefaultEncoding.Free;
  if TEncoding.IsStandardEncoding(Value) then
    FDefaultEncoding := Value
  else if Value <> nil then
    FDefaultEncoding := Value.Clone
  else
    FDefaultEncoding := TEncoding.Default;
end;

procedure TMicroStrings.SetDelimiter(const Value: Char);
begin
  if (FDelimiter <> Value) or not (sdDelimiter in FDefined) then
  begin
    Include(FDefined, sdDelimiter);
    FDelimiter := Value;
  end
end;

procedure TMicroStrings.SetEncoding(const Value: TEncoding);
begin
  if not TEncoding.IsStandardEncoding(FEncoding) then
    FEncoding.Free;
  if TEncoding.IsStandardEncoding(Value) then
    FEncoding := Value
  else if Value <> nil then
    FEncoding := Value.Clone
  else
    FEncoding := TEncoding.Default;
end;

procedure TMicroStrings.SetLineBreak(const Value: string);
begin
  if (FLineBreak <> Value) or not (sdLineBreak in FDefined) then
  begin
    Include(FDefined, sdLineBreak);
    FLineBreak := Value;
  end
end;

procedure TMicroStrings.SetQuoteChar(const Value: Char);
begin
  if (FQuoteChar <> Value) or not (sdQuoteChar in FDefined) then
  begin
    Include(FDefined, sdQuoteChar);
    FQuoteChar := Value;
  end
end;

procedure TMicroStrings.SetStrictDelimiter(const Value: Boolean);
begin
  if (FStrictDelimiter <> Value) or not (sdStrictDelimiter in FDefined) then
  begin
    Include(FDefined, sdStrictDelimiter);
    FStrictDelimiter := Value;
  end
end;

function TMicroStrings.CompareStrings(const S1, S2: string): Integer;
begin
  Result := AnsiCompareText(S1, S2);
end;

function TMicroStrings.GetNameValueSeparator: Char;
begin
  if not (sdNameValueSeparator in FDefined) then
    NameValueSeparator := '=';
  Result := FNameValueSeparator;
end;

procedure TMicroStrings.SetNameValueSeparator(const Value: Char);
begin
  if (FNameValueSeparator <> Value) or not (sdNameValueSeparator in FDefined) then
  begin
    Include(FDefined, sdNameValueSeparator);
    FNameValueSeparator := Value;
  end
end;

function TMicroStrings.GetValueFromIndex(Index: Integer): string;
var
  SepPos: Integer;
begin
  if Index >= 0 then
  begin
    Result := Get(Index);
    SepPos := AnsiPos(NameValueSeparator, Result);
    if (SepPos > 0) then
      System.Delete(Result, 1, SepPos)
    else
      Result := '';
  end
  else
    Result := '';
end;

procedure TMicroStrings.SetValueFromIndex(Index: Integer; const Value: string);
begin
  if Value <> '' then
  begin
    if Index < 0 then Index := Add('');
    Put(Index, Names[Index] + NameValueSeparator + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

function TMicroStrings.ToStringArray: TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Strings[I];
end;

function TMicroStrings.ToObjectArray: TArray<TObject>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Objects[I];
end;

{ TStringsEnumerator }

constructor TMicroStringsEnumerator.Create(AStrings: TMicroStrings);
begin
  inherited Create;
  FIndex := -1;
  FStrings := AStrings;
end;

function TMicroStringsEnumerator.GetCurrent: string;
begin
  Result := FStrings[FIndex];
end;

function TMicroStringsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FStrings.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TMicroStringList }

destructor TMicroStringList.Destroy;
var
  I: Integer;
  Temp: TArray<TObject>;
begin
  FOnChange := nil;
  FOnChanging := nil;

  // If the list owns the Objects gather them and free after the list is disposed
  if OwnsObjects then
  begin
    SetLength(Temp, FCount);
    for I := 0 to FCount - 1 do
      Temp[I] := FList[I].FObject;
  end;

  inherited Destroy;
  FCount := 0;
  SetCapacity(0);

  // Free the objects that were owned by the list
  if Length(Temp) > 0 then
    for I := 0 to Length(Temp) - 1 do
      Temp[I].DisposeOf;
end;

function TMicroStringList.Add(const S: string): Integer;
begin
  Result := AddObject(S, nil);
end;

function TMicroStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: raise exception.Create('Error');
      end;
  InsertItem(Result, S, AObject);
end;

procedure TMicroStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TMicroStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TMicroStringList.Clear;
var
  I: Integer;
  Temp: TArray<TObject>;
begin
  if FCount <> 0 then
  begin
    Changing;

    // If the list owns the Objects gather them and free after the list is disposed
    if OwnsObjects then
    begin
      SetLength(Temp, FCount);
      for I := 0 to FCount - 1 do
        Temp[I] := FList[I].FObject;
    end;

    FCount := 0;
    SetCapacity(0);

    // Free the objects that were owned by the list
    if Length(Temp) > 0 then
      for I := 0 to Length(Temp) - 1 do
        Temp[I].Free;

    Changed;
  end;
end;

procedure TMicroStringList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  if (Index < 0) or (Index >= FCount) then raise exception.Create('Error');
  Changing;
  // If this list owns its objects then free the associated TObject with this index
  if OwnsObjects then
    Obj := FList[Index].FObject
  else
    Obj := nil;

  // Direct memory writing to managed array follows
  //  see http://dn.embarcadero.com/article/33423
  // Explicitly finalize the element we about to stomp on with move
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(TMicroStringItem));
    // Make sure there is no danglng pointer in the last (now unused) element
    PPointer(@FList[FCount].FString)^ := nil;
    PPointer(@FList[FCount].FObject)^ := nil;
  end;
  if Obj <> nil then
    Obj.Free;
  Changed;
end;

procedure TMicroStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then raise exception.Create('Error');
  if (Index2 < 0) or (Index2 >= FCount) then raise exception.Create('Error');
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TMicroStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Pointer;
  Item1, Item2: PMicroStringItem;
begin
  Item1 := @FList[Index1];
  Item2 := @FList[Index2];
  Temp := Pointer(Item1^.FString);
  Pointer(Item1^.FString) := Pointer(Item2^.FString);
  Pointer(Item2^.FString) := Temp;
  Temp := Pointer(Item1^.FObject);
  Pointer(Item1^.FObject) := Pointer(Item2^.FObject);
  Pointer(Item2^.FObject) := Temp;
end;

function TMicroStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStrings(FList[I].FString, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TMicroStringList.Get(Index: Integer): string;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    raise exception.Create('Error');
  Result := FList[Index].FString;
end;

function TMicroStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TMicroStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TMicroStringList.GetObject(Index: Integer): TObject;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    raise exception.Create('Error');
  Result := FList[Index].FObject;
end;

procedure TMicroStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TMicroStringList.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S) else
    if not Find(S, Result) then Result := -1;
end;

procedure TMicroStringList.Insert(Index: Integer; const S: string);
begin
  InsertObject(Index, S, nil);
end;

procedure TMicroStringList.InsertObject(Index: Integer; const S: string;
  AObject: TObject);
begin
  if Sorted then raise exception.Create('Error');
  if (Index < 0) or (Index > FCount) then raise exception.Create('Error');
  InsertItem(Index, S, AObject);
end;

procedure TMicroStringList.InsertItem(Index: Integer; const S: string; AObject: TObject);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(TMicroStringItem));
  Pointer(FList[Index].FString) := nil;
  Pointer(FList[Index].FObject) := nil;
  FList[Index].FObject := AObject;
  FList[Index].FString := S;
  Inc(FCount);
  Changed;
end;

procedure TMicroStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then raise exception.Create('Error');
  if Cardinal(Index) >= Cardinal(FCount) then
    raise exception.Create('Error');
  Changing;
  FList[Index].FString := S;
  Changed;
end;

procedure TMicroStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    raise exception.Create('Error');
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;

procedure TMicroStringList.QuickSort(L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
          ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TMicroStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    raise exception.Create('Error');
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TMicroStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TMicroStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListCompareStrings(List: TMicroStringList; Index1, Index2: Integer): Integer;
begin
  Result := List.CompareStrings(List.FList[Index1].FString,
                                List.FList[Index2].FString);
end;

procedure TMicroStringList.Sort;
begin
  CustomSort(StringListCompareStrings);
end;

procedure TMicroStringList.CustomSort(Compare: TStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TMicroStringList.CompareStrings(const S1, S2: string): Integer;
begin
  if CaseSensitive then
    Result := AnsiCompareStr(S1, S2)
  else
    Result := AnsiCompareText(S1, S2);
end;

constructor TMicroStringList.Create;
begin
  inherited Create;
end;

constructor TMicroStringList.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObject := OwnsObjects;
end;

procedure TMicroStringList.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then
    begin
      // Calling Sort won't sort the list because CustomSort will
      // only sort the list if it's not already sorted
      Sorted := False;
      Sorted := True;
    end;
  end;
end;

end.
