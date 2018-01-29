unit adot.Tools.IO;

interface

{
  TCustomStreamExt = class
    Extensions of TStream.

  TDelegatedMemoryStream = class
    Readonly stream for specified memory block.

  TArrayStream<T: record> = class
    Makes any array of ordinal type to be accessable as stream of bytes.

  TStreamUtils = class
    Block reader and other utils.

  TPIReader = class
    Platform independent stream reader.

  TPIWriter = class
    Platform independent stream writer.

  TReader = record
    Stream block reader (ReadNext to get next block of data from stream as array of byte)

  TFileUtils = class
    File manipulation utils.

  TBuffer = record
    Simple and fast managed analog of TMemoryStream.

}

uses
  adot.Types,
  adot.Collections,
  adot.Collections.Sets,
  {$If Defined(MSWindows)}
    { Option "$HINTS OFF" doesn't hide following hint (Delphi 10.2.1), we add Windows here to suppress it:
      H2443 Inline function 'RenameFile' has not been expanded because unit 'Winapi.Windows' is not specified in USES list }
    Winapi.Windows,
  {$EndIf}
  {$If Defined(POSIX)}
    Posix.Unistd,
    Posix.Stdio,
  {$EndIf}
  System.SysConst,
  System.Classes,
  System.SysUtils,
  System.Math,
  System.DateUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Types,
  System.IOUtils;

type
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

  { Block reader and other utils }
  TStreamUtils = class
  public
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
    procedure Init;

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

implementation

uses
  adot.Tools;

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

procedure TDelegatedMemoryStream.SetSize(NewSize: Longint);
begin
  raise Exception.Create('Error');
end;

procedure TDelegatedMemoryStream.SetSize(const NewSize: Int64);
begin
  raise Exception.Create('Error');
end;

function TDelegatedMemoryStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise Exception.Create('Error');
end;

function TDelegatedMemoryStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  raise Exception.Create('Error');
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

function TArrayStream<T>.Read(var Buffer; Count: Longint): Longint;
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

function TArrayStream<T>.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
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

function TArrayStream<T>.Write(const Buffer; Count: Longint): Longint;
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

function TArrayStream<T>.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := Write(Buffer[Offset], Count);
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

function TCustomReadOnlyStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

procedure TCustomReadOnlyStream.SetSize(const NewSize: Int64);
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

procedure TCustomReadOnlyStream.SetSize(NewSize: Longint);
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

function TCustomReadOnlyStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
end;

function TCustomReadOnlyStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  raise EAbstractError.CreateRes(@SAbstractError);
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
  {$IFDEF MSWINDOWS}
  if AMode and fmShareDenyRead  = fmShareDenyRead  then result := result + ', fmShareDenyRead';
  {$EndIf}
  if AMode and fmShareDenyNone  = fmShareDenyNone  then result := result + ', fmShareDenyNone';
  result := result + '}';
  {$WARN SYMBOL_PLATFORM ON}
end;

class function TFileUtils.AccessAllowed(const AFileName: string; ADesiredAccess: word): boolean;
var
  F: TFileStream;
begin
  try
    F := TFileStream.Create(AFileName, ADesiredAccess);
    F.Free;
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
  DisabledChars = [0..31, Byte('<'), Byte('>'), Byte(':'), Byte('"'), Byte('/'), Byte('\'), Byte('|'), Byte('?'), Byte('*')];
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
    if (c<#127) and (Byte(c) in DisabledChars) then
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

procedure TBuffer.Init;
begin
  Self := Default(TBuffer);
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
    Capacity := Sys.Max(MinCapacity, Capacity shl 1, 16);
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

end.
