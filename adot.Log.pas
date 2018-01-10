unit adot.Log;
(*
  Example 1:

  Uses
    adot.Log, VCL.Log;

  procedure TFormMain.FormCreate(Sender: TObject);
  begin
    AppLog := TMixLog.Create([
      TVCLStringsLog.Create(MemoLog.Lines),
      TSyncFileLog.Create(ChangeFileExt(ParamStr(0), '.log'))
    ]);
  end;

  Example 2:

    uses
      adot.Log;

    begin
      InitDefaultLogger;
      AppLog.Add('test');
    end;
*)
interface

uses
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.IOUtils,
  System.Generics.Collections,
  System.TypInfo;

type
  // All descandants are thread-safe.
  TCustomLog = class
  public
    type
      TInfoType = (itInfo, itWarning, itError, itException);
      TLogStrPrefix = function: string;
      TLogPlacement = (lpCommonFolder, lpUserFolder);
    const
      LogPrefix: array[TInfoType] of string = (
        'INF', 'WAR', 'ERR', 'EXC');

  protected
    FHeaderSent: Boolean;
    FLinePrefix: TLogStrPrefix;
    FEnabled: Boolean;
    FOffset: integer;

    function LogLinesPrefix: String; virtual;
    procedure LogSysInfo; virtual;

    // to be implemented in descendants
    procedure Send(AData: pointer; ASize: Integer); virtual; abstract;
    procedure DoFlush; virtual; abstract;
    class function DataAsString(AData: pointer; ASize: integer): string; static;

    procedure DoAdd(const Msg: string; InfoType: TInfoType = itInfo);
  public

    constructor Create;

    procedure BeginBlock(const BlockName: string);
    procedure EndBlock;
    procedure Add(const Msg: string; InfoType: TInfoType = itInfo); overload; virtual;
    procedure Add(const Msg: string; const Args: array of const; InfoType: TInfoType = itInfo); overload; virtual;
    procedure Add(const Msg: TArray<string>; InfoType: TInfoType = itInfo); overload;
    procedure Add(const Msg: TEnumerable<string>; InfoType: TInfoType = itInfo); overload;
    procedure Add(const Exc: Exception); overload;

    procedure Flush; virtual;

    // temporarily disable logging (all output will be ignored)
    property Enabled: Boolean read FEnabled write FEnabled;

    // Can be used to define custom (platform dependant - including
    // ProcessID/threadID  etc) line header.
    property LinePrefix: TLogStrPrefix read FLinePrefix write FLinePrefix;

    // GetLogFileName(lpCommonFolder, 'CompanyName\ProductName') =
    // 'C:\ProgramData\Logs\CompanyName\ProductName\Demo.exe.20150401.230829.384.log'
    // GetLogFileName(lpUserFolder, 'CompanyName\ProductName') =
    // 'C:\ProgramData\Logs\CompanyName\ProductName\Demo.exe.20150401.230933.480.log'
    class function GetLogFileName(const APlacement: TLogPlacement = lpCommonFolder;
      const LogSubpath: string = ''; AIncludePath: Boolean = True): String; static;
    class function GetLogFolder(const APlacement: TLogPlacement = lpCommonFolder;
      const LogSubpath: string = ''): String; static;
    class procedure DeleteOldLogFiles(
      const APlacement: TLogPlacement = lpCommonFolder;
      const LogSubpath: string = '';
      const FileMask: string = '*.log';
      MaxAllowedTotalSize : integer =  {$IFDEF Debug} 100*1024 {$ELSE} 3*1024*1024 {$ENDIF};
      MaxAllowedTotalCount: integer =  {$IFDEF Debug} 100      {$ELSE} 1000        {$ENDIF};
      CleanUpChancePercent: integer =  {$IFDEF Debug} 100      {$ELSE} 10          {$ENDIF}); static;
    class function CreateOrAppendFile(const ALogFileName: string): TFileStream; static;
  end;

  TCustomDelegatedLog = class(TCustomLog)
  public
    type
      TOnLogMessage = reference to procedure(const AMsg: string);
  protected
    FOnLogMessage: TOnLogMessage;

    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;
  public
    constructor Create(AOnLogMessage: TOnLogMessage);

    property OnLogMessage: TOnLogMessage read FOnLogMessage write FOnLogMessage;
  end;

  // If you need logging inside of thread with Synchronize calls, consider
  // to use TVCLDelegatedLog (from VCL.Log.pas) insead of TDelegatedLog.
  TDelegatedLog = class(TCustomDelegatedLog)
  protected
    FIgnoreNotMainThread: Boolean;

    procedure Send(AData: pointer; ASize: Integer); override;
  public
    constructor Create(AIgnoreNotMainThread: Boolean;
      AOnLogMessage: TCustomDelegatedLog.TOnLogMessage);

    property IgnoreNotMainThread: Boolean read FIgnoreNotMainThread write FIgnoreNotMainThread;
  end;

  // Duplicate all log output to several logs, for
  // example send all logging to both, file and VCL-controls.
  TMixLog = class(TCustomLog)
  protected
    FLogs: TObjectList<TCustomLog>;

    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;

    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(const Value: boolean);
  public
    constructor Create; overload;
    constructor Create(const ALogs: array of TCustomLog); overload;
    destructor Destroy; override;
    procedure AddLogger(ALog: TCustomLog);
    procedure Remove(ALog: TCustomLog);

    property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
  end;

  // Simple and fast logger (buffered + asynchronous).
  // Under some conditions Delphi threading library fails in DLL, consider
  // to use logger withour treading for such DLLs (injected DLL etc).
  // Thread-safe.
  TFileLog = class(TCustomLog)
  protected
    type
      TLogThread = class(TThread)
      protected
        Stream: TStream;
        OwnsStream: Boolean;
        Buffer: TMemoryStream;
        Trigger: TEvent;
        Mutex: TMutex;
        AutoFlushMS: Cardinal;
        MaxBufSize: Cardinal;
      public
        constructor Create(ADstStream: TStream; AOwnsStream: Boolean;
          AAutoFlushMS: cardinal = 9000; AMaxBufSize: Cardinal = 4096);
        procedure Write(AData: pointer; ASize: Integer);
        procedure DoFlush;
        destructor Destroy; override;
        procedure Execute; override;
      end;

    var
      LogThread: TLogThread;

    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;

  public
    constructor Create(ADstStream: TStream; AOwnsStream: Boolean); overload;
    constructor Create(ADstFileName: string); overload;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  // Simplest and most reliable logger, all logged data became visible
  // immediately, but not so fast as buffered/threaded loggers, use it only if
  // app is not supposed to log a lot of information.
  // Thread-safe.
  TSyncFileLog = class(TCustomLog)
  protected
    Stream: TStream;
    OwnsStream: Boolean;
    CS: TCriticalSection;

    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;

  public
    constructor Create(ADstStream: TStream; AOwnsStream: Boolean); overload;
    constructor Create(ADstFileName: string); overload;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  // Buffered but synchronous logger (faster than TSyncFileLog, but still not
  // so fast as TFileLog).
  // Thread-safe.
  TSyncBufFileLog = class(TSyncFileLog)
  protected
    Buffer: array[0..65535] of Byte;
    BufPos: integer;

    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;
  public
    destructor Destroy; override;
  end;

  // If creation of real logger is not possible (no access to file system etc),
  // it can be usefull create empty logger to keep code simple and readable.
  // Thread-safe.
  TNullLog = class(TCustomLog)
  protected
    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;
  public
    constructor Create; overload;
  end;
  
var
  AppLog: TCustomLog;

function CreateDefaultLogger: TCustomLog;
procedure InitDefaultLogger;
procedure AddLogger(ALogger: TCustomLog);
procedure ReplaceLogger(ALogger: TCustomLog);

implementation

uses
  adot.Tools,
  adot.Tools.Rtti;

{ TCustomLog }

procedure TCustomLog.DoAdd(const Msg: string; InfoType: TInfoType);
var
  S: string;
  utf8: RawByteString;
begin
  if not Enabled then
    Exit;
  if not FHeaderSent then
    LogSysInfo;
  S := LogLinesPrefix + ' ' + LogPrefix[InfoType] + ' ' + StringOfChar(' ',FOffset*2) + Msg + #13#10;
  utf8 := UTF8Encode(S);
  Send(@utf8[Low(utf8)], Length(utf8));
end;

procedure TCustomLog.Add(const Msg: string; InfoType: TInfoType = itInfo);
begin
  if not Enabled then
    Exit;
  DoAdd(Msg, InfoType);
end;

procedure TCustomLog.Add(const Msg: string; const Args: array of const;
  InfoType: TInfoType = itInfo);
begin
  if not Enabled then
    Exit;
  DoAdd(Format(Msg, Args), InfoType);
end;

procedure TCustomLog.Add(const Msg: TArray<string>; InfoType: TInfoType);
var
  S: string;
  P: PChar;
  I,J: Integer;
begin
  if not Enabled then
    Exit;
  if Length(Msg) = 0 then
    Exit;
  J := (Length(Msg)-1)*2;
  for I := Low(Msg) to High(Msg) do
    inc(J, Length(Msg[I]));
  SetLength(S, J);
  P := PChar(S);
  for I := Low(Msg) to High(Msg) do
  begin
    if I > Low(Msg) then
    begin
      P^ := #13; inc(P);
      P^ := #10; inc(P);
    end;
    J := Length(Msg[I]);
    if J > 0 then
    begin
      System.Move(Msg[I][Low(Msg[I])], P^, J*SizeOf(Char));
      inc(P, J);
    end;
  end;
  Assert(P-PChar(S)=Length(S));
  DoAdd(S, InfoType);
end;

procedure TCustomLog.Add(const Msg: TEnumerable<string>; InfoType: TInfoType);
begin
  if not Enabled then
    Exit;
  Add(Msg.ToArray, InfoType);
end;

procedure TCustomLog.Add(const Exc: Exception);
begin
  if not Enabled then
    Exit;
  Add('%s: %s',[Exc.ClassName, Exc.Message], itException);
end;

procedure TCustomLog.BeginBlock(const BlockName: string);
begin
  DoAdd(BlockName);
  DoAdd('{');
  inc(FOffset);
end;

procedure TCustomLog.EndBlock;
begin
  assert(FOffset>0);
  dec(FOffset);
  DoAdd('}');
end;

constructor TCustomLog.Create;
begin
  FEnabled := True;
end;

class function TCustomLog.CreateOrAppendFile(
  const ALogFileName: string): TFileStream;
begin
  if not FileExists(ALogFileName) then
  begin
    TDirectory.CreateDirectory(ExtractFilePath(ALogFileName));
    Result := TFileStream.Create(ALogFileName, fmCreate or fmShareDenyWrite);
  end
  else
  begin
    Result := TFileStream.Create(ALogFileName, fmOpenWrite or fmShareDenyWrite);
    Result.Seek(0, soFromEnd);
  end;
end;

procedure TCustomLog.Flush;
begin
  if not Enabled then
    Exit;
  DoFlush;
end;

function TCustomLog.LogLinesPrefix: String;
begin
  if Assigned(LinePrefix) then
    Result := LinePrefix
  else
    Result := FormatDateTime('dd.mm.yy hh:mm:ss.zzz', Now);
end;

procedure TCustomLog.LogSysInfo;

  function ParamToStr(const Par: string): String;
  begin
    if Par.Contains(' ') then
      Result := '"'+Par+'"'
    else
      Result := Par;
  end;

var
  i: Integer;
  s: string;
begin
  if not Enabled or FHeaderSent then
    Exit;
  FHeaderSent := True;
  Add('Platform: %s; Architecture: %s; OS: %s', [
    TEnumeration<TOSVersion.TPlatform>.ToString(TOSVersion.Platform).Substring(2),
    TEnumeration<TOSVersion.TArchitecture>.ToString(TOSVersion.Architecture).Substring(2),
    TOSVersion.ToString
  ]);
  s := ParamStr(0);
  for i := 1 to ParamCount do
    s := s + ' ' + ParamToStr(ParamStr(i));
  Add(s);
end;

class procedure TCustomLog.DeleteOldLogFiles(
  const APlacement: TLogPlacement = lpCommonFolder;
  const LogSubpath: string = '';
  const FileMask: string = '*.log';
  MaxAllowedTotalSize : integer =  {$IFDEF Debug} 100*1024 {$ELSE} 3*1024*1024 {$ENDIF};
  MaxAllowedTotalCount: integer =  {$IFDEF Debug} 100      {$ELSE} 1000        {$ENDIF};
  CleanUpChancePercent: integer =  {$IFDEF Debug} 100      {$ELSE} 10          {$ENDIF});
begin
  TFileUtils.CleanUpOldFiles(
    GetLogFolder(APlacement, LogSubpath) + FileMask,
    MaxAllowedTotalSize,
    MaxAllowedTotalCount,
    CleanUpChancePercent/100);
end;

class function TCustomLog.DataAsString(AData: pointer; ASize: integer): string;
var
  u: RawByteString;
begin
  SetLength(u, ASize);
  move(AData^, u[low(u)], length(u));
  result := UTF8ToString(u);
end;

class function TCustomLog.GetLogFileName(const APlacement: TLogPlacement = lpCommonFolder;
  const LogSubpath: string = ''; AIncludePath: Boolean = True): String;
begin
  Result := Format('%s.%s.log', [
    ExtractFileName(ParamStr(0)),
    FormatDateTime('yyyymmdd.hhmmss.zzz', Now)
  ]);
  if AIncludePath then
    Result := GetLogFolder(APlacement, LogSubpath) + Result;
end;

class function TCustomLog.GetLogFolder(const APlacement: TLogPlacement = lpCommonFolder;
  const LogSubpath: string = ''): String;
begin
  //  GetSharedDocumentsPath = C:\Users\Public\Documents
  //  GetPublicPath          = C:\ProgramData
  //  GetHomePath            = C:\Users\<username>\AppData\Roaming
  {$IF [Low(APlacement)..High(APlacement)]<>[lpCommonFolder, lpUserFolder]}
  Fix it here!
  {$ENDIF}
  case APlacement of
    lpCommonFolder:
      Result := TPath.GetPublicPath;
    lpUserFolder:
      Result := TPath.GetHomePath;
  end;
  Result := IncludeTrailingPathDelimiter(TPath.GetPublicPath) + 'Logs\' + Trim(LogSubpath);
  Result := IncludeTrailingPathDelimiter(Result);
end;

{ TFileLog }

constructor TFileLog.Create(ADstStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  LogThread := TLogThread.Create(ADstStream, AOwnsStream);
end;

constructor TFileLog.Create(ADstFileName: string);
begin
  Create(CreateOrAppendFile(ADstFileName), True);
end;

constructor TFileLog.Create;
begin
  Create(GetLogFileName);
end;

destructor TFileLog.Destroy;
begin
  inherited;
  FreeAndNil(LogThread);
end;

procedure TFileLog.DoFlush;
begin
  LogThread.DoFlush;
end;

procedure TFileLog.Send(AData: pointer; ASize: Integer);
begin
  LogThread.Write(AData, ASize);
end;

{ TFileLog.TLogThread }

constructor TFileLog.TLogThread.Create(ADstStream: TStream; AOwnsStream: Boolean;
  AAutoFlushMS: cardinal = 9000; AMaxBufSize: Cardinal = 4096);
begin
  AutoFlushMs := AAutoFlushMS;
  MaxBufSize := AMaxBufSize;
  Stream := ADstStream;
  Buffer := TMemoryStream.Create;
  Trigger := TEvent.Create(nil, False, False, '');
  Mutex := TMutex.Create;
  inherited Create;
end;

destructor TFileLog.TLogThread.Destroy;
begin
  Terminate;
  DoFlush;
  inherited;
  FreeAndNil(Buffer);
  FreeAndNil(Trigger);
  FreeAndNil(Mutex);
  if OwnsStream then
    FreeAndNil(Stream)
  else
    Stream := nil;
end;

procedure TFileLog.TLogThread.Execute;

  procedure DoFlush;
  begin
    Mutex.Acquire;
    try
      if Buffer.Size>0 then
        Stream.Write(Buffer.Memory^, Buffer.Size);
      Buffer.Size := 0;
    finally
      Mutex.Release;
    end;
  end;

begin
  repeat
    Trigger.WaitFor(AutoFlushMs);
    DoFlush;
  until Terminated;
  DoFlush;
end;

procedure TFileLog.TLogThread.Write(AData: pointer; ASize: Integer);
begin
  Mutex.WaitFor(Infinite);
  try
    Buffer.Write(AData^, ASize);
    if Buffer.Size>=MaxBufSize then
      DoFlush;
  finally
    Mutex.Release;
  end;
end;

procedure TFileLog.TLogThread.DoFlush;
begin
  if Trigger<>nil then
    Trigger.SetEvent;
end;

{ TSyncFileLog }

constructor TSyncFileLog.Create(ADstStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create;
  Stream := ADstStream;
  OwnsStream := AOwnsStream;
  CS := TCriticalSection.Create;
end;

constructor TSyncFileLog.Create(ADstFileName: string);
begin
  Create(CreateOrAppendFile(ADstFileName), True);
end;

constructor TSyncFileLog.Create;
begin
  Create(GetLogFileName);
end;

destructor TSyncFileLog.Destroy;
begin
  inherited;
  if OwnsStream then
    FreeAndNil(Stream)
  else
    Stream := nil;
  FreeAndNil(CS);
end;

procedure TSyncFileLog.DoFlush;
begin
  // There is no buffering, we always are flushing...
end;

procedure TSyncFileLog.Send(AData: pointer; ASize: Integer);
begin
  CS.Enter;
  try
    Stream.Write(AData^, ASize);
  finally
    CS.Leave;
  end;
end;

{ TNullLog }

constructor TNullLog.Create;
begin
  Inherited Create;
  Enabled := False;
end;

procedure TNullLog.DoFlush;
begin
end;

procedure TNullLog.Send(AData: pointer; ASize: Integer);
begin
end;

{ TSyncBufFileLog }

destructor TSyncBufFileLog.Destroy;
begin
  DoFlush;
  inherited;
end;

procedure TSyncBufFileLog.Send(AData: pointer; ASize: Integer);
begin
  CS.Enter;
  try
    if SizeOf(Buffer)-BufPos-ASize<0 then
    begin
      inherited Send(@Buffer, BufPos);
      BufPos := 0;
      if ASize>=SizeOf(Buffer) then
      begin
        inherited Send(AData, ASize);
        Exit;
      end
    end;
    Move(AData^, Buffer[BufPos], ASize);
    inc(BufPos, ASize);
  finally
    CS.Leave;
  end;
end;

procedure TSyncBufFileLog.DoFlush;
begin
  CS.Enter;
  try
    if BufPos>0 then
      inherited Send(@Buffer, BufPos);
    BufPos := 0;
  finally
    CS.Leave;
  end;
end;

{ TDelegatedLog }

constructor TDelegatedLog.Create(AIgnoreNotMainThread: Boolean;
  AOnLogMessage: TCustomDelegatedLog.TOnLogMessage);
begin
  FIgnoreNotMainThread := AIgnoreNotMainThread;
  inherited Create(AOnLogMessage);
end;

procedure TDelegatedLog.Send(AData: pointer; ASize: Integer);
begin
  if FIgnoreNotMainThread and (TThread.CurrentThread.ThreadID<>MainThreadID) then
    Exit;
  inherited Send(AData, ASize);
end;

{ TMixLog }

constructor TMixLog.Create;
begin
  inherited Create;
  FLogs := TObjectList<TCustomLog>.Create;
end;

constructor TMixLog.Create(const ALogs: array of TCustomLog);
var
  i: Integer;
begin
  Create;
  for i := Low(ALogs) to High(ALogs) do
    AddLogger(ALogs[i]);
end;

procedure TMixLog.AddLogger(ALog: TCustomLog);
begin
  FLogs.Add(ALog);
end;

procedure TMixLog.Remove(ALog: TCustomLog);
begin
  FLogs.Remove(ALog);
end;

destructor TMixLog.Destroy;
begin
  FreeAndNil(FLogs);
  inherited;
end;

procedure TMixLog.SetOwnsObjects(const Value: boolean);
begin
  FLogs.OwnsObjects := Value;
end;

function TMixLog.GetOwnsObjects: boolean;
begin
  result := FLogs.OwnsObjects;
end;

procedure TMixLog.DoFlush;
begin
end;

procedure TMixLog.Send(AData: pointer; ASize: Integer);
var
  i: Integer;
begin
  for i := 0 to FLogs.Count-1 do
    FLogs[i].Send(AData, ASize);
end;

{ TCustomDelegatedLog }

constructor TCustomDelegatedLog.Create(AOnLogMessage: TOnLogMessage);
begin
  FOnLogMessage := AOnLogMessage;
  inherited Create;
end;

procedure TCustomDelegatedLog.DoFlush;
begin
end;

procedure TCustomDelegatedLog.Send(AData: pointer; ASize: Integer);
begin
  if Assigned(FOnLogMessage) then
    FOnLogMessage(DataAsString(AData, ASize));
end;

function CreateDefaultLogger: TCustomLog;
begin
  result := TSyncFileLog.Create(TCustomLog.GetLogFileName(lpCommonFolder, ''));
end;

procedure InitDefaultLogger;
begin
  if AppLog=nil then
    AppLog := CreateDefaultLogger;
end;

procedure AddLogger(ALogger: TCustomLog);
begin
  InitDefaultLogger;
  if AppLog is TMixLog then
    TMixLog(AppLog).AddLogger(ALogger)
  else
    AppLog := TMixLog.Create([
      AppLog,
      ALogger
     ]);
end;

procedure ReplaceLogger(ALogger: TCustomLog);
begin
  FreeAndNil(AppLog);
  AppLog := ALogger;
end;

initialization
  InitDefaultLogger;

end.
