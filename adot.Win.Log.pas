unit adot.Win.Log;

interface

uses
  Winapi.Windows,
  adot.Win.Tools,
  adot.Tools,
  adot.Log,
  adot.Strings,
  System.SysUtils,
  System.SyncObjs,
  System.Math,
  System.Classes,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  // buffered + asynchronous, thread-safe.
  // most general class for logging.
  TWinFileLog = class(TFileLog)
  protected
    procedure LogSysInfo; override;
    function LogLinesPrefix: String; override;
  end;

  // NOT buffered, NOT asynchronous, thread-safe
  // Use it only if you need to see result immediately.
  TWinSyncFileLog = class(TSyncFileLog)
  protected
    procedure LogSysInfo; override;
    function LogLinesPrefix: String; override;
  end;

  // buffered, NOT asynchronous, thread-safe
  // Use it only if multithreading is not allowed for some reason.
  TWinSyncBufFileLog = class(TSyncBufFileLog)
  protected
    procedure LogSysInfo; override;
    function LogLinesPrefix: String; override;
  end;

  TWinNullLog = TNullLog;

  // Makes any logger to be GetLastError-transparent (for logging from injected
  // DLLs etc). Usually it will be TSysErrorTransparentLog
  TSysErrorTransparentLog = class(TCustomLog)
  protected
    type
      TCustomLogH = class(TCustomLog);
    var
      FLog: TCustomLogH;
      FOwns: Boolean;

    procedure LogSysInfo; override;
    function LogLinesPrefix: String; override;
    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;
  public
    constructor Create(ALog: TCustomLog; AOwns: Boolean);
    destructor Destroy; override;
  end;

  // single server, multiple clients (same machine, any process).
  TSharedMemoryLog = class
  public
    type
      // Receives data from client and sends to the actual log.
      TOnNewLog = reference to function(const ALogfileName: string): TCustomLog;
      TCustomLogH = class(TCustomLog);
      TCmd = (cmdSend, cmdFlush, cmdClose);
      PSharedMemoryLogRec = ^TSharedMemoryLogRec;
      TSharedMemoryLogRec = record
        // Clients should not perform any operations with closed server,
        // all wait operations will never signalated. Normally clients should
        // be shut down before server is closed.
        ServerClosed: Boolean; // w: server

        // Every server object generated uniqueue session ID.
        // Named synchronization objects are based on this value
        // (check EventCanReadName/EventCanWriteName).
        SessionId: TGUID; // w: server

        // Information to be send from client to server.
        MsgCmd: TCmd;
        LogNameLen: integer; // length in chars (not bytes!)
        LogName: array[0..255] of Char;
        MsgSize: integer;    // size in bytes
        MsgData: array[0..2047] of Byte;

        function BaseName: string;
        function EventCanReadName: string;
        function EventCanWriteName: string;

        procedure SetLogName(const AName: string);
        function GetLogName: string;
      end;

      // Instead of logging it sends data to server.
      TClient = class(TCustomLog)
      protected
        const
          WaitWriteBufMs: cardinal = infinite; //3000;
        var
          FSharedMem: PSharedMemoryLogRec;
          FEvtMsgCanRead: THandle;
          FEvtMsgCanWrite: THandle;
          FLogFileName: String;
          FCS: TCriticalSection;

        procedure Send(AData: pointer; ASize: Integer); override;
        procedure DoFlush; override;

        procedure SendClose;
        function AquireMsgQueue: Boolean;
        procedure ReleaseMsgQueue;

        procedure LogSysInfo; override;
        function LogLinesPrefix: String; override;
      public
        constructor Create(ASharedMem: PSharedMemoryLogRec; const ALogFileName: string);
        destructor Destroy; override;
      end;

      TServer = class
      protected
        type
          TLogThread = class(TThread)
          protected
            FSharedMem: PSharedMemoryLogRec;
            FEvtMsgCanRead: TEvent;
            FEvtMsgCanWrite: TEvent;
            FLogs: TObjectDictionary<String, TCustomLog>;
            FSrvStarted: TEvent;
          public
            constructor Create(ASharedMem: PSharedMemoryLogRec);
            destructor Destroy; override;
            procedure Execute; override;
          end;

        var
          SrvLog: TLogThread;
      public
        constructor Create(ASharedMem: PSharedMemoryLogRec; ANewLog: TOnNewLog);
        destructor Destroy; override;
      end;
  end;

implementation

function PCharLen(S: PChar; AMaxLen: integer): integer;
var
  i: Integer;
begin
  for i := 0 to AMaxLen-1 do
    if S[i]=#0 then
    begin
      Result := i;
      Exit;
    end;
  Result := AMaxLen;
end;

function LogLineExtraPrefix: string;
begin
  result := ' ' + TStr.IntToString(GetCurrentThreadId, 9);
end;

procedure LogExtraSysInfo(ADst: TCustomLog);
begin
  ADst.Log('PID='+IntToStr(GetCurrentProcessId));
  ADst.Log('Ingerity level: '+TProcess.GetIntegrityLevel);
end;

{ TWinFileLog }

function TWinFileLog.LogLinesPrefix: String;
begin
  result := inherited LogLinesPrefix + LogLineExtraPrefix;
end;

procedure TWinFileLog.LogSysInfo;
begin
  inherited;
  LogExtraSysInfo(Self);
end;

{ TWinSyncFileLog }

function TWinSyncFileLog.LogLinesPrefix: String;
begin
  result := inherited LogLinesPrefix + LogLineExtraPrefix;
end;

procedure TWinSyncFileLog.LogSysInfo;
begin
  inherited;
  LogExtraSysInfo(Self);
end;

{ TWinSyncBufFileLog }

function TWinSyncBufFileLog.LogLinesPrefix: String;
begin
  result := inherited LogLinesPrefix + LogLineExtraPrefix;
end;

procedure TWinSyncBufFileLog.LogSysInfo;
begin
  inherited;
  LogExtraSysInfo(Self);
end;

{ TSysErrorTransparentLog }

constructor TSysErrorTransparentLog.Create(ALog: TCustomLog; AOwns: Boolean);
begin
  FLog := TCustomLogH(ALog);
  FOwns := AOwns;
  inherited Create;
end;

destructor TSysErrorTransparentLog.Destroy;
begin
  inherited;
  if FOwns then
    FreeAndNil(FLog)
  else
    FLog := nil;
end;

procedure TSysErrorTransparentLog.DoFlush;
var
  LastError: Cardinal;
begin
  LastError := GetLastError;
  FLog.DoFlush;
  if LastError<>GetLastError then
    SetLastError(LastError);
end;

function TSysErrorTransparentLog.LogLinesPrefix: String;
var
  LastError: Cardinal;
begin
  LastError := GetLastError;
  Result := FLog.LogLinesPrefix;
  if LastError<>GetLastError then
    SetLastError(LastError);
end;

procedure TSysErrorTransparentLog.LogSysInfo;
var
  LastError: Cardinal;
begin
  if FHeaderSent then
    Exit;
  FHeaderSent := True;
  LastError := GetLastError;
  FLog.LogSysInfo;
  if LastError<>GetLastError then
    SetLastError(LastError);
end;

procedure TSysErrorTransparentLog.Send(AData: pointer; ASize: Integer);
var
  LastError: Cardinal;
begin
  LastError := GetLastError;
  FLog.Send(AData, ASize);
  if LastError<>GetLastError then
    SetLastError(LastError);
end;

{ TSharedMemoryLog.TSharedMemoryLogRec }

function TSharedMemoryLog.TSharedMemoryLogRec.BaseName: string;
begin
  result := THex.Encode(SessionId, SizeOf(SessionId));
end;

function TSharedMemoryLog.TSharedMemoryLogRec.EventCanReadName: string;
begin
  Result := 'EvtRd' + BaseName;
end;

function TSharedMemoryLog.TSharedMemoryLogRec.EventCanWriteName: string;
begin
  Result := 'EvtWr' + BaseName;
end;

procedure TSharedMemoryLog.TSharedMemoryLogRec.SetLogName(const AName: string);
begin
  LogNameLen := Min(Length(AName), Length(LogName));
  Move(AName[Low(AName)], LogName, LogNameLen*SizeOf(Char));
end;

function TSharedMemoryLog.TSharedMemoryLogRec.GetLogName: string;
begin
  SetLength(Result, LogNameLen);
  Move(LogName, Result[Low(Result)], LogNameLen*SizeOf(Char));
end;

{ TSharedMemoryLog.TClient }

constructor TSharedMemoryLog.TClient.Create(ASharedMem: PSharedMemoryLogRec; const ALogFileName: string);
begin
  FSharedMem := ASharedMem;
  FLogFileName := ALogFileName;
  FEvtMsgCanRead := OpenEvent(SYNCHRONIZE or EVENT_MODIFY_STATE, False, PChar(FSharedMem.EventCanReadName));
  FEvtMsgCanWrite := OpenEvent(SYNCHRONIZE or EVENT_MODIFY_STATE, False, PChar(FSharedMem.EventCanWriteName));
  FCS := TCriticalSection.Create;
  inherited Create;
end;

destructor TSharedMemoryLog.TClient.Destroy;
begin
  if (FSharedMem<>nil) and
    (FEvtMsgCanRead<>0) and
    (FEvtMsgCanWrite<>0) and
    (FLogFileName<>'') and
    (FCS<>nil)
  then
    SendClose;
  CloseHandle(FEvtMsgCanWrite);
  CloseHandle(FEvtMsgCanRead);
  FSharedMem := nil;
  FReeAndNil(FCS);
  inherited;
end;

function TSharedMemoryLog.TClient.AquireMsgQueue: Boolean;
const
  BreakIntervalMs = 50;
var
  RemainDelayMs, IntervalDelayMs: Cardinal;
begin
  // Server can became unavailable any time, so we have to break waiting
  // operation time to time to make sure, that server is not closed yet.
  Result := False;
  RemainDelayMs := WaitWriteBufMs;
  IntervalDelayMs := Min(BreakIntervalMs, WaitWriteBufMs);
  repeat
    if FSharedMem.ServerClosed then
      Exit;
    Result := WaitForSingleObject(FEvtMsgCanWrite,IntervalDelayMs)=WAIT_OBJECT_0;
    if Result then
      Exit;
    if RemainDelayMs<>INFINITE then
      if RemainDelayMs<=IntervalDelayMs then
        Exit
      else
        Dec(RemainDelayMs, IntervalDelayMs);
  until False;
end;

procedure TSharedMemoryLog.TClient.ReleaseMsgQueue;
begin
  // enable server to process the message
  SetEvent(FEvtMsgCanRead);
end;

procedure TSharedMemoryLog.TClient.DoFlush;
begin
  FCS.Enter;
  try
    if AquireMsgQueue then
      try
        FSharedMem.MsgCmd := cmdFlush;
      finally
        ReleaseMsgQueue;
      end;
  finally
    FCS.Leave;
  end;
end;

function TSharedMemoryLog.TClient.LogLinesPrefix: String;
begin
  // AH: DLL injected into chrome.exe may fail on GetLocalTime function,
  // so we should not use any API call except something really important here,
  // timestamp etc can be added on the server side.
  result := format('%.9d', [GetCurrentProcessId]);
end;

procedure TSharedMemoryLog.TClient.LogSysInfo;
begin
  // AH: we should log here only absolutely required information.
end;

procedure TSharedMemoryLog.TClient.SendClose;
begin
  FCS.Enter;
  try
    if AquireMsgQueue then
      try
        FSharedMem.MsgCmd := cmdClose;
        FSharedMem.SetLogName(FLogFileName);
      finally
        ReleaseMsgQueue;
      end;
  finally
    FCS.Leave;
  end;
end;

procedure TSharedMemoryLog.TClient.Send(AData: pointer; ASize: Integer);
var
  n: Integer;
begin
  FCS.Enter;
  try
    while ASize>0 do
    begin
      n := Min(ASize, SizeOf(FSharedMem.MsgData));
      if AquireMsgQueue then
        try
          FSharedMem.MsgCmd := cmdSend;
          FSharedMem.SetLogName(FLogfileName);
          Move(AData^, FSharedMem.MsgData, n);
          FSharedMem.MsgSize := n;
        finally
          ReleaseMsgQueue;
        end
      else
        Break;
      inc(PByte(AData), n);
      dec(ASize, n);
    end;
  finally
    FCS.Leave;
  end;
end;

{ TSharedMemoryLog.TServer }

constructor TSharedMemoryLog.TServer.Create(ASharedMem: PSharedMemoryLogRec;
  ANewLog: TOnNewLog);
begin
  SrvLog := TLogThread.Create(ASharedMem);
  SrvLog.FSrvStarted.WaitFor(INFINITE);
end;

destructor TSharedMemoryLog.TServer.Destroy;
begin
  FreeAndNil(SrvLog);
  inherited;
end;

{ TLogThread }

constructor TSharedMemoryLog.TServer.TLogThread.Create(ASharedMem: PSharedMemoryLogRec);
var
  Sec: TSecurity.TDescrAttr;
begin
  FSharedMem := ASharedMem;
  FSharedMem.ServerClosed := False;
  CreateGUID(FSharedMem.SessionId);
  TSecurity.NullDACL(Sec);
  FEvtMsgCanRead := TEvent.Create(@Sec.Attrs, False, False, FSharedMem.EventCanReadName);
  FEvtMsgCanWrite := TEvent.Create(@Sec.Attrs, False, True, FSharedMem.EventCanWriteName);
  FLogs := TObjectDictionary<String, TCustomLog>.Create;
  FSrvStarted := TEvent.Create;
  inherited Create(False);
end;

destructor TSharedMemoryLog.TServer.TLogThread.Destroy;
begin
  if FSharedMem<>nil then
    FSharedMem.ServerClosed := True;
  Terminate;
  if FEvtMsgCanRead<>nil then
    FEvtMsgCanRead.SetEvent;
  inherited;
  FreeAndNil(FLogs);
  FreeAndNil(FEvtMsgCanWrite);
  FreeAndNil(FEvtMsgCanRead);
  FSharedMem := nil;
  FReeAndNil(FSrvStarted);
end;

procedure TSharedMemoryLog.TServer.TLogThread.Execute;

  function GetLog(const AName: string): TCustomLog;
  begin
    if not FLogs.TryGetValue(AName, TCustomLog(Result)) then
    begin
      Result := TFileLog.Create(TCustomLog.GetLogFolder + ExtractFileName(AName));
      FLogs.Add(AName, Result);
    end;
  end;

var
  L: TCustomLogH;
  N: String;
begin

try
  FSrvStarted.SetEvent;
  // allow to send next msg
  // we create it with initial state = true (already signalated)
  //FEvtMsgCanWrite.SetEvent;
  repeat

    // wait for message in the buf
    FEvtMsgCanRead.WaitFor(INFINITE);
    if Terminated then
      Break;

    {$IF [Low(FSharedMem.MsgCmd)..High(FSharedMem.MsgCmd)]<>[cmdSend, cmdFlush, cmdClose]} Fix it here! {$ENDIF}
    // read & execute log command
    N := Trim(FSharedMem.GetLogName);
    if N<>'' then
    begin
      L := TCustomLogH(GetLog(N));
      case FSharedMem.MsgCmd of
        cmdSend:
          L.Send(@FSharedMem.MsgData, FSharedMem.MsgSize);
        cmdFlush:
          L.Flush;
        cmdClose:
          FLogs.Remove(N);
      end;
    end;

    // allow to send next msg
    FEvtMsgCanWrite.SetEvent;
  until False;
except
  on e: exception do
    raise;
end;

end;

end.
