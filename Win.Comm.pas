unit Win.Comm;

interface

uses
  System.SyncObjs, System.SysUtils, Winapi.Windows, System.Classes,
  System.StrUtils, System.DateUtils, System.Math, Win.Tools, CrossPlatform.Log,
  System.Generics.Collections, System.Generics.Defaults, CrossPlatform.Tools;

const
  // it is recommended to send data in smaller packages - up to 64kb for example
  MaxMessageSize = 512*1024 * 1000;

type
  TErrorInfo = record
    Msg: string;
  end;

  // Pipe created with default settings: byte-read(not message-read), blocking-wait.
  // Not-seekable, write-only stream.
  TPipeClientStream = class(TStream)
  private
    FDisconnectOnError: Boolean;
  protected
    FPipeHandle: THandle;
    FComputerName: string;
    FPipeInstanceName: string;
    FConnected: boolean;
    FTimeout: cardinal;

    function GetName: string; virtual;
    procedure RaiseNoSupport; virtual;
    procedure SendDisconnectMessage;
    function SendData(const Buffer; Count: DWORD): Boolean;
    //function SendDataPackageSize(const ASize: int64): Boolean; inline;

    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;

  public
    constructor Create(const AComputerName, APipeInstanceName: string; AConnect: Boolean = True);
    destructor Destroy; override;
    function Connect(AConnectTimeout: Cardinal = 0): Boolean;
    procedure Disconnect;

    function WriteDataPackage(const Buffer; const Count: Int64): Int64;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    function Write(ASrc: TMemoryStream): Int64; overload;

    // not supported methods (should never be called, will raise exception)
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; overload; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;

    property ComputerName: string read FComputerName write FComputerName;
    property PipeInstanceName: string read FPipeInstanceName write FPipeInstanceName;
    property Name: string read GetName;
    property Connected: Boolean read FConnected;
    property Timeout: cardinal read FTimeout write FTimeout;
    property PipeHandle: THandle read FPipeHandle;
    property DisconnectOnError: Boolean read FDisconnectOnError write FDisconnectOnError;
  end;

  TPipeServer<TSessionType> = class
  public
    type
      TOnClientConnected = reference to procedure(var ASession: TSessionType);
      TOnClientMessage = reference to procedure(ASession: TSessionType;
        AMessage: TMemoryStream; var ATookOwnership: Boolean);
      TOnClientDisconnected = reference to procedure(ASession: TSessionType; const AError: TErrorInfo);

  protected
    type

      TListenThread = class(TThread)
      private
      protected
        const
          // pipe read/write buffer size
          BufferSize = 4*1024;
          MaxClientCount = 1024;
        var
          FOwner: TPipeServer<TSessionType>;

          // name of pipe from owner: //./pipe_instance_name
          FPipeFullName: string;
          FOverlappEvent: THandle;
          FStartedEvent: THandle;

          // used to save error message
          FErrorMsg: string;

        procedure Execute; override;

        // to be called after constructor (raises exception in case of error)
        procedure RaiseIfNotStarted;

        // called from Execute when client is connected (forwards call to FOwner.ClientConnected)
        procedure ClientConnected(APipeHandle: THandle);

      public
        constructor Create(AOwner: TPipeServer<TSessionType>);

        // can be called any time (will shutdown pipe server gracefully)
        destructor Destroy; override;
      end;

      // read message & send it to callback
      TWorkerThread = class(TListableThread)
      private
      protected
        var
          FOwner: TPipeServer<TSessionType>;
          FPipeHandle: THandle;
          FOverlappEvent: THandle;
          FOverlapped: TOverlapped;
          FMessage: TMemoryStream;
          FSession: TSessionType;
          FErrorMessage: string;
          FConnected: Boolean;

        procedure Execute; override;
        procedure TerminatedSet; override;

        function ReadMessageSize(var AMessageSize: DWORD): Boolean;
        function ReadBlock(var ABuf; ASize: integer): Boolean;

      public
        // takes ownership of PipeHandle
        constructor Create( AList: TListOfThreads; AOwner: TPipeServer<TSessionType>; APipeHandle: THandle );

        // can be called any time (will shutdown pipe server gracefully)
        destructor Destroy; override;
      end;

    var
      FThread: TListenThread;
      FWorkerThreads: TListOfThreads;
      FPipeInstanceName: string;
      FComputerName: string;
      FOnClientConnected: TOnClientConnected;
      FOnClientDisconnected: TOnClientDisconnected;
      FOnClientMessage: TOnClientMessage;

    function GetName: string; virtual;
    procedure ClientConnected(APipeHandle: THandle); virtual;

  public
    constructor Create(const APipeInstanceName: string);
    procedure Start;
    destructor Destroy; override;

    property ComputerName: string read FComputerName write FComputerName;
    property PipeInstanceName: string read FPipeInstanceName write FPipeInstanceName;
    property Name: string read GetName;

    property OnClientConnected: TOnClientConnected read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TOnClientDisconnected read FOnClientDisconnected write FOnClientDisconnected;
    property OnClientMessage: TOnClientMessage read FOnClientMessage write FOnClientMessage;
  end;

implementation

{ TPipeClientStream }

constructor TPipeClientStream.Create(const AComputerName, APipeInstanceName: string; AConnect: Boolean = True);
begin
  FPipeHandle := INVALID_HANDLE_VALUE;
  FComputerName := AComputerName;
  FPipeInstanceName := APipeInstanceName;
  FTimeout := 1000;
  DisconnectOnError := True;
  if AConnect then
    Connect;
end;

destructor TPipeClientStream.Destroy;
begin
  Disconnect;
  inherited;
end;

function TPipeClientStream.Connect(AConnectTimeout: Cardinal): Boolean;
var
  StartTime: TDateTime;
  LastError: DWORD;
  Name: string;
  WaitDelay: Cardinal;
  CommTimeouts: TCommTimeouts;
begin
  WaitDelay := INFINITE; // supress stupid warning (XE7)
  if not Connected then
  begin
    Name := GetName;
    StartTime := Now;
    repeat
      FPipeHandle := CreateFile(PChar(Name), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
      FConnected := FPipeHandle<>INVALID_HANDLE_VALUE;
      if FConnected then
        Break;

      // check fatal errors (waiting doesn't make sence)
      LastError := GetLastError;
      if (LastError<>ERROR_PIPE_BUSY) and (LastError<>ERROR_FILE_NOT_FOUND) then
        Break;

      // calculate delay and wait for pipe
      if AConnectTimeout=INFINITE then
        WaitDelay := INFINITE
      else
        WaitDelay := Max(0, AConnectTimeout - MilliSecondsBetween(Now, StartTime));

    until (WaitDelay=0) or not WaitNamedPipe(PChar(Name), WaitDelay);
  end;
  Result := Connected;
  if Connected and (FTimeout>0) and GetCommTimeouts(FPipeHandle, CommTimeouts) then
  begin
    CommTimeouts.ReadTotalTimeoutMultiplier := MAXDWORD;
    CommTimeouts.ReadTotalTimeoutConstant := FTimeout;
    CommTimeouts.ReadIntervalTimeout := MAXDWORD;
    SetCommTimeouts(FPipeHandle, CommTimeouts);
  end;
end;

procedure TPipeClientStream.Disconnect;
begin
  if not Connected then
    Exit;
  if FPipeHandle<>INVALID_HANDLE_VALUE then
  begin
    //SendDisconnectMessage;
    CloseHandle(FPipeHandle);
    FPipeHandle := INVALID_HANDLE_VALUE;
  end;
  FConnected := False;
end;

function TPipeClientStream.GetName: string;
begin
  Result := Format('\\%s\pipe\%s', [IfThen(ComputerName='', '.', ComputerName), PipeInstanceName]);
end;

procedure TPipeClientStream.RaiseNoSupport;
begin
  raise Exception.Create('Method is not supported');
end;

function TPipeClientStream.GetSize: Int64;
begin
  RaiseNoSupport;
  result := -1;
end;

function TPipeClientStream.Read(var Buffer; Count: Integer): Longint;
begin
  RaiseNoSupport;
  result := -1;
end;

function TPipeClientStream.Read(Buffer: TBytes; Offset, Count: Integer): Longint;
begin
  RaiseNoSupport;
  result := -1;
end;

function TPipeClientStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  RaiseNoSupport;
  result := -1;
end;

function TPipeClientStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  RaiseNoSupport;
  result := -1;
end;

procedure TPipeClientStream.SetSize(NewSize: Integer);
begin
  RaiseNoSupport;
end;

procedure TPipeClientStream.SetSize(const NewSize: Int64);
begin
  RaiseNoSupport;
end;

function TPipeClientStream.SendData(const Buffer; Count: DWORD): Boolean;
var
  BytesWritten: DWORD;
begin
  if not Connected then
    result := False
  else
  begin
    Result := WriteFile(FPipeHandle, Buffer, Count, BytesWritten, nil) and (BytesWritten=Count);
    if not Result and DisconnectOnError then
      Disconnect;
  end;
end;

{function TPipeClientStream.SendDataPackageSize(const ASize: int64): Boolean;
begin
  Result := SendData(ASize, SizeOf(ASize));
end;
 }
procedure TPipeClientStream.SendDisconnectMessage;
begin
  //SendDataPackageSize(-1);
end;

function TPipeClientStream.WriteDataPackage(const Buffer; const Count: Int64): Int64;
begin
  if Count<=0 then
    Result := 0
  else
    if {SendDataPackageSize(Count) and} SendData(Buffer, DWORD(Count)) then
      result := Count
    else
      result := 0;
end;

function TPipeClientStream.Write(const Buffer; Count: Integer): Longint;
begin
  result := WriteDataPackage(Buffer, Count);
end;

function TPipeClientStream.Write(ASrc: TMemoryStream): Int64;
begin
  result := WriteDataPackage(ASrc.Memory^, ASrc.Size);
end;

{ TPipeServer.TListenThread }

constructor TPipeServer<TSessionType>.TListenThread.Create(AOwner: TPipeServer<TSessionType>);
begin
  try
    FOwner := AOwner;
    FOverlappEvent := CreateEvent(nil, False, False, nil);
    FStartedEvent := CreateEvent(nil, False, False, nil);
    FPipeFullName := AOwner.Name;
    inherited Create(False);
  except
    on e: exception do
    begin
      if AppLog<>nil then
        AppLog.Log('TPipeServer.TListenThread.Create exception: %s', [e.Message]);
      raise;
    end;
  end;
end;

procedure TPipeServer<TSessionType>.TListenThread.RaiseIfNotStarted;
var
  h: array[0..1] of THandle;
begin
  h[0] := Handle;
  h[1] := FStartedEvent;
  case WaitForMultipleObjects(2, @h, False, INFINITE) of
    WAIT_OBJECT_0:
      raise Exception.Create('Thread failed: '+FErrorMsg);
    WAIT_OBJECT_0+1:
      {Ok};
    else
      raise Exception.Create('Error');
  end;
end;

destructor TPipeServer<TSessionType>.TListenThread.Destroy;
begin
  Terminate;
  if FOverlappEvent<>0 then
  begin
    SetEvent(FOverlappEvent);
    WaitFor;
    CloseHandle(FOverlappEvent);
  end;
  FOverlappEvent := 0;
  if FStartedEvent<>0 then
    CloseHandle(FStartedEvent);
  FStartedEvent := 0;
  inherited;
end;

function ConnectNamedPipeCodeToMsg(ALastErrorCode: DWORD): string;
begin
{  case ALastErrorCode of

  end;}
end;

procedure TPipeServer<TSessionType>.TListenThread.Execute;
var
  SecAttrs: TSecurity.TDescrAttr;
  PipeFullName: string;
  PipeHandle: THANDLE;
  Overlapped: TOverlapped;
  StartReported: Boolean;
begin
  try
    PipeFullName := FPipeFullName;

    // create null dacl (grant access to any user)
    if not TSecurity.NullDACL(SecAttrs) then
    begin
      FErrorMsg := 'Null DACL creation failed';
      Exit;
    end;

    StartReported := False;
    PipeHandle := INVALID_HANDLE_VALUE;
    try

      // main cycle (create instance of pipe and wait for client to be connected)
      while not Terminated do
      begin

        // create server side of pipe
        PipeHandle := CreateNamedPipe(
          PChar(PipeFullName),
          PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED,
          PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE,
          //PIPE_TYPE_BYTE or PIPE_READMODE_BYTE,
          PIPE_UNLIMITED_INSTANCES,
          BufferSize,
          BufferSize,
          NMPWAIT_USE_DEFAULT_WAIT,
          @SecAttrs.Attrs
        );
        if AppLog<>nil then
          if PipeHandle=INVALID_HANDLE_VALUE then
            AppLog.Log('TPipeServer.TListenThread.Execute'+' CreateNamedPipe error: %s', [SysErrorMessage(GetLastError)])
          else
            AppLog.Log('TPipeServer.TListenThread.Execute'+' CreateNamedPipe handle: %d', [PipeHandle]);
        if PipeHandle=INVALID_HANDLE_VALUE then
        begin
          FErrorMsg := Format('CreateNamedPipe "%s" failed.', [PipeFullName]);
          Break;
        end;

        if not StartReported then
        begin
          if AppLog<>nil then
            AppLog.Log('TPipeServer.TListenThread.Execute'+' report started');
          StartReported := True;
          SetEvent(FStartedEvent);
        end;

        // wait for client connection (or signal to stop)
        FillChar(Overlapped, SizeOf(TOverlapped), 0);
        Overlapped.hEvent := FOverlappEvent;
        ConnectNamedPipe(PipeHandle, @Overlapped);
        if AppLog<>nil then
          AppLog.Log('TPipeServer.TListenThread.Execute'+' ConnectNamedPipe: %s', [SysErrorMessage(GetLastError)]);
        if Terminated then
          Break;
        case GetLastError of
          ERROR_PIPE_CONNECTED:
            begin
              ClientConnected(PipeHandle);
              PipeHandle := INVALID_HANDLE_VALUE;
              Continue;
            end;
          ERROR_IO_PENDING:
            if WaitForSingleObject(FOverlappEvent, INFINITE) = WAIT_OBJECT_0 then
            begin
              if Terminated then
                Break;
              ClientConnected(PipeHandle);
              PipeHandle := INVALID_HANDLE_VALUE;
              Continue;
            end
        end; // case GetLastError of
        FErrorMsg := SysErrorMessage(GetLastError);
        Break;
      end; // while not Terminated do

    finally
      if PipeHandle<>INVALID_HANDLE_VALUE then
        CloseHandle(PipeHandle);
      if AppLog<>nil then
        AppLog.Log('TPipeServer.TListenThread.Execute'+' done, ErrorMessage=%s', [FErrorMsg]);
    end;
  finally
    // we need to call it to remove the thread from thread's list
    inherited Execute;
  end;
end;

procedure TPipeServer<TSessionType>.TListenThread.ClientConnected(APipeHandle: THandle);
begin
  if AppLog<>nil then
    AppLog.Log('TPipeServer.TListenThread.ClientConnected: %d', [APipeHandle]);
  FOwner.ClientConnected(APipeHandle);
end;

{ TPipeServer }

constructor TPipeServer<TSessionType>.Create(const APipeInstanceName: string);
begin
  PipeInstanceName := APipeInstanceName;
  FWorkerThreads := TListOfThreads.Create;
  inherited Create;
end;

procedure TPipeServer<TSessionType>.Start;
var
  S: string;
begin
  S := Name;

  // quick check, maybe such server already exists
  if WaitNamedPipe(PChar(S), 100 {ms}) then
    raise Exception.Create(Format('Pipe "%s" already exists.', [S]));

  FreeAndNil(FThread);
  FThread := TPipeServer<TSessionType>.TListenThread.Create(Self);
  FThread.RaiseIfNotStarted;
end;

destructor TPipeServer<TSessionType>.Destroy;
begin
  FreeAndNil(FThread);
  FreeAndNil(FWorkerThreads);
  inherited;
end;

function TPipeServer<TSessionType>.GetName: string;
begin
  Result := Format('\\%s\pipe\%s', [IfThen(ComputerName='', '.', ComputerName), PipeInstanceName]);
end;

procedure TPipeServer<TSessionType>.ClientConnected(APipeHandle: THandle);
begin
  if AppLog<>nil then
    AppLog.Log('TPipeServer.ClientConnected(%d)', [APipeHandle]);
  TWorkerThread.Create( FWorkerThreads, Self, APipeHandle );
end;

{ TPipeServer.TWorkerThread }

constructor TPipeServer<TSessionType>.TWorkerThread.Create( AList: TListOfThreads;
  AOwner: TPipeServer<TSessionType>; APipeHandle: THandle);
begin
  FOwner := AOwner;
  FPipeHandle := APipeHandle;
  FOverlappEvent := CreateEvent(nil, False, False, nil);
  FillChar(FOverlapped, SizeOf(TOverlapped), 0);
  FOverlapped.hEvent := FOverlappEvent;
  FMessage := TMemoryStream.Create;
  FreeOnTerminate := True;

  inherited Create(AList);
end;

destructor TPipeServer<TSessionType>.TWorkerThread.Destroy;
begin
  // There are two ways how destructor may be called:
  // - Thread (.Execute method) finished the job, and we here because of FreeOnTerminate
  // - List of thread is destroying, so it is signalated FDestroyingListEvent and finished thread
  // In any case the thread is already terminated and we just neet to clean up resources.
  if FOverlappEvent<>0 then
    CloseHandle(FOverlappEvent);
  FOverlappEvent := 0;
  FreeAndNil(FMessage);
  if FPipeHandle<>0 then
    CloseHandle(FPipeHandle);
  FPipeHandle := 0;
  inherited;
end;

procedure TPipeServer<TSessionType>.TWorkerThread.TerminatedSet;
begin
  inherited;
  if FOverlappEvent<>0 then
    SetEvent(FOverlappEvent);
end;

function TPipeServer<TSessionType>.TWorkerThread.ReadMessageSize(var AMessageSize: DWORD): Boolean;
var
  BytesRead: DWORD;
begin
  // To manage memory fterminatedefficiently we need to know size of the message.
  // Reading of message size in the pipe is not so trivial, so we put it
  // here as separate method.
  AMessageSize := 0;
  result := ReadFile(FPipeHandle, nil^, 0, BytesRead, @FOverlapped);
  if Terminated then begin result := False; Exit; end;
  if not result and (GetLastError=ERROR_IO_PENDING) then
  begin
    result := GetOverlappedResult(FPipeHandle, FOverlapped, AMessageSize, True);
    if Terminated then begin result := False; Exit; end;
  end;
  if result or (GetLastError=ERROR_MORE_DATA) then
  begin
    result := PeekNamedPipe(FPipeHandle, nil, 0, nil, nil, @AMessageSize);
    if Terminated then begin result := False; Exit; end;
  end;
  if result and (AMessageSize > MaxMessageSize) then
  begin
    result := False;
    FErrorMessage := 'Bad message size';
  end;
end;

function TPipeServer<TSessionType>.TWorkerThread.ReadBlock(var ABuf; ASize: integer): Boolean;
var
  BytesRead: cardinal;
begin
  // message-oriented pipe should alway read whole message
  result := ReadFile(FPipeHandle, ABuf, ASize, BytesRead, @FOverlapped) and
    (BytesRead=dword(ASize)) and
    not Terminated;
end;

procedure TPipeServer<TSessionType>.TWorkerThread.Execute;
var
  TookOwnership: boolean;
  MessageSize: dword;
  ErrorInfo: TErrorInfo;
begin

  try
    try

      // request Session
      if Assigned(FOwner.FOnClientConnected) then
      begin
        FOwner.FOnClientConnected(FSession);
        FConnected := True;
      end;

      try

        // main cycle (read&process messages from the pipe)
        repeat
          if not ReadMessageSize(MessageSize) then
            Break;
          FMessage.SetSize(MessageSize);
          if not ReadBlock(FMessage.Memory^, FMessage.Size) then
            Break;
          TookOwnership := False;
          FOwner.FOnClientMessage(FSession, FMessage, TookOwnership);
          if TookOwnership then
          begin
            FMessage := nil;
            FMessage := TMemoryStream.Create;
          end;
        until Terminated;

      finally

        // destroy Session
        if FConnected then
        begin
          FConnected := False;
          if Assigned(FOwner.FOnClientDisconnected) then
          begin
            ErrorInfo.Msg := FErrorMessage;
            FOwner.FOnClientDisconnected(FSession, ErrorInfo);
          end;
        end;

      end;

    // we save error message
    except
      on e: exception do
        FErrorMessage := e.Message;
    end;

  // unregister thread, set FreeOnTerminate etc.
  finally
    ThreadFinished;
    FOwner := nil;
  end;
end;

end.
