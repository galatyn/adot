  TListOfThreads = class;

  TListableThread = class(TThread)
  protected
    FDestroyingListEvent: TEvent;
    FThreadList: TListOfThreads;

    // Descendants _must_ call it at end of execute method.
    // It should be called inside of Execute method.
    procedure ThreadFinished;

    // Descendants must support it - when called, execute should be finished ASAP.
    procedure TerminatedSet; override;

  public
    constructor Create(AList: TListOfThreads);
    destructor Destroy; override;

    // Descendants must use it in all wait operations to be sure, that thread will
    // be resumed and terminated by request of owner (list) as soon as possible.
    property TerminateRequestEvent: TEvent read FDestroyingListEvent;
  end;

  // Will destroy all contained threads gracefully on destruction.
  TListOfThreads = class
  protected
    FListMutex: TMutex;
    FList: TList<TListableThread>;
    FTimeout: DWORD;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AThread: TListableThread);
    procedure ThreadFinished(ACaller: TListableThread);

    property Timeout: DWORD read FTimeout write FTimeout;
  end;

{ TListableThread }

constructor TListableThread.Create(AList: TListOfThreads);
begin
  FDestroyingListEvent := TEvent.Create(nil, True, False, '');
  FThreadList := AList;

  // when creating new thread, list must not be in-destroying state
  // so it is safe to aquire FListMutex
  FThreadList.Add(Self);

  inherited Create(False);
end;

destructor TListableThread.Destroy;
begin
  // Task can be destroyed:
  // 1. If Execute finished (then FreOnTerminate will be set).
  // 2. If owner is destroying.
  // In any case we should not wait for thread here, just clean up.
  FThreadList := nil;
  FreeAndNil(FDestroyingListEvent);
  inherited;
end;

procedure TListableThread.TerminatedSet;
begin
  inherited;
  if FDestroyingListEvent<>nil then
    FDestroyingListEvent.SetEvent;
end;

procedure TListableThread.ThreadFinished;
begin
  FThreadList.ThreadFinished(Self);
end;

{ TListOfThreads<T> }

constructor TListOfThreads.Create;
begin
  FListMutex := TMutex.Create;
  FList := TList<TListableThread>.Create;
  FTimeout := 3000;
end;

procedure TListOfThreads.Add(AThread: TListableThread);
begin
  FListMutex.Acquire;
  try
    FList.Add(AThread);
  finally
    FListMutex.Release;
  end;
end;

procedure TListOfThreads.ThreadFinished(ACaller: TListableThread);
var
  Signalated: THandleObject;
  ObjArr: THandleObjectArray;
begin
  // This method is always called in context of ACaller thread.
  // It mean that any other method (including dstructor) may run at same time.
  setlength(ObjArr, 2);
  ObjArr[0] := FListMutex;
  ObjArr[1] := ACaller.FDestroyingListEvent;
  if THandleObject.WaitForMultiple(ObjArr, INFINITE, False, Signalated)=wrSignaled then
    if Signalated=ACaller.FDestroyingListEvent then
    begin
      // destroying is initiated by TListOfThreads.Destroy (list will care of destroying)
      ACaller.FreeOnTerminate := False;
      ACaller.FThreadList := nil;
    end
    else
    try
      // destroying is initiated by thread (finished)
      // and we aquired FListMutex
      FList.Remove(ACaller);
      ACaller.FThreadList := nil;
      ACaller.FreeOnTerminate := True;
    finally
      FListMutex.Release;
    end;
end;

destructor TListOfThreads.Destroy;
var
  i: Integer;
  t: TListableThread;
begin
  if (FList<>nil) and (FListMutex<>nil) then
  begin
    FListMutex.Acquire;
    try

      // Iinform all threads to finish (terminate).
      // Signalate DestroyingList event (to avoid of deadlocks).
      for i := FList.Count-1 downto 0 do
      begin
        t := FList[i];
        t.Terminate; // will call FDestroyingListEvent.SetEvent from TerminatedSet
      end;

      // Wait for threads and destroy.
      // Delphi doesn't have platform independent function to wait several threads,
      // but we already signalated all threads, so it is ok to wait them one by one.
      // The only potential problem - we can't use general timeout for full wait operation.
      for i := FList.Count-1 downto 0 do
        try
          t := FList[i];
          t.WaitFor;
          FList[i].Free;
        except
        end;
      FList.Clear;
    finally
      FListMutex.Release;
    end;
  end;

  FreeAndNil(FList);
  FreeAndNil(FListMutex);
  inherited;
end;

