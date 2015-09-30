unit adot.VCL.Log;

interface

uses
  adot.Log, adot.VCL.Tools, System.SysUtils, Winapi.Messages,
  Winapi.Windows, System.SyncObjs, System.Classes, System.StrUtils;

type

  // Thread-safe implementation of VCL logger (message based to avoid any
  // deadlock posibility).
  TVCLDelegatedLog = class(TCustomDelegatedLog)
  protected
    const
      wmLogMessage = wm_user + 1;
    var
      FLogLines: TStringList;
      FLogLinesMsgSent: Boolean;
      FLogLinesCS: TCriticalSection;
      FMessanger: TMessenger;
      FOnBeginUpdate, FOnEndUpdate: TProc;

    procedure Send(AData: pointer; ASize: Integer); override;
    procedure DoFlush; override;
    procedure OnMessage(var AMessage: TMessage); virtual;
  public
    constructor Create(AOnLogMessage: TCustomDelegatedLog.TOnLogMessage;
      AOnBeginUpdate: TProc = nil; AOnEndUpdate: TProc = nil);
    destructor Destroy; override;
  end;

  TVCLStringsLog = class(TVCLDelegatedLog)
  protected
    FStrings: TStrings;

    procedure LogString(const Msg: string);
    procedure OnMessage(var AMessage: TMessage); override;
  public
    constructor Create(AStrings: TStrings);
  end;

procedure AddMemoLog(ALines: TStrings);

implementation

{ TVCLDelegatedLog }

constructor TVCLDelegatedLog.Create(AOnLogMessage: TCustomDelegatedLog.TOnLogMessage;
  AOnBeginUpdate: TProc = nil; AOnEndUpdate: TProc = nil);
begin
  FLogLines := TStringList.Create;
  FLogLinesCS := TCriticalSection.Create;
  FMessanger := TMessenger.Create(OnMessage);
  FOnBeginUpdate := AOnBeginUpdate;
  FOnEndUpdate := AOnEndUpdate;
  inherited Create(AOnLogMessage);
end;

destructor TVCLDelegatedLog.Destroy;
begin
  FreeAndNil(FMessanger);
  FreeAndNil(FLogLinesCS);
  FreeAndNil(FLogLines);
  inherited;
end;

procedure TVCLDelegatedLog.DoFlush;
begin
end;

procedure TVCLDelegatedLog.Send(AData: pointer; ASize: Integer);
var
  s: string;
begin
  s := DataAsString(Adata, ASize);
  FLogLinesCS.Enter;
  try
    FLogLines.Add(s);
    if not FLogLinesMsgSent then
      FLogLinesMsgSent := PostMessage(FMessanger.Handle, wmLogMessage, 0,0);
  finally
    FLogLinesCS.Leave;
  end;
end;

procedure TVCLDelegatedLog.OnMessage(var AMessage: TMessage);
var
  i: Integer;
begin
  if not Assigned(FOnLogMessage) or (AMessage.Msg<>wmLogMessage) then
    Exit;
  FLogLinesCS.Enter;
  try
    FLogLinesMsgSent := False;
    if Assigned(FOnBeginUpdate) then
      FOnBeginUpdate;
    try
      for i := 0 to FLogLines.Count-1 do
        FOnLogMessage(FLogLines[i]);
    finally
      if Assigned(FOnEndUpdate) then
        FOnEndUpdate;
    end;
    FLogLines.Clear;
  finally
    FLogLinesCS.Leave;
  end;
end;

{ TVCLStringsLog }

constructor TVCLStringsLog.Create(AStrings: TStrings);
begin
  FStrings := AStrings;
  inherited Create(LogString);
end;

procedure TVCLStringsLog.LogString(const Msg: string);
var
  s: string;
begin
  s := Msg;
  if EndsText(#13#10, s) or EndsText(#10#13, s) then
    SetLength(s, Length(s)-2);
  FStrings.Add(s);
end;

procedure TVCLStringsLog.OnMessage(var AMessage: TMessage);
begin
  FStrings.BeginUpdate;
  try
    inherited;
  finally
    FStrings.EndUpdate;
  end;
end;

procedure AddMemoLog(ALines: TStrings);
begin
  adot.Log.AddLogger(TVCLStringsLog.Create(ALines));
end;

end.
