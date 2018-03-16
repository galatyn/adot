unit adot.Tools.Performance;

{
  TTiming = class
    Time measuring functions (recursive Start/Stop etc)

  TTimeOut = record
    Allows to avoid of some operations to be executed too often.

  TEventStat = class
    Simple way to maintain statistic of action calls
}

interface

uses
  adot.Collections.Maps,
  System.Diagnostics,  { TStopwatch }
  System.TimeSpan,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils;

type
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
        procedure Init(const ASpan: TTimeSpan; const ACalls: int64);
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
    procedure Init;

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
    function GetCategoryStat(const EventCategory: string): int64;
  end;

implementation

uses
  adot.Tools;

{ TTiming.TTotalStat }

procedure TTiming.TTotalStat.Init(const ASpan: TTimeSpan; const ACalls: int64);
begin
  Self := Default(TTiming.TTotalStat);
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
    ATotalStat.Init(TTimeSpan.Create(0), 0);
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

procedure TTimeOut.Init;
begin
  Self := Default(TTimeOut);
end;

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

function TEventStat.GetCategoryStat(const EventCategory: string): int64;
begin
  if not FEvents.TryGetValue(EventCategory, result) then
    result := 0;
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

end.
