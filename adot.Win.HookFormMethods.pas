unit Win.HookFormMethods;

interface

uses
  Win.HookClassMethods, Vcl.Forms, Classes, System.Generics.Collections,
  System.Generics.Defaults, System.SysUtils;

type
{
  High level API to catch TForm.DnCreate/TForm.DoShow/TFrame.Create.
  1. It is NOT thread-safe.
  2. It is safe to use it from threads different from main one, but all
     calls must be serialized (because of 1.). Indeed it is NOT tested yet.
  3. It is safe to register same handler for different events if they are
     compatible (like TNotifyFormOnCreate and TNotifyFormOnShow).
  4. It is not neccessary to unhook if handler is valid all the time.
  5. It is safe to unhook handler several times (returns True only first time).
  6. It is safe to use it from Initialization section of the unit.
  7. Compatible with Windows x32 and x64 platforms.
  8. Use it only if it is really neccessary (if there is no standard way).
}
  TNotifyFormOnCreate = reference to procedure(AForm: TCustomForm; ABeforeOriginal: Boolean);
  TNotifyFormOnShow = reference to procedure(AForm: TCustomForm; ABeforeOriginal: Boolean);
  TNotifyFrameCreate = reference to procedure(AFrame: TCustomFrame; ABeforeOriginal: Boolean);

function RegHookFormOnCreate(AHandler: TNotifyFormOnCreate):Int64;
function RegHookFormOnShow(AHandler: TNotifyFormOnShow):Int64;
function RegHookFrameCreate(AHandler: TNotifyFrameCreate):Int64;
function UnregHook(const AHookId: int64):Boolean;

{
  Unregister all handlers and disable hooks. By default it is called
  automatically from Finalization section, but this behaviour can be disabled
  (for example if you need to use it in finalization section of other unit).
}
procedure FinalizeHooks;
procedure DisableAutoFinalization;

implementation

type
  THookedForm = class(TCustomForm)
    procedure HookedDoCreate;
    procedure HookedDoShow;
  end;

  THookedFrame = class(TCustomFrame)
    constructor Create(AOwner: TComponent); override;
  end;

var
  AutoFinalizationDisabled: Boolean;
  Initialized: Boolean;
  IdCounter: int64;
  FormDoCreate: TPatchRecord;
  FormDoShow: TPatchRecord;
  FrameCreate: TPatchRecord;
  HooksFormOnCreate: TDictionary<int64, TNotifyFormOnCreate>;
  HooksFormOnShow: TDictionary<int64, TNotifyFormOnShow>;
  HooksFrameCreate: TDictionary<int64, TNotifyFrameCreate>;

procedure PatchCreate;
begin
  FormDoCreate.InstallHook(@THookedForm.DoCreate, @THookedForm.HookedDoCreate);
  FormDoShow.InstallHook(@THookedForm.DoShow, @THookedForm.HookedDoShow);
  FrameCreate.InstallHook(@TCustomFrame.Create, @THookedFrame.Create);
end;

procedure InitHook;
begin
  if Initialized then
    Exit;
  Initialized := True;
  HooksFormOnCreate := TDictionary<int64, TNotifyFormOnCreate>.Create;
  HooksFormOnShow := TDictionary<int64, TNotifyFormOnShow>.Create;
  HooksFrameCreate := TDictionary<int64, TNotifyFrameCreate>.Create;
  PatchCreate;
end;

procedure FinalizeHooks;
begin
  if not Initialized then
    Exit;
  Initialized := False;
  FormDoCreate.DisableHook;
  FormDoShow.DisableHook;
  FrameCreate.DisableHook;
  FreeAndNil(HooksFormOnCreate);
  FreeAndNil(HooksFormOnShow);
  FreeAndNil(HooksFrameCreate);
end;

procedure DisableAutoFinalization;
begin
  AutoFinalizationDisabled := True;
end;

function RegHookFormOnCreate(AHandler: TNotifyFormOnCreate):Int64;
begin
  InitHook;
  inc(IdCounter);
  Result := IdCounter;
  HooksFormOnCreate.Add(Result, AHandler);
end;

function RegHookFormOnShow(AHandler: TNotifyFormOnShow):Int64;
begin
  InitHook;
  inc(IdCounter);
  Result := IdCounter;
  HooksFormOnShow.Add(Result, AHandler);
end;

function RegHookFrameCreate(AHandler: TNotifyFrameCreate):Int64;
begin
  InitHook;
  inc(IdCounter);
  Result := IdCounter;
  HooksFrameCreate.Add(Result, AHandler);
end;

function UnregHook(const AHookId: int64):Boolean;
begin
  Result := False;
  if not Initialized then
    Exit;
  if HooksFormOnCreate.ContainsKey(AHookId) then
  begin
    Result := True;
    HooksFormOnCreate.Remove(AHookId);
  end;
  if HooksFormOnShow.ContainsKey(AHookId) then
  begin
    Result := True;
    HooksFormOnShow.Remove(AHookId);
  end;
  if HooksFrameCreate.ContainsKey(AHookId) then
  begin
    Result := True;
    HooksFrameCreate.Remove(AHookId);
  end;
end;

{ THookedForm }

procedure THookedForm.HookedDoCreate;
var
  h: TNotifyFormOnCreate;
begin
  for h in HooksFormOnCreate.Values do
    h(Self, True);

  FormDoCreate.DisableHook;
  try
    DoCreate;
  finally
    FormDoCreate.EnableHook;
  end;

  for h in HooksFormOnCreate.Values do
    h(Self, False);
end;

procedure THookedForm.HookedDoShow;
var
  h: TNotifyFormOnShow;
begin
  for h in HooksFormOnShow.Values do
    h(Self, True);

  FormDoShow.DisableHook;
  try
    DoShow;
  finally
    FormDoShow.EnableHook;
  end;

  for h in HooksFormOnShow.Values do
    h(Self, False);
end;

{ THookedFrame }

constructor THookedFrame.Create(AOwner: TComponent);
var
  h: TNotifyFrameCreate;
begin
  for h in HooksFrameCreate.Values do
    h(Self, True);

  FrameCreate.DisableHook;
  try
    inherited Create(AOwner);
  finally
    FrameCreate.EnableHook;
  end;

  for h in HooksFrameCreate.Values do
    h(Self, False);
end;

initialization
  // we initialize hooks on first request

finalization
  if not AutoFinalizationDisabled then
    FinalizeHooks;

end.
