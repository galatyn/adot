unit adot.Win.HookClassMethods;

// AH: Simple API to hook some method calls of some classes.
// Based on the code from SQLite3i18n.pas (Synopse SQLite3 database framework).
// According to this unit, "hook logic was inspired from GetText()".
// So initially it is based on GetText library.

interface

uses
  Windows, SysUtils, System.Generics.Collections, System.Generics.Defaults;

type

  {
    Low level API for hooking of any method/procedure at specified address. Example:
    var
      FormDoCreate: TPatchRecord; // global var

    Type THookedForm = class(TCustomForm)
      procedure HookedDoCreate; // this method will be called instead of standard TCustomForm.DoCreate
    end;

    procedure THookedForm.HookedDoCreate;
    begin
      Caption := Caption + ' before'; // do something before original code will be called
      FormDoCreate.DisableHook;
      try
        DoCreate; // call original procedure (without hook, otherwise we get infinite cycle and stack overflow).
      finally
        FormDoCreate.EnableHook;
      end;
      Caption := Caption + ' after'; // do something when original code already executed
    end;

    initialization // install hook at unit initialization
      FormDoCreate.InstallHook(@THookedForm.DoCreate, @THookedForm.HookedDoCreate);

    finalization // uninstall hook at unit deinitialization (not neccessary in this case, just for demonstration)
      FormDoCreate.RemoveHook;
  }
  PPatchCode = ^TPatchCode;
  TPatchCode = packed record
    Jump   : byte; // asm opcode to patch an existing routine (JMP xxx)
    Offset : integer;
  end;
  TPatchRecord = record
    NewCode, OriginalCode: TPatchCode;
    PatchPos: PPatchCode;
    HookInstalled: string; // HookInstalled<>'' after successfull InstallHook

    procedure InstallHook(AOriginalProc, AHookProc: pointer);
    procedure UninstallHook;
    function Installed: Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure DisableHook; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure EnableHook; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

(*
// Example (manager for hooking of TdxBarManagerMDIStateHelper.DoActiveChildChanged):
unit HookDxBarMethods;
interface
uses
  dxBar, HookClassMethods, System.SysUtils, Winapi.Windows;

type
  TOnDoActiveChildChanged = reference to procedure(
    AObj: TdxBarManagerMDIStateHelper;
    const ANewActiveChild, AOldActiveChild: HWND;
    ABeforeOriginalCode: Boolean);
  TdxBarManagerMDIStateHelperHook = class(TdxBarManagerMDIStateHelper)
  public
    procedure Hook_DoActiveChildChanged(const ANewActiveChild, AOldActiveChild: HWND);
  end;

var
  DoActiveChildChanged_HookManager: TMethodHookManager<TOnDoActiveChildChanged>;

implementation

procedure TdxBarManagerMDIStateHelperHook.Hook_DoActiveChildChanged(
  const ANewActiveChild, AOldActiveChild: HWND);
var
  h: TOnDoActiveChildChanged;
begin
  for h in DoActiveChildChanged_HookManager.Handlers.Values do
    h(Self, ANewActiveChild, AOldActiveChild, True);
  DoActiveChildChanged_HookManager.DisableHook;
  try
    inherited DoActiveChildChanged(ANewActiveChild, AOldActiveChild);
  finally
    DoActiveChildChanged_HookManager.EnableHook;
  end;
  for h in DoActiveChildChanged_HookManager.Handlers.Values do
    h(Self, ANewActiveChild, AOldActiveChild, False);
end;

initialization
  DoActiveChildChanged_HookManager := TMethodHookManager<TOnDoActiveChildChanged>.Create(
    @TdxBarManagerMDIStateHelperHook.DoActiveChildChanged,
    @TdxBarManagerMDIStateHelperHook.Hook_DoActiveChildChanged);
finalization
  FreeAndNil(DoActiveChildChanged_HookManager);
end.

// Hot to use such manager:
procedure OnDoActiveChildChanged(AObj: TdxBarManagerMDIStateHelper;
  const ANewActiveChild, AOldActiveChild: HWND; ABeforeOriginalCode: Boolean);
begin
  // ...
end;

initialization
  DoActiveChildChanged_HookManager.RegHook(OnDoActiveChildChanged);
*)
  TMethodHookManager<T> = class
  protected
    class var
      IdCounter: int64;
    var
      Patch: TPatchRecord;
  public
    Handlers: TDictionary<int64, T>;

    constructor Create(AOriginalProc, AHookProc: pointer);
    destructor Destroy; override;
    procedure DisableHook;
    procedure EnableHook;

    // user-friendly interface to hook
    function RegHook(AHandler: T):Int64;
    function UnregHook(const AHookId: int64):Boolean;
  end;

implementation

function TPatchRecord.Installed: Boolean;
begin
  result := HookInstalled<>'';
end;

procedure TPatchRecord.InstallHook(AOriginalProc, AHookProc: pointer);
var
  OldProtect: Cardinal;
begin
  PatchPos       := AOriginalProc;
  OriginalCode   := PatchPos^;
  NewCode.Jump   := $E9; // Jmp opcode
  NewCode.Offset := PByte(AHookProc)-PByte(PatchPos)-SizeOf(TPatchCode);
  if not VirtualProtect(PatchPos, SizeOf(TPatchCode), PAGE_EXECUTE_READWRITE, @OldProtect) then
    RaiseLastOSError;

  // Empty static function may have single "RET" instruction.
  // For patch we need 5 bytes at least, so we can't patch such functions.
  Assert(PByte(PatchPos)^<>$C3, 'Too short to patch...');

  EnableHook;
  HookInstalled := 'Y';
end;

procedure TPatchRecord.UninstallHook;
begin
  if Installed then
  begin
    HookInstalled := '';
    DisableHook;
  end;
end;

procedure TPatchRecord.EnableHook;
begin
  PatchPos^ := NewCode;
end;

procedure TPatchRecord.DisableHook;
begin
  PatchPos^ := OriginalCode;
end;

{ TMethodHookManager<T> }

constructor TMethodHookManager<T>.Create(AOriginalProc, AHookProc: pointer);
begin
  Handlers := TDictionary<int64, T>.Create;
  Patch.InstallHook(AOriginalProc, AHookProc);
end;

destructor TMethodHookManager<T>.Destroy;
begin
  if Patch.Installed then
    Patch.DisableHook;
  FreeAndNil(Handlers);
  inherited;
end;

function TMethodHookManager<T>.RegHook(AHandler: T): Int64;
begin
  inc(IdCounter);
  Result := IdCounter;
  Handlers.Add(Result, AHandler);
end;

procedure TMethodHookManager<T>.DisableHook;
begin
  Patch.DisableHook;
end;

procedure TMethodHookManager<T>.EnableHook;
begin
  Patch.EnableHook;
end;

function TMethodHookManager<T>.UnregHook(const AHookId: int64): Boolean;
begin
  Result := Handlers.ContainsKey(AHookId);
  if Result then
    Handlers.Remove(AHookId);
end;

end.
