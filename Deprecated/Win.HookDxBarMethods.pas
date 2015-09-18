unit Win.HookDxBarMethods;
{
  03.02.2015 (AH)
  In this unit we are trying to fix (or minimize effect) of DEX bugs:
  https://www.devexpress.com/Support/Center/Question/Details/T203158
    Form with TdxRibbon is flickering when ribbons are merging.
  https://www.devexpress.com/Support/Center/Question/Details/T203602
    MDI-child TdxRibbonForm painted with artifacts when Style is RS2013
    and SupportNonClientDrawing=True
  To apply the patch just include HookDxBarMethods somewhere in the project.

  IMPORTANT! Patch is based on the original code from dxBar.pas and may became
  invalid/broken if (when) dxBar.pas will be changed. This patch should be
  removed from the projects as soon as bugs are fixed by DevExpress.
}

interface

uses
  dxBar, dxMessages, Win.HookClassMethods, System.SysUtils, Winapi.Windows,
  Winapi.Messages, Vcl.Controls, Vcl.Forms;

const
  DXM_BAR_MSGBASE        = WM_USER + 3200;
  DXM_BAR_UNLOCKMAINFORM = DXM_BAR_MSGBASE + 1;
  DXM_BAR_FIXCORNERS     = DXM_BAR_MSGBASE + 2;

type
  TdxBarManagerMDIStateHelperHook = class(TdxBarManagerMDIStateHelper)
  protected
    procedure Hook_WndProc(var Message: TMessage);
    function MyDoChildStateChanged(AWnd: HWND; AChange: TdxBarMDIStateChange): Boolean;
  public
    procedure Hook_DoActiveChildChanged(const ANewActiveChild, AOldActiveChild: HWND);
    procedure Hook_DoActiveChildMaximizedChanged(const AActiveChild: HWND);
  end;

var
  PatchWndProc: TPatchRecord;
  PatchDoActiveChildChanged: TPatchRecord;
  PatchDoActiveChildMaximizedChanged: TPatchRecord;

procedure Install;
procedure Uninstall;

implementation

procedure DisableFormDraw(f: TWinControl);
begin
  if f=nil then exit;
  SendMessage(f.Handle, WM_SETREDRAW, WPARAM(False), 0);
end;

procedure EnableFormDraw(f: TWinControl);
begin
  if f=nil then exit;
  SendMessage(f.Handle, WM_SETREDRAW, WPARAM(True), 0);
  RedrawWindow(f.Handle, nil, 0, RDW_INVALIDATE or RDW_FRAME or RDW_ERASE or RDW_ALLCHILDREN);
end;

procedure CorrectMDIChilds;
var
  i: Integer;
begin
  if Application.MainForm<>nil then
    for i := 0 to Application.MainForm.MDIChildCount-1 do
    begin
      Application.MainForm.MDIChildren[i].Width := Application.MainForm.MDIChildren[i].Width+1;
      Application.MainForm.MDIChildren[i].Width := Application.MainForm.MDIChildren[i].Width-1;
    end;
end;

// HOOK.
procedure TdxBarManagerMDIStateHelperHook.Hook_WndProc(var Message: TMessage);
begin
  if (Message.Msg = DXM_BAR_UNLOCKMAINFORM) and (Application.MainForm<>nil) then
    EnableFormDraw(Application.MainForm);
  if Message.Msg = DXM_BAR_FIXCORNERS then
    CorrectMDIChilds;
  PatchWndProc.DisableHook;
  try
    inherited WndProc(Message);
  finally
    PatchWndProc.EnableHook;
  end;
end;

type
  TdxBarManagerH = class(TdxBarManager);

// Copy-paste of from TdxBarManagerMDIStateHelper.DoChildStateChanged.
// We implemented it as function, because we need to know when operation
// was postponded (PostMessage), in this case we will unlock redrawing later.
function TdxBarManagerMDIStateHelperHook.MyDoChildStateChanged(AWnd: HWND;
  AChange: TdxBarMDIStateChange): Boolean;
var
  AChildBarManager: TdxBarManager;
  AMergeOperation: TdxBarMergeOperation;
begin
  Result := False;
  UpdateSystemMenu(AChange, AWnd);
  if GetMergingParameters(AWnd, AChange, AMergeOperation, AChildBarManager) then
  begin
    PostMergeOperations.AddOrSet(AChildBarManager, AMergeOperation);
    if ((AChange = scMaximizedChanged) or IsZoomed(AWnd)) and not TdxBarManagerH(AChildBarManager).IsDestroying then
    begin
      PostMessage(Handle, DXM_BAR_MERGE, 0, LPARAM(AChildBarManager));
      Result := True;
    end
    else
      SendMessage(Handle, DXM_BAR_MERGE, 0, LPARAM(AChildBarManager));
  end;
  NotifyMDIStateChangedHandlers(AChange, AWnd);
end;

// HOOK.
// Copy-paste from TdxBarManagerMDIStateHelper.DoChildStateChanged.
// But we postpond EnableFormDraw operation if necessary.
procedure TdxBarManagerMDIStateHelperHook.Hook_DoActiveChildChanged(
  const ANewActiveChild, AOldActiveChild: HWND);

  function NeedLockMainForm: Boolean;
  var
    AChildBarManager: TdxBarManager;
    AMergeOperation: TdxBarMergeOperation;
  begin
    Result := (Application.MainForm <> nil) and IsWindowVisible(Application.MainForm.Handle) and
      (GetMergingParameters(ANewActiveChild, scChildActivated, AMergeOperation, AChildBarManager) or
       GetMergingParameters(AOldActiveChild, scChildDeactivated, AMergeOperation, AChildBarManager));
  end;

var
  IsLockMainFormDrawingNeeded: Boolean;
  Delayed: Boolean;
begin
  IsLockMainFormDrawingNeeded := NeedLockMainForm;
  Delayed := False;
  if IsLockMainFormDrawingNeeded then
    DisableFormDraw(Application.MainForm);
  if MyDoChildStateChanged(AOldActiveChild, scChildDeactivated) then Delayed := True;
  if MyDoChildStateChanged(ANewActiveChild, scChildActivated) then Delayed := True;
  if IsLockMainFormDrawingNeeded then
    if Delayed then
      PostMessage(Handle, DXM_BAR_UNLOCKMAINFORM, 0,0)
    else
      EnableFormDraw(Application.MainForm);
end;

// HOOK.
// Copy-paste from TdxBarManagerMDIStateHelper.DoChildStateChanged, but:
// - We use DisableFormDraw/EnableFormDraw here.
// - We send DXM_BAR_FIXCORNERS to fix later bug of DEX (see link bellow).
// https://www.devexpress.com/support/center/Question/Details/T203602
procedure TdxBarManagerMDIStateHelperHook.Hook_DoActiveChildMaximizedChanged(
  const AActiveChild: HWND);
begin
  if Application.MainForm<>nil then
    DisableFormDraw(Application.MainForm);
  MyDoChildStateChanged(AActiveChild, scMaximizedChanged);
  if Application.MainForm<>nil then
    PostMessage(Handle, DXM_BAR_UNLOCKMAINFORM, 0,0);
  if not IsZoomed(AActiveChild) then
    PostMessage(Handle, DXM_BAR_FIXCORNERS, 0,0);
end;

procedure Install;
begin
  PatchWndProc.InstallHook(
    @TdxBarManagerMDIStateHelperHook     .WndProc,
    @TdxBarManagerMDIStateHelperHook.Hook_WndProc);
  PatchDoActiveChildChanged.InstallHook(
    @TdxBarManagerMDIStateHelperHook     .DoActiveChildChanged,
    @TdxBarManagerMDIStateHelperHook.Hook_DoActiveChildChanged);
  PatchDoActiveChildMaximizedChanged.InstallHook(
    @TdxBarManagerMDIStateHelperHook     .DoActiveChildMaximizedChanged,
    @TdxBarManagerMDIStateHelperHook.Hook_DoActiveChildMaximizedChanged);
end;

procedure Uninstall;
begin
  PatchWndProc.UninstallHook;
  PatchDoActiveChildChanged.UninstallHook;
  PatchDoActiveChildMaximizedChanged.UninstallHook;
end;

end.
