unit VCL.MouseWheelRedirector;
(*******************************************************************************
Info    : Enhanced processing of mouse wheel messages (app-wide).
Author  : Andrei Galatyn
Date    : 10.07.2015
  Application-wide mouse wheel redirector:
  - Redirects mouse wheel messages to control at mouse position (Delphi sends such messages to focused control).
  - If control doesn't process wheel message, then redirect it to parent (up to main form).
  - Add mouse wheel support for TScrollbox/TcxScrollBox.
  - Direct mouse wheel messages to combo box control only if it has focus AND pointed by mouse at same time.
  - If there is no control pointed by mouse, then scroll focused control instead (but not ComboBox).
  - If MDI main form is pointed by mouse, then focused control/form should be scrolled.
  To enable mouse wheel redirector, include the unit to project.
*******************************************************************************)

{$DEFINE MWR_DevExpress}

interface

implementation

uses
  {$IFDEF MWR_DevExpress}
  cxDropDownEdit, cxGridCustomView, cxControls, cxScrollBox,
  {$ENDIF}
  System.SysUtils, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Forms,
  System.Math, Vcl.StdCtrls, Vcl.AppEvnts, System.Classes;

type
  {$IFDEF MWR_DevExpress}
  TcxControlAccess = class(TcxControl);
  {$ENDIF}
  TWinControlAccess = class(TWinControl);
  TMouseWheelRedirector = class
  protected
    class var
      FInstance: TMouseWheelRedirector;
    var
      Events: TApplicationEvents;

    procedure AppOnMessage(var Msg: TMsg; var Handled: Boolean);
    class function VScrollControl(C: TWinControl; Down: Boolean; LineCount: integer): Boolean; static;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function FindSubcontrolAtPos(AControl: TControl; AScreenPos, AClientPos: TPoint): TControl;
var
  i: Integer;
  C: TControl;
begin
  Result := nil;
  C := AControl;
  if (C=nil) or not C.Visible or not TRect.Create(C.Left, C.Top, C.Left+C.Width, C.Top+C.Height).Contains(AClientPos) then
    Exit;
  Result := AControl;
  if AControl is TWinControl then
    for i := 0 to TWinControl(AControl).ControlCount-1 do
    begin
      C := FindSubcontrolAtPos(TWinControl(AControl).Controls[i], AScreenPos, AControl.ScreenToClient(AScreenPos));
      if C<>nil then
        Result := C;
    end;
end;

function FindControlAtPos(AScreenPos: TPoint): TControl;
var
  i: Integer;
  f,m: TForm;
  p: TPoint;
  r: TRect;
begin
  Result := nil;
  for i := Screen.FormCount-1 downto 0 do
    begin
      f := Screen.Forms[i];
      if f.Visible and (f.Parent=nil) and (f.FormStyle<>fsMDIChild) and
        TRect.Create(f.Left, f.Top, f.Left+f.Width, f.Top+f.Height).Contains(AScreenPos)
      then
        Result := f;
    end;
  Result := FindSubcontrolAtPos(Result, AScreenPos, AScreenPos);
  if (Result is TForm) and (TForm(Result).ClientHandle<>0) then
  begin
    WinAPI.Windows.GetWindowRect(TForm(Result).ClientHandle, r);
    p := TPoint.Create(AScreenPos.X-r.Left, AScreenPos.Y-r.Top);
    m := nil;
    for i := TForm(Result).MDIChildCount-1 downto 0 do
    begin
      f := TForm(Result).MDIChildren[i];
      if TRect.Create(f.Left, f.Top, f.Left+f.Width, f.Top+f.Height).Contains(p) then
        m := f;
    end;
    if m<>nil then
      Result := FindSubcontrolAtPos(m, AScreenPos, p);
  end;
end;

function VertScrollBarVisible(WindowHandle: THandle): Boolean;
begin
  Result := (GetWindowlong(WindowHandle, GWL_STYLE) AND WS_VSCROLL) <> 0
end;

{$IFDEF MWR_DevExpress}
function cxGridIsFocused: Boolean;
var
  C: TWinControl;
begin
  C := Screen.ActiveControl;
  while C<>nil do
  begin
    if C is TcxGridSite then
    begin
      result := True;
      Exit;
    end;
    C := C.Parent;
  end;
  result := False;
end;

function cxScrollGrid(ASite: TcxGridSite; ADelta: integer): Boolean;
var
  i,j,p: Integer;
begin
  j := ASite.VScrollBar.Position;
  for i := 0 to 2 do
  begin
    p := ASite.VScrollBar.Position;
    if ADelta>0 then
      TcxControlAccess(ASite).Scroll(sbVertical, scLineUp, p)
    else
      TcxControlAccess(ASite).Scroll(sbVertical, scLineDown, p);
  end;
  result := j<>ASite.VScrollBar.Position;
end;
{$ENDIF}

{ TMouseWheelRedirector }

{
  j := SetScrollInfo(TcxCustomComboBox(C).Handle, ???

  1. SetScrollPos changes position of bar, but content doesn't scroll (msg processed by bar, not by control)
    SetScrollPos(TWinControl(C).Handle, SB_VERT, i-Delta, True);

  2. Normally SendMessage works fine, but cxGrid installs message hook (seems so).
  When grid is focused, we can't deliver the message to the recipient (it is hooked&processed by grid instead).
    SendMessage(TWinControl(C).Handle, Msg.Message, Msg.wParam, Msg.lParam);

  3. Same problem as with direct call of WndProc - when cxGrid is focused, destination control
  doesn't process the message (when grid is not focused everything is ok).
    TWinControlAccess(C).WndProc(M);

  4. We don't have access to WMVScroll method ("private" section).
    TWinControlAccess(C).WMVScroll(
}
class function TMouseWheelRedirector.VScrollControl(C: TWinControl;
  Down: Boolean; LineCount: integer): Boolean;
var
  Msg: TWMScroll;
  VPos: Integer;
begin
  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Msg := WM_VSCROLL;
  Msg.ScrollCode := IfThen(Down, SB_LINEDOWN, SB_LINEUP);
  VPos := GetScrollPos(C.Handle, SB_VERT);
  while LineCount>0 do
  begin
    SendMessage(C.Handle, WM_VSCROLL, TMessage(Msg).WParam, TMessage(Msg).LParam);
    Dec(LineCount);
  end;
  Result := VPos<>GetScrollPos(C.Handle, SB_VERT);
end;

procedure TMouseWheelRedirector.AppOnMessage(var Msg: TMsg;
  var Handled: Boolean);
var
  i,Delta: Integer;
  M: TMessage;
  C: TControl;
  AutoPointed: Boolean;
begin
  if (Msg.Message<>WM_MOUSEWHEEL) and (Msg.Message<>WM_MOUSEHWHEEL) then
    Exit;

  M.Msg := Msg.message;
  M.WParam := Msg.wParam;
  M.LParam := Msg.lParam;
  M.Result := 0;

  // TMSHMouseWheel differ from TWMMouseWheel in Delphi, but according to MSDN
  // they are equal, so we use TWMMouseWheel for both Message types.
  Delta := TWMMouseWheel(M).WheelDelta;
  C := FindControlAtPos(TPoint.Create(TWMMouseWheel(M).XPos, TWMMouseWheel(M).YPos));
  AutoPointed := False; // FindControlAtPos returns actual control pointed by mouse.
  if (C=nil) or (C=Application.MainForm) and (Application.MainForm.FormStyle=fsMDIForm) then
  begin
    C := Screen.ActiveControl;
    AutoPointed := True; // If there is no pointed control, we use focused instead.
  end;
  if C=nil then
    Exit;
  repeat
    if (C is TWinControl) and C.Enabled and C.Visible then
      if False then begin end

      // TScrollBox doesn't support mouse wheel (XE8), we have to workaround it.
      // We can use general VScrollControl function, but it is better to
      // use specific method and scroll pos according to Delta.
      else
      if (C is TScrollingWinControl) and TScrollingWinControl(C).VertScrollbar.Visible then
      begin
        i := TScrollingWinControl(C).VertScrollbar.Position;
        TScrollingWinControl(C).VertScrollbar.Position := TScrollingWinControl(C).VertScrollbar.Position - Delta;
        Handled := i<>TScrollingWinControl(C).VertScrollbar.Position;
      end

      {$IFDEF MWR_DevExpress}
      else
      if (C is TcxCustomScrollBox) and TcxCustomScrollBox(C).VertScrollbar.Visible then
      begin
        i := TcxCustomScrollBox(C).VertScrollbar.Position;
        TcxCustomScrollBox(C).VertScrollbar.Position := TcxCustomScrollBox(C).VertScrollbar.Position - Delta;
        Handled := i<>TcxCustomScrollBox(C).VertScrollbar.Position;
      end
      {$ENDIF}

      // General VScrollControl function works fine for TMemo/TcxMemo.
      // TcxMemo will be processed here too, because TcxCustomInnerMemo is
      // inherited from TCustomMemo.
//      else
//      if C is TCustomMemo then
//      begin
//        i := SendMessage(TCustomMemo(C).Handle, EM_GETFIRSTVISIBLELINE, 0,0);
//        SendMessage(TCustomMemo(C).Handle, EM_LINESCROLL, 0, -Sign(Delta)*3);
//        Handled := i<>SendMessage(TCustomMemo(C).Handle, EM_GETFIRSTVISIBLELINE, 0,0);
//      end

      // TComboBox / TcxComboBox (they don't have scrollbar)
      // It is dangerous to scroll combo by wheel, we allow it
      // only if combo is focused and pointed by mouse at same time.
      else
      if not AutoPointed and TWinControl(C).Focused and (C is TCustomComboBox) then
      begin
        i := TCustomComboBox(C).ItemIndex;
        SendMessage(TComboBox(C).Handle, Msg.Message, Msg.wParam, Msg.lParam);
        Handled := i<>TCustomComboBox(C).ItemIndex;
      end

      {$IFDEF MWR_DevExpress}
      else
      if TWinControl(C).Focused and (C is TcxCustomComboBox) then
      begin
        i := TcxCustomComboBox(C).ItemIndex;
        SendMessage(TcxCustomComboBox(C).Handle, Msg.Message, Msg.wParam, Msg.lParam);
        Handled := i<>TcxCustomComboBox(C).ItemIndex;
      end
      else
      if C is TcxGridSite then
        Handled := cxScrollGrid(TcxGridSite(C), Delta)
      {$ENDIF}

      // Any scrollable WinControl (even if vertical scrollbar is not visible)
      else
      //if VertScrollBarVisible(TWinControl(C).Handle) then
        Handled := VScrollControl(TWinControl(C), Delta<0, 3);

    C := C.Parent;
  until (C=nil) or Handled;

  // If we didn't process the wheel message, then control should process it by default.
//  Handled := Handled
//    or (Screen.ActiveControl is TComboBox)
//    {$IFDEF MWR_DevExpress}
//    or (Screen.ActiveControl<>nil) and (Screen.ActiveControl.Parent is TcxComboBox)
//    {$ENDIF}
//  ;

  // We have to disable default wheel processing for controls, because it is hard
  // to eliminate all side effects. Another reason - if we don't support some
  // component, we should know it as soons as possible.
  Handled := True;
end;

constructor TMouseWheelRedirector.Create;
begin
  Events := TApplicationEvents.Create(nil);
  Events.OnMessage := AppOnMessage;
end;

destructor TMouseWheelRedirector.Destroy;
begin
  FreeAndNil(Events);
  inherited;
end;

initialization
  TMouseWheelRedirector.FInstance := TMouseWheelRedirector.Create;

finalization
  FreeAndNil(TMouseWheelRedirector.FInstance);

end.