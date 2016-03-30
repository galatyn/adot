unit adot.VCL.Tools;

interface

uses
  {$IFDEF DEX}
  cxGrid, cxGridCustomView, cxGridCustomTableView, cxGridChartView, cxCustomData, cxStyles,
  {$ENDIF}
  IdGlobal,
  IdHashMessageDigest,
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.StrUtils,
  System.Math,
  System.UITypes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.ActnList,
  Winapi.Windows,
  Winapi.Messages;

type
  TDelegatedOnControlWithBreak = reference to procedure(AControl: TControl; var ABreak: boolean);
  TDelegatedOnControl = reference to procedure(AControl: TControl);

procedure ForEachControl(AStart: TControl; ACallback: TDelegatedOnControlWithBreak); overload;
procedure ForEachControl(AStart: TControl; ACallback: TDelegatedOnControl); overload;

type
  TFormSnapshot = class
  private
  protected
    type
      TControlH = class(TControl);

    class function GetComponentId(C: TComponent): String; static;
    {$IFDEF DEX}
    class procedure GetCxGridState(Grid: TcxGrid; Dst: TStrings); overload; static;
    class procedure GetCxViewState(View: TcxCustomGridTableView; Dst: TStrings); overload; static;
    class procedure GetCxViewState(View: TcxGridChartView; Dst: TStrings); overload; static;
    {$ENDIF}
    class procedure GetControlState(C: TControl; Dst: TStrings); static;
    class function StrToReadable(s: string): String; static;
    class function ValueToText(AValue: string): string; static;
    class function ToJsonStr(const s: string; const q: string = '"'): string; static;
  public
    class procedure Get(AForm: TCustomForm; ADst: TStrings); overload; static;
    class procedure Get(ADst: TStrings); overload; static;
    class function CaptureScreen(ADst: Vcl.Graphics.TBitmap; x,y,w,h,screenx,screeny: integer):Boolean; static;
  end;

  // To receive/process messages in component when it is not inherited from TWinControl.
  // For every message (call of WndProc) will call handler twice - before and after standard processing.
  TMessenger = class
  public
    type
      TOnMessage = procedure(var AMessage: TMessage) of object;
      TOnMessageRef = reference to procedure(var AMessage: TMessage);

  protected
    FOnMessage: TOnMessage;
    FOnMessageRef: TOnMessageRef;
    FWnd: HWND;

    procedure WndProc(var Message: TMessage);

  public
    constructor Create; overload;
    constructor Create(AMessageHandler: TOnMessageRef); overload;
    constructor Create(AMessageHandler: TOnMessage); overload;
    destructor Destroy; override;

    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    property OnMessageRef: TOnMessageRef read FOnMessageRef write FOnMessageRef;
    property Handle: HWND read FWnd;
  end;
  
procedure DebugMsg(const Msg: String);
procedure DisableFormDraw(f: TWinControl);
procedure EnableFormDraw(f: TWinControl);

function FindControlAtPos(AScreenPos: TPoint): TControl;
function FindControlAtMousePos: TControl;

implementation

uses
  adot.Tools,
  adot.Graphics;

function ForEachControlBrk(AStart: TControl; ACallback: TDelegatedOnControlWithBreak):Boolean;
var
  i: Integer;
begin
  result := False; // break
  ACallback(AStart, result);
  if not result and (AStart is TWinControl) then
    for i := TWinControl(AStart).ControlCount-1 downto 0 do
      if ForEachControlBrk(TWinControl(AStart).Controls[i], ACallback) then
      begin
        result := True;
        break;
      end;
end;

procedure ForEachControl(AStart: TControl; ACallback: TDelegatedOnControlWithBreak);
begin
  ForEachControlBrk(AStart, ACallback);
end;

procedure ForEachControl(AStart: TControl; ACallback: TDelegatedOnControl);
var
  i: Integer;
begin
  if AStart=nil then
    exit;
  ACallback(AStart);
  if AStart is TWinControl then
    for i := TWinControl(AStart).ControlCount-1 downto 0 do
      ForEachControl(TWinControl(AStart).Controls[i], ACallback);
end;

{ TFormSnapshot }

class function TFormSnapshot.StrToReadable(s: string): String;
var
  i: Integer;
begin
  result := '';
  for i := Low(s) to High(s) do
    if (s[i]<#32) or (s[i]=#255) then
      result := result + '#' + IntToStr(Integer(s[i]))
    else
      result := result + s[i];
end;

class function TFormSnapshot.ValueToText(AValue: string): string;
begin
  AValue := StrToReadable(AValue);
  if length(AValue)<=40 then
    result := AValue
  else
    result := format('MD5(%d)=', [length(AValue)]) + String(THash.MD5.GetHashString(AValue));
end;

class procedure TFormSnapshot.GetControlState(C: TControl; Dst: TStrings);
var
  S: String;
begin
  S := TControlH(C).Text;
  while TControlH(C).ParentColor and (TControlH(C).Parent<>nil) do
    C := C.Parent;
  Dst.Add(Format('{"V": %s, "C": %s}', [
    ToJsonStr(ValueToText(S)),
    ToJsonStr(adot.Graphics.TColorUtils.GetBasicColorName(TControlH(C).Color))
  ]));
end;

class function TFormSnapshot.GetComponentId(C: TComponent): String;
var
  s: TList<String>;
  i: Integer;
begin
  s := TList<String>.Create;
  try
    while (c<>nil) do
      // Normally it is "inner" editors created internally by higher-level
      // controls, so we can skip them.
      if c.Name='' then
        Break
      else
      begin
        s.Add(c.Name);
        c := c.Owner;
      end;
    if s.Count=0 then
      result := ''
    else
      result := s[s.Count-1];
    for i := s.Count-2 downto 0 do
      result := result + '\' + s[i];
  finally
    FreeAndNil(s);
  end;
end;

class function TFormSnapshot.ToJsonStr(const s: string; const q: string = '"'):string;
var
  i: Integer;
begin
  if (pos('"', s)<Low(s)) and (pos('\', s)<Low(s)) then
    result := q + s + q
  else
  begin
    result := q;
    for i := Low(s) to High(s) do
      if (s[i]='"') or (s[i]='\') then
        result := result + '\' + s[i]
      else
        result := result + s[i];
    result := result + q;
  end;
end;

{$IFDEF DEX}
class procedure TFormSnapshot.GetCxViewState(View: TcxCustomGridTableView; Dst: TStrings);
var
  i,j: Integer;
  Rec: TcxCustomGridRecord;
  CellStyle: TcxStyle;
  t,s: string;
begin
  Dst.Add('{');

  // columns
  j := 0;
  for i := 0 to View.VisibleItemCount-1 do
    if Trim(View.VisibleItems[i].Caption)<>'' then
      inc(j);
  if j>0 then
  begin
    Dst.Add('  "Columns": [');
      s := '    ';
      for i := 0 to View.VisibleItemCount-1 do
        s := s + IfThen(i=0,'',', ') + ToJsonStr(Trim(View.VisibleItems[i].Caption));
      Dst.Add(s);
    Dst.Add('  ],');
  end;

  // data
  Dst.Add('  "Data": [');
  View.ViewData.Expand(True);
  for i := 0 to View.ViewData.RecordCount-1 do
  begin
    Rec := View.ViewData.Records[i];
    if not Rec.IsData then
      Continue;
    s := '    [';
    for j := 0 to View.VisibleItemCount-1 do
    begin
      s := s + IfThen(j=0,'',', ') + '{';
      if Assigned(View.Styles.OnGetContentStyle) then
        View.Styles.OnGetContentStyle(View, Rec, View.VisibleItems[j], CellStyle)
      else
        CellStyle := View.Styles.Content;
      if CellStyle=nil then
        t := 'default'
      else
        t := TColors.GetBaseColorName(CellStyle.Color);
      s := s + format('"V": %s, ', [ToJsonStr(TVar.AsString(Rec.Values[View.VisibleItems[j].Index]))]);
      s := s + format('"C": %s', [ToJsonStr(t)]);
      s := s + '}';
    end;
    s := s + ']' + IfThen(i<View.ViewData.RecordCount-1,',','');
    Dst.Add(s);
  end;
  Dst.Add('  ]'); // data
  Dst.Add('}'); // view

{  dc := View.DataController;
  for i := 0 to dc.FilteredRecordCount-1 do
  begin
    RecordIndex := dc.FilteredRecordIndex[i];
    s := '';
    for j := 0 to View.VisibleItemCount-1 do
      s := s + IfThen(j=0,'',' ') + EnquoteCaption(TVar.AsString(dc.Values[RecordIndex, View.VisibleItems[j].Index]));
    result := result + s + #13#10;
  end;}
end;
{$ENDIF}

{$IFDEF DEX}
class procedure TFormSnapshot.GetCxViewState(View: TcxGridChartView; Dst: TStrings);
var
  i,j: Integer;
  s: String;
  Series: TcxGridChartSeries;
begin
  Dst.Add('{');
  j := 0;
  for i := 0 to View.VisibleSeriesCount-1 do
    if Trim(View.VisibleSeries[i].DisplayText)<>'' then
      inc(j);
  if j>0 then
  begin
    Dst.Add('  "Columns": [');
      s := '  ';
      for i := 0 to View.VisibleSeriesCount-1 do
        s := s + IfThen(i=0,'',', ') + ToJsonStr(Trim(View.VisibleSeries[i].DisplayText));
      Dst.Add(s);
    Dst.Add('  ],');
  end;

  // data
  Dst.Add('  "Data": [');
  for i := 0 to View.VisibleSeriesCount-1 do
  begin
    s := '    [';
    Series := View.VisibleSeries[i];
    for j := 0 to Series.ValueCount-1 do
      s := s + IfThen(j=0,'',', ') + ToJsonStr(TVar.AsString(Series.Values[j]));
    s := s + ']' + IfThen(i<View.VisibleSeriesCount-1, ',', '');
    Dst.Add(s);
  end;
  Dst.Add('  ]'); // data
  Dst.Add('}'); // view
end;
{$ENDIF}

{$IFDEF DEX}
class procedure TFormSnapshot.GetCxGridState(Grid: TcxGrid; Dst: TStrings);
var
  View: TcxCustomGridView;
begin
  if Grid.Levels.Count=1 then
  begin
    View := Grid.Levels[0].GridView;
    if View is TcxCustomGridTableView then
      GetCxViewState(TcxCustomGridTableView(View), Dst)
    else
    if View is TcxGridChartView then
      GetCxViewState(TcxGridChartView(View), Dst);
  end;
end;
{$ENDIF}

type
  TFormsnapshotRec = class
    Id: String;
    Lines: TStringList;

    constructor Create(const AId: string);
    destructor Destroy; override;
  end;

constructor TFormsnapshotRec.Create(const AId: string);
begin
  inherited Create;
  Id := AId;
  Lines := TStringList.Create;
end;

destructor TFormsnapshotRec.Destroy;
begin
  FreeAndNil(Lines);
  inherited;
end;

class procedure TFormSnapshot.Get(AForm: TCustomForm; ADst: TStrings);
var
  Items: TObjectList<TFormsnapshotRec>;
  i,j: Integer;
  l: TStrings;
  s: string;
begin
  Items := TObjectList<TFormsnapshotRec>.Create(TDelegatedComparer<TFormsnapshotRec>.Create(
    function(const L,R: TFormsnapshotRec): Integer
    begin
      Result := CompareText(L.Id, R.Id);
    end));
  try

    // collect info
    TComponentUtils.ForEachComponent(AForm,
      procedure(C: TComponent)
      var
        Id: String;
        Snapshot: TFormsnapshotRec;
      begin
        if not (C is TControl) or not TControl(C).Visible then
          Exit;
        Id := GetComponentId(C);
        if Id='' then
          Exit;
        Snapshot := TFormsnapshotRec.Create(Id);
        Items.Add(Snapshot);
        {$IFDEF DEX}
        if C is TcxGrid then
          GetCxGridState(TCxGrid(C), Snapshot.Lines)
        else
          GetControlState(TControl(C), Snapshot.Lines);
        {$ELSE}
        GetControlState(TControl(C), Snapshot.Lines);
        {$ENDIF}
      end);

    // get result
    Items.Sort;
    ADst.Add('{');
    for i := 0 to Items.Count-1 do
    begin
      l := Items[i].Lines;
      s := IfThen(i<Items.Count-1, ',', '');
      case l.Count of
        0: ADst.Add(Format('  %s: null%s', [ToJsonStr(Items[i].Id), s]));
        1: ADst.Add(Format('  %s: %s%s', [ToJsonStr(Items[i].Id), l[0], s]));
        else
        begin
          ADst.Add(Format('  %s:', [ToJsonStr(Items[i].Id)]));
          for j := 0 to l.Count-2 do
            ADst.Add(Format('    %s', [l[j]]));
          ADst.Add(Format('    %s%s', [l[l.Count-1], s]));
        end;
      end;
    end;
    ADst.Add('}');
  finally
    FreeAndNil(Items);
  end;
end;

class function TFormSnapshot.CaptureScreen(ADst: Vcl.Graphics.TBitmap; x, y, w, h, screenx,
  screeny: integer): Boolean;
var
  DC: hWnd;
begin
  DC := GetDC(0);
  Result := DC<>0;
  if Result then
    bitblt(ADst.Canvas.Handle, x, y, w, h, DC, screenx, screeny, SRCCOPY{ or CAPTUREBLT});
  ReleaseDC(0, DC);
end;

class procedure TFormSnapshot.Get(ADst: TStrings);
var
  i: Integer;
  c: TComponent;
  VisibleForms: TDictionary<TForm, Byte>;
begin
  VisibleForms := TDictionary<TForm, Byte>.Create;
  try
    for i := 0 to Application.MainForm.ComponentCount-1 do
    begin
      c := Application.MainForm.Components[i];
      if not (c is TAction) or
        not Assigned(TAction(c).OnExecute) or
        not TAction(c).Visible or
        not TAction(c).Enabled
      then
        Continue;
      VisibleForms.Clear;

    end;
  finally
    FreeAndNil(VisibleForms);
  end;
end;

procedure DebugMsg(const Msg: String);
begin
  OutputDebugString(PChar(Msg))
end;

procedure InvalidateComponent(c: TComponent; Recursive: boolean = True);
var
  i: Integer;
begin
  if c=nil then
    exit;
  if c is TControl then
    TControl(c).Invalidate;
  if Recursive then
    for i := 0 to c.ComponentCount-1 do
      InvalidateComponent(c.Components[i], True);
end;

procedure DisableFormDraw(f: TWinControl);
begin
  if f=nil then
    exit;
  SendMessage(f.Handle, WM_SETREDRAW, WPARAM(False), 0);
end;

procedure EnableFormDraw(f: TWinControl);
begin
  if f=nil then
    exit;
  SendMessage(f.Handle, WM_SETREDRAW, WPARAM(True), 0);
  RedrawWindow(f.Handle, nil, 0,
    RDW_INVALIDATE or
    RDW_INTERNALPAINT or
    RDW_ERASE or
    RDW_ALLCHILDREN or
    RDW_UPDATENOW or
    RDW_ERASENOW or
    RDW_FRAME
  );
(*    //RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
  InvalidateComponent(f);
  f.Repaint;
  for i := 0 to Application.MainForm.MDIChildCount-1 do
  begin
    Application.MainForm.MDIChildren[i].Width := Application.MainForm.MDIChildren[i].Width+1;
    Application.MainForm.MDIChildren[i].Width := Application.MainForm.MDIChildren[i].Width-1;
  end;*)
end;

{ TMessenger }

constructor TMessenger.Create;
begin
  inherited;
  FWnd := AllocateHWnd(WndProc);
end;

destructor TMessenger.Destroy;
begin
  inherited;
  DeallocateHWnd(FWnd);
end;

constructor TMessenger.Create(AMessageHandler: TOnMessage);
begin
  Create;
  FOnMessage := AMessageHandler;
end;

constructor TMessenger.Create(AMessageHandler: TOnMessageRef);
begin
  Create;
  FOnMessageRef := AMessageHandler;
end;

procedure TMessenger.WndProc(var Message: TMessage);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Message);
  if Assigned(FOnMessageRef) then
    FOnMessageRef(Message);
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

function FindControlAtMousePos: TControl;
begin
  Result := FindControlAtPos(Mouse.CursorPos);
end;

end.

