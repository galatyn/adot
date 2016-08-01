unit adot.VCL.Tools;

{ Definition of classes/record types:

  TAppActions = class
    Get all "executable" components of form (TMenu/TAction/...).
    Used by automated testing framework & quick search window (available by F11).

  TCanvasUtils = class
    MaxFitLength / BlendRectangle and other graphic functions specific to VCL.

  TControlUtils = class
    ForEach, FindForm, GetShortCaption and other.

  TMDIHelper<T: class> = class
    FindMDIChild etc.

  TVCLFileUtils = class
    CopyFile without locking UI etc.

  TVCLStreamUtils = class
    Copy streams without locking UI etc.

}
interface

uses
  adot.Types,
  Winapi.ShellAPI,
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Character,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.StrUtils,
  Vcl.ActnList,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Menus,
  Vcl.Controls;

type

  {
    Screen object has event OnActiveControlChange, but it doesn't support multiple handlers.
    Chaining of handlers for such event is not safe.
    Objects can install handlers in one sequence and then uninstall it in different sequence, restoring
    "dead" handlers as active.
    To avoid of errors we use list for chaining of handlers.
  }
  TActiveControlListeners = class
  private
    class var
      FList: TList<TNotifyEvent>;
      FOldOnActiveControlChange: TNotifyEvent;

    class property List: TList<TNotifyEvent> read FList;
    class procedure ActiveControlChange(Data, Sender: TObject); static;
  public
    class constructor Create;
    class destructor Destroy;

    class function Add(AHandler: TNotifyEvent): boolean; static;
    class function Exists(AHandler: TNotifyEvent): boolean; static;
    class function Remove(AHandler: TNotifyEvent): boolean; static;
  end;

  {  Simple interface to search for MDI form of specific type. Example:
     var
       MVA: TMVAAvstemmingForm;
     begin
       MVA := TMDIHelper<TMVAAvstemmingForm>.FindMDIChild;
     end; }
  { FindMDIChild etc }	 
  TMDIHelper<T: class> = class
  public
    class function FindMDIChild(ParentForm:TForm; BringToFront : Boolean = True; Sjekk: TFunc<TForm,Boolean> = nil):T; overload; static;
    class function FindMDIChild(BringToFront : Boolean = True; Sjekk: TFunc<TForm,Boolean> = nil):T; overload; static;
    //class function MdiParentClientArea(AParent: TForm = nil): TRect; static;
  end;

  { Get all "executable" components of form (TMenu/TAction/...).
    Used by automated testing framework & quick search window (available by F11). }
  TAppActions = class
  public
    type
      TVerb = record
        Text: string;
        Obj: TComponent;

        constructor Create(const AText: string; AObj: TComponent); overload;
        constructor Create(const AText: string); overload;
      end;

  private
    class function GetIntInPos(const s: string; n: integer): string; static;
    class function FilterAccept(
          Caption, SoekeTekst : String;
          InnholdSoek         : Boolean;
      var Tekst               : string;
      var Indeks              : integer;
          AddedItems          : TDictionary<string, boolean>
    ): Boolean; static;
    class function IsInteger(const s: string): Boolean; static;
    class function GetActionObjComparer(const ASoeketekst: string): TDelegatedComparer<TAppActions.TVerb>; static;

  public

    { Both chars belong to same type (printable char, digit, space, or special char) }
    class function SameCharCategory(const a, b: Char): Boolean; static;

    { TMenu, TButton, ... -> TBasicAction }
    class function ComponentAction(aComponent: TComponent): TBasicAction; static;

    { Component (TMenu, TButton, ...) and/or assigned action are visible }
    class function ComponentVisible(aComponent: TComponent): Boolean; static;

    { Component (TMenu, TButton, ...) and/or assigned action are enabled }
    class function ComponentEnabled(aComponent: TComponent): Boolean; static;

    { Parent of TMenuItem is TMenuITem, parent of TControl is TControl, etc }
    class function ComponentParent(aComponent: TComponent): TComponent; static;

    { Component (TMenu, TAction) has assigned OnClick/OnExecute }
    class function ComponentExecutable(aComponent: TComponent): Boolean; static;

    { Component is enabled&visible + all parents are visible }
    class function ComponentAccessible(aComponent: TComponent): Boolean; static;

    { Execute corresponding OnClick/Action.OnExecute/... for component }
    class function ExecuteComponent(aComponent: TComponent): Boolean; static;

    { find all objects which can be executed (actions, menus, ...) }
    class function FindFormActions(
      AForm                   : TForm;
      AExtraComponents        : TStrings;
      ASoeketekst             : string;
      AAutoAppendMenuItemsTag : integer;
      AAutoAppendActionsTag   : integer;
      ATest                   : TFunc<TComponent,Boolean>
    ): TList<TAppActions.TVerb>;

  end;

  { MaxFitLength / BlendRectangle and other graphic functions specific to VCL }
  TCanvasUtils = class
  public
    class procedure BlendRectangle(Dst: TCanvas; const R: TRect; C: TColor; MixPercent: Byte); static;
    class function MaxFitLength(const Src: string; Dst: TCanvas; WidthPixels: integer): integer; static;
  end;

  { ForEach, FindForm, GetShortCaption and other }
  TControlUtils = class
  public
    type
      TControlBrkProc = reference to procedure(AControl: TControl; var ABreak: boolean);
      TControlProc = reference to procedure(AControl: TControl);

    class procedure SetEnabled(C: TControl; Value: Boolean; Recursive: Boolean = True); static;
    class function FindParentOfClass(AControl: TControl; const AParentClass: TClass): TControl; static;
    class function FindForm(C: TControl): TForm; static;


    { "file://c:\aaaa\sss\ddddddd\file.zip" -> "file://c:\aaa...\file.zip" }
    class function GetShortCaption(
      const Caption        : string;
            FixedHeadChars : integer;
            FixedTailChars : integer;
            Dst            : TCanvas;
            WidthPixels    : integer;
            Separator      : string = '...'): string; static;

    { returns True if enumeration complete, False if canceled }
    class function ForEach(AStart: TControl; ACallback: TControlBrkProc): Boolean; overload; static;
    class procedure ForEach(AStart: TControl; ACallback: TControlProc); overload; static;
    class function GetAll(AStart: TControl): TArray<TControl>; static;
  end;

  { Copy streams without locking UI etc }
  TVCLStreamUtils = class
  public

    { Copy stream functions (from simple to feature-rich):
      1. TStreamUtils.Copy:
           uses standard Delphi streams, UI will freeze until operation is complete.
      2. TVCLStreamUtils.Copy:
           uses standard Delphi streams, UI will not freeze. }
    class function Copy(Src,Dst: TStream; Count,BufSize: integer; ProgressProc: TCopyStreamProgressProc): int64; overload; static;
    class function Copy(Src,Dst: TStream; ProgressProc: TCopyStreamProgressProc): int64; overload; static;
    class function Copy(Src,Dst: TStream): int64; overload; static;
  end;

  { CopyFile without locking UI etc }
  TVCLFileUtils = class
  public

    { Copy file functions (from simple to feature-rich):
      1. TFileUtils.CopyFile:
           uses standard Delphi streams, UI will freeze until operation is complete.
      2. TWinFileUtils.CopyFile:
           uses Windows function CopyFileEx, UI will freeze until operation is complete.
      3. TVCLFileUtils.Copyfile:
           uses standard Delphi streams, UI will not freeze.
      4. TCopyFileProgressDlg.CopyFile:
           uses either Delphi streams or CopyFileEx, UI will not freeze,
           progress bar with cancel command will be available for long operations). }
    class function CopyFile(
      const SrcFileName,DstFileName : string;
        out ErrorMessage            : string;
            ProgressProc            : TCopyFileProgressProc): boolean; overload;

    class function CopyFile(
      const SrcFileName,DstFileName : string;
        out ErrorMessage            : string): boolean; overload;
  end;

implementation

uses
  adot.Collections,
  adot.Tools,
  adot.Win.Tools;

{ MDIHelper }

class function TMDIHelper<T>.FindMDIChild(ParentForm: TForm; BringToFront: Boolean = True; Sjekk: TFunc<TForm,Boolean> = nil): T;
var
  I: integer;
begin
  for I := 0 to ParentForm.MDIChildcount-1 do
    if (ParentForm.MDIChildren[I] is T) and (not Assigned(Sjekk) or Sjekk(ParentForm.MDIChildren[I])) then
    begin
      result := ParentForm.MDIChildren[I] as T;
      if BringToFront then
      begin
        (result as TForm).Show;
        (result as TForm).BringToFront;
      end;
      exit;
    end;
  result := nil;
end;

class function TMDIHelper<T>.FindMDIChild(BringToFront : Boolean = True; Sjekk: TFunc<TForm,Boolean> = nil):T;
begin
  Result := FindMDIChild(Application.MainForm, BringToFront, Sjekk);
end;

//class function TMDIHelper<T>.MdiParentClientArea(AParent: TForm): TRect;
//begin
//  if AParent=nil then
//    AParent := Application.MainForm;
//  if (AParent=nil) or not GetClientRect(AParent.ClientHandle, Result) then
//    result := TRect.Empty;
//end;

{ TVerb }

constructor TAppActions.TVerb.Create(const AText: string; AObj: TComponent);
begin
  Text := AText;
  Obj := AObj;
end;

constructor TAppActions.TVerb.Create(const AText: string);
begin
  Create(AText, nil);
end;

{ TAppActions }

class function TAppActions.ComponentAction (aComponent : TComponent):TBasicAction;
begin
  if aComponent is TMenuItem then
    Result := TMenuItem(aComponent).Action
  else if aComponent is TAction then
    Result := TBasicAction(aComponent)
  else if aComponent is TControl then
    Result := TControl(aComponent).Action
  else
    Result := nil;
end;

class function TAppActions.ComponentVisible (aComponent : TComponent):Boolean;
begin
  if aComponent is TMenuItem then
    Result := TMenuItem(aComponent).Visible
  else if aComponent is TAction then
    Result := TAction(aComponent).Visible
  else if aComponent is TControl then
    Result := TControl(aComponent).Visible
  else
    Result := True;
end;

class function TAppActions.ComponentEnabled (aComponent : TComponent):Boolean;
begin
  if aComponent is TMenuItem then
    Result := TMenuItem(aComponent).Enabled
  else if aComponent is TAction then
    Result := TAction(aComponent).Enabled
  else if aComponent is TControl then
    Result := TControl(aComponent).Enabled
  else
    Result := True;
end;

class function TAppActions.ComponentParent (aComponent : TComponent):TComponent;
begin
  if aComponent is TMenuItem then
    Result := TMenuItem(aComponent).Parent
  else if aComponent is TControl then
    Result := TControl(aComponent).Parent
  else
    Result := nil;
end;

type
  TControlHack = class (TControl);

class function TAppActions.ComponentExecutable (aComponent : TComponent) : Boolean;
begin
  if aComponent is TMenuItem then
    Result := Assigned (TMenuItem(aComponent).OnClick)
  else if aComponent is TAction then
    Result := Assigned (TAction(aComponent).OnExecute)
  else if aComponent is TControl then
    Result := Assigned (TControlHack(aComponent).OnClick)
  else
    Result := False;
end;

class function TAppActions.ComponentAccessible(aComponent: TComponent): Boolean;
var
  Cmp:TComponent;
begin
  Result:= False;

  if aComponent = nil then
    Exit;
  {* Kontrollerer aComponent (enabled og visible)*}
  Cmp:= aComponent;
  if ComponentAction(Cmp) <> nil then
    ComponentAction(Cmp).Update;
  if (not ComponentEnabled(cmp)) or (not ComponentVisible(Cmp)) then
    Exit;

  {* Kontrollerer parents (visible) *}
  Cmp:= ComponentParent (Cmp);
  while Cmp <> nil do
  begin
    if ComponentAction(Cmp) <> nil then
      ComponentAction(Cmp).Update;
    if not ComponentVisible(Cmp) then
      Exit;
    Cmp:= ComponentParent (Cmp);
  end;

  Result:= True;
end;

class function TAppActions.ExecuteComponent(aComponent : TComponent): Boolean;
begin
  result := Assigned(aComponent);
  if result then
    if (aComponent is TMenuItem) and Assigned(TMenuItem(aComponent).OnClick) then
      TMenuItem(aComponent).OnClick(aComponent)
    else if aComponent is TBasicAction then
      TBasicAction(aComponent).Execute
    else if aComponent is TControl then
      TControlHack (aComponent).Click
    else
      result := false;
end;

class function TAppActions.SameCharCategory(const a,b: Char): Boolean;
var
  f1, f2: integer;
begin
  f1 :=
    IfThen(a.IsWhiteSpace,   1, 0) +
    IfThen(a.IsSeparator,    2, 0) +
    IfThen(a.IsDigit,        4, 0) +
    IfThen(a.IsLetter,       8, 0) +
    IfThen(a.IsPunctuation, 16, 0) +
    IfThen(a.IsSymbol,      32, 0);
  f2 :=
    IfThen(b.IsWhiteSpace,   1, 0) +
    IfThen(b.IsSeparator,    2, 0) +
    IfThen(b.IsDigit,        4, 0) +
    IfThen(b.IsLetter,       8, 0) +
    IfThen(b.IsPunctuation, 16, 0) +
    IfThen(b.IsSymbol,      32, 0);
  result := f1=f2;
end;

class function TAppActions.GetIntInPos(const s: string; n: integer): string;
var
  i: Integer;
begin
  for i := n to High(s) do
    if not s[i].IsDigit then
    begin
      result := s.Substring(n, i-n);
      exit;
    end;
  result := s.Substring(n);
end;

class function TAppActions.IsInteger(const s: string): Boolean;
var
  n: integer;
begin
  result := TryStrToInt(Trim(s), n);
end;

class function TAppActions.FilterAccept(
      Caption, SoekeTekst : String;
      InnholdSoek         : Boolean;
  var Tekst               : string;
  var Indeks              : integer;
      AddedItems          : TDictionary<string, boolean>
): Boolean;
var
  i: Integer;
  RF,Num: Boolean;
  b: Boolean;
begin
  //Tekstlengde:= Length(Soeketekst);
  Tekst:= StripHotKey(Caption);

  // Sørger for at vi ikke får like elementer
  if AddedItems.TryGetValue(AnsiLowerCase(Tekst), b) then
  //if ListBox.Items.IndexOf(Tekst) <> -1 then
  begin
    Result := False;
    Exit;
  end;
  Indeks:= Pos(AnsiLowerCase(Soeketekst), AnsiLowerCase(Tekst));

  // RF:= (Indeks in [3,4]) and ErHeltall(AnsiLowercase(Soeketekst), True);
  Num := IsInteger(SoekeTekst);
  RF := false;
  if Indeks>0 then
    if Num then
      RF := Pos(AnsiLowerCase('rf-'+Soeketekst), AnsiLowerCase(Tekst))>0
    else
    if (Pos('rf-',AnsiLowercase(Soeketekst))>0) then
      RF := true;

  Result :=
    InnholdSoek and (Indeks <> 0) //and ((Tekstlengde > 0) or Num)
    or ((not Innholdsoek) and (Indeks = 1))
    or ((not Innholdsoek) and RF)
    or (Indeks>1) and // example: searcrh "kost" should find "10 Kostnader"
      ((Tekst[Indeks-1]<'0') or (Tekst[Indeks-1]>'9')) and
      TryStrToInt(Trim(Copy(Tekst,1,Indeks-1)), i)
    or (Soeketekst = '');
end;

class function TAppActions.GetActionObjComparer(const ASoeketekst: string):TDelegatedComparer<TAppActions.TVerb>;
begin
  // priority of items ordering:
  // - items where Query and Result have same category (digit or nondigit)
  // - both items starts from number - compare as numbers
  // otherwise compare case insensitively as strings
  result := TDelegatedComparer<TAppActions.TVerb>.Create(
    function(const A,B: TAppActions.TVerb):Integer
    var
      c1,c2: Boolean;
      n1,n2: integer;
    begin

      // category
      c1 := (ASoeketekst<>'') and (A.Text<>'') and SameCharCategory(A.Text[Low(A.Text)], ASoeketekst[Low(ASoeketekst)]);
      c2 := (ASoeketekst<>'') and (B.Text<>'') and SameCharCategory(B.Text[Low(B.Text)], ASoeketekst[Low(ASoeketekst)]);
      if (c1 or c2) and (c1<>c2) then
      begin
        result := IfThen(c1, -1, 1);
        exit;
      end;

      // number
      if (A.Text<>'') and (B.Text<>'') and
        A.Text[Low(A.Text)].IsDigit and B.Text[Low(B.Text)].IsDigit and
        TryStrToInt(GetIntInPos(A.Text,Low(A.Text)), n1) and
        TryStrToInt(GetIntInPos(B.Text,Low(B.Text)), n2)
      then
        result := n1-n2
      else
        result := AnsiCompareText(A.Text, B.Text);
    end
  );
end;

class function TAppActions.FindFormActions(
  AForm                   : TForm;
  AExtraComponents        : TStrings;
  ASoeketekst             : string;
  AAutoAppendMenuItemsTag : integer;
  AAutoAppendActionsTag   : integer;
  ATest                   : TFunc<TComponent,Boolean>
): TList<TAppActions.TVerb>;
var
  InnholdSoek: Boolean;
  i,j: Integer;
  Cmp: TComponent;
  Tekst: String;
  Indeks: Integer;
  AddedItems: TAutoFree<TDictionary<string, boolean>>;
  s: string;
begin
  Assert(AForm <> nil, 'HostForm ikke satt under init.');
  AddedItems.Value := TDictionary<string, boolean>.Create;

  InnholdSoek := Trim(ASoeketekst).StartsWith('*');
  while Trim(ASoeketekst).StartsWith('*') do
    ASoeketekst := ASoeketekst.Replace('*', '');

  result := TList<TAppActions.TVerb>.Create(GetActionObjComparer(ASoeketekst));

  // manually added components
  if AExtraComponents<>nil then
    for i := 0 to AExtraComponents.Count - 1 do
      if AExtraComponents.Objects[i]<>nil then
      begin
        Cmp := TComponent(AExtraComponents.Objects[i]);
        if TAppActions.ComponentExecutable(Cmp) and
          (TAppActions.ComponentAccessible(Cmp) or Assigned(ATest) and ATest(Cmp)) and
          FilterAccept(AExtraComponents[i], ASoeketekst, InnholdSoek, Tekst, Indeks, AddedItems.Value)
        then
        begin
          result.Add(TAppActions.TVerb.Create(Tekst, AExtraComponents.Objects[i] as TComponent));
          AddedItems.Value.Add(AnsiLowerCase(Tekst), False);
        end;
      end;

  // components from HostForm
  if AForm<>nil then
    for i:= 0 to AForm.ComponentCount-1 do
    begin
      Cmp := AForm.components[i];

      if (AAutoAppendMenuItemsTag<>0) and (Cmp is TMenuItem) then
        if (Cmp.tag = AAutoAppendMenuItemsTag) and
          TAppActions.ComponentExecutable(Cmp) and
          (TAppActions.ComponentAccessible(Cmp) or Assigned(ATest) and ATest(Cmp)) and
          FilterAccept(TMenuItem(Cmp).Caption, ASoeketekst, InnholdSoek, Tekst, Indeks, AddedItems.Value)
        then
        begin
          result.Add(TAppActions.TVerb.Create(Tekst, Cmp));
          AddedItems.Value.Add(AnsiLowerCase(Tekst), False);
        end;

      if (AAutoAppendActionsTag<>0) and (Cmp is TAction) then
      begin
        s := TAction(Cmp).Caption;
        if TryStrToInt(s, j) then
          s := Trim(s + ' ' + TAction(Cmp).Hint);
        if (Cmp.tag = AAutoAppendActionsTag) and
          TAppActions.ComponentExecutable(Cmp) and
          (TAppActions.ComponentAccessible(Cmp) or Assigned(ATest) and ATest(Cmp)) and
          FilterAccept(s, ASoeketekst, InnholdSoek, Tekst, Indeks, AddedItems.Value)
        then
        begin
          result.Add(TAppActions.TVerb.Create(Tekst, AForm.components[i]));
          AddedItems.Value.Add(AnsiLowerCase(Tekst), False);
        end;
      end;

    end;

  Result.Sort;
end;

{ TCanvasUtils }

class procedure TCanvasUtils.BlendRectangle(Dst: TCanvas; const R: TRect; C: TColor; MixPercent: Byte);
var
  Bmp: TBitmap;
  Blend: TBlendFunction;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.Width              := 1;
    Bmp.Height             := 1;
    Bmp.Canvas.Pixels[0,0] := C;

    Blend.BlendOp             := AC_SRC_OVER;
    Blend.BlendFlags          := 0;
    Blend.SourceConstantAlpha := (50 + 255*MixPercent) Div 100;
    Blend.AlphaFormat         := 0;

    AlphaBlend(Dst.Handle, R.Left, R.Top, R.Right-R.Left, R.Bottom-R.Top, Bmp.Canvas.Handle, 0, 0, 1, 1, Blend);
  finally
    Bmp.Free;
  end;
end;

class function TCanvasUtils.MaxFitLength(const Src: string; Dst: TCanvas; WidthPixels: integer): integer;
var
  l,r,w: integer;
begin
  result := 0;
  if Src='' then
    Exit;
  l := 1;
  r := Length(Src);
  while r-l>=2 do
  begin
    result := (r+l) shr 1;
    w := Dst.TextWidth(Src.Substring(0,result));
    if w=WidthPixels then
      Break;
    if w<WidthPixels then
      l := result
    else
      r := result;
  end;
  if not (r-l>=2) then
    if Dst.TextWidth(Src.Substring(0,r)) <= WidthPixels then
      result := r
    else
      result := l;
  if Dst.TextWidth(Src.Substring(0,result)) > WidthPixels then
    dec(result);
  if result<0 then
    result := 0;
end;

{ TControlUtils }

class procedure TControlUtils.SetEnabled(C: TControl; Value, Recursive: Boolean);
var
  I: Integer;
begin
  C.Enabled := Value;
  if Recursive and (C is TWinControl) then
    for I := 0 to TWinControl(C).ControlCount-1 do
      SetEnabled(TWinControl(C).Controls[I], Value, Recursive);
end;

class function TControlUtils.ForEach(AStart: TControl; ACallback: TControlBrkProc): Boolean;
var
  i: Integer;
begin
  if AStart<>nil then
  begin

    { check if canceled by callback function }
    result := False;
    ACallback(AStart, result);
    if result then
      Exit(False);

    { check if any subsearch canceled }
    if AStart is TWinControl then
      for i := TWinControl(AStart).ControlCount-1 downto 0 do
        if not ForEach(TWinControl(AStart).Controls[i], ACallback) then
          Exit(False);
  end;
  result := True;
end;

class function TControlUtils.FindForm(C: TControl): TForm;
begin
  Result := TForm(FindParentOfClass(C, TForm));
end;

class function TControlUtils.FindParentOfClass(AControl: TControl; const AParentClass: TClass): TControl;
begin
  result := AControl;
  while (result<>nil) and not (result is AParentClass) do
    result := result.Parent;
end;

class procedure TControlUtils.ForEach(AStart: TControl; ACallback: TControlProc);
var
  i: Integer;
begin
  if AStart=nil then
    Exit;
  ACallback(AStart);
  if AStart is TWinControl then
    for i := TWinControl(AStart).ControlCount-1 downto 0 do
      ForEach(TWinControl(AStart).Controls[i], ACallback);
end;

class function TControlUtils.GetAll(AStart: TControl): TArray<TControl>;
var
  Dst: TArray<TControl>;
  Count: integer;
begin
  Count := 0;
  SetLength(Dst, 1000);
  ForEach(AStart,
    procedure(C: TControl)
    begin
      if Count>=Length(Dst) then
        SetLength(Dst, Length(Dst)*2);
      Dst[Count] := C;
      inc(Count);
    end);
  SetLength(Dst, Count);
  Result := Dst;
end;

class function TControlUtils.GetShortCaption(
  const Caption        : string;
        FixedHeadChars : integer;
        FixedTailChars : integer;
        Dst            : TCanvas;
        WidthPixels    : integer;
        Separator      : string = '...'): string;
var
  L,FixedLen: Integer;
begin

  { full caption is ok }
  Result := Caption;
  if Dst.TextWidth(Result) <= WidthPixels then
    Exit;

  { only fixed part may fit }
  Result :=
    Caption.Substring(0,FixedHeadChars) +
    Separator +
    Caption.Substring(Length(Caption)-FixedTailChars,FixedTailChars);
  FixedLen := Dst.TextWidth(Result);
  if Dst.TextWidth(Result) >= WidthPixels then
    Exit;

  { we have to cut some part }
  Result := Caption.Substring(FixedHeadChars, Length(Caption) - FixedHeadChars - FixedTailChars);
  L := TCanvasUtils.MaxFitLength(Result, Dst, WidthPixels-FixedLen);
  result :=
    Caption.Substring(0, FixedHeadChars+L) +
    Separator +
    Caption.Substring(Length(Caption)-FixedTailChars,FixedTailChars);
end;

type
  TCopyStreamsForm = class(TForm)
  protected
    FSrc, FDst: TStream;
    FCount, FBufSize: integer;
    FProgressProc: TCopyStreamProgressProc;
    FLocked: boolean;
    FTransferred: int64;

    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CreateParams(var Params:TCreateParams); override;
  public
    constructor Create(Src, Dst: TStream; Count, BufSize: integer; ProgressProc: TCopyStreamProgressProc); reintroduce;

    property Transferred: int64 read FTransferred;
  end;

{ TCopyStreamsForm }

constructor TCopyStreamsForm.Create(Src, Dst: TStream; Count, BufSize: integer; ProgressProc: TCopyStreamProgressProc);
begin
  inherited CreateNew(nil);

  { form properties (we will try to make the form invisible/transparent) }
  BorderStyle  := bsNone;
  Left         := 0;
  Top          := 0;
  Width        := 0;
  Height       := 0;
  OnShow       := FormShow;
  OnCloseQuery := FormCloseQuery;

  FSrc          := Src;
  FDst          := Dst;
  FCount        := Count;
  FBufSize      := BufSize;
  FProgressProc := ProgressProc;
  FLocked       := True;

end;

procedure TCopyStreamsForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := WS_EX_TRANSPARENT or WS_EX_TOPMOST;
end;

procedure TCopyStreamsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FLocked;
end;

procedure TCopyStreamsForm.FormShow(Sender: TObject);
begin
  FLocked := True;
  try
    FTransferred := TStreamUtils.Copy(FSrc, FDst, FCount, FBufSize,
      procedure(const Transferred: int64; var Cancel: boolean)
      begin

        { user defined callback }
        if Assigned(FProgressProc) then
          FProgressProc(Transferred, Cancel);

        { to keep ui alive ()}
        Application.ProcessMessages;
      end);
  finally
    FLocked := False;
    ModalResult := mrOk;

    { .Close mwthod will not close the form from FormShow, so we send wm_close message instead }
    PostMessage(Handle, wm_close,0,0);

  end;
end;

{ TVCLStreamUtils }

class function TVCLStreamUtils.Copy(Src, Dst: TStream; Count, BufSize: integer; ProgressProc: TCopyStreamProgressProc): int64;
var
  LockUIForm: TCopyStreamsForm;
begin
  LockUIForm := TCopyStreamsForm.Create(Src, Dst, Count, BufSize, ProgressProc);
  try
    LockUIForm.ShowModal;
    result := LockUIForm.Transferred;
  finally
    LockUIForm.Free;
  end;
end;

class function TVCLStreamUtils.Copy(Src, Dst: TStream; ProgressProc: TCopyStreamProgressProc): int64;
begin
  result := Copy(Src, Dst, 0,0,ProgressProc);
end;

class function TVCLStreamUtils.Copy(Src, Dst: TStream): int64;
begin
  result := Copy(Src, Dst, 0,0,nil);
end;

{ TVCLFileUtils }

class function TVCLFileUtils.CopyFile(
  const SrcFileName, DstFileName : string;
    out ErrorMessage             : string;
        ProgressProc             : TCopyFileProgressProc): boolean;
var
  AutoFreeCollection: TAutoFreeCollection;
  CI: TCopyFileInfo;
  Src,Dst: TStream;
begin
  try
    Result := True;
    Src := AutoFreeCollection.Add(TFileStream.Create(SrcFileName, fmOpenRead or fmShareDenyWrite));
    Dst := AutoFreeCollection.Add(TFileStream.Create(DstFileName, fmCreate));
    CI.FileSize    := Src.Size;
    CI.SrcFileName := SrcFileName;
    CI.DstFileName := DstFileName;
    if not Assigned(ProgressProc) then
      TVCLStreamUtils.Copy(Src, Dst)
    else
    begin
      TVCLStreamUtils.Copy(Src, Dst,
        procedure(const Transferred: int64; var Cancel: boolean)
        begin
          CI.Copied := Transferred;
          ProgressProc(CI, Cancel);
        end);
    end;
  except
    on e: exception do
    begin
      Result := True;
      ErrorMessage := Format('Can''t copy file "%s" to "%s": %s', [SrcFileName, DstFileName, SysErrorMessage(GetLastError)]);
    end;
  end;
end;

class function TVCLFileUtils.CopyFile(
  const SrcFileName, DstFileName : string;
    out ErrorMessage             : string): boolean;
begin
  result := CopyFile(SrcFileName, DstFileName, ErrorMessage, nil);
end;

{ TActiveControlListeners }

class constructor TActiveControlListeners.Create;
var
  NotifyEvent: TNotifyEvent;
begin
  FList := TList<TNotifyEvent>.Create;
  TMethod(NotifyEvent).Code := @ActiveControlChange;
  TMethod(NotifyEvent).Data := nil;
  FOldOnActiveControlChange := Screen.OnActiveControlChange;
  Screen.OnActiveControlChange := NotifyEvent;
end;

class destructor TActiveControlListeners.Destroy;
begin
  FList.Free;
  FList := nil;
  Screen.OnActiveControlChange := FOldOnActiveControlChange;
  FOldOnActiveControlChange := nil;
end;

class procedure TActiveControlListeners.ActiveControlChange(Data, Sender: TObject);
var
  I: Integer;
begin
  if Assigned(FOldOnActiveControlChange) then
    FOldOnActiveControlChange(Sender);
  if List <> nil then
    for I := 0 to List.Count-1 do
      List[I](Sender);
end;

class function TActiveControlListeners.Add(AHandler: TNotifyEvent): boolean;
begin
  result := (List <> nil) and not Exists(AHandler);
  if result then
    List.Add(AHandler);
end;

class function TActiveControlListeners.Exists(AHandler: TNotifyEvent): boolean;
begin
  result := (List <> nil) and List.Contains(AHandler);
end;

class function TActiveControlListeners.Remove(AHandler: TNotifyEvent): boolean;
begin
  result := (List <> nil) and Exists(AHandler);
  if result then
    List.Remove(AHandler);
end;

end.
