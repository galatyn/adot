unit adot.SpreadSheet.ExcelOle;

interface

{$WARN SYMBOL_PLATFORM OFF}

uses
  adot.SpreadSheet.Types,
  adot.SpreadSheet.Classes,
  adot.Collections,
  adot.Tools,
  adot.Strings,
  adot.Graphics,
  adot.Windows.Tools,
  MsCxTools,
  Excel2000,
  Vcl.Graphics,
  Winapi.ActiveX,
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Types,
  System.IOUtils,
  System.Variants,
  System.Math;

type
  { Class defined as abstract only to disable direct instantiating. Use fabric of classes from FellesKlasser.SpreadSheet.Export 
    unit, it allows to switch underlying engine from DevExpress to anything else with zero changes in code of apps. }
  TXLSExportExcelOle = class abstract(TXLSBook)
  protected
    const
      BorderStyleTranslation: array[TXLSBorderStyle] of TOleEnum = (
        xlContinuous, xlContinuous{xebsHair}, xlDot, xlDashDotDot, xlDashDot, xlDash,
        xlContinuous, xlDashDotDot, xlSlantDashDot, xlDashDot, xlDash, xlContinuous,
        xlContinuous, xlDouble, xlLineStyleNone);

      BorderWeightTranslation: array[TXLSBorderStyle] of TOleEnum = (
        xlThin, xlHairline, xlThin, xlThin, xlThin, xlThin, xlThin,
        xlMedium, xlThin, xlMedium, xlMedium, xlMedium,
        xlThick, xlThick, xlThin);

      AlignHorzTranslation: array[TXLSAlignHorz] of TOleEnum = (
        xlGeneral, xlLeft, xlCenter, xlRight, xlFill, xlJustify, xlDistributed );

      AlignVertTranslation: array[TXLSAlignVert] of TOleEnum = (
        xlTop, xlCenter, xlBottom, xlJustify, xlDistributed );

    var
      FNumberFormat: string;
      FMoneyFormat: String;
      FDateFormat: string;
      FDateTimeFormat: string;

    type
      TCompareCells = function(A,B: TXlsCell):boolean of object;

      TProcessRegion = procedure(Src: TXLSSheet; Dst: _Worksheet; var Cells: TVector2D<TXLSCell>; const Rect,DataRect: TRect) of object;

    class function GetExcelAddress(Col,Row: integer): string; static;
    function GetValueFromCell(Cell: TXLSCell): Variant;
    procedure ApplyCellStyleToRange(Src: TXLSCell; Range: ExcelRange);
    procedure ProcessEqualSheetRegions(Src: TXLSSheet; Dst: _Worksheet; const DataRect: TRect; BeforeApplyBestFit: boolean;
      CompareCells: TCompareCells; ProcessRegion: TProcessRegion);

    procedure ExportToWorkBook(WorkBook: _Workbook);
    procedure ExportSheet(Src: TXLSSheet; Dst: _Worksheet);

    function ExportSheetValues_RgnCompare(A, B: TXlsCell): boolean;
    procedure ExportSheetValues_RgnProcess(
            Src            : TXLSSheet;
            Dst            : _Worksheet;
        var Cells          : TVector2D<TXLSCell>;
      const RectInDataRect : TRect;
      const DataRect       : TRect);
    procedure ExportSheetValues(Src: TXLSSheet; Dst: _Worksheet; const DataRect: TRect; BeforeApplyBestFit: boolean);

    function ExportSheetStyles_RgnCompare(A, B: TXlsCell): boolean;
    procedure ExportSheetStyles_RgnProcess(
            Src            : TXLSSheet;
            Dst            : _Worksheet;
        var Cells          : TVector2D<TXLSCell>;
      const RectInDataRect : TRect;
      const DataRect       : TRect);
    procedure ExportSheetStyles(Src: TXLSSheet; Dst: _Worksheet; const DataRect: TRect; BeforeApplyBestFit: boolean);

    procedure ExportToNewWorkbook(var App: TExcelApplication; var WorkBook: _Workbook; ShowBook: boolean);

    procedure DoSaveToStream(Dst: TStream; const FileType: string); override;
    function DoSaveToFile(var FileName: string; ShowSaveDialog: Boolean): TSaveStatus; override;

    { should not be used for import, load is not supported yet}
    procedure DoLoadFromStream(Src: TStream); override;
    procedure DoLoadFromFile(const FileName: string); override;
    procedure DoPrint(const Options: TXLSPrintOptions); override;

    class function DoIsPasswordProtected(const SpreadsheetFileName: string): boolean; override;
    class function DoIsValidPassword(const SpreadsheetFileName,Password: string): boolean; override;

    function GetExcelNumberFormat(App: TExcelApplication): String;
    function GetExcelMoneyFormat(App: TExcelApplication): String;
    function GetExcelDateFormat(App: TExcelApplication): String;
    function GetExcelDateTimeFormat(App: TExcelApplication): String;
  public
    class function IsAvailable: boolean; static;
  end;

implementation

{ TXLSExportExcelOle }

class function TXLSExportExcelOle.GetExcelAddress(Col,Row: integer): string;
var
  m: integer;
begin
  Result := '';
  inc(Col);
  while Col > 0 do
  begin
    m := (Col - 1) mod 26;
    Result := Chr(Ord('A') + m) + Result;
    Col := (Col - m) div 26;
  end;
  result := result + IntToStr(Row+1);
end;

procedure TXLSExportExcelOle.ExportToNewWorkbook(var App: TExcelApplication; var WorkBook: _Workbook; ShowBook: boolean);
begin
  App := TExcelApplication.Create(nil);
  App.Connect;
  WorkBook := App.Workbooks.Add(xlWBatWorkSheet, LOCALE_USER_DEFAULT);
  try
    FNumberFormat := GetExcelNumberFormat(App);
    FMoneyFormat  := GetExcelMoneyFormat(App);
    FDateFormat   := GetExcelDateFormat(App);
    FDateTimeFormat := GetExcelDateTimeFormat(App);
    ExportToWorkBook(WorkBook);
  finally
    if ShowBook then
    begin
      App.Visible[LOCALE_USER_DEFAULT] := True; { show Excel }
      if Workbook.Sheets.Count > 0 then
        try
          (Workbook.Sheets.Item[Workbook.Sheets.Count] as _Worksheet).Activate(LOCALE_USER_DEFAULT);

          { If there is more than 1 open workbook in Excel, then .Activate may not work. Workarounds:
            - Minimize and then restore Excel application (in some tests works "time to time").
            - Find window by title and activate it by direct WinApi call (seems to be most reliable workaround). }
          TWinUtils.AppActivate(App.Caption);

        except
        end;
    end;
  end;
end;

function TXLSExportExcelOle.DoSaveToFile(var FileName: string; ShowSaveDialog: Boolean): TSaveStatus;
var
  App: TExcelApplication;
  WorkBook: _Workbook;
begin
  App := nil;
  WorkBook := nil;
  try
    ExportToNewWorkbook(App, WorkBook, True);
    {WorkBook.SaveAs(FileName, xlDefaultAutoFormat, null, null, null, null, null, null, null, cp_xxx, null, LOCALE_USER_DEFAULT);}
    FileName := '';
    result := ssOpened;
  finally
    WorkBook := nil;
    FreeAndNil(App);
  end;
end;

procedure TXLSExportExcelOle.DoSaveToStream(Dst: TStream; const FileType: string);
var
  TempFileName: string;
  Stream: TFileStream;
begin
  TempFileName := IncludeTrailingPathDelimiter(TPath.GetTempPath) + TGUIDUtils.GetNewAsString + FileType;
  try
    DoSaveToFile(TempFileName, False);
    Stream := TFileStream.Create(TempFileName, fmOpenRead);
    try
      Dst.CopyFrom(Stream, Stream.Size);
    finally
      FreeAndNil(Stream);
    end;
  finally
    System.SysUtils.DeleteFile(TempFileName);
  end;
end;

procedure TXLSExportExcelOle.ExportToWorkBook(WorkBook: _Workbook);
var
  I: Integer;
  Sheet: _Worksheet;
begin
  Assert(SheetCount > 0);
  for I := 0 to SheetCount-1 do
  begin
    if I >= WorkBook.Worksheets.Count
      then Sheet := WorkBook.Worksheets.Add(null, null, I, xlWBatWorkSheet, LOCALE_USER_DEFAULT) as _Worksheet
      else Sheet := WorkBook.Worksheets.Item[I+1] as _Worksheet;
    Sheet.Name := Sheets[I].SheetName;
    if Password<>'' then
      Sheet.Protect(Password, True, True, True, True, LOCALE_USER_DEFAULT);
    ExportSheet(Sheets[I], Sheet);
  end;
  if (ActiveSheetIndex >= 0) and (ActiveSheetIndex < WorkBook.Worksheets.Count) then
  begin
    Sheet := WorkBook.Worksheets.Item[ActiveSheetIndex+1] as _Worksheet;
    Sheet.Activate(LOCALE_USER_DEFAULT);
  end;
end;

procedure TXLSExportExcelOle.ExportSheet(Src: TXLSSheet; Dst: _Worksheet);
var
  DataRect: TRect;
  MergeRegion: TRect;
  Range: ExcelRange;
  FrozenColumnCnt,FrozenRowCnt: Integer;
  CW: TPair<integer, TXLSCellWidth>;
  CH: TPair<integer, integer>;
  R: TRange;
begin
  DataRect := Src.DataRect;

  { Cells where ConsiderWhenApplyBestFit=True should be moved before ApplyBestFit }
  if (DataRect.Width >= 0) and (DataRect.Height >= 0) then
  begin
    ExportSheetValues(Src, Dst, DataRect, True);
    ExportSheetStyles(Src, Dst, DataRect, True);
  end;

  { merge regions }
  for MergeRegion in Src.MergeRegionsCollection do
  begin
    MergeRegion.Offset(Src.HorOffset, Src.VertOffset);
    Range := Dst.Range[GetExcelAddress(MergeRegion.Left, MergeRegion.Top), GetExcelAddress(MergeRegion.Right, MergeRegion.Bottom)];
    Range.MergeCells := True;
  end;

  { ColWidth / RowHeight }
  { Src.ColWidthCollection returns original values, we should add HorOffset/VertOffset.
    It is not mentioned in MSDN, but ColumnWidth/RowHeight may fail if assigned value is too big. }
  for CW in Src.ColWidthCollection do
  try
    Dst.Range[GetExcelAddress(CW.Key + Src.HorOffset, 0), GetExcelAddress(CW.Key + Src.HorOffset, 0)].ColumnWidth := CW.Value.CharSize;
  except
  end;
  for CH in Src.RowHeightCollection do
  try
    Dst.Range[GetExcelAddress(0, CW.Key + Src.VertOffset), GetExcelAddress(0, CW.Key + Src.VertOffset)].RowHeight := CH.Value;
  except
  end;

  { ApplyBestFit }
  for R in TArrayUtils.Ranges(Src.ApplyBestFitCollection.ToArray) do
    Dst.Range[GetExcelAddress(R.Start, 0), GetExcelAddress(R.Finish, 0)].EntireColumn.AutoFit;

  { Cells where ConsiderWhenApplyBestFit=False should be moved after ApplyBestFit }
  if (DataRect.Width >= 0) and (DataRect.Height >= 0) then
  begin
    ExportSheetValues(Src, Dst, DataRect, False);
    ExportSheetStyles(Src, Dst, DataRect, False);
  end;

  { OptionsView }
  FrozenColumnCnt := Max(Src.OptionsView.FrozenColumns + Src.HorOffset, 0);
  FrozenRowCnt    := Max(Src.OptionsView.FrozenRows + Src.HorOffset, 0);
  if (FrozenRowCnt > 0) or (FrozenColumnCnt > 0) then
  begin
    Dst.Application.ActiveWindow.SplitColumn := FrozenColumnCnt;
    Dst.Application.ActiveWindow.SplitRow := FrozenRowCnt;
    Dst.Application.ActiveWindow.FreezePanes := True;
  end;

  { For one sheet it fails with OLE exception when we try to hide.
    For now we do not implement Visible for tabs.
  if Src.OptionsView.Visible then
    Dst.Visible[0] := xlSheetVisible
  else
    Dst.Visible[0] := xlSheetVeryHidden; }

  { OptionsPrint }
  if (Src.OptionsPrint.FitToPagesWide > 0) or (Src.OptionsPrint.FitToPagesTall > 0) then
    Dst.PageSetup.Zoom := False;
  if Src.OptionsPrint.FitToPagesWide > 0 then
    Dst.PageSetup.FitToPagesWide := Src.OptionsPrint.FitToPagesWide;
  if Src.OptionsPrint.FitToPagesTall > 0 then
    Dst.PageSetup.FitToPagesTall := Src.OptionsPrint.FitToPagesTall;
  case Src.OptionsPrint.Orientation of
    poDefault   : ;
    poPortrait  : Dst.PageSetup.Orientation := xlPortrait;
    poLandscape : Dst.PageSetup.Orientation := xlLandscape;
  end;
end;

procedure TXLSExportExcelOle.ApplyCellStyleToRange(Src: TXLSCell; Range: ExcelRange);
begin

  { SrcCell.Style.Font }
  with Src.Style.Font do
  begin
    if not Name.Empty then
      Range.Font.Name := Name;
    if not Color.Empty then
      Range.Font.Color := TColorUtils.ColorToRGB(Color);
    if not Style.Empty then
    begin
      Range.Font.Bold := fsBold in Style.Value;
      Range.Font.Italic := fsItalic in Style.Value;
      Range.Font.Underline := fsUnderline in Style.Value;
      Range.Font.Strikethrough := fsStrikeOut in Style.Value;
    end;
    {if not Height.Empty then
      Range.Font.Size := Height;}
    if not Size.Empty then
      Range.Font.Size := Size;
  end;

  { Src.Style.Borders }
  with Src.Style do
  begin
    { BorderStyle }
    if not Borders[xbLeft].Style.Empty then
    begin
      Range.Borders.Item[xlEdgeLeft].LineStyle := BorderStyleTranslation [Borders[xbLeft].Style.Value];
      Range.Borders.Item[xlEdgeLeft].Weight    := BorderWeightTranslation[Borders[xbLeft].Style.Value];
    end;
    if not Borders[xbTop].Style.Empty then
    begin
      Range.Borders.Item[xlEdgeTop].LineStyle := BorderStyleTranslation [Borders[xbTop].Style.Value];
      Range.Borders.Item[xlEdgeTop].Weight    := BorderWeightTranslation[Borders[xbTop].Style.Value];
    end;
    if not Borders[xbRight].Style.Empty then
    begin
      Range.Borders.Item[xlEdgeRight].LineStyle := BorderStyleTranslation [Borders[xbRight].Style.Value];
      Range.Borders.Item[xlEdgeRight].Weight    := BorderWeightTranslation[Borders[xbRight].Style.Value];
    end;
    if not Borders[xbBottom].Style.Empty then
    begin
      Range.Borders.Item[xlEdgeBottom].LineStyle := BorderStyleTranslation [Borders[xbBottom].Style.Value];
      Range.Borders.Item[xlEdgeBottom].Weight    := BorderWeightTranslation[Borders[xbBottom].Style.Value];
    end;
    { BorderColor }
    if not Borders[xbLeft].Color.Empty then
      Range.Borders.Item[xlEdgeLeft].Color := TColorUtils.ColorToRGB(Borders[xbLeft].Color.Value);
    if not Borders[xbTop].Color.Empty then
      Range.Borders.Item[xlEdgeTop].Color := TColorUtils.ColorToRGB(Borders[xbTop].Color.Value);
    if not Borders[xbRight].Color.Empty then
      Range.Borders.Item[xlEdgeRight].Color := TColorUtils.ColorToRGB(Borders[xbRight].Color.Value);
    if not Borders[xbBottom].Color.Empty then
      Range.Borders.Item[xlEdgeBottom].Color := TColorUtils.ColorToRGB(Borders[xbBottom].Color.Value);
  end;

  { Src.Style.BackgroundColor }
  with Src.Style.BackgroundColor do
    if not Empty then
      Range.Interior.Color := TColorUtils.ColorToRGB(Value);

  { Src.Style.AlignHorz }
  with Src.Style.AlignHorz do
    if not Empty then
      Range.HorizontalAlignment := AlignHorzTranslation[Value];

  { Src.Style.AlignVert }
  with Src.Style.AlignVert do
    if not Empty then
      Range.VerticalAlignment := AlignVertTranslation[Value];

  {$IF [Low(Src.DisplayFormat)..High(Src.DisplayFormat)]<>[dfDefault, dfCurrency, dfFloatNumber, dfIntNumber, dfDateTime, dfDate, dfBoolean, dfString]}
    {$Message Error 'Support of new DisplayFormat must be implemented here'}
  {$ENDIF}
  case Src.DisplayFormat of
    dfDefault     : ;
    dfCurrency    : Range.NumberFormat := FMoneyFormat;
    dfFloatNumber : Range.NumberFormat := FNumberFormat;
    dfIntNumber   : Range.NumberFormat := '0';
    dfDateTime    : Range.NumberFormat := FDateTimeFormat;
    dfDate        : Range.NumberFormat := FDateFormat;
    dfBoolean     : Range.NumberFormat := '" ";"+"';
    dfString      :
      if Src.PreserveStringType then
        Range.NumberFormat := '@'; { keep string as string even if it looks like number/date etc }
    //xevtCellPercent: Result := '0%';
  end;

end;

function TXLSExportExcelOle.GetExcelNumberFormat(App: TExcelApplication): String;
begin
  Result := Format('#%s##0', [
    App.International[xlThousandsSeparator, LOCALE_USER_DEFAULT]
  ]);
end;

function TXLSExportExcelOle.GetExcelMoneyFormat(App: TExcelApplication): String;
begin
  Result := Format('#%s##0%s%s', [
    App.International[xlThousandsSeparator, LOCALE_USER_DEFAULT],
    App.International[xlDecimalSeparator, LOCALE_USER_DEFAULT],
    StringOfChar('0', Integer(App.International[xlCurrencyDigits, LOCALE_USER_DEFAULT]))
  ]);
end;

function TXLSExportExcelOle.GetExcelDateFormat(App: TExcelApplication): String;
var
  d, m, y: Integer;
begin
  if App.International[xlDayLeadingZero, LOCALE_USER_DEFAULT] then d := 2 else d := 1;
  if App.International[xlMonthLeadingZero, LOCALE_USER_DEFAULT] then m := 2 else m := 1;
  if App.International[xl4DigitYears, LOCALE_USER_DEFAULT] then y := 4 else y := 2;

  Result := Format('%1:s%0:s%2:s%0:s%3:s', [
    App.International[xlDateSeparator, LOCALE_USER_DEFAULT],
    StringOfChar(VarToStr(App.International[xlDayCode, LOCALE_USER_DEFAULT])[1], d),
    StringOfChar(VarToStr(App.International[xlMonthCode, LOCALE_USER_DEFAULT])[1], m),
    StringOfChar(VarToStr(App.International[xlYearCode, LOCALE_USER_DEFAULT])[1], y)
  ]);
end;

function TXLSExportExcelOle.GetExcelDateTimeFormat(App: TExcelApplication): String;
begin
  Result :=
    GetExcelDateFormat(App) + ' ' +
    Format('%s%s%s%s%s', [
    App.International[xlHourCode, LOCALE_USER_DEFAULT],
    App.International[xlHourCode, LOCALE_USER_DEFAULT],
    App.International[xlTimeSeparator, LOCALE_USER_DEFAULT],
    App.International[xlMinuteCode, LOCALE_USER_DEFAULT],
    App.International[xlMinuteCode, LOCALE_USER_DEFAULT]
  ]);
end;

function TXLSExportExcelOle.GetValueFromCell(Cell: TXLSCell): Variant;
begin
  if Cell.ValueType = xevtString then
    Result := '''' + Cell.AsString
  else
    Result := Cell.AsVariant;
end;

function TXLSExportExcelOle.ExportSheetValues_RgnCompare(A,B: TXlsCell): boolean;
begin
  result := True;
end;

procedure TXLSExportExcelOle.ExportSheetValues_RgnProcess(
        Src            : TXLSSheet;
        Dst            : _Worksheet;
    var Cells          : TVector2D<TXLSCell>;
  const RectInDataRect : TRect;
  const DataRect       : TRect);
var
  VarArray: variant;
  X,Y: integer;
  DstRect: TRect;
  Range: ExcelRange;
begin

  { create and fill variant array }
  VarArray := VarArrayCreate([0, RectInDataRect.Height, 0, RectInDataRect.Width], varVariant);
  for Y := RectInDataRect.Top to RectInDataRect.Bottom do
    for X := RectInDataRect.Left to RectInDataRect.Right do
      VarArray[Y-RectInDataRect.Top, X-RectInDataRect.Left] := GetValueFromCell(Cells[X,Y]);

  { transfer data to Excel }
  DstRect := RectInDataRect;
  DstRect.Offset(DataRect.Left + Src.HorOffset, DataRect.Top + Src.VertOffset);
  Range := Dst.Range[GetExcelAddress(DstRect.Left, DstRect.Top), GetExcelAddress(DstRect.Right, DstRect.Bottom)];
  Range.Value := VarArray;
end;

procedure TXLSExportExcelOle.ExportSheetValues(Src: TXLSSheet; Dst: _Worksheet; const DataRect: TRect; BeforeApplyBestFit: boolean);
begin
  ProcessEqualSheetRegions(
    Src, Dst, DataRect, BeforeApplyBestFit,
    ExportSheetValues_RgnCompare,
    ExportSheetValues_RgnProcess);
end;

function TXLSExportExcelOle.ExportSheetStyles_RgnCompare(A,B: TXlsCell): boolean;
begin
  result := (A.Style = B.Style) and (A.ValueType = B.ValueType);
end;

procedure TXLSExportExcelOle.ExportSheetStyles_RgnProcess(
        Src            : TXLSSheet;
        Dst            : _Worksheet;
    var Cells          : TVector2D<TXLSCell>;
  const RectInDataRect : TRect;
  const DataRect       : TRect);
var
  X,Y: integer;
  Range: ExcelRange;
begin

  { apply settings for the area }
  X := RectInDataRect.Left + DataRect.Left + Src.HorOffset;
  Y := RectInDataRect.Top + DataRect.Top + Src.VertOffset;
  Range := Dst.Range[ GetExcelAddress(X,Y), GetExcelAddress(X+RectInDataRect.Width, Y+RectInDataRect.Height) ];
  ApplyCellStyleToRange(Cells[RectInDataRect.Left, RectInDataRect.Top], Range);
end;

procedure TXLSExportExcelOle.ExportSheetStyles(Src: TXLSSheet; Dst: _Worksheet; const DataRect: TRect; BeforeApplyBestFit: boolean);
begin
  ProcessEqualSheetRegions(
    Src, Dst, DataRect, BeforeApplyBestFit,
    ExportSheetStyles_RgnCompare,
    ExportSheetStyles_RgnProcess);
end;

procedure TXLSExportExcelOle.ProcessEqualSheetRegions(
        Src: TXLSSheet;
        Dst: _Worksheet;
  const DataRect: TRect;
        BeforeApplyBestFit: boolean;
        CompareCells: TCompareCells;
        ProcessRegion: TProcessRegion);
var
  Cells: TVector2D<TXLSCell>;
  SrcPair: TPair<TXLSPos, TXLSCell>;
  X,Y,I,J,W,H: Integer;
  BottomIsFound: boolean;
  SrcCell: TXLSCell;
begin

  { get all cells to 2D matrix }
  Cells := TVector2D<TXLSCell>.Create(DataRect.Width+1, DataRect.Height+1);
  for SrcPair in Src do
    if not SrcPair.Value.Empty and (SrcPair.Value.ConsiderWhenApplyBestFit = BeforeApplyBestFit) then
      Cells[SrcPair.Key.Col-DataRect.Left, SrcPair.Key.Row-DataRect.Top] := SrcPair.Value;

  { now we can process every rectangual area with equal settings as single operation on range }
  for Y := 0 to Cells.RowCount-1 do
    for X := 0 to Cells.Count[Y]-1 do
      if Cells[X,Y]<>nil then
      begin
        SrcCell := Cells[X,Y];

        { find width of the area with equal settings }
        W := 1;
        for I := X+1 to Cells.Count[Y]-1 do
          if (Cells[I,Y] <> nil) and CompareCells(Cells[I,Y], SrcCell) then
            inc(W)
          else
            Break;

        { find height of the area with equal settings }
        H := 1;
        BottomIsFound := False;
        for J := Y+1 to Cells.RowCount-1 do
        begin
          for I := X to X+W-1 do
            if (Cells[I,J] = nil) or not CompareCells(Cells[I,J], SrcCell) then
            begin
              BottomIsFound := True;
              Break;
            end;
          if BottomIsFound then
            Break;
          inc(H);
        end;

        { process region }
        ProcessRegion(Src, Dst, Cells, TRect.Create(X, Y, X+W-1, Y+H-1), DataRect);

        { remove the area from further processing }
        for J := Y to Y+H-1 do
          for I := X to X+W-1 do
            Cells[I,J] := nil;

      end; { if Cells[X,Y]<>nil / for X / for Y}

end;

class function TXLSExportExcelOle.IsAvailable: boolean;
begin
  try
    TExcelApplication.Create(nil).Free;
    result := True;
  except
    result := False;
  end;
end;

class function TXLSExportExcelOle.DoIsPasswordProtected(const SpreadsheetFileName: string): boolean;
begin
  raise Exception.Create('Not implemented');
end;

class function TXLSExportExcelOle.DoIsValidPassword(const SpreadsheetFileName, Password: string): boolean;
begin
  raise Exception.Create('Not implemented');
end;

procedure TXLSExportExcelOle.DoLoadFromFile(const FileName: string);
begin
  raise Exception.Create('Not implemented');
end;

procedure TXLSExportExcelOle.DoLoadFromStream(Src: TStream);
begin
  raise Exception.Create('Not implemented');
end;

procedure TXLSExportExcelOle.DoPrint(const Options: TXLSPrintOptions);
var
  App: TExcelApplication;
  WorkBook: _Workbook;
begin
  App := nil;
  WorkBook := nil;
  try
    ExportToNewWorkbook(App, WorkBook, False);
    WorkBook.PrintOut(EmptyParam, EmptyParam, EmptyParam, Options.SettingsUI, Options.PrinterName, False, True, EmptyParam, LOCALE_USER_DEFAULT);
  finally
    WorkBook := nil;
    FreeAndNil(App);
  end;
end;

end.
