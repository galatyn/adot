unit adot.SpreadSheet.ExcelDevExpress;

interface

uses
  adot.SpreadSheet.Types,
  adot.SpreadSheet.Classes,
  adot.Collections,
  adot.Tools,
  adot.Strings,
  adot.DevExpress,
  dxSpreadSheet,
  dxSpreadSheetCore,
  dxSpreadSheetGraphics,
  dxCore,
  cxGraphics,
  dxSpreadSheetStrs,
  dxSpreadSheetTypes,
  dxOLECryptoContainer,
  dxHashUtils,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Types,
  Vcl.Graphics;

type
  { Class defined as abstract only to disable direct instantiating. Use fabric of classes from SpreadSheet.Export 
    unit, it allows to switch underlying engine from DevExpress to anything else with zero changes in code of apps. }
  TXLSExportDevExpress = class abstract(TXLSBook)
  protected
    type

      TPswValidation = class
      private
        FPassword: string;

        function OnGetPsw(Sender: TObject; out Password: string): Boolean;
        function DoIsPasswordProtected(const SpreadsheetFileName: string): boolean;
        function DoIsValidPassword(const SpreadsheetFileName,Password: string): boolean;
      public
        class function IsPasswordProtected(const SpreadsheetFileName: string): boolean;
        class function IsValidPassword(const SpreadsheetFileName,Password: string): boolean;
      end;

    const

      { SpreadSheet -> TdxSpreadSheet convertion }

      BorderStyleTranslation: array[TXLSBorderStyle] of TdxSpreadSheetCellBorderStyle = (
        sscbsDefault, sscbsHair, sscbsDotted, sscbsDashDotDot, sscbsDashDot, sscbsDashed, sscbsThin,
        sscbsMediumDashDotDot, sscbsSlantedDashDot, sscbsMediumDashDot, sscbsMediumDashed, sscbsMedium,
        sscbsThick, sscbsDouble, sscbsNone );

      AlignHorzTranslation: array[TXLSAlignHorz] of TdxSpreadSheetDataAlignHorz = (
        ssahGeneral, ssahLeft, ssahCenter, ssahRight, ssahFill, ssahJustify, ssahDistributed );

      AlignVertTranslation: array[TXLSAlignVert] of TdxSpreadSheetDataAlignVert = (
        ssavTop, ssavCenter, ssavBottom, ssavJustify, ssavDistributed );

      { TdxSpreadSheet -> SpreadSheet convertion }

      BorderTypeTranslationBack: array[TdxSpreadSheetCellBorderStyle] of TXLSBorderStyle = (
        xebsDefault, xebsHair, xebsDotted, xebsDashDotDot, xebsDashDot, xebsDashed,
        xebsThin, xebsMediumDashDotDot, xebsSlantedDashDot, xebsMediumDashDot, xebsMediumDashed, xebsMedium,
        xebsThick, xebsDouble, xebsNone);

      AlignHorzTranslationBack: array[TdxSpreadSheetDataAlignHorz] of TXLSAlignHorz = (
        xeahGeneral, xeahLeft, xeahCenter, xeahRight, xeahFill, xeahJustify, xeahDistributed );

      AlignVertTranslationBack: array[TdxSpreadSheetDataAlignVert] of TXLSAlignVert = (
        xeavTop, xeavCenter, xeavBottom, xeavJustify, xeavDistributed );

    { export }
    procedure ExportToSpreadSheet(SpreadSheet: TdxSpreadSheet);
    procedure ExportSheet(Src: TXLSSheet; Dst: TdxSpreadSheetTableView);
    procedure ExportCell(Src: TXLSCell; Dst: TdxSpreadSheetCell);

    { import }
    procedure ImportFromSpreadSheet(SpreadSheet: TdxSpreadSheet);
    procedure ImportSheet(Src: TdxSpreadSheetTableView; Dst: TXLSSheet);
    procedure ImportCell(Src: TdxSpreadSheetCell; Dst: TXLSCell);

    procedure ExportSheetCells(Src: TXLSSheet; Dst: TdxSpreadSheetTableView; BeforeApplyBestFit: boolean);
    procedure SetExcelCellFormat(Src: TXLSCell; Dst: TdxSpreadSheetCell);

    procedure DoSaveToStream(Dst: TStream; const FileType: string); override;
    { Basic class has implementation of DoSaveToFile via DoSaveToStream. }
    {procedure DoSaveToFile(const FileName: string); override;}
    procedure DoLoadFromStream(Src: TStream); override;
    { we override LoadFromFile to avoid format detection by file content (it is bit faster to check just ext) }
    procedure DoLoadFromFile(const FileName: string); override;
    class function DoIsPasswordProtected(const SpreadsheetFileName: string): boolean; override;
    class function DoIsValidPassword(const SpreadsheetFileName,Password: string): boolean; override;

  public
  end;

implementation

{ TXLSExportDevExpress }

{procedure TXLSExportDevExpress.SaveToFile(const FileName: string);
var
  SpreadSheet: TdxSpreadSheet;
begin
  SpreadSheet := TdxSpreadSheet.Create(nil);
  try
    ExportToSpreadSheet(SpreadSheet);
    SpreadSheet.SaveToFile(FileName);
  finally
    FreeAndNil(SpreadSheet);
  end;
end;}

procedure TXLSExportDevExpress.DoSaveToStream(Dst: TStream; const FileType: string);
var
  SpreadSheet: TdxSpreadSheet;
  SpreadSheetFormat: TdxSpreadSheetCustomFormatClass;
begin
  SpreadSheet := TdxSpreadSheet.Create(nil);
  try
    ExportToSpreadSheet(SpreadSheet);

    { TdxSpreadSheet.SaveToStream is much faster if we provide format parameter for XLS files.
      Probably if we don't, then they use another format (xlsx?). }
    if not dxSpreadSheetFormatsRepository.Find(FileType, SpreadSheetFormat) then
      raise EdxSpreadSheetFormatError.Create(cxGetResourceString(@sdxErrorUnsupportedDocumentFormat));
    SpreadSheet.SaveToStream(Dst, SpreadSheetFormat);
  finally
    FreeAndNil(SpreadSheet);
  end;
end;

procedure TXLSExportDevExpress.ExportToSpreadSheet(SpreadSheet: TdxSpreadSheet);
var
  Sheet: TdxSpreadSheetCustomView;
  I: Integer;
begin
  Assert(SheetCount > 0);
  SpreadSheet.BeginUpdate;
  try
    SpreadSheet.ClearAll;
    for I := 0 to SheetCount-1 do
    begin
      if I < SpreadSheet.SheetCount
        then Sheet := SpreadSheet.Sheets[I]
        else Sheet := SpreadSheet.AddSheet(Sheets[I].SheetName);
      ExportSheet(Sheets[I], Sheet as TdxSpreadSheetTableView);
    end;
    if ActiveSheetIndex >= 0 then
      SpreadSheet.ActiveSheetIndex := ActiveSheetIndex;
    if Password<>'' then
      SpreadSheet.Password := Password;
  finally
    SpreadSheet.EndUpdate;
  end;
end;

procedure TXLSExportDevExpress.ExportSheetCells(Src: TXLSSheet; Dst: TdxSpreadSheetTableView; BeforeApplyBestFit: boolean);
var
  SrcPair: TPair<TXLSPos, TXLSCell>;
  DstCell: TdxSpreadSheetCell;
begin
  for SrcPair in Src do
    if not SrcPair.Value.Empty and (SrcPair.Value.ConsiderWhenApplyBestFit=BeforeApplyBestFit) then
    begin
      DstCell := Dst.CreateCell(SrcPair.Key.Row + Src.VertOffset, SrcPair.Key.Col + Src.HorOffset);
      ExportCell(SrcPair.Value, DstCell);
    end;
end;

procedure TXLSExportDevExpress.ExportSheet(Src: TXLSSheet; Dst: TdxSpreadSheetTableView);
var
  MergeRegion: TRect;
  Col: integer;
  CW: TPair<integer, integer>;
begin
  Dst.BeginUpdate;
  try

    { Cells where ConsiderWhenApplyBestFit=True should be moved before ApplyBestFit }
    ExportSheetCells(Src, Dst, True);

    { merge regions }
    for MergeRegion in Src.MergeRegionsCollection do
    begin
      MergeRegion.Offset(Src.HorOffset, Src.VertOffset);
      Dst.Selection.Clear;
      Dst.Selection.Add(MergeRegion);
      Dst.MergeSelected;
    end;

    { ColWidth / RowHeight }
    { Src.ColWidthCollection returns original values, we use }
    for CW in Src.ColWidthCollection do
      if Dst.Columns[CW.Key + Src.HorOffset]<>nil then
        Dst.Columns[CW.Key + Src.HorOffset].Size := CW.Value;
    for CW in Src.RowHeightCollection do
      if Dst.Rows[CW.Key + Src.VertOffset]<>nil then
        Dst.Rows[CW.Key + Src.VertOffset].Size := CW.Value;

    { ApplyBestFit }
    for Col in Src.ApplyBestFitCollection do
      if Dst.Columns[Col + Src.HorOffset]<>nil then
        Dst.Columns[Col + Src.HorOffset].ApplyBestFit;

    { Cells where ConsiderWhenApplyBestFit=False should be moved after ApplyBestFit }
    ExportSheetCells(Src, Dst, False);

    { OptionsView }
    Dst.FrozenColumn := Src.OptionsView.FrozenColumns + Src.HorOffset - 1;
    Dst.FrozenRow := Src.OptionsView.FrozenRows + Src.HorOffset - 1;
    Dst.Visible := Src.OptionsView.Visible;

    { OptionsPrint }
    if Src.OptionsPrint.FitToPagesWide > 0 then
      Dst.OptionsPrint.Page.FitToWidth := Src.OptionsPrint.FitToPagesWide;
    if Src.OptionsPrint.FitToPagesTall > 0 then
      Dst.OptionsPrint.Page.FitToHeight := Src.OptionsPrint.FitToPagesTall;
  finally
    Dst.EndUpdate;
  end;
end;

procedure TXLSExportDevExpress.ImportSheet(Src: TdxSpreadSheetTableView; Dst: TXLSSheet);
var
  Col: TdxSpreadSheetTableColumn;
  Row: TdxSpreadSheetTableRow;
  Cell: TdxSpreadSheetCell;
  MergedCell: TdxSpreadSheetMergedCell;
begin

  { copy cells }
  for Cell in TdxEnumerators.SpreadSheetCells(Src) do
    ImportCell(Cell, Dst.Cells[Cell.RowIndex, Cell.ColumnIndex]);

  { merge regions }
  for MergedCell in TdxEnumerators.SpreadSheetMergedCells(Src) do
    Dst.Merge(MergedCell.Area);

  { ApplyBestFit
    nothing to do here (can be done only when export) }

  { Columns.Size / Rows.Size }
  for Col in TdxEnumerators.SpreadSheetColumns(Src) do
    if not Col.DefaultSize then
      Dst.ColWidth[Col.Index] := Col.Size;
  for Row in TdxEnumerators.SpreadSheetRows(Src) do
    if not Row.DefaultSize then
      Dst.RowHeight[Row.Index] := Row.Size;

  { OptionsView }
  Dst.OptionsView.FrozenRows := Src.FrozenRow;
  Dst.OptionsView.FrozenColumns := Src.FrozenColumn;
  Dst.OptionsView.Visible := Src.Visible;

  { Print options }
  if Src.OptionsPrint.Page.FitToWidth > 0 then
    Dst.OptionsPrint.FitToPagesWide := Src.OptionsPrint.Page.FitToWidth;
  if Src.OptionsPrint.Page.FitToHeight > 0 then
    Dst.OptionsPrint.FitToPagesTall := Src.OptionsPrint.Page.FitToHeight;
end;

procedure TXLSExportDevExpress.SetExcelCellFormat(Src: TXLSCell; Dst: TdxSpreadSheetCell);
begin
  {$IF [Low(Src.DisplayFormat)..High(Src.DisplayFormat)]<>[dfDefault, dfCurrency, dfFloatNumber, dfIntNumber, dfDateTime, dfDate, dfBoolean, dfString]}
    {$Message Error 'Support of new DisplayFormat must be implemented here'}
  {$ENDIF}
  case Src.DisplayFormat of
    dfDefault     : ;
    dfCurrency    : Dst.Style.DataFormat.FormatCodeID := $4; { #,##0.00 (see TdxSpreadSheetCustomDataFormat.FormatCodeID) }
    dfFloatNumber : Dst.Style.DataFormat.FormatCodeID := $0; { GENERAL (see TdxSpreadSheetCustomDataFormat.FormatCodeID) }
    dfIntNumber   : Dst.Style.DataFormat.FormatCodeID := $0;
    dfDateTime    : Dst.Style.DataFormat.FormatCodeID := $0;
    dfDate        : Dst.Style.DataFormat.FormatCodeID := $0;
    dfBoolean     : Dst.Style.DataFormat.FormatCodeID := $0;
    dfString      :
      if Src.PreserveStringType then
        Dst.Style.DataFormat.FormatCode := '@'; { keep string as string even if it looks like number/date etc }
  end;
end;

procedure TXLSExportDevExpress.ExportCell(Src: TXLSCell; Dst: TdxSpreadSheetCell);
begin
  Dst.Style.BeginUpdate;
  try

    { Src.Style.Format / Src.Value}
    {$IF [Low(TXLSValueType)..High(TXLSValueType)] <> [xevtNull, xevtBoolean, xevtInteger, xevtFloat, xevtCurrency, xevtDateTime, xevtDate, xevtString, xevtFormula]}
      {$Message Error 'support of new TXLSValueType value should be implemented here'}
    {$ENDIF}
    case Src.ValueType of
      xevtNull     : ;
      xevtBoolean  : Dst.AsBoolean  := Src.AsBoolean;
      xevtInteger  : Dst.AsInteger  := Src.AsInteger;
      xevtFloat    : Dst.AsFloat    := Src.AsFloat;
      xevtCurrency : Dst.AsCurrency := Src.AsCurrency;
      xevtDateTime : Dst.AsDateTime := Src.AsDateTime;
      xevtDate     : Dst.AsDateTime := Src.AsDate;
      xevtString   : Dst.AsString   := Src.AsString;
      xevtFormula  : Dst.SetText(Src.AsFormula, True);
    end;

    { set display format }
    SetExcelCellFormat(Src, Dst);

    { Src.Style.Font }
    with Src.Style.Font do
    begin
      if not Name.Empty then
        Dst.Style.Font.Name := Name;
      if not Color.Empty then
        Dst.Style.Font.Color := Color;
      if not Style.Empty then
        Dst.Style.Font.Style := Style;
      if not Height.Empty then
        Dst.Style.Font.Height := Height;
      if not Size.Empty then
        Dst.Style.Font.Size := Size;
    end;

    { Src.Style.Borders }
    with Src.Style do
    begin
      { BorderStyle }
      if not Borders[xbLeft].Style.Empty then
        Dst.Style.Borders[bLeft].Style := BorderStyleTranslation[Borders[xbLeft].Style.Value];
      if not Borders[xbTop].Style.Empty then
        Dst.Style.Borders[bTop].Style := BorderStyleTranslation[Borders[xbTop].Style.Value];
      if not Borders[xbRight].Style.Empty then
        Dst.Style.Borders[bRight].Style := BorderStyleTranslation[Borders[xbRight].Style.Value];
      if not Borders[xbBottom].Style.Empty then
        Dst.Style.Borders[bBottom].Style := BorderStyleTranslation[Borders[xbBottom].Style.Value];
      { BorderColor }
      if not Borders[xbLeft].Color.Empty then
        Dst.Style.Borders[bLeft].Color := Borders[xbLeft].Color.Value;
      if not Borders[xbTop].Color.Empty then
        Dst.Style.Borders[bTop].Color := Borders[xbTop].Color.Value;
      if not Borders[xbRight].Color.Empty then
        Dst.Style.Borders[bRight].Color := Borders[xbRight].Color.Value;
      if not Borders[xbBottom].Color.Empty then
        Dst.Style.Borders[bBottom].Color := Borders[xbBottom].Color.Value;
    end;

    { Src.Style.BackgroundColor }
    with Src.Style.BackgroundColor do
      if not Empty then
        Dst.Style.Brush.BackgroundColor := Value;

    { Src.Style.AlignHorz }
    with Src.Style.AlignHorz do
      if not Empty then
        Dst.Style.AlignHorz := AlignHorzTranslation[Value];

    { Src.Style.AlignVert }
    with Src.Style.AlignVert do
      if not Empty then
        Dst.Style.AlignVert := AlignVertTranslation[Value];

  finally
    Dst.Style.EndUpdate;
  end;
end;

class function TXLSExportDevExpress.DoIsPasswordProtected(const SpreadsheetFileName: string): boolean;
begin
  result := TPswValidation.IsPasswordProtected(SpreadsheetFileName);
end;

class function TXLSExportDevExpress.DoIsValidPassword(const SpreadsheetFileName, Password: string): boolean;
begin
  result := TPswValidation.IsValidPassword(SpreadsheetFileName, Password);
end;

procedure TXLSExportDevExpress.DoLoadFromFile(const FileName: string);
var
  SpreadSheet: TdxSpreadSheet;
begin
  SpreadSheet := TdxSpreadSheet.Create(nil);
  try
    SpreadSheet.LoadFromFile(FileName);
    ImportFromSpreadSheet(SpreadSheet);
  finally
    FreeAndNil(SpreadSheet);
  end;
end;

procedure TXLSExportDevExpress.DoLoadFromStream(Src: TStream);
var
  SpreadSheet: TdxSpreadSheet;
begin
  SpreadSheet := TdxSpreadSheet.Create(nil);
  try
    SpreadSheet.LoadFromStream(Src);
    ImportFromSpreadSheet(SpreadSheet);
  finally
    FreeAndNil(SpreadSheet);
  end;
end;

procedure TXLSExportDevExpress.ImportFromSpreadSheet(SpreadSheet: TdxSpreadSheet);
var
  Sheet: TdxSpreadSheetTableView;
  I: Integer;
begin
  SpreadSheet.BeginUpdate;
  try
    Clear;
    for I := 0 to SpreadSheet.SheetCount-1 do
    begin
      Sheet := SpreadSheet.Sheets[I] as TdxSpreadSheetTableView;
      ImportSheet(Sheet, AddSheet(Sheet.Caption));
    end;
    if SpreadSheet.ActiveSheetIndex >= 0 then
      ActiveSheetIndex := SpreadSheet.ActiveSheetIndex;
    if SpreadSheet.Password <> '' then
      FPassword := SpreadSheet.Password;
  finally
    SpreadSheet.EndUpdate;
  end;
end;

procedure TXLSExportDevExpress.ImportCell(Src: TdxSpreadSheetCell; Dst: TXLSCell);
begin

  { Src.Style.Format / Src.Value}
  {$IF [Low(TdxSpreadSheetCellDataType)..High(TdxSpreadSheetCellDataType)] <>
    [cdtBlank, cdtBoolean, cdtError, cdtCurrency, cdtFloat, cdtDateTime, cdtInteger, cdtString, cdtFormula]}
    implement support for new types bellow!
  {$ENDIF}
  case Src.DataType of
    cdtBlank    : ;
    cdtBoolean  : Dst.AsBoolean  := Src.AsBoolean;
    cdtError    : ;
    cdtCurrency : Dst.AsCurrency := Src.AsCurrency; { format? }
    cdtFloat    : Dst.AsFloat    := Src.AsFloat;    { format? }
    cdtDateTime : Dst.AsDateTime := Src.AsDateTime; { format? }
    cdtInteger  : Dst.AsInteger  := Src.AsInteger;
    cdtString   : Dst.AsString   := Src.AsString;
    cdtFormula  : Src.AsString   := Src.AsFormula.AsText;
  end;

  { Src.Style.Font }
  with Src.Style.Font do
  begin
    Dst.Style.Font.Name   := Name;
    Dst.Style.Font.Color  := Color;
    Dst.Style.Font.Style  := Style;
    Dst.Style.Font.Height := Height;
    Dst.Style.Font.Size   := Size;
  end;

  { Src.Style }
  with Src.Style do
  begin
    { Src.Style.Borders[].BorderStyle }
    if Borders[bLeft].Style <> sscbsDefault then
      Dst.Style.Borders[xbLeft].Style := BorderTypeTranslationBack[Borders[bLeft].Style];
    if Borders[bTop].Style <> sscbsDefault then
      Dst.Style.Borders[xbTop].Style := BorderTypeTranslationBack[Borders[bTop].Style];
    if Borders[bRight].Style <> sscbsDefault then
      Dst.Style.Borders[xbRight].Style := BorderTypeTranslationBack[Borders[bRight].Style];
    if Borders[bBottom].Style <> sscbsDefault then
      Dst.Style.Borders[xbBottom].Style := BorderTypeTranslationBack[Borders[bBottom].Style];
    { Src.Style.Borders[].Color }
    if Borders[bLeft].Color <> clDefault then
      Dst.Style.Borders[xbLeft].Color := Borders[bLeft].Color;
    if Borders[bTop].Color <> clDefault then
      Dst.Style.Borders[xbTop].Color := Borders[bTop].Color;
    if Borders[bRight].Color <> clDefault then
      Dst.Style.Borders[xbRight].Color := Borders[bRight].Color;
    if Borders[bBottom].Color <> clDefault then
      Dst.Style.Borders[xbBottom].Color := Borders[bBottom].Color;
  end;

  { Src.Style.BackgroundColor }
  with Src.Style.Brush do
    if BackgroundColor<>clDefault then
      Dst.Style.BackgroundColor := BackgroundColor;

  { Src.Style.AlignHorz }
  Dst.Style.AlignHorz := AlignHorzTranslationBack[Src.Style.AlignHorz];

  { Src.Style.AlignVert }
  Dst.Style.AlignVert := AlignVertTranslationBack[Src.Style.AlignVert];

end;

{ TXLSExportDevExpress.TPswValidation }

class function TXLSExportDevExpress.TPswValidation.IsPasswordProtected(const SpreadsheetFileName: string): boolean;
var
  v: TPswValidation;
begin
  v := TPswValidation.Create;
  try
    result := v.DoIsPasswordProtected(SpreadsheetFileName);
  finally
    v.Free;
  end;
end;

class function TXLSExportDevExpress.TPswValidation.IsValidPassword(const SpreadsheetFileName, Password: string): boolean;
var
  v: TPswValidation;
begin
  v := TPswValidation.Create;
  try
    result := v.DoIsValidPassword(SpreadsheetFileName, Password);
  finally
    v.Free;
  end;
end;

function TXLSExportDevExpress.TPswValidation.OnGetPsw(Sender: TObject; out Password: string): Boolean;
begin
  Password := FPassword;
  result := True;
end;

function TXLSExportDevExpress.TPswValidation.DoIsPasswordProtected(const SpreadsheetFileName: string): boolean;
begin
  result := not IsValidPassword(SpreadsheetFileName, '');
end;

function TXLSExportDevExpress.TPswValidation.DoIsValidPassword(const SpreadsheetFileName, Password: string): boolean;
var
  b: TdxSpreadSheet;
begin
  result := True;
  b := TdxSpreadSheet.Create(nil);
  try
    FPassword := Password;
    { We set OnGetPassword to disable password input dialog }
    b.OnGetPassword := OnGetPsw;
    try
      b.LoadFromFile(SpreadsheetFileName);
    except
      on e: EdxOLECryptoContainerError do { dxOLECryptoContainer.pas }
        result := False;
      else
        { in case of unknown error (stream read error, damaged file etc) }
        Raise;
    end;
  finally
    b.Free;
  end;
end;

end.
