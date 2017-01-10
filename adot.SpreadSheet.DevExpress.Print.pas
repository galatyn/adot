unit adot.SpreadSheet.DevExpress.Print;

interface

uses
  adot.Tools,
  adot.SpreadSheet.Types,
  System.SysUtils, System.Classes, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev, dxPSCompsProvider,
  dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore, dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPSPrVwAdv,
  dxPSPrVwRibbon, dxPScxPageControlProducer, dxPSdxSpreadSheetLnk, dxPScxEditorProducers, dxPScxExtEditorProducers,
  dxPSCore, cxClasses, dxSpreadSheet, System.Math, dxSpreadSheetCore,
  dxSpreadSheetPrinting;

type
  TPrintExcelFiles = class(TDataModule)
    dxComponentPrinter: TdxComponentPrinter;
    dxComponentPrinterLinkComposition: TdxCompositionReportLink;
    procedure dxComponentPrinterLinkCompositionBeforeBuildReport(Sender: TdxCompositionReportLink;
      AItem: TdxCompositionLinkItem);
  private
    class var
      FOrdinal: TPrintExcelFiles;

    class function GetOrdinal: TPrintExcelFiles; static;
    class destructor DestroyClass; static;

  public
    class procedure BuildCompositionForSheets(Book: TdxSpreadSheet; Composite: TdxCompositionReportLink; MaxSheetCount: integer); static;
    class function SelectPrinter(const PrinterName: string): boolean; static;

    { Example: TPrintExcelFiles.Ordinal.Print('MyTest.xlsx'); }
    procedure Print(SpreadSheet: TdxSpreadSheet); overload;
    procedure Print(SpreadSheet: TdxSpreadSheet; const Options: TXLSPrintOptions); overload;
    procedure Print(const SpreadSheetFileName: string); overload;
    procedure Print(const SpreadSheetFileName: string; const Options: TXLSPrintOptions); overload;

    class property Ordinal: TPrintExcelFiles read GetOrdinal;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TPrintExcelFiles }

class function TPrintExcelFiles.GetOrdinal: TPrintExcelFiles;
begin
  if FOrdinal=nil then
    FOrdinal := TPrintExcelFiles.Create(nil);
  result := FOrdinal;
end;

class destructor TPrintExcelFiles.DestroyClass;
begin
  Sys.FreeAndNil(FOrdinal);
end;

procedure TPrintExcelFiles.dxComponentPrinterLinkCompositionBeforeBuildReport(Sender: TdxCompositionReportLink;
  AItem: TdxCompositionLinkItem);
begin
  { When we print sublink, we change active sheet correspondingly (printing engine always prints active sheet only). }
  (AItem.ReportLink.Component as TdxSpreadSheet).ActiveSheetIndex := AItem.ReportLink.Tag;
end;

class function TPrintExcelFiles.SelectPrinter(const PrinterName: string): boolean;
var
  I: Integer;
begin
  I := dxPrintDevice.Printers.IndexOf(PrinterName);
  result := I >= 0;
  if result then
    dxPrintDevice.PrinterIndex := I;
end;

procedure TPrintExcelFiles.Print(SpreadSheet: TdxSpreadSheet);
begin
  Print(SpreadSheet, TXLSPrintOptions.Create('', False));
end;

procedure TPrintExcelFiles.Print(const SpreadSheetFileName: string);
begin
  Print(SpreadSheetFileName, TXLSPrintOptions.Create('', False));
end;

procedure TPrintExcelFiles.Print(SpreadSheet: TdxSpreadSheet; const Options: TXLSPrintOptions);
var
  I,J: Integer;
begin
  I := dxPrintDevice.PrinterIndex;
  J := SpreadSheet.ActiveSheetIndex;
  try
    SelectPrinter(Options.PrinterName);
    BuildCompositionForSheets(SpreadSheet, dxComponentPrinterLinkComposition, high(integer));
    dxComponentPrinter.Print(Options.SettingsUI, nil);
  finally
    SpreadSheet.ActiveSheetIndex := J;
    dxPrintDevice.PrinterIndex := I;
  end;
end;

procedure TPrintExcelFiles.Print(const SpreadSheetFileName: string; const Options: TXLSPrintOptions);
var
  SpreadSheet: TdxSpreadSheet;
begin
  SpreadSheet := TdxSpreadSheet.Create(nil);
  try
    SpreadSheet.LoadFromFile(SpreadSheetFileName);
    Print(SpreadSheet, Options);
  finally
    SpreadSheet.Free;
  end;
end;

class procedure TPrintExcelFiles.BuildCompositionForSheets(Book: TdxSpreadSheet; Composite: TdxCompositionReportLink;
  MaxSheetCount: integer);
var
  OnBeforeBuildReport: TdxCompositionReportLinkEvent;
  I: Integer;
  Link: TdxSpreadSheetReportLnk;
  View: TdxSpreadSheetTableView;
  Shrink: boolean;
begin
  if (Book = nil) or (Composite = nil) then
    Exit;

  { When we add sublink to composition it triggers OnBeforeBuildReport (where we change active sheet etc).
    We need OnBeforeBuildReport to be triggered only when we print document, so we disable it here. }
  OnBeforeBuildReport := Composite.OnBeforeBuildReport;
  try
    Composite.OnBeforeBuildReport := nil;

    { Printing component always prints active sheet only (at least in DevExpress 16.2.2 and earlier).
      To print all sheets we have to create composite link with set of sublinks (one sublink for every sheet).
      When we print sublink, we change active sheet correspondingly
      (look at event handler dxComponentPrinterLinkCompositionBeforeBuildReport). }
    Composite.Items.Clear;
    Shrink := False;
    for I := 0 to Min(Book.SheetCount, MaxSheetCount) - 1 do
    begin
      if (I = Book.ActiveSheetIndex) and (Book.Sheets[I] is TdxSpreadSheetTableView) then
      begin
        View := Book.Sheets[I] as TdxSpreadSheetTableView;
        Shrink := Shrink or (View.OptionsPrint.Page.ScaleMode = oppsmFitToPage);
      end;

      Link := TdxSpreadSheetReportLnk(Composite.ComponentPrinter.AddLink(Book));
      Link.Tag := I;
      Link.OptionsView := [];
      Composite.Items.Add.ReportLink := Link;
    end;

    { ShrinkToPageWidth in sublinks is ignored, only ShrinkToPageWidth of composite link is in use.
      And it can not be assigned in OnBeforeBuildReport event handler of composite link.
      We have to assign it globally for all sheets according to settings of active sheet
      (not perfect solution, but better than ignoring it at all). }
    Composite.ShrinkToPageWidth := Shrink;

  finally
    Composite.OnBeforeBuildReport := OnBeforeBuildReport;
  end;

end;

end.
