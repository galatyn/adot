unit adot.SpreadSheet.DevExpress.Print;

interface

uses
  adot.SpreadSheet.Types,
  Winapi.Windows, dxPSGlbl, dxPSUtl, dxPSEngn, dxPrnPg, dxBkgnd, dxWrap, dxPrnDev, dxPSCompsProvider, dxPSFillPatterns, dxPSEdgePatterns, dxPSPDFExportCore,
  dxPSPDFExport, cxDrawTextUtils, dxPSPrVwStd, dxPSPrVwAdv, dxPSPrVwRibbon, dxPScxPageControlProducer, dxPSdxSpreadSheetLnk, dxPScxEditorProducers,
  dxPScxExtEditorProducers, dxPSCore, dxPSBaseGridLnk, System.Classes, cxClasses,
  Vcl.Forms, dxSpreadSheet;

type
  TPrintSpreadsheetDevExpress = class(TForm)
    dxComponentPrinter: TdxComponentPrinter;
    dxComponentPrinterLink1: TdxSpreadSheetReportLnk;
  private
  public
    procedure Print(SpreadSheet: TdxSpreadSheet; const Options: TXLSPrintOptions); overload;
    procedure Print(const SpreadSheetFileName: string; const Options: TXLSPrintOptions); overload;
  end;

implementation

{$R *.dfm}

{ TPrintSpreadsheetDevExpress }

procedure TPrintSpreadsheetDevExpress.Print(SpreadSheet: TdxSpreadSheet; const Options: TXLSPrintOptions);
var
  I: Integer;
begin
  dxComponentPrinterLink1.Component := SpreadSheet;
  try
    I := dxPrintDevice.Printers.IndexOf(Options.PrinterName);
    if I >= 0 then
      dxPrintDevice.PrinterIndex := I;
    dxComponentPrinter.Print(Options.SettingsUI, nil);
  finally
    dxComponentPrinterLink1.Component := nil;
  end;
end;

procedure TPrintSpreadsheetDevExpress.Print(const SpreadSheetFileName: string; const Options: TXLSPrintOptions);
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

end.
