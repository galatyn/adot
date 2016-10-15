unit adot.SpreadSheet.Export;

interface

uses
  adot.SpreadSheet.Classes,
  adot.SpreadSheet.ExcelDevExpress,
  adot.SpreadSheet.ExcelOle,
  System.SysUtils;

(* Example of export:

    Uses
      adot.SpreadSheet.Types, adot.SpreadSheet.Classes, adot.SpreadSheet.Export;

    procedure Demo;
    var
      Book: TXLSBook;
      Sheet: TXLSSheet;
    begin
      Book := ExportFactory.NewBook;

      { Add sheet and fill cells with data. }
      Sheet := Book.AddSheet('Sheet1');
      Sheet.Writeln(['Const', 'Value', 'Date'], [fsBold]);
      Sheet.Writeln(['Pi', 3.1415926, Now]);
      Sheet.Writeln(['E', 2.71828, Now]);
      Sheet.ApplyBestFitForAllColumns;

      { Add one more sheet with data. }
      Sheet := Book.AddSheet('Sheet2');
      Sheet.Merge(0,0,1,0);
      Sheet[0,0].AsString := 'Title';
      Sheet[0,0].Style.AlignHorz := xeahCenter;
      Sheet[1,0].AsString := 'text1';
      Sheet[1,1].AsString := 'text2';
      Sheet.ApplyBestFitForAllColumns;

      { Save to file (SaveToStream is also available). }
      Book.SaveToFile('c:\test.xls');

      { We destroy only the book, all book internals will be destroyed automatically. }
      FreeAndNil(Book);
    end; *)

var
  ExportFactory: TXlsFactory;

implementation

type
  TFactory = class(TXlsFactory)
  protected
    function DoNewBook: TXLSBook; override;
  end;

{ TFactory }

function TFactory.DoNewBook: TXLSBook;
begin
  //result := TXLSExportDevExpress.Create;
  result := TXLSExportExcelOle.Create;
end;

initialization
  ExportFactory := TFactory.Create;

finalization
  FreeAndNil(ExportFactory);

end.
