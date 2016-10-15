unit adot.SpreadSheet.Import;

interface

uses
  adot.SpreadSheet.Classes,
  adot.SpreadSheet.ExcelDevExpress,
  adot.SpreadSheet.ExcelOle,
  System.SysUtils;

var
  ImportFactory: TXlsFactory;

implementation

type
  TFactory = class(TXlsFactory)
  protected
    function DoNewBook: TXLSBook; override;
  end;

{ TFactory }

function TFactory.DoNewBook: TXLSBook;
begin
  result := TXLSExportDevExpress.Create;
end;

initialization
  ImportFactory := TFactory.Create;

finalization
  FreeAndNil(ImportFactory);

end.
