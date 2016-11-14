unit adot.SpreadSheet.Import;

interface

uses
  adot.SpreadSheet.Classes,
  adot.SpreadSheet.ExcelDevExpress,
  adot.SpreadSheet.ExcelOle,
  System.SysUtils;

function ImportFactory: TXlsFactory;
procedure SetNewImportFactory(AFactory: TXlsFactory);

implementation

type
  TDefaultImportFactory = class(TXlsFactory)
  protected
    function DoNewBook: TXLSBook; override;
  end;

{ TDefaultImportFactory }

function TDefaultImportFactory.DoNewBook: TXLSBook;
begin
  { for import we don't need to use Excel over OLE, it is faster to use DevExpress }
  result := TXLSExportDevExpress.Create;
end;

{ ImportFactory / SetNewImportFactory }

var
  ImportFactoryInst: TXlsFactory;

procedure SetNewImportFactory(AFactory: TXlsFactory);
begin
  if ImportFactoryInst <> AFactory then
  begin
    FreeAndNil(ImportFactoryInst);
    ImportFactoryInst := AFactory;
  end;
end;

function ImportFactory: TXlsFactory;
begin
  if ImportFactoryInst = nil then
    SetNewImportFactory(TDefaultImportFactory.Create);
  result := ImportFactoryInst;
end;

initialization

finalization
  SetNewImportFactory(nil);

end.
