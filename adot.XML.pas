unit adot.XML;

interface

uses
  System.StrUtils,
  System.SysUtils,
  System.Classes,
  Soap.XSBuiltIns,
  Data.FMTBcd,
  Xml.xmldom,
  Xml.XMLDoc,
  Xml.XMLIntf;

type
  TXMLUtils = class
    class function IsWellFormedXML(const XML: string): boolean; overload; static;
    class function IsWellFormedXML(const XML: TStream): boolean; overload; static;
    class procedure MakeReadableCData(Src: IXMLNode); static;
    class function GetReadableXML(const SrcXML: string): string; static;
    class function GetReadableXMLFile(const AFileName: string): string; static;
    class procedure MakeXMLFileReadable(const AFileName: string); static;
    class procedure SaveToFile(const AXML, AFileName: string; AMakeReadable: boolean = True); static;
    class function GetXML(const ASrc: TArray<byte>): string; overload;
    class function GetXML(const ASrc: TStream): string; overload;

    class function TryDecodeDecimal(const ADecimal: string; out Value: double): boolean; static;
    class function DecodeDecimal(const ADecimal: string): double; static;
    class function DecodeDecimalDef(const ADecimal: string; ADef: double): double; static;

    class function TryDecodeXMLTime(const AXMLTime: string; out Value: TDateTime): boolean; static;
    class function DecodeXMLTime(const AXMLTime: string): TDateTime; static;
    class function DecodeXMLTimeDef(const AXMLTime: string; ADef: TDateTime): TDateTime; static;
  end;

implementation

{ TXMLUtils }

class function TXMLUtils.IsWellFormedXML(const XML: string): boolean;
var
  dom: IDOMDocument;
begin
  dom := GetDom.createDocument('', '', nil);
  (dom as IDOMParseoptions).validate := false;
  (dom as IDomPersist).loadXML(XML);
  result := (dom as IDOMParseError).errorCode = 0;
end;

class procedure TXMLUtils.MakeReadableCData(Src: IXMLNode);
var
  i: Integer;
  xml: DOMString;
begin
  for i := 0 to Src.ChildNodes.Count-1 do
    MakeReadableCData(Src.ChildNodes[i]);
  if Src.NodeType<>ntCData then
    Exit;
  xml := Src.Text;
  if IsWellFormedXML(xml) then
    Src.Text := IfThen(xml.StartsWith(#10),'',#10) + FormatXMLData(xml) + IfThen(xml.EndsWith(#10),'',#10);
end;

class function TXMLUtils.GetReadableXML(const SrcXML: string): string;
var
  doc: IXMLDocument;
begin
  doc := TXMLDocument.Create(nil);
  doc.LoadFromXML(SrcXML);
  MakeReadableCData(doc.DocumentElement);
  doc.SaveToXML(result);
  result := FormatXMLData(result);
end;

class function TXMLUtils.GetReadableXMLFile(const AFileName: string): string;
var
  doc: IXMLDocument;
begin
  doc := TXMLDocument.Create(nil);
  doc.LoadFromFile(AFileName);
  MakeReadableCData(doc.DocumentElement);
  doc.SaveToXML(result);
  result := FormatXMLData(result);
end;

class function TXMLUtils.GetXML(const ASrc: TStream): string;
var
  dom: IDOMDocument;
begin
  dom := GetDom.createDocument('', '', nil);
  (dom as IDOMParseoptions).validate := false;
  (dom as IDomPersist).loadFromStream(ASrc);
  result := (dom as IDomPersist).xml;
end;

class function TXMLUtils.GetXML(const ASrc: TArray<byte>): string;
var
  LSrcStream: TBytesStream;
begin
  // OB 11.02.2016 endret fra :
  // result := TEncoding.UTF8.GetString(ASrc);

  // OB 11.02.2016 endret til :
  LSrcStream := TBytesStream.Create(ASrc);
  try
    LSrcStream.Position := 0;
    Result := GetXML(LSrcStream);
  finally
    FreeAndNil(LSrcStream);
  end;
end;

class function TXMLUtils.IsWellFormedXML(const XML: TStream): boolean;
var
  dom: IDOMDocument;
begin
  dom := GetDom.createDocument('', '', nil);
  (dom as IDOMParseoptions).validate := false;
  (dom as IDomPersist).loadFromStream(XML);
  result := (dom as IDOMParseError).errorCode = 0;
end;

class procedure TXMLUtils.MakeXMLFileReadable(const AFileName: string);
var
  doc: IXMLDocument;
  xml: string;
begin
  doc := TXMLDocument.Create(nil);
  doc.LoadFromFile(AFileName);
  MakeReadableCData(doc.DocumentElement);
  doc.SaveToXML(xml);
  xml := FormatXMLData(xml);
  doc.LoadFromXML(xml);
  doc.SaveToFile(AFileName);
end;

class procedure TXMLUtils.SaveToFile(const AXML, AFileName: string; AMakeReadable: boolean);
var
  b: TArray<Byte>;
  s: string;
  d: TFileStream;
begin
  if AMakeReadable then
    s := GetReadableXML(AXML)
  else
    s := AXML;
  b := TEncoding.UTF8.GetBytes(s);
  d := TFileStream.Create(AFileName, fmCreate);
  try
    d.WriteBuffer(b, Length(b));
  finally
    d.Free;
  end;
end;

class function TXMLUtils.TryDecodeDecimal(const ADecimal: string; out Value: double): boolean;
begin
  result := ADecimal<>'';
  if result then
    try
      Value := DecodeDecimal(ADecimal);
    except
      result := False;
    end;
end;

class function TXMLUtils.DecodeDecimal(const ADecimal: string): double;
var
  Decimal: TXSDecimal;
begin
  Decimal := TXSDecimal.Create;
  try
    Decimal.XSToNative(ADecimal);
    result := BCDToCurrency(Decimal.AsBcd);
  finally
    Decimal.Free;
  end;
end;

class function TXMLUtils.DecodeDecimalDef(const ADecimal: string; ADef: double): double;
begin
  if not TryDecodeDecimal(ADecimal, result) then
    result := ADef;
end;

class function TXMLUtils.TryDecodeXMLTime(const AXMLTime: string; out Value: TDateTime): boolean;
begin
  result := AXMLTime<>'';
  if result then
    try
      Value := DecodeXMLTime(AXMLTime);
    except
      result := False;
    end;
end;

class function TXMLUtils.DecodeXMLTime(const AXMLTime: string): TDateTime;
begin
  result := XMLTimeToDateTime(AXMLTime);
end;

class function TXMLUtils.DecodeXMLTimeDef(const AXMLTime: string; ADef: TDateTime): TDateTime;
begin
  if not TryDecodeXMLTime(AXMLTime, result) then
    result := ADef;
end;

end.
