unit adot.XML.XPath;

interface

uses
  Xml.XMLDoc,
  Xml.XMLIntf,
  Xml.xmldom,
  Xml.omnixmldom,
  //Xml.adomxmldom,
  System.SysUtils,
  System.Masks;

type

  (*
    Example:
      XPath: TXPath;
      Node: IDOMNode;
    begin
      XPath.Init; { creates new XML document with vendor supporting XPath }
      XPath.Document.LoadFromXML(XMLString);
      if XPath.Find('/Project/PropertyGroup[DCC_Define=''RELEASE;$(DCC_Define)'']', Node) then
        if XPath.Find(Node, 'DCC_MapFile[.=''3'']') then
          Exit;
    end;
  *)
  TXPath = record
  private
    FDoc: IXMLDocument;

    function FindSelect(Node: IDomNode): IDomNodeSelect;

  public
    procedure Init; overload;
    procedure Init(Doc: TXMLDocument); overload;
    procedure Init(Doc: IXMLDocument); overload;

    class function Create: TXPath; overload; static;
    class function Create(Doc: TXMLDocument): TXPath; overload; static;
    class function Create(Doc: IXMLDocument): TXPath; overload; static;

    function Find(Where: IDomNode; const XPath: string; out Res: IDomNode): boolean; overload;
    function Find(Where: IDomNode; const XPath: string; out Res: IDOMNodeList): boolean; overload;
    function Find(Where: IDomNode; const XPath: string): boolean; overload;

    function Find(const XPath: string; out Res: IDomNode): boolean; overload;
    function Find(const XPath: string; out Res: IDOMNodeList): boolean; overload;
    function Find(const XPath: string): boolean; overload;

    class operator Implicit(const a : IXMLDocument) : TXPath;
    class operator Implicit(const a : TXMLDocument) : TXPath;
    class operator Explicit(const a : IXMLDocument) : TXPath;
    class operator Explicit(const a : TXMLDocument) : TXPath;

    property Document: IXMLDocument read FDoc;
  end;

implementation

{ TXPath }

class function TXPath.Create: TXPath;
begin
  result.Init;
end;

class function TXPath.Create(Doc: IXMLDocument): TXPath;
begin
  result.Init(Doc);
end;

class function TXPath.Create(Doc: TXMLDocument): TXPath;
begin
  result.Init(Doc);
end;

procedure TXPath.Init;
var
  Doc: TXMLDocument;
begin
  Doc := TXMLDocument.Create(nil);

  (*
    Following XPath query works with OmniXML, but fails with ADOM (tested with Delphi 10.2):
     XPath: /Project/PropertyGroup[DCC_MapFile='3']   - works
  *)
  Doc.DOMVendor := DOMVendors.Find(sOmniXmlVendor);
  //Doc.DOMVendor := DOMVendors.Find(sAdom4XmlVendor);

  Init(Doc);
end;

procedure TXPath.Init(Doc: IXMLDocument);
begin
  FDoc := Doc;
end;

procedure TXPath.Init(Doc: TXMLDocument);
begin
  FDoc := Doc;
end;

class operator TXPath.Explicit(const a: IXMLDocument): TXPath;
begin
  result.Init(a);
end;

class operator TXPath.Explicit(const a: TXMLDocument): TXPath;
begin
  result.Init(a);
end;

class operator TXPath.Implicit(const a: IXMLDocument): TXPath;
begin
  result.Init(a);
end;

class operator TXPath.Implicit(const a: TXMLDocument): TXPath;
begin
  result.Init(a);
end;

function TXPath.Find(Where: IDomNode; const XPath: string): boolean;
var
  Res: IDomNode;
begin
  result := Find(Where, XPath, Res);
end;

function TXPath.FindSelect(Node: IDomNode): IDomNodeSelect;
begin
  if not Supports(Node, IDomNodeSelect, result) then
    raise Exception.Create('IDomNodeSelect is not supported');
end;

function TXPath.Find(Where: IDomNode; const XPath: string; out Res: IDOMNodeList): boolean;
var
  Select: IDomNodeSelect;
begin
  Res := nil;
  Result := False;
  if not Assigned(Where) then
    Exit;
  Select := FindSelect(Where);
  Res    := Select.selectNodes(XPath);
  Result := Assigned(Res);
end;

function TXPath.Find(Where: IDomNode; const XPath: string; out Res: IDomNode): boolean;
var
  Select: IDomNodeSelect;
begin
  Res := nil;
  Result := False;
  if not Assigned(Where) then
    Exit;
  Select := FindSelect(Where);
  Res    := Select.selectNode(XPath);
  Result := Assigned(Res);
end;

function TXPath.Find(const XPath: string): boolean;
begin
  result := Find(FDoc.DocumentElement.DOMNode, XPath);
end;

function TXPath.Find(const XPath: string; out Res: IDOMNodeList): boolean;
begin
  result := Find(FDoc.DocumentElement.DOMNode, XPath, Res);
end;

function TXPath.Find(const XPath: string; out Res: IDomNode): boolean;
begin
  result := Find(FDoc.DocumentElement.DOMNode, XPath, Res);
end;

end.
