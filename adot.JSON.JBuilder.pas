unit adot.JSON.JBuilder;

interface

uses
  Adot.Collections,
  Adot.Tools,
  System.Classes,
  System.SysUtils,
  System.JSON.Types,
  System.JSON.Builders,
  System.JSON.Writers;

type
  (*
    var
      Builder: TJBuilder;
    begin
      Builder.Init
        .BeginObject
          .BeginArray('Transaction')
            .BeginObject
              .Add('id', 662713)
              .Add('firstName', 'John')
              .Add('lastName', 'Doe')
              .Add('price', 2.1)
              .AddNull('parent_id')
              .Add('validated', true)
            .EndObject
            .BeginObject
              .Add('id', 662715)
              .Add('firstName', 'Peter')
              .Add('lastName', 'Jones')
              .Add('price', 3.6)
              .AddNull('parent_id')
              .Add('validated', true)
            .EndObject
          .EndArray
        .EndObject;
      Result := Builder.ToString;
    end;
  *)
  TJBuilder = record
  private
    FAutoFreeCollection : TAutoFreeCollection;
    FStringBuilder      : TStringBuilder;
    FStringWriter       : TStringWriter;
    FJsonTextWriter     : TJsonTextWriter;
    FBuilder            : TJSONObjectBuilder;

    function GetText: string;

  public
    function Init: TJSONObjectBuilder;

    property Builder: TJSONObjectBuilder read FBuilder;
    property ToString: string read GetText;
  end;

implementation

{ TJBuilder }

function TJBuilder.Init: TJSONObjectBuilder;
begin
  FAutoFreeCollection.Init;
  FStringBuilder             := FAutoFreeCollection.Add( TStringBuilder.Create );
  FStringWriter              := FAutoFreeCollection.Add( TStringWriter.Create(FStringBuilder) );
  FJsonTextWriter            := FAutoFreeCollection.Add( TJsonTextWriter.Create(FStringWriter) );
  FJsonTextWriter.Formatting := TJsonFormatting.Indented;
  FBuilder                   := FAutoFreeCollection.Add( TJSONObjectBuilder.Create(FJsonTextWriter) );
  result                     := Builder;
end;

function TJBuilder.GetText: string;
begin
  FJsonTextWriter.Flush;
  FStringWriter.Flush;
  result := FStringBuilder.ToString;
end;

end.
