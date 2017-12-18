unit adot.JSON.JBuilder;

interface

uses
  Adot.Collections,
  System.Classes,
  System.SysUtils,
  System.JSON.Types,
  System.JSON.Writers,
  System.Generics.Collections;

type
  (*
    Delphi has 3 frameworks for JSON:
      1. Objects
        + max flexibility
        - lot of variables, code is not as readable as it can be
      2. Readers and Writers (TJSonWriter)
        + almost as powerful/flexible as object model
        + cleaner code than in object model
        - adding of items to object (map) requires two commands
        - not as readable as builder
      3. Builder (TJSONObjectBuilder)
        + great for simple structures / very readable
        - recursive syntax
        - when some parts should be generated in cycles/by ext routines,
          it became tricky & code is not as readable as it should

    In TJBuilder we tried to collect the best from all:
      - simple & readable
      - flexible as writer
      - as it is a record type, no need to use Try-Finally & free manually

    Example:
    var
      B: TJBuilder;
    begin
      B.Init;
      B.BeginObject;
        B.BeginArray('Transaction');
          B.BeginObject;
            B.Add('id', 662713);
            B.Add('firstName', 'John');
            B.Add('lastName', 'Doe');
            B.Add('price', 2.1);
            B.AddNull('parent_id');
            B.Add('validated', true);
          B.EndObject;
          B.BeginObject;
            B.AddArray('A', [1,2,3]);
            B.AddArray('B', ['Ver','2.2.1']);
          B.EndObject;
        B.EndArray;
      B.EndObject;
      Result := B.ToString;
    end;
  *)
  TJBuilder = record
  private
    FJsonWriter: TJsonTextWriter;
    FTextWriter: TTextWriter;
    FAutoFreeCollection: TAutoFreeCollection;

  public
    procedure Init; overload;
    procedure Init(const TextWriter: TTextWriter; TakeOwnership: boolean = False); overload;

    class function Create: TJBuilder; overload; static;
    class function Create(const TextWriter: TTextWriter; TakeOwnership: boolean = False): TJBuilder; overload; static;

    { add value }
    procedure BeginObject; overload;
    procedure BeginArray; overload;
    procedure AddNull; overload;
    procedure Add(const Value: string); overload;
    procedure Add(Value: Integer); overload;
    procedure Add(Value: UInt32); overload;
    procedure Add(Value: Int64); overload;
    procedure Add(Value: UInt64); overload;
    procedure Add(Value: Single); overload;
    procedure Add(Value: Double); overload;
    procedure Add(Value: Extended); overload;
    procedure Add(Value: Boolean); overload;
    procedure Add(Value: Char); overload;
    procedure Add(Value: Byte); overload;
    procedure Add(Value: TDateTime); overload;
    procedure Add(const Value: TGUID); overload;
    procedure Add(const Value: TBytes; BinaryType: TJsonBinaryType = TJsonBinaryType.Generic); overload;
    procedure Add(const Value: TJsonOid); overload;
    procedure Add(const Value: TJsonRegEx); overload;
    procedure Add(const Value: TJsonDBRef); overload;
    procedure Add(const Value: TJsonCodeWScope); overload;

    { add to object }
    procedure BeginObject(const Name: string); overload;
    procedure BeginArray(const Name: string); overload;
    procedure AddNull(const Name: string); overload;
    procedure Add(const Name: string; const Value: string); overload;
    procedure Add(const Name: string; Value: Integer); overload;
    procedure Add(const Name: string; Value: UInt32); overload;
    procedure Add(const Name: string; Value: Int64); overload;
    procedure Add(const Name: string; Value: UInt64); overload;
    procedure Add(const Name: string; Value: Single); overload;
    procedure Add(const Name: string; Value: Double); overload;
    procedure Add(const Name: string; Value: Extended); overload;
    procedure Add(const Name: string; Value: Boolean); overload;
    procedure Add(const Name: string; Value: Char); overload;
    procedure Add(const Name: string; Value: Byte); overload;
    procedure Add(const Name: string; Value: TDateTime); overload;
    procedure Add(const Name: string; const Value: TGUID); overload;
    procedure Add(const Name: string; const Value: TBytes; BinaryType: TJsonBinaryType = TJsonBinaryType.Generic); overload;
    procedure Add(const Name: string; const Value: TJsonOid); overload;
    procedure Add(const Name: string; const Value: TJsonRegEx); overload;
    procedure Add(const Name: string; const Value: TJsonDBRef); overload;
    procedure Add(const Name: string; const Value: TJsonCodeWScope); overload;

    procedure EndObject; overload;
    procedure EndArray; overload;

    { higher level }
    { add array }
    procedure AddArray(const Values: TEnumerable<Integer>); overload;
    procedure AddArray(const Values: TEnumerable<Double>); overload;
    procedure AddArray(const Values: TEnumerable<String>); overload;
    procedure AddArray(const Values: TArray<Integer>); overload;
    procedure AddArray(const Values: TArray<Double>); overload;
    procedure AddArray(const Values: TArray<String>); overload;
    procedure AddArray(const Name: string; const Values: TEnumerable<Integer>); overload;
    procedure AddArray(const Name: string; const Values: TEnumerable<Double>); overload;
    procedure AddArray(const Name: string; const Values: TEnumerable<String>); overload;
    procedure AddArray(const Name: string; const Values: TArray<Integer>); overload;
    procedure AddArray(const Name: string; const Values: TArray<Double>); overload;
    procedure AddArray(const Name: string; const Values: TArray<String>); overload;
    { add object }
    procedure AddObject(const Values: TEnumerable<TPair<String,Integer>>); overload;
    procedure AddObject(const Values: TEnumerable<TPair<String,Double>>); overload;
    procedure AddObject(const Values: TEnumerable<TPair<String,String>>); overload;
    procedure AddObject(const Name: string; const Values: TEnumerable<TPair<String,Integer>>); overload;
    procedure AddObject(const Name: string; const Values: TEnumerable<TPair<String,Double>>); overload;
    procedure AddObject(const Name: string; const Values: TEnumerable<TPair<String,String>>); overload;

    function ToString: string;

    class operator Implicit(const B: TJBuilder): string;
    class operator Explicit(const B: TJBuilder): string;
  end;

implementation

{ TJBuilder }

procedure TJBuilder.Init;
begin
  Init(TStringWriter.Create, True);
end;

procedure TJBuilder.Init(const TextWriter: TTextWriter; TakeOwnership: boolean);
begin

  { destroy old instance }
  FAutoFreeCollection.Init;
  FTextWriter := nil;
  FJsonWriter := nil;

  { create new instance }
  if TakeOwnership
    then FTextWriter := FAutoFreeCollection.Add( TextWriter )
    else FTextWriter := TextWriter;
  FJsonWriter := FAutoFreeCollection.Add( TJsonTextWriter.Create(FTextWriter) );
end;

class function TJBuilder.Create: TJBuilder;
begin
  result.Init;
end;

class function TJBuilder.Create(const TextWriter: TTextWriter; TakeOwnership: boolean): TJBuilder;
begin
  result.Init(TextWriter, TakeOwnership);
end;

procedure TJBuilder.EndArray;
begin
  FJsonWriter.WriteEndArray;
end;

procedure TJBuilder.EndObject;
begin
  FJsonWriter.WriteEndObject;
end;

class operator TJBuilder.Explicit(const B: TJBuilder): string;
begin
  result := B.ToString;
end;

class operator TJBuilder.Implicit(const B: TJBuilder): string;
begin
  result := B.ToString;
end;

function TJBuilder.ToString: string;
begin
  FJsonWriter.Flush;
  FTextWriter.Flush;
  result := FTextWriter.ToString;
end;

procedure TJBuilder.BeginArray;
begin
  FJsonWriter.WriteStartArray;
end;

procedure TJBuilder.BeginObject(const Name: string);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteStartObject;
end;

procedure TJBuilder.BeginArray(const Name: string);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteStartArray;
end;

procedure TJBuilder.BeginObject;
begin
  FJsonWriter.WriteStartObject;
end;

procedure TJBuilder.Add(const Name: string; Value: Single);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: UInt64);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: Extended);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: Double);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: Integer);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name, Value: string);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: Int64);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: UInt32);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: Boolean);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; const Value: TJsonRegEx);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; const Value: TJsonOid);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.AddNull(const Name: string);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteNull;
end;

procedure TJBuilder.Add(const Name: string; const Value: TJsonCodeWScope);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; const Value: TJsonDBRef);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; const Value: TBytes; BinaryType: TJsonBinaryType);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: Byte);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: Char);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; const Value: TGUID);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Name: string; Value: TDateTime);
begin
  FJsonWriter.WritePropertyName(Name);
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Double);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Single);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Boolean);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Extended);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: UInt64);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Integer);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Value: string);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Int64);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: UInt32);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Char);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Value: TJsonRegEx);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Value: TJsonOid);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Value: TJsonCodeWScope);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Value: TJsonDBRef);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: TDateTime);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(Value: Byte);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Value: TBytes; BinaryType: TJsonBinaryType);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.Add(const Value: TGUID);
begin
  FJsonWriter.WriteValue(Value);
end;

procedure TJBuilder.AddNull;
begin
  FJsonWriter.WriteNull;
end;

procedure TJBuilder.AddArray(const Values: TEnumerable<String>);
var
  Value: String;
begin
  FJsonWriter.WriteStartArray;
  for Value in Values do
    FJsonWriter.WriteValue(Value);
  FJsonWriter.WriteEndArray;
end;

procedure TJBuilder.AddArray(const Values: TEnumerable<Double>);
var
  Value: Double;
begin
  FJsonWriter.WriteStartArray;
  for Value in Values do
    FJsonWriter.WriteValue(Value);
  FJsonWriter.WriteEndArray;
end;

procedure TJBuilder.AddArray(const Values: TEnumerable<Integer>);
var
  Value: Integer;
begin
  FJsonWriter.WriteStartArray;
  for Value in Values do
    FJsonWriter.WriteValue(Value);
  FJsonWriter.WriteEndArray;
end;

procedure TJBuilder.AddArray(const Values: TArray<Integer>);
var
  Value: Integer;
begin
  FJsonWriter.WriteStartArray;
  for Value in Values do
    FJsonWriter.WriteValue(Value);
  FJsonWriter.WriteEndArray;
end;

procedure TJBuilder.AddArray(const Values: TArray<Double>);
var
  Value: Double;
begin
  FJsonWriter.WriteStartArray;
  for Value in Values do
    FJsonWriter.WriteValue(Value);
  FJsonWriter.WriteEndArray;
end;

procedure TJBuilder.AddArray(const Values: TArray<String>);
var
  Value: String;
begin
  FJsonWriter.WriteStartArray;
  for Value in Values do
    FJsonWriter.WriteValue(Value);
  FJsonWriter.WriteEndArray;
end;

procedure TJBuilder.AddArray(const Name: string; const Values: TEnumerable<Integer>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddArray(Values);
end;

procedure TJBuilder.AddArray(const Name: string; const Values: TEnumerable<Double>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddArray(Values);
end;

procedure TJBuilder.AddArray(const Name: string; const Values: TEnumerable<String>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddArray(Values);
end;

procedure TJBuilder.AddArray(const Name: string; const Values: TArray<Integer>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddArray(Values);
end;

procedure TJBuilder.AddArray(const Name: string; const Values: TArray<Double>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddArray(Values);
end;

procedure TJBuilder.AddArray(const Name: string; const Values: TArray<String>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddArray(Values);
end;

procedure TJBuilder.AddObject(const Values: TEnumerable<TPair<String, String>>);
var
  Value: TPair<String, String>;
begin
  FJsonWriter.WriteStartObject;
  for Value in Values do
  begin
    FJsonWriter.WritePropertyName(Value.Key);
    FJsonWriter.WriteValue(Value.Value);
  end;
  FJsonWriter.WriteEndObject;
end;

procedure TJBuilder.AddObject(const Values: TEnumerable<TPair<String, Double>>);
var
  Value: TPair<String, Double>;
begin
  FJsonWriter.WriteStartObject;
  for Value in Values do
  begin
    FJsonWriter.WritePropertyName(Value.Key);
    FJsonWriter.WriteValue(Value.Value);
  end;
  FJsonWriter.WriteEndObject;
end;

procedure TJBuilder.AddObject(const Values: TEnumerable<TPair<String, Integer>>);
var
  Value: TPair<String, Integer>;
begin
  FJsonWriter.WriteStartObject;
  for Value in Values do
  begin
    FJsonWriter.WritePropertyName(Value.Key);
    FJsonWriter.WriteValue(Value.Value);
  end;
  FJsonWriter.WriteEndObject;
end;

procedure TJBuilder.AddObject(const Name: string; const Values: TEnumerable<TPair<String, Integer>>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddObject(Values);
end;

procedure TJBuilder.AddObject(const Name: string; const Values: TEnumerable<TPair<String, Double>>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddObject(Values);
end;

procedure TJBuilder.AddObject(const Name: string; const Values: TEnumerable<TPair<String, String>>);
begin
  FJsonWriter.WritePropertyName(Name);
  AddObject(Values);
end;

end.
