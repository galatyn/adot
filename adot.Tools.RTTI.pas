unit adot.Tools.Rtti;

{ Definition of classes/record types:

  TEnumeration<T: record> = record
    Conversion between enumeration type, ordinal value and string value.

  TRttiUtils = class
    IsInstance<T>, ValueAsString<T>, CreateInstance<T: class> etc.

}
interface

uses
  System.Rtti,
  System.TypInfo,
  System.Math,
  System.Classes,
  System.SysUtils;

type
  TConvOption = (
    coReadable,
    coStreamAsString
  );
  TConvOptions = set of TConvOption;

const
  defConvOptions = [coReadable, coStreamAsString];

type
  TConvSetings = record
    Options: TConvOptions;
    MaxArrayCount: integer;
    MaxDepth: integer;

    procedure Init;
  end;

  { IsInstance<T>, ValueAsString<T>, CreateInstance<T: class> etc }
  TRttiUtils = class
  private
  public

    { Returns True if T is class (instance can be created) }
    class function IsInstance<T>: boolean; static;
    class function IsOrdinal<T>: boolean; static;

    { Convert value of type T to string }
    class function ValueAsString<T>(const Value: T): string; static;

    { Find latest public constructor in hierarchy and create instance/object of the class. Supports:
        Create()
        Create(ACapacity: integer)
        Create(Capacity: integer)
        Create(AOwner: TComponent)
        Create(Owner: TComponent) }
    class function CreateInstance<T: class>: T; static;

    class function FromVariant<T>(const Src: Variant): T; static;

    { can be used for access to private fields }
    class function GetFieldValue<T>(Obj: TObject; const AFieldName: string): T; static;
    class procedure SetFieldValue<T>(Obj: TObject; const AFieldName: string; const Value: T); static;

    { returns object details in readable JSON (for log etc) }
    class function ObjectAsJson(Src: TObject): string; overload; static;
    class function ObjectAsJson(Src: TObject; const Settings: TConvSetings): string; overload; static;
  end;

  { Simple convertion EnumType->string->EnumType etc.
    http://stackoverflow.com/questions/31601707/generic-functions-for-converting-an-enumeration-to-string-and-back#31604647 }
  { Conversion between enumeration type, ordinal value and string value }	
  TEnumeration<T: record> = record
  strict private
    class function TypeInfo: PTypeInfo; static;
    class function TypeData: PTypeData; static;
  public
    class function IsEnumeration: Boolean; static;
    class function ToOrdinal(Enum: T): Integer; static;
    class function FromOrdinal(Value: Integer): T; static;
    class function ToString(Enum: T): string; static;
    class function FromString(const S: string): T; static;
    class function MinValue: Integer; static;
    class function MaxValue: Integer; static;
    class function InRange(Value: Integer): Boolean; static;
    class function EnsureRange(Value: Integer): Integer; static;
  end;

implementation

uses
  adot.Tools,
  adot.Strings,
  adot.JSON.JBuilder;

type
  TObjectToJson = class
  private
    Settings: TConvSetings;
    Dst: TJBuilder;

    procedure ObjectToJson(const PropName: string; Src: TObject; Depth: integer);
    procedure ValueToJson(const PropName: string; var Value: TValue; Depth: integer);
    procedure StreamToJson(const PropName: string; Stream: TStream);

  public
    constructor Create(const ASettings: TConvSetings); overload;
    constructor Create; overload;
    function Get(Src: TObject): string;
  end;

{ TObjectToJson }

constructor TObjectToJson.Create(const ASettings: TConvSetings);
begin
  Settings := ASettings;
end;

constructor TObjectToJson.Create;
var Settings: TConvSetings;
begin
  Settings.Init;
  Create(Settings);
end;

procedure TObjectToJson.StreamToJson(const PropName: string; Stream: TStream);
var
  P: Int64;
  B: TArray<byte>;
  S: string;
  I: Integer;
  AsText: Boolean;
begin

  { read as bytes }
  P := Stream.Position;
  Stream.Position := 0;
  SetLength(B, Stream.Size);
  Stream.ReadBuffer(B, Stream.Size);
  Stream.Position := P;

  { convert to text }
  AsText := (coStreamAsString in Settings.Options);
  if AsText and TStr.IsValidUtf8(B) then
  try
    S := TEncoding.UTF8.GetString(B);
  except
    AsText := False;
  end;

  { write to dst }
  if AsText then
  begin
    if PropName=''
      then Dst.Add(S)
      else Dst.Add(PropName, S)
  end
  else
  begin
    if PropName=''
      then Dst.BeginArray
      else Dst.BeginArray(PropName);
    for I := 0 to Min(Length(B), Settings.MaxArrayCount)-1 do
      Dst.Add(B[I]);
    if Length(B) > Settings.MaxArrayCount then
      Dst.Add('<...>');
    Dst.EndArray;
  end;
end;

procedure TObjectToJson.ValueToJson(const PropName: string; var Value: TValue; Depth: integer);
var
  I: Integer;
  V: TValue;
  Obj: TObject;
begin
  case Value.Kind of
    tkClass:
      begin
        Obj := Value.AsObject;
        if Obj is TStream then
        begin
          if PropName <> '' then
            Dst.BeginObject(PropName);
          StreamToJson(Value.ToString, TStream(Obj));
          if PropName <> '' then
            Dst.EndObject;
          Exit;
        end
        else
        if Depth > 1 then
        begin
          ObjectToJson(PropName, Obj, Depth-1);
          Exit;
        end;
      end;

    tkDynArray:
      begin
        if PropName = ''
          then Dst.BeginArray
          else Dst.BeginArray(PropName);
        for I := 0 to Min(Value.GetArrayLength, Settings.MaxArrayCount)-1 do
        begin
          V := Value.GetArrayElement(I);
          ValueToJson('', V, Depth-1);
        end;
        if Value.GetArrayLength > Settings.MaxArrayCount then
          Dst.Add('<...>');
        Dst.EndArray;
        Exit;
      end;

  end;

  if PropName = ''
    then Dst.Add(Value.ToString)
    else Dst.Add(PropName, Value.ToString)
end;

procedure TObjectToJson.ObjectToJson(const PropName: string; Src: TObject; Depth: integer);
var
  RttiType: TRttiType;
  RttiContext: TRttiContext;
  RttiProp: TRttiProperty;
  RttiValue: TValue;
  S: string;
begin
  S := TRttiUtils.ValueAsString<TObject>(Src);
  if Src = nil
    then S := 'nil'
    else S := TRttiUtils.ValueAsString<TObject>(Src);
  if (Depth <= 0) or (Src = nil) then
  begin
    if PropName = ''
      then Dst.Add(S)
      else Dst.Add(PropName, S);
    Exit;
  end;

  if PropName = '' then
    Dst.BeginObject(S)
  else
  begin
    Dst.BeginObject(PropName);
    Dst.BeginObject(S);
  end;
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(Src.ClassType);
  for RttiProp in RttiType.GetProperties do
  begin
    if not RttiProp.IsReadable then
      Continue;
    RttiType := RttiProp.PropertyType;
    if not RttiType.IsPublicType then
      Continue;
    RttiValue := RttiProp.GetValue(Src);
    ValueToJson(RttiProp.Name, RttiValue, Depth-1);
  end;
  Dst.EndObject;
  if PropName <> '' then
    Dst.EndObject;
end;

function TObjectToJson.Get(Src: TObject): string;
begin
  Dst.Init;
  ObjectToJson('', Src, Settings.MaxDepth);
  result := Dst.ToString;
end;

{ TConvSetings }

procedure TConvSetings.Init;
begin
  Self := Default(TConvSetings);
  Options := defConvOptions;
  MaxArrayCount := High(MaxArrayCount);
  MaxDepth := 10*1024*1024;
end;

{ TRttiUtils }

class function TRttiUtils.CreateInstance<T>: T;
var
  TypInf: PTypeInfo;
  Value: TValue;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiInstanceType: TRttiInstanceType;
  Method: TRttiMethod;
  Params: TArray<TRttiParameter>;
begin
  TypInf := TypeInfo(T);

  {  We use more general solution for "Create(AOwner: TComponent)", but
    for descendants of TComponent we can avoid extra checks. }
  if TypInf.TypeData.ClassType.InheritsFrom(TComponent) then
    result := T(TComponentClass(TypInf.TypeData.ClassType).Create(nil));
  begin
    RttiContext := TRttiContext.Create;
    RttiType := RttiContext.GetType(TypeInfo(T));
    if not RttiType.IsInstance then
      Exit;
    for Method in RttiType.GetMethods do
      if Method.IsConstructor and SameText(Method.Name, 'Create') then
      begin
        Params := Method.GetParameters;

        { "Create()". }
        if Length(Params)=0 then
        begin
          RttiInstanceType := RttiType.AsInstance;
          Value := Method.Invoke(RttiInstanceType.MetaclassType, []);
          Result := Value.AsType<T>;
          Exit;
        end;

        // "Create(ACapacity: integer = 0)".
        // There is no way to check default value, but usually such constructor
        // hides "Create()", for example:
        // "TDictionary<byte,byte>.Create" means "Create(ACapacity = 0)".
        if (Length(Params)=1) and
          ((Params[0].Flags=[]) or (Params[0].Flags=[TParamFlag.pfConst])) and
          (Params[0].ParamType<>nil) and (Params[0].ParamType.TypeKind in [tkInteger, tkInt64]) and
          (AnsiSameText(Params[0].Name, 'ACapacity') or AnsiSameText(Params[0].Name, 'Capacity'))
        then
        begin
          RttiInstanceType := RttiType.AsInstance;
          Value := Method.Invoke(RttiInstanceType.MetaclassType, [0]);
          Result := Value.AsType<T>;
          Exit;
        end;

        // "Create(AOwner: TComponent)".
        if (Length(Params)=1) and
          (Params[0].Flags=[TParamFlag.pfAddress]) and
          (Params[0].ParamType<>nil) and (Params[0].ParamType.TypeKind in [tkClass]) and
          (AnsiSameText(Params[0].Name, 'AOwner') or AnsiSameText(Params[0].Name, 'Owner'))
        then
        begin
          RttiInstanceType := RttiType.AsInstance;
          Value := Method.Invoke(RttiInstanceType.MetaclassType, [nil]);
          Result := Value.AsType<T>;
          Exit;
        end;

      end; // For Method in RttiType.GetMethods
  end; // If

  // Should never happend, because TObject has "Create()".
  raise Exception.Create('Default constructor is not found');
end;

class function TRttiUtils.FromVariant<T>(const Src: Variant): T;
begin
  result := TValue.FromVariant(Src).AsType<T>;
end;

class function TRttiUtils.GetFieldValue<T>(Obj: TObject; const AFieldName: string): T;
var
  RttiType    : TRttiType;
  RttiContext : TRttiContext;
  RttiField   : TRttiField;
  Ptr         : ^T;
begin
  RttiType := RttiContext.GetType(Obj.ClassType);
  Assert(Assigned(RttiType));
  for RttiField in RttiType.GetFields do
    if AnsiLowerCase(AFieldName)=AnsiLowerCase(RttiField.Name) then
    begin
      Ptr := Pointer(PByte(Obj) + RttiField.Offset);
      result := Ptr^;
      Exit;
    end;
  raise Exception.Create(format('Field "%s" is not found (%s)', [AFieldName, Obj.ClassName]));
end;

class procedure TRttiUtils.SetFieldValue<T>(Obj: TObject; const AFieldName: string; const Value: T);
var
  RttiType    : TRttiType;
  RttiContext : TRttiContext;
  RttiField   : TRttiField;
  Ptr         : ^T;
begin
  RttiType := RttiContext.GetType(Obj.ClassType);
  Assert(Assigned(RttiType));
  for RttiField in RttiType.GetFields do
    if AnsiLowerCase(AFieldName)=AnsiLowerCase(RttiField.Name) then
    begin
      Ptr := Pointer(PByte(Obj) + RttiField.Offset);
      Ptr^ := Value;
      Exit;
    end;
  raise Exception.Create(format('Field "%s" is not found (%s)', [AFieldName, Obj.ClassName]));
end;

class function TRttiUtils.IsInstance<T>: boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfo(T));
  result := (RttiType<>nil) and RttiType.IsInstance;
end;

class function TRttiUtils.IsOrdinal<T>: boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
begin
  RttiContext := TRttiContext.Create;
  RttiType := RttiContext.GetType(TypeInfo(T));
  result := (RttiType<>nil) and RttiType.IsOrdinal;
end;

class function TRttiUtils.ObjectAsJson(Src: TObject; const Settings: TConvSetings): string;
var
  C: TObjectToJson;
begin
  C := TObjectToJson.Create(Settings);
  try
    result := C.Get(Src);
  finally
    C.Free;
  end;
end;

class function TRttiUtils.ObjectAsJson(Src: TObject): string;
var
  Settings: TConvSetings;
begin
  Settings.Init;
  result := ObjectAsJson(Src, Settings);
end;

class function TRttiUtils.ValueAsString<T>(const Value: T): string;
begin
  result := TValue.From<T>(Value).ToString;
end;

{ TEnumeration<T> }

class function TEnumeration<T>.TypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(T);
end;

class function TEnumeration<T>.TypeData: PTypeData;
begin
  Result := System.TypInfo.GetTypeData(TypeInfo);
end;

class function TEnumeration<T>.IsEnumeration: Boolean;
begin
  Result := TypeInfo.Kind=tkEnumeration;
end;

class function TEnumeration<T>.ToOrdinal(Enum: T): Integer;
begin
  Assert(IsEnumeration);
  Assert(SizeOf(Enum)<=SizeOf(Result));
  Result := 0; // needed when SizeOf(Enum) < SizeOf(Result)
  Move(Enum, Result, SizeOf(Enum));
  Assert(InRange(Result));
end;

class function TEnumeration<T>.FromOrdinal(Value: Integer): T;
begin
  Assert(IsEnumeration);
  Assert(InRange(Value));
  Assert(SizeOf(Result)<=SizeOf(Value));
  Move(Value, Result, SizeOf(Result));
end;

class function TEnumeration<T>.ToString(Enum: T): string;
begin
  Result := GetEnumName(TypeInfo, ToOrdinal(Enum));
end;

class function TEnumeration<T>.FromString(const S: string): T;
begin
  Result := FromOrdinal(GetEnumValue(TypeInfo, S));
end;

class function TEnumeration<T>.MinValue: Integer;
begin
  Assert(IsEnumeration);
  Result := TypeData.MinValue;
end;

class function TEnumeration<T>.MaxValue: Integer;
begin
  Assert(IsEnumeration);
  Result := TypeData.MaxValue;
end;

class function TEnumeration<T>.InRange(Value: Integer): Boolean;
var
  ptd: PTypeData;
begin
  Assert(IsEnumeration);
  ptd := TypeData;
  Result := System.Math.InRange(Value, ptd.MinValue, ptd.MaxValue);
end;

class function TEnumeration<T>.EnsureRange(Value: Integer): Integer;
var
  ptd: PTypeData;
begin
  Assert(IsEnumeration);
  ptd := TypeData;
  Result := System.Math.EnsureRange(Value, ptd.MinValue, ptd.MaxValue);
end;

end.
