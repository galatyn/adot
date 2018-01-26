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
    class function ObjectAsJson(Src: TObject): string; static;
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
  adot.JSON.JBuilder;

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

procedure AddRttiValueAsJson(
  const PropName  : string;
  const Kind      : TTypeKind;
    var RttiValue : TValue;
    var Dst       : TJBuilder);
var
  I: Integer;
  Value: TValue;
begin
  //RttiDynArrayType := TRttiDynamicArrayType(RttiType);
  case Kind of
    tkDynArray:
      begin
        if PropName = ''
          then Dst.BeginArray
          else Dst.BeginArray(PropName);
        for I := 0 to RttiValue.GetArrayLength-1 do
        begin
          Value := RttiValue.GetArrayElement(I);
          AddRttiValueAsJson('', Value.Kind, Value, Dst);
        end;
        Dst.EndArray;
      end;
    else
      if PropName = ''
        then Dst.Add(RttiValue.ToString)
        else Dst.Add(PropName, RttiValue.ToString)
  end;
end;

class function TRttiUtils.ObjectAsJson(Src: TObject): string;
var
  Dst: TJBuilder;
  RttiType: TRttiType;
  RttiContext: TRttiContext;
  RttiProp: TRttiProperty;
  RttiValue: TValue;
begin
  Dst.Init;
  Dst.BeginObject;
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
    AddRttiValueAsJson(RttiProp.Name, RttiType.TypeKind, RttiValue, Dst);
  end;
  Dst.EndObject;
  result := Dst.ToString;
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
