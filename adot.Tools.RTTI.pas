unit adot.Tools.RTTI;

interface

uses
  System.Rtti,
  System.TypInfo,
  System.Math,
  System.Classes,
  System.SysUtils;

type
  TRttiUtils = class
  public

    { Returns True if T is class (instance can be created) }
    class function IsInstance<T>: boolean; static;

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
  end;

  { Simple convertion EnumType->string->EnumType etc.
    http://stackoverflow.com/questions/31601707/generic-functions-for-converting-an-enumeration-to-string-and-back#31604647 }
  TEnumeration<T: record> = record
  strict private
    class function TypeInfo: PTypeInfo; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function TypeData: PTypeData; {$IFNDEF DEBUG}inline;{$ENDIF} static;
  public
    class function IsEnumeration: Boolean; static;
    class function ToOrdinal(Enum: T): Integer; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function FromOrdinal(Value: Integer): T; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function ToString(Enum: T): string; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function FromString(const S: string): T; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function MinValue: Integer; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function MaxValue: Integer; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function InRange(Value: Integer): Boolean; {$IFNDEF DEBUG}inline;{$ENDIF} static;
    class function EnsureRange(Value: Integer): Integer; {$IFNDEF DEBUG}inline;{$ENDIF} static;
  end;

implementation

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

class function TRttiUtils.IsInstance<T>: boolean;
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
begin
  RttiType := RttiContext.GetType(TypeInfo(T));
  result := (RttiType<>nil) and RttiType.IsInstance;
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
