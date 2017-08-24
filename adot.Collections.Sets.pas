unit adot.Collections.Sets;

interface

{
  TSetClass<T>
  TSet<T>
}

uses
  adot.Types,
  adot.Collections.Types,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.StrUtils,
  System.SysUtils;

type
  TSetOp = (soUnion, soIntersection, soDifference, soSymmetricDifference);

  { Generic class for set. }
  TSetClass<T> = class(TEnumerableExt<T>)
  public
    type
      TEnumerator = TDictionary<T, TEmptyRec>.TKeyEnumerator;

      TSetObjectDictionary<TSetDictKey,TSetDictValue> = class(TDictionary<TSetDictKey,TSetDictValue>)
      protected
        OwnsKeys: boolean;

        procedure KeyNotify(const Key: TSetDictKey; Action: TCollectionNotification); override;
      end;

  protected
    var
      FSet: TSetObjectDictionary<T, TEmptyRec>;
      FComparerCopy: IEqualityComparer<T>; { FSet.Comparer is hidden in private section, so we keep copy }

    function GetCount: integer;
    function DoGetEnumerator: TEnumerator<T>; override;
    function GetComparer: IEqualityComparer<T>;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetOwnsValues: boolean;
    function GetAsArray: TArray<T>;
    function GetAsString: string;

  public
    constructor Create(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AValues: array of T; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AValues: TEnumerable<T>; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AOperands: TArray<TSetClass<T>>; ASetOp: TSetOp; AComparer: IEqualityComparer<T> = nil); overload;

    destructor Destroy; override;
    function GetEnumerator: TEnumerator; reintroduce;
    procedure Add(const AValue: T); overload;
    procedure Add(const ASet: array of T); overload;
    procedure Add(const AValues: TEnumerable<T>); overload;
    procedure IncludeLogicalAnd(const A,B: TSetClass<T>);
    procedure IncludeLogicalOr(const A,B: TSetClass<T>);
    procedure IncludeLogicalXor(const A,B: TSetClass<T>);
    procedure Include(const AValue: T); overload;
    procedure Include(const ASet: array of T); overload;
    procedure Include(const AValues: TEnumerable<T>); overload;
    procedure Remove(const AValue: T); overload;
    procedure Remove(const ASet: array of T); overload;
    procedure Remove(const AValues: TEnumerable<T>); overload;
    function Contains(const AValue: T): boolean; overload;
    function Contains(const ASet: array of T): boolean; overload;
    function Contains(const AValues: TEnumerable<T>): boolean; overload;
    procedure Clear;
    function Empty: Boolean;
    property Count: integer read GetCount;
    property Comparer: IEqualityComparer<T> read GetComparer;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property AsString: string read GetAsString;
    property AsArray: TArray<T> read GetAsArray;
  end;

  {  Example:
      var
        a,b,c: TSet<string>;
        s: string;
      begin
        a := ['Mandag', 'Tirsdag', 'Fredag'];
        b := ['Fredag', 'Lørdag'];
        c := a and b;                           // ['Fredag']
        c := a or b;                            // ['Mandag', 'Tirsdag', 'Fredag', 'Lørdag']
        c := a + b - ['Mandag', 'Tirsdag'];     // ['Fredag', 'Lørdag']
        if a xor b = TSet<string>.Create(['Mandag', 'Tirsdag', 'Lørdag']) then
          [...]
        if (b in a) or ('Fredag' in a) then
          [...]
        if a>b then     // "a" contains all items from "b" and at least one item extra
          [...]
        if a>=b then    // "a" contains all items from "b" and maybe some items extra
          [...]
        for s in c do   // enumerate all values from the set
          [...]
        c := ['En'];
        c.Add(['To', 'Tre']);
        c.Remove('To'); // ['En', 'Tre']
        Assert( ('En' in c) and ('en' in c) ); // default comparer for "string" type is case insensitive
        c := TSet<string>.Create(['En','To'], 0,TStringComparer.Ordinal);
        Assert( ('En' in c) and NOT ('en' in c) ); // now we used case sensitive comparer
      end; }
  { Record type for set. Support operators for all set operations and copy-on-write. }
  TSet<T> = record
  private
    FSetInt: IInterfacedObject<TSetClass<T>>;

    procedure CreateSet(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);

    function GetReadonly: TSetClass<T>;
    function GetReadWrite: TSetClass<T>;
    function GetAsString: string;
    function GetAsArray: TArray<T>;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetCount: integer;
    function GetEmpty: Boolean;
    function GetCollection: TEnumerable<T>;

    property ReadOnly: TSetClass<T> read GetReadonly;
    property ReadWrite: TSetClass<T> read GetReadWrite;
  public
    type
      TEnumerator = TSetClass<T>.TEnumerator;

    { Record type TSet<T> can be used without constructor, use constructor only if you
      need some customization: set Capacity, provide custom comparer etc. }
    constructor Create(ACapacity: integer; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: array of T; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: array of TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(V: TSet<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;

    function GetEnumerator: TEnumerator;

    procedure Add(const V: T); overload;
    procedure Add(const V: array of T); overload;
    procedure Add(const V: TEnumerable<T>); overload;
    procedure Add(const V: array of TEnumerable<T>); overload;
    procedure Add(V: TSet<T>); overload;

    procedure Remove(const V: T); overload;
    procedure Remove(const V: array of T); overload;
    procedure Remove(const V: TEnumerable<T>); overload;
    procedure Remove(const V: array of TEnumerable<T>); overload;
    procedure Remove(V: TSet<T>); overload;

    { It is prefered to use syntax "Item in SomSet" over "SomeSet.Contains(Item)", but in
      rare situations compiler can be confused and then "Contains" method is the only way to go }
    function Contains(const a: T) : Boolean; overload;
    function Contains(const a: TEnumerable<T>) : Boolean; overload;
    function Contains(a: TSet<T>) : Boolean; overload;

    function Copy: TSet<T>;

    procedure Clear;

    class operator In(const a: T; b: TSet<T>) : Boolean;
    class operator In(const a: TEnumerable<T>; b: TSet<T>) : Boolean;
    class operator In(a: TSet<T>; b: TSet<T>) : Boolean;

    class operator Implicit(const a : T) : TSet<T>;
    class operator Implicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Implicit(const a : array of T) : TSet<T>;
    class operator Implicit(const a : TArray<T>) : TSet<T>;

    class operator Explicit(const a : T) : TSet<T>;
    class operator Explicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Explicit(const a : array of T) : TSet<T>;
    class operator Explicit(const a : TArray<T>) : TSet<T>;

    class operator Add(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator Add(a: TSet<T>; const b: T): TSet<T>;
    class operator Add(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
    class operator Add(a: TSet<T>; const b: array of T): TSet<T>;
    class operator Add(const a: T;              b: TSet<T>): TSet<T>;
    class operator Add(const a: array of T;     b: TSet<T>): TSet<T>;
    class operator Add(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;

    class operator Subtract(a: TSet<T>; const b: T): TSet<T>;
    class operator Subtract(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator Subtract(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
    class operator Subtract(a: TSet<T>; const b: array of T): TSet<T>;
    class operator Subtract(const a: T;              b: TSet<T>): TSet<T>;
    class operator Subtract(const a: array of T;     b: TSet<T>): TSet<T>;
    class operator Subtract(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;

    class operator Equal(a: TSet<T>;       b: TSet<T>) : Boolean;
    class operator Equal(a: TSet<T>; const b: TEnumerable<T>) : Boolean;

    class operator NotEqual(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator NotEqual(a: TSet<T>; const b: TEnumerable<T>) : Boolean;

    class operator GreaterThanOrEqual(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator GreaterThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator GreaterThan(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator GreaterThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator LessThan(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator LessThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator LessThanOrEqual(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator LessThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator LogicalAnd(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator LogicalAnd(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;

    class operator LogicalOr(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator LogicalOr(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;

    class operator LogicalXor(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator LogicalXor(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;

    property AsString: string read GetAsString;
    property AsArray: TArray<T> read GetAsArray;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property Empty: Boolean read GetEmpty;
    property Count: integer read GetCount;
    property Collection: TEnumerable<T> read GetCollection;
  end;

implementation

uses
  adot.Tools,
  adot.Strings,
  adot.Tools.RTTI,
  adot.Collections;

{ TSetClass<T>.TSetObjectDictionary<TSetDictKey, TSetDictValue> }

procedure TSetClass<T>.TSetObjectDictionary<TSetDictKey, TSetDictValue>.KeyNotify(
  const Key: TSetDictKey; Action: TCollectionNotification);
begin
  inherited;
  if OwnsKeys and (Action = TCollectionNotification.cnRemoved) then
    PObject(@Key)^.DisposeOf;
end;

{ TSetClass<T> }

constructor TSetClass<T>.Create(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  inherited Create;
  FSet := TSetObjectDictionary<T, TEmptyRec>.Create(ACapacity, AComparer);
  FComparerCopy := AComparer;
end;

constructor TSetClass<T>.Create(const AValues: array of T; AComparer: IEqualityComparer<T> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TSetClass<T>.Create(const AValues: TEnumerable<T>; AComparer: IEqualityComparer<T> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TSetClass<T>.Create(const AOperands: TArray<TSetClass<T>>; ASetOp: TSetOp;
  AComparer: IEqualityComparer<T>);
var
  FoundInAll: Boolean;
  Value: T;
  I,J: Integer;
  Found: Boolean;
begin
  Create(0, AComparer);
  case ASetOp of

    soUnion:
      for I := 0 to High(AOperands) do
        Include(AOperands[I]);

    soIntersection:
      begin
        if Length(AOperands)=0 then
          Exit;
        J := 0;
        for I := 1 to High(AOperands) do
          if AOperands[I].Count<AOperands[J].Count then
            J := I;
        for Value in AOperands[J] do
        begin
          FoundInAll := True;
          for I := 0 to High(AOperands) do
            if (I<>J) and not AOperands[I].Contains(Value) then
            begin
              FoundInAll := False;
              Break;
            end;
          if FoundInAll then
            Add(Value);
        end;
      end;

    soDifference:
      begin
        if Length(AOperands)>0 then
          Add(AOperands[0]);
        for I := 1 to High(AOperands) do
          Remove(AOperands[I]);
      end;

    soSymmetricDifference:
      for i := 0 to High(AOperands) do
        for Value in AOperands[i] do
        begin
          Found := False;
          for j := 0 to High(AOperands) do
            if (i<>j) and AOperands[j].Contains(Value) then
            begin
              Found := True;
              Break;
            end;
          if not Found then
            Add(Value);
        end;
  end;
end;

destructor TSetClass<T>.Destroy;
begin
  Sys.FreeAndNil(FSet);
  inherited;
end;

function TSetClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TSetClass<T>.Empty: Boolean;
begin
  result := FSet.Count=0;
end;

function TSetClass<T>.GetAsArray: TArray<T>;
var
  i: Integer;
  Value: T;
begin
  SetLength(Result, Count);
  i := 0;
  for Value in Self do
  begin
    Result[i] := Value;
    inc(i);
  end;
end;

function TSetClass<T>.GetAsString: string;
var
  Arr: TArray<T>;
  Value: T;
  Buf: TStringBuffer;
begin
  Arr := AsArray;
  TArray.Sort<T>(Arr);
  Buf.Clear;
  for Value in Arr do
    Buf.Write(IfThen(Buf.Empty,'',' ') + TRttiUtils.ValueAsString<T>(Value));
  Result := Buf.Text;
end;

function TSetClass<T>.GetComparer: IEqualityComparer<T>;
begin
  result := FComparerCopy;
end;

function TSetClass<T>.GetCount: integer;
begin
  result := FSet.Count;
end;

function TSetClass<T>.GetEnumerator: TEnumerator;
begin
  result := FSet.Keys.GetEnumerator;
end;

procedure TSetClass<T>.Remove(const ASet: array of T);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Remove(ASet[i]);
end;

procedure TSetClass<T>.Remove(const AValues: TEnumerable<T>);
var
  Item: T;
begin
  for Item in AValues do
    Remove(Item);
end;

function TSetClass<T>.GetOwnsValues: boolean;
begin
  result := FSet.OwnsKeys;
end;

procedure TSetClass<T>.SetOwnsValues(AOwnsValues: boolean);
begin
  if AOwnsValues and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FSet.OwnsKeys := AOwnsValues;
end;

procedure TSetClass<T>.Add(const AValue: T);
var R: TEmptyRec;
begin
  FSet.Add(AValue, R);
end;

procedure TSetClass<T>.Add(const ASet: array of T);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Add(ASet[i]);
end;

procedure TSetClass<T>.Add(const AValues: TEnumerable<T>);
var
  Item: T;
begin
  for Item in AValues do
    Add(Item);
end;

procedure TSetClass<T>.IncludeLogicalAnd(const A, B: TSetClass<T>);
var
  Value: T;
begin
  if A.Count<=B.Count then
  begin
    for Value in A do
      if B.Contains(Value) then
        Include(Value);
  end
  else
  begin
    for Value in B do
      if A.Contains(Value) then
        Include(Value);
  end
end;

procedure TSetClass<T>.IncludeLogicalOr(const A, B: TSetClass<T>);
var
  Value: T;
begin
  for Value in A do
    Include(Value);
  for Value in B do
    Include(Value);
end;

procedure TSetClass<T>.IncludeLogicalXor(const A, B: TSetClass<T>);
var
  Value: T;
begin
  for Value in A do
    if not B.Contains(Value) then
      Include(Value);
  for Value in B do
    if not A.Contains(Value) then
      Include(Value);
end;

procedure TSetClass<T>.Include(const AValue: T);
var R: TEmptyRec;
begin
  FSet.AddOrSetValue(AValue, R);
end;

procedure TSetClass<T>.Include(const ASet: array of T);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Include(ASet[i]);
end;

procedure TSetClass<T>.Include(const AValues: TEnumerable<T>);
var
  Item: T;
begin
  for Item in AValues do
    Include(Item);
end;

procedure TSetClass<T>.Clear;
begin
  FSet.Clear;
end;

function TSetClass<T>.Contains(const AValue: T): boolean;
begin
  result := FSet.ContainsKey(AValue);
end;

function TSetClass<T>.Contains(const ASet: array of T): boolean;
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    if not Contains(ASet[i]) then
      Exit(False);
  result := True;
end;

function TSetClass<T>.Contains(const AValues: TEnumerable<T>): boolean;
var
  Item: T;
begin
  for Item in AValues do
    if not Contains(Item) then
      Exit(False);
  result := True;
end;

procedure TSetClass<T>.Remove(const AValue: T);
begin
  FSet.Remove(AValue);
end;

{ TSet<T> }

procedure TSet<T>.Add(const V: T);
begin
  ReadWrite.Include(V);
end;

procedure TSet<T>.Add(const V: TEnumerable<T>);
var
  D: TSetClass<T>;
  Value: T;
begin
  D := ReadWrite;
  for Value in V do
    D.Include(Value);
end;

procedure TSet<T>.Add(const V: array of T);
var
  D: TSetClass<T>;
  Value: T;
begin
  D := ReadWrite;
  for Value in V do
    D.Include(Value);
end;

procedure TSet<T>.Add(V: TSet<T>);
var
  S,D: TSetClass<T>;
  Value: T;
begin
  D := ReadWrite;
  S := V.ReadOnly;
  for Value in S do
    D.Include(Value);
end;

procedure TSet<T>.Add(const V: array of TEnumerable<T>);
var
  D: TSetClass<T>;
  Enum: TEnumerable<T>;
  Value: T;
begin
  D := ReadWrite;
  for Enum in V do
    for Value in Enum do
      D.Include(Value);
end;

function TSet<T>.GetEmpty: Boolean;
begin
  result := Count = 0;
end;

function TSet<T>.GetEnumerator: TEnumerator;
begin
  result := Readonly.GetEnumerator;
end;

function TSet<T>.GetReadonly:TSetClass<T>;
begin
  if FSetInt=nil then
    CreateSet;
  result := FSetInt.Data;
end;

function TSet<T>.GetReadWrite: TSetClass<T>;
var
  SrcSetInt: IInterfacedObject<TSetClass<T>>;
begin
  if FSetInt=nil then
    CreateSet
  else
    if FSetInt.GetRefCount<>1 then
    begin
      { Copy on write }
      SrcSetInt := FSetInt;
      CreateSet(SrcSetInt.Data.Count, SrcSetInt.Data.Comparer);
      FSetInt.Data.Include(SrcSetInt.Data);
      FSetInt.Data.OwnsValues := SrcSetInt.Data.OwnsValues;
    end;
  result := FSetInt.Data;
end;

procedure TSet<T>.CreateSet(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
var
  C: IEqualityComparer<T>;
begin
  if AComparer=nil
    then C := TComparerUtils.DefaultEqualityComparer<T>
    else C := AComparer;
  FSetInt := TInterfacedObject<TSetClass<T>>.Create( TSetClass<T>.Create(ACapacity, C) );
end;

function TSet<T>.GetAsArray: TArray<T>;
begin
  Result := Readonly.AsArray;
end;

function TSet<T>.GetOwnsValues: boolean;
begin
  result := ReadOnly.OwnsValues;
end;

procedure TSet<T>.SetOwnsValues(AOwnsValues: boolean);
begin
  if AOwnsValues<>OwnsValues then
    ReadWrite.OwnsValues := AOwnsValues;
end;

function TSet<T>.GetAsString: string;
begin
  Result := Readonly.AsString;
end;

function TSet<T>.Contains(const a: T) : Boolean;
begin
  result := ReadOnly.Contains(a);
end;

function TSet<T>.Contains(const a: TEnumerable<T>) : Boolean;
begin
  result := ReadOnly.Contains(a);
end;

function TSet<T>.Contains(a: TSet<T>) : Boolean;
begin
  result := ReadOnly.Contains(a.ReadOnly);
end;

class operator TSet<T>.In(const a: TEnumerable<T>; b: TSet<T>): Boolean;
begin
  result := b.ReadOnly.Contains(a);
end;

class operator TSet<T>.In(const a: T; b: TSet<T>): Boolean;
begin
  result := b.ReadOnly.Contains(a);
end;

class operator TSet<T>.In(a, b: TSet<T>): Boolean;
begin
  result := b.ReadOnly.Contains(a.ReadOnly);
end;

class operator TSet<T>.Implicit(const a: T): TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Implicit(const a: TEnumerable<T>): TSet<T>;
begin
  result.FSetInt := nil;
  if a<>nil then
    result.Add(a);
end;

class operator TSet<T>.Implicit(const a : array of T) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Implicit(const a : TArray<T>) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Explicit(const a : T) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Explicit(const a : TEnumerable<T>) : TSet<T>;
begin
  result.FSetInt := nil;
  if a<>nil then
    result.Add(a);
end;

class operator TSet<T>.Explicit(const a : array of T) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Explicit(const a : TArray<T>) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Add(a, b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b.ReadOnly);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: array of T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.Add(const a: T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Include(b.ReadOnly);
end;

class operator TSet<T>.Add(const a: array of T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Include(b.ReadOnly);
end;

class operator TSet<T>.Add(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Include(b.ReadOnly);
end;

procedure TSet<T>.Clear;
begin
  Self := Default(TSet<T>);
end;

function TSet<T>.GetCollection: TEnumerable<T>;
begin
  result := Readonly;
end;

function TSet<T>.GetCount: integer;
begin
  result := ReadOnly.Count;
end;

constructor TSet<T>.Create(const V: TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

constructor TSet<T>.Create(const V: array of T; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

constructor TSet<T>.Create(ACapacity: integer; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
end;

constructor TSet<T>.Create(V: TSet<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

function TSet<T>.Copy: TSet<T>;
begin
  if FSetInt=nil then
    result.FSetInt := nil
  else
  begin
    result := TSet<T>.Create(Count, FSetInt.Data.Comparer);
    result.Add(Self);
  end;
end;

constructor TSet<T>.Create(const V: array of TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

procedure TSet<T>.Remove(const V: TEnumerable<T>);
begin
  ReadWrite.Remove(V);
end;

procedure TSet<T>.Remove(const V: array of T);
begin
  ReadWrite.Remove(V);
end;

procedure TSet<T>.Remove(const V: T);
begin
  ReadWrite.Remove(V);
end;

procedure TSet<T>.Remove(V: TSet<T>);
begin
  ReadWrite.Remove(V.ReadOnly);
end;

procedure TSet<T>.Remove(const V: array of TEnumerable<T>);
var
  i: integer;
  D: TSetClass<T>;
begin
  D := ReadWrite;
  for i := 0 to High(v) do
    D.Remove(V[i]);
end;

class operator TSet<T>.Equal(a, b: TSet<T>): Boolean;
var
  S,D: TSetClass<T>;
begin
  S := a.ReadOnly;
  D := b.ReadOnly;
  result := (S.Count=D.Count) and D.Contains(S);
end;

class operator TSet<T>.Equal(a: TSet<T>; const b: TEnumerable<T>): Boolean;
var
  Value: T;
  D: TSetClass<T>;
  N: Integer;
begin
  D := a.ReadOnly;
  N := 0;
  for Value in b do
    if D.Contains(Value) then
      inc(N)
    else
      exit(False);
  result := N=D.Count;
end;

class operator TSet<T>.NotEqual(a, b: TSet<T>): Boolean;
begin
  result := not (a=b);
end;

class operator TSet<T>.NotEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := not (a=b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b);
end;

class operator TSet<T>.Subtract(a, b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: array of T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b);
end;

class operator TSet<T>.Subtract(const a: T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.Subtract(const a: array of T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.Subtract(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.GreaterThanOrEqual(a, b: TSet<T>): Boolean;
begin
  result := b in a;
end;

class operator TSet<T>.GreaterThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := b in a;
end;

class operator TSet<T>.GreaterThan(a, b: TSet<T>): Boolean;
begin
  result := (a.Count>b.Count) and (b in a);
end;

class operator TSet<T>.GreaterThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := (a<>b) and (b in a);
end;

class operator TSet<T>.LessThan(a, b: TSet<T>): Boolean;
begin
  result := (a.Count<b.Count) and (a in b);
end;

class operator TSet<T>.LessThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := (a<>b) and (a in b);
end;

class operator TSet<T>.LessThanOrEqual(a, b: TSet<T>): Boolean;
begin
  result := a in b;
end;

class operator TSet<T>.LessThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := a in b;
end;

class operator TSet<T>.LogicalAnd(a, b: TSet<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result.ReadWrite.IncludeLogicalAnd(a.ReadOnly, b.ReadOnly);
end;

class operator TSet<T>.LogicalAnd(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  S,R: TSetClass<T>;
  Value: T;
begin
  result.FSetInt := nil;
  S := a.ReadOnly;
  R := result.ReadWrite;
  for Value in b do
    if S.Contains(Value) then
      R.Include(Value);
end;

class operator TSet<T>.LogicalOr(a, b: TSet<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result.ReadWrite.IncludeLogicalOr(a.ReadOnly, b.ReadOnly);
end;

class operator TSet<T>.LogicalOr(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.LogicalXor(a, b: TSet<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result.ReadWrite.IncludeLogicalXor(a.ReadOnly, b.ReadOnly);
end;

class operator TSet<T>.LogicalXor(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result := a xor TSet<T>(b);
end;

end.
