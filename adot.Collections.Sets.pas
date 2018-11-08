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
  System.SysUtils,
  System.RTLConsts;

type
  TSetOp = (soUnion, soIntersection, soDifference, soSymmetricDifference);

  { Generic class for unordered set }
  TSetClass<T> = class(TEnumerableExt<T>)
  public
    type
      { TObjectDictionary (Delphi 10.2.1) doesn't allow to change Ownership for existing objects,
        we can provide Ownership in constructor only. We implement own version of ObjectDictionary to fix it. }
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

  public
    constructor Create(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AValues: TArray<T>; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AValues: TEnumerable<T>; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AOperands: TArray<TSetClass<T>>; ASetOp: TSetOp; AComparer: IEqualityComparer<T> = nil); overload;

    destructor Destroy; override;

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
    function ToArray: TArray<T>; override;

    property Count: integer read GetCount;
    property Comparer: IEqualityComparer<T> read GetComparer;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  { check TSet bellow }
  TCustomSetRec<T> = record
  private
    type
      TItem = record
        HashCode: Integer;
        Value: T;
      end;

      TValueEnumerator = record
      private
        FItems: TArray<TItem>;
        FIndex: Integer;

        function GetCurrent: T;

      public
        constructor Create(const AItems: TArray<TItem>);
        function MoveNext: Boolean;

        property Current: T read GetCurrent;
      end;

      TValueNotifyEvent = procedure(const Value: T; Action: TCollectionNotification) of object;

    const
      EMPTY_HASH = -1;

    var
      FItems: TArray<TItem>;
      FCount: Integer;
      FComparer: IEqualityComparer<T>;
      FGrowThreshold: Integer;
      FOnValueNotify: TValueNotifyEvent;
      FOwnsValues: boolean;

    procedure SetCapacity(ACapacity: Integer);
    procedure Rehash(NewCapPow2: Integer);
    procedure Grow;
    function GetBucketIndex(const Value: T; HashCode: Integer): Integer;
    function Hash(const Key: T): Integer;
    procedure RehashAdd(HashCode: Integer; const Value: T);
    procedure DoAdd(HashCode, Index: Integer; const Value: T);
    procedure DoSetValue(Index: Integer; const Value: T);
    function DoRemove(const Key: T; HashCode: Integer; Notification: TCollectionNotification): T;
    procedure ValueNotify(const Value: T; Action: TCollectionNotification);

  private
    procedure Init(ACapacity: Integer = 0; const AComparer: IEqualityComparer<T> = nil); overload;
    procedure Init(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T> = nil); overload;
    procedure Init(const AValues: TArray<T>; const AComparer: IEqualityComparer<T> = nil); overload;
    procedure Init(const AValues: array of T; const AComparer: IEqualityComparer<T> = nil); overload;

    procedure Add(const Value: T); overload;
    procedure Add(const Values: TEnumerable<T>); overload;
    procedure Add(const Values: TArray<T>); overload;
    procedure Add(const Values: array of T); overload;
    procedure Add(const Values: TCustomSetRec<T>); overload;

    procedure Remove(const Value: T); overload;
    procedure Remove(const Values: TEnumerable<T>); overload;
    procedure Remove(const Values: TArray<T>); overload;
    procedure Remove(const Values: TCustomSetRec<T>); overload;

    function Contains(const Value: T): Boolean; overload;
    function Contains(const Values: TEnumerable<T>): Boolean; overload;
    function Contains(const Values: TArray<T>): Boolean; overload;
    function Contains(const Values: TCustomSetRec<T>): Boolean; overload;

    procedure CopyFrom(const Src: TCustomSetRec<T>);
    function ToArray: TArray<T>;
    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;

    function Extract(const Value: T): T;
    procedure Clear;
    procedure TrimExcess;
    procedure SetOwnsValues(const Value: boolean);

    property Count: Integer read FCount;
    property OwnsValues: boolean read FOwnsValues write SetOwnsValues;
    property OnValueNotify: TValueNotifyEvent read FOnValueNotify write FOnValueNotify;
    property Comparer: IEqualityComparer<T> read FComparer write FComparer;

  public
    function GetEnumerator: TValueEnumerator; { must be public to allow "for in" syntax }
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
  TSet<T> = record
  private
    type
      TData = class(TInterfacedObject, IInterface)
      private
        FSet: TCustomSetRec<T>;
      public
        destructor Destroy; override;
      end;

  public
    type
      TValueEnumerator = TCustomSetRec<T>.TValueEnumerator;
      TValueNotifyEvent = TCustomSetRec<T>.TValueNotifyEvent;

  private
    FData: TData;          { valid as long as FDataIntf is alive }
    FDataIntf: IInterface; { maintain lifecycle }

    function GetCount: Integer;
    function GetOnValueNotify: TValueNotifyEvent;
    procedure SetOnValueNotify(const Value: TValueNotifyEvent);
    function GetComparer: IEqualityComparer<T>;
    procedure SetComparer(const Value: IEqualityComparer<T>);
    function GetEmpty: Boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(const Value: boolean);

  public
    procedure Init; overload;
    procedure Init(ACapacity: Integer; const AComparer: IEqualityComparer<T> = nil); overload;
    procedure Init(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T> = nil); overload;
    procedure Init(const AValues: TArray<T>; const AComparer: IEqualityComparer<T> = nil); overload;
    procedure Init(const AValues: array of T; const AComparer: IEqualityComparer<T> = nil); overload;

    class function Create: TSet<T>; overload; static;
    class function Create(ACapacity: Integer; const AComparer: IEqualityComparer<T> = nil): TSet<T>; overload; static;
    class function Create(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T> = nil): TSet<T>; overload; static;
    class function Create(const AValues: TArray<T>; const AComparer: IEqualityComparer<T> = nil): TSet<T>; overload; static;
    class function Create(const AValues: array of T; const AComparer: IEqualityComparer<T> = nil): TSet<T>; overload; static;

    function GetEnumerator: TValueEnumerator;

    procedure Add(const AValue: T); overload;
    procedure Add(const AValues: TEnumerable<T>); overload;
    procedure Add(const AValues: TArray<T>); overload;
    procedure Add(const AValues: array of T); overload;
    procedure Add(const AValues: TSet<T>); overload;

    procedure Remove(const AValue: T); overload;
    procedure Remove(const AValues: TEnumerable<T>); overload;
    procedure Remove(const AValues: TArray<T>); overload;
    procedure Remove(const AValues: TSet<T>); overload;

    { It is prefered to use syntax "Item in SomSet" over "SomeSet.Contains(Item)", but in
      rare situations compiler can be confused and then "Contains" method is the only way to go }
    function Contains(const AValue: T): Boolean; overload;
    function Contains(const AValues: TEnumerable<T>) : Boolean; overload;
    function Contains(const AValues: TArray<T>) : Boolean; overload;
    function Contains(const AValues: TSet<T>) : Boolean; overload;

    function Extract(const Value: T): T;

    procedure CopyFrom(const Src: TSet<T>);
    function Copy: TSet<T>;
    function ToArray: TArray<T>;
    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;
    procedure Clear;
    procedure TrimExcess;

    class operator In(const a: T;              b: TSet<T>) : Boolean;
    class operator In(const a: TEnumerable<T>; b: TSet<T>) : Boolean;
    class operator In(const a: TArray<T>;      b: TSet<T>) : Boolean;
    class operator In(const a: TSet<T>;        b: TSet<T>) : Boolean;

    class operator Implicit(const a : T)              : TSet<T>;
    class operator Implicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Implicit(const a : TArray<T>)      : TSet<T>;

    class operator Explicit(const a : T)              : TSet<T>;
    class operator Explicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Explicit(const a : TArray<T>)      : TSet<T>;

    class operator Add(a: TSet<T>; const b: T              ): TSet<T>;
    class operator Add(a: TSet<T>; const b: TEnumerable<T> ): TSet<T>;
    class operator Add(a: TSet<T>; const b: TArray<T>      ): TSet<T>;
    class operator Add(a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    class operator Add(const a: T;              b: TSet<T>): TSet<T>;
    class operator Add(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
    class operator Add(const a: TArray<T>;      b: TSet<T>): TSet<T>;

    class operator Subtract(a: TSet<T>; const b: T              ): TSet<T>;
    class operator Subtract(a: TSet<T>; const b: TEnumerable<T> ): TSet<T>;
    class operator Subtract(a: TSet<T>; const b: TArray<T>      ): TSet<T>;
    class operator Subtract(a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    class operator Subtract(const a: T;              b: TSet<T>): TSet<T>;
    class operator Subtract(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
    class operator Subtract(const a: TArray<T>;      b: TSet<T>): TSet<T>;

    class operator Equal(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator Equal(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator Equal(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator NotEqual(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator NotEqual(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator NotEqual(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator GreaterThanOrEqual(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator GreaterThanOrEqual(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator GreaterThanOrEqual(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator GreaterThan(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator GreaterThan(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator GreaterThan(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator LessThan(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator LessThan(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator LessThan(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator LessThanOrEqual(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator LessThanOrEqual(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator LessThanOrEqual(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator LogicalAnd(const a: TSet<T>; const b: TEnumerable<T> ): TSet<T>;
    class operator LogicalAnd(const a: TSet<T>; const b: TArray<T>      ): TSet<T>;
    class operator LogicalAnd(const a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    class operator LogicalOr(const a: TSet<T>; const b: TEnumerable<T> ): TSet<T>;
    class operator LogicalOr(const a: TSet<T>; const b: TArray<T>      ): TSet<T>;
    class operator LogicalOr(const a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    class operator LogicalXor(const a: TSet<T>; const b: TEnumerable<T> ): TSet<T>;
    class operator LogicalXor(const a: TSet<T>; const b: TArray<T>      ): TSet<T>;
    class operator LogicalXor(const a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property OnValueNotify: TValueNotifyEvent read GetOnValueNotify write SetOnValueNotify;
    property Comparer: IEqualityComparer<T> read GetComparer write SetComparer;
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

constructor TSetClass<T>.Create(const AValues: TArray<T>; AComparer: IEqualityComparer<T> = nil);
begin
  Create(Length(AValues), AComparer);
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
  result := FSet.Keys.GetEnumerator;
end;

function TSetClass<T>.Empty: Boolean;
begin
  result := FSet.Count=0;
end;

function TSetClass<T>.ToArray: TArray<T>;
var
  i: Integer;
  Value: T;
begin
  SetLength(Result, Count);
  i := 0;
  for Value in FSet.Keys do
  begin
    Result[i] := Value;
    inc(i);
  end;
  Assert(Count=i);
end;

function TSetClass<T>.GetComparer: IEqualityComparer<T>;
begin
  result := FComparerCopy;
end;

function TSetClass<T>.GetCount: integer;
begin
  result := FSet.Count;
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

{ TCustomSetRec<T>.TValueEnumerator }

constructor TCustomSetRec<T>.TValueEnumerator.Create(const AItems: TArray<TItem>);
begin
  FItems := AItems;
  FIndex := -1;
end;

function TCustomSetRec<T>.TValueEnumerator.GetCurrent: T;
begin
  Result := FItems[FIndex].Value;
end;

function TCustomSetRec<T>.TValueEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FItems) - 1 do
  begin
    Inc(FIndex);
    if FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

{ TCustomSetRec<T> }

procedure TCustomSetRec<T>.Init(ACapacity: Integer = 0; const AComparer: IEqualityComparer<T> = nil);
var
  cap: Integer;
begin
  if ACapacity < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Self := Default(TCustomSetRec<T>);
  if AComparer = nil
    then FComparer := TComparerUtils.DefaultEqualityComparer<T>
    else FComparer := AComparer;
  SetCapacity(ACapacity);
end;

procedure TCustomSetRec<T>.Init(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T>);
var
  Item: T;
begin
  Init(0, AComparer);
  for Item in AValues do
    Add(Item);
end;

procedure TCustomSetRec<T>.Init(const AValues: TArray<T>; const AComparer: IEqualityComparer<T> = nil);
var
  Item: T;
begin
  Init(Length(AValues), AComparer);
  for Item in AValues do
    Add(Item);
end;

procedure TCustomSetRec<T>.Init(const AValues: array of T; const AComparer: IEqualityComparer<T> = nil);
var
  Item: T;
begin
  Init(Length(AValues), AComparer);
  for Item in AValues do
    Add(Item);
end;

procedure TCustomSetRec<T>.Add(const Value: T);
var
  hc: Integer;
  index: Integer;
begin
  hc := Hash(Value);
  index := GetBucketIndex(Value, hc);
  if index >= 0 then
    DoSetValue(index, Value)
  else
  begin
    // We only grow if we are inserting a new value.
    if Count >= FGrowThreshold then
    begin
      Grow;
      // We need a new Bucket Index because the array has grown.
      index := GetBucketIndex(Value, hc);
    end;
    DoAdd(hc, not index, Value);
  end;
end;

procedure TCustomSetRec<T>.Add(const Values: TEnumerable<T>);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomSetRec<T>.Add(const Values: TArray<T>);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomSetRec<T>.Add(const Values: array of T);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomSetRec<T>.Add(const Values: TCustomSetRec<T>);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomSetRec<T>.Clear;
var
  i: Integer;
  oldItems: TArray<TItem>;
begin
  oldItems := FItems;
  FCount := 0;
  SetLength(FItems, 0);
  SetCapacity(0);
  FGrowThreshold := 0;

  for i := 0 to Length(oldItems) - 1 do
    if oldItems[i].HashCode <> EMPTY_HASH then
      ValueNotify(oldItems[i].Value, cnRemoved);
end;

function TCustomSetRec<T>.Contains(const Value: T): Boolean;
begin
  Result := GetBucketIndex(Value, Hash(Value)) >= 0;
end;

function TCustomSetRec<T>.Contains(const Values: TEnumerable<T>): Boolean;
var
  Value: T;
begin
  for Value in Values do
    if GetBucketIndex(Value, Hash(Value)) < 0 then
      Exit(False);
  result := True;
end;

function TCustomSetRec<T>.Contains(const Values: TArray<T>): Boolean;
var
  Value: T;
begin
  for Value in Values do
    if GetBucketIndex(Value, Hash(Value)) < 0 then
      Exit(False);
  result := True;
end;

function TCustomSetRec<T>.Contains(const Values: TCustomSetRec<T>): Boolean;
var
  Value: T;
begin
  for Value in Values do
    if GetBucketIndex(Value, Hash(Value)) < 0 then
      Exit(False);
  result := True;
end;

procedure TCustomSetRec<T>.CopyFrom(const Src: TCustomSetRec<T>);
var
  I: Integer;
begin
  SetLength(FItems, Length(Src.FItems));
  for I := Low(FItems) to High(FItems) do
    FItems[I] := Src.FItems[I];
  FCount := Src.FCount;
  FComparer := Src.FComparer;
  FGrowThreshold := Src.FGrowThreshold;
  FOnValueNotify := Src.FOnValueNotify;
  FOwnsValues := Src.FOwnsValues;
end;

procedure TCustomSetRec<T>.DoAdd(HashCode, Index: Integer; const Value: T);
begin
  FItems[Index].HashCode := HashCode;
  FItems[Index].Value := Value;
  Inc(FCount);
  ValueNotify(Value, cnAdded);
end;

function TCustomSetRec<T>.DoRemove(const Key: T; HashCode: Integer; Notification: TCollectionNotification): T;
var
  gap, index, hc, bucket: Integer;
begin
  index := GetBucketIndex(Key, HashCode);
  if index < 0 then
    Exit(Default(T));

  FItems[index].HashCode := EMPTY_HASH;
  Result := FItems[index].Value;

  gap := index;
  while True do
  begin
    Inc(index);
    if index = Length(FItems) then
      index := 0;

    hc := FItems[index].HashCode;
    if hc = EMPTY_HASH then
      Break;

    bucket := hc and (Length(FItems) - 1);
    if not InCircularRange(gap, bucket, index) then
    begin
      FItems[gap] := FItems[index];
      gap := index;
      // The gap moved, but we still need to find it to terminate.
      FItems[gap].HashCode := EMPTY_HASH;
    end;
  end;

  FItems[gap].HashCode := EMPTY_HASH;
  FItems[gap].Value := Default(T);
  Dec(FCount);

  ValueNotify(Result, Notification);
end;

procedure TCustomSetRec<T>.DoSetValue(Index: Integer; const Value: T);
var
  oldValue: T;
begin
  oldValue := FItems[Index].Value;
  FItems[Index].Value := Value;

  ValueNotify(oldValue, cnRemoved);
  ValueNotify(Value, cnAdded);
end;

function TCustomSetRec<T>.Extract(const Value: T): T;
var
  hc, index: Integer;
begin
  hc := Hash(Value);
  index := GetBucketIndex(Value, hc);
  if index < 0 then
    Exit(Default(T));

  Result := DoRemove(Value, hc, cnExtracted);
end;

function TCustomSetRec<T>.GetBucketIndex(const Value: T; HashCode: Integer): Integer;
var
  start, hc: Integer;
begin
  if Length(FItems) = 0 then
    Exit(not High(Integer));

  start := HashCode and (Length(FItems) - 1);
  Result := start;
  while True do
  begin
    hc := FItems[Result].HashCode;

    // Not found: return complement of insertion point.
    if hc = EMPTY_HASH then
      Exit(not Result);

    // Found: return location.
    if (hc = HashCode) and FComparer.Equals(FItems[Result].Value, Value) then
      Exit(Result);

    Inc(Result);
    if Result >= Length(FItems) then
      Result := 0;
  end;
end;

function TCustomSetRec<T>.GetEnumerator: TValueEnumerator;
begin
  result := TValueEnumerator.Create(FItems);
end;

procedure TCustomSetRec<T>.Grow;
var
  newCap: Integer;
begin
  newCap := Length(FItems) * 2;
  if newCap = 0 then
    newCap := 4;
  Rehash(newCap);
end;

function TCustomSetRec<T>.Hash(const Key: T): Integer;
const
  PositiveMask = not Integer($80000000);
begin
  // Double-Abs to avoid -MaxInt and MinInt problems.
  // Not using compiler-Abs because we *must* get a positive integer;
  // for compiler, Abs(Low(Integer)) is a null op.
  Result := PositiveMask and ((PositiveMask and FComparer.GetHashCode(Key)) + 1);
end;

procedure TCustomSetRec<T>.Rehash(NewCapPow2: Integer);
var
  oldItems, newItems: TArray<TItem>;
  i: Integer;
begin
  if NewCapPow2 = Length(FItems) then
    Exit
  else if NewCapPow2 < 0 then
    OutOfMemoryError;

  oldItems := FItems;
  SetLength(newItems, NewCapPow2);
  for i := 0 to Length(newItems) - 1 do
    newItems[i].HashCode := EMPTY_HASH;
  FItems := newItems;
  FGrowThreshold := NewCapPow2 shr 1 + NewCapPow2 shr 2; // 75%

  for i := 0 to Length(oldItems) - 1 do
    if oldItems[i].HashCode <> EMPTY_HASH then
      RehashAdd(oldItems[i].HashCode, oldItems[i].Value);
end;

procedure TCustomSetRec<T>.RehashAdd(HashCode: Integer; const Value: T);
var
  index: Integer;
begin
  index := not GetBucketIndex(Value, HashCode);
  FItems[index].HashCode := HashCode;
  FItems[index].Value := Value;
end;

procedure TCustomSetRec<T>.Remove(const Values: TEnumerable<T>);
var
  Value: T;
begin
  for Value in Values do
    DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomSetRec<T>.Remove(const Values: TArray<T>);
var
  Value: T;
begin
  for Value in Values do
    DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomSetRec<T>.Remove(const Values: TCustomSetRec<T>);
var
  Value: T;
begin
  for Value in Values do
    DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomSetRec<T>.Remove(const Value: T);
begin
  DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomSetRec<T>.SetCapacity(ACapacity: Integer);
var
  newCap: Integer;
begin
  if ACapacity < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if ACapacity = 0 then
    Rehash(0)
  else
  begin
    newCap := 4;
    while newCap < ACapacity do
      newCap := newCap shl 1;
    Rehash(newCap);
  end
end;

procedure TCustomSetRec<T>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := Value;
end;

function TCustomSetRec<T>.ToArray: TArray<T>;
var
  Value: T;
  I: integer;
begin
  // We assume our caller has passed correct Count
  SetLength(Result, Count);
  I := 0;
  for Value in Self do
  begin
    Result[I] := Value;
    Inc(I);
  end;
end;

function TCustomSetRec<T>.ToString: string;
begin
  result := ToText(' ');
end;

function TCustomSetRec<T>.ToText(const ValuesDelimiter: string): string;
var
  Builder: TStringBuilder;
  V: T;
  N: Boolean;
begin
  Builder := TStringBuilder.Create;
  try
    N := False;
    for V in Self do
    begin
      if N then
        Builder.Append(ValuesDelimiter)
      else
        N := True;
      Builder.Append(TRttiUtils.ValueAsString<T>(V));
    end;
    result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

procedure TCustomSetRec<T>.TrimExcess;
begin
  // Ensure at least one empty slot for GetBucketIndex to terminate.
  SetCapacity(Count + 1);
end;

procedure TCustomSetRec<T>.ValueNotify(const Value: T; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Value, Action);
  if FOwnsValues and (Action = TCollectionNotification.cnRemoved) then
    PObject(@Value)^.DisposeOf;
end;

{ TSet<T>.TData }

destructor TSet<T>.TData.Destroy;
begin
  FSet.Clear;
  inherited;
end;

{ TSet<T> }

procedure TSet<T>.Init;
begin
  Self := Default(TSet<T>);
  FData := TData.Create;
  FDataIntf := FData;
  FData.FSet.Init;
end;

procedure TSet<T>.Init(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T> = nil);
begin
  Self := Default(TSet<T>);
  FData := TData.Create;
  FDataIntf := FData;
  FData.FSet.Init(AValues, AComparer);
end;

class operator TSet<T>.In(const a: T; b: TSet<T>): Boolean;
begin
  result := b.Contains(a);
end;

class operator TSet<T>.In(const a: TEnumerable<T>; b: TSet<T>): Boolean;
begin
  result := b.Contains(a);
end;

class operator TSet<T>.In(const a: TArray<T>; b: TSet<T>): Boolean;
begin
  result := b.Contains(a);
end;

class operator TSet<T>.Implicit(const a: T): TSet<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TSet<T>.Implicit(const a: TEnumerable<T>): TSet<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TSet<T>.Implicit(const a: TArray<T>): TSet<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TSet<T>.In(const a: TSet<T>; b: TSet<T>): Boolean;
begin
  result := b.Contains(a);
end;

procedure TSet<T>.Init(const AValues: TArray<T>; const AComparer: IEqualityComparer<T> = nil);
begin
  Self := Default(TSet<T>);
  FData := TData.Create;
  FDataIntf := FData;
  FData.FSet.Init(AValues, AComparer);
end;

procedure TSet<T>.Init(const AValues: array of T; const AComparer: IEqualityComparer<T> = nil);
begin
  Self := Default(TSet<T>);
  FData := TData.Create;
  FDataIntf := FData;
  FData.FSet.Init(AValues, AComparer);
end;

class operator TSet<T>.LessThan(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := not (a >= b);
end;

class operator TSet<T>.LessThan(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := not (a >= b);
end;

class operator TSet<T>.LessThan(const a, b: TSet<T>): Boolean;
begin
  result := not (a >= b);
end;

class operator TSet<T>.LessThanOrEqual(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := not (a > b);
end;

class operator TSet<T>.LessThanOrEqual(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := not (a > b);
end;

class operator TSet<T>.LessThanOrEqual(const a, b: TSet<T>): Boolean;
begin
  result := not (a > b);
end;

class operator TSet<T>.LogicalAnd(const a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  Value: T;
begin
  result.Init;
  for Value in b do
    if a.Contains(Value) then
      result.Add(Value);
end;

class operator TSet<T>.LogicalAnd(const a: TSet<T>; const b: TArray<T>): TSet<T>;
var
  Value: T;
begin
  result.Init;
  for Value in b do
    if a.Contains(Value) then
      result.Add(Value);
end;

class operator TSet<T>.LogicalAnd(const a, b: TSet<T>): TSet<T>;
var
  Value: T;
begin
  if a.Count > b.Count then
    result := b and a
  else
  begin
    result.Init;
    for Value in a do
      if b.Contains(Value) then
        result.Add(Value);
  end;
end;

class operator TSet<T>.LogicalOr(const a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.LogicalOr(const a: TSet<T>; const b: TArray<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.LogicalOr(const a, b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.LogicalXor(const a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result := a xor TSet<T>(b);
end;

class operator TSet<T>.LogicalXor(const a: TSet<T>; const b: TArray<T>): TSet<T>;
begin
  result := a xor TSet<T>(b);
end;

class operator TSet<T>.LogicalXor(const a, b: TSet<T>): TSet<T>;
var
  Value: T;
begin
  result.Init;
  for Value in A do
    if not B.Contains(Value) then
      result.Add(Value);
  for Value in B do
    if not A.Contains(Value) then
      result.Add(Value);
end;

class operator TSet<T>.NotEqual(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := not (a = b);
end;

class operator TSet<T>.NotEqual(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := not (a = b);
end;

class operator TSet<T>.NotEqual(const a, b: TSet<T>): Boolean;
begin
  result := not (a = b);
end;

procedure TSet<T>.Init(ACapacity: Integer; const AComparer: IEqualityComparer<T> = nil);
begin
  Self := Default(TSet<T>);
  FData := TData.Create;
  FDataIntf := FData;
  FData.FSet.Init(ACapacity, AComparer);
end;

class function TSet<T>.Create: TSet<T>;
begin
  result.Init;
end;

class function TSet<T>.Create(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T> = nil): TSet<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TSet<T>.Create(const AValues: TArray<T>; const AComparer: IEqualityComparer<T> = nil): TSet<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TSet<T>.Create(const AValues: array of T; const AComparer: IEqualityComparer<T> = nil): TSet<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TSet<T>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<T> = nil): TSet<T>;
begin
  result.Init(ACapacity, AComparer);
end;

function TSet<T>.GetComparer: IEqualityComparer<T>;
begin
  result := FData.FSet.Comparer;
end;

function TSet<T>.GetCount: Integer;
begin
  result := FData.FSet.Count;
end;

function TSet<T>.GetEmpty: Boolean;
begin
  result := FData.FSet.Count = 0;
end;

function TSet<T>.GetEnumerator: TValueEnumerator;
begin
  result := FData.FSet.GetEnumerator;
end;

function TSet<T>.GetOnValueNotify: TValueNotifyEvent;
begin
  result := FData.FSet.OnValueNotify;
end;

function TSet<T>.GetOwnsValues: boolean;
begin
  result := FData.FSet.OwnsValues;
end;

class operator TSet<T>.GreaterThanOrEqual(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := a.Contains(b);
end;

class operator TSet<T>.GreaterThanOrEqual(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := a.Contains(b);
end;

class operator TSet<T>.GreaterThanOrEqual(const a, b: TSet<T>): Boolean;
begin
  result := a.Contains(b);
end;

class operator TSet<T>.GreaterThan(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := a > TSet<T>(b);
end;

class operator TSet<T>.GreaterThan(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := a > TSet<T>(b);
end;

class operator TSet<T>.GreaterThan(const a, b: TSet<T>): Boolean;
begin
  result := (a.Count > b.Count) and a.Contains(b);
end;

procedure TSet<T>.Add(const AValue: T);
begin
  FData.FSet.Add(AValue);
end;

procedure TSet<T>.Add(const AValues: TEnumerable<T>);
begin
  FData.FSet.Add(AValues);
end;

procedure TSet<T>.Add(const AValues: TArray<T>);
begin
  FData.FSet.Add(AValues);
end;

procedure TSet<T>.Add(const AValues: array of T);
begin
  FData.FSet.Add(AValues);
end;

procedure TSet<T>.Add(const AValues: TSet<T>);
begin
  FData.FSet.Add(AValues.FData.FSet);
end;

class operator TSet<T>.Explicit(const a: T): TSet<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TSet<T>.Explicit(const a: TEnumerable<T>): TSet<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TSet<T>.Equal(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := TSet<T>(b) = a;
end;

class operator TSet<T>.Equal(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := TSet<T>(b) = a;
end;

class operator TSet<T>.Equal(const a, b: TSet<T>): Boolean;
begin
  result := (a.Count = b.Count) and a.Contains(b);
end;

class operator TSet<T>.Explicit(const a: TArray<T>): TSet<T>;
begin
  result.Init;
  result.Add(a);
end;

function TSet<T>.Extract(const Value: T): T;
begin
  result := FData.FSet.Extract(Value);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: T): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: TArray<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.Add(const a: T; b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(b);
  result.Add(a);
end;

class operator TSet<T>.Add(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(b);
  result.Add(a);
end;

class operator TSet<T>.Add(const a: TArray<T>; b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(b);
  result.Add(a);
end;

procedure TSet<T>.Clear;
begin
  FData.FSet.Clear;
end;

function TSet<T>.Contains(const AValue: T): Boolean;
begin
  result := FData.FSet.Contains(AValue);
end;

function TSet<T>.Contains(const AValues: TEnumerable<T>): Boolean;
begin
  result := FData.FSet.Contains(AValues);
end;

function TSet<T>.Contains(const AValues: TArray<T>): Boolean;
begin
  result := FData.FSet.Contains(AValues);
end;

function TSet<T>.Contains(const AValues: TSet<T>): Boolean;
begin
  result := FData.FSet.Contains(AValues.FData.FSet);
end;

function TSet<T>.Copy: TSet<T>;
begin
  result.Init;
  result.FData.FSet.CopyFrom(FData.FSet);
end;

procedure TSet<T>.CopyFrom(const Src: TSet<T>);
begin
  Init;
  FData.FSet.CopyFrom(Src.FData.FSet);
end;

procedure TSet<T>.Remove(const AValue: T);
begin
  FData.FSet.Remove(AValue);
end;

procedure TSet<T>.Remove(const AValues: TEnumerable<T>);
begin
  FData.FSet.Remove(AValues);
end;

procedure TSet<T>.Remove(const AValues: TArray<T>);
begin
  FData.FSet.Remove(AValues);
end;

procedure TSet<T>.Remove(const AValues: TSet<T>);
begin
  FData.FSet.Remove(AValues.FData.FSet);
end;

procedure TSet<T>.SetComparer(const Value: IEqualityComparer<T>);
begin
  FData.FSet.Comparer := Value;
end;

procedure TSet<T>.SetOnValueNotify(const Value: TValueNotifyEvent);
begin
  FData.FSet.OnValueNotify := Value;
end;

procedure TSet<T>.SetOwnsValues(const Value: boolean);
begin
  FData.FSet.OwnsValues := Value;
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: T): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: TArray<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
end;

class operator TSet<T>.Subtract(const a: T; b: TSet<T>): TSet<T>;
begin
  result.Init;
  if not b.Contains(a) then
    result.Add(a);
end;

class operator TSet<T>.Subtract(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
var
  Value: T;
begin
  result.Init;
  for Value in a do
    if not b.Contains(Value) then
      result.Add(Value);
end;

class operator TSet<T>.Subtract(const a: TArray<T>; b: TSet<T>): TSet<T>;
var
  Value: T;
begin
  result.Init;
  for Value in a do
    if not b.Contains(Value) then
      result.Add(Value);
end;

function TSet<T>.ToArray: TArray<T>;
begin
  result := FData.FSet.ToArray;
end;

function TSet<T>.ToString: string;
begin
  result := FData.FSet.ToString;
end;

function TSet<T>.ToText(const ValuesDelimiter: string): string;
begin
  result := FData.FSet.ToText(ValuesDelimiter);
end;

procedure TSet<T>.TrimExcess;
begin
  FData.FSet.TrimExcess;
end;

end.
