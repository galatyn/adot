unit CrossPlatform.Containers;

interface

uses
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils;

type
  TEmptyRec = record end;

  TSet<TValue> = class(TEnumerable<TValue>)
  private
  protected
    type
      TEnumerator = TDictionary<TValue, TEmptyRec>.TKeyEnumerator;

    var
      FSet: TDictionary<TValue, TEmptyRec>;

    function GetCount: integer; inline;
    function DoGetEnumerator: TEnumerator<TValue>; override;

  public
    constructor Create; overload;
    constructor Create(const AValues: array of TValue); overload;
    constructor Create(const AValues: TEnumerable<TValue>); overload;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator; reintroduce;
    procedure Add(const AValue: TValue); overload; inline;
    procedure Add(const ASet: array of TValue); overload;
    procedure Add(const AValues: TEnumerable<TValue>); overload;
    procedure Include(const AValue: TValue); overload; inline;
    procedure Include(const ASet: array of TValue); overload;
    procedure Include(const AValues: TEnumerable<TValue>); overload;
    procedure Remove(const AValue: TValue); overload; inline;
    procedure Remove(const ASet: array of TValue); overload;
    procedure Remove(const AValues: TEnumerable<TValue>); overload;
    function Contains(const AValue: TValue): boolean; overload; inline;
    function Contains(const ASet: array of TValue): boolean; overload;
    function Contains(const AValues: TEnumerable<TValue>): boolean; overload;
    procedure Clear; inline;
    property Count: integer read GetCount;
  end;

  TMultimap<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  protected
    type
      TMultimapKey = record
        Key: TKey;
        Number: integer;
      end;

      // TKey can be String for example, so we can't use default comparer for
      // TMultimapKey record type, we have to implement specific one.
      TMultimapKeyEqualityComparer = class(TEqualityComparer<TMultimapKey>)
      private
        FKeyComparer: IEqualityComparer<TKey>;
      public
        constructor Create(AKeyComparer: IEqualityComparer<TKey>);
        function Equals(const Left, Right: TMultimapKey): Boolean; overload; override;
        function GetHashCode(const Value: TMultimapKey): Integer; overload; override;
      end;

  public
    type
      TPair = System.Generics.Collections.TPair<TKey,TValue>;

      // Standard containers in Delphi use classes for enumerators.
      // It is ok when we keep single instance of the class inside, but for multimap we
      // will have to keep lot of instances (for every key where items enumerator has requested).
      // To avoid of this we use record type instead of class type.
      TValueEnumerator = record
      private
        FMultimap: TMultimap<TKey,TValue>;
        FMultimapKey: TMultimapKey;

        constructor Create(AMultimap: TMultimap<TKey,TValue>; const AKey: TKey);
        function GetCurrent: TValue;
        function GetKey: TKey;
      public
        function MoveNext: Boolean;
        property Current: TValue read GetCurrent;
        property Key: TKey read GetKey;
        procedure Free; // does nothing, only for kind of "compatibility" with classes.
      end;

      // Don't use it directly, use default enumerator for TMultimap<> instead.
      TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
      private
        FMultimap: TMultimap<TKey,TValue>;
        FCurrentKey: TDictionary<TKey, integer>.TKeyEnumerator;
        FInEnumKey: boolean;
        FCurrentValue: TValueEnumerator;

        function GetCurrent: TPair<TKey,TValue>;
      protected
        function DoGetCurrent: TPair<TKey,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMultimap: TMultimap<TKey,TValue>);
        destructor Destroy; override;
        property Current: TPair<TKey,TValue> read GetCurrent;
        function MoveNext: Boolean;
      end;

      TKeyEnumerator = TDictionary<TKey, integer>.TKeyEnumerator;
      TKeyCollection = TDictionary<TKey, integer>.TKeyCollection;

  protected
    FCount: TDictionary<TKey, integer>;
    FValues: TDictionary<TMultimapKey, TValue>;
    FKeyCollection: TKeyCollection;

    function GetTotalValuesCount: integer;
    function GetValuesCount(const AKey: TKey):Integer;
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetValuesEnumerator(const AKey: TKey): TValueEnumerator;
    function GetKeys: TKeyCollection;

  public

    constructor Create; overload;
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const AKey: TKey; const AValues: array of TValue); overload;
    procedure Add(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;

    //    e := m.Values[Key];
    //    while e.MoveNext do
    //      if m.Current=10 then m.RemoveValue(e);
    function Remove(const AKey: TKey):Boolean;
    procedure RemoveValue(const AEnum: TValueEnumerator);
    procedure RemoveValues(const AKey: TKey; const AValues: TSet<TValue>); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: array of TValue); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsKeys(const AKeys: array of TKey): Boolean;

    // Adds default enumerator to the multmap. Example:
    //    p: TPair<string, integer>;
    //    for p in m do
    //      [do something] p.Key / p.Value;
    function GetEnumerator: TPairEnumerator; reintroduce;

    property TotalValuesCount: integer read GetTotalValuesCount;
    property ValuesCount[const AKey: TKey]:integer read GetValuesCount;

    // Enumerator of values for specific Key. Returns records, ne need to destroy.
    // Example 1:
    //    Enum := m.Values['test'];
    //    While Enum.MoveNext do
    //      if Enum.Current=314 then m.RemoveValue(Enum);
    // Example 2:
    //    for s in m.Values['test'] do
    //      <do something with s>
    property Values[const AKey: TKey]: TValueEnumerator read GetValuesEnumerator; default;

    // Enumerator of the keys. For example we can enumerate all values this way
    // (alternative to default enumerator):
    //    for Key in m.Keys do
    //      for Value in m.Values[Key] do
    property Keys: TKeyCollection read GetKeys;
  end;

  THeap<T> = class(TEnumerable<T>)
  private
  protected
    FValues: TList<T>;
    FComparer: IComparer<T>;

    function GetCount: integer;
    function GetValue(n: integer): T;
    function GetCapacity: integer;
    procedure SetCapacity(const AValue: integer);
    procedure Swap(n1, n2: integer); inline;
  public
    constructor Create(const AComparer: IComparer<T> = nil; ACapacity: integer = 0); overload;
    constructor Create(const ACollection: TEnumerable<T>; const AComparer: IComparer<T> = nil; ACapacity: integer = 0); overload;
    destructor Destroy; override;
    function GetEnumerator: TList<T>.TEnumerator;

    procedure Clear; inline;
    procedure TrimExcess; inline;
    function Add(const AValue: T): integer; overload;
    procedure Add(const AValues: array of T); overload;
    procedure Add(const ACollection: TEnumerable<T>); overload;
    function Find(const AValue: T): integer; // slow (enumerating of all items)
    function MinValue: T; inline;
    procedure Delete(n: integer);
    procedure DeleteMin; inline;
    function ExtractMin: T; inline;

    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Values[n: integer]: T read GetValue; default;
  end;

  TPriorityQueue<T> = class(THeap<T>);

implementation

{ TSet<TValue> }

constructor TSet<TValue>.Create;
begin
  FSet := TDictionary<TValue, TEmptyRec>.Create;
end;

destructor TSet<TValue>.Destroy;
begin
  FreeAndNil(FSet);
  inherited;
end;

function TSet<TValue>.DoGetEnumerator: TEnumerator<TValue>;
begin
  Result := GetEnumerator;
end;

function TSet<TValue>.GetCount: integer;
begin
  result := FSet.Count;
end;

function TSet<TValue>.GetEnumerator: TEnumerator;
begin
  result := FSet.Keys.GetEnumerator;
end;

procedure TSet<TValue>.Remove(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Remove(ASet[i]);
end;

procedure TSet<TValue>.Remove(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Remove(Item);
end;

procedure TSet<TValue>.Add(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.Add(AValue, R);
end;

procedure TSet<TValue>.Add(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Add(ASet[i]);
end;

procedure TSet<TValue>.Add(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Add(Item);
end;

procedure TSet<TValue>.Include(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.AddOrSetValue(AValue, R);
end;

procedure TSet<TValue>.Include(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Include(ASet[i]);
end;

procedure TSet<TValue>.Include(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Include(Item);
end;

procedure TSet<TValue>.Clear;
begin
  FSet.Clear;
end;

function TSet<TValue>.Contains(const AValue: TValue): boolean;
begin
  result := FSet.ContainsKey(AValue);
end;

function TSet<TValue>.Contains(const ASet: array of TValue): boolean;
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    if not Contains(ASet[i]) then
      Exit(False);
  result := True;
end;

function TSet<TValue>.Contains(const AValues: TEnumerable<TValue>): boolean;
var
  Item: TValue;
begin
  for Item in AValues do
    if not Contains(Item) then
      Exit(False);
  result := True;
end;

constructor TSet<TValue>.Create(const AValues: array of TValue);
begin
  Create;
  Add(AValues);
end;

constructor TSet<TValue>.Create(const AValues: TEnumerable<TValue>);
begin
  Create;
  Add(AValues);
end;

procedure TSet<TValue>.Remove(const AValue: TValue);
begin
  FSet.Remove(AValue);
end;

{ TMultimap<TKey, TValue> }

constructor TMultimap<TKey, TValue>.Create;
begin
  Create(IEqualityComparer<TKey>(nil));
end;

constructor TMultimap<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FCount := TDictionary<TKey, integer>.Create(AComparer);
  FValues := TDictionary<TMultimapKey, TValue>.Create(TMultimapKeyEqualityComparer.Create(AComparer));
end;

constructor TMultimap<TKey, TValue>.Create(
  const ACollection: TEnumerable<TPair<TKey, TValue>>);
begin
  Create(IEqualityComparer<TKey>(nil));
end;

destructor TMultimap<TKey, TValue>.Destroy;
begin
  FreeAndNil(FCount);
  FreeAndNil(FValues);
  FreeAndNil(FKeyCollection);
  inherited;
end;

function TMultimap<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  result := GetEnumerator;
end;

function TMultimap<TKey, TValue>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TMultimap<TKey, TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(FCount);
  Result := FKeyCollection;
end;

function TMultimap<TKey, TValue>.GetTotalValuesCount: integer;
begin
  result := FValues.Count;
end;

procedure TMultimap<TKey, TValue>.Clear;
begin
  FCount.Clear;
  FValues.Clear;
end;

function TMultimap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  result := GetValuesCount(AKey)>0;
end;

function TMultimap<TKey, TValue>.ContainsKeys(
  const AKeys: array of TKey): Boolean;
var
  i: Integer;
begin
  for i := Low(AKeys) to High(AKeys) do
    if not ContainsKey(AKeys[i]) then
      Exit(False);
  result := True;
end;

function TMultimap<TKey, TValue>.GetValuesCount(const AKey: TKey): Integer;
begin
  if not FCount.TryGetValue(AKey, result) then
    result := 0;
end;

procedure TMultimap<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  MKey: TMultimapKey;
begin
  MKey.Key := AKey;
  if not FCount.TryGetValue(AKey, MKey.Number) then
    MKey.Number := 0;
  FValues.Add(MKey, AValue);
  inc(MKey.Number);
  FCount.AddOrSetValue(AKey, MKey.Number);
end;

procedure TMultimap<TKey, TValue>.Add(const AKey: TKey;
  const AValues: array of TValue);
var
  MKey: TMultimapKey;
  i: Integer;
begin
  MKey.Key := AKey;
  if not FCount.TryGetValue(AKey, MKey.Number) then
    MKey.Number := 0;
  for i := Low(AValues) to High(AValues) do
  begin
    FValues.Add(MKey, AValues[i]);
    inc(MKey.Number);
  end;
  FCount.AddOrSetValue(AKey, MKey.Number);
end;

procedure TMultimap<TKey, TValue>.Add(const AKey: TKey; const AValues: TEnumerable<TValue>);
var
  item: TValue;
begin
  for item in AValues do
    Add(AKey, item);
end;

procedure TMultimap<TKey, TValue>.Add(
  const ACollection: TEnumerable<TPair<TKey, TValue>>);
var
  item: TPair<TKey,TValue>;
begin
  for item in ACollection do
    Add(item.Key, item.Value);
end;

function TMultimap<TKey, TValue>.Remove(const AKey: TKey): Boolean;
var
  MKey: TMultimapKey;
begin
  result := FCount.TryGetValue(AKey, MKey.Number);
  if not result then
    Exit;
  FCount.Remove(AKey);
  MKey.Key := AKey;
  while MKey.Number>0 do
  begin
    dec(MKey.Number);
    FValues.Remove(MKey);
  end;
end;

procedure TMultimap<TKey, TValue>.RemoveValue(const AEnum: TValueEnumerator);
var
  LastKey: TMultimapKey;
  LastValue: TValue;
begin
  if not FValues.ContainsKey(AEnum.FMultimapKey) or not FCount.TryGetValue(AEnum.Key, LastKey.Number) then
    raise Exception.Create('Error');
  dec(LastKey.Number);
  LastKey.Key := AEnum.Key;
  if not FValues.TryGetValue(LastKey, LastValue) then
    raise Exception.Create('Error');
  FValues.AddOrSetValue(AEnum.FMultimapKey, LastValue);
  FValues.Remove(LastKey);
  if LastKey.Number=0 then
    FCount.Remove(AEnum.Key)
  else
    FCount.AddOrSetValue(AEnum.Key, LastKey.Number);
end;

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TSet<TValue>);
var
  Enum: TValueEnumerator;
begin
  Enum := Values[AKey];
  while Enum.MoveNext do
    if AValues.Contains(Enum.Current) then
      RemoveValue(Enum);
end;

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: array of TValue);
var
  s: TSet<TValue>;
begin
  s := TSet<TValue>.Create(AValues);
  try
    RemoveValues(AKey, s);
  finally
    FReeAndNil(s);
  end;
end;

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>);
var
  s: TSet<TValue>;
begin
  s := TSet<TValue>.Create(AValues);
  try
    RemoveValues(AKey, s);
  finally
    FReeAndNil(s);
  end;
end;

function TMultimap<TKey, TValue>.GetValuesEnumerator(const AKey: TKey): TValueEnumerator;
begin
  result := TValueEnumerator.Create(Self, AKey);
end;

{ TMultimap<TKey, TValue>.TValueEnumerator }

constructor TMultimap<TKey, TValue>.TValueEnumerator.Create(AMultimap: TMultimap<TKey, TValue>; const AKey: TKey);
begin
  FMultimap := AMultimap;
  FMultimapKey.Key := AKey;
  if not FMultimap.FCount.TryGetValue(AKey, FMultimapKey.Number) then
    FMultimapKey.Number := -1;
end;

function TMultimap<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  result := FMultimapKey.Number>0;
  if result then
    dec(FMultimapKey.Number);
end;

procedure TMultimap<TKey, TValue>.TValueEnumerator.Free;
begin
end;

function TMultimap<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if not FMultimap.FValues.TryGetValue(FMultimapKey, result) then
    raise Exception.Create('Error');
end;

function TMultimap<TKey, TValue>.TValueEnumerator.GetKey: TKey;
begin
  result := FMultimapKey.Key;
end;

{ TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer }

constructor TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer.Create(AKeyComparer: IEqualityComparer<TKey>);
begin
  FKeyComparer := AKeyComparer;
  if FKeyComparer=nil then
    FKeyComparer := TEqualityComparer<TKey>.Default;
end;

function TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer.Equals(const Left,
  Right: TMultimapKey): Boolean;
begin
  result := (Left.Number=Right.Number) and FKeyComparer.Equals(Left.Key, Right.Key);
end;

function TMultimap<TKey, TValue>.TMultimapKeyEqualityComparer.GetHashCode(
  const Value: TMultimapKey): Integer;
begin
  result := FKeyComparer.GetHashCode(Value.Key) xor Value.Number;
end;

{ TMultimap<TKey, TValue>.TPairEnumerator }

constructor TMultimap<TKey, TValue>.TPairEnumerator.Create(
  const AMultimap: TMultimap<TKey, TValue>);
begin
  inherited Create;
  FMultimap := AMultimap;
  FCurrentKey := FMultimap.FCount.Keys.GetEnumerator;
end;

destructor TMultimap<TKey, TValue>.TPairEnumerator.Destroy;
begin
  FMultimap := nil;
  FreeAndNil(FCurrentKey);
  inherited;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  Result := MoveNext;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  Result := GetCurrent;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.MoveNext: Boolean;
begin
  if not FInEnumKey then
  begin
    result := FCurrentKey.MoveNext;
    if not result then
      Exit;
    FCurrentValue := FMultimap.Values[FCurrentKey.Current];
    result := FCurrentValue.MoveNext;
    Assert(result); // if key is here, then there must be at least one value
    FInEnumKey := True;
    Exit;
  end;
  result := FCurrentValue.MoveNext;
  if not result then
  begin
    FInEnumKey := False;
    result := MoveNext;
  end;
end;

function TMultimap<TKey, TValue>.TPairEnumerator.GetCurrent: TPair<TKey, TValue>;
begin
  result.Key := FCurrentKey.Current;
  result.Value := FCurrentValue.Current;
end;

{ THeap<T> }

constructor THeap<T>.Create(const AComparer: IComparer<T>; ACapacity: integer);
begin
  FValues := TList<T>.Create(AComparer);
  if ACapacity>0 then
    FValues.Capacity := ACapacity;
  FComparer := AComparer;
  if FComparer=nil then
    FComparer := TComparer<T>.Default;
end;

procedure THeap<T>.Clear;
begin
  FValues.Clear;
end;

constructor THeap<T>.Create(const ACollection: TEnumerable<T>; const AComparer: IComparer<T>;
  ACapacity: integer);
begin
  Create(AComparer, ACapacity);
  Add(ACollection);
end;

destructor THeap<T>.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function THeap<T>.GetCapacity: integer;
begin
  result := FValues.Capacity;
end;

function THeap<T>.GetCount: integer;
begin
  result := FValues.Count;
end;

function THeap<T>.GetEnumerator: TList<T>.TEnumerator;
begin
  result := FValues.GetEnumerator;
end;

function THeap<T>.GetValue(n: integer): T;
begin
  result := FValues[n];
end;

function THeap<T>.MinValue: T;
begin
  result := FValues[0];
end;

function THeap<T>.Find(const AValue: T): integer;
var
  i: Integer;
begin
  for i := 0 to FValues.Count-1 do
    if FComparer.Compare(FValues[i], AValue)=0 then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function THeap<T>.Add(const AValue: T): integer;
var
  i: Integer;
  v: T;
begin
  Result := FValues.Add(Default(T));
  Result := Count;
  while Result > 1 do
  begin
    i := Result shr 1;
    if FComparer.Compare(FValues[(i-1)], AValue) <= 0 then
      Break;
    v := FValues[i-1]; FValues[i-1] := FValues[Result-1]; FValues[Result-1] := v;
    Result := i;
  end;
  dec(Result);
  FValues[result] := AValue;
end;

procedure THeap<T>.Add(const AValues: array of T);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    Add(AValues[i]);
end;

procedure THeap<T>.Add(const ACollection: TEnumerable<T>);
var
  v: T;
begin
  for v in ACollection do
    Add(v);
end;

procedure THeap<T>.SetCapacity(const AValue: integer);
begin
  FValues.Capacity := AValue;
end;

procedure THeap<T>.Swap(n1,n2: integer);
var
  Temp: T;
begin
  Temp := FValues[n1];
  FValues[n1] := FValues[n2];
  FValues[n2] := Temp;
end;

procedure THeap<T>.TrimExcess;
begin
  FValues.TrimExcess;
end;

procedure THeap<T>.Delete(n: integer);
var
  c: integer;
begin
  c := Count;
  inc(n);
  while n * 2 < c do
  begin
    n := n * 2;
    if FComparer.Compare(FValues[n], FValues[n - 1]) < 0 then
      Inc(n);
    Swap(n - 1, (n shr 1) - 1);
  end;
  if n * 2 <= c then
  begin
    n := n * 2;
    Swap(n - 1, (n shr 1) - 1);
  end;
  while n > 1 do
    if FComparer.Compare(FValues[(n shr 1) - 1], FValues[c - 1]) <= 0 then
      break
    else
    begin
      Swap((n shr 1) - 1, n - 1);
      n := n shr 1;
    end;
  Swap(c - 1, n - 1);
  FValues.Delete(c-1);
end;

procedure THeap<T>.DeleteMin;
begin
  Delete(0);
end;

function THeap<T>.ExtractMin: T;
begin
  result := MinValue;
  DeleteMin;
end;

end.
