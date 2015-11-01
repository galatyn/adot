unit adot.Generics.Collections;

{
  - TSet<TValue>
  - TMap<TKey,TValue> (TTextDictionary)
  - TTextSet
  - TTextDictionary<TValue>
  - TTextMap<TValue> (TTextDictionary)
  - TMultimap<TKey,TValue>
  - THeap<T> (aka TPriorityQueue<T>)
  - TCyclicBuffer<T>
  - TCache<TKey,TValue>
}
interface

uses
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils,
  System.Classes, System.Character;

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
    constructor Create(ACapacity: integer = 0; const AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AValues: array of TValue; const AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AValues: TEnumerable<TValue>; const AComparer: IEqualityComparer<TValue> = nil); overload;
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

  // Set of case insensitive strings.
  TTextSet = class(TSet<String>)
  public
    constructor Create(ACapacity: integer = 0); overload;
    constructor Create(const Values: array of String); overload;
    constructor Create(Values: TEnumerable<String>); overload;
  end;

  // Dictionary with case insensitive keys.
  TTextDictionary<ValueType> = class(TDictionary<String, ValueType>)
  public
    constructor Create;
  end;
  TTextMap<ValueType> = class(TTextDictionary<ValueType>);

  // Alias to dictionary.
  TMap<TKey,TValue> = class(TDictionary<TKey,TValue>);

  // Unsorted multimap container (one key -> many values).
  TContainsCheckType = (cctAll, cctAnyOf);
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

        // Does nothing, only for kind of "syntax compatibility" with classes.
        procedure Free;

        // Returns copy. It allows us to use both syntaxes:
        // Example 1.
        //   Enum := Multimap.Values['key1'];
        //   while Enum.MoveNext do
        //     Fun(Enum.Current);
        // Example 2.
        //   for v in Multimap['key1'] do
        //     Fun(v);
        function GetEnumerator: TValueEnumerator;
      end;

      // Usually we don't need to use it directly. Use default enumerator for TMultimap<> instead.
      // We have to use class (not record) because we want TMultimap to be compatible with TEnumerable
      // and TEnumerable.DoGetEnumerator returns class TEnumerator<TPair<TKey,TValue>>.
      TPairEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
      protected
        FCurrentValue: TValueEnumerator;
        FInEnumKey: boolean;
        FMultimap: TMultimap<TKey,TValue>;
        FCurrentKey: TDictionary<TKey, integer>.TKeyEnumerator;

        function DoGetCurrent: TPair<TKey,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMultimap: TMultimap<TKey,TValue>);
      end;

      TKeyEnumerator = TDictionary<TKey, integer>.TKeyEnumerator;
      TKeyCollection = TDictionary<TKey, integer>.TKeyCollection;

  protected
    FCount: TDictionary<TKey, integer>;
    FValues: TDictionary<TMultimapKey, TValue>;

    // Single instance created by request.
    // Provides TEnumerable interface to keys (Multimap.Keys.ToArray etc).
    FKeyCollection: TKeyCollection;

    function GetTotalValuesCount: integer;
    function GetValuesCount(const AKey: TKey):Integer;
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetValuesEnumerator(const AKey: TKey): TValueEnumerator;
    function GetKeys: TKeyCollection;

  public

    constructor Create(const AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const ACollection: TEnumerable<TPair>; const AComparer: IEqualityComparer<TKey> = nil); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const AKey: TKey; const AValues: array of TValue); overload;
    procedure Add(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    procedure Add(const ACollection: TEnumerable<TPair>); overload;

    //    e := m.Values[Key];
    //    while e.MoveNext do
    //      if m.Current=10 then m.RemoveValue(e);
    function Remove(const AKey: TKey):Boolean;
    procedure RemoveValue(const AEnum: TValueEnumerator);
    procedure RemoveValues(const AKey: TKey; const AValues: TSet<TValue>); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: array of TValue); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;

    function ContainsKey(const AKey: TKey): Boolean; inline;
    function ContainsKeys(const AKeys: array of TKey; AContainsCheckType: TContainsCheckType = cctAnyOf): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; AContainsCheckType: TContainsCheckType = cctAnyOf): Boolean; overload;

    function ContainsValue(const AKey: TKey; const AValue: TValue; AComparer: IEqualityComparer<TValue> = nil): Boolean;
    function ContainsValues(const AKey: TKey; const AValues: array of TValue; AContainsCheckType: TContainsCheckType = cctAnyOf;
      AComparer: IEqualityComparer<TValue> = nil): Boolean; overload;
    function ContainsValues(const AKey: TKey; const AValues: TEnumerable<TValue>; AContainsCheckType: TContainsCheckType = cctAnyOf;
      AComparer: IEqualityComparer<TValue> = nil): Boolean; overload;

    // Enumerator. Example:
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
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    constructor Create(const AComparer: IComparer<T> = nil; ACapacity: integer = 0); overload;
    constructor Create(const ACollection: TEnumerable<T>; const AComparer: IComparer<T> = nil; ACapacity: integer = 0); overload;
    constructor Create(const AValues: array of T; const AComparer: IComparer<T> = nil; ACapacity: integer = 0); overload;
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

  // http://en.wikipedia.org/wiki/Circular_buffer
  // Add items to head, get items from tail.
  // Can resize only when empty.
  TCyclicBuffer<T> = class
  protected
    FValues: TList<T>;
    FFirst: integer;
    FCount: integer;

    function GetItem(n: integer): T;
    function GetCapacity: integer;
    procedure SetCapacity(ANewCapacity: integer);
  public
    constructor Create(ANewCapacity: integer);
    destructor Destroy; override;
    procedure Push(v: T); overload; // add to head
    procedure Push(const a: array of T); overload;
    function Pop: T; // get from tail
    function Head: T;
    function Tail: T;
    procedure Clear;
    function Full: Boolean;

    property Capacity: integer read GetCapacity write SetCapacity; // should have Count=0 to resize
    property Items[n: integer]:T read GetItem; default;
    property Pos: integer read FFirst write FFirst;
    property Count: integer read FCount write FCount;
    property List: TList<T> read FValues;
    property First: T read Head;
    property Last: T read Tail;
  end;

  // Similar to TDictionary, but automatically deletes data if they take more space than allowed.
  TCache<TKey,TValue> = class
  protected
    Cache: TDictionary<TKey,TValue>;
    Size, MaxSize: longint;
  public
    constructor Create(AMaxSize: longint = 1024*1024);
    destructor Destroy; override;
    procedure Add(K: TKey; V: TValue; ASize: longint);
    function TryGetValue(K: TKey; var V: TValue):boolean;
  end;

implementation

{ TSet<TValue> }

constructor TSet<TValue>.Create(ACapacity: integer = 0; const AComparer: IEqualityComparer<TValue> = nil);
begin
  FSet := TDictionary<TValue, TEmptyRec>.Create(ACapacity, AComparer);
end;

constructor TSet<TValue>.Create(const AValues: array of TValue; const AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TSet<TValue>.Create(const AValues: TEnumerable<TValue>; const AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
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

procedure TSet<TValue>.Remove(const AValue: TValue);
begin
  FSet.Remove(AValue);
end;

{ TTextSet }

constructor TTextSet.Create(ACapacity: integer = 0);
begin
  inherited Create(ACapacity, TOrdinalIStringComparer.Create);
end;

constructor TTextSet.Create(const Values: array of String);
begin
  Create;
  Add(Values);
end;

constructor TTextSet.Create(Values: TEnumerable<String>);
begin
  Create;
  Add(Values);
end;

{ TTextDictionary<ValueType> }

constructor TTextDictionary<ValueType>.Create;
begin
  inherited Create(TOrdinalIStringComparer.Create);
end;

{ TMultimap<TKey, TValue> }

constructor TMultimap<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey> = nil);
begin
  inherited Create;
  FCount := TDictionary<TKey, integer>.Create(AComparer);
  FValues := TDictionary<TMultimapKey, TValue>.Create(TMultimapKeyEqualityComparer.Create(AComparer));
end;

constructor TMultimap<TKey, TValue>.Create(const ACollection: TEnumerable<TPair>; const AComparer: IEqualityComparer<TKey> = nil);
begin
  Create(AComparer);
  Add(ACollection);
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

function TMultimap<TKey, TValue>.ContainsKeys(const AKeys: array of TKey; AContainsCheckType: TContainsCheckType): Boolean;
var
  i: Integer;
begin
  case AContainsCheckType of
    cctAll:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if not ContainsKey(AKeys[i]) then
            Exit(False);
        result := True;
      end;
    cctAnyOf:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if ContainsKey(AKeys[i]) then
            Exit(True);
        result := False;
      end;
    else
      raise exception.Create('');
  end;
end;

function TMultimap<TKey, TValue>.ContainsKeys(const AKeys: TEnumerable<TKey>; AContainsCheckType: TContainsCheckType): Boolean;
var
  Key: TKey;
begin
  case AContainsCheckType of
    cctAll:
      begin
        for Key in AKeys do
          if not ContainsKey(Key) then
            Exit(False);
        result := True;
      end;
    cctAnyOf:
      begin
        for Key in AKeys do
          if ContainsKey(Key) then
            Exit(True);
        result := False;
      end;
    else
      raise exception.Create('');
  end;
end;

function TMultimap<TKey, TValue>.ContainsValue(const AKey: TKey; const AValue: TValue; AComparer: IEqualityComparer<TValue> = nil): Boolean;
var
  V: TValue;
begin
  if AComparer=nil then
    AComparer := TEqualityComparer<TValue>.Default;
  for V in Values[AKey] do
    if AComparer.Equals(AValue, V) then
      Exit(True);
  result := False;
end;

function TMultimap<TKey, TValue>.ContainsValues(const AKey: TKey; const AValues: array of TValue; AContainsCheckType: TContainsCheckType;
  AComparer: IEqualityComparer<TValue>): Boolean;
var
  ValueSet: TSet<TValue>;
  i: Integer;
begin
  if AComparer=nil then
    AComparer := TEqualityComparer<TValue>.Default;
  ValueSet := TSet<TValue>.Create(0, AComparer);
  try
    case AContainsCheckType of
      cctAll:
        begin
          for i := Low(AValues) to High(AValues) do
            if not ValueSet.Contains(AValues[i]) then
              Exit(False);
          result := True;
        end;
      cctAnyOf:
        begin
          for i := Low(AValues) to High(AValues) do
            if ValueSet.Contains(AValues[i]) then
              Exit(True);
          result := False;
        end;
      else
        raise exception.Create('');
    end;
  finally
    FreeAndNil(ValueSet);
  end;
end;

function TMultimap<TKey, TValue>.ContainsValues(const AKey: TKey; const AValues: TEnumerable<TValue>; AContainsCheckType: TContainsCheckType;
  AComparer: IEqualityComparer<TValue>): Boolean;
var
  ValueSet: TSet<TValue>;
  V: TValue;
begin
  if AComparer=nil then
    AComparer := TEqualityComparer<TValue>.Default;
  ValueSet := TSet<TValue>.Create(0, AComparer);
  try
    case AContainsCheckType of
      cctAll:
        begin
          for V in AValues do
            if not ValueSet.Contains(V) then
              Exit(False);
          result := True;
        end;
      cctAnyOf:
        begin
          for V in AValues do
            if ValueSet.Contains(V) then
              Exit(True);
          result := False;
        end;
      else
        raise exception.Create('');
    end;
  finally
    FreeAndNil(ValueSet);
  end;
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

procedure TMultimap<TKey, TValue>.Add(const AKey: TKey; const AValues: array of TValue);
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

procedure TMultimap<TKey, TValue>.Add(const ACollection: TEnumerable<TPair>);
var
  item: TPair;
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

function TMultimap<TKey, TValue>.TValueEnumerator.GetEnumerator: TValueEnumerator;
begin
  result.FMultimap := FMultimap;
  result.FMultimapKey := FMultimapKey;
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

function TMultimap<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
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

function TMultimap<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
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

constructor THeap<T>.Create(const ACollection: TEnumerable<T>; const AComparer: IComparer<T>;
  ACapacity: integer);
begin
  Create(AComparer, ACapacity);
  Add(ACollection);
end;

constructor THeap<T>.Create(const AValues: array of T; const AComparer: IComparer<T>;
  ACapacity: integer);
begin
  Create(AComparer, ACapacity);
  Add(AValues);
end;

destructor THeap<T>.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function THeap<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := GetEnumerator;
end;

procedure THeap<T>.Clear;
begin
  FValues.Clear;
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
begin
  Result := FValues.Add(Default(T))+1;
  while Result > 1 do
  begin
    i := Result shr 1;
    if FComparer.Compare(FValues[i-1], AValue) <= 0 then
      Break;
    Swap(i-1, Result-1);
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

{ TCyclicBuffer<T> }

constructor TCyclicBuffer<T>.Create(ANewCapacity: integer);
begin
  FValues := TList<T>.Create;
  Capacity := ANewCapacity;
end;

destructor TCyclicBuffer<T>.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TCyclicBuffer<T>.Full: Boolean;
begin
  result := FValues.Count=Count;
end;

function TCyclicBuffer<T>.GetCapacity: integer;
begin
  result := FValues.Count;
end;

function TCyclicBuffer<T>.GetItem(n: integer): T;
begin
  assert(FCount>0);
  result := FValues[(FFirst+n) mod FValues.Count];
end;

procedure TCyclicBuffer<T>.Clear;
begin
  FFirst := 0;
  FCount := 0;
end;

function TCyclicBuffer<T>.Head: T;
begin
  assert(FCount>0);
  result := FValues[FFirst];
end;

function TCyclicBuffer<T>.Tail: T;
begin
  assert(FCount>0);
  result := FValues[(FFirst+FCount-1) mod FValues.Count];
end;

procedure TCyclicBuffer<T>.Push(v: T);
begin
  if FCount<FValues.Count then
    inc(FCount);
  Dec(FFirst);
  if FFirst<0 then
    FFirst := FValues.Count-1;
  FValues[FFirst] := v;
end;

procedure TCyclicBuffer<T>.Push(const a: array of T);
var
  i: Integer;
begin
  for i := 0 to length(a)-1 do
    Push(a[i]);
end;

procedure TCyclicBuffer<T>.SetCapacity(ANewCapacity: integer);
begin
  assert(Count=0);
  FValues.Count := ANewCapacity;
end;

function TCyclicBuffer<T>.Pop: T;
begin
  assert(FCount>0);
  Dec(FCount);
  result := FValues[(FFirst+FCount) mod FValues.Count];
end;

{ TCache<TKey, TValue> }

constructor TCache<TKey, TValue>.Create(AMaxSize: Integer);
begin
  MaxSize := AMaxSize;
  Cache := TDictionary<TKey,TValue>.Create;
end;

destructor TCache<TKey, TValue>.Destroy;
begin
  FreeAndNil(Cache);
end;

procedure TCache<TKey, TValue>.Add(K: TKey; V: TValue; ASize: longint);
begin
  if Size>=MaxSize then
  begin
    Size := 0;
    Cache.Clear;
  end;
  Cache.Add(K,V);
  inc(Size, ASize);
end;

function TCache<TKey, TValue>.TryGetValue(K: TKey; var V: TValue): boolean;
begin
  result := Cache.TryGetValue(K,V);
end;

end.
