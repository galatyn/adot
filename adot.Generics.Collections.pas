unit adot.Generics.Collections;

{
  - TUnsortedSet<TValue>
  - TSet<TValue> (sorted set of values)
  - TUnsortedMap<TKey,TValue>
  - TMap<TKey,TValue> (sorted by key set of pairs key->value)
  - TTextSet
  - TTextDictionary<TValue>
?  - TTextMap<TValue> (TTextDictionary)
  - TMultimap<TKey,TValue>
  - THeap<T> (aka TPriorityQueue<T>)
  - TCyclicBuffer<T>
  - TCache<TKey,TValue>
  - TBinarySearchTree<TKey, TValue> (TAATree<TKey, TValue>)
}
interface

uses
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils,
  System.Classes, System.Character, System.Math;

type
  TEmptyRec = record end;

  TUnsortedSet<TValue> = class(TEnumerable<TValue>)
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
  TTextSet = class(TUnsortedSet<String>)
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
  TUnsortedMap<TKey,TValue> = class(TDictionary<TKey,TValue>);

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
    procedure RemoveValues(const AKey: TKey; const AValues: TUnsortedSet<TValue>); overload;
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

  {
    AA-tree is kind of balanced BST. It has good performance,
    equal/comparable to Red-Black tree, but simplier implementation.
    http://en.wikipedia.org/wiki/AA_tree
  }
  TItemHandle = NativeInt;
  TSearchType = (stAll, stAny);
  TAATree<TKey,TValue> = class
  protected
    type
      P_AATreeItem = ^T_AATreeItem;
      T_AATreeItem = record
        Parent, Left, Right: P_AATreeItem;
        Level: integer;
        Data: TPair<TKey, TValue>;
      end;

    var
      FCount: integer;
      FRoot, FBottom, FDeleted, FLast: P_AATreeItem;
      FComparer: IComparer<TKey>;

    function AllocNewItem: P_AATreeItem; inline;
    procedure ReleaseItem(p: P_AATreeItem);
    function PtrTohandle(p: P_AATreeItem): TItemHandle;

    procedure treeSkew(var p: P_AATreeItem);
    procedure treeSplit(var p: P_AATreeItem);
    function treeAdd(p,aparent: P_AATreeItem; var Dst: P_AATreeItem): Boolean;
    function treeGetHeight(p: P_AATreeItem): integer;
    function treeFullHeight: integer; inline;
    function treeMin(p: P_AATreeItem): P_AATreeItem;
    function treeMax(p: P_AATreeItem): P_AATreeItem;
    function treeSuccessor(p: P_AATreeItem): P_AATreeItem;
    function treePredecessor(p: P_AATreeItem): P_AATreeItem;
    procedure treeClear(p: P_AATreeItem);
    function treeDelete(x: P_AATreeItem; var t: P_AATreeItem): boolean;
    function treeFind(const AKey: TKey): P_AATreeItem;
    procedure treeMove(Src, Dst: P_AATreeItem); inline;
    procedure treeReplace(Src, Dst: P_AATreeItem);

    function GetKey(AHandle: TItemHandle): TKey;
    function GetValue(AHandle: TItemHandle): TValue;
    procedure SetValue(AHandle: TItemHandle; const AValue: TValue);
    function GetValueByKey(const AKey: TKey): TValue;
    procedure SetValueByKey(const AKey: TKey; const AValue: TValue);

  public
    constructor Create; overload;
    constructor Create(AComparer: IComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    procedure Clear;
    function Add(const AKey: TKey; const AValue: TValue): TItemHandle; overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    function AddOrSetValue(const AKey: TKey; const AValue: TValue): TItemHandle;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Delete(AHandle: TItemHandle);
    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKeys: array of TKey); overload;
    procedure Remove(const AKeys: TEnumerable<TKey>); overload;
    function ContainsKey(const Key: TKey): Boolean; overload;
    function ContainsKeys(const AKeys: array of TKey; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue> = nil): Boolean;
    function MinKey: TKey;
    function MaxKey: TKey;

    // Enumeration of all items from min to max and in reverse order.
    function FindMin: TItemHandle;
    function FindMax: TItemHandle;
    function Find(const AKey: TKey): TItemHandle;
    function First(var AHandle: TItemHandle): Boolean;
    function Prev(var AHandle: TItemHandle): Boolean;
    function Next(var AHandle: TItemHandle): Boolean;

    property TreeHeight: integer read treeFullHeight;
    property Keys[AHandle: TItemHandle]: TKey read GetKey;
    property Values[AHandle: TItemHandle]: TValue read GetValue write SetValue;
    property Items[const AKey: TKey]: TValue read GetValueByKey write SetValueByKey; default;
    property Count: integer read FCount;
  end;

  TMap<TKey,TValue> = Class(TEnumerable<TPair<TKey,TValue>>)
  protected
    FTree: TAATree<TKey,TValue>;

    type
      TPairEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
      protected
        [Weak] FTree: TAATree<TKey,TValue>;
        FCurrentItem: TItemHandle;
        FInEnumKey: boolean;

        function DoGetCurrent: TPair<TKey,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMultimap: TMultimap<TKey,TValue>);
      end;

      TKeyEnumerator = class(TEnumerator<TKey>)
      private
        FPairEnumerator: TPairEnumerator;

        function GetCurrent: TKey;
      protected
        function DoGetCurrent: TKey; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMap: TMap<TKey,TValue>);
        property Current: TKey read GetCurrent;
        function MoveNext: Boolean;
      end;

      TValueEnumerator = class(TEnumerator<TValue>)
      private
        FPairEnumerator: TPairEnumerator;

        function GetCurrent: TValue;
      protected
        function DoGetCurrent: TValue; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMap: TMap<TKey,TValue>);
        property Current: TValue read GetCurrent;
        function MoveNext: Boolean;
      end;

      TKeyCollection = class(TEnumerable<TKey>)
      private
        [Weak] FDictionary: TDictionary<TKey,TValue>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TKey>; override;
      public
        constructor Create(const ADictionary: TDictionary<TKey,TValue>);
        function GetEnumerator: TKeyEnumerator; reintroduce;
        function ToArray: TArray<TKey>; override; final;
        property Count: Integer read GetCount;
      end;

      TValueCollection = class(TEnumerable<TValue>)
      private
        [Weak] FDictionary: TDictionary<TKey,TValue>;
        function GetCount: Integer;
      protected
        function DoGetEnumerator: TEnumerator<TValue>; override;
      public
        constructor Create(const ADictionary: TDictionary<TKey,TValue>);
        function GetEnumerator: TValueEnumerator; reintroduce;
        function ToArray: TArray<TValue>; override; final;
        property Count: Integer read GetCount;
      end;

  public
    constructor Create; overload;
    constructor Create(AComparer: IComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue);
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKeys: array of TKey); overload;
    procedure Remove(const AKeys: TEnumerable<TKey>); overload;
    function ContainsKey(const Key: TKey): Boolean; overload;
    function ContainsKeys(const AKeys: array of TKey; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue> = nil): Boolean;
    function MinKey: TKey;
    function MaxKey: TKey;

    function Next(const AKey: TKey; var ANewKey: TKey): Boolean;
    function Prev(const AKey: TKey; var ANewKey: TKey): Boolean;

    property TreeHeight: integer read GetTreeHeight;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property Items[const AKey: TKey]: TValue read GetItem write SetItem ; default;
    property Count: integer read GetCount;
  end;

//  TSet<TKey> = Class(TAATree<TKey,TEmptyRec>)
//  End;

  TBinarySearchTree<TKey,TValue> = Class(TAATree<TKey,TValue>);

implementation

{ TSet<TValue> }

constructor TUnsortedSet<TValue>.Create(ACapacity: integer = 0; const AComparer: IEqualityComparer<TValue> = nil);
begin
  FSet := TDictionary<TValue, TEmptyRec>.Create(ACapacity, AComparer);
end;

constructor TUnsortedSet<TValue>.Create(const AValues: array of TValue; const AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TUnsortedSet<TValue>.Create(const AValues: TEnumerable<TValue>; const AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

destructor TUnsortedSet<TValue>.Destroy;
begin
  FreeAndNil(FSet);
  inherited;
end;

function TUnsortedSet<TValue>.DoGetEnumerator: TEnumerator<TValue>;
begin
  Result := GetEnumerator;
end;

function TUnsortedSet<TValue>.GetCount: integer;
begin
  result := FSet.Count;
end;

function TUnsortedSet<TValue>.GetEnumerator: TEnumerator;
begin
  result := FSet.Keys.GetEnumerator;
end;

procedure TUnsortedSet<TValue>.Remove(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Remove(ASet[i]);
end;

procedure TUnsortedSet<TValue>.Remove(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Remove(Item);
end;

procedure TUnsortedSet<TValue>.Add(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.Add(AValue, R);
end;

procedure TUnsortedSet<TValue>.Add(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Add(ASet[i]);
end;

procedure TUnsortedSet<TValue>.Add(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Add(Item);
end;

procedure TUnsortedSet<TValue>.Include(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.AddOrSetValue(AValue, R);
end;

procedure TUnsortedSet<TValue>.Include(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Include(ASet[i]);
end;

procedure TUnsortedSet<TValue>.Include(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Include(Item);
end;

procedure TUnsortedSet<TValue>.Clear;
begin
  FSet.Clear;
end;

function TUnsortedSet<TValue>.Contains(const AValue: TValue): boolean;
begin
  result := FSet.ContainsKey(AValue);
end;

function TUnsortedSet<TValue>.Contains(const ASet: array of TValue): boolean;
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    if not Contains(ASet[i]) then
      Exit(False);
  result := True;
end;

function TUnsortedSet<TValue>.Contains(const AValues: TEnumerable<TValue>): boolean;
var
  Item: TValue;
begin
  for Item in AValues do
    if not Contains(Item) then
      Exit(False);
  result := True;
end;

procedure TUnsortedSet<TValue>.Remove(const AValue: TValue);
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
  ValueSet: TUnsortedSet<TValue>;
  i: Integer;
begin
  if AComparer=nil then
    AComparer := TEqualityComparer<TValue>.Default;
  ValueSet := TUnsortedSet<TValue>.Create(0, AComparer);
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
  ValueSet: TUnsortedSet<TValue>;
  V: TValue;
begin
  if AComparer=nil then
    AComparer := TEqualityComparer<TValue>.Default;
  ValueSet := TUnsortedSet<TValue>.Create(0, AComparer);
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

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TUnsortedSet<TValue>);
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
  s: TUnsortedSet<TValue>;
begin
  s := TUnsortedSet<TValue>.Create(AValues);
  try
    RemoveValues(AKey, s);
  finally
    FReeAndNil(s);
  end;
end;

procedure TMultimap<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>);
var
  s: TUnsortedSet<TValue>;
begin
  s := TUnsortedSet<TValue>.Create(AValues);
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

{ TAATree<TKey, TValue> }

constructor TAATree<TKey, TValue>.Create;
begin
  Create(TComparer<TKey>.Default);
end;

constructor TAATree<TKey, TValue>.Create(AComparer: IComparer<TKey>);
begin
  inherited Create;
  FComparer := AComparer;
  FBottom := AllocNewItem;
  FBottom.Level := 0;
  FBottom.Left := FBottom;
  FBottom.Right := FBottom;
  FDeleted := FBottom;
  FRoot := FBottom;
  FLast := FBottom;
end;

constructor TAATree<TKey, TValue>.Create(const ACollection: TEnumerable<TPair<TKey,TValue>>;
  const AComparer: IComparer<TKey> = nil);
begin
  Create(AComparer);
  Add(ACollection);
end;

destructor TAATree<TKey, TValue>.Destroy;
begin
  Clear;
  ReleaseItem(FBottom);
  FBottom := nil;
  FDeleted := nil;
  FRoot := nil;
  inherited;
end;

function TAATree<TKey, TValue>.AllocNewItem: P_AATreeItem;
begin
  result := AllocMem(SizeOf(T_AATreeItem));
end;

procedure TAATree<TKey, TValue>.ReleaseItem(p: P_AATreeItem);
begin
  if p=nil then
    Exit;
  p.Data.Key := Default(TKey);
  p.Data.Value := Default(TValue);
  FreeMem(p);
end;

function TAATree<TKey, TValue>.PtrTohandle(p: P_AATreeItem): TItemHandle;
begin
  if (p=nil) or (p=FBottom) then
    Result := -1
  else
    Result := TItemHandle(p);
end;

procedure TAATree<TKey, TValue>.Clear;
begin
  if FCount=0 then
    exit;
  treeClear(FRoot);
  FBottom.Level := 0;
  FBottom.Left := FBottom;
  FBottom.Right := FBottom;
  FDeleted := FBottom;
  FRoot := FBottom;
  FCount := 0;
end;

function TAATree<TKey, TValue>.treeAdd(p, aparent: P_AATreeItem; var Dst: P_AATreeItem): Boolean;
var
  r: integer;
begin
  if Dst = FBottom then
    with p^ do
    begin
      Parent := AParent;
      Left := FBottom;
      Right := FBottom;
      Level := 1;
      Dst := p;
      result := true;
      exit;
    end;
  r := FComparer.Compare(p.Data.Key, Dst.Data.Key);
  if r>0 then
    result := treeAdd(p, Dst, Dst.right)
  else
  if r<0 then
    result := treeAdd(p, Dst, Dst.left)
  else
    result := false;
  if not result then
    exit;
  treeSkew(Dst);
  treeSplit(Dst);
end;

procedure TAATree<TKey, TValue>.treeClear(p: P_AATreeItem);
begin
  if (p=nil) or (p=FBottom) then
    exit;
  treeClear(p.Left);
  treeClear(p.Right);
  releaseItem(p);
end;

function TAATree<TKey, TValue>.treeDelete(x: P_AATreeItem; var t: P_AATreeItem): boolean;
begin
  result := false;
  if (t=nil) or (t=FBottom) then
    exit;

  // search down the tree and set pointers last and deleted
  Flast := t;
  if FComparer.Compare(x.Data.Key, t.Data.Key) < 0 then
    result := treeDelete(x, t.Left)
  else
  begin
    FDeleted := t;
    result := treeDelete(x, t.Right);
  end;

  // At the bottom of the tree we remove the element (if it exists)
  if (t = FLast) and (FDeleted <> FBottom) and (FComparer.Compare(x.Data.Key, FDeleted.Data.Key)=0) then
  begin
    // We copy key, because it is necessary to rebalance the tree and move
    // FDeleted into right position (position for FLast).
    if FLast<>FDeleted then
      FDeleted.Data.Key := FLast.Data.Key;
    t.Right.Parent := t.Parent;
    t := t.Right;
    result := true;
  end
  else
    // On the way back, we rebalance
    if (t.Left.Level < t.Level-1) or (t.Right.Level < t.Level-1) then
    begin
      dec(t.Level);
      if t.Right.Level > t.Level then
        t.right.level := t.level;
      treeSkew(t);
      treeSkew(t.right);
      treeSkew(t.right.right);
      treeSplit(t);
      treeSplit(t.right);
    end;
end;

function TAATree<TKey, TValue>.treeFind(const AKey: TKey): P_AATreeItem;
var n: integer;
begin
  result := FRoot;
  while result<>FBottom do
  begin
    n := FComparer.Compare(AKey, result.Data.Key);
    if n<0 then
      result := result.Left
    else
      if n>0 then
        result := result.Right
      else
        exit;
  end;
end;

function TAATree<TKey, TValue>.treeFullHeight: integer;
begin
  result := treeGetHeight(FRoot);
end;

function TAATree<TKey, TValue>.treeGetHeight(p: P_AATreeItem): integer;
begin
  if p=FBottom then
    result := 0
  else
    result := Max(treeGetHeight(p.Left), treeGetHeight(p.Right)) + 1;
end;

function TAATree<TKey, TValue>.treeMax(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
  begin
    result := p;
    while result.Right <> FBottom do
      result := result.Right;
  end;
end;

function TAATree<TKey, TValue>.treeMin(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
  begin
    result := p;
    while result.Left <> FBottom do
      result := result.Left;
  end;
end;

procedure TAATree<TKey, TValue>.treeMove(Src, Dst: P_AATreeItem);
begin
  Dst.Data := Src.Data;
end;

function TAATree<TKey, TValue>.treePredecessor(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
    if p.Left <> FBottom then
      result := treeMax(p.Left)
    else
    begin
      result := p.Parent;
      while (result<>FBottom) and (p=result.Left) do
      begin
        p := result;
        result := result.Parent;
      end;
    end;
end;

function TAATree<TKey, TValue>.treeSuccessor(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
    if p.Right <> FBottom then
      result := treeMin(p.Right)
    else
    begin
      result := p.Parent;
      while (result<>FBottom) and (p=result.Right) do
      begin
        p := result;
        result := result.Parent;
      end;
    end;
end;

function TAATree<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
var
  h: TItemHandle;
begin
  h := Find(Key);
  result := h<>-1;
  if result then
    Value := Values[h];
end;

// replace position of Dst item in the tree with Src item
procedure TAATree<TKey, TValue>.treeReplace(Src, Dst: P_AATreeItem);
begin
  Src.Parent := Dst.Parent;
  Src.Left := Dst.Left;
  Src.Right := Dst.Right;
  Src.Level := Dst.Level;

  // root item has "parent=FBottom"
  // but FBottom.left/right MUST refer to FBottom
  if Src.Parent<>FBottom then
    if Src.Parent.Left=Dst then
      Src.Parent.Left := Src
    else
      Src.Parent.Right := Src;
  Src.Left.Parent := Src;
  Src.Right.Parent := Src;

  if FRoot=Dst then
    FRoot := Src;
end;

{
  Src: 1(p)   Dst:  2(p)
       / \          / \
     2(t) X        Y  1(t)
     / \              / \
    Y   Z            Z   X
}
procedure TAATree<TKey, TValue>.treeSkew(var p: P_AATreeItem);
var
  t: P_AATreeItem;
begin
  if (p.Left.Level = p.Level) then
  begin

    // change Right&Left links
    t := p;
    p := p.left;
    t.left := p.right;
    p.right := t;

    // change Parent links
    p.Parent := t.Parent;
    t.Parent := p;
    t.Left.Parent := t;
  end;
end;

{
  Src: 1(p)     Dst:  2(p)
       / \            /  \
     X  2(t)        1(t)  Z
        /  \        /  \
       Y    Z      X    Y
}
procedure TAATree<TKey, TValue>.treeSplit(var p: P_AATreeItem);
var
  t: P_AATreeItem;
begin
  if (p.Right.Right.Level = p.Level) then
  begin

    // change Right&Left links
    t := p;
    p := p.Right;
    t.Right := p.Left;
    p.Left := t;
    inc(p.Level);

    // change Parent links
    p.Parent := t.Parent;
    t.Parent := p;
    t.Right.Parent := t;
  end;
end;

function TAATree<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue): TItemHandle;
var
  p: P_AATreeItem;
begin
  p := AllocNewItem;
  p.Data.Key := AKey;
  p.Data.Value := AValue;
  if treeAdd(p, FBottom, FRoot) then
  begin
    result := TItemHandle(p);
    inc(FCount);
  end
  else
  begin
    ReleaseItem(p);
    result := -1;
  end;
end;

procedure TAATree<TKey, TValue>.Add(const ACollection: TEnumerable<TPair<TKey,TValue>>);
var
  Pair: TPair<TKey,TValue>;
begin
  for Pair in ACollection do
    Add(Pair.Key, Pair.Value);
end;

function TAATree<TKey, TValue>.AddOrSetValue(const AKey: TKey; const AValue: TValue): TItemHandle;
begin
  result := Find(AKey);
  if result=-1 then
    result := Add(AKey, AValue)
  else
    Values[result] := AValue;
end;

procedure TAATree<TKey, TValue>.Delete(AHandle: TItemHandle);
begin
  if AHandle=-1 then
    raise Exception.Create('Error');

  if not treeDelete(P_AATreeItem(AHandle), FRoot) then
    exit;

  // content of FDeleted has cleaned & replaced with key from FLast
  // now we move FLast into position of FDeleted
  if FLast<>FDeleted then
  begin
    treeMove(FLast, FDeleted);
    treeReplace(FLast, FDeleted);
  end;

  // ItemDeleted for deleted item has called from treeDelete
  // so we must not call it again
  ReleaseItem(FDeleted);
  FDeleted := FBottom;
  FLast := FBottom;

  dec(FCount);
end;

function TAATree<TKey, TValue>.Find(const AKey: TKey): TItemHandle;
begin
  result := PtrToHandle( treeFind(AKey) );
end;

function TAATree<TKey, TValue>.FindMax: TItemHandle;
begin
  result := PtrTohandle( treeMax(FRoot) );
end;

function TAATree<TKey, TValue>.FindMin: TItemHandle;
begin
  result := PtrTohandle( treeMin(FRoot) );
end;

function TAATree<TKey, TValue>.First(var AHandle: TItemHandle): Boolean;
begin
  AHandle := PtrTohandle( treeMin(FRoot) );
  result := AHandle<>-1;
end;

function TAATree<TKey, TValue>.Prev(var AHandle: TItemHandle): Boolean;
begin
  AHandle := PtrToHandle( treePredecessor(P_AATreeItem(AHandle)) );
  result := AHandle<>-1;
end;

function TAATree<TKey, TValue>.Next(var AHandle: TItemHandle): Boolean;
begin
  AHandle := PtrToHandle( treeSuccessor(P_AATreeItem(AHandle)) );
  result := AHandle<>-1;
end;

function TAATree<TKey, TValue>.GetKey(AHandle: TItemHandle): TKey;
begin
  result := P_AATreeItem(AHandle).Data.Key;
end;

function TAATree<TKey, TValue>.GetValue(AHandle: TItemHandle): TValue;
begin
  if AHandle<>-1 then
    result := P_AATreeItem(AHandle).Data.Value
  else
    raise Exception.Create('Error');
end;

procedure TAATree<TKey, TValue>.SetValue(AHandle: TItemHandle; const AValue: TValue);
begin
  if AHandle<>-1 then
    P_AATreeItem(AHandle).Data.Value := AValue
  else
    raise Exception.Create('Error');
end;

function TAATree<TKey, TValue>.GetValueByKey(const AKey: TKey): TValue;
begin
  result := Values[Find(AKey)];
end;

procedure TAATree<TKey, TValue>.SetValueByKey(const AKey: TKey; const AValue: TValue);
var
  h: TItemHandle;
begin
  h := Find(AKey);
  if h=-1 then
    Add(AKey, AValue)
  else
    Values[h] := AValue;
end;

function TAATree<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  result := Find(Key)<>-1;
end;

function TAATree<TKey, TValue>.ContainsKeys(const AKeys: array of TKey;
  ASearchType: TSearchType): Boolean;
var
  i: Integer;
begin
  case ASearchType of
    stAll:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if not ContainsKey(AKeys[i]) then
            Exit(False);
        Exit(True);
      end;
    stAny:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if ContainsKey(AKeys[i]) then
            Exit(True);
        Exit(False);
      end;
  end;
end;

function TAATree<TKey, TValue>.ContainsKeys(const AKeys: TEnumerable<TKey>;
  ASearchType: TSearchType): Boolean;
var
  Key: TKey;
begin
  case ASearchType of
    stAll:
      begin
        for Key in AKeys do
          if not ContainsKey(Key) then
            Exit(False);
        Exit(True);
      end;
    stAny:
      begin
        for Key in AKeys do
          if ContainsKey(Key) then
            Exit(True);
        Exit(False);
      end;
  end;
end;

function TAATree<TKey, TValue>.ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue> = nil): Boolean;
var
  h: TItemHandle;
begin
  if AEqualityComparer=nil then
    AEqualityComparer := TEqualityComparer<TValue>.Default;
  if not First(h) then
    result := False
  else
    repeat
      result := AEqualityComparer.Equals(Value, Values[h]);
    until result or not Next(h);
end;

function TAATree<TKey, TValue>.MinKey: TKey;
begin
  result := Keys[FindMin];
end;

function TAATree<TKey, TValue>.MaxKey: TKey;
begin
  result := Keys[FindMax];
end;

procedure TAATree<TKey, TValue>.Remove(const AKey: TKey);
begin
  Delete(Find(AKey));
end;

procedure TAATree<TKey, TValue>.Remove(const AKeys: array of TKey);
var
  i: Integer;
begin
  for i := Low(AKeys) to High(AKeys) do
    Remove(AKeys[i]);
end;

procedure TAATree<TKey, TValue>.Remove(const AKeys: TEnumerable<TKey>);
var
  Key: TKey;
begin
  for Key in AKeys do
    Remove(Key);
end;

end.
