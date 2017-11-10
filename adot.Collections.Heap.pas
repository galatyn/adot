unit adot.Collections.Heap;

interface

{
  TBHeap<TKey, TValue>            Binary heap functions
  TBinaryHeapClass<TKey,TValue>   Binary heap implementation for pairs <TKey,TValues>
  TBinaryHeapClass<TKey>          Binary heap implementation for <TKey>
}

uses
  adot.Types,
  adot.Collections.Types,
  adot.Collections.Vectors,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils;

type
  { Low level heap operations on array.}
  TBHeap<TKey, TValue> = class
    class function GetLeft(ParentIdx: integer): integer; static;
    class function GetRight(ParentIdx: integer): integer; static;
    class function GetParent(ChildIdx: integer): integer; static;
    class procedure MoveUp(var Items: TArray<TPair<TKey,TValue>>; ItemIndex: integer; Comparer: IComparer<TKey>); static;
    class procedure Delete(var Items: TArray<TPair<TKey,TValue>>; ItemIndex,Count: integer; Comparer: IComparer<TKey>); static;
    class function ExtractMin(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>):TPair<TKey,TValue>; static;
    class procedure Build(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>); static;
    class procedure Sort(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>); static;
    class procedure Replace(var Items: TArray<TPair<TKey,TValue>>; ItemIndex,Count: integer;
      Comparer: IComparer<TKey>; const Pair:TPair<TKey,TValue>); static;
  end;

  { Binary heap of pairs [Key;Value]. }
  TBinaryHeapClass<TKey,TValue> = class(TEnumerableExt<TPair<TKey,TValue>>)
  public
    type
      TPairsEnumerator = TArr<TPair<TKey,TValue>>.TEnumerator;

      {# Enumerator for heap (we have to inherit from TEnumerator to be comatible with TEnumerable) }
      TBinaryHeapEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
      protected
        PairEnumerator: TPairsEnumerator;

        function DoGetCurrent: TPair<TKey, TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const PairEnumerator: TPairsEnumerator);
      end;

      {# Enumerator of keys }
      TKeyEnumerator = record
      private
        PairEnumerator: TPairsEnumerator;

        function GetCurrent: TKey;

      public
        procedure Init(const APairEnumerator: TPairsEnumerator);

        function MoveNext: Boolean;
        property Current: TKey read GetCurrent;
      end;

      {# Collection of keys }
      TKeyCollection = record
      private
        PairEnumerator: TPairsEnumerator;

      public
        procedure Init(const APairEnumerator: TPairsEnumerator);
        function GetEnumerator: TKeyEnumerator;
      end;

  protected
    FItems: TArr<TPair<TKey,TValue>>;
    FComparer: IComparer<TKey>;
    FOwnerships: TDictionaryOwnerships;

    function GetCapacity: integer;
    function GetCount: integer;
    procedure SetCapacity(ACapacity: integer);
    procedure SetValue(n: integer; const Value: TPair<TKey, TValue>);
    function GetValue(Index: integer): TPair<TKey,TValue>;
    function GetKeyCollection: TKeyCollection;
    function DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>; override;
    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);
    procedure DisposeItem(n: integer);
    procedure DisposeAll;

    { this function is O(N), avoid of use it }
    function Find(const Key: TKey): integer;

  public
    constructor Create(ACapacity: integer = 0); overload;
    constructor Create(ACapacity: integer; AComparer: IComparer<TKey>); overload;
    constructor Create(ACapacity: integer; AComparison: TComparison<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; ACapacity: integer = 0; AComparison: TComparison<TKey> = nil); overload;
    destructor Destroy; override;

    function Add(const Key: TKey; const Value: TValue): integer; overload;
    function Add(const Pair: TPair<TKey,TValue>): integer; overload;
    procedure Add(const Values: TArray<TPair<TKey,TValue>>); overload;
    procedure Add(const Values: TEnumerable<TPair<TKey,TValue>>); overload;

    function MinValue: TPair<TKey,TValue>;    { O(1)      }
    function ExtractMin: TPair<TKey,TValue>;  { O(Log(n)) }
    procedure DeleteMin;
    procedure Delete(n: integer);

    procedure Clear;
    procedure TrimExcess;

    function Empty: boolean;

    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Values[n: integer]: TPair<TKey,TValue> read GetValue write SetValue; default;
    property Keys: TKeyCollection read GetKeyCollection;
    property Comparer: IComparer<TKey> read FComparer;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  { Binary heap with key only. }
  TBinaryHeapClass<TKey> = class(TEnumerableExt<TKey>)
  protected
    type

      {# Enumerator for heap (we have to inherit from TEnumerator to be comatible with TEnumerable) }
      TBinaryHeapEnumerator = class(TEnumerator<TKey>)
      protected
        FPairEnumerator: TEnumerator<TPair<TKey, TEmptyRec>>;

        function DoGetCurrent: TKey; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const PairEnumerator: TEnumerator<TPair<TKey, TEmptyRec>>);
        destructor Destroy; override;
      end;

    var
      FHeap: TBinaryHeapClass<TKey,TEmptyRec>;

    function DoGetEnumerator: TEnumerator<TKey>; override;
    function GetCapacity: integer;
    function GetCount: integer;
    function GetValue(n: integer): TKey;
    procedure SetCapacity(const Value: integer);
    procedure SetValue(n: integer; const Value: TKey);

    { this function is O(N), avoid of use it }
    function Find(const Key: TKey): integer;

  public
    constructor Create(ACapacity: integer = 0); overload;
    constructor Create(ACapacity: integer; AComparer: IComparer<TKey>); overload;
    constructor Create(ACapacity: integer; AComparison: TComparison<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TKey>; ACapacity: integer = 0; AComparison: TComparison<TKey> = nil); overload;
    destructor Destroy; override;

    function Add(const Value: TKey): integer; overload;
    procedure Add(const Values: TArray<TKey>); overload;
    procedure Add(const Values: TEnumerable<TKey>); overload;

    function MinValue: TKey;    { O(1)      }
    function ExtractMin: TKey;  { O(Log(n)) }

    procedure DeleteMin;
    procedure Delete(n: integer);

    procedure Clear;
    procedure TrimExcess;
    function Empty: boolean;

    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Values[n: integer]: TKey read GetValue write SetValue; default;
  end;

implementation

uses
  adot.Tools,
  adot.Collections,
  adot.Tools.RTTI;

{ TBHeap<TKey, TValue> }

class function TBHeap<TKey, TValue>.GetLeft(ParentIdx: integer): integer;
begin
  result := (ParentIdx shl 1) + 1;
end;

class function TBHeap<TKey, TValue>.GetRight(ParentIdx: integer): integer;
begin
  result := (ParentIdx shl 1) + 2;
end;

class function TBHeap<TKey, TValue>.GetParent(ChildIdx: integer): integer;
begin
  result := (ChildIdx - 1) shr 1;
end;

class procedure TBHeap<TKey, TValue>.MoveUp(var Items: TArray<TPair<TKey, TValue>>;
  ItemIndex: integer; Comparer: IComparer<TKey>);
var
  ParentIndex: Integer;
  Value: TPair<TKey, TValue>;
begin
  while ItemIndex > 0 do
  begin
    ParentIndex := GetParent(ItemIndex);
    if Comparer.Compare(Items[ParentIndex].Key, Items[ItemIndex].Key)<=0 then
      Break;
    Value := Items[ParentIndex];
    Items[ParentIndex] := Items[ItemIndex];
    Items[ItemIndex] := Value;
    ItemIndex := ParentIndex;
  end;
end;

class procedure TBHeap<TKey, TValue>.Replace(var Items: TArray<TPair<TKey, TValue>>; ItemIndex, Count: integer;
  Comparer: IComparer<TKey>; const Pair: TPair<TKey, TValue>);
begin
  Delete(Items, ItemIndex,Count, Comparer);
  Items[Count-1] := Pair;
  MoveUp(Items, Count-1, Comparer);
end;

class procedure TBHeap<TKey, TValue>.Delete(var Items: TArray<TPair<TKey, TValue>>;
  ItemIndex,Count: integer; Comparer: IComparer<TKey>);
var
  L,R: integer;
begin
  repeat
    L := GetLeft(ItemIndex);
    if L >= Count then
      Break;
    R := GetRight(ItemIndex);
    if R >= Count then
    begin
      Items[ItemIndex] := Items[L];
      ItemIndex := L;
      Break;
    end;
    if Comparer.Compare(Items[L].Key, Items[R].Key) < 0 then
    begin
      Items[ItemIndex] := Items[L];
      ItemIndex := L;
    end
    else
    begin
      Items[ItemIndex] := Items[R];
      ItemIndex := R;
    end;
  until False;
  if ItemIndex < Count-1 then
  begin
    Items[ItemIndex] := Items[Count-1];
    MoveUp(Items, ItemIndex, Comparer);
  end;
end;

class function TBHeap<TKey, TValue>.ExtractMin(var Items: TArray<TPair<TKey, TValue>>;
  Count: integer; Comparer: IComparer<TKey>): TPair<TKey, TValue>;
begin
  result := Items[0];
  Delete(Items, 0, Count, Comparer);
end;

class procedure TBHeap<TKey, TValue>.Build(var Items: TArray<TPair<TKey, TValue>>;
  Count: integer; Comparer: IComparer<TKey>);
var
  I: Integer;
begin
  for I := Low(Items)+1 to Count-1 do
    MoveUp(Items, I, Comparer);
end;

class procedure TBHeap<TKey, TValue>.Sort(var Items: TArray<TPair<TKey, TValue>>;
  Count: integer; Comparer: IComparer<TKey>);
var
  I: Integer;
begin
  Build(Items, Count, Comparer);
  for I := Length(Items)-1 downto 0 do
    Items[I] := ExtractMin(Items, I+1, Comparer);
  TArrayUtils.Inverse<TPair<TKey, TValue>>(Items);
end;

{ TBinaryHeapClass<TKey, TValue>.TEnumerator }

constructor TBinaryHeapClass<TKey, TValue>.TBinaryHeapEnumerator.Create(const PairEnumerator: TPairsEnumerator);
begin
  inherited Create;
  Self.PairEnumerator := PairEnumerator;
end;

function TBinaryHeapClass<TKey, TValue>.TBinaryHeapEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  result := PairEnumerator.Current;
end;

function TBinaryHeapClass<TKey, TValue>.TBinaryHeapEnumerator.DoMoveNext: Boolean;
begin
  result := PairEnumerator.MoveNext;
end;

{ TBinaryHeapClass<TKey, TValue>.TKeyEnumerator }

procedure TBinaryHeapClass<TKey, TValue>.TKeyEnumerator.Init(const APairEnumerator: TPairsEnumerator);
begin
  Self := Default(TKeyEnumerator);
  PairEnumerator := APairEnumerator;
end;

function TBinaryHeapClass<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  result := PairEnumerator.Current.Key;
end;

function TBinaryHeapClass<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  result := PairEnumerator.MoveNext;
end;

{ TBinaryHeapClass<TKey, TValue>.TKeyCollection }

procedure TBinaryHeapClass<TKey, TValue>.TKeyCollection.Init(const APairEnumerator: TPairsEnumerator);
begin
  Self := Default(TKeyCollection);
  PairEnumerator := APairEnumerator;
end;

function TBinaryHeapClass<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  result.Init(PairEnumerator);
end;

{ TBinaryHeapClass<TKey, TValue> }

constructor TBinaryHeapClass<TKey, TValue>.Create(ACapacity: integer);
begin
  inherited Create;
  Capacity := ACapacity;
  FComparer := TComparerUtils.DefaultComparer<TKey>;
end;

constructor TBinaryHeapClass<TKey, TValue>.Create(ACapacity: integer; AComparer: IComparer<TKey>);
begin
  inherited Create;
  Capacity := ACapacity;
  if AComparer=nil
    then FComparer := TComparerUtils.DefaultComparer<TKey>
    else FComparer := AComparer;
end;

constructor TBinaryHeapClass<TKey, TValue>.Create(ACapacity: integer; AComparison: TComparison<TKey>);
var
  C: IComparer<TKey>;
begin
  if Assigned(AComparison)
    then C := TDelegatedComparer<TKey>.Create(AComparison)
    else C := nil;
  Create(ACapacity, C);
end;

constructor TBinaryHeapClass<TKey, TValue>.Create(const ACollection: TEnumerable<TPair<TKey, TValue>>;
  ACapacity: integer; AComparison: TComparison<TKey>);
begin
  Create(ACapacity, AComparison);
  Add(ACollection);
end;

destructor TBinaryHeapClass<TKey, TValue>.Destroy;
begin
  Clear;
  FComparer := nil;
  inherited;
end;

procedure TBinaryHeapClass<TKey, TValue>.Clear;
begin
  DisposeAll;
  FItems.Clear;
end;

procedure TBinaryHeapClass<TKey, TValue>.DeleteMin;
begin
  Delete(0);
end;

procedure TBinaryHeapClass<TKey, TValue>.Delete(n: integer);
begin
  DisposeItem(n);
  TBHeap<TKey, TValue>.Delete(FItems.Items, n, Count, FComparer);
  FItems.Delete(Count-1);
end;

function TBinaryHeapClass<TKey, TValue>.Empty: boolean;
begin
  result := FItems.Count=0;
end;

function TBinaryHeapClass<TKey, TValue>.Find(const Key: TKey): integer;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    if FComparer.Compare(FItems.Items[I].Key, Key)=0 then
      Exit(I);
  result := -1;
end;

function TBinaryHeapClass<TKey, TValue>.GetCapacity: integer;
begin
  result := FItems.Capacity;
end;

function TBinaryHeapClass<TKey, TValue>.GetCount: integer;
begin
  result := FItems.Count;
end;

function TBinaryHeapClass<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  result := TBinaryHeapEnumerator.Create(FItems.GetEnumerator);
end;

function TBinaryHeapClass<TKey, TValue>.GetKeyCollection: TKeyCollection;
begin
  result.Init( FItems.GetEnumerator );
end;

function TBinaryHeapClass<TKey, TValue>.GetOwnsKeys: boolean;
begin
  result := doOwnsKeys in FOwnerships;
end;

function TBinaryHeapClass<TKey, TValue>.GetOwnsValues: boolean;
begin
  result := doOwnsValues in FOwnerships;
end;

procedure TBinaryHeapClass<TKey, TValue>.SetOwnsKeys(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TKey> then
    raise Exception.Create('Generic type is not a class.');
  if Value
    then include(FOwnerships, doOwnsKeys)
    else exclude(FOwnerships, doOwnsKeys);
end;

procedure TBinaryHeapClass<TKey, TValue>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TValue> then
    raise Exception.Create('Generic type is not a class.');
  if Value
    then include(FOwnerships, doOwnsValues)
    else exclude(FOwnerships, doOwnsValues);
end;

function TBinaryHeapClass<TKey, TValue>.GetValue(Index: integer): TPair<TKey, TValue>;
begin
  result := FItems.Items[Index];
end;

procedure TBinaryHeapClass<TKey, TValue>.SetCapacity(ACapacity: integer);
begin
  FItems.Capacity := ACapacity;
end;

procedure TBinaryHeapClass<TKey, TValue>.DisposeAll;
var
  I: Integer;
begin
  if FOwnerships<>[] then
    for I := 0 to FItems.Count-1 do
    begin
      if doOwnsKeys in FOwnerships then
        PObject(@FItems.Items[I].Key)^.DisposeOf;
      if doOwnsValues in FOwnerships then
        PObject(@FItems.Items[I].Value)^.DisposeOf;
    end;
end;

procedure TBinaryHeapClass<TKey, TValue>.DisposeItem(n: integer);
begin
  if FOwnerships<>[] then
  begin
    if doOwnsKeys in FOwnerships then
      PObject(@FItems.Items[n].Key)^.DisposeOf;
    if doOwnsValues in FOwnerships then
      PObject(@FItems.Items[n].Value)^.DisposeOf;
  end;
end;

procedure TBinaryHeapClass<TKey, TValue>.SetValue(n: integer; const Value: TPair<TKey, TValue>);
begin
  DisposeItem(n);
  TBHeap<TKey, TValue>.Replace(FItems.Items, n, Count, FComparer, Value);
end;

procedure TBinaryHeapClass<TKey, TValue>.TrimExcess;
begin
  FItems.TrimExcess;
end;

function TBinaryHeapClass<TKey, TValue>.Add(const Pair: TPair<TKey, TValue>): integer;
begin
  result := FItems.Add(Pair);
  TBHeap<TKey, TValue>.MoveUp(FItems.Items, result, FComparer);
end;

function TBinaryHeapClass<TKey, TValue>.Add(const Key: TKey; const Value: TValue): integer;
var
  P: TPair<TKey,TValue>;
begin
  P.Key := Key;
  P.Value := Value;
  result := FItems.Add(P);
  TBHeap<TKey, TValue>.MoveUp(FItems.Items, result, FComparer);
end;

procedure TBinaryHeapClass<TKey, TValue>.Add(const Values: TEnumerable<TPair<TKey, TValue>>);
var
  Pair: TPair<TKey, TValue>;
  I: Integer;
begin
  for Pair in Values do
  begin
    I := FItems.Add(Pair);
    TBHeap<TKey, TValue>.MoveUp(FItems.Items, I, FComparer);
  end;
end;

procedure TBinaryHeapClass<TKey, TValue>.Add(const Values: TArray<TPair<TKey, TValue>>);
var
  I,J: Integer;
begin
  for I := Low(Values) to High(Values) do
  begin
    J := FItems.Add(Values[I]);
    TBHeap<TKey, TValue>.MoveUp(FItems.Items, J, FComparer);
  end;
end;

function TBinaryHeapClass<TKey, TValue>.ExtractMin: TPair<TKey, TValue>;
begin
  result := FItems.Items[0];
  DeleteMin;
end;

function TBinaryHeapClass<TKey, TValue>.MinValue: TPair<TKey, TValue>;
begin
  result := FItems.Items[0];
end;

{ TBinaryHeapClass<TKey>.TEnumerator }

constructor TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.Create(const PairEnumerator: TEnumerator<TPair<TKey, TEmptyRec>>);
begin
  inherited Create;
  FPairEnumerator := PairEnumerator;
end;

destructor TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.Destroy;
begin
  FreeAndNil(FPairEnumerator);
  inherited;
end;

function TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.DoGetCurrent: TKey;
begin
  result := FPairEnumerator.Current.Key;
end;

function TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.DoMoveNext: Boolean;
begin
  result := FPairEnumerator.MoveNext;
end;

{ TBinaryHeapClass<TKey> }

constructor TBinaryHeapClass<TKey>.Create(ACapacity: integer);
begin
  inherited Create;
  FHeap := TBinaryHeapClass<TKey,TEmptyRec>.Create(ACapacity);
end;

constructor TBinaryHeapClass<TKey>.Create(ACapacity: integer; AComparer: IComparer<TKey>);
begin
  inherited Create;
  FHeap := TBinaryHeapClass<TKey,TEmptyRec>.Create(ACapacity, AComparer);
end;

constructor TBinaryHeapClass<TKey>.Create(ACapacity: integer; AComparison: TComparison<TKey>);
var
  C: IComparer<TKey>;
begin
  if Assigned(AComparison)
    then C := TDelegatedComparer<TKey>.Create(AComparison)
    else C := nil;
  Create(ACapacity, C);
end;

constructor TBinaryHeapClass<TKey>.Create(const ACollection: TEnumerable<TKey>; ACapacity: integer; AComparison: TComparison<TKey>);
begin
  Create(ACapacity, AComparison);
  Add(ACollection);
end;

procedure TBinaryHeapClass<TKey>.Clear;
begin
  FHeap.Clear;
end;

procedure TBinaryHeapClass<TKey>.Delete(n: integer);
begin
  FHeap.Delete(n);
end;

procedure TBinaryHeapClass<TKey>.DeleteMin;
begin
  FHeap.DeleteMin;
end;

destructor TBinaryHeapClass<TKey>.Destroy;
begin
  FreeAndNil(FHeap);
  inherited;
end;

function TBinaryHeapClass<TKey>.DoGetEnumerator: TEnumerator<TKey>;
begin
  result := TBinaryHeapEnumerator.Create(FHeap.GetEnumerator);
end;

function TBinaryHeapClass<TKey>.Add(const Value: TKey): integer;
var
  P: TPair<TKey,TEmptyRec>;
begin
  P.Key := Value;
  result := FHeap.Add(P);
end;

procedure TBinaryHeapClass<TKey>.Add(const Values: TArray<TKey>);
var
  P: TPair<TKey, TEmptyRec>;
  I: Integer;
begin
  I := Count + Length(Values);
  if I > Capacity then
    Capacity := I;
  for I := Low(Values) to High(Values) do
  begin
    P.Key := Values[I];
    FHeap.Add(P);
  end;
end;

procedure TBinaryHeapClass<TKey>.Add(const Values: TEnumerable<TKey>);
var
  P: TPair<TKey, TEmptyRec>;
  K: TKey;
begin
  for K in Values do
  begin
    P.Key := K;
    FHeap.Add(P);
  end;
end;

function TBinaryHeapClass<TKey>.MinValue: TKey;
begin
  result := FHeap.MinValue.Key;
end;

function TBinaryHeapClass<TKey>.Empty: boolean;
begin
  result := FHeap.Empty;
end;

procedure TBinaryHeapClass<TKey>.SetCapacity(const Value: integer);
begin
  FHeap.Capacity := Value;
end;

procedure TBinaryHeapClass<TKey>.SetValue(n: integer; const Value: TKey);
var
  p: TPair<TKey, TEmptyRec>;
begin
  p.Key := Value;
  FHeap.Values[n] := p;
end;

procedure TBinaryHeapClass<TKey>.TrimExcess;
begin
  FHeap.TrimExcess;
end;

function TBinaryHeapClass<TKey>.ExtractMin: TKey;
begin
  result := FHeap.ExtractMin.Key;
end;

function TBinaryHeapClass<TKey>.Find(const Key: TKey): integer;
begin
  result := FHeap.Find(Key);
end;

function TBinaryHeapClass<TKey>.GetCapacity: integer;
begin
  result := FHeap.Capacity;
end;

function TBinaryHeapClass<TKey>.GetCount: integer;
begin
  result := FHeap.Count;
end;

function TBinaryHeapClass<TKey>.GetValue(n: integer): TKey;
begin
  result := FHeap.Values[n].Key;
end;

end.
