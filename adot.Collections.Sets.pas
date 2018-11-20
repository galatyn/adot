unit adot.Collections.Sets;

interface

{
  TSet<T>
    record type with full set of (compatible) overloaded operators:
    - can be initialized either by Init/Create or assigment operator:
        S.Init([1,2,3]); // call Init to initialize
        S := [1,2,3];    // same
    - default string comparer is case insensitive
        S := ['one','two'];
        Assert('ONE' in S);
    - supports all logical operators (and,or,xor)
        A := [1,2,3];
        B := [2,3,4];
        Assert((A and B).Count = 2);
        Assert(A or B = TSet<integer>.Create([1,2,3,4]));
    - supports all compare operators (<,<=,=,<>,>,>=)
        A := [1,2];
        B := [1,2,3,4];
        Assert(A < B);
    - supports IN,Add,Subtract,Implicit,Explicit operators
        A := [1,2];
        B := [1,2,3,4];
        Assert(B - A = TSet<integer>.Create([3,4]));
    - has .OwnsValues property (can be used as TObjectSet<T>)
    - can be sent as parameter similar to regular object pointer, all changes will be visible in the source set

        procedure RemoveNegative(S: TSet<integer>); // "var" specifier can be used as well
        var Value: integer;
        begin
          for Value in S.ToArray do
            if Value < 0 then
              S.Remove(Value);
        end;

        function CountPositives(const S: TSet<integer>): integer;
        var Value: integer;
        begin
          result := 0;
          for Value in S do
            if Value > 0 then
              inc(result);
        end;

    - supports automatic reference counting (no need to destruct manually)
        var S: TSet<string>;
        begin
          S := ['1','2','test'];
          assert('1' in S);
        end;
    - implementation is based on TDictionary, performance is very similar (performance penalty is close to zero)

  TSetClass<T>
    class based implementation (not so handy as record based TSet<T>, but sometime
    class or manual life time control is required).

}

uses
  adot.Collections.Types,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.RTLConsts;

type
  { check TSet<T> / TSetClass<T> bellow }
  TCustomUnorderedSet<T> = record
  private
    type
      TItem = record
      private
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
    procedure Add(const Values: TCustomUnorderedSet<T>); overload;

    procedure Remove(const Value: T); overload;
    procedure Remove(const Values: TEnumerable<T>); overload;
    procedure Remove(const Values: TArray<T>); overload;
    procedure Remove(const Values: TCustomUnorderedSet<T>); overload;

    function Contains(const Value: T): Boolean; overload;
    function Contains(const Values: TEnumerable<T>): Boolean; overload;
    function Contains(const Values: TArray<T>): Boolean; overload;
    function Contains(const Values: TCustomUnorderedSet<T>): Boolean; overload;

    procedure CopyFrom(const Src: TCustomUnorderedSet<T>);
    function ToArray: TArray<T>;
    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;

    function Extract(const Value: T): T;
    procedure Clear;
    procedure TrimExcess;
    procedure SetOwnsValues(const Value: boolean);
    function GetCapacity: integer;

    property Count: Integer read FCount;
    property OwnsValues: boolean read FOwnsValues write SetOwnsValues;
    property OnValueNotify: TValueNotifyEvent read FOnValueNotify write FOnValueNotify;
    property Comparer: IEqualityComparer<T> read FComparer;
    property Capacity: integer read GetCapacity;

  public
    function GetEnumerator: TValueEnumerator; { must be public to allow "for in" syntax }
  end;

  TSet<T> = record
  private
    type
      TData = class(TInterfacedObject, IInterface)
      private
        FSet: TCustomUnorderedSet<T>;
      public
        destructor Destroy; override;
      end;

  public
    type
      TValueEnumerator = TCustomUnorderedSet<T>.TValueEnumerator;
      TValueNotifyEvent = TCustomUnorderedSet<T>.TValueNotifyEvent;

  private
    FData: TData;          { valid as long as FDataIntf is alive }
    FDataIntf: IInterface; { maintain lifecycle }

    function GetCount: Integer;
    function GetOnValueNotify: TValueNotifyEvent;
    procedure SetOnValueNotify(const Value: TValueNotifyEvent);
    function GetComparer: IEqualityComparer<T>;
    function GetEmpty: Boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(const Value: boolean);
    function GetCapacity: integer;

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

    { Normally it is preferred to use syntax "Item in SomSet" }
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

    class operator Implicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Implicit(const a : TArray<T>)      : TSet<T>;

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

    { To compare TEnumerable<T>/TArray<T> againt TSet<T>, we convert them to TSet<T> anyway.
      It can be done by Implicit operator, no need to overload Equal operator for TEnumerable<T>/TArray<T>.
      It is correct for some other operators as well (GreaterThan etc). }
    class operator Equal(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator NotEqual(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator GreaterThanOrEqual(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator GreaterThanOrEqual(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator GreaterThanOrEqual(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator GreaterThan(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator LessThan(const a: TSet<T>; const b: TEnumerable<T> ): Boolean;
    class operator LessThan(const a: TSet<T>; const b: TArray<T>      ): Boolean;
    class operator LessThan(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator LessThanOrEqual(const a: TSet<T>; const b: TSet<T>        ): Boolean;

    class operator LogicalAnd(const a: TSet<T>; const b: TEnumerable<T> ): TSet<T>;
    class operator LogicalAnd(const a: TSet<T>; const b: TArray<T>      ): TSet<T>;
    class operator LogicalAnd(const a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    class operator LogicalOr(const a: TSet<T>; const b: TEnumerable<T> ): TSet<T>;
    class operator LogicalOr(const a: TSet<T>; const b: TArray<T>      ): TSet<T>;
    class operator LogicalOr(const a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    class operator LogicalXor(const a: TSet<T>; const b: TSet<T>        ): TSet<T>;

    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property OnValueNotify: TValueNotifyEvent read GetOnValueNotify write SetOnValueNotify;
    property Comparer: IEqualityComparer<T> read GetComparer;
    property Capacity: integer read GetCapacity;
  end;

  TSetClass<T> = class(TEnumerableExt<T>)
  public
    type
      TValueNotifyEvent = TCustomUnorderedSet<T>.TValueNotifyEvent;

      TEnumerator = class(TEnumerator<T>)
      protected
        FEnum: TCustomUnorderedSet<T>.TValueEnumerator;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;

      public
        constructor Create(AOwner: TSetClass<T>);
      end;

  private
    FSet: TCustomUnorderedSet<T>;

    function GetCount: integer;
    function GetComparer: IEqualityComparer<T>;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetOwnsValues: boolean;
    function GetContainsValue(const AValue: T): boolean;
    procedure SetContainsValue(const AValue: T; const AContains: boolean);
    function GetOnValueNotify: TValueNotifyEvent;
    procedure SetOnValueNotify(const Value: TValueNotifyEvent);
    function GetCapacity: integer;
    function GetEmpty: Boolean;

  protected
    function DoGetEnumerator: TEnumerator<T>; override;

  public
    constructor Create(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AValues: TArray<T>; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const AValues: TEnumerable<T>; AComparer: IEqualityComparer<T> = nil); overload;

    destructor Destroy; override;

    procedure Add(const AValue: T); overload;
    procedure Add(const AValues: TArray<T>); overload;
    procedure Add(const AValues: TEnumerable<T>); overload;

    procedure Remove(const AValue: T); overload;
    procedure Remove(const AValues: TArray<T>); overload;
    procedure Remove(const AValues: TEnumerable<T>); overload;

    function Contains(const AValue: T): boolean; overload;
    function Contains(const AValues: TArray<T>): boolean; overload;
    function Contains(const AValues: TEnumerable<T>): boolean; overload;

    procedure Clear;
    function ToArray: TArray<T>; override;

    property Count: integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property OnValueNotify: TValueNotifyEvent read GetOnValueNotify write SetOnValueNotify;
    property Comparer: IEqualityComparer<T> read GetComparer;
    property Capacity: integer read GetCapacity;
    property ContainsValue[const Value: T]: boolean read GetContainsValue write SetContainsValue; default;
  end;

implementation

uses
  adot.Tools.RTTI,
  adot.Collections;

{ TSetClass<T>.TEnumerator }

constructor TSetClass<T>.TEnumerator.Create(AOwner: TSetClass<T>);
begin
  FEnum := AOwner.FSet.GetEnumerator;
end;

function TSetClass<T>.TEnumerator.DoGetCurrent: T;
begin
  result := FEnum.Current;
end;

function TSetClass<T>.TEnumerator.DoMoveNext: Boolean;
begin
  result := FEnum.MoveNext;
end;

{ TSetClass<T> }

constructor TSetClass<T>.Create(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  inherited Create;
  FSet.Init(ACapacity, AComparer);
end;

constructor TSetClass<T>.Create(const AValues: TArray<T>; AComparer: IEqualityComparer<T> = nil);
begin
  FSet.Init(AValues, AComparer);
end;

constructor TSetClass<T>.Create(const AValues: TEnumerable<T>; AComparer: IEqualityComparer<T> = nil);
begin
  FSet.Init(AValues, AComparer);
end;

destructor TSetClass<T>.Destroy;
begin
  FSet.Clear;
  inherited;
end;

function TSetClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := TEnumerator.Create(Self);
end;

function TSetClass<T>.ToArray: TArray<T>;
begin
  result := FSet.ToArray;
end;

function TSetClass<T>.GetCapacity: integer;
begin
  result := FSet.Capacity;
end;

function TSetClass<T>.GetComparer: IEqualityComparer<T>;
begin
  result := FSet.Comparer;
end;

function TSetClass<T>.GetContainsValue(const AValue: T): boolean;
begin
  result := FSet.Contains(AValue);
end;

function TSetClass<T>.GetCount: integer;
begin
  result := FSet.Count;
end;

function TSetClass<T>.GetEmpty: Boolean;
begin
  result := FSet.Count=0;
end;

procedure TSetClass<T>.Remove(const AValue: T);
begin
  FSet.Remove(AValue);
end;

procedure TSetClass<T>.Remove(const AValues: TArray<T>);
begin
  FSet.Remove(AValues);
end;

procedure TSetClass<T>.Remove(const AValues: TEnumerable<T>);
begin
  FSet.Remove(AValues);
end;

function TSetClass<T>.GetOnValueNotify: TValueNotifyEvent;
begin
  result := FSet.OnValueNotify;
end;

function TSetClass<T>.GetOwnsValues: boolean;
begin
  result := FSet.OwnsValues;
end;

procedure TSetClass<T>.SetContainsValue(const AValue: T; const AContains: boolean);
begin
  if AContains then FSet.Add(AValue) else FSet.Remove(AValue);
end;

procedure TSetClass<T>.SetOnValueNotify(const Value: TValueNotifyEvent);
begin
  FSet.OnValueNotify := Value;
end;

procedure TSetClass<T>.SetOwnsValues(AOwnsValues: boolean);
begin
  FSet.OwnsValues := AOwnsValues;
end;

procedure TSetClass<T>.Add(const AValue: T);
begin
  FSet.Add(AValue);
end;

procedure TSetClass<T>.Add(const AValues: TArray<T>);
begin
  FSet.Add(AValues);
end;

procedure TSetClass<T>.Add(const AValues: TEnumerable<T>);
begin
  FSet.Add(AValues);
end;

procedure TSetClass<T>.Clear;
begin
  FSet.Clear;
end;

function TSetClass<T>.Contains(const AValue: T): boolean;
begin
  result := FSet.Contains(AValue);
end;

function TSetClass<T>.Contains(const AValues: TArray<T>): boolean;
begin
  result := FSet.Contains(AValues);
end;

function TSetClass<T>.Contains(const AValues: TEnumerable<T>): boolean;
begin
  result := FSet.Contains(AValues);
end;

{ TCustomUnorderedSet<T>.TValueEnumerator }

constructor TCustomUnorderedSet<T>.TValueEnumerator.Create(const AItems: TArray<TItem>);
begin
  FItems := AItems;
  FIndex := -1;
end;

function TCustomUnorderedSet<T>.TValueEnumerator.GetCurrent: T;
begin
  Result := FItems[FIndex].Value;
end;

function TCustomUnorderedSet<T>.TValueEnumerator.MoveNext: Boolean;
begin
  while FIndex < Length(FItems) - 1 do
  begin
    Inc(FIndex);
    if FItems[FIndex].HashCode <> EMPTY_HASH then
      Exit(True);
  end;
  Result := False;
end;

{ TCustomUnorderedSet<T> }

procedure TCustomUnorderedSet<T>.Init(ACapacity: Integer = 0; const AComparer: IEqualityComparer<T> = nil);
var
  cap: Integer;
begin
  if ACapacity < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Self := Default(TCustomUnorderedSet<T>);
  if AComparer = nil
    then FComparer := TComparerUtils.DefaultEqualityComparer<T>
    else FComparer := AComparer;
  SetCapacity(ACapacity);
end;

procedure TCustomUnorderedSet<T>.Init(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T>);
var
  Item: T;
begin
  Init(0, AComparer);
  for Item in AValues do
    Add(Item);
end;

procedure TCustomUnorderedSet<T>.Init(const AValues: TArray<T>; const AComparer: IEqualityComparer<T> = nil);
var
  Item: T;
begin
  Init(Length(AValues), AComparer);
  for Item in AValues do
    Add(Item);
end;

procedure TCustomUnorderedSet<T>.Init(const AValues: array of T; const AComparer: IEqualityComparer<T> = nil);
var
  Item: T;
begin
  Init(Length(AValues), AComparer);
  for Item in AValues do
    Add(Item);
end;

procedure TCustomUnorderedSet<T>.Add(const Value: T);
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

procedure TCustomUnorderedSet<T>.Add(const Values: TEnumerable<T>);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomUnorderedSet<T>.Add(const Values: TArray<T>);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomUnorderedSet<T>.Add(const Values: array of T);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomUnorderedSet<T>.Add(const Values: TCustomUnorderedSet<T>);
var
  V: T;
begin
  for V in Values do
    Add(V);
end;

procedure TCustomUnorderedSet<T>.Clear;
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

function TCustomUnorderedSet<T>.Contains(const Value: T): Boolean;
begin
  Result := GetBucketIndex(Value, Hash(Value)) >= 0;
end;

function TCustomUnorderedSet<T>.Contains(const Values: TEnumerable<T>): Boolean;
var
  Value: T;
begin
  for Value in Values do
    if GetBucketIndex(Value, Hash(Value)) < 0 then
      Exit(False);
  result := True;
end;

function TCustomUnorderedSet<T>.Contains(const Values: TArray<T>): Boolean;
var
  Value: T;
begin
  for Value in Values do
    if GetBucketIndex(Value, Hash(Value)) < 0 then
      Exit(False);
  result := True;
end;

function TCustomUnorderedSet<T>.Contains(const Values: TCustomUnorderedSet<T>): Boolean;
var
  Value: T;
begin
  for Value in Values do
    if GetBucketIndex(Value, Hash(Value)) < 0 then
      Exit(False);
  result := True;
end;

procedure TCustomUnorderedSet<T>.CopyFrom(const Src: TCustomUnorderedSet<T>);
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

procedure TCustomUnorderedSet<T>.DoAdd(HashCode, Index: Integer; const Value: T);
begin
  FItems[Index].HashCode := HashCode;
  FItems[Index].Value := Value;
  Inc(FCount);
  ValueNotify(Value, cnAdded);
end;

function TCustomUnorderedSet<T>.DoRemove(const Key: T; HashCode: Integer; Notification: TCollectionNotification): T;
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

procedure TCustomUnorderedSet<T>.DoSetValue(Index: Integer; const Value: T);
var
  oldValue: T;
begin
  oldValue := FItems[Index].Value;
  FItems[Index].Value := Value;

  ValueNotify(oldValue, cnRemoved);
  ValueNotify(Value, cnAdded);
end;

function TCustomUnorderedSet<T>.Extract(const Value: T): T;
var
  hc, index: Integer;
begin
  hc := Hash(Value);
  index := GetBucketIndex(Value, hc);
  if index < 0 then
    Exit(Default(T));

  Result := DoRemove(Value, hc, cnExtracted);
end;

function TCustomUnorderedSet<T>.GetBucketIndex(const Value: T; HashCode: Integer): Integer;
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

function TCustomUnorderedSet<T>.GetCapacity: integer;
begin
  result := Length(FItems)-1; { check SetCapacity - we always allocate 1 extra item }
  if result < 0 then
    result := 0;
end;

function TCustomUnorderedSet<T>.GetEnumerator: TValueEnumerator;
begin
  result := TValueEnumerator.Create(FItems);
end;

procedure TCustomUnorderedSet<T>.Grow;
var
  newCap: Integer;
begin
  newCap := Length(FItems) * 2;
  if newCap = 0 then
    newCap := 4;
  Rehash(newCap);
end;

function TCustomUnorderedSet<T>.Hash(const Key: T): Integer;
const
  PositiveMask = not Integer($80000000);
begin
  // Double-Abs to avoid -MaxInt and MinInt problems.
  // Not using compiler-Abs because we *must* get a positive integer;
  // for compiler, Abs(Low(Integer)) is a null op.
  Result := PositiveMask and ((PositiveMask and FComparer.GetHashCode(Key)) + 1);
end;

procedure TCustomUnorderedSet<T>.Rehash(NewCapPow2: Integer);
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

procedure TCustomUnorderedSet<T>.RehashAdd(HashCode: Integer; const Value: T);
var
  index: Integer;
begin
  index := not GetBucketIndex(Value, HashCode);
  FItems[index].HashCode := HashCode;
  FItems[index].Value := Value;
end;

procedure TCustomUnorderedSet<T>.Remove(const Values: TEnumerable<T>);
var
  Value: T;
begin
  for Value in Values do
    DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomUnorderedSet<T>.Remove(const Values: TArray<T>);
var
  Value: T;
begin
  for Value in Values do
    DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomUnorderedSet<T>.Remove(const Values: TCustomUnorderedSet<T>);
var
  Value: T;
begin
  for Value in Values do
    DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomUnorderedSet<T>.Remove(const Value: T);
begin
  DoRemove(Value, Hash(Value), cnRemoved);
end;

procedure TCustomUnorderedSet<T>.SetCapacity(ACapacity: Integer);
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

procedure TCustomUnorderedSet<T>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := Value;
end;

function TCustomUnorderedSet<T>.ToArray: TArray<T>;
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

function TCustomUnorderedSet<T>.ToString: string;
begin
  result := ToText(' ');
end;

function TCustomUnorderedSet<T>.ToText(const ValuesDelimiter: string): string;
var
  Builder: TStringBuilder;
  V: T;
  N: Boolean;
  Values: TArray<T>;
begin
  Values := ToArray;
  TArray.Sort<T>(Values);
  Builder := TStringBuilder.Create;
  try
    N := False;
    for V in Values do
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

procedure TCustomUnorderedSet<T>.TrimExcess;
begin
  // Ensure at least one empty slot for GetBucketIndex to terminate.
  SetCapacity(Count + 1);
end;

procedure TCustomUnorderedSet<T>.ValueNotify(const Value: T; Action: TCollectionNotification);
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
  result := not a.Contains(b);
end;

class operator TSet<T>.LessThan(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := not a.Contains(b);
end;

class operator TSet<T>.LessThan(const a, b: TSet<T>): Boolean;
begin
  result := not a.Contains(b);
end;

class operator TSet<T>.LessThanOrEqual(const a, b: TSet<T>): Boolean;
begin
  result := (a.Count <= b.Count) or not a.Contains(b); { not (A > B) }
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

class operator TSet<T>.NotEqual(const a, b: TSet<T>): Boolean;
begin
  result := (a.Count <> b.Count) or not a.Contains(b);
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

function TSet<T>.GetCapacity: integer;
begin
  result := FData.FSet.Capacity;
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

class operator TSet<T>.Explicit(const a: TEnumerable<T>): TSet<T>;
begin
  result.Init;
  result.Add(a);
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
