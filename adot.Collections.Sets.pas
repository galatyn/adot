unit adot.Collections.Sets;

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

        // "const" (as in following example) should be avoided.
        // For const params compiler will not trigger internal interface ref counter.
        // If TSet is not stored explicitly in some variable then it may lead to memory leak.
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
    - based on TDictionary, performance penalty (in comparing with TDictionary) is close to zero

  TSetClass<T>
    class based implementation (not so handy as record based TSet<T>, but sometime
    class or manual life time control is required).
}

interface

uses
  adot.Collections.Types,
  adot.Types,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TSetDataStorage<T> = class(TDictionary<T, TEmptyRec>)
  protected
    procedure KeyNotify(const Value: T; Action: TCollectionNotification); override;

  private
    { TDictionary hides FComparer in private section, we have to keep the copy }
    FDataComparer: IEqualityComparer<T>;
    FOwnsValues: boolean;

    procedure SetOwnsValues(const Value: boolean);

    property OwnsValues: boolean read FOwnsValues write SetOwnsValues;
    property DataComparer: IEqualityComparer<T> read FDataComparer;

    constructor Create(ACapacity: Integer; const AComparer: IEqualityComparer<T>); reintroduce;
  end;

  TSet<T> = record
  private
    FData: TSetDataStorage<T>;                        { valid as long as FDataIntf is alive }
    FDataIntf: IInterfacedObject<TObject>; { maintains lifecycle }

    function GetCount: Integer;
    function GetOnNotify: TCollectionNotifyEvent<T>;
    procedure SetOnNotify(const Value: TCollectionNotifyEvent<T>);
    function GetComparer: IEqualityComparer<T>;
    function GetEmpty: Boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(const Value: boolean);
    function GetCollection: TEnumerable<T>;
    function GetContainsValue(const AValue: T): boolean;
    procedure SetContainsValue(const AValue: T; const AContains: boolean);

  public
    type
      { we can't define it as "TSetDataStorage<T>.TKeyEnumerator", delphi 10.2.3 fails to compile (internal error) }
      TEnumerator = TDictionary<T, TEmptyRec>.TKeyEnumerator;

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

    function GetEnumerator: TEnumerator;

    procedure Add(const AValue: T); overload;
    procedure Add(const AValues: TEnumerable<T>); overload;
    procedure Add(const AValues: TArray<T>); overload;
    procedure Add(const AValues: array of T); overload;
    procedure Add(const AValues: TSet<T>); overload;

    procedure Remove(const AValue: T); overload;
    procedure Remove(const AValues: TEnumerable<T>); overload;
    procedure Remove(const AValues: TArray<T>); overload;
    procedure Remove(const AValues: TSet<T>); overload;

    { Normally it is preferred to use syntax "Item in MySet" }
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
    procedure Clear; { Unlike Init it does not initialize the structure, it only removes data, all props remain unchanged }
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
    property OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;
    property Comparer: IEqualityComparer<T> read GetComparer;
    property Collection: TEnumerable<T> read GetCollection;
    property ContainsValue[const Value: T]: boolean read GetContainsValue write SetContainsValue; default;
  end;

  TSetClass<T> = class(TEnumerableExt<T>)
  private
    FData: TSetDataStorage<T>;

    function GetCount: integer;
    function GetComparer: IEqualityComparer<T>;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetOwnsValues: boolean;
    function GetContainsValue(const AValue: T): boolean;
    procedure SetContainsValue(const AValue: T; const AContains: boolean);
    function GetOnNotify: TCollectionNotifyEvent<T>;
    procedure SetOnNotify(const Value: TCollectionNotifyEvent<T>);
    function GetEmpty: Boolean;

  protected
    type
      TEnumerator = TDictionary<T, TEmptyRec>.TKeyEnumerator;

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
    property OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;
    property Comparer: IEqualityComparer<T> read GetComparer;
    property ContainsValue[const Value: T]: boolean read GetContainsValue write SetContainsValue; default;
  end;

implementation

uses
  adot.Collections,
  adot.Tools,
  adot.Tools.RTTI,
  System.SysUtils;

{ TSetDataStorage<T> }

constructor TSetDataStorage<T>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<T>);
begin
  if AComparer = nil
    then FDataComparer := TComparerUtils.DefaultEqualityComparer<T>
    else FDataComparer := AComparer;
  inherited Create(ACapacity, FDataComparer);
end;

procedure TSetDataStorage<T>.KeyNotify(const Value: T; Action: TCollectionNotification);
begin
  inherited;
  if FOwnsValues and (Action = TCollectionNotification.cnRemoved) then
    PObject(@Value)^.DisposeOf;
end;

procedure TSetDataStorage<T>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := Value;
end;

{ TSet<T> }

class operator TSet<T>.Add(a: TSet<T>; const b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: TArray<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

class operator TSet<T>.Add(const a: TArray<T>; b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(b);
  result.Add(a);
end;

class operator TSet<T>.Add(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(b);
  result.Add(a);
end;

class operator TSet<T>.Add(const a: T; b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(b);
  result.Add(a);
end;

procedure TSet<T>.Add(const AValues: TArray<T>);
var
  I: Integer;
  EmptyRec: TEmptyRec;
begin
  for I := Low(AValues) to High(AValues) do
    FData.AddOrSetValue(AValues[I], EmptyRec);
end;

procedure TSet<T>.Add(const AValues: TEnumerable<T>);
var
  Value: T;
  EmptyRec: TEmptyRec;
begin
  for Value in AValues do
    FData.AddOrSetValue(Value, EmptyRec);
end;

procedure TSet<T>.Add(const AValue: T);
var EmptyRec: TEmptyRec;
begin
  FData.AddOrSetValue(AValue, EmptyRec);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: T): TSet<T>;
begin
  result.CopyFrom(a);
  result.Add(b);
end;

procedure TSet<T>.Add(const AValues: TSet<T>);
var
  Value: T;
  EmptyRec: TEmptyRec;
begin
  for Value in AValues do
    FData.AddOrSetValue(Value, EmptyRec);
end;

procedure TSet<T>.Add(const AValues: array of T);
var
  Value: T;
  EmptyRec: TEmptyRec;
begin
  for Value in AValues do
    FData.AddOrSetValue(Value, EmptyRec);
end;

procedure TSet<T>.Clear;
begin
  FData.Clear;
end;

function TSet<T>.Contains(const AValue: T): Boolean;
begin
  result := FData.ContainsKey(AValue);
end;

function TSet<T>.Contains(const AValues: TEnumerable<T>): Boolean;
var
  Value: T;
begin
  for Value in AValues do
    if not FData.ContainsKey(Value) then
      Exit(False);
  result := True;
end;

function TSet<T>.Contains(const AValues: TArray<T>): Boolean;
var
  Value: T;
begin
  for Value in AValues do
    if not FData.ContainsKey(Value) then
      Exit(False);
  result := True;
end;

function TSet<T>.Contains(const AValues: TSet<T>): Boolean;
var
  Value: T;
begin
  for Value in AValues do
    if not FData.ContainsKey(Value) then
      Exit(False);
  result := True;
end;

function TSet<T>.Copy: TSet<T>;
begin
  result.CopyFrom(Self);
end;

procedure TSet<T>.CopyFrom(const Src: TSet<T>);
begin
  Init(Src.Count*3 shr 1, Src.Comparer);
  { Doesn't make sense to copy when OwnsValues=True
    (when item deleted in source, it became invalid) }
  assert(not Src.OwnsValues or Src.Empty);
  OwnsValues := Src.OwnsValues;
  OnNotify := Src.OnNotify;
  Add(Src);
end;

class function TSet<T>.Create(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T>): TSet<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TSet<T>.Create(ACapacity: Integer; const AComparer: IEqualityComparer<T>): TSet<T>;
begin
  result.Init(ACapacity, AComparer);
end;

class function TSet<T>.Create(const AValues: TArray<T>; const AComparer: IEqualityComparer<T>): TSet<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TSet<T>.Create(const AValues: array of T; const AComparer: IEqualityComparer<T>): TSet<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TSet<T>.Create: TSet<T>;
begin
  result.Init;
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

class operator TSet<T>.Explicit(const a: TEnumerable<T>): TSet<T>;
begin
  result.Init;
  result.Add(a);
end;

function TSet<T>.Extract(const Value: T): T;
begin
  { generates TData.KeyNotify(Valuie, cnExtracted), no need to set OwnsValues=False }
  result := FData.ExtractPair(Value).Key;
end;

function TSet<T>.GetCollection: TEnumerable<T>;
begin
  result := FData.Keys;
end;

function TSet<T>.GetComparer: IEqualityComparer<T>;
begin
  result := FData.DataComparer;
end;

function TSet<T>.GetContainsValue(const AValue: T): boolean;
begin
  result := FData.ContainsKey(AValue);
end;

function TSet<T>.GetCount: Integer;
begin
  result := FData.Count;
end;

function TSet<T>.GetEmpty: Boolean;
begin
  result := FData.Count = 0;
end;

function TSet<T>.GetEnumerator: TEnumerator;
begin
  result := FData.Keys.GetEnumerator;
end;

function TSet<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
  result := FData.OnKeyNotify;
end;

function TSet<T>.GetOwnsValues: boolean;
begin
  result := FData.OwnsValues;
end;

class operator TSet<T>.GreaterThan(const a, b: TSet<T>): Boolean;
begin
  result := (a.Count > b.Count) and a.Contains(b);
end;

class operator TSet<T>.GreaterThanOrEqual(const a, b: TSet<T>): Boolean;
begin
  result := a.Contains(b);
end;

class operator TSet<T>.GreaterThanOrEqual(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := a.Contains(b);
end;

class operator TSet<T>.GreaterThanOrEqual(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := a.Contains(b);
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

class operator TSet<T>.In(const a: TArray<T>; b: TSet<T>): Boolean;
begin
  result := b.Contains(a);
end;

class operator TSet<T>.In(const a: TEnumerable<T>; b: TSet<T>): Boolean;
begin
  result := b.Contains(a);
end;

class operator TSet<T>.In(const a: T; b: TSet<T>): Boolean;
begin
  result := b.Contains(a);
end;

procedure TSet<T>.Init(ACapacity: Integer; const AComparer: IEqualityComparer<T>);
begin
  Self := Default(TSet<T>);
  FData := TSetDataStorage<T>.Create(ACapacity, AComparer);
  FDataIntf := TInterfacedObject<TObject>.Create(FData);
end;

procedure TSet<T>.Init;
begin
  Init(0, nil);
end;

procedure TSet<T>.Init(const AValues: array of T; const AComparer: IEqualityComparer<T>);
begin
  Init(Length(AValues)*3 shr 1, AComparer);
  Add(AValues);
end;

procedure TSet<T>.Init(const AValues: TArray<T>; const AComparer: IEqualityComparer<T>);
begin
  Init(Length(AValues)*3 shr 1, AComparer);
  Add(AValues);
end;

procedure TSet<T>.Init(const AValues: TEnumerable<T>; const AComparer: IEqualityComparer<T>);
begin
  Init(0, AComparer);
  Add(AValues);
end;

class operator TSet<T>.LessThan(const a, b: TSet<T>): Boolean;
begin
  result := not a.Contains(b);
end;

class operator TSet<T>.LessThan(const a: TSet<T>; const b: TArray<T>): Boolean;
begin
  result := not a.Contains(b);
end;

class operator TSet<T>.LessThan(const a: TSet<T>; const b: TEnumerable<T>): Boolean;
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

class operator TSet<T>.LogicalAnd(const a: TSet<T>; const b: TArray<T>): TSet<T>;
var
  Value: T;
begin
  result.Init;
  for Value in b do
    if a.Contains(Value) then
      result.Add(Value);
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

procedure TSet<T>.Remove(const AValue: T);
begin
  FData.Remove(AValue);
end;

procedure TSet<T>.Remove(const AValues: TSet<T>);
var
  Value: T;
begin
  for Value in AValues do
    FData.Remove(Value);
end;

procedure TSet<T>.Remove(const AValues: TArray<T>);
var
  Value: T;
begin
  for Value in AValues do
    FData.Remove(Value);
end;

procedure TSet<T>.Remove(const AValues: TEnumerable<T>);
var
  Value: T;
begin
  for Value in AValues do
    FData.Remove(Value);
end;

procedure TSet<T>.SetContainsValue(const AValue: T; const AContains: boolean);
var
  EmptyRec: TEmptyRec;
begin
  if AContains
    then FData.AddOrSetValue(AValue, EmptyRec)
    else FData.Remove(AValue);
end;

procedure TSet<T>.SetOnNotify(const Value: TCollectionNotifyEvent<T>);
begin
  FData.OnKeyNotify := Value;
end;

procedure TSet<T>.SetOwnsValues(const Value: boolean);
begin
  FData.OwnsValues := Value;
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

class operator TSet<T>.Subtract(a: TSet<T>; const b: TArray<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: T): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
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

class operator TSet<T>.Subtract(const a: T; b: TSet<T>): TSet<T>;
begin
  result.Init;
  if not b.Contains(a) then
    result.Add(a);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: TSet<T>): TSet<T>;
begin
  result.CopyFrom(a);
  result.Remove(b);
end;

function TSet<T>.ToArray: TArray<T>;
begin
  result := FData.Keys.ToArray;
end;

function TSet<T>.ToString: string;
begin
  result := ToText(' ');
end;

function TSet<T>.ToText(const ValuesDelimiter: string): string;
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

procedure TSet<T>.TrimExcess;
begin
  FData.TrimExcess;
end;

{ TSetClass<T> }

procedure TSetClass<T>.Add(const AValues: TEnumerable<T>);
var
  EmptyRec: TEmptyRec;
  Value: T;
begin
  for Value in AValues do
    FData.AddOrSetValue(Value, EmptyRec);
end;

procedure TSetClass<T>.Add(const AValues: TArray<T>);
var
  EmptyRec: TEmptyRec;
  Value: T;
begin
  for Value in AValues do
    FData.AddOrSetValue(Value, EmptyRec);
end;

procedure TSetClass<T>.Add(const AValue: T);
var
  EmptyRec: TEmptyRec;
begin
  FData.AddOrSetValue(AValue, EmptyRec);
end;

procedure TSetClass<T>.Clear;
begin
  FData.Clear;
end;

function TSetClass<T>.Contains(const AValues: TArray<T>): boolean;
var
  Value: T;
begin
  for Value in AValues do
    if not FData.ContainsKey(Value) then
      Exit(False);
  result := True;
end;

function TSetClass<T>.Contains(const AValues: TEnumerable<T>): boolean;
var
  Value: T;
begin
  for Value in AValues do
    if not FData.ContainsKey(Value) then
      Exit(False);
  result := True;
end;

function TSetClass<T>.Contains(const AValue: T): boolean;
begin
  result := FData.ContainsKey(AValue);
end;

constructor TSetClass<T>.Create(ACapacity: integer; AComparer: IEqualityComparer<T>);
begin
  FData := TSetDataStorage<T>.Create(ACapacity, AComparer);
end;

constructor TSetClass<T>.Create(const AValues: TEnumerable<T>; AComparer: IEqualityComparer<T>);
begin
  FData := TSetDataStorage<T>.Create(0, AComparer);
  Add(AValues);
end;

constructor TSetClass<T>.Create(const AValues: TArray<T>; AComparer: IEqualityComparer<T>);
begin
  FData := TSetDataStorage<T>.Create(Length(AValues)*3 shr 1, AComparer);
  Add(AValues);
end;

destructor TSetClass<T>.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TSetClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := FData.Keys.GetEnumerator;
end;

function TSetClass<T>.GetComparer: IEqualityComparer<T>;
begin
  result := FData.DataComparer;
end;

function TSetClass<T>.GetContainsValue(const AValue: T): boolean;
begin
  result := FData.ContainsKey(AValue);
end;

function TSetClass<T>.GetCount: integer;
begin
  result := FData.Count;
end;

function TSetClass<T>.GetEmpty: Boolean;
begin
  result := FData.Count = 0;
end;

function TSetClass<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
  result := FData.OnKeyNotify;
end;

function TSetClass<T>.GetOwnsValues: boolean;
begin
  result := FData.OwnsValues;
end;

procedure TSetClass<T>.Remove(const AValues: TArray<T>);
var
  Value: T;
begin
  for Value in AValues do
    FData.Remove(Value);
end;

procedure TSetClass<T>.Remove(const AValue: T);
begin
  FData.Remove(AValue);
end;

procedure TSetClass<T>.Remove(const AValues: TEnumerable<T>);
var
  Value: T;
begin
  for Value in AValues do
    FData.Remove(Value);
end;

procedure TSetClass<T>.SetContainsValue(const AValue: T; const AContains: boolean);
var
  EmptyRec: TEmptyRec;
begin
  if AContains
    then FData.AddOrSetValue(AValue, EmptyRec)
    else FData.Remove(AValue);
end;

procedure TSetClass<T>.SetOnNotify(const Value: TCollectionNotifyEvent<T>);
begin
  FData.OnKeyNotify := Value;
end;

procedure TSetClass<T>.SetOwnsValues(AOwnsValues: boolean);
begin
  FData.OwnsValues := AOwnsValues;
end;

function TSetClass<T>.ToArray: TArray<T>;
begin
  result := FData.Keys.ToArray;
end;

end.
