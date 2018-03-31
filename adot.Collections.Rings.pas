unit adot.Collections.Rings;

interface

{
  TRingClass<T>
}

uses
  adot.Types,
  adot.Collections.Types,
  adot.Collections.Vectors,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  { Cyclic/circular buffer based on array. Add/delete items to head/tail. }
  TRingClass<T> = class(TEnumerableExt<T>)
  protected
    { FValues.Items is inner array, Items is property to access elements of the ring by index }
    FValues: TVector<T>;
    FHead: integer;
    FCount: integer;
    FOwnsValues: Boolean;

    function GetItemFromHead(n: integer): T;
    procedure SetItemFromHead(n: integer; const Value: T);
    function GetItemFromTail(n: integer): T;
    procedure SetItemFromTail(n: integer; const Value: T);
    function GetCapacity: integer;
    procedure SetCapacity(ANewCapacity: integer);
    function GetHead: T;
    procedure SetHead(const Value: T);
    function GetTail: T;
    procedure SetTail(const Value: T);
    function GetEmpty: boolean;
    procedure SetOwnsValues(const Value: Boolean);
    function DoGetEnumerator: TEnumerator<T>; override;
    function GetIsFull: boolean;

    type
      {# Enumerator for the ring (we inherit from TEnumerator to be comatible with TEnumerable) }
      TRingEnumerator = class(TEnumerator<T>)
      protected
        [Weak] FRing: TRingClass<T>;
        FPos: integer;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(ARing: TRingClass<T>);
      end;

  public
    constructor Create(ACapacity: integer);
    destructor Destroy; override;
    procedure Clear;

    { add to head }
    procedure Add(const Value: T); overload;
    procedure Add(const Values: TArray<T>); overload;
    procedure Add(const AValues: TEnumerable<T>); overload;

    { add to tail }
    procedure AddToTail(const Value: T); overload;
    procedure AddToTail(const Values: TArray<T>); overload;
    procedure AddToTail(const AValues: TEnumerable<T>); overload;

    { from tail }
    function Extract: T;
    function ExtractTail: T;
    { from head }
    function ExtractHead: T;

    procedure Delete;
    procedure DeleteHead;

    property Capacity: integer read GetCapacity write SetCapacity; { should have Count=0 to resize }
    property Count: integer read FCount write FCount;
    property Empty: boolean read GetEmpty;
    property Head: T read GetHead write SetHead;
    property Tail: T read GetTail write SetTail;
    property ItemsFromHead[n: integer]:T read GetItemFromHead write SetItemFromHead; default; { from Head to Tail }
    property ItemsFromTail[n: integer]:T read GetItemFromTail write SetItemFromTail;          { from Tail to Head }
    property OwnsValues: Boolean read FOwnsValues write SetOwnsValues;
    property IsFull: boolean read GetIsFull;
  end;

implementation

uses
  adot.Tools.RTTI;

{ TRingClass<T>.TRingEnumerator }

constructor TRingClass<T>.TRingEnumerator.Create(ARing: TRingClass<T>);
begin
  inherited Create;
  FRing := ARing;
  FPos := 0;
end;

function TRingClass<T>.TRingEnumerator.DoMoveNext: Boolean;
begin
  result := FPos < FRing.Count;
  if result then
    Inc(FPos);
end;

function TRingClass<T>.TRingEnumerator.DoGetCurrent: T;
begin
  result := FRing.ItemsFromHead[FPos-1];
end;

{ TRingClass<T> }

constructor TRingClass<T>.Create(ACapacity: integer);
begin
  Assert(ACapacity > 0);
  inherited Create;
  Capacity := ACapacity;
end;

destructor TRingClass<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TRingClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := TRingEnumerator.Create(Self);
end;

function TRingClass<T>.GetCapacity: integer;
begin
  result := FValues.Count;
end;

procedure TRingClass<T>.SetCapacity(ANewCapacity: integer);
var
  Temp: TArray<T>;
  I: Integer;
begin
  Assert((ANewCapacity >= Count) and (ANewCapacity>0));
  if Capacity=ANewCapacity then
    Exit;
  SetLength(Temp, Count);
  for I := 0 to Count-1 do
  begin
    Temp[I] := ItemsFromHead[I];
    ItemsFromHead[I] := Default(T);
  end;
  FValues.Count := ANewCapacity;
  FHead := 0;
  for I := 0 to Count-1 do
    FValues[I] := Temp[I];
end;

procedure TRingClass<T>.Clear;
var
  I: Integer;
begin
  if FOwnsValues then
    for I := 0 to Count-1 do
      ItemsFromHead[I] := Default(T);
  FHead  := 0;
  FCount := 0;
end;

function TRingClass<T>.GetIsFull: boolean;
begin
  result := Count = Capacity;
end;

function TRingClass<T>.GetItemFromHead(n: integer): T;
begin
  result := FValues.Items[(FHead + n) mod Capacity];
end;

procedure TRingClass<T>.SetItemFromHead(n: integer; const Value: T);
begin
  n := (FHead + n) mod Capacity;
  if FOwnsValues then
    PObject(@FValues.Items[n])^.DisposeOf;
  FValues.Items[n] := Value;
end;

function TRingClass<T>.GetItemFromTail(n: integer): T;
begin
  result := FValues.Items[(FHead + FCount - n - 1) mod Capacity];
end;

procedure TRingClass<T>.SetItemFromTail(n: integer; const Value: T);
begin
  n := (FHead + FCount - n - 1) mod Capacity;
  if FOwnsValues then
    PObject(@FValues.Items[n])^.DisposeOf;
  FValues.Items[n] := Value;
end;

procedure TRingClass<T>.SetOwnsValues(const Value: Boolean);
begin
  if Value and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := Value;
end;

function TRingClass<T>.GetHead: T;
begin
  result := ItemsFromHead[0];
end;

procedure TRingClass<T>.SetHead(const Value: T);
begin
  ItemsFromHead[0] := Value;
end;

function TRingClass<T>.GetTail: T;
begin
  result := ItemsFromTail[0];
end;

procedure TRingClass<T>.SetTail(const Value: T);
begin
  ItemsFromTail[0] := Value;
end;

procedure TRingClass<T>.Add(const Value: T);
begin
  if FCount < Capacity then
    inc(FCount);
  Dec(FHead);
  if FHead < 0 then
    Inc(FHead, Capacity);
  if FOwnsValues then
    PObject(@FValues.Items[FHead])^.DisposeOf;
  FValues.Items[FHead] := Value;
end;

procedure TRingClass<T>.AddToTail(const Value: T);
begin
  if FCount < Capacity then
    inc(FCount)
  else
  begin
    Inc(FHead);
    if FHead >= Capacity then
      FHead := 0;
  end;
  ItemsFromTail[0] := Value;
end;

procedure TRingClass<T>.Add(const Values: TArray<T>);
var
  i: Integer;
begin
  for i := 0 to High(Values) do
    Add(Values[i]);
end;

procedure TRingClass<T>.Add(const AValues: TEnumerable<T>);
var
  Value: T;
begin
  for Value in AValues do
    Add(Value);
end;

procedure TRingClass<T>.AddToTail(const Values: TArray<T>);
var
  i: Integer;
begin
  for i := 0 to High(Values) do
    AddToTail(Values[i]);
end;

procedure TRingClass<T>.AddToTail(const AValues: TEnumerable<T>);
var
  Value: T;
begin
  for Value in AValues do
    AddToTail(Value);
end;

function TRingClass<T>.GetEmpty: boolean;
begin
  result := FCount=0;
end;

function TRingClass<T>.Extract: T;
begin
  result := ItemsFromTail[0];
  Delete;
end;

function TRingClass<T>.ExtractTail: T;
begin
  result := ItemsFromTail[0];
  Delete;
end;

function TRingClass<T>.ExtractHead: T;
begin
  result := ItemsFromHead[0];
  DeleteHead;
end;

procedure TRingClass<T>.Delete;
begin
  ItemsFromTail[0] := Default(T);
  Dec(FCount);
end;

procedure TRingClass<T>.DeleteHead;
begin
  ItemsFromHead[0] := Default(T);
  Dec(FCount);
  Inc(FHead);
  if (FHead >= Capacity) then
    FHead := 0;
end;

end.
