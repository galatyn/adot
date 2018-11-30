unit adot.Collections.VectorsNew;

interface

uses
  adot.Collections,
  adot.Collections.Types,
  adot.Types,
  adot.Collections.Slices,
  adot.Collections.Sets,
  adot.Tools,
  adot.Tools.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults, System.SysUtils;

type
  TVectorDataStorage<T> = class(TList<T>)
  private
    { TList hides FComparer in private section, we have to keep the copy }
    FDataComparer: IComparer<T>;
    FDefaultComparer: boolean;
    FOwnsValues: boolean;

    procedure SetOwnsValues(const Value: boolean);

    property OwnsValues: boolean read FOwnsValues write SetOwnsValues;
    property DataComparer: IComparer<T> read FDataComparer;

    constructor Create(ACapacity: Integer; const AComparer: IComparer<T>); reintroduce;

  protected
    procedure Notify(const Item: T; Action: TCollectionNotification); override;
  end;

  TVector<T> = record
  private
    FData: TVectorDataStorage<T>;          { valid as long as FDataIntf is alive }
    FDataIntf: IInterfacedObject<TObject>; { maintains lifecycle }

    procedure SetCount(ACount: NativeInt);
    procedure SetCapacity(ACapacity: integer);
    procedure Grow;
    function GetCapacity: integer;
    function GetItem(ItemIndex: integer): T;
    procedure SetItem(ItemIndex: integer; const Value: T);
    function GetFirst: T;
    function GetLast: T;
    procedure SetFirst(const Value: T);
    procedure SetLast(const Value: T);
    function GetEmpty: Boolean;
    function GetTotalSizeBytes: int64;
    function VectorContains(const AValues: TArray<T>; AComparer: IComparer<T>): boolean; overload;
    function VectorContains(const AValues: TEnumerable<T>; AComparer: IComparer<T>): boolean; overload;
    function GetCount: NativeInt;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(const Value: boolean);
    function GetOnNotify: TCollectionNotifyEvent<T>;
    procedure SetOnNotify(const Value: TCollectionNotifyEvent<T>);

  public
    type
      TEnumerator = TVectorDataStorage<T>.TEnumerator;

    { Init is preferred over Create, it is obviously distinguished from classes.
      Class-like Create can be useful when new instance is param of some routine }
    procedure Init; overload;
    procedure Init(AComparer: IComparer<T>); overload;
    procedure Init(ACapacity: integer; AComparer: IComparer<T> = nil); overload;
    procedure Init(AValues: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Init(AValues: TArray<T>;      AComparer: IComparer<T> = nil); overload;
    procedure Init(AValues: TVector<T>;     AComparer: IComparer<T> = nil); overload;

    class function Create: TVector<T>; overload; static;
    class function Create(AComparer: IComparer<T>): TVector<T>; overload; static;
    class function Create(ACapacity: integer; AComparer: IComparer<T> = nil): TVector<T>; overload; static;
    class function Create(AValues: TEnumerable<T>; AComparer: IComparer<T> = nil): TVector<T>; overload; static;
    class function Create(AValues: TArray<T>;      AComparer: IComparer<T> = nil): TVector<T>; overload; static;
    class function Create(AValues: TVector<T>;     AComparer: IComparer<T> = nil): TVector<T>; overload; static;

    { Creates empty slice on underlying array }
    function GetSlice: TSlice<T>; overload;
    { Creates slice from range of underlying array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates slice on the array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

    procedure Clear;  { Unlike Init it does not initialize the structure, it only removes data, all props remain unchanged }
    procedure TrimExcess;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Values: TArray<T>); overload;
    procedure Add(const Values: TArray<T>; AStartIndex,ACount: integer); overload;
    procedure Add(const Values: TEnumerable<T>); overload;
    procedure Add(Values: TVector<T>); overload;

    function Copy: TVector<T>;
    procedure CopyFrom(Src: TVector<T>);

    function Insert(Index: integer; const Value: T): integer;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(AStartIndex,ACount: integer); overload;
    procedure Delete(const AIndices: TArray<integer>); overload;
    procedure Delete(AIndices: TSet<integer>); overload;
    procedure DeleteLast;

    procedure Remove(const V: T; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(AFilter: TFuncFilterValueIndex<T>); overload;

    function Extract(ItemIndex: integer): T;
    function ExtractAll: TArray<T>;
    function ExtractLast: T;

    procedure Move(SrcIndex, DstIndex: integer);

    procedure Exchange(Index1,Index2: integer);
        { Reverse( [1,2,3,4,5], 1, 3 ) = [1, 4,3,2, 5] }
    procedure Reverse; overload;
    procedure Reverse(AStartIndex,ACount: integer); overload;
    { RotateLeft( [1,2,3,4,5], 1, 3, 1 ) = [1, 3,4,2, 5]
      RotateLeft( [1,2,3,4,5], 1, 3,-1 ) = [1, 4,2,3, 5] }
    procedure RotateLeft(Index1,Index2,Shift: integer);
    { RotateRight( [1,2,3,4,5], 1, 3, 1 ) = [1, 4,2,3, 5]
      RotateRight( [1,2,3,4,5], 1, 3,-1 ) = [1, 3,4,2, 5] }
    procedure RotateRight(Index1,Index2,Shift: integer);
    { Shuffle items of the range in random order }
    procedure Shuffle; overload;
    procedure Shuffle(AStartIndex,ACount: integer); overload;
    { Generate all permutations. Permutations of [2,1,3]:
        [1,2,3] [1,3,2] [2,1,3] [2,3,1] [3,1,2] [3,2,1] }
    procedure FirstPermutation;
    function NextPermutation: boolean;
    function PrevPermutation: boolean;

    function Contains(const Value: T): boolean; overload;
    function Contains(const Value: T; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TArray<T>): boolean; overload;
    function Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean; overload;

    function Sorted: boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparer: IComparer<T>): boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparison: TComparison<T>): boolean; overload;

    function IndexOf(const Value: T): integer; overload;
    function IndexOf(const Value: T; Comparer: IComparer<T>): integer; overload;
    function FindFirst(const Value: T; var Index: integer): boolean; overload;
    function FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;
    function FindNext(const Value: T; var Index: integer): boolean; overload;
    function FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;

    procedure Sort; overload;
    procedure Sort(AIndex, ACount: Integer); overload;
    procedure Sort(Comparer: IComparer<T>); overload;
    procedure Sort(Comparer: IComparer<T>; AIndex, ACount: Integer); overload;
    procedure Sort(Comparison: TComparison<T>); overload;
    procedure Sort(Comparison: TComparison<T>; AIndex, ACount: Integer); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex,ACount: Integer): Boolean; overload;

    { TArray }
    function Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(const B: TArray<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;
    { TEnumerable }
    function Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(B: TEnumerable<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;

    { Trims and returns Items }
    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;
    function ToArray: TArray<T>;

    { for-in support }
    function GetEnumerator: TEnumerator<T>;

    class operator In(const a: T; b: TVector<T>) : Boolean;
    class operator In(a: TVector<T>; b: TVector<T>) : Boolean;
    class operator In(const a: TArray<T>; b: TVector<T>) : Boolean;
    class operator In(const a: TEnumerable<T>; b: TVector<T>) : Boolean;

    class operator Implicit(const a : T) : TVector<T>;
    class operator Implicit(const a : TArray<T>) : TVector<T>;
    class operator Implicit(const a : TEnumerable<T>) : TVector<T>;
    class operator Implicit(a : TVector<T>) : TArray<T>;

    class operator Explicit(const a : T) : TVector<T>;
    class operator Explicit(const a : TArray<T>) : TVector<T>;
    class operator Explicit(const a : TEnumerable<T>) : TVector<T>;
    class operator Explicit(a : TVector<T>) : TArray<T>;

    class operator Add(a: TVector<T>; const b: T): TVector<T>;
    class operator Add(a: TVector<T>;       b: TVector<T>): TVector<T>;
    class operator Add(a: TVector<T>; const b: TArray<T>): TVector<T>;
    class operator Add(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
    class operator Add(const a: T;              b: TVector<T>): TVector<T>;
    class operator Add(const a: TArray<T>;      b: TVector<T>): TVector<T>;
    class operator Add(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;

    class operator Subtract(a: TVector<T>; const b: T): TVector<T>;
    class operator Subtract(a: TVector<T>;       b: TVector<T>): TVector<T>;
    class operator Subtract(a: TVector<T>; const b: TArray<T>): TVector<T>;
    class operator Subtract(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
    class operator Subtract(const a: T;              b: TVector<T>): TVector<T>;
    class operator Subtract(const a: TArray<T>;      b: TVector<T>): TVector<T>;
    class operator Subtract(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;

    class operator Equal(a: TVector<T>;       b: TVector<T>) : Boolean;
    class operator Equal(a: TVector<T>; const b: TArray<T>) : Boolean;
    class operator Equal(a: TVector<T>; const b: TEnumerable<T>) : Boolean;
    class operator Equal(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator Equal(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator NotEqual(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator NotEqual(a: TVector<T>; const b: TArray<T>) : Boolean;
    class operator NotEqual(a: TVector<T>; const b: TEnumerable<T>) : Boolean;
    class operator NotEqual(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator NotEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator GreaterThanOrEqual(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator GreaterThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator GreaterThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator GreaterThanOrEqual(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator GreaterThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator GreaterThan(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator GreaterThan(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator GreaterThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator GreaterThan(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator GreaterThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator LessThan(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator LessThan(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator LessThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator LessThan(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator LessThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator LessThanOrEqual(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator LessThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator LessThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator LessThanOrEqual(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator LessThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Count: NativeInt read GetCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Elements[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property OnNotify: TCollectionNotifyEvent<T> read GetOnNotify write SetOnNotify;
  end;

implementation

{ TVectorDataStorage<T> }

constructor TVectorDataStorage<T>.Create(ACapacity: Integer; const AComparer: IComparer<T>);
begin
  FDefaultComparer := AComparer = nil;
  if FDefaultComparer
    then FDataComparer := TComparerUtils.DefaultComparer<T>
    else FDataComparer := AComparer;
  inherited Create(FDataComparer);
end;

procedure TVectorDataStorage<T>.Notify(const Item: T; Action: TCollectionNotification);
begin
  inherited;
  if FOwnsValues and (Action = TCollectionNotification.cnRemoved) then
    PObject(@Item)^.DisposeOf;
end;

procedure TVectorDataStorage<T>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := Value;
end;

{ TVector<T> }

class function TVector<T>.Create: TVector<T>;
begin
  result.Init;
end;

class function TVector<T>.Create(ACapacity: integer; AComparer: IComparer<T>): TVector<T>;
begin
  result.Init(ACapacity, AComparer);
end;

class function TVector<T>.Create(AComparer: IComparer<T>): TVector<T>;
begin
  result.Init(AComparer);
end;

class function TVector<T>.Create(AValues: TEnumerable<T>; AComparer: IComparer<T>): TVector<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TVector<T>.Create(AValues: TArray<T>; AComparer: IComparer<T>): TVector<T>;
begin
  result.Init(AValues, AComparer);
end;

class function TVector<T>.Create(AValues: TVector<T>; AComparer: IComparer<T>): TVector<T>;
begin
  result.Init(AValues, AComparer);
end;

procedure TVector<T>.Init;
begin
  Init(0, nil);
end;

procedure TVector<T>.Init(AComparer: IComparer<T>);
begin
  Init(0, AComparer);
end;

procedure TVector<T>.Init(ACapacity: integer; AComparer: IComparer<T>);
begin
  Self := Default(TVector<T>);
  FData := TVectorDataStorage<T>.Create(ACapacity, AComparer);
  FDataIntf := TInterfacedObject<TObject>.Create(FData);
end;

procedure TVector<T>.Init(AValues: TEnumerable<T>; AComparer: IComparer<T>);
begin
  Init(0, AComparer);
  Add(AValues);
end;

procedure TVector<T>.Init(AValues: TArray<T>; AComparer: IComparer<T>);
begin
  Init(Length(AValues), AComparer);
  Add(AValues);
end;

procedure TVector<T>.Init(AValues: TVector<T>; AComparer: IComparer<T>);
begin
  Init(AValues.Count, AComparer);
  Add(AValues);
end;

function TVector<T>.Add: integer;
begin
  result := FData.Add(Default(T));
end;

function TVector<T>.Add(const Value: T): integer;
begin
  result := FData.Add(Value);
end;

procedure TVector<T>.Add(const Values: TArray<T>);
var
  I: Integer;
begin
  for I := Low(Values) to High(Values) do
    FData.Add(Values[I]);
end;

procedure TVector<T>.Add(const Values: TArray<T>; AStartIndex,ACount: integer);
var
  I: Integer;
begin
  if ACount <= 0 then
    Exit;
  I := FData.Count + ACount;
  if I > FData.Capacity then
    FData.Capacity := I;
  for I := AStartIndex to AStartIndex+ACount-1 do
    FData.Add(Values[I]);
end;

procedure TVector<T>.Add(const Values: TEnumerable<T>);
var
  V: T;
begin
  for V in Values do
    FData.Add(V);
end;

procedure TVector<T>.Clear;
begin
  FData.Clear;
end;

procedure TVector<T>.TrimExcess;
begin
  FData.TrimExcess;
end;

function TVector<T>.VectorContains(const AValues: TEnumerable<T>; AComparer: IComparer<T>): boolean;
var
  DataSet: TDictionary<T, TEmptyRec>;
  EmptyRec: TEmptyRec;
  SortedData: TArray<T>;
  I: Integer;
  Value: T;
begin

  { With default comparer we can use TDictionary as most efficient way }
  if FData.FDefaultComparer then
  begin
    DataSet := TDictionary<T, TEmptyRec>.Create(FData.Count*2);
    try
      for I := 0 to FData.Count-1 do
        DataSet.AddOrSetValue(FData[I], EmptyRec);
      for Value in AValues do
        if not DataSet.ContainsKey(Value) then
          Exit(False);
      Exit(True);
    finally
      Sys.FreeAndNil(DataSet);
    end;
  end;

  { We can't use TDictionary, because it needs IEqualityComparer.GetHash,
    IComparer can not be tranformed to IEqualityComparer }
  SortedData := FData.ToArray;
  TArray.Sort<T>(SortedData, AComparer);
  for Value in AValues do
    if not TArray.BinarySearch<T>(SortedData, Value, I, AComparer) then
      Exit(False);

  result := True;
end;

function TVector<T>.VectorContains(const AValues: TArray<T>; AComparer: IComparer<T>): boolean;
var
  DataSet: TDictionary<T, TEmptyRec>;
  EmptyRec: TEmptyRec;
  SortedData: TArray<T>;
  I,J: Integer;
begin

  { With default comparer we can use TDictionary as most efficient way }
  if FData.FDefaultComparer then
  begin
    DataSet := TDictionary<T, TEmptyRec>.Create(FData.Count*2);
    try
      for I := 0 to FData.Count-1 do
        DataSet.AddOrSetValue(FData[I], EmptyRec);
      for I := 0 to High(AValues) do
        if not DataSet.ContainsKey(AValues[I]) then
          Exit(False);
      Exit(True);
    finally
      Sys.FreeAndNil(DataSet);
    end;
  end;

  { We can't use TDictionary, because it needs IEqualityComparer.GetHash,
    IComparer can not be tranformed to IEqualityComparer }
  SortedData := FData.ToArray;
  TArray.Sort<T>(SortedData, AComparer);
  for I := 0 to High(AValues) do
    if not TArray.BinarySearch<T>(SortedData, AValues[I], J, AComparer) then
      Exit(False);

  result := True;
end;

function TVector<T>.Contains(const Value: T): boolean;
begin
  result := FData.IndexOf(Value)>=0;
end;

function TVector<T>.Contains(const Value: T; Comparer: IComparer<T>): boolean;
begin
  result := IndexOf(Value, Comparer)>=0;
end;

function TVector<T>.Contains(const Values: TArray<T>): boolean;
begin
  result := VectorContains(Values, FData.FDataComparer);
end;

function TVector<T>.Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean;
begin
  result := VectorContains(Values, Comparer);
end;

function TVector<T>.Compare(B: TEnumerable<T>; AComparer: IComparer<T>): integer;
var
  Value: T;
  BItemsCount: integer;
begin
  BItemsCount := 0;
  for Value in B do
    inc(BItemsCount);
  if FData.Count = BItemsCount then
    result := Compare(B,0,0,FData.Count, AComparer)
  else
    if FData.Count < BItemsCount
      then result := -1
      else result := 1;
end;

function TVector<T>.Compare(const B: TArray<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
var
  I: Integer;
begin
  if AComparer = nil then
    AComparer := FData.FDataComparer;
  Assert((ACount=0) or (ACount>0) and (AStartIndex>=0) and (AStartIndex+ACount-1<FData.Count));
  Assert((ACount=0) or (ACount>0) and (BStartIndex>=0) and (BStartIndex+ACount-1<Length(B)));
  if ACount <= 0 then
    result := 0
  else
    for I := 0 to ACount-1 do
    begin
      result := AComparer.Compare(FData[I+AStartIndex], B[I+BStartIndex]);
      if result <> 0 then
        Break;
    end;
end;

function TVector<T>.Compare(const B: TArray<T>; AComparer: IComparer<T>): integer;
begin
  if Count = Length(B) then
    result := Compare(B,0,0,FData.Count, AComparer)
  else
    if FData.Count < Length(B)
      then result := -1
      else result := 1;
end;

function TVector<T>.Compare(B: TEnumerable<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
var
  Value: T;
  BItemsCount: integer;
begin
  if AComparer = nil then
    AComparer := FData.FDataComparer;
  BItemsCount := 0;
  for Value in B do
    inc(BItemsCount);
  Assert((ACount=0) or (ACount>0) and (AStartIndex>=0) and (AStartIndex+ACount-1<FData.Count));
  Assert((ACount=0) or (ACount>0) and (BStartIndex>=0) and (BStartIndex+ACount-1<BItemsCount));
  result := 0;
  for Value in B do
    if BStartIndex > 0 then
      dec(BStartIndex)
    else
    begin
      result := AComparer.Compare(FData[AStartIndex], Value);
      inc(AStartIndex);
      dec(ACount);
      if (result <> 0) or (ACount <= 0) then
        break;
    end;
end;

function TVector<T>.Contains(const Values: TEnumerable<T>): boolean;
begin
  result := VectorContains(Values, FData.FDataComparer);
end;

function TVector<T>.Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean;
begin
  result := VectorContains(Values, Comparer);
end;

function TVector<T>.Copy: TVector<T>;
begin
  result.CopyFrom(Self);
end;

procedure TVector<T>.CopyFrom(Src: TVector<T>);
begin
  Init(Src.Count, Src.FData.FDataComparer);
  { Doesn't make sense to copy when OwnsValues=True
    (when item deleted in source, it became invalid) }
  assert(not Src.OwnsValues or Src.Empty);
  OwnsValues := Src.OwnsValues;
  OnNotify := Src.OnNotify;
  Add(Src);
end;

procedure TVector<T>.Delete(ItemIndex: integer);
begin
  FData.Delete(ItemIndex);
end;

procedure TVector<T>.Delete(AStartIndex,ACount: integer);
begin
  FData.DeleteRange(AStartIndex,ACount);
end;

procedure TVector<T>.Delete(const AIndices: TArray<integer>);
begin
  Delete(TSet<integer>.Create(AIndices));
end;

procedure TVector<T>.Delete(AIndices: TSet<integer>);
var
  S,D,I: Integer;
begin
  if (AIndices.Count = 0) or (FData.Count = 0) then
    Exit;
  S := 0;
  D := 0;
  for I := 0 to FData.Count-1 do
    if not (I in AIndices) then
    begin
      FData[D] := FData[I];
      inc(D);
    end;
  for I := D to FData.Count-1 do
    FData[I] := Default(T);
  FData.Count := D;
end;

procedure TVector<T>.DeleteLast;
begin
  FData.Count := FData.Count-1;
end;

class operator TVector<T>.Equal(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

procedure TVector<T>.Exchange(Index1, Index2: integer);
begin
  FData.Exchange(Index1, Index2);
end;

class operator TVector<T>.Explicit(const a: T): TVector<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TVector<T>.Explicit(const a: TArray<T>): TVector<T>;
begin
  result.Init(a);
end;

class operator TVector<T>.Explicit(const a: TEnumerable<T>): TVector<T>;
begin
  result.Init(a);
end;

class operator TVector<T>.Explicit(a: TVector<T>): TArray<T>;
begin
  result := a.ToArray;
end;

function TVector<T>.ToString: string;
begin
  result := ToText(' ');
end;

function TVector<T>.ToText(const ValuesDelimiter: string = #13#10): string;
var
  S: TStringBuilder;
  I: Integer;
begin
  S := TStringBuilder.Create;
  for I := 0 to FData.Count-1 do
  begin
    if S.Length > 0 then
      S.Append(ValuesDelimiter);
    S.Append(TRttiUtils.ValueAsString<T>(FData[I]));
  end;
  result := S.ToString;
end;

function TVector<T>.ToArray: TArray<T>;
begin
  result := FData.ToArray;
end;

function TVector<T>.Extract(ItemIndex: integer): T;
begin
aaa  FData.Extract()
  Assert((ItemIndex>=0) and (ItemIndex<Count));
  result := Items[ItemIndex];
  Items[ItemIndex] := Default(T);
  Delete(ItemIndex);
end;

function TVector<T>.ExtractAll: TArray<T>;
var L: TArray<T>;
begin
  TrimExcess;
  result := Items;
  SetLength(L, 0);
  Items := L;
  FCount := 0;
end;

function TVector<T>.ExtractLast: T;
begin
  Assert(FCount>0);
  Dec(FCount);
  result := Items[FCount];
  Items[FCount] := Default(T);
end;

class operator TVector<T>.GreaterThanOrEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.GreaterThan(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThan(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThan(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.GreaterThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.GreaterThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

procedure TVector<T>.Grow;
begin
  if Capacity < 4 then
    Capacity := Capacity+1
  else
  if Capacity < 64 then
    Capacity := 64
  else
    Capacity := Capacity * 2;
end;

class operator TVector<T>.In(const a: T; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.In(a, b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.In(const a: TArray<T>; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.Implicit(const a: T): TVector<T>;
begin
  result.Init;
  result.Add(A);
end;

class operator TVector<T>.Implicit(const a: TArray<T>): TVector<T>;
begin
  result.Init(a);
end;

class operator TVector<T>.Implicit(const a: TEnumerable<T>): TVector<T>;
begin
  result.Init(a, 0);
end;

class operator TVector<T>.Implicit(a: TVector<T>): TArray<T>;
begin
  result := a.ToArray;
end;

class operator TVector<T>.In(const a: TEnumerable<T>; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

function TVector<T>.IndexOf(const Value: T; Comparer: IComparer<T>): integer;
begin
  if not FindFirst(Value, Result, Comparer) then
    result := -1;
end;

function TVector<T>.IndexOf(const Value: T): integer;
begin
  if not FindFirst(Value, Result) then
    result := -1;
end;

function TVector<T>.FindFirst(const Value: T; var Index: integer): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, TComparerUtils.DefaultComparer<T>);
end;

function TVector<T>.FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, Comparer);
end;

function TVector<T>.FindNext(const Value: T; var Index: integer): boolean;
begin
  result := FindNext(Value, Index, TComparerUtils.DefaultComparer<T>);
end;

function TVector<T>.FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
var
  I: Integer;
begin
  if Comparer = nil then
    Comparer := FData.FDataComparer;
  for I := Index+1 to FData.Count-1 do
    if Comparer.Compare(FData[I], Value)=0 then
    begin
      Index := I;
      Exit(True);
    end;
  result := False;
end;

procedure TVector<T>.FirstPermutation;
begin
  Sort;
end;

function TVector<T>.Insert(Index: integer; const Value: T): integer;
begin
  for result := Add downto Index+1 do
    Items[result] := Items[result-1];
  result := Index;
  Items[result] := Value;
end;

class operator TVector<T>.LessThan(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.LessThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.LessThanOrEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.LessThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

procedure TVector<T>.Move(SrcIndex, DstIndex: integer);
var
  I: integer;
  Value: T;
begin
  if SrcIndex < DstIndex then
  begin
    {      src   dst
      1 2 [3] 4 [5] 6 7 }
    Value := Items[SrcIndex];
    for I := SrcIndex to DstIndex-1 do
      Items[I] := Items[I+1];
    Items[DstIndex] := Value;
  end
  else
  if SrcIndex > DstIndex then
  begin
    {      dst   src
      1 2 [3] 4 [5] 6 7 }
    Value := Items[SrcIndex];
    for I := SrcIndex downto DstIndex+1 do
      Items[I] := Items[I-1];
    Items[DstIndex] := Value;
  end;
end;

function TVector<T>.NextPermutation: boolean;
var
  i,x,n: integer;
  C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;

  { find max N where A[N] < A[N+1] }
  n := -1;
  for i := Count-2 downto 0 do
    if C.Compare(Items[i], Items[i+1]) < 0 then
    begin
      n := i;
      break;
    end;

  { if A[N] > A[N+1] for any N then there is no more permutations }
  result := n<>-1;
  if not result then
    exit;

  { let's order range [N+1; FCount-1]
    now it has reverse order so just call .reverse }
  Reverse(n+1,FCount-n-1);

  { find value next to A[N] in range [N+1; Count-1]
    such value exists because at least original A[N+1] > A[N] }
  x := -1;
  for i := N+1 to Count-1 do
    if C.Compare(Items[i], Items[N]) > 0 then
    begin
      x := i;
      break;
    end;

  { swap A[N] and A[X] }
  Exchange(n, x);

  { change position of A[X] to make range [N+1; FCoun-1] ordered again }
  i := x;
  while (i > n+1) and (C.Compare(Items[i-1], Items[x]) > 0) do
    dec(i);
  while (i < Count-1) and (C.Compare(Items[x], Items[i+1]) > 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

class operator TVector<T>.NotEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

function TVector<T>.PrevPermutation: boolean;
var
  i,x,n: integer;
  C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;

  { find max N where A[N] > A[N+1] }
  n := -1;
  for i := FCount-2 downto 0 do
    if C.Compare(Items[i], Items[i+1]) > 0 then
    begin
      n := i;
      break;
    end;

  { if A[N] > A[N+1] for any N then there is no more permutations }
  result := n<>-1;
  if not result then
    exit;

  { let's order range [N+1; FCoun-1]
    now it has reverse order so just call .reverse }
  reverse(n+1,FCount-n-1);

  { find value previous to A[N] in range [N+1; FCount-1]
    such value exists because at least original A[N+1] < A[N] }
  x := -1;
  for i := N+1 to FCount-1 do
    if C.Compare(Items[i], Items[N]) < 0 then
    begin
      x := i;
      break;
    end;

  { swap A[N] and A[X] }
  Exchange(n,x);

  { change position of A[X] to make range [N+1; FCoun-1] back ordered again }
  i := x;
  while (i > n+1) and (C.Compare(Items[i-1], Items[x]) < 0) do
    dec(i);
  while (i < FCount-1) and (C.Compare(Items[x], Items[i+1]) < 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

procedure TVector<T>.Remove(const V: T; AComparer: IComparer<T>);
var
  I,D: Integer;
begin
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  D := 0;
  for I := 0 to FCount-1 do
    if AComparer.Compare(Items[I], V) <> 0 then
    begin
      Items[D] := Items[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    Items[I] := Default(T);
  FCount := D;
end;

procedure TVector<T>.Remove(const V: TArray<T>; AComparer: IComparer<T>);
var
  S: TVector<T>;
  I,J,D: integer;
begin
  if (Length(V)=0) or (Count=0) then
    Exit;
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  S.Init(TArrayUtils.Copy<T>(V));
  S.Sort(AComparer);
  D := 0;
  for I := 0 to FCount-1 do
    if not S.BinarySearch(Items[I],J,AComparer) then
    begin
      Items[D] := Items[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    Items[I] := Default(T);
  FCount := D;
end;

procedure TVector<T>.Remove(const V: TEnumerable<T>; AComparer: IComparer<T>);
begin
  Remove(V.ToArray, AComparer);
end;

procedure TVector<T>.Remove(AFilter: TFuncFilterValueIndex<T>);
var
  I,D: Integer;
begin
  D := 0;
  for I := 0 to FCount-1 do
    if not AFilter(Items[I], I) then
    begin
      Items[D] := Items[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    Items[I] := Default(T);
  FCount := D;
end;

procedure TVector<T>.Reverse;
begin
  Reverse(0, Count);
end;

procedure TVector<T>.Reverse(AStartIndex, ACount: integer);
var
  I: Integer;
  Value: T;
begin
  if ACount <= 0 then
    Exit;
  Assert((AStartIndex >= 0) and (AStartIndex + ACount <= Count));
  for I := 0 to (ACount shr 1) - 1 do
  begin
    Value := Items[AStartIndex+I];
    Items[AStartIndex+I] := Items[AStartIndex+ACount-1-I];
    Items[AStartIndex+ACount-1-I] := Value;
  end;
end;

procedure TVector<T>.RotateLeft(Index1, Index2, Shift: integer);
var
  I: integer;
begin
  Assert((Index1>=0) and (Index1<Count) and (Index2>=0) and (Index2<Count));
  if Index2 < Index1 then
  begin
    I := Index1;
    Index1 := Index2;
    Index2 := I;
  end;
  I := Index2-Index1+1;
  Shift := (I - (Shift mod I)) mod I;
  if Shift <= 0 then
    if Shift < 0 then
      Inc(Shift, I)
    else
      Exit;
  Reverse(Index1, Index2-Index1+1);
  Reverse(Index1, Shift);
  Reverse(Index1+Shift, Index2-Index1+1-Shift);
end;

procedure TVector<T>.RotateRight(Index1, Index2, Shift: integer);
var
  I: integer;
begin
  Assert((Index1>=0) and (Index1<Count) and (Index2>=0) and (Index2<Count));
  if Index2 < Index1 then
  begin
    I := Index1;
    Index1 := Index2;
    Index2 := I;
  end;
  I := Index2-Index1+1;
  Shift := Shift mod I;
  if Shift <= 0 then
    if Shift < 0 then
      Inc(Shift, I)
    else
      Exit;
  Reverse(Index1, Index2-Index1+1);
  Reverse(Index1, Shift);
  Reverse(Index1+Shift, Index2-Index1+1-Shift);
end;

function TVector<T>.GetCapacity: integer;
begin
  result := Length(Items);
end;

function TVector<T>.GetCount: NativeInt;
begin
  result := FData.Count;
end;

procedure TVector<T>.SetCapacity(ACapacity: integer);
begin
  Assert(ACapacity>=Count);
  SetLength(Items, ACapacity);
end;

function TVector<T>.GetEmpty: Boolean;
begin
  Result := FCount <= 0;
end;

function TVector<T>.GetEnumerator: TEnumerator<T>;
begin
  result := FData.GetEnumerator;
end;

function TVector<T>.GetFirst: T;
begin
  Result := Items[0];
end;

procedure TVector<T>.SetFirst(const Value: T);
begin
  Items[0] := Value;
end;

function TVector<T>.GetLast: T;
begin
  Result := Items[Count-1];
end;

function TVector<T>.GetOnNotify: TCollectionNotifyEvent<T>;
begin
  result := FData.OnNotify;
end;

function TVector<T>.GetOwnsValues: boolean;
begin
  result := FData.OwnsValues;
end;

function TVector<T>.GetSlice: TSlice<T>;
begin
  result.Init(Items, FCount);
end;

function TVector<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
begin
  result.Init(Items, FCount);
  result.Add(AStartSliceIndexIncl, AEndSliceIndexExcl);
end;

function TVector<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
var
  I: Integer;
begin
  result.Init(Items, FCount);
  for I := 0 to FCount-1 do
    if AFilter(Items[I], I) then
      result.Add(I);
end;

function TVector<T>.GetTotalSizeBytes: int64;
begin
  result := (High(Items)-Low(Items)+1)*SizeOf(T);
end;

procedure TVector<T>.SetLast(const Value: T);
begin
  Items[Count-1] := Value;
end;

procedure TVector<T>.SetOnNotify(const Value: TCollectionNotifyEvent<T>);
begin
  FData.OnNotify := Value;
end;

procedure TVector<T>.SetOwnsValues(const Value: boolean);
begin
  FData.OwnsValues := Value;
end;

procedure TVector<T>.Shuffle(AStartIndex, ACount: integer);
var
  I: Integer;
begin
  if ACount <= 1 then
    Exit;
  Assert((AStartIndex >= 0) and (AStartIndex + ACount <= Count));
  for I := ACount-1 downto 1 do
    Exchange(I+AStartIndex, Random(I+1)+AStartIndex);
end;

procedure TVector<T>.Shuffle;
begin
  Shuffle(0, Count);
end;

function TVector<T>.GetItem(ItemIndex: integer): T;
begin
  Assert((ItemIndex >= 0) and (ItemIndex < Count));
  result := Items[ItemIndex];
end;

procedure TVector<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  Assert((ItemIndex >= 0) and (ItemIndex < Count));
  Items[ItemIndex] := Value;
end;

procedure TVector<T>.SetCount(ACount: NativeInt);
var
  I: Integer;
begin
  for I := ACount to Count-1 do
    Items[I] := Default(T);
  FCount := ACount;
  if ACount > Capacity then
    Capacity := ACount;
end;

procedure TVector<T>.Sort;
begin
  TArray.Sort<T>(Items, TComparerUtils.DefaultComparer<T>, 0,Count);
end;

procedure TVector<T>.Sort(AIndex, ACount: Integer);
begin
  TArray.Sort<T>(Items, TComparerUtils.DefaultComparer<T>, AIndex, ACount);
end;

procedure TVector<T>.Sort(Comparer: IComparer<T>);
begin
  Sort(Comparer, 0, Count);
end;

procedure TVector<T>.Sort(Comparer: IComparer<T>; AIndex, ACount: Integer);
begin
  TArray.Sort<T>(Items, Comparer, AIndex, ACount);
end;

procedure TVector<T>.Sort(Comparison: TComparison<T>);
begin
  Sort(Comparison, 0, Count);
end;

procedure TVector<T>.Sort(Comparison: TComparison<T>; AIndex, ACount: Integer);
var
  C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(Comparison);
  TArray.Sort<T>(Items, C, AIndex, ACount);
end;

function TVector<T>.Sorted: boolean;
var C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;
  result := TArrayUtils.Sorted<T>(Items, 0, Count, C);
end;

function TVector<T>.Sorted(AStartIndex, ACount: integer; AComparer: IComparer<T>): boolean;
begin
  result := TArrayUtils.Sorted<T>(Items, AStartIndex, ACount, AComparer);
end;

function TVector<T>.Sorted(AStartIndex, ACount: integer; AComparison: TComparison<T>): boolean;
var C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(AComparison);
  result := TArrayUtils.Sorted<T>(Items, AStartIndex, ACount, C);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: TArray<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(a, b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: T): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(const a: TArray<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(const a: T; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer): Boolean;
var C: IComparer<T>;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, TComparerUtils.DefaultComparer<T>, 0, Count);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, Comparer, 0, Count);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, Comparer, AIndex,ACount);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex, ACount: Integer): Boolean;
begin
  result := BinarySearch(Item, FoundIndex, TDelegatedComparer<T>.Create(Comparison), AIndex, ACount);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean;
begin
  result := BinarySearch(Item, FoundIndex, TDelegatedComparer<T>.Create(Comparison), 0, Count);
end;

procedure TVector<T>.Add(Value: TVector<T>);
var
  I: Integer;
begin
  for I := 0 to Value.Count-1 do
    Items[Add] := Value[I];
end;

class operator TVector<T>.Add(a: TVector<T>; const b: TArray<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a, b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a: TVector<T>; const b: T): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: TArray<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: T; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

end.
