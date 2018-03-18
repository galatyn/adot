unit adot.Collections.Slices;

interface

uses
  adot.Types,
  adot.Collections,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.SysUtils,
  System.Math;

type
  TSlice<T> = record
  private
    FValues: TArray<T>;
    FValuesCount: integer;    { FValuesCount <= array.capacity = length(FValues) }
    FSlice: TArray<integer>;
    FSliceCount: integer;     { FSliceCount <= slice.capacity = length(FSlice) }

    function GetArrayIndex(SliceIndex: integer): integer;
    function GetValue(SliceIndex: integer): T;
    procedure SetArrayIndex(SliceIndex: integer; const Value: integer);
    procedure SetCount(const Value: integer);
    procedure SetValue(SliceIndex: integer; const Value: T);
    procedure ExpandSlice;
    class function GetExpandSize(CurSize: integer): integer; static;
    procedure ExpandValues;

  public
    type
      TSliceEnumerator = class(TEnumerator<T>)
      protected
        FValues: TArray<T>;
        FSlice: TArray<integer>;
        FSliceCount: integer;     { FSliceCount <= slice.capacity = length(FSlice) }
        FPosition: integer;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(var Slice: TSlice<T>);
      end;

  public
    { Init is preferred over Create, it is obviously distinguished from classes.
      Class-like Create can be useful when new instance is param of some routine }
    { Init empty slice on empty array (can be populated by Append method) }
    procedure Init; overload;
    { Init empty slice on specified array }
    procedure Init(const AValues: TArray<T>); overload;
    procedure Init(const AValues: TArray<T>; AValuesCount: integer); overload;
    { Init slice for range of values from specified array }
    procedure Init(const AValues: TArray<T>; AStartIndexIncl,AEndIndexExcl: integer); overload;
    { Init slice on specified array and select some values by filter function }
    procedure Init(const AValues: TArray<T>; AFilter: TFuncFilterValueIndex<T>); overload;

    { Slice from array }
    class function Create: TSlice<T>; overload; static;
    class function Create(const AValues: TArray<T>): TSlice<T>; overload; static;
    class function Create(const AValues: TArray<T>; AStartIndexIncl,AEndIndexExcl: integer): TSlice<T>; overload; static;
    class function Create(const AValues: TArray<T>; AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload; static;

    { Creates copy of the slice (pointing same array) }
    function GetCopy: TSlice<T>;

    { Creates empty slice on same array }
    function GetSlice: TSlice<T>; overload;
    { Creates subrange slice on same array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates new slice on same array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

    { Clear the slice (will not change underlying array) }
    procedure Clear;
    { Compact the slice (will not change underlying array) }
    procedure Compact;

    {
      Adds new array index into the slice. Returns new slice index.
        for I := Low(Arr) to High(Arr) do
          if I mod 3 = 0 then
            Slice.Add(I);
    }
    function Add(ArrayIndex: integer): integer; overload;
    procedure Add(AStartArrayIndexIncl,AEndArrayIndexExcl: integer); overload;
    procedure Add(ArrayIndices: TEnumerable<integer>); overload;
    procedure Add(const ArrayIndices: TArray<integer>); overload;

    {
      Appends the array. Returns new slice index.
      Can be used as simple way to populate the array with data.
        for I := Low(Src) to High(Dst) do
          if WeLikeIt(Src[I]) then
            Slice.Append(Src[I]);
    }
    function Append(Value: T): integer; overload;
    procedure Append(Values: TEnumerable<T>); overload;
    procedure Append(const Values: TArray<T>); overload;

    function GetEnumerator: TEnumerator<T>;

    { Sort the slice. Underlying array will remain unchanged }
    procedure Sort; overload;
    procedure Sort(AComparison: TComparison<T>); overload;
    procedure Sort(AComparer: IComparer<T>); overload;
    procedure Sort(AStartIndexIncl,AEndIndexExcl: integer); overload;
    procedure Sort(AStartIndexIncl,AEndIndexExcl: integer; AComparison: TComparison<T>); overload;
    procedure Sort(AStartIndexIncl,AEndIndexExcl: integer; AComparer: IComparer<T>); overload;

    { The slice must be sorted }
    function BinarySearch(const Value: T; out FoundIndex: integer): Boolean; overload;
    function BinarySearch(const Value: T; out FoundIndex: integer; AComparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Value: T; out FoundIndex: integer; AComparison: TComparison<T>): Boolean; overload;
    function BinarySearch(const Value: T; out FoundIndex: integer; AStartIndexIncl,AEndIndexExcl: integer): Boolean; overload;
    function BinarySearch(const Value: T; out FoundIndex: integer; AStartIndexIncl,AEndIndexExcl: integer; AComparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Value: T; out FoundIndex: integer; AStartIndexIncl,AEndIndexExcl: integer; AComparison: TComparison<T>): Boolean; overload;

    { The slice must be sorted }
    function Contains(const Value: T): Boolean; overload;
    function Contains(const Value: T; AComparer: IComparer<T>): Boolean; overload;
    function Contains(const Value: T; AComparison: TComparison<T>): Boolean; overload;
    function Contains(const Value: T; AStartIndexIncl,AEndIndexExcl: integer): Boolean; overload;
    function Contains(const Value: T; AStartIndexIncl,AEndIndexExcl: integer; AComparer: IComparer<T>): Boolean; overload;
    function Contains(const Value: T; AStartIndexIncl,AEndIndexExcl: integer; AComparison: TComparison<T>): Boolean; overload;

    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;
    function ToArray: TArray<T>;

    function First: T;
    function Last: T;

    { the slice }
    property Count: integer read FSliceCount write SetCount;
    property Values[SliceIndex: integer]: T read GetValue write SetValue; default;
    property Indices[SliceIndex: integer]: integer read GetArrayIndex write SetArrayIndex;
    { underlying array }
    property ValuesArray: TArray<T> read FValues;
    { returns number of items added by Append }
    property ValuesArrayLength: integer read FValuesCount;
  end;

implementation

uses
  adot.Tools,
  adot.Tools.RTTI;

{ TSlice<T>.TSliceEnumerator<T> }

constructor TSlice<T>.TSliceEnumerator.Create(var Slice: TSlice<T>);
begin
  FValues     := Slice.FValues;
  FSlice      := Slice.FSlice;
  FSliceCount := Slice.FSliceCount;
  FPosition   := 0;
end;

function TSlice<T>.TSliceEnumerator.DoGetCurrent: T;
begin
  result := FValues[FSlice[FPosition-1]];
end;

function TSlice<T>.TSliceEnumerator.DoMoveNext: Boolean;
begin
  result := FPosition < FSliceCount;
  inc(FPosition);
end;

{ TSlice<T> }

procedure TSlice<T>.Init(const AValues: TArray<T>);
begin
  Self := Default(TSlice<T>);
  FValues := AValues;
  FValuesCount := Length(FValues);
end;

procedure TSlice<T>.Init(const AValues: TArray<T>; AValuesCount: integer);
begin
  Self := Default(TSlice<T>);
  FValues := AValues;
  FValuesCount := AValuesCount;
end;

procedure TSlice<T>.Init(const AValues: TArray<T>; AFilter: TFuncFilterValueIndex<T>);
var
  I: Integer;
begin
  Self := Default(TSlice<T>);
  FValues      := AValues;
  FValuesCount := Length(FValues);               { array.Count = array.Capacity }
  for I := 0 to High(AValues) do
    if AFilter(AValues[I], I) then
      Add(I);
end;

procedure TSlice<T>.Init(const AValues: TArray<T>; AStartIndexIncl, AEndIndexExcl: integer);
var
  I: Integer;
begin
  FValues      := AValues;
  FValuesCount := Length(FValues);               { array.Count = array.Capacity }
  FSliceCount  := AEndIndexExcl-AStartIndexIncl; { slice.Count = slice.Capacity }
  SetLength(FSlice, FSliceCount);
  for I := 0 to FSliceCount-1 do
    FSlice[I] := I + AStartIndexIncl;
end;

procedure TSlice<T>.Init;
begin
  Self := Default(TSlice<T>);
end;

class function TSlice<T>.Create(const AValues: TArray<T>; AStartIndexIncl, AEndIndexExcl: integer): TSlice<T>;
begin
  result.Init(AValues, AStartIndexIncl, AEndIndexExcl);
end;

class function TSlice<T>.Create(const AValues: TArray<T>): TSlice<T>;
begin
  result.Init(AValues);
end;

class function TSlice<T>.Create: TSlice<T>;
begin
  result.Init;
end;

class function TSlice<T>.Create(const AValues: TArray<T>; AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
begin
  result.Init(AValues, AFilter);
end;

class function TSlice<T>.GetExpandSize(CurSize: integer): integer;
begin
  if CurSize < 8 then
    result := 8
  else
  if CurSize < 10000000
    then result := CurSize*2        { most common case - extend by factor x2   }
    else result := CurSize*3 div 2; { for huge slices  - extend by factor x1.5 }
end;

procedure TSlice<T>.ExpandSlice;
begin
  SetLength(FSlice, GetExpandSize(Length(FSlice)));
end;

procedure TSlice<T>.ExpandValues;
begin
  SetLength(FValues, GetExpandSize(Length(FValues)));
end;

procedure TSlice<T>.Add(ArrayIndices: TEnumerable<integer>);
var
  ArrayIndex: integer;
begin
  for ArrayIndex in ArrayIndices do
    Add(ArrayIndex);
end;

procedure TSlice<T>.Add(const ArrayIndices: TArray<integer>);
var
  ArrayIndex: integer;
begin
  for ArrayIndex in ArrayIndices do
    Add(ArrayIndex);
end;

function TSlice<T>.Add(ArrayIndex: integer): integer;
begin
  Assert((ArrayIndex>=0) and (ArrayIndex<FValuesCount));
  if FSliceCount >= Length(FSlice) then
    ExpandSlice;
  result := FSliceCount;
  FSlice[result] := ArrayIndex;
  inc(FSliceCount);
end;

procedure TSlice<T>.Add(AStartArrayIndexIncl,AEndArrayIndexExcl: integer);
var
  L,I: Integer;
begin
  if AEndArrayIndexExcl <= AStartArrayIndexIncl then
    Exit;
  Assert((AStartArrayIndexIncl>=0) and (AEndArrayIndexExcl<=FValuesCount));
  L := AEndArrayIndexExcl - AStartArrayIndexIncl;
  if FSliceCount + L > Length(FSlice) then
    SetLength(FSlice, Max(FSliceCount + L, GetExpandSize(Length(FSlice))));
  for I := 0 to L-1 do
    FSlice[I+FSliceCount] := I+AStartArrayIndexIncl;
  inc(FSliceCount, L);
end;

function TSlice<T>.Append(Value: T): integer;
begin
  if FValuesCount >= Length(FValues) then
    ExpandValues;
  FValues[FValuesCount] := Value;
  if FSliceCount >= Length(FSlice) then
    ExpandSlice;
  result := FSliceCount;
  FSlice[result] := FValuesCount;
  inc(FSliceCount);
  inc(FValuesCount);
end;

procedure TSlice<T>.Append(Values: TEnumerable<T>);
var
  Value: T;
begin
  for Value in Values do
    Append(Value);
end;

procedure TSlice<T>.Append(const Values: TArray<T>);
var
  Value: T;
begin
  for Value in Values do
    Append(Value);
end;

function TSlice<T>.BinarySearch(const Value: T; out FoundIndex: integer; AStartIndexIncl,AEndIndexExcl: integer; AComparison: TComparison<T>): Boolean;
begin
  result := BinarySearch(Value, FoundIndex, AStartIndexIncl, AEndIndexExcl, TDelegatedComparer<T>.Create(AComparison));
end;

function TSlice<T>.BinarySearch(const Value: T; out FoundIndex: integer): Boolean;
var C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;
  result := BinarySearch(Value, FoundIndex, 0, FSliceCount, C);
end;

function TSlice<T>.BinarySearch(const Value: T; out FoundIndex: integer; AComparer: IComparer<T>): Boolean;
begin
  result := BinarySearch(Value, FoundIndex, 0, FSliceCount, AComparer);
end;

function TSlice<T>.BinarySearch(const Value: T; out FoundIndex: integer; AComparison: TComparison<T>): Boolean;
begin
  result := BinarySearch(Value, FoundIndex, 0, FSliceCount, TDelegatedComparer<T>.Create(AComparison));
end;

function TSlice<T>.BinarySearch(const Value: T; out FoundIndex: integer; AStartIndexIncl,AEndIndexExcl: integer): Boolean;
var C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;
  result := BinarySearch(Value, FoundIndex, 0, FSliceCount, C);
end;

function TSlice<T>.BinarySearch(const Value: T; out FoundIndex: integer; AStartIndexIncl,AEndIndexExcl: integer; AComparer: IComparer<T>): Boolean;
var
  M,C: Integer;
begin
  Result := AEndIndexExcl > AStartIndexIncl;
  if not Result then
    Exit;
  Assert((AStartIndexIncl>=0) and (AEndIndexExcl<=FSliceCount));
  dec(AEndIndexExcl); { we need [L;R] (including right side) }
  while (AEndIndexExcl-AStartIndexIncl > 1) do
  begin
    M := (AStartIndexIncl+AEndIndexExcl) shr 1;
    C := AComparer.Compare(FValues[FSlice[M]], Value);
    if C < 0 then AStartIndexIncl := M else
      if C > 0 then AEndIndexExcl := M else
      begin
        FoundIndex := M;
        Exit;
      end;
  end;
  if AComparer.Compare(FValues[FSlice[AStartIndexIncl]], Value) = 0 then
    FoundIndex := AStartIndexIncl
  else
  if (AEndIndexExcl<>AStartIndexIncl) and (AComparer.Compare(FValues[FSlice[AEndIndexExcl]], Value) = 0) then
    FoundIndex := AEndIndexExcl
  else
    Result := False;
end;

function TSlice<T>.Contains(const Value: T; AStartIndexIncl,AEndIndexExcl: integer; AComparer: IComparer<T>): Boolean;
var
  FoundIndex: integer;
begin
  result := BinarySearch(Value, FoundIndex, AStartIndexIncl,AEndIndexExcl, AComparer);
end;

function TSlice<T>.Contains(const Value: T; AStartIndexIncl,AEndIndexExcl: integer; AComparison: TComparison<T>): Boolean;
var
  FoundIndex: integer;
begin
  result := BinarySearch(Value, FoundIndex, AStartIndexIncl,AEndIndexExcl, AComparison);
end;

function TSlice<T>.Contains(const Value: T; AStartIndexIncl,AEndIndexExcl: integer): Boolean;
var
  FoundIndex: integer;
begin
  result := BinarySearch(Value, FoundIndex, AStartIndexIncl,AEndIndexExcl);
end;

function TSlice<T>.Contains(const Value: T): Boolean;
var
  FoundIndex: integer;
begin
  result := BinarySearch(Value, FoundIndex);
end;

function TSlice<T>.Contains(const Value: T; AComparer: IComparer<T>): Boolean;
var
  FoundIndex: integer;
begin
  result := BinarySearch(Value, FoundIndex, AComparer);
end;

function TSlice<T>.Contains(const Value: T; AComparison: TComparison<T>): Boolean;
var
  FoundIndex: integer;
begin
  result := BinarySearch(Value, FoundIndex, AComparison);
end;

procedure TSlice<T>.Clear;
begin
  FSliceCount := 0;
end;

procedure TSlice<T>.Compact;
begin
  SetLength(FSlice, FSliceCount);
end;

function TSlice<T>.First: T;
begin
  Assert(FSliceCount>0);
  result := FValues[FSlice[0]];
end;

function TSlice<T>.Last: T;
begin
  Assert(FSliceCount>0);
  result := FValues[FSlice[FSliceCount-1]];
end;

function TSlice<T>.GetArrayIndex(SliceIndex: integer): integer;
begin
  Assert((SliceIndex>=0) and (SliceIndex<FSliceCount));
  result := FSlice[SliceIndex];
end;

procedure TSlice<T>.SetArrayIndex(SliceIndex: integer; const Value: integer);
begin
  Assert((SliceIndex>=0) and (SliceIndex < FSliceCount));
  FSlice[SliceIndex] := Value;
end;

function TSlice<T>.GetCopy: TSlice<T>;
begin
  result := Default(TSlice<T>);
  result.FValues      := FValues;
  result.FValuesCount := FValuesCount;
  result.FSlice       := TArrayUtils.Copy<integer>(FSlice, 0, FSliceCount);
  result.FSliceCount  := FSliceCount;
end;

function TSlice<T>.GetEnumerator: TEnumerator<T>;
begin
  result := TSliceEnumerator.Create(Self);
end;

function TSlice<T>.GetSlice: TSlice<T>;
begin
  result := Default(TSlice<T>);
  result.FValues := FValues;
  result.FValuesCount := FValuesCount;
end;

function TSlice<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
var
  I: Integer;
begin
  result := Default(TSlice<T>);
  result.FValues := FValues;
  result.FValuesCount := FValuesCount;
  for I := 0 to FSliceCount-1 do
    if AFilter(FValues[FSlice[I]], I) then
      result.Add(FSlice[I]);
end;

function TSlice<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
var
  I: Integer;
begin
  result := Default(TSlice<T>);
  result.FValues := FValues;
  result.FValuesCount := FValuesCount;
  result.FSlice := TArrayUtils.Copy<integer>(FSlice, AStartSliceIndexIncl, AEndSliceIndexExcl-AStartSliceIndexIncl);
  if AEndSliceIndexExcl >= AStartSliceIndexIncl
    then result.FSliceCount := AEndSliceIndexExcl-AStartSliceIndexIncl
    else result.FSliceCount := 0;
end;

function TSlice<T>.GetValue(SliceIndex: integer): T;
begin
  Assert((SliceIndex>=0) and (SliceIndex < FSliceCount));
  result := FValues[FSlice[SliceIndex]];
end;

procedure TSlice<T>.SetValue(SliceIndex: integer; const Value: T);
begin
  Assert((SliceIndex>=0) and (SliceIndex < FSliceCount));
  FValues[FSlice[SliceIndex]] := Value;
end;

procedure TSlice<T>.SetCount(const Value: integer);
begin
  if Value > Length(FSlice) then
    SetLength(FSlice, Value);
  FSliceCount := Value;
end;

procedure TSlice<T>.Sort;
var
  C: IComparer<T>;
begin
  {
    We can't call directly
      Sort(TComparerUtils.DefaultComparer<T>)
    because of bug in Delphi (tested in Delphi 10.2.3)
  }
  C := TComparerUtils.DefaultComparer<T>;
  Sort(C);
end;

procedure TSlice<T>.Sort(AComparer: IComparer<T>);
begin
  Sort(0, FSliceCount, AComparer);
end;

procedure TSlice<T>.Sort(AComparison: TComparison<T>);
begin
  Sort(0, FSliceCount, TDelegatedComparer<T>.Create(AComparison));
end;

procedure TSlice<T>.Sort(AStartIndexIncl,AEndIndexExcl: integer);
var
  C: IComparer<T>;
begin
  {
    We can't call directly
      Sort(AStartIndexIncl,AEndIndexExcl, TComparerUtils.DefaultComparer<T>);
    because of bug in Delphi (tested in Delphi 10.2.3)
  }
  C := TComparerUtils.DefaultComparer<T>;
  Sort(AStartIndexIncl,AEndIndexExcl, C);
end;

procedure TSlice<T>.Sort(AStartIndexIncl,AEndIndexExcl: integer; AComparison: TComparison<T>);
begin
  Sort(AStartIndexIncl,AEndIndexExcl, TDelegatedComparer<T>.Create(AComparison));
end;

procedure TSlice<T>.Sort(AStartIndexIncl,AEndIndexExcl: integer; AComparer: IComparer<T>);
var
  Values: TArray<T>;
  IndexComparer: IComparer<integer>;
begin
  if AEndIndexExcl <= AStartIndexIncl then
    Exit;
  Assert((AStartIndexIncl>=0) and (AEndIndexExcl<=FSliceCount));
  Values := FValues;
  IndexComparer := TDelegatedComparer<integer>.Create(
    function(const L,R: integer): integer
    begin
      result := AComparer.Compare(Values[L], Values[R]);
    end);
  TArray.Sort<integer>(FSlice, IndexComparer, AStartIndexIncl, AEndIndexExcl-AStartIndexIncl);
end;

function TSlice<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, FSliceCount);
  for I := 0 to FSliceCount-1 do
    result[I] := FValues[FSlice[I]];
end;

function TSlice<T>.ToString: string;
begin
  result := ToText(' ');
end;

function TSlice<T>.ToText(const ValuesDelimiter: string): string;
var
  S: TStringBuilder;
  I: Integer;
begin
  S := TStringBuilder.Create;
  for I := 0 to FSliceCount-1 do
  begin
    if S.Length > 0 then
      S.Append(ValuesDelimiter);
    S.Append(TRttiUtils.ValueAsString<T>(FValues[FSlice[I]]));
  end;
  result := S.ToString;
end;

end.
