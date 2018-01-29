unit adot.Alg.IntegralImage;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils;

type
  { Summed area table. Precalculates sums for very fast calculation of SUM of any area [x1,y1,x2,y2].
    https://en.wikipedia.org/wiki/Summed_area_table}
  TIntegralImageInt64 = record
  private
    procedure BuildLine(ADst: PInt64Array);
    function GetLine(y: integer): PInt64Array;
    function GetSum(x1, y1, x2, y2: integer): int64;
    function GetAvg(x1, y1, x2, y2: integer): int64;
  public
    Image: TArray<int64>;
    Width,Height: integer;

    procedure Init;

    procedure SetSize(AWidth,AHeight: integer);

    procedure Build;

    { release memory etc }
    procedure Clear;

    { usefull to fill with initial values }
    property Lines[y: integer]: PInt64Array read GetLine;

    { Fastest, no range checks etc }
    property Sum[x1,y1,x2,y2: integer]:int64 read GetSum;

    { positions are adjusted to be inside of the image }
    property Avg[x1,y1,x2,y2: integer]:int64 read GetAvg; default;
  end;

  TInterpolation_Int64Custom = class
  public
    type
      TPt = record
        X,Y: int64;
      end;

  protected
    FPoints: TArray<TPt>;
    FUpdateCnt: integer;
    FComparer: IComparer<TPt>;

    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
    function DoGetValue(const X: int64):int64; virtual; abstract;

    function GetPointCount: integer;
    function GetPoint(i: integer): TPt;
    procedure SetPoint(i: integer; const Value: TPt);

  public
    constructor Create(PointCount: integer); virtual;

    procedure BeginUpdate;
    procedure EndUpdate;

    property PointCount: integer read GetPointCount;
    property Points[i: integer]: TPt read GetPoint write SetPoint;

    property Values[const x: int64]: int64 read DoGetValue; default;
  end;

  TLinearInterpolation_Int64 = class(TInterpolation_Int64Custom)
  protected
    function DoGetValue(const X: int64):int64; override;
  end;

implementation

{ TIntegralImageInt64 }

procedure TIntegralImageInt64.Init;
begin
  Self := Default(TIntegralImageInt64);
end;

procedure TIntegralImageInt64.SetSize(AWidth, AHeight: integer);
begin
  Width := AWidth;
  Height := AHeight;
  SetLength(Image, 0);
  SetLength(Image, Width*Height);
end;

procedure TIntegralImageInt64.Build;
var
  x,y: Integer;
begin
  for x := 1 to Width-1 do
    Inc(Image[x],Image[x-1]);
  for y := 1 to Height-1 do
    Inc(Image[y*Width],Image[(y-1)*Width]);
  for y := 1 to Height-1 do
    BuildLine(@Image[y*Width+1]);
end;

(*
  #####
  #   #
  ####?
*)
procedure TIntegralImageInt64.BuildLine(ADst: PInt64Array);
var
  x: Integer;
begin
  for x := 0 to Width-2 do
    inc(ADst[x], ADst[x-1]+ADst[x-Width]-ADst[x-Width-1]);
end;

procedure TIntegralImageInt64.Clear;
begin
  SetSize(0,0);
end;

function TIntegralImageInt64.GetLine(y: integer): PInt64Array;
begin
  result := @Image[y*Width];
end;

function TIntegralImageInt64.GetSum(x1, y1, x2, y2: integer): int64;
begin
  Result := Image[x2+y2*Width];
  if x1>0 then
  begin
    if y1>0 then
      Inc(Result, Image[x1-1+(y1-1)*Width]);
    dec(Result, Image[x1-1+y2*Width]);
  end;
  if y1>0 then
    dec(Result, Image[x2+(y1-1)*Width]);
end;

function TIntegralImageInt64.GetAvg(x1, y1, x2, y2: integer): int64;
begin
  assert((x2>=x1) and (y2>=y1));
  if x2-x1+1>width then
    x2 := x1+width-1;
  if y2-y1+1>height then
    y2 := y1+height-1;
  if x1<0 then
  begin
    dec(x2, x1);
    x1 := 0;
  end;
  if y1<0 then
  begin
    dec(y2, y1);
    y1 := 0;
  end;
  if x2>=width then
  begin
    dec(x1,x2-width+1);
    x2 := width-1;
  end;
  if y2>=height then
  begin
    dec(y1,y2-height+1);
    y2 := height-1;
  end;
  result := Sum[x1,y1,x2,y2] div ((x2-x1+1)*(y2-y1+1));
end;

{ TInterpolation_Int64Custom }

constructor TInterpolation_Int64Custom.Create(PointCount: integer);
begin
  SetLength(FPoints, PointCount);
  FComparer := TDelegatedComparer<TPt>.Create(
    function (const A, B: TPt): integer
    begin
      if A.X < B.X then result := -1 else
        if A.X = B.X then result := 0 else
          result := 1;
    end);
end;

procedure TInterpolation_Int64Custom.BeginUpdate;
begin
  inc(FUpdateCnt);
  if FUpdateCnt=1 then
    DoBeginUpdate;
end;

procedure TInterpolation_Int64Custom.EndUpdate;
begin
  dec(FUpdateCnt);
  if FUpdateCnt=0 then
    DoEndUpdate;
end;

procedure TInterpolation_Int64Custom.DoBeginUpdate;
begin
end;

procedure TInterpolation_Int64Custom.DoEndUpdate;
var
  I: Integer;
begin

  { reorder if necessary }
  for I := 0 to High(FPoints)-1 do
    if FPoints[I].X > FPoints[I+1].X then
    begin
      TArray.Sort<TPt>(FPoints, FComparer);
      Break;
    end;

  { It is not allowed to have Xi=Xj for any i<>j,
    items are ordered, so we can check it eficiently. }
  for I := 0 to High(FPoints)-1 do
    if FPoints[I].X=FPoints[I+1].X then
      raise Exception.Create('Error');
end;

function TInterpolation_Int64Custom.GetPoint(i: integer): TPt;
begin
  result := FPoints[i];
end;

procedure TInterpolation_Int64Custom.SetPoint(i: integer; const Value: TPt);
begin
  Assert((FUpdateCnt>0) and (i>=0) and (i<=High(FPoints)));
  FPoints[i] := Value;
end;

function TInterpolation_Int64Custom.GetPointCount: integer;
begin
  result := Length(FPoints);
end;

{ TLinearInterpolation_Int64 }

function TLinearInterpolation_Int64.DoGetValue(const X: int64): int64;
var
  Item: TPt;
  FoundIndex: Integer;
begin
  Assert(Length(FPoints)>0);
  Item.X := X;
  Item.Y := 0;
  if TArray.BinarySearch<TPt>(FPoints, Item, FoundIndex, FComparer) then
    result := FPoints[FoundIndex].Y
  else
    { If not found, FoundIndex contains the index of the first entry larger than Item }
    if FoundIndex = 0 then result := FPoints[0].Y else
      if FoundIndex > High(FPoints) then result := FPoints[High(FPoints)].Y else
        { Xi<>Xj for any i<>j (check DoEndUpdate), so div by zero is not possible here. }
        result := FPoints[FoundIndex-1].Y +
          (X-FPoints[FoundIndex-1].X) *
          (FPoints[FoundIndex].Y-FPoints[FoundIndex-1].Y) div
          (FPoints[FoundIndex].X-FPoints[FoundIndex-1].X);
end;

end.
