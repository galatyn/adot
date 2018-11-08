unit adot.Collections.Lists;

interface

{
  TDoublyLinkedListClass<T>
}

uses
  adot.Types,
  adot.Collections.Types,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils;

type
  { Doubly linked list }
  TDoublyLinkedListClass<T> = class(TEnumerableExt<T>)
  public
    type

      { We use allocation of items by AllocMem, no extra initialization is needed }
      PDoublyLinkedListItem = ^TDoublyLinkedListItem;
      TDoublyLinkedListItem = record
        Data: T;
        Prev: PDoublyLinkedListItem;
        Next: PDoublyLinkedListItem;
      end;

      TDListEnumerator = class(TEnumerator<T>)
      protected
        Data,First,Last: PDoublyLinkedListItem;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(AFirst,ALast: PDoublyLinkedListItem);
      end;

  protected
    FFront: PDoublyLinkedListItem;
    FBack:  PDoublyLinkedListItem;
    FCount: integer;
    FOwnsValues: boolean;
    FComparer: IComparer<T>;

    function DoGetEnumerator: TEnumerator<T>; override;
    procedure SetOwnsValues(AOwnsValues: boolean);
    class function MergeSortedRanges(C: PDoublyLinkedListItem; Comparer: IComparer<T>): PDoublyLinkedListItem;
    class procedure MergeSort(List: TDoublyLinkedListClass<T>; AFirst, ALast: PDoublyLinkedListItem; Comparer: IComparer<T>);
    function GetIsConsistent: boolean;
    function GetEmpty: boolean;

  public
    constructor Create(Comparer: IComparer<T> = nil); overload;
    constructor Create(const AValues: array of T; Comparer: IComparer<T> = nil); overload;
    constructor Create(const AValues: TEnumerable<T>; Comparer: IComparer<T> = nil); overload;
    destructor Destroy; override;

    procedure Assign(Src: TEnumerable<T>); overload;
    procedure Assign(Src: TDoublyLinkedListClass<T>); overload;
    procedure Assign(const Src: TArray<T>); overload;

    function AllocItem: PDoublyLinkedListItem; overload;
    function AllocItem(const Value: T): PDoublyLinkedListItem; overload;
    procedure FreeItem(Item: PDoublyLinkedListItem);
    function ExtractItem(Item: PDoublyLinkedListItem): PDoublyLinkedListItem;

    { [X,Y,Z].AddToFront([A,B,C]) -> A,B,C,X,Y,Z }
    procedure AddToFront(Value: T); overload;
    procedure AddToFront(const AValues: array of T); overload;
    procedure AddToFront(const AValues: TEnumerable<T>); overload;

    { [X,Y,Z].AddToBack([A,B,C]) -> X,Y,Z,A,B,C }
    procedure AddToBack(Value: T); overload;
    procedure AddToBack(const AValues: array of T); overload;
    procedure AddToBack(const AValues: TEnumerable<T>); overload;

    procedure InsertBefore(Value: T; Dst: PDoublyLinkedListItem);
    procedure InsertAfter(Value: T; Dst: PDoublyLinkedListItem);

    procedure Delete(Item: PDoublyLinkedListItem); overload;
    procedure DeleteRange(ItemStart,ItemEnd: PDoublyLinkedListItem); overload;
    function Remove(Value: T): integer; overload;
    function Remove(Value: T; Comparer: IComparer<T>): integer; overload;
    procedure Clear;

    function Extract(Item: PDoublyLinkedListItem): T;
    function ExtractFront: T;
    function ExtractBack: T;

    { Remove duplicate values }
    procedure Unique;
    procedure Sort; overload;
    procedure Sort(Comparer: IComparer<T>); overload;

    function Find(Value: T): PDoublyLinkedListItem; overload;
    function Find(Value: T; Comparer: IComparer<T>): PDoublyLinkedListItem; overload;
    function FindNext(Item: PDoublyLinkedListItem): PDoublyLinkedListItem; overload;
    function FindNext(Item: PDoublyLinkedListItem; Comparer: IComparer<T>): PDoublyLinkedListItem; overload;

    function FindByIndex(Index: integer): PDoublyLinkedListItem;
    function FindValueByIndex(Index: integer): T;

    procedure Exchange(Item1, Item2: PDoublyLinkedListItem);
    procedure Reverse(FirstItem, LastItem: PDoublyLinkedListItem; CheckDirection: boolean = True);
    procedure Rotate(FirstItem, LastItem: PDoublyLinkedListItem; Shift: integer);

    property Front: PDoublyLinkedListItem read FFront;
    property Back: PDoublyLinkedListItem read FBack;
    property Count: integer read FCount;
    property IsEmpty: boolean read GetEmpty;
    property OwnsValues: boolean read FOwnsValues write SetOwnsValues;
    property IsConsistent: boolean read GetIsConsistent;
  end;

implementation

uses
  adot.Collections,
  adot.Collections.Sets,
  adot.Tools.RTTI;

{ TDoublyLinkedListClass<T>.TDListEnumerator }

constructor TDoublyLinkedListClass<T>.TDListEnumerator.Create(AFirst, ALast: PDoublyLinkedListItem);
begin
  inherited Create;
  First := AFirst;
  Last := ALast;
  Data := nil;
end;

function TDoublyLinkedListClass<T>.TDListEnumerator.DoMoveNext: Boolean;
begin
  result := (First<>nil);
  if result then
  begin
    Data := First;
    if (First=Last) then
      First := nil
    else
      First := First.Next;
  end;
end;

function TDoublyLinkedListClass<T>.TDListEnumerator.DoGetCurrent: T;
begin
  result := Data.Data;
end;

{ TDoublyLinkedListClass<T> }

constructor TDoublyLinkedListClass<T>.Create(Comparer: IComparer<T>);
begin
  inherited Create;
  if Comparer = nil then
    FComparer := TComparerUtils.DefaultComparer<T>
  else
    FComparer := Comparer;
end;

constructor TDoublyLinkedListClass<T>.Create(const AValues: TEnumerable<T>; Comparer: IComparer<T> = nil);
begin
  Create(Comparer);
  AddToBack(AValues);
end;

constructor TDoublyLinkedListClass<T>.Create(const AValues: array of T; Comparer: IComparer<T> = nil);
begin
  Create(Comparer);
  AddToBack(AValues);
end;

destructor TDoublyLinkedListClass<T>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TDoublyLinkedListClass<T>.AddToBack(Value: T);
var
  Item: PDoublyLinkedListItem;
begin
  if FBack <> nil then
    InsertAfter(Value, FBack)
  else
  begin
    Item := AllocItem(Value);
    FFront := Item;
    FBack  := Item;
    inc(FCount);
  end;
end;

procedure TDoublyLinkedListClass<T>.AddToBack(const AValues: array of T);
var
  I: integer;
begin
  for I := Low(AValues) to High(AValues) do
    AddToBack(AValues[I]);
end;

procedure TDoublyLinkedListClass<T>.AddToBack(const AValues: TEnumerable<T>);
var
  Value: T;
begin
  for Value in AValues do
    AddToBack(Value);
end;

procedure TDoublyLinkedListClass<T>.AddToFront(Value: T);
var
  Item: PDoublyLinkedListItem;
begin
  if FFront <> nil then
    InsertBefore(Value, FFront)
  else
  begin
    Item := AllocItem(Value);
    FFront := Item;
    FBack  := Item;
    inc(FCount);
  end;
end;

procedure TDoublyLinkedListClass<T>.AddToFront(const AValues: array of T);
var
  I: integer;
begin
  for I := High(AValues) downto Low(AValues) do
    AddToFront(AValues[I]);
end;

procedure TDoublyLinkedListClass<T>.AddToFront(const AValues: TEnumerable<T>);
var
  Value: T;
  Dst: PDoublyLinkedListItem;
begin
  Dst := nil;
  for Value in AValues do
    if Dst = nil then
    begin
      AddToFront(Value);
      Dst := Front;
    end
    else
    begin
      InsertAfter(Value, Dst);
      Dst := Dst.Next;
    end;
end;

{        v
          \
  p.Prev-> . <-p }
procedure TDoublyLinkedListClass<T>.InsertBefore(Value: T; Dst: PDoublyLinkedListItem);
var
  Item: PDoublyLinkedListItem;
begin
  Item := AllocItem(Value);
  if FFront = Dst then
    FFront := Item;
  if Dst.Prev <> nil then
    Dst.Prev.Next := Item;
  Item.Prev := Dst.Prev;
  Dst.Prev := Item;
  Item.Next := Dst;
  inc(FCount);
end;

procedure TDoublyLinkedListClass<T>.Unique;
var
  Values: TSet<T>;
  Item,D: PDoublyLinkedListItem;
begin
  Values.Init;
  Item := FFront;
  while Item <> nil do
    if Item.Data in Values then
    begin
      D := Item;
      Item := Item.Next;
      Delete(D);
    end
    else
    begin
      Values.Add(Item.Data);
      Item := Item.Next;
    end;
end;

class function TDoublyLinkedListClass<T>.MergeSortedRanges(C: PDoublyLinkedListItem; Comparer: IComparer<T>): PDoublyLinkedListItem;
var
  l,r,n: PDoublyLinkedListItem;
begin
  result := c;
  if c=nil then
    exit;
  if c.next=nil then
    exit;

  { split C->L+R }
  l := nil;
  r := nil;
  repeat
    n := c;
    c := c.Next;
    n.Next := l;
    l := n;
    if c=nil then
      break;
    n := c;
    c := c.Next;
    n.Next := r;
    r := n;
  until c=nil;

  { sort L&R }
  l := MergeSortedRanges(l, Comparer);
  r := MergeSortedRanges(r, Comparer);

  { merge L+R->Result }
  if l=nil then
    result := r
  else
  if r=nil then
    result := l
  else
  begin

    { result=Head, c=Tail }
    if Comparer.Compare(l.Data, r.Data) <= 0 then // L < R
    begin
      result := l;
      l := l.Next;
    end else begin
      result := r;
      r := r.Next;
    end;
    c := result;

    { L&R -> Tail (c) }
    while (l<>nil) and (r<>nil) do
      if Comparer.Compare(l.Data, r.Data) <= 0 then // L < R
      begin
        c.Next := l;
        c := l;
        l := l.Next;
      end else begin
        c.Next := r;
        c := r;
        r := r.Next;
      end;

    { put remain Items to back }
    if l=nil then
      c.Next := r
    else
      c.Next := l;
  end;
end;

class procedure TDoublyLinkedListClass<T>.MergeSort(List: TDoublyLinkedListClass<T>; AFirst, ALast: PDoublyLinkedListItem; Comparer: IComparer<T>);
var
  f,l,n: PDoublyLinkedListItem;
begin
  n := ALast.Next;
  ALast.Next := nil;
  f := MergeSortedRanges(AFirst, Comparer);
  l := f;
  while l.Next<>nil do
    l := l.Next;
  if ALast=List.FBack then
    List.FBack := l;
  l.Next := n;
  if AFirst=List.FFront then
    List.FFront := f
  else
    AFirst.Prev.Next := f;

  { restore backward links }
  f := nil;
  n := List.FFront;
  while n<>nil do
  begin
    n.Prev := f;
    f := n;
    n := n.Next;
  end;
end;

procedure TDoublyLinkedListClass<T>.Sort;
begin
  Sort(FComparer);
end;

procedure TDoublyLinkedListClass<T>.Sort(Comparer: IComparer<T>);
begin
  if FFront <> nil then
    if Comparer = nil then
      MergeSort(Self, FFront, FBack, FComparer)
    else
      MergeSort(Self, FFront, FBack, Comparer);
end;

{    v
      \
   p-> . <-p.next }
procedure TDoublyLinkedListClass<T>.InsertAfter(Value: T; Dst: PDoublyLinkedListItem);
var
  Item: PDoublyLinkedListItem;
begin
  Item := AllocItem(Value);
  if FBack = Dst then
    FBack := Item;
  if Dst.Next <> nil then
    Dst.Next.Prev := Item;
  Item.Next := Dst.Next;
  Dst.Next := Item;
  Item.Prev := Dst;
  inc(FCount);
end;

procedure TDoublyLinkedListClass<T>.Exchange(Item1, Item2: PDoublyLinkedListItem);
var
  p: PDoublyLinkedListItem;
begin
  if Item1 = Item2 then
    Exit;
  if Item1.Next = Item2 then
  begin
    { * - Item1 - Item2 - * }
    if Item1.Prev = nil
      then FFront := Item2
      else Item1.Prev.Next := Item2;
    if Item2.Next = nil
      then FBack := Item1
      else Item2.Next.Prev := Item1;
    p := Item1.Prev;
    Item1.Prev := Item2;
    Item1.Next := Item2.Next;
    Item2.Prev := p;
    Item2.Next := Item1;
  end
  else
  if Item2.Next = Item1 then
  begin
    { * - Item2 - Item1 - * }
    if Item2.Prev = nil
      then FFront := Item1
      else Item2.Prev.Next := Item1;
    if Item1.Next = nil
      then FBack := Item2
      else Item1.Next.Prev := Item2;
    p := Item2.Prev;
    Item2.Prev := Item1;
    Item2.Next := Item1.Next;
    Item1.Prev := p;
    Item1.Next := Item2;
  end
  else
  begin
    { * - Item1/2 - [...] - Item1/2 - * }
    p := Item1.Prev; Item1.Prev := Item2.Prev; Item2.Prev := p;
    p := Item1.Next; Item1.Next := Item2.Next; Item2.Next := p;
    if Item1.Prev = nil
      then FFront := Item1
      else Item1.Prev.Next := Item1;
    if Item1.Next = nil
      then FBack := Item1
      else Item1.Next.Prev := Item1;
    if Item2.Prev = nil
      then FFront := Item2
      else Item2.Prev.Next := Item2;
    if Item2.Next = nil
      then FBack := Item2
      else Item2.Next.Prev := Item2;
  end;
end;

function TDoublyLinkedListClass<T>.AllocItem: PDoublyLinkedListItem;
begin
  result := AllocMem(SizeOf(TDoublyLinkedListItem));
end;

function TDoublyLinkedListClass<T>.AllocItem(const Value: T): PDoublyLinkedListItem;
begin
  result := AllocMem(SizeOf(TDoublyLinkedListItem));
  result.Data := Value;
end;

procedure TDoublyLinkedListClass<T>.Assign(Src: TDoublyLinkedListClass<T>);
begin
  Assign(TEnumerable<T>(Src));
  FOwnsValues := Src.FOwnsValues;
  FComparer := Src.FComparer;
end;

procedure TDoublyLinkedListClass<T>.Assign(Src: TEnumerable<T>);
begin
  Clear;
  AddToBack(Src);
end;

procedure TDoublyLinkedListClass<T>.Assign(const Src: TArray<T>);
begin
  Clear;
  AddToBack(Src);
end;

function TDoublyLinkedListClass<T>.Find(Value: T; Comparer: IComparer<T>): PDoublyLinkedListItem;
begin
  result := FFront;
  while (result <> nil) and (Comparer.Compare(result.Data, Value) <> 0) do
    result := result.Next;
end;

function TDoublyLinkedListClass<T>.Find(Value: T): PDoublyLinkedListItem;
begin
  result := Find(Value, FComparer);
end;

function TDoublyLinkedListClass<T>.FindNext(Item: PDoublyLinkedListItem; Comparer: IComparer<T>): PDoublyLinkedListItem;
begin
  result := Item;
  if result <> nil then
    repeat
      result := result.Next;
    until (result = nil) or (Comparer.Compare(result.Data, Item.Data) = 0);
end;

function TDoublyLinkedListClass<T>.FindNext(Item: PDoublyLinkedListItem): PDoublyLinkedListItem;
begin
  result := FindNext(Item, FComparer);
end;

function TDoublyLinkedListClass<T>.FindByIndex(Index: integer): PDoublyLinkedListItem;
begin
  result := FFront;
  while (Index > 0) and (result <> nil) do
  begin
    Dec(Index);
    result := result.Next;
  end;
end;

function TDoublyLinkedListClass<T>.FindValueByIndex(Index: integer): T;
begin
  result := FindByIndex(Index).Data;
end;

procedure TDoublyLinkedListClass<T>.FreeItem(Item: PDoublyLinkedListItem);
begin
  if FOwnsValues then
    PObject(@Item.Data)^.DisposeOf;
  Item.Data := Default(T);
  FreeMem(Item);
end;

function TDoublyLinkedListClass<T>.GetEmpty: boolean;
begin
  result := Count=0;
end;

function TDoublyLinkedListClass<T>.GetIsConsistent: boolean;
var
  L: PDoublyLinkedListItem;
  I: integer;
begin
  result := False;
  if Count < 0 then
    Exit;
  if (FFront=nil) <> (FBack=nil) then
    Exit;
  if (FFront<>nil) then
    if (FFront.Prev<>nil) or (FBack.Next<>nil) then
      Exit;

  L := FFront;
  for I := 0 to Count-2 do
    if L = nil then
      Exit
    else
      L := L.Next;
  if L <> FBack then
    Exit;

  L := FBack;
  for I := 0 to Count-2 do
    if L = nil then
      Exit
    else
      L := L.Prev;
  if L <> FFront then
    Exit;

  result := True;
end;

procedure TDoublyLinkedListClass<T>.SetOwnsValues(AOwnsValues: boolean);
begin
  if AOwnsValues and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := AOwnsValues;
end;

function TDoublyLinkedListClass<T>.Extract(Item: PDoublyLinkedListItem): T;
begin
  result := Item.Data;
  Item.Data := Default(T);
  Delete(Item);
end;

function TDoublyLinkedListClass<T>.ExtractBack: T;
begin
  result := Extract(FBack);
end;

function TDoublyLinkedListClass<T>.ExtractFront: T;
begin
  result := Extract(FFront);
end;

function TDoublyLinkedListClass<T>.ExtractItem(Item: PDoublyLinkedListItem): PDoublyLinkedListItem;
begin
  result := Item;
  if Item.Prev <> nil then
    Item.Prev.Next := Item.Next;
  if Item.Next <> nil then
    Item.Next.Prev := Item.Prev;
  if Item = FFront then
    FFront := Item.Next;
  if Item = FBack then
    FBack := Item.Prev;
  dec(FCount);
  result.Prev := nil;
  result.Next := nil;
end;

procedure TDoublyLinkedListClass<T>.Delete(Item: PDoublyLinkedListItem);
begin
  FreeItem(ExtractItem(Item));
end;

procedure TDoublyLinkedListClass<T>.DeleteRange(ItemStart, ItemEnd: PDoublyLinkedListItem);
var
  d: PDoublyLinkedListItem;
begin
  if ItemStart = nil then
    ItemStart := FFront;
  while ItemStart <> ItemEnd do
  begin
    d := ItemStart;
    ItemStart := ItemStart.Next;
    FreeItem(ExtractItem(d));
  end;
  if ItemStart <> nil then
    FreeItem(ExtractItem(ItemEnd));
end;

procedure TDoublyLinkedListClass<T>.Clear;
var A,B: PDoublyLinkedListItem;
begin
  A := FFront;
  while A<>nil do
  begin
    B := A;
    A := A.Next;
    FreeItem(B);
  end;
  FFront := nil;
  FBack := nil;
  FCount := 0;
end;

function TDoublyLinkedListClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := TDListEnumerator.Create(FFront, FBack);
end;

function TDoublyLinkedListClass<T>.Remove(Value: T): integer;
begin
  Remove(Value, FComparer);
end;

function TDoublyLinkedListClass<T>.Remove(Value: T; Comparer: IComparer<T>): integer;
var
  C,D: PDoublyLinkedListItem;
begin
  C := FFront;
  while C <> nil do
    if Comparer.Compare(C.Data, Value) <> 0 then
      C := C.Next
    else
    begin
      D := C;
      C := C.Next;
      Delete(D);
    end;
end;

procedure TDoublyLinkedListClass<T>.Reverse(FirstItem, LastItem: PDoublyLinkedListItem; CheckDirection: boolean = True);
var
  q, Temp, NFirst, NPrev, NNext: PDoublyLinkedListItem;
begin
  if FirstItem = nil then
    FirstItem := FFront;
  if LastItem = nil then
    LastItem := FBack;
  if FirstItem = LastItem then
    exit;

  { check direction }
  if CheckDirection then
  begin
    q := FirstItem;
    while (q <> nil) and (q <> LastItem) do
      q := q.Next;
    if q = nil then
    begin
      {$If Defined(Debug)}
        q := LastItem;
        while (q <> nil) and (q <> FirstItem) do
          q := q.Next;
        Assert(q <> nil);
      {$EndIf}
      Reverse(LastItem, FirstItem, False);
      Exit;
    end;
  end;

  { NPrev -> A->B->...->C -> NNext }
  NPrev := FirstItem.Prev;
  NFirst:= FirstItem;
  NNext := LastItem.Next;
  q := nil;
  repeat
    Temp := FirstItem;
    if FirstItem=LastItem then
      FirstItem := nil
    else
      FirstItem := FirstItem.Next;
    Temp.Next := q;
    q := Temp;
  until FirstItem=nil;

  { NPrev -> C->...->B->A -> NNext }
  if NPrev=nil then
    FFront := q
  else
    NPrev.Next := q;

  { "NFirst" has moved to the end of reverse range }
  NFirst.Next := NNext;
  if NNext=nil then
    FBack := NFirst;

  { restore backward links }
  LastItem.Prev := NPrev;
  while LastItem.next<>nil do
  begin
    LastItem.Next.Prev := LastItem;
    if LastItem=NFirst then
      Break;
    LastItem := LastItem.Next;
  end;
end;

procedure TDoublyLinkedListClass<T>.Rotate(FirstItem, LastItem: PDoublyLinkedListItem; Shift: integer);
var
  p, NPrev, NFirst, NLast: PDoublyLinkedListItem;
  i,w: integer;
begin
  if FirstItem=LastItem then
    Exit;

  { find W and make Shift positive and less than W }
  w := 1;
  p := FirstItem;
  while (p<>LastItem) and (p<>nil) do
  begin
    p := p.Next;
    inc(w);
  end;
  if p=nil then
  begin
    Rotate(LastItem, FirstItem, Shift);
    exit;
  end;
  Shift := Shift mod w;
  if Shift=0 then
    exit;
  if Shift<0 then
    inc(Shift, w);

  { number of left shifts = w-Shift }
  p := FirstItem;
  for i := 0 to w-Shift-1 do
    p := p.Next;

  { prev(FirstItem) <-> P-LastItem <-> FirstItem-prev(p) <-> next(LastItem) }
  NPrev := p.prev;
  NFirst := FirstItem.prev;
  NLast := LastItem.next;

  if NFirst=nil then
    FFront := p
  else
    NFirst.Next := p;
  NPrev.Next := LastItem.Next;
  LastItem.Next := FirstItem;
  if FBack=LastItem then
    FBack := NPrev;

  p.Prev := NFirst;
  FirstItem.prev := LastItem;
  if NLast<>nil then
    NLast.Prev := NPrev;
end;

end.
