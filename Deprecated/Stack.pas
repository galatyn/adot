  // Unlike TObjectStack, TObjectStackExt provides Items property for direct index-based access to all items
  // (TObjectStack hides internal array behind private section, so we can't extend functionality in normal OO-way).
  TObjectStackExt<TValue: class> = class
  protected
    FItems: TObjectList<TValue>;

    function GetCount: integer; inline;
    procedure SetCount(const Value: integer); inline;
    function GetItem(index: integer): TValue; inline;
    function GetPeek: TValue; inline;
    procedure SetItem(index: integer; const Value: TValue); inline;
    function GetOwnsObjects: boolean; inline;
    procedure SetOwnsObjects(const Value: boolean); inline;

  public
    type
      TEnumerator = TObjectList<TValue>.TEnumerator;

    constructor Create(AOwnsObjects: Boolean = True);
    destructor Destroy; override;

    function Push(AValue: TValue):TValue; inline;
    function Pop: TValue; inline;
    procedure Clear; inline;
    procedure TrimExcess; inline;
    function ToArray: TArray<TValue>;
    function GetEnumerator: TEnumerator;

    property Peek: TValue read GetPeek;
    property Extract: TValue read Pop;
    property Count: integer read GetCount write SetCount;
    property Items[index: integer]:TValue read GetItem write SetItem; default;
    property OwnsObjects: boolean read GetOwnsObjects write SetOwnsObjects;
  end;

{ TObjectStackExt<TValue> }

procedure TObjectStackExt<TValue>.Clear;
begin
  FItems.Clear;
end;

constructor TObjectStackExt<TValue>.Create(AOwnsObjects: Boolean = True);
begin
  FItems := TObjectList<TValue>.Create(AOwnsObjects);
end;

destructor TObjectStackExt<TValue>.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TObjectStackExt<TValue>.GetCount: integer;
begin
  result := FItems.Count;
end;

procedure TObjectStackExt<TValue>.SetCount(const Value: integer);
begin
  FItems.Count := Value;
end;

function TObjectStackExt<TValue>.GetEnumerator: TEnumerator;
begin
  result := FItems.GetEnumerator;
end;

function TObjectStackExt<TValue>.GetItem(index: integer): TValue;
begin
  result := FItems[index];
end;

function TObjectStackExt<TValue>.GetPeek: TValue;
begin
  result := FItems.Last;
end;

function TObjectStackExt<TValue>.Pop: TValue;
begin
  result := FItems.Last;
  FItems.Delete(FItems.Count-1);
end;

function TObjectStackExt<TValue>.Push(AValue: TValue):TValue;
begin
  FItems.Add(AValue);
  result := AValue;
end;

procedure TObjectStackExt<TValue>.SetItem(index: integer; const Value: TValue);
begin
  FItems[index] := Value;
end;

function TObjectStackExt<TValue>.GetOwnsObjects: boolean;
begin
  result := FItems.OwnsObjects;
end;

procedure TObjectStackExt<TValue>.SetOwnsObjects(const Value: boolean);
begin
  FItems.OwnsObjects := Value;
end;

function TObjectStackExt<TValue>.ToArray: TArray<TValue>;
begin
  result := FItems.ToArray;
end;

procedure TObjectStackExt<TValue>.TrimExcess;
begin
  FItems.TrimExcess;
end;

