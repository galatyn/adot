unit Multimap;

interface

uses
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils;

type
  TMultimap<TKey,TValue> = class(TDictionary<TKey,TValue>)
  protected
    type
      TMultimapKey = record
        Key: TKey;
        Number: integer;
      end;
    var
      FCount: TDictionary<TKey, integer>;
      FValues: TDictionary<TMultimapKey, TValue>;

    function GetCount: integer;

  public
    type
      TSearch = record
      private
        Number: integer;
      end;

    constructor Create;
    destructor Destroy; override;

    procedure Add(const AKey: TKey; const AValue: TValue);
    function ContainsKey(const AKey: TKey): Boolean;
    function Remove(const AKey: TKey):Boolean;
    function GetFirstValue(const AKey: TKey; var AValue: TValue; var AIterator: TSearch): Boolean;
    function GetNextValue(const AKey: TKey; var AValue: TValue; var AIterator: TSearch): Boolean;
    function GetValuesCount(const AKey: TKey):Integer;

    property Count: integer read GetCount;
  end;

implementation

{ TMultimap<TKey, TValue> }

constructor TMultimap<TKey, TValue>.Create;
begin
  FCount := TDictionary<TKey, integer>.Create;
  FValues := TDictionary<TMultimapKey, TValue>.Create;
end;

destructor TMultimap<TKey, TValue>.Destroy;
begin
  FreeAndNil(FCount);
  FreeAndNil(FValues);
  inherited;
end;

function TMultimap<TKey, TValue>.GetCount: integer;
begin
  result := FValues.Count;
end;

function TMultimap<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  result := GetValuesCount(AKey)>0;
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
  FCount.AddOrSetValue(AKey, MKey.Number+1);
  FValues.Add(MKey, AValue);
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

function TMultimap<TKey, TValue>.GetFirstValue(const AKey: TKey; var AValue: TValue; var AIterator: TSearch): Boolean;
begin
  result := FCount.TryGetValue(AKey, AIterator.Number) and GetNextValue(AKey, AValue, AIterator);
end;

function TMultimap<TKey, TValue>.GetNextValue(const AKey: TKey; var AValue: TValue; var AIterator: TSearch): Boolean;
var
  MKey: TMultimapKey;
begin
  result := AIterator.Number>0;
  if not result then
    Exit;
  dec(AIterator.Number);
  MKey.Key := AKey;
  MKey.Number := AIterator.Number;
  if not FValues.TryGetValue(MKey, AValue) then
    raise Exception.Create('Error');
end;

end.
