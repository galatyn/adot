unit adot.Grammar.Types;

interface

uses
  adot.Types,
  adot.Collections,
  adot.Tools,
  adot.Tools.Rtti,
  System.Generics.Collections,
  System.SysUtils;

type
  TGrammarClass = class;
  TPos = record
    Start,Len: integer;

    procedure SetPos(AStart,ALen: integer); {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  //PMatchingResult = ^TMatchingResult;
  TMatchingResult = record
    Rule: TGrammarClass;
    Position: TPos;
    FirstChild: integer;
    NextSibling: integer;

    procedure SetUp(ARule: TGrammarClass; AStart,ALen: integer); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetUp(ARule: TGrammarClass; AStart,ALen,AFirstChild: integer); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  TParserCache = class abstract
  public
    procedure Clear; virtual; abstract;
    function TryGetValue(const RuleId: int64; Position: integer; var P: TPos; var Accept: boolean): Boolean; virtual; abstract;
  end;

  TParserRes = record
    Position: TPos;
    Rule: TGrammarClass;
    SubExprStart: integer;
    SubExprCount: integer;
  end;

  { grammar based parser }
  TGrammarParser = class abstract
  public
    Data: TBuffer;
    Results: TVector<TParserRes>;
    Grammar: TGrammarClass;

    constructor Create(AMainRule: TGrammarClass); overload; virtual;
    constructor Create(AMainRule: IInterfacedObject<TGrammarClass>); overload;
    procedure Clear; virtual;
    function Accepted: Boolean; overload; virtual; abstract;
    function Accepted(const AData: TBuffer): Boolean; overload;
    function Accepted(const AData: string): Boolean; overload;
    procedure LogResult; virtual; abstract;
  end;

  TGrammarType = (gtUnknown, gtLink, gtString, gtChar, gtSequence, gtSelection, gtRepeat, gtNot, gtEOF);

  { basic class for grammar definition }
  TGrammarClass = class abstract(TEnumerable<IInterfacedObject<TGrammarClass>>)
  protected
    type
      TCustomGrammarEnumerator = class(TEnumerator<IInterfacedObject<TGrammarClass>>)
      protected
        FItems: TVector<IInterfacedObject<TGrammarClass>>;
        FPos: integer;

        function DoGetCurrent: IInterfacedObject<TGrammarClass>; override;
        function DoMoveNext: Boolean; override;

      public
        constructor Create(AGrammar: TGrammarClass);
      end;

    class var
      FIdCnt: int64;
    var
      FGrammarType: TGrammarType;
      FId: int64;

    { implements TEnumerate using GetOperands method }
    function DoGetEnumerator: TEnumerator<IInterfacedObject<TGrammarClass>>; override;
    function GetInfo: string; virtual;
    class function GetOperandInfo(Operand: TGrammarClass): string; overload; static;
    class function GetOperandInfo(const Operand: IInterfacedObject<TGrammarClass>): string; overload; static;

  public
    constructor Create(AGrammarType: TGrammarType);

    { release all internal links (IInterfacedObject<TGrammarClass> etc)}
    procedure Release; virtual; abstract;
    { add operands to the collections }
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); virtual; abstract;
    { called once only for main rule by parser (PEG for example) }
    procedure SetupMainRule; virtual;
    { called by SetupMainRule for every rule in the tree (one time when parser is to be initialized) }
    procedure SetupRule; virtual;

    property GrammarType: TGrammarType read FGrammarType;

    { global (process-wide) identifier of the instance }
    property Id: int64 read FId;

    { descriptive/readable text presentation }
    property Info: string read GetInfo;
  end;

implementation

{ TPos }

procedure TPos.SetPos(AStart, ALen: integer);
begin
  Start := AStart;
  Len := ALen;
end;

{ TMatchingResult }

procedure TMatchingResult.SetUp(ARule: TGrammarClass; AStart,ALen: integer);
begin
  Rule := ARule;
  Position.SetPos(AStart, ALen);
  FirstChild := -1;
  NextSibling := -1;
end;

procedure TMatchingResult.SetUp(ARule: TGrammarClass; AStart, ALen, AFirstChild: integer);
begin
  Rule := ARule;
  Position.SetPos(AStart, ALen);
  FirstChild := AFirstChild;
  NextSibling := -1;
end;

{ TGrammarClass.TCustomGrammarEnumerator }

constructor TGrammarClass.TCustomGrammarEnumerator.Create(AGrammar: TGrammarClass);
begin
  FItems.Clear;
  AGrammar.GetOperands(FItems);
  FPos := 0;
end;

function TGrammarClass.TCustomGrammarEnumerator.DoGetCurrent: IInterfacedObject<TGrammarClass>;
begin
  result := FItems[FPos-1];
end;

function TGrammarClass.TCustomGrammarEnumerator.DoMoveNext: Boolean;
begin
  result := FPos < FItems.Count;
  if result then
    inc(FPos);
end;

{ TGrammarClass }

constructor TGrammarClass.Create(AGrammarType: TGrammarType);
begin
  inc(FIdCnt);
  FId := FIdCnt;
  FGrammarType := AGrammarType;
end;

function TGrammarClass.DoGetEnumerator: TEnumerator<IInterfacedObject<TGrammarClass>>;
begin
  result := TCustomGrammarEnumerator.Create(Self);
end;

function TGrammarClass.GetInfo: string;
begin
  result := Format('#%d %s (%s)', [Id, TEnumeration<TGrammarType>.ToString(FGrammarType), ClassName]);
end;

class function TGrammarClass.GetOperandInfo(const Operand: IInterfacedObject<TGrammarClass>): string;
begin
  if Operand=nil then
    result := 'Op:nil'
  else
    result := GetOperandInfo(Operand.Data);
end;

class function TGrammarClass.GetOperandInfo(Operand: TGrammarClass): string;
var
  Operands: TVector<IInterfacedObject<TGrammarClass>>;
begin
  if Operand=nil then
    result := 'Op:nil'
  else
    if Operand.FGrammarType=gtLink then
    begin
      Operands.Clear;
      Operand.GetOperands(Operands);
      Assert(Operands.Count=1);
      result := Format('Op:%s(#%d)={#%d: %s}', [
        TEnumeration<TGrammarType>.ToString(Operand.GrammarType), Operand.Id,
        Operands[0].Data.Id, TEnumeration<TGrammarType>.ToString(Operands[0].Data.GrammarType)
      ])
    end
    else
      result := Format('Op:%s (#%d)', [TEnumeration<TGrammarType>.ToString(Operand.GrammarType), Operand.Id]);
end;

procedure TGrammarClass.SetupMainRule;
var
  Queue: TVector<TGrammarClass>;
  QueuedIds: TSet<int64>;
  Operands: TVector<IInterfacedObject<TGrammarClass>>;
  Item: TGrammarClass;
  I: integer;
begin
  inherited;
  Queue.Clear;
  Queue.Add(Self);
  QueuedIds.Clear;
  QueuedIds.Add(Id);
  repeat

    { process next rule }
    Item := Queue.ExtractLast;
    Item.SetupRule;

    { process operands of the rule }
    Operands.Clear;
    Item.GetOperands(Operands);
    for I := 0 to Operands.Count-1 do
    begin
      Assert(Operands[I]<>nil, 'Operand is not initialized');
      Item := Operands[I].Data;
      if Item.Id in QueuedIds then
        Continue;
      QueuedIds.Add(Item.Id);
      Queue.Add(Item);
    end;
  until Queue.Count=0;
end;

procedure TGrammarClass.SetupRule;
begin
end;

{ TGrammarParser }

constructor TGrammarParser.Create(AMainRule: TGrammarClass);
begin
  Grammar := AMainRule;
  Grammar.SetupMainRule;
end;

constructor TGrammarParser.Create(AMainRule: IInterfacedObject<TGrammarClass>);
begin
  Create(AMainRule.Data);
end;

function TGrammarParser.Accepted(const AData: TBuffer): Boolean;
begin
  Data := AData;
  Data.Position := 0;
  Results.Clear;
  Result := Accepted;
end;

function TGrammarParser.Accepted(const AData: string): Boolean;
begin
  Data.Text := AData;
  Result := Accepted;
end;

procedure TGrammarParser.Clear;
begin
  Data.Clear;
  Results.Clear;
end;

end.
