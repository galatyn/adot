unit adot.Grammar.Types;

interface

uses
  adot.Types,
  adot.Collections,
  adot.Collections.Vectors,
  adot.Collections.Sets,
  adot.Collections.Trees,
  adot.Tools,
  adot.Tools.IO,
  adot.Tools.Rtti,
  adot.Strings,
  {$IF Defined(LogExceptions)}
    {$Define LogGrammar}
    adot.Log,
  {$ENDIF}
  System.Generics.Collections,
  System.Character,
  System.SysUtils,
  System.StrUtils;

const
  infinitely = High(integer);

type
  TGrammarClass = class;
  TRuleId = int64;

  TParseTreeItem = record
    Rule: TGrammarClass;
    Position: TTokenPos;

    procedure Init(ARule: TGrammarClass; AStart,ALen: integer); overload;
    procedure Init(const ASrc: TParseTreeItem); overload;
  end;

  TEnumTreeProc = reference to procedure(TreeNode: integer; var Cancel: boolean);
  TFilterTreeProc = reference to procedure(Tree: TTreeArrayClass<TParseTreeItem>; Node: integer; var Accept: boolean);

  TParseTree = class
  public
    type
      { Enumerates subtree starting from specified node }
      TSubtreeEnumerator = TTreeArrayClass<TParseTreeItem>.TSubtreeEnumerator;
      TSubtreeCollection = TTreeArrayClass<TParseTreeItem>.TSubtreeCollection;

      { Snumerates ParseTree starting from specified node, returns only matches of specified rule }
      TRuleMatchesEnumerator = record
      private
        Enum: TSubtreeEnumerator;
        Nodes: TTreeArrayClass<TParseTreeItem>;
        RuleId: TruleId;

        function CurrentNode: integer;

      public
        procedure Init(ANodes: TTreeArrayClass<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);

        function MoveNext: Boolean;
        property Current: integer read CurrentNode;
      end;

      { collection for TRuleMatchesEnumerator }
      TRuleMatchesCollection = record
        Tree: TTreeArrayClass<TParseTreeItem>;
        Root: integer;
        RuleId: TruleId;

        procedure Init(ATree: TTreeArrayClass<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);
        function GetEnumerator: TRuleMatchesEnumerator;
      end;

  private
    function GetItem(n: integer): TParseTreeItem;
    procedure SetItem(n: integer; const Item: TParseTreeItem);
    function GetCount: integer;
    function GetChilds(ARoot: integer): TSubtreeCollection;
    function GetRuleMatches(RootNode: integer; const RuleId: TRuleId): TRuleMatchesCollection;
    function GetFirstChild(Node: integer): integer;
    function GetNextSibling(Node: integer): integer;
    function GetTotalSizeBytes: int64;
  public
    Tree: TTreeArrayClass<TParseTreeItem>;
    Root: integer;

    constructor Create;
    destructor Destroy; override;

    function Add(ARule: TGrammarClass; AStart, ALen: integer): integer;
    function Append(ARule: TGrammarClass; AStart, ALen: integer): integer; overload;
    function Append: integer; overload;
    function Commit(ARule: TGrammarClass; AStart, ALen: integer): integer; overload;
    function Commit: integer; overload;
    procedure Rollback;

    { clear Tree and set Root=-1 }
    procedure Clear;

    { support for "for" syntax: for I in Tree do ... }
    function GetEnumerator: TSubtreeEnumerator;

    { build tree from subset of nodes according to the filter (original tree where filtered nodes are ommited) }
    function GetSubTree(StartNode: integer; FilterProc: TFilterTreeProc): TParseTree;

    procedure LogTextInputParseTree(var InputData: TBuffer);

    { shortcuts to Tree.Items / Tree.Count }
    property TreeItems[RootNode: integer]:TParseTreeItem read GetItem write SetItem; default;
    property Count: integer read GetCount;

    { enumerable collection of child nodes (including specified Root) }
    property Matches[Root: integer]: TSubtreeCollection read GetChilds;

    { enumerable collection of child nodes (including specified Root), only matched of specified rule returned }
    property RuleMatches[RootNode: integer; const RuleId: TRuleId]:TRuleMatchesCollection read GetRuleMatches;

    property FirstChild[Node: integer]: integer read GetFirstChild;
    property NextSibling[Node: integer]: integer read GetNextSibling;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
  end;

  { grammar based parser }
  TGrammarParser = class abstract
  protected
    function GetDataToken(const P: TTokenPos): string;
    function GetTreeToken(ParseTreeNode: integer): string;
    function GetTotalSizeBytes: int64;
  public
    Data: TBuffer;          { input data (text or bytes) }
    Grammar: TGrammarClass; { main rule of the grammar (class doesn't own that grammar) }
    ParseTree: TParseTree;  { parse tree (available if .Accepted returns True) }

    constructor Create(AMainRule: TGrammarClass); overload; virtual;
    constructor Create(AMainRule: IInterfacedObject<TGrammarClass>); overload;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Accepted: Boolean; overload; virtual; abstract;
    function Accepts(const AData: TBuffer): Boolean; overload;
    function Accepts(const AData: string): Boolean; overload;
    procedure LogParseTree; virtual; abstract;

    { for any TTokenPos (position and length in bytes) returns corresponding string from Data }
    property DataToken[const P: TTokenPos]: string read GetDataToken;
    { returns string corresponding to element of ParseTree (position and length in bytes) }
    property TreeToken[ParseTreeNode: integer]: string read GetTreeToken;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
  end;

  TGrammarType = (
    gtUnknown,   { invalid/not initialized properly }
    gtLink,      { link to another rule }
    gtString,    { Delphi-compatible string (2 bytes per char) + CaseSensitive flag }
    gtChar,      { range of chars + CaseSensitive flag }
    gtCharClass, { char class (letter, digit, whitespace etc) }
    gtCharSet,   { set of chars + CaseSensitive flag }
    gtBytes,     { any sequence/array of bytes }
    gtSequence,  { sequence of rules (accepted if all rules in the sequence are accepted in defined order) }
    gtSelection, { selection from several rules (accepted when first rule in the list is accepted) }
    gtRepeat,    { greedy repeater (accepted if operand rule is accepted N times and N is in the range) }
    gtNot,       { negation, doesn't consume data from the input buffer (accepted if operand rule is not accepted) }
    gtEOF);      { accepted if all data is read from the input (input buffer is empty already) }

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

      TAllRulesEnumeratorPreProcessing = class(TEnumerator<TGrammarClass>)
      protected
        Queue: TVector<IInterfacedObject<TGrammarClass>>;
        IdSet: TSet<TRuleId>;
        CurrentItemInt: IInterfacedObject<TGrammarClass>;
        CurrentItem: TGrammarClass;
        Operands: TVector<IInterfacedObject<TGrammarClass>>;
        StartGrammar: TGrammarClass;

        function DoGetCurrent: TGrammarClass; override;
        function DoMoveNext: Boolean; override;

      public
        constructor Create(AStartGrammar: TGrammarClass);
      end;

      TAllRulesEnumeratorPostProcessing = class(TEnumerator<TGrammarClass>)
      protected
        Queue: TVector<IInterfacedObject<TGrammarClass>>;
        IdSet: TSet<TRuleId>;
        CurrentItemInt: IInterfacedObject<TGrammarClass>;
        CurrentItem: TGrammarClass;
        Operands: TVector<IInterfacedObject<TGrammarClass>>;
        StartGrammar: TGrammarClass;

        function DoGetCurrent: TGrammarClass; override;
        function DoMoveNext: Boolean; override;

      public
        constructor Create(AStartGrammar: TGrammarClass);
      end;

      TAllRulesCollection = class(TEnumerable<TGrammarClass>)
      protected
        StartGrammar: TGrammarClass;
        PreProcess: Boolean;

        function DoGetEnumerator: TEnumerator<TGrammarClass>; override;
      public
        constructor Create(AStartGrammar: TGrammarClass; APreProcess: Boolean);
      end;

    class var
      FIdCnt: TRuleId;
      FActiveInstCount: int64;
    var
      FGrammarType: TGrammarType;
      FId: TRuleId;
      FIncludeIntoParseTree: Boolean;
      FName: string;

    { Enumerates all operands (child rules without recursion). Implements TEnumerate using GetOperands }
    function DoGetEnumerator: TEnumerator<IInterfacedObject<TGrammarClass>>; override;
    function GetInfo: string; virtual;
    class function GetOperandInfo(Operand: TGrammarClass): string; overload; static;
    class function GetOperandInfo(const Operand: IInterfacedObject<TGrammarClass>): string; overload; static;
    procedure SetIncludeIntoParseTree(const Value: Boolean); virtual;
    procedure DoRelease; virtual; abstract;
    function GetAllRulesCollection(PreProcess: Boolean): TAllRulesCollection;

  public
    constructor Create(AGrammarType: TGrammarType);
    destructor Destroy; override;

    { release internal links (IInterfacedObject<TGrammarClass> etc) to avoid circular references etc }
    procedure Release;

    { add operands to the collections }
    procedure GetOperands(var Dst: TVector<IInterfacedObject<TGrammarClass>>); virtual; abstract;

    { called once for main rule by parser (TPegParser for example) }
    procedure SetupMainRule; virtual;

    { called by SetupMainRule for every rule in the tree (one time when parser is initializing) }
    procedure SetupRule; virtual;

    property GrammarType: TGrammarType read FGrammarType;

    { global (process-wide) identifier of the instance }
    property Id: TRuleId read FId;

    { descriptive/readable text presentation (for logging etc) }
    property Info: string read GetInfo;

    { Default = True for TGrammar rules and False for other (intermediate) rules. }
    property IncludeIntoParseTree: Boolean read FIncludeIntoParseTree write SetIncludeIntoParseTree;

    { user friendly name (can be set by user) }
    property Name: string read FName write FName;

    { Non-recursive enumerator for all subrules including rule where method is called.
      PreProcess=True  : rule is returned first, after that subrules are enlisted (when we initialize subrules for example)
      PreProcess=False : subrules are enlisted first, after that rule is returned (when we finalize subrules for example) }
    property AllRules[PreProcess: Boolean]: TAllRulesCollection read GetAllRulesCollection;

    class property ActiveInstCount: int64 read FActiveInstCount;
  end;

implementation

{ TParseTreeItem }

procedure TParseTreeItem.Init(ARule: TGrammarClass; AStart,ALen: integer);
begin
  {$IF SizeOf(TParseTreeItem)<>SizeOf(Rule)+SizeOf(Position)}
    Self := Default(TParseTreeItem);
  {$ENDIF}
  Rule := ARule;
  Position.SetPos(AStart, ALen);
end;

procedure TParseTreeItem.Init(const ASrc: TParseTreeItem);
begin
  {$IF SizeOf(TParseTreeItem)<>SizeOf(Rule)+SizeOf(Position)}
    Self := Default(TParseTreeItem);
  {$ENDIF}
  Rule     := ASrc.Rule;
  Position := ASrc.Position;
end;

{ TParseTree.TRuleMatchesCollection }

procedure TParseTree.TRuleMatchesCollection.Init(ATree: TTreeArrayClass<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);
begin
  {$IF SizeOf(TRuleMatchesCollection)<>SizeOf(Tree)+SizeOf(Root)+SizeOf(RuleId)}
    Self := Default(TRuleMatchesCollection);
  {$ENDIF}
  Tree := ATree;
  Root := ARoot;
  RuleId := ARuleId;
end;

function TParseTree.TRuleMatchesCollection.GetEnumerator: TRuleMatchesEnumerator;
begin
  result.Init(Tree, Root, RuleId);
end;

{ TParseTree.TRuleMatchesEnumerator }

procedure TParseTree.TRuleMatchesEnumerator.Init(ANodes: TTreeArrayClass<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);
begin
  {$IF SizeOf(TRuleMatchesEnumerator)<>SizeOf(Enum)+SizeOf(Nodes)+SizeOf(RuleId)}
    Self := Default(TRuleMatchesEnumerator);
  {$ENDIF}
  Enum.Init(ANodes.Nodes, ARoot);
  Nodes := ANodes;
  RuleId := ARuleId;
end;

function TParseTree.TRuleMatchesEnumerator.MoveNext: Boolean;
begin
  repeat
    result := Enum.MoveNext;
  until not result or (Nodes[Enum.Current].Rule.Id=RuleId);
end;

function TParseTree.TRuleMatchesEnumerator.CurrentNode: integer;
begin
  result := Enum.Current;
end;

{ TParseTree }

function TParseTree.Add(ARule: TGrammarClass; AStart, ALen: integer): integer;
var
  Node: TParseTreeItem;
begin
  Node.Rule := ARule;
  Node.Position.Start := AStart;
  Node.Position.Len := ALen;
  result := Tree.Add(Node);
end;

function TParseTree.Append(ARule: TGrammarClass; AStart, ALen: integer): integer;
var
  Node: TParseTreeItem;
begin
  Node.Rule := ARule;
  Node.Position.Start := AStart;
  Node.Position.Len := ALen;
  result := Tree.Append(Node);
end;

function TParseTree.Append: integer;
begin
  result := Tree.Append;
end;

function TParseTree.Commit(ARule: TGrammarClass; AStart, ALen: integer): integer;
begin
  result := Tree.Commit;
  Tree.Nodes.Items[result].Data.Init(ARule, AStart, ALen);
end;

function TParseTree.Commit: integer;
begin
  result := Tree.Commit(False);
end;

procedure TParseTree.Rollback;
begin
  Tree.Rollback;
end;

procedure TParseTree.Clear;
begin
  Tree.Clear;
  Root := -1;
end;

constructor TParseTree.Create;
begin
  inherited Create;
  Tree := TTreeArrayClass<TParseTreeItem>.Create;
end;

destructor TParseTree.Destroy;
begin
  FreeAndNil(Tree);
  inherited;
end;

function TParseTree.GetChilds(ARoot: integer): TSubtreeCollection;
begin
  result.Init(Tree.Nodes, ARoot);
end;

function TParseTree.GetRuleMatches(RootNode: integer; const RuleId: TRuleId): TRuleMatchesCollection;
begin
  result.Init(Tree, RootNode, RuleId);
end;

function TParseTree.GetCount: integer;
begin
  result := Tree.Count;
end;

function TParseTree.GetEnumerator: TSubtreeEnumerator;
begin
  result.Init(Tree.Nodes, Root);
end;

function TParseTree.GetItem(n: integer): TParseTreeItem;
begin
  result := Tree.Nodes[n].Data;
end;

function TParseTree.GetFirstChild(Node: integer): integer;
begin
  result := Tree.Nodes[Node].FirstChild;
end;

function TParseTree.GetNextSibling(Node: integer): integer;
begin
  result := Tree.Nodes[Node].NextSibling;
end;

function TParseTree.GetSubTree(StartNode: integer; FilterProc: TFilterTreeProc): TParseTree;
var
  Stack: TVector<TCompound<integer,integer>>;  { Src, DstParent }
  DstLastChild: TVector<integer>;              { indices in Dst }
  I: TCompound<integer,integer>;
  J: integer;
  Accept: Boolean;
begin
  Result := TParseTree.Create;
  Stack.Clear;
  DstLastChild.Clear;
  if Root >= 0 then
    Stack.Add(TCompound<integer,integer>.Create(Root, -1));
  while Stack.Count>0 do
  begin
    I := Stack.ExtractLast;
    Accept := True;
    FilterProc(Tree, I.A, Accept);
    if Accept then
    begin
      J := Result.Tree.Add;
      Result.Tree.Nodes[J].Data.Init(Tree.Nodes[I.A].Data);
      DstLastChild.Add(-1);
      if I.B >= 0 then
      begin
        if Result.Tree.Nodes[I.B].FirstChild < 0 then
          Result.Tree.Nodes.Items[I.B].FirstChild := J
        else
          Result.Tree.Nodes.Items[DstLastChild[I.B]].NextSibling := J;
        DstLastChild[I.B] := J;
      end;
      I.B := J;
    end;
    J := Stack.Count;
    I.A := Tree.Nodes[I.A].FirstChild;
    while I.A >= 0 do
    begin
      Stack.Add(I);
      I.A := Tree.Nodes[I.A].NextSibling;
    end;
    if Stack.Count > J then
      TArrayUtils.Inverse<TCompound<integer,integer>>(Stack.Items, J, Stack.Count-J);
  end;
  Result.Tree.Nodes.TrimExcess;
end;

function TParseTree.GetTotalSizeBytes: int64;
begin
  result := Tree.TotalSizeBytes + SizeOf(Root);
end;

procedure TParseTree.LogTextInputParseTree(var InputData: TBuffer);

  procedure L(const S: string; const Args: array of const; Margin: integer);
  begin
    {$IF Defined(LogGrammar)}
      AppLog.Log(StringOfChar(' ', Margin) + Format(S, Args));
    {$ENDIF}
  end;

  function ShowWS(const S: string): String;
  const
    CWhiteSpace = #$2591  { shade char to show trailing spaces };
    { arrows are not available for most of mono fonts, we use russian char instead }  //#$2B10; { arrow char to show empty string position }
    CEmptyStrPos = 'Ã';
  var
    I: Integer;
  begin
    result := S;

    { we replace trailing whitespaces to make them visible }
    for I := Low(Result) to High(Result) do
      if Result[I].IsWhiteSpace then Result[I] := CWhiteSpace else Break;
    for I := High(Result) downto Low(Result) do
      if Result[I].IsWhiteSpace then Result[I] := CWhiteSpace else Break;

    { we replace empty string to indicate position }
    if Result='' then
      Result := CEmptyStrPos;
  end;

  procedure LogTree(ResIndex, Margin: integer);
  var
    R: TParseTreeItem;
    S,T: string;
  begin
    while ResIndex>=0 do
    begin
      R := Tree.Nodes.Items[ResIndex].Data;

      Assert(R.Position.Len mod SizeOf(Char)=0);
      L('%s Pos: %d Len: %d)', [R.Rule.Info, R.Position.Start, R.Position.Len], Margin);
      InputData.Position := R.Position.Start;
      InputData.Read(S, R.Position.Len div SizeOf(Char));
      T := InputData.Text;
      if T=S then
        L('       %s', [TEnc.GetPrintable(S)], Margin)
      else
      begin
        L('       %s', [StringOfChar(' ', R.Position.Start div 2) + ShowWS(TEnc.GetPrintable(S)) ], Margin);
        L('       %s', [ShowWS(TEnc.GetPrintable(T))], Margin);
      end;

      LogTree(Tree.Nodes.Items[ResIndex].FirstChild, Margin + 2);
      ResIndex := Tree.Nodes.Items[ResIndex].NextSibling;
    end;
  end;

begin
  {$IF Defined(LogEnabled)}
    LogTree(0, 0);
  {$ENDIF}
end;

procedure TParseTree.SetItem(n: integer; const Item: TParseTreeItem);
begin
  Tree.Nodes.Items[N].Data := Item;
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

{ TGrammarClass.TAllRulesEnumeratorPreProcessing }

constructor TGrammarClass.TAllRulesEnumeratorPreProcessing.Create(AStartGrammar: TGrammarClass);
begin
  inherited Create;
  StartGrammar := AStartGrammar;
end;

function TGrammarClass.TAllRulesEnumeratorPreProcessing.DoGetCurrent: TGrammarClass;
begin
  result := CurrentItem;
end;

function TGrammarClass.TAllRulesEnumeratorPreProcessing.DoMoveNext: Boolean;
var
  I: Integer;
  Item: IInterfacedObject<TGrammarClass>;
begin
  if StartGrammar<>nil then
  begin
    CurrentItemInt := nil;
    CurrentItem    := StartGrammar;
    StartGrammar   := nil;
    Queue.Clear;
    IdSet.Clear;
    IdSet.Add(CurrentItem.Id);
    Exit(True);
  end;

  if CurrentItem=nil then
    Exit(False);

  { We return current item (rule) first and only at next iteration we ask for operands.
    It allows caller to initialize/preprocess operands. }
  Operands.Clear;
  CurrentItem.GetOperands(Operands);
  for I := 0 to Operands.Count-1 do
  begin
    Item := Operands[I];
    Assert(Item<>nil, 'Operand is not initialized');
    if not (Item.Data.Id in IdSet) then
    begin
      IdSet.Add(Item.Data.Id);
      Queue.Add(Item);
    end;
  end;

  result := not Queue.Empty;
  if result then
  begin
    CurrentItemInt := Queue.ExtractLast;
    CurrentItem    := CurrentItemInt.Data;
  end
  else
  begin
    CurrentItemInt := nil;
    CurrentItem    := nil;
  end;
end;

{ TGrammarClass.TAllRulesEnumeratorPostProcessing }

constructor TGrammarClass.TAllRulesEnumeratorPostProcessing.Create(AStartGrammar: TGrammarClass);
begin
  inherited Create;
  StartGrammar := AStartGrammar;
end;

function TGrammarClass.TAllRulesEnumeratorPostProcessing.DoGetCurrent: TGrammarClass;
begin
  result := CurrentItem;
end;

function TGrammarClass.TAllRulesEnumeratorPostProcessing.DoMoveNext: Boolean;
var
  I: Integer;
  Item: IInterfacedObject<TGrammarClass>;
begin
  if StartGrammar<>nil then
  begin
    CurrentItemInt := nil;
    CurrentItem    := StartGrammar;
    StartGrammar   := nil;
    Queue.Clear;
    IdSet.Clear;
    IdSet.Add(CurrentItem.Id);
  end
  else
  begin
    if Queue.Empty then
      Exit(False);
    CurrentItemInt := Queue.ExtractLast;
    CurrentItem    := CurrentItemInt.Data;
  end;

  { We ask for operands and only after that return the item (rule).
    It allows caller to process childs even if item is finilazied. }
  Operands.Clear;
  CurrentItem.GetOperands(Operands);
  for I := 0 to Operands.Count-1 do
  begin
    Item := Operands[I];
    Assert(Item<>nil, 'Operand is not initialized');
    if not (Item.Data.Id in IdSet) then
    begin
      IdSet.Add(Item.Data.Id);
      Queue.Add(Item);
    end;
  end;

  result := true;
end;

{ TGrammarClass.TAllRulesCollection }

constructor TGrammarClass.TAllRulesCollection.Create(AStartGrammar: TGrammarClass; APreProcess: Boolean);
begin
  inherited Create;
  StartGrammar := AStartGrammar;
  PreProcess := APreProcess;
end;

function TGrammarClass.TAllRulesCollection.DoGetEnumerator: TEnumerator<TGrammarClass>;
begin
  if PreProcess then
    result := TAllRulesEnumeratorPreProcessing.Create(StartGrammar)
  else
    result := TAllRulesEnumeratorPostProcessing.Create(StartGrammar);
end;

{ TGrammarClass }

constructor TGrammarClass.Create(AGrammarType: TGrammarType);
begin
  {$IFDEF Debug}
  Inc(FActiveInstCount);
  {$ENDIF}
  inc(FIdCnt);
  FId := FIdCnt;
  FGrammarType := AGrammarType;
  FIncludeIntoParseTree := False;
end;

destructor TGrammarClass.Destroy;
begin
  {$IFDEF Debug}
  Dec(FActiveInstCount);
  {$ENDIF}
  inherited;
end;

function TGrammarClass.DoGetEnumerator: TEnumerator<IInterfacedObject<TGrammarClass>>;
begin
  result := TCustomGrammarEnumerator.Create(Self);
end;

function TGrammarClass.GetAllRulesCollection(PreProcess: Boolean): TAllRulesCollection;
begin
  result := TAllRulesCollection.Create(Self, PreProcess);
end;

function TGrammarClass.GetInfo: string;
begin
  result := Format('%s#%d     %s (%s)', [IfThen(Name='','',Format('{%s} ',[Name])), Id, TEnumeration<TGrammarType>.ToString(FGrammarType), ClassName]);
end;

class function TGrammarClass.GetOperandInfo(const Operand: IInterfacedObject<TGrammarClass>): string;
begin
  if Operand=nil then
    result := 'Op:nil'
  else
    result := GetOperandInfo(Operand.Data);
end;

procedure TGrammarClass.Release;
var
  Item: TGrammarClass;
begin
  for Item in AllRules[False] do
    Item.DoRelease;
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

procedure TGrammarClass.SetIncludeIntoParseTree(const Value: Boolean);
begin
  FIncludeIntoParseTree := Value;
end;

procedure TGrammarClass.SetupMainRule;
var
  Item: TGrammarClass;
begin
  inherited;
  for Item in AllRules[True] do
    Item.SetupRule;
end;

procedure TGrammarClass.SetupRule;
begin
end;

{ TGrammarParser }

constructor TGrammarParser.Create(AMainRule: TGrammarClass);
begin
  inherited Create;
  Grammar := AMainRule;
  Grammar.SetupMainRule;
  ParseTree := TParseTree.Create;
end;

constructor TGrammarParser.Create(AMainRule: IInterfacedObject<TGrammarClass>);
begin
  Create(AMainRule.Data);
end;

destructor TGrammarParser.Destroy;
begin
  if Grammar<>nil then
    Grammar.Release;
  FreeAndNil(ParseTree);
  inherited;
end;

function TGrammarParser.GetTotalSizeBytes: int64;
begin
  result := Data.Size + ParseTree.TotalSizeBytes;
end;

function TGrammarParser.GetTreeToken(ParseTreeNode: integer): string;
begin
  Result := DataToken[ParseTree.Tree.Nodes[ParseTreeNode].Data.Position];
end;

function TGrammarParser.GetDataToken(const P: TTokenPos): string;
begin
  Assert(P.Len mod SizeOf(Char) = 0);
  Data.Position := P.Start;
  Data.Read(Result, P.Len div SizeOf(Char));
end;

function TGrammarParser.Accepts(const AData: TBuffer): Boolean;
begin
  Data := AData;
  Data.Position := 0;
  Result := Accepted;
end;

function TGrammarParser.Accepts(const AData: string): Boolean;
begin
  Data.Text := AData;
  Result := Accepted;
end;

procedure TGrammarParser.Clear;
begin
  Data.Clear;
end;

end.
