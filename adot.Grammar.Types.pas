unit adot.Grammar.Types;

interface

uses
  adot.Types,
  adot.Collections,
  adot.Tools,
  adot.Tools.Rtti,
  adot.Strings,
  System.Generics.Collections,
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
    FirstChild: integer;
    NextSibling: integer;

    procedure SetUp(ARule: TGrammarClass; AStart,ALen: integer); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetUp(ARule: TGrammarClass; AStart,ALen,AFirstChild: integer); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetUp(const ASrc: TParseTreeItem); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;

  TEnumTreeProc = reference to procedure(TreeNode: integer; var Cancel: boolean);
  TFilterTreeProc = reference to procedure(var Tree: TVector<TParseTreeItem>; Node: integer; var Accept: boolean);

  TParseTree = record
  public
    type
      { recursively enumerates ParseTree starting from specified node }
      TEnumerator = record
      private
        Tree: TVector<TParseTreeItem>;
        Stack: TVector<integer>;
        CurrentNode: integer;

      public
        constructor Create(const ATree: TVector<TParseTreeItem>; ARoot: integer);
        function MoveNext: Boolean;
        property Current: integer read CurrentNode;
      end;

      { recursively enumerates ParseTree starting from specified node, returns only matches of specified rule }
      TFilteredEnumerator = record
      private
        Tree: TVector<TParseTreeItem>;
        Stack: TVector<integer>;
        CurrentNode: integer;
        RuleId: TruleId;

      public
        constructor Create(const ATree: TVector<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);
        function MoveNext: Boolean;
        property Current: integer read CurrentNode;
      end;

      { subtree starting from specified node, mostly used for enumeration }
      TCollection = record
        Tree: TVector<TParseTreeItem>;
        Root: integer;

        constructor Create(const ATree: TVector<TParseTreeItem>; ARoot: integer);
        function GetEnumerator: TEnumerator; {$IFNDEF DEBUG}inline;{$ENDIF}
      end;

      { subtree starting from specified node, returns only items matched by specified rule }
      TFilteredCollection = record
        Tree: TVector<TParseTreeItem>;
        Root: integer;
        RuleId: TruleId;

        constructor Create(const ATree: TVector<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);
        function GetEnumerator: TFilteredEnumerator; {$IFNDEF DEBUG}inline;{$ENDIF}
      end;

  private
    function GetItem(n: integer): TParseTreeItem; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetItem(n: integer; const Item: TParseTreeItem); {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetCount: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetChilds(ARoot: integer): TCollection; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetRuleMatches(RootNode: integer; const RuleId: TRuleId): TFilteredCollection; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetFirstChild(Node: integer): integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetNextSibling(Node: integer): integer; {$IFNDEF DEBUG}inline;{$ENDIF}
  public
    Tree: TVector<TParseTreeItem>;
    Root: integer;

    { clear Tree and set Root=-1 }
    procedure Clear;

    { support for "for" syntax: for I in Tree do ... }
    function GetEnumerator: TEnumerator;

    { build tree from subset of nodes according to the filter (original tree where filtered nodes are ommited) }
    function GetSubTree(StartNode: integer; FilterProc: TFilterTreeProc): TParseTree;

    { shortcuts to Tree.Items / Tree.Count }
    property TreeItems[RootNode: integer]:TParseTreeItem read GetItem write SetItem; default;
    property Count: integer read GetCount;

    { enumerable collection of child nodes (including specified Root) }
    property Matches[Root: integer]: TCollection read GetChilds;

    { enumerable collection of child nodes (including specified Root), only matched of specified rule returned }
    property RuleMatches[RootNode: integer; const RuleId: TRuleId]:TFilteredCollection read GetRuleMatches;

    property FirstChild[Node: integer]: integer read GetFirstChild;
    property NextSibling[Node: integer]: integer read GetNextSibling;
  end;

  { grammar based parser }
  TGrammarParser = class abstract
  protected
    function GetDataToken(const P: TTokenPos): string;
    function GetTreeToken(ParseTreeNode: integer): string;
  public
    Data: TBuffer;          { input data (text or bytes) }
    Grammar: TGrammarClass; { main rule of the grammar (class doesn't own that grammar) }
    ParseTree: TParseTree;  { parse tree (available if .Accepted returns True) }

    constructor Create(AMainRule: TGrammarClass); overload; virtual;
    constructor Create(AMainRule: IInterfacedObject<TGrammarClass>); overload;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Accepted: Boolean; overload; virtual; abstract;
    function Accepted(const AData: TBuffer): Boolean; overload;
    function Accepted(const AData: string): Boolean; overload;
    procedure LogParseTree; virtual; abstract;

    { Returns subtree of .Tree with rules assigned to TGRammar, skips all rules with IsIntermediate=True }
    function GetParseTree(var Dst: TVector<TParseTreeItem>): Boolean;

    { for any TTokenPos (position and length in bytes) returns corresponding string from Data }
    property DataToken[const P: TTokenPos]: string read GetDataToken;
    { returns string corresponding to element of ParseTree (position and length in bytes) }
    property TreeToken[ParseTreeNode: integer]: string read GetTreeToken;
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

procedure TParseTreeItem.SetUp(ARule: TGrammarClass; AStart,ALen: integer);
begin
  Rule := ARule;
  Position.SetPos(AStart, ALen);
  FirstChild := -1;
  NextSibling := -1;
end;

procedure TParseTreeItem.SetUp(ARule: TGrammarClass; AStart, ALen, AFirstChild: integer);
begin
  Rule := ARule;
  Position.SetPos(AStart, ALen);
  FirstChild := AFirstChild;
  NextSibling := -1;
end;

procedure TParseTreeItem.SetUp(const ASrc: TParseTreeItem);
begin
  Rule        := ASrc.Rule;
  Position    := ASrc.Position;
  FirstChild  := -1;
  NextSibling := -1;
end;

{ TParseTree.TCollection }

constructor TParseTree.TCollection.Create(const ATree: TVector<TParseTreeItem>; ARoot: integer);
begin
  Tree := ATree;
  Root := ARoot;
end;

function TParseTree.TCollection.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(Tree, Root);
end;

{ TParseTree.TFilteredCollection }

constructor TParseTree.TFilteredCollection.Create(const ATree: TVector<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);
begin
  Tree := ATree;
  Root := ARoot;
  RuleId := ARuleId;
end;

function TParseTree.TFilteredCollection.GetEnumerator: TFilteredEnumerator;
begin
  result := TFilteredEnumerator.Create(Tree, Root, RuleId);
end;

{ TParseTree.TEnumerator }

constructor TParseTree.TEnumerator.Create(const ATree: TVector<TParseTreeItem>; ARoot: integer);
begin
  Tree := ATree;
  Stack.Clear;
  Stack.Add(ARoot);
  CurrentNode := -1;
end;

function TParseTree.TEnumerator.MoveNext: Boolean;
var
  I,J: integer;
begin
  Result := not Stack.Empty;
  if not Result then
    Exit;
  CurrentNode := Stack.ExtractLast;
  J := Stack.Count;
  I := Tree.Items[CurrentNode].FirstChild;
  while I >= 0 do
  begin
    Stack.Add(I);
    I := Tree.Items[I].NextSibling;
  end;
  TArrayUtils.Inverse<integer>(Stack.Items, J, Stack.Count-J);
end;

{ TParseTree.TFilteredEnumerator }

constructor TParseTree.TFilteredEnumerator.Create(const ATree: TVector<TParseTreeItem>; ARoot: integer; const ARuleId: TruleId);
begin
  Tree := ATree;
  Stack.Clear;
  Stack.Add(ARoot);
  CurrentNode := -1;
  RuleId := ARuleId;
end;

function TParseTree.TFilteredEnumerator.MoveNext: Boolean;
var
  I,J: integer;
begin
  repeat
    if Stack.Empty then
      Exit(False);
    CurrentNode := Stack.ExtractLast;
    J := Stack.Count;
    I := Tree.Items[CurrentNode].FirstChild;
    while I >= 0 do
    begin
      Stack.Add(I);
      I := Tree.Items[I].NextSibling;
    end;
    TArrayUtils.Inverse<integer>(Stack.Items, J, Stack.Count-J);
  until Tree.Items[CurrentNode].Rule.Id=RuleId;
  Result := true;
end;

{ TParseTree }

procedure TParseTree.Clear;
begin
  Tree.Clear;
  Root := -1;
end;

function TParseTree.GetChilds(ARoot: integer): TCollection;
begin
  result := TCollection.Create(Tree, ARoot);
end;

function TParseTree.GetRuleMatches(RootNode: integer; const RuleId: TRuleId): TFilteredCollection;
begin
  result := TFilteredCollection.Create(Tree, RootNode, RuleId);
end;

function TParseTree.GetCount: integer;
begin
  result := Tree.Count;
end;

function TParseTree.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Tree, Root);
end;

function TParseTree.GetItem(n: integer): TParseTreeItem;
begin
  result := Tree.Items[n];
end;

function TParseTree.GetFirstChild(Node: integer): integer;
begin
  result := Tree.Items[Node].FirstChild;
end;

function TParseTree.GetNextSibling(Node: integer): integer;
begin
  result := Tree.Items[Node].NextSibling;
end;

function TParseTree.GetSubTree(StartNode: integer; FilterProc: TFilterTreeProc): TParseTree;
var
  Stack: TVector<TCompound<integer,integer>>;  { Src, DstParent }
  DstLastChild: TVector<integer>;              { indices in Dst }
  I: TCompound<integer,integer>;
  J: integer;
  Accept: Boolean;
begin
  Result.Clear;
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
      Result.Tree.Items[J].SetUp(Tree.Items[I.A]);
      DstLastChild.Add(-1);
      if I.B >= 0 then
      begin
        if Result.Tree.Items[I.B].FirstChild < 0 then
          Result.Tree.Items[I.B].FirstChild := J
        else
          Result.Tree.Items[DstLastChild[I.B]].NextSibling := J;
        DstLastChild[I.B] := J;
      end;
      I.B := J;
    end;
    J := Stack.Count;
    I.A := Tree.Items[I.A].FirstChild;
    while I.A >= 0 do
    begin
      Stack.Add(I);
      I.A := Tree.Items[I.A].NextSibling;
    end;
    if Stack.Count > J then
      TArrayUtils.Inverse<TCompound<integer,integer>>(Stack.Items, J, Stack.Count-J);
  end;
  Result.Tree.TrimExcess;
end;

procedure TParseTree.SetItem(n: integer; const Item: TParseTreeItem);
begin
  Tree.Items[N] := Item;
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
  Grammar := AMainRule;
  Grammar.SetupMainRule;
end;

constructor TGrammarParser.Create(AMainRule: IInterfacedObject<TGrammarClass>);
begin
  Create(AMainRule.Data);
end;

destructor TGrammarParser.Destroy;
begin
  if Grammar<>nil then
    Grammar.Release;
  inherited;
end;

function TGrammarParser.GetParseTree(var Dst: TVector<TParseTreeItem>): Boolean;
var
  Stack: TVector<TCompound<integer,integer>>;  { Src, DstParent }
  DstLastChild: TVector<integer>;              { indices in Dst }
  I: TCompound<integer,integer>;
  J: integer;
begin
  Dst.Clear;
  Stack.Clear;
  DstLastChild.Clear;
  if ParseTree.Root >= 0 then
    Stack.Add(TCompound<integer,integer>.Create(ParseTree.Root, -1));
  while Stack.Count>0 do
  begin
    I := Stack.ExtractLast;
    if ParseTree.Tree.Items[I.A].Rule.IncludeIntoParseTree then
    begin
      J := Dst.Add;
      Dst.Items[J].SetUp(ParseTree.Tree.Items[I.A]);
      DstLastChild.Add(-1);
      if I.B >= 0 then
      begin
        if Dst.Items[I.B].FirstChild < 0 then
          Dst.Items[I.B].FirstChild := J
        else
          Dst.Items[DstLastChild[I.B]].NextSibling := J;
        DstLastChild[I.B] := J;
      end;
      I.B := J;
    end;
    J := Stack.Count;
    I.A := ParseTree.Tree.Items[I.A].FirstChild;
    while I.A >= 0 do
    begin
      Stack.Add(I);
      I.A := ParseTree.Tree.Items[I.A].NextSibling;
    end;
    if Stack.Count > J then
      TArrayUtils.Inverse<TCompound<integer,integer>>(Stack.Items, J, Stack.Count-J);
  end;
  Dst.TrimExcess;
  result := Dst.Count >= 0;
end;

function TGrammarParser.GetTreeToken(ParseTreeNode: integer): string;
begin
  Result := DataToken[ParseTree.Tree.Items[ParseTreeNode].Position];
end;

function TGrammarParser.GetDataToken(const P: TTokenPos): string;
begin
  Assert(P.Len mod SizeOf(Char) = 0);
  Data.Position := P.Start;
  Data.Read(Result, P.Len div SizeOf(Char));
end;

function TGrammarParser.Accepted(const AData: TBuffer): Boolean;
begin
  Data := AData;
  Data.Position := 0;
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
end;

end.
