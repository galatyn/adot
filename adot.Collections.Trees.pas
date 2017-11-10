unit adot.Collections.Trees;

interface

{
  TTreeArrayClass<T>
}

uses
  adot.Types,
  adot.Collections.Types,
  adot.Collections.Vectors,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  { Simple class to build/keep tree as array of nodes with FirstChild/NextSibling properties.
    The tree is kept as array of nodes and thus there is no Delete/Remove functionality.
    The only way to delete a node (with subnodes) is call Rollback.
    As soon as Node is commited (Append + Commit or Add) the only way to delete it is Clear for whole tree. }
  TTreeArrayClass<T> = class
  public
    type
      TNode = record
        Data: T;
        FirstChild: integer;
        NextSibling: integer;
      end;
      PNode = ^TNode;

      { Enumerates all nodes (by index) }
      TEnumerator = record
      private
        Nodes: TArr<TNode>;
        Index: integer;

        function GetCurrent: integer;

      public
        procedure Init(const ANodes: TArr<TNode>);

        function MoveNext: Boolean;
        property Current: integer read GetCurrent;
      end;

      { Same as TEnumerator but returns value instead of index }
      TValuesEnumerator = record
      private
        Enum: TEnumerator;

        function GetCurrent: T;

      public
        procedure Init(const ANodes: TArr<TNode>);

        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;

      { Enumerates all nodes (by index) starting from specified one }
      TSubtreeEnumerator = record
      private
        Nodes: TArr<TNode>;
        Stack: TArr<integer>;
        CurrentNode: integer;

      public
        procedure Init(const ANodes: TArr<TNode>; ARoot: integer);

        function MoveNext: Boolean;
        property Current: integer read CurrentNode;
      end;

      { Enumerates all values }
      TValuesCollection = record
        Nodes: TArr<TNode>;

        procedure Init(const ANodes: TArr<TNode>);
        function GetEnumerator: TValuesEnumerator;
      end;

      { Enumerable subtree starting from specified node }
      TSubtreeCollection = record
        Nodes: TArr<TNode>;
        Root: integer;

        procedure Init(const ANodes: TArr<TNode>; ARoot: integer);
        function GetEnumerator: TSubtreeEnumerator;
      end;

    var
      Nodes: TArr<TNode>;   { Tree of the nodes stored as array. It is recommended to create single root node. }
      Stack: TArr<integer>; { Stack for tracking of CurParent }
      CurParent: integer;      { Current destination (parent node) for Append/Add. }

  private
    function GetEmpty: boolean;
    function GetCount: integer;
    function GetValue(n: integer): T;
    procedure SetValue(n: integer; const Value: T);
    function GetValuesAsArray: TArray<T>;
    function GetTotalSizeBytes: int64;
    function GetSubtreeCollection(StaringNode: integer): TSubtreeCollection;
    function GetValuesCollection: TValuesCollection;

  public
    { Empty Nodes and other structures. }
    procedure Clear;

    { Sequentional adding. Append/Commit(or Rollback) must be balanced. Example:
        Tree.Append('root item');
          Tree.Add('child 1');
          Tree.Append('child 2');
            Tree.Add('child 2.1');
            Tree.Add('child 2.2');
            Tree.Add('child 2.3');
          Tree.Commit;
          Tree.Add('child 3');
        Tree.Commit;
      No need to keep/handle pointers to nodes, no need to navigate directly
      parent/child node etc. }

    { add new node (as child to CurParent) and make it CurParent }
    function Append(const Value: T): integer; overload;
    function Append: integer; overload;
    { Add = Append + Commit (add single child without own sibling/child nodes) }
    function Add(const Value: T): integer; overload;
    function Add: integer; overload;
    { all subchilds are added, assign parent node as CurParent }
    function Commit: integer; overload;
    function Commit(ReverseOrderOfChildNodes: Boolean): integer; overload;
    { remove CurParent with all childs and assign parent node as CurParent }
    function Rollback: integer;

    { Random add functions. Example:
        Tree.Clear;
        I := Tree.AddChild('root item', -1);
          J := Tree.AddChild('child 1', I);
          J := Tree.AddSibling('child 2', J);
            K := Tree.AddChild('child 2.1', J);
            K := Tree.AddSibling('child 2.2', K);
            K := Tree.AddSibling('child 2.3', K);
          J := Tree.AddSibling('child 3', J); }

    { Add child node (AParent=-1 for root node). Only one root node is allowed. }
    function AddChild(const Value: T; AParent: integer): integer;
    { Add sibling node. APrevSibling must be provided. }
    function AddSibling(const Value: T; APrevSibling: integer): integer;

    { Example: for I in Tree do (*...*) ; }
    { Enumerator of all nodes (top-down,left-right). }
    function GetEnumerator: TEnumerator;

    property Empty: boolean read GetEmpty;
    property Count: integer read GetCount;
    property Values[n: integer]: T read GetValue write SetValue; default;
    property AsArray: TArray<T> read GetValuesAsArray;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;

    { Enumerator of all nodes from subtree (top-down,left-right). }
    property Subtree[StaringNode: integer]: TSubtreeCollection read GetSubtreeCollection;

    property ValuesCollection: TValuesCollection read GetValuesCollection;
  end;

implementation

uses
  adot.Tools;

{ TTreeArrayClass<T>.TEnumerator }

procedure TTreeArrayClass<T>.TEnumerator.Init(const ANodes: TArr<TNode>);
begin
  Self := Default(TEnumerator);
  Nodes := ANodes;
  Index := 0;
end;

function TTreeArrayClass<T>.TEnumerator.GetCurrent: integer;
begin
  result := Index-1;
end;

function TTreeArrayClass<T>.TEnumerator.MoveNext: Boolean;
begin
  result := Index < Nodes.Count;
  if result then
    inc(Index);
end;

{ TTreeArrayClass<T> }

function TTreeArrayClass<T>.GetCount: integer;
begin
  result := Nodes.Count;
end;

function TTreeArrayClass<T>.GetEmpty: boolean;
begin
  result := Nodes.Empty;
end;

function TTreeArrayClass<T>.GetEnumerator: TEnumerator;
begin
  result.Init(Nodes);
end;

function TTreeArrayClass<T>.GetSubtreeCollection(StaringNode: integer): TSubtreeCollection;
begin
  result.Init(Nodes, StaringNode);
end;

function TTreeArrayClass<T>.GetTotalSizeBytes: int64;
begin
  result := Nodes.TotalSizeBytes + Stack.TotalSizeBytes + SizeOf(CurParent);
end;

function TTreeArrayClass<T>.GetValue(n: integer): T;
begin
  result := Nodes.Items[n].Data;
end;

procedure TTreeArrayClass<T>.SetValue(n: integer; const Value: T);
begin
  Nodes.Items[n].Data := Value;
end;

function TTreeArrayClass<T>.GetValuesAsArray: TArray<T>;
var
  I: Integer;
begin
  setLength(result, Nodes.Count);
  for I := 0 to Nodes.Count-1 do
    result[I] := Nodes.Items[I].Data;
end;

function TTreeArrayClass<T>.GetValuesCollection: TValuesCollection;
begin
  result.Init(Nodes);
end;

procedure TTreeArrayClass<T>.Clear;
begin
  Nodes.Clear;
  Stack.Clear;
  CurParent := -1;
end;

function TTreeArrayClass<T>.Append(const Value: T): integer;
var
  Node: PNode;
begin
  Result := Nodes.Add;
  Node := @Nodes.Items[result];
  Node.Data := Value;
  Node.FirstChild := -1;
  if CurParent < 0 then
    if Nodes.Count > 1 then
    begin
      Nodes.DeleteLast;
      raise EForbiddenOperation.Create('Only one root node is allowed');
    end
    else
      Node.NextSibling := -1
  else
    with Nodes.Items[CurParent] do
    begin
      Node.NextSibling := FirstChild;
      FirstChild := result;
    end;
  Stack.Add(result); { to be able rollback, we save Nodes.Count + CurParent  }
  Stack.Add(CurParent);
  CurParent := result;
end;

function TTreeArrayClass<T>.Append: integer;
begin
  result := Append(Default(T));
end;

function TTreeArrayClass<T>.Add(const Value: T): integer;
var
  Node: PNode;
begin

  { We can use pair Append+Commit here, but we can make it more
    efficient if we avoid manipulations with Stack/CurParent. }
//  result := Append(Value);
//  Commit;

  Result := Nodes.Add;
  Node := @Nodes.Items[result];
  Node.Data := Value;
  Node.FirstChild := -1;
  if CurParent < 0 then
    if Nodes.Count > 1 then
    begin
      Nodes.DeleteLast;
      raise EForbiddenOperation.Create('Only one root node is allowed');
    end
    else
      Node.NextSibling := -1
  else
    with Nodes.Items[CurParent] do
    begin
      Node.NextSibling := FirstChild;
      FirstChild := result;
    end;
end;

function TTreeArrayClass<T>.Add: integer;
begin
  result := Add(Default(T));
end;

function TTreeArrayClass<T>.Commit(ReverseOrderOfChildNodes: Boolean): integer;
var
  FirstChild, C,I,J: Integer;
begin
  C := CurParent;
  CurParent := Stack.ExtractLast; { stored CurParrent }
  Result := Stack.ExtractLast; { stored Count (used by rollback) = index of item to be commited }
  { we added items in reverse order, we restore normal order here }
  if ReverseOrderOfChildNodes then
    Exit;

  { if there is no child nodes, we can exit }
  FirstChild := Nodes.Items[C].FirstChild;
  if FirstChild < 0 then
    Exit;

  { if there is one only child node, we can exit }
  I := Nodes.Items[FirstChild].NextSibling;
  if I < 0 then
    Exit;

  { We just taken NextSibling from FirstChild and will insert all childs before that node.
    It means FirstChild became last node in new chain and we should assign NextSibling = -1. }
  Nodes.Items[FirstChild].NextSibling := -1;
  repeat
    J := Nodes.Items[I].NextSibling;
    Nodes.Items[I].NextSibling := FirstChild;
    FirstChild := I;
    I := J;
  until I < 0;
  Nodes.Items[C].FirstChild := FirstChild;
end;

function TTreeArrayClass<T>.Commit: integer;
begin
  Result := Commit(False);
end;

function TTreeArrayClass<T>.Rollback: integer;
begin
  CurParent := Stack.ExtractLast;

  { Append method always adds item as first child to its parent.
    It means we don't need to scan chain to remove the node currectly. }
  if CurParent >= 0 then
    with Nodes.Items[CurParent] do
      FirstChild := Nodes.Items[FirstChild].NextSibling;

  { now we can delete the node with all subnodes }
  Result := Stack.ExtractLast;
  Nodes.Count := Result;
end;

function TTreeArrayClass<T>.AddChild(const Value: T; AParent: integer): integer;
var
  Node: PNode;
begin
  Result := Nodes.Add;
  Node := @Nodes.Items[result];
  Node.Data := Value;
  Node.FirstChild := -1;
  if AParent=-1 then
    Node.NextSibling := -1
  else
  with Nodes.Items[AParent] do
  begin
    Node.NextSibling := FirstChild;
    FirstChild := Result;
  end;
end;

function TTreeArrayClass<T>.AddSibling(const Value: T; APrevSibling: integer): integer;
var
  Node: PNode;
begin
  Assert(APrevSibling >= 0);
  Result := Nodes.Add;
  Nodes.Items[APrevSibling].NextSibling := Result;
  Node := @Nodes.Items[result];
  Node.Data := Value;
  Node.FirstChild := -1;
  Node.NextSibling := -1;
end;

{ TTreeArrayClass<T>.TSubtreeEnumerator }

procedure TTreeArrayClass<T>.TSubtreeEnumerator.Init(const ANodes: TArr<TNode>; ARoot: integer);
begin
  Self := Default(TSubtreeEnumerator);
  Nodes := ANodes;
  Stack.Clear;
  Stack.Add(ARoot);
  CurrentNode := -1;
end;

function TTreeArrayClass<T>.TSubtreeEnumerator.MoveNext: Boolean;
var
  I,J: integer;
begin
  Result := not Stack.Empty;
  if not Result then
    Exit;
  CurrentNode := Stack.ExtractLast;
  J := Stack.Count;
  I := Nodes[CurrentNode].FirstChild;
  while I >= 0 do
  begin
    Stack.Add(I);
    I := Nodes[I].NextSibling;
  end;
  TArrayUtils.Inverse<integer>(Stack.Items, J, Stack.Count-J);
end;

{ TTreeArrayClass<T>.TSubtreeCollection }

procedure TTreeArrayClass<T>.TSubtreeCollection.Init(const ANodes: TArr<TNode>; ARoot: integer);
begin
  Self := Default(TSubtreeCollection);
  Nodes := ANodes;
  Root := ARoot;
end;

function TTreeArrayClass<T>.TSubtreeCollection.GetEnumerator: TSubtreeEnumerator;
begin
  result.Init(Nodes, Root);
end;

{ TTreeArrayClass<T>.TValuesEnumerator }

procedure TTreeArrayClass<T>.TValuesEnumerator.Init(const ANodes: TArr<TNode>);
begin
  Self := Default(TValuesEnumerator);
  Enum.Init(ANodes);
end;

function TTreeArrayClass<T>.TValuesEnumerator.GetCurrent: T;
begin
  result := Enum.Nodes.Items[Enum.Current].Data;
end;

function TTreeArrayClass<T>.TValuesEnumerator.MoveNext: Boolean;
begin
  result := Enum.MoveNext;
end;

{ TTreeArrayClass<T>.TValuesCollection }

procedure TTreeArrayClass<T>.TValuesCollection.Init(const ANodes: TArr<TNode>);
begin
  Self := Default(TValuesCollection);
  Nodes := ANodes;
end;

function TTreeArrayClass<T>.TValuesCollection.GetEnumerator: TValuesEnumerator;
begin
  result.Init(Nodes);
end;

end.
