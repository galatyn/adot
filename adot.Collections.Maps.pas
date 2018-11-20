unit adot.Collections.Maps;

interface

{
  Unordered containers:
    TMapClass<TKey,TValue>           Slightly extended TDictionary<TKey,TValues>
    TMap<TKey,TValue>                Managed analog of TDictionary<TKey,TValues>
    TMultimapClass<TKey,TValue>      Multimap implementation based on TDictionary<TKey,TValues>
  Ordered containers:
    TAATree<TKey,TValue>             Implementation of AA-tree
    TBinarySearchTree<TKey,TValue>   Binary search tree based on TAATree<TKey,TValue>
    TOrderedMapClass<TKey,TValue>    Ordered map implementation based on TDictionary<TKey,TValues>
}

uses
  adot.Types,
  adot.Strings,
  adot.Collections.Types,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.Character,
  System.StrUtils,
  System.Math;

type
  { Class for map. Based on TDictionary and extends it with some features. }
  TMapClass<TKey,TValue> = class(TDictionary<TKey,TValue>)
  private
  protected
    FComparerCopy: IEqualityComparer<TKey>; { FDictionary.Comparer is hidden in private section, so we keep copy }
    FOwnerships: TDictionaryOwnerships;

    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); override;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); override;
    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);
    class function EscapeStrVal(const S: string): string; static;

  public
    constructor Create(ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const AValues: array of TPair<TKey,TValue>; AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const AValues: TEnumerable<TPair<TKey,TValue>>; AComparer: IEqualityComparer<TKey> = nil); overload;

    procedure Add(const AValues: array of TPair<TKey,TValue>); overload;
    procedure Add(const AValues: TEnumerable<TPair<TKey,TValue>>); overload;

    procedure AddOrSetValue(const AValues: array of TPair<TKey,TValue>); overload;
    procedure AddOrSetValue(const AValues: TEnumerable<TPair<TKey,TValue>>); overload;

    procedure Remove(const AKeys: array of TKey); overload;
    procedure Remove(const AKeys: TEnumerable<TKey>); overload;

    function Empty: boolean;
    function ToText(const Delimiter: string = #13#10): string;
    function ToString: string; override;

    property Comparer: IEqualityComparer<TKey> read FComparerCopy;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  { Class for map. Based on TDictionary and extends it with some features. }
  TMap<TKey,TValue> = record
  public
    { Delphi 10.1 Seattle has issues with code generation for overloaded
      operators if they are inlined. The issue reported here:
        https://quality.embarcadero.com/browse/RSP-15196
      For now we have to avoid of using _inline_ methods here. }
    type
      TPairEnumerator  = TMapClass<TKey,TValue>.TPairEnumerator;
      TKeyEnumerator   = TMapClass<TKey,TValue>.TKeyEnumerator;
      TValueEnumerator = TMapClass<TKey,TValue>.TValueEnumerator;
      TKeyCollection   = TMapClass<TKey,TValue>.TKeyCollection;
      TValueCollection = TMapClass<TKey,TValue>.TValueCollection;

  private
    FMapInt: IInterfacedObject<TMapClass<TKey,TValue>>;

    procedure CreateMap(ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil);

    function GetReadonly: TMapClass<TKey,TValue>;
    function GetReadWrite: TMapClass<TKey,TValue>;
    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);
    function GetCollection: TEnumerable<TPair<TKey, TValue>>;

    property ReadOnly: TMapClass<TKey,TValue> read GetReadonly;
    property ReadWrite: TMapClass<TKey,TValue> read GetReadWrite;

    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    function GetCount: integer;
    function GetEmpty: Boolean;

  public
    procedure Init; overload;
    procedure Init(ACapacity: integer; AComparer: IEqualityComparer<TKey> = nil); overload;
    procedure Init(const V: array of TPair<TKey,TValue>; ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil); overload;
    procedure Init(const V: TEnumerable<TPair<TKey,TValue>>; ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil); overload;
    procedure Init(V: TMap<TKey,TValue>; ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil); overload;

    function Copy: TMap<TKey,TValue>;

    function GetEnumerator: TPairEnumerator;

    procedure Add(const Key: TKey; Value: TValue); overload;
    procedure Add(const Pair: TPair<TKey, TValue>); overload;
    procedure Add(const V: TEnumerable<TPair<TKey, TValue>>); overload;
    procedure Add(V: TMap<TKey,TValue>); overload;
    procedure Add(const V: array of TPair<TKey, TValue>); overload;

    procedure AddOrSetValue(const Key: TKey; Value: TValue); overload;
    procedure AddOrSetValue(const Pair: TPair<TKey, TValue>); overload;
    procedure AddOrSetValue(const V: TEnumerable<TPair<TKey, TValue>>); overload;
    procedure AddOrSetValue(V: TMap<TKey,TValue>); overload;
    procedure AddOrSetValue(const V: array of TPair<TKey, TValue>); overload;

    procedure Remove(const V: TKey); overload;
    procedure Remove(const V: TEnumerable<TKey>); overload;
    procedure Remove(const V: array of TKey); overload;

    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    function GetValue(const Key: TKey): TValue;
    function GetValueDef(const Key: TKey): TValue;
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    procedure Clear;
    procedure Release; { release underlying object }
    procedure TrimExcess;
    function ContainsKey(const Key: TKey): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>;

    function ToString: string;
    function ToText(const Delimiter: string = #13#10): string;

    class operator Equal(A,B: TMap<TKey,TValue>): Boolean;
    class operator NotEqual(A,B: TMap<TKey,TValue>): Boolean;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property Collection: TEnumerable<TPair<TKey, TValue>> read GetCollection;
  end;

  TContainsCheckType = (cctAll, cctAnyOf);

  { Multimap class. Supports multiple items sharing same key. Keeps items in efficient way.}
  TMultimapClass<TKey,TValue> = class(TEnumerableExt<TPair<TKey,TValue>>)
  protected
    type
      TMultimapKey = record
        Key: TKey;
        Number: integer;
      end;

      {# TKey can be String for example, so we can't use default comparer for
        TMultimapKey record type, we have to implement specific one. }
      TMultimapKeyEqualityComparer = class(TEqualityComparer<TMultimapKey>)
      private
        FKeyComparer: IEqualityComparer<TKey>;

      public
        constructor Create(AKeyComparer: IEqualityComparer<TKey>);

        function Equals(const Left, Right: TMultimapKey): Boolean; overload; override;
        function GetHashCode(const Value: TMultimapKey): Integer; overload; override;
      end;

  public
    type

      {# Standard containers in Delphi use classes for enumerators.
         It is ok when we keep single instance of the class inside, but for multimap we
         will have to keep lot of instances (for every key where items enumerator has requested).
         To avoid of this we use record type instead of class type. }
      TValueEnumerator = record
      private
        FMultimap: TMultimapClass<TKey,TValue>;
        FMultimapKey: TMultimapKey;

        function GetCurrent: TValue;
        function GetKey: TKey;

      public
        procedure Init(AMultimap: TMultimapClass<TKey,TValue>; const AKey: TKey);

        function MoveNext: Boolean;
        property Current: TValue read GetCurrent;
        property Key: TKey read GetKey;

        {  Returns copy. It allows us to use both syntaxes:
           Example 1.
             Enum := Multimap.Values['key1'];
             while Enum.MoveNext do
               Fun(Enum.Current);
           Example 2.
             for v in Multimap['key1'] do
               Fun(v); }
        function GetEnumerator: TValueEnumerator;
      end;

      {# Usually we don't need to use it directly. Use default enumerator for TMultimapClass<> instead.
         We have to use class (not record) because we want TMultimapClass to be compatible with TEnumerable
         and TEnumerable.DoGetEnumerator returns class TEnumerator<TPair<TKey,TValue>>. }
      TPairEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
      protected
        FCurrentValue: TValueEnumerator;
        FInEnumKey: boolean;
        FMultimap: TMultimapClass<TKey,TValue>;
        FCurrentKey: TDictionary<TKey, integer>.TKeyEnumerator;

        function DoGetCurrent: TPair<TKey,TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const AMultimap: TMultimapClass<TKey,TValue>);
      end;

      TKeyEnumerator = TDictionary<TKey, integer>.TKeyEnumerator;
      TKeyCollection = TDictionary<TKey, integer>.TKeyCollection;

  protected
    FCount: TDictionary<TKey, integer>;
    FValues: TDictionary<TMultimapKey, TValue>;

    { Single instance created by request.
      Provides TEnumerable interface to keys (Multimap.Keys.ToArray etc). }
    FKeyCollection: TKeyCollection;

    function GetTotalValuesCount: integer;
    function GetValuesCount(const AKey: TKey):Integer;
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetValuesEnumerator(const AKey: TKey): TValueEnumerator;
    function GetKeys: TKeyCollection;

  public
    constructor Create; overload;
    constructor Create(AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; AComparer: IEqualityComparer<TKey> = nil); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const AKey: TKey; const AValues: TArray<TValue>); overload;
    procedure Add(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;

    { e := m.Values[Key];
      while e.MoveNext do
        if m.Current=10 then m.RemoveValue(e); }
    function Remove(const AKey: TKey):Boolean;
    procedure RemoveValue(const AEnum: TValueEnumerator);
    procedure RemoveValues(const AKey: TKey; const AValues: TArray<TValue>); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;

    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsKeys(const AKeys: array of TKey; AContainsCheckType: TContainsCheckType = cctAnyOf): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; AContainsCheckType: TContainsCheckType = cctAnyOf): Boolean; overload;

    function ContainsValue(const AKey: TKey; const AValue: TValue; AComparer: IEqualityComparer<TValue> = nil): Boolean;
    function ContainsValues(const AKey: TKey; const AValues: TArray<TValue>; AContainsCheckType: TContainsCheckType = cctAnyOf;
      AComparer: IEqualityComparer<TValue> = nil): Boolean; overload;
    function ContainsValues(const AKey: TKey; const AValues: TEnumerable<TValue>; AContainsCheckType: TContainsCheckType = cctAnyOf;
      AComparer: IEqualityComparer<TValue> = nil): Boolean; overload;

    { Enumerator. Example:
      p: TPair<string, integer>;
        for p in m do
          [do something] p.Key / p.Value; }
    function GetEnumerator: TPairEnumerator; reintroduce;

    function Empty: boolean;

    property TotalValuesCount: integer read GetTotalValuesCount;
    property ValuesCount[const AKey: TKey]:integer read GetValuesCount;

    {  Enumerator of values for specific Key. Returns records, ne need to destroy.
       Example 1:
          Enum := m.Values['test'];
          While Enum.MoveNext do
            if Enum.Current=314 then m.RemoveValue(Enum);
       Example 2:
          for s in m.Values['test'] do
            <do something with s> }
    property Values[const AKey: TKey]: TValueEnumerator read GetValuesEnumerator; default;

    {  Enumerator of the keys. For example we can enumerate all values this way
       (alternative to default enumerator):
          for Key in m.Keys do
            for Value in m.Values[Key] do }
    property Keys: TKeyCollection read GetKeys;
  end;

  {# Balanced binary search tree (BST). The performance of an AA tree is equivalent to the performance of a red-black tree.
    http://en.wikipedia.org/wiki/AA_tree }
  TSearchType = (stAll, stAny);
  TItemHandle = NativeInt;
  TAATree<TKey,TValue> = class(TEnumerableExt<TPair<TKey,TValue>>)
  private
  protected
    type

      { We use allocation of items by AllocMem, no extra initialization is needed }
      P_AATreeItem = ^T_AATreeItem;
      T_AATreeItem = record
        Parent, Left, Right: P_AATreeItem;
        Level: integer;
        Data: TPair<TKey, TValue>;
      end;

      TPairEnumerator = class(TEnumerator<TPair<TKey,TValue>>)
      protected
        [Weak] FTree: TAATree<TKey,TValue>;
        FCurrentItem: TItemHandle;

        function DoMoveNext: Boolean; override;
        function DoGetCurrent: TPair<TKey,TValue>; override;
      public
        constructor Create(const ATree: TAATree<TKey,TValue>);
      end;

      TKeyEnumerator = class(TEnumerator<TKey>)
      protected
        [Weak] FTree: TAATree<TKey,TValue>;
        FCurrentItem: TItemHandle;

        function DoMoveNext: Boolean; override;
        function DoGetCurrent: TKey; override;
      public
        constructor Create(const ATree: TAATree<TKey,TValue>);
      end;

      TValueEnumerator = class(TEnumerator<TValue>)
      protected
        [Weak] FTree: TAATree<TKey,TValue>;
        FCurrentItem: TItemHandle;

        function DoMoveNext: Boolean; override;
        function DoGetCurrent: TValue; override;
      public
        constructor Create(const ATree: TAATree<TKey,TValue>);
      end;

      TKeyCollection = class(TEnumerableExt<TKey>)
      protected
        [Weak] FTree: TAATree<TKey,TValue>;

        function DoGetEnumerator: TEnumerator<TKey>; override;
      public
        constructor Create(const ATree: TAATree<TKey,TValue>);
      end;

      TValueCollection = class(TEnumerableExt<TValue>)
      protected
        [Weak] FTree: TAATree<TKey,TValue>;

        function DoGetEnumerator: TEnumerator<TValue>; override;
      public
        constructor Create(const ATree: TAATree<TKey,TValue>);
      end;

    var
      FCount: integer;
      FRoot, FBottom, FDeleted, FLast: P_AATreeItem;
      FComparer: IComparer<TKey>;
      FKeyCollection: TKeyCollection;
      FValueCollection: TValueCollection;
      FOnKeyNotify: TCollectionNotifyEvent<TKey>;
      FOnValueNotify: TCollectionNotifyEvent<TValue>;

    function AllocNewItem: P_AATreeItem;
    procedure ReleaseItem(p: P_AATreeItem);
    function PtrToHandle(p: P_AATreeItem): TItemHandle;
    function HandleToPtr(p: TItemHandle): P_AATreeItem;
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetKeyCollection: TKeyCollection;
    function GetValueCollection: TValueCollection;
    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); virtual;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); virtual;

    procedure treeSkew(var p: P_AATreeItem);
    procedure treeSplit(var p: P_AATreeItem);
    function treeAdd(p,aparent: P_AATreeItem; var Dst: P_AATreeItem): Boolean;
    function treeGetHeight(p: P_AATreeItem): integer;
    function treeFullHeight: integer;
    function treeMin(p: P_AATreeItem): P_AATreeItem;
    function treeMax(p: P_AATreeItem): P_AATreeItem;
    function treeSuccessor(p: P_AATreeItem): P_AATreeItem;
    function treePredecessor(p: P_AATreeItem): P_AATreeItem;
    procedure treeClear(p: P_AATreeItem);
    function treeDelete(x: P_AATreeItem; var t: P_AATreeItem): boolean;
    function treeFind(const AKey: TKey): P_AATreeItem;
    procedure treeMove(Src, Dst: P_AATreeItem);
    procedure treeReplace(Src, Dst: P_AATreeItem);

    function GetKey(AHandle: TItemHandle): TKey;
    function GetValue(AHandle: TItemHandle): TValue;
    procedure SetValue(AHandle: TItemHandle; const AValue: TValue);
    function GetValueByKey(const AKey: TKey): TValue;
    procedure SetValueByKey(const AKey: TKey; const AValue: TValue);
    function GetPair(AHandle: TItemHandle): TPair<TKEy,TValue>;

  public
    constructor Create; overload;
    constructor Create(AComparer: IComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;
    function GetEnumerator: TPairEnumerator;

    procedure Clear;
    function Add(const AKey: TKey; const AValue: TValue): TItemHandle; overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    function AddOrSetValue(const AKey: TKey; const AValue: TValue): TItemHandle;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Delete(AHandle: TItemHandle);
    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKeys: TArray<TKey>); overload;
    procedure Remove(const AKeys: TEnumerable<TKey>); overload;
    function ContainsKey(const Key: TKey): Boolean; overload;
    function ContainsKeys(const AKeys: array of TKey; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue> = nil): Boolean;
    function MinKey: TKey;
    function MaxKey: TKey;

    { navigate over the tree }
    function GetRoot(var ARoot: TItemHandle): Boolean;
    function GetLeftChild(AItem: TItemHandle; var AChild: TItemHandle): Boolean;
    function GetRightChild(AItem: TItemHandle; var AChild: TItemHandle): Boolean;
    function GetParent(AItem: TItemHandle; var AParent: TItemHandle): Boolean;

    { Enumeration of all items from min to max and in reverse order. }
    function FindMin: TItemHandle;
    function FindMax: TItemHandle;
    function Find(const AKey: TKey): TItemHandle;
    function First(var AHandle: TItemHandle): Boolean;
    function Last(var AHandle: TItemHandle): Boolean;
    function Prev(var AHandle: TItemHandle): Boolean;
    function Next(var AHandle: TItemHandle): Boolean;

    property TreeHeight: integer read treeFullHeight;
    property Keys[AHandle: TItemHandle]: TKey read GetKey;
    property Values[AHandle: TItemHandle]: TValue read GetValue write SetValue;
    property Pairs[AHandle: TItemHandle]: TPair<TKey,TValue> read GetPair;
    property Items[const AKey: TKey]: TValue read GetValueByKey write SetValueByKey; default;
    property Count: integer read FCount;
    property KeyCollection: TKeyCollection read GetKeyCollection;
    property ValueCollection: TValueCollection read GetValueCollection;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  end;

  { Binary search tree. }
  TBinarySearchTree<TKey,TValue> = Class(TAATree<TKey,TValue>);

  { We do not expose handle-related functions here. That is why TAATree is slightly
    faster, but TOrderedMapClass has interface similar to TDictionary.
    Usually TMap is preferred over direct access to TAATree. }
  { Ordered map. }
  TOrderedMapClass<TKey,TValue> = class(TEnumerableExt<TPair<TKey,TValue>>)
  private
  protected
    type
      TPairEnumerator = TAATree<TKey,TValue>.TPairEnumerator;
      TKeyEnumerator = TAATree<TKey,TValue>.TKeyEnumerator;
      TValueEnumerator = TAATree<TKey,TValue>.TValueEnumerator;
      TKeyCollection = TAATree<TKey,TValue>.TKeyCollection;
      TValueCollection = TAATree<TKey,TValue>.TValueCollection;

    var
      FTree: TAATree<TKey,TValue>;
      FOwnerships: TDictionaryOwnerships;
      FOnKeyNotify: TCollectionNotifyEvent<TKey>;
      FOnValueNotify: TCollectionNotifyEvent<TValue>;

    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetKeyCollection: TKeyCollection;
    function GetValueCollection: TValueCollection;

    function GetCount: integer;
    function GetItem(const AKey: TKey): TValue;
    function GetTreeHeight: integer;
    procedure SetItem(const AKey: TKey; const Value: TValue);

    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);
    procedure treeOnKeyNotify(Sender: TObject; const Key: TKey; Action: TCollectionNotification);
    procedure treeOnValueNotify(Sender: TObject; const Value: TValue; Action: TCollectionNotification);

  public
    constructor Create; overload;
    constructor Create(AComparer: IComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue);
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKeys: TArray<TKey>); overload;
    procedure Remove(const AKeys: TEnumerable<TKey>); overload;
    function ContainsKey(const Key: TKey): Boolean; overload;
    function ContainsKeys(const AKeys: array of TKey; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue> = nil): Boolean;
    function Min: TPair<TKey, TValue>;
    function Max: TPair<TKey, TValue>;
    function FindKeyByIndex(Index: integer): TKey;

    { navigate over the tree }
    function GetRoot(var Key: TKey): Boolean;
    function GetLeftChild(const AKey: TKey; var AChild: TKey): Boolean;
    function GetRightChild(const AKey: TKey; var AChild: TKey): Boolean;
    function GetParent(const AKey: TKey; var AParent: TKey): Boolean;

    { navigate all items from min to max (in both directions) }
    function First(var AKey: TKey): Boolean;
    function Last(var AKey: TKey): Boolean;
    function Next(const AKey: TKey; var ANewKey: TKey): Boolean;
    function Prev(const AKey: TKey; var ANewKey: TKey): Boolean;

    property Count: integer read GetCount;
    property Items[const AKey: TKey]: TValue read GetItem write SetItem ; default;
    property KeyCollection: TKeyCollection read GetKeyCollection;
    property ValueCollection: TValueCollection read GetValueCollection;
    property TreeHeight: integer read GetTreeHeight;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property OnKeyNotify: TCollectionNotifyEvent<TKey> read FOnKeyNotify write FOnKeyNotify;
    property OnValueNotify: TCollectionNotifyEvent<TValue> read FOnValueNotify write FOnValueNotify;
  end;

implementation

uses
  adot.Hash,
  adot.Collections,
  adot.Tools,
  adot.Tools.RTTI,
  adot.Collections.Sets;

{ TMapClass<TKey, TValue> }

constructor TMapClass<TKey, TValue>.Create(ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);
  FComparerCopy := AComparer;
end;

constructor TMapClass<TKey, TValue>.Create(const AValues: array of TPair<TKey, TValue>; AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AComparer);
  FComparerCopy := AComparer;
  Add(AValues);
end;

constructor TMapClass<TKey, TValue>.Create(const AValues: TEnumerable<TPair<TKey, TValue>>; AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AComparer);
  FComparerCopy := AComparer;
  Add(AValues);
end;

function TMapClass<TKey, TValue>.Empty: boolean;
begin
  result := Count=0;
end;

class function TMapClass<TKey, TValue>.EscapeStrVal(const S: string): string;
var
  i: Integer;
begin
  for i := Low(S) to High(S) do
    if S[i].IsLetter or (S[i]=',') then
      Exit('"' + s + '"');
  result := S;
end;

function TMapClass<TKey, TValue>.ToText(const Delimiter: string = #13#10): string;
var
  Arr: TArray<TPair<TKey, TValue>>;
  KeyComparer: IComparer<TKey>;
  ValueComparer: IComparer<TValue>;
  PairComparer: IComparer<TPair<TKey, TValue>>;
  Pair: TPair<TKey, TValue>;
  i: Integer;
  Buf: TStringBuilder;
begin
  Arr := ToArray;
  KeyComparer := TComparerUtils.DefaultComparer<TKey>;
  ValueComparer := TComparerUtils.DefaultComparer<TValue>;
  PairComparer := TDelegatedComparer<TPair<TKey, TValue>>.Create(
    function (const L,R: TPair<TKey, TValue>): integer
    begin
      result := KeyComparer.Compare(L.Key, R.Key);
      if result=0 then
        result := ValueComparer.Compare(L.Value, R.Value);
    end);
  TArray.Sort<TPair<TKey, TValue>>(Arr, PairComparer);
  Buf := TStringBuilder.Create;
  for Pair in Arr do
  begin
    if Buf.Length>0 then Buf.Append(Delimiter);
    Buf.Append('(');
    Buf.Append( EscapeStrVal(TRttiUtils.ValueAsString<TKey>(Pair.Key)));
    Buf.Append(', ');
    Buf.Append(TRttiUtils.ValueAsString<TValue>(Pair.Value));
    Buf.Append(')');
  end;
  Result := Buf.ToString;
end;

function TMapClass<TKey, TValue>.ToString: string;
begin
  result := ToText(' ');
end;

function TMapClass<TKey, TValue>.GetOwnsKeys: boolean;
begin
  result := doOwnsKeys in FOwnerships;
end;

function TMapClass<TKey, TValue>.GetOwnsValues: boolean;
begin
  result := doOwnsValues in FOwnerships;
end;

procedure TMapClass<TKey, TValue>.SetOwnsKeys(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TKey> then
    raise Exception.Create('Generic type is not a class.');
  if Value
    then include(FOwnerships, doOwnsKeys)
    else exclude(FOwnerships, doOwnsKeys);
end;

procedure TMapClass<TKey, TValue>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TValue> then
    raise Exception.Create('Generic type is not a class.');
  if Value
    then include(FOwnerships, doOwnsValues)
    else exclude(FOwnerships, doOwnsValues);
end;

procedure TMapClass<TKey, TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  inherited;
  if (Action = TCollectionNotification.cnRemoved) and (doOwnsKeys in FOwnerships) then
    PObject(@Key)^.DisposeOf;
end;

procedure TMapClass<TKey, TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  inherited;
  if (Action = TCollectionNotification.cnRemoved) and (doOwnsValues in FOwnerships) then
    PObject(@Value)^.DisposeOf;
end;

procedure TMapClass<TKey, TValue>.Remove(const AKeys: array of TKey);
var
  i: Integer;
begin
  for i := Low(AKeys) to High(AKeys) do
    Remove(AKeys[i]);
end;

procedure TMapClass<TKey, TValue>.Remove(const AKeys: TEnumerable<TKey>);
var
  Key: TKey;
begin
  for Key in AKeys do
    Remove(Key);
end;

procedure TMapClass<TKey, TValue>.Add(const AValues: array of TPair<TKey, TValue>);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    Add(AValues[i].Key, AValues[i].Value);
end;

procedure TMapClass<TKey, TValue>.Add(const AValues: TEnumerable<TPair<TKey, TValue>>);
var
  Pair: TPair<TKey,TValue>;
begin
  for Pair in AValues do
    Add(Pair.Key, Pair.Value);
end;

procedure TMapClass<TKey, TValue>.AddOrSetValue(const AValues: array of TPair<TKey, TValue>);
var
  i: Integer;
begin
  for i := Low(AValues) to High(AValues) do
    AddOrSetValue(AValues[i].Key, AValues[i].Value);
end;

procedure TMapClass<TKey, TValue>.AddOrSetValue(const AValues: TEnumerable<TPair<TKey, TValue>>);
var
  Pair: TPair<TKey,TValue>;
begin
  for Pair in AValues do
    AddOrSetValue(Pair.Key, Pair.Value);
end;

{ TMultimapClass<TKey, TValue> }

constructor TMultimapClass<TKey, TValue>.Create;
begin
  Create(IEqualityComparer<TKey>(nil));
end;

constructor TMultimapClass<TKey, TValue>.Create(AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultEqualityComparer<TKey>;
  FCount := TDictionary<TKey, integer>.Create(AComparer);
  FValues := TDictionary<TMultimapKey, TValue>.Create(TMultimapKeyEqualityComparer.Create(AComparer));
end;

constructor TMultimapClass<TKey, TValue>.Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; AComparer: IEqualityComparer<TKey> = nil);
begin
  Create(AComparer);
  Add(ACollection);
end;

destructor TMultimapClass<TKey, TValue>.Destroy;
begin
  FreeAndNil(FCount);
  FreeAndNil(FValues);
  FreeAndNil(FKeyCollection);
  inherited;
end;

function TMultimapClass<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  result := GetEnumerator;
end;

function TMultimapClass<TKey, TValue>.Empty: boolean;
begin
  result := FValues.Count=0;
end;

function TMultimapClass<TKey, TValue>.GetEnumerator: TPairEnumerator;
begin
  Result := TPairEnumerator.Create(Self);
end;

function TMultimapClass<TKey, TValue>.GetKeys: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(FCount);
  Result := FKeyCollection;
end;

function TMultimapClass<TKey, TValue>.GetTotalValuesCount: integer;
begin
  result := FValues.Count;
end;

procedure TMultimapClass<TKey, TValue>.Clear;
begin
  FCount.Clear;
  FValues.Clear;
end;

function TMultimapClass<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  result := GetValuesCount(AKey)>0;
end;

function TMultimapClass<TKey, TValue>.ContainsKeys(const AKeys: array of TKey; AContainsCheckType: TContainsCheckType): Boolean;
var
  i: Integer;
begin
  case AContainsCheckType of
    cctAll:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if not ContainsKey(AKeys[i]) then
            Exit(False);
        result := True;
      end;
    cctAnyOf:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if ContainsKey(AKeys[i]) then
            Exit(True);
        result := False;
      end;
    else
      raise exception.Create('');
  end;
end;

function TMultimapClass<TKey, TValue>.ContainsKeys(const AKeys: TEnumerable<TKey>; AContainsCheckType: TContainsCheckType): Boolean;
var
  Key: TKey;
begin
  case AContainsCheckType of
    cctAll:
      begin
        for Key in AKeys do
          if not ContainsKey(Key) then
            Exit(False);
        result := True;
      end;
    cctAnyOf:
      begin
        for Key in AKeys do
          if ContainsKey(Key) then
            Exit(True);
        result := False;
      end;
    else
      raise exception.Create('');
  end;
end;

function TMultimapClass<TKey, TValue>.ContainsValue(const AKey: TKey; const AValue: TValue; AComparer: IEqualityComparer<TValue> = nil): Boolean;
var
  V: TValue;
begin
  if AComparer=nil then
    AComparer := TComparerUtils.DefaultEqualityComparer<TValue>;
  for V in Values[AKey] do
    if AComparer.Equals(AValue, V) then
      Exit(True);
  result := False;
end;

function TMultimapClass<TKey, TValue>.ContainsValues(const AKey: TKey; const AValues: TArray<TValue>; AContainsCheckType: TContainsCheckType;
  AComparer: IEqualityComparer<TValue>): Boolean;
var
  ValueSet: TSetClass<TValue>;
  i: Integer;
begin
  if AComparer=nil then
    AComparer := TComparerUtils.DefaultEqualityComparer<TValue>;
  ValueSet := TSetClass<TValue>.Create(0, AComparer);
  try
    case AContainsCheckType of
      cctAll:
        begin
          for i := Low(AValues) to High(AValues) do
            if not ValueSet.Contains(AValues[i]) then
              Exit(False);
          result := True;
        end;
      cctAnyOf:
        begin
          for i := Low(AValues) to High(AValues) do
            if ValueSet.Contains(AValues[i]) then
              Exit(True);
          result := False;
        end;
      else
        raise exception.Create('');
    end;
  finally
    FreeAndNil(ValueSet);
  end;
end;

function TMultimapClass<TKey, TValue>.ContainsValues(const AKey: TKey; const AValues: TEnumerable<TValue>; AContainsCheckType: TContainsCheckType;
  AComparer: IEqualityComparer<TValue>): Boolean;
var
  ValueSet: TSetClass<TValue>;
  V: TValue;
begin
  if AComparer=nil then
    AComparer := TComparerUtils.DefaultEqualityComparer<TValue>;
  ValueSet := TSetClass<TValue>.Create(0, AComparer);
  try
    case AContainsCheckType of
      cctAll:
        begin
          for V in AValues do
            if not ValueSet.Contains(V) then
              Exit(False);
          result := True;
        end;
      cctAnyOf:
        begin
          for V in AValues do
            if ValueSet.Contains(V) then
              Exit(True);
          result := False;
        end;
      else
        raise exception.Create('');
    end;
  finally
    FreeAndNil(ValueSet);
  end;
end;

function TMultimapClass<TKey, TValue>.GetValuesCount(const AKey: TKey): Integer;
begin
  if not FCount.TryGetValue(AKey, result) then
    result := 0;
end;

procedure TMultimapClass<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
var
  MKey: TMultimapKey;
begin
  MKey := Default(TMultimapKey);
  MKey.Key := AKey;
  if not FCount.TryGetValue(AKey, MKey.Number) then
    MKey.Number := 0;
  FValues.Add(MKey, AValue);
  inc(MKey.Number);
  FCount.AddOrSetValue(AKey, MKey.Number);
end;

procedure TMultimapClass<TKey, TValue>.Add(const AKey: TKey; const AValues: TArray<TValue>);
var
  MKey: TMultimapKey;
  i: Integer;
begin
  MKey := Default(TMultimapKey);
  MKey.Key := AKey;
  if not FCount.TryGetValue(AKey, MKey.Number) then
    MKey.Number := 0;
  for i := Low(AValues) to High(AValues) do
  begin
    FValues.Add(MKey, AValues[i]);
    inc(MKey.Number);
  end;
  FCount.AddOrSetValue(AKey, MKey.Number);
end;

procedure TMultimapClass<TKey, TValue>.Add(const AKey: TKey; const AValues: TEnumerable<TValue>);
var
  item: TValue;
begin
  for item in AValues do
    Add(AKey, item);
end;

procedure TMultimapClass<TKey, TValue>.Add(const ACollection: TEnumerable<TPair<TKey,TValue>>);
var
  item: TPair<TKey,TValue>;
begin
  for item in ACollection do
    Add(item.Key, item.Value);
end;

function TMultimapClass<TKey, TValue>.Remove(const AKey: TKey): Boolean;
var
  MKey: TMultimapKey;
begin
  MKey := Default(TMultimapKey);
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

procedure TMultimapClass<TKey, TValue>.RemoveValue(const AEnum: TValueEnumerator);
var
  LastKey: TMultimapKey;
  LastValue: TValue;
begin
  LastKey := Default(TMultimapKey);
  if not FValues.ContainsKey(AEnum.FMultimapKey) or not FCount.TryGetValue(AEnum.Key, LastKey.Number) then
    raise Exception.Create('Error');
  dec(LastKey.Number);
  LastKey.Key := AEnum.Key;
  if not FValues.TryGetValue(LastKey, LastValue) then
    raise Exception.Create('Error');
  FValues.AddOrSetValue(AEnum.FMultimapKey, LastValue);
  FValues.Remove(LastKey);
  if LastKey.Number=0 then
    FCount.Remove(AEnum.Key)
  else
    FCount.AddOrSetValue(AEnum.Key, LastKey.Number);
end;

procedure TMultimapClass<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TArray<TValue>);
var
  S: TSetClass<TValue>;
  E: TValueEnumerator;
begin
  S := TSetClass<TValue>.Create(AValues);
  try
    E := Values[AKey];
    while E.MoveNext do
      if S.Contains(E.Current) then
        RemoveValue(E);
  finally
    FReeAndNil(S);
  end;
end;

procedure TMultimapClass<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>);
var
  s: TSetClass<TValue>;
begin
  s := TSetClass<TValue>.Create(AValues);
  try
    RemoveValues(AKey, s);
  finally
    FReeAndNil(s);
  end;
end;

function TMultimapClass<TKey, TValue>.GetValuesEnumerator(const AKey: TKey): TValueEnumerator;
begin
  result.Init(Self, AKey);
end;

{ TMultimapClass<TKey, TValue>.TValueEnumerator }

procedure TMultimapClass<TKey, TValue>.TValueEnumerator.Init(AMultimap: TMultimapClass<TKey, TValue>; const AKey: TKey);
begin
  Self := Default(TValueEnumerator);
  FMultimap := AMultimap;
  FMultimapKey := Default(TMultimapKey);
  FMultimapKey.Key := AKey;
  if not FMultimap.FCount.TryGetValue(AKey, FMultimapKey.Number) then
    FMultimapKey.Number := -1;
end;

function TMultimapClass<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  result := FMultimapKey.Number>0;
  if result then
    dec(FMultimapKey.Number);
end;

function TMultimapClass<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  if not FMultimap.FValues.TryGetValue(FMultimapKey, result) then
    raise Exception.Create('Error');
end;

function TMultimapClass<TKey, TValue>.TValueEnumerator.GetEnumerator: TValueEnumerator;
begin
  result.FMultimap := FMultimap;
  result.FMultimapKey := FMultimapKey;
end;

function TMultimapClass<TKey, TValue>.TValueEnumerator.GetKey: TKey;
begin
  result := FMultimapKey.Key;
end;

{ TMultimapClass<TKey, TValue>.TMultimapKeyEqualityComparer }

constructor TMultimapClass<TKey, TValue>.TMultimapKeyEqualityComparer.Create(AKeyComparer: IEqualityComparer<TKey>);
begin
  FKeyComparer := AKeyComparer;
  if FKeyComparer=nil then
    FKeyComparer := TComparerUtils.DefaultEqualityComparer<TKey>;
end;

function TMultimapClass<TKey, TValue>.TMultimapKeyEqualityComparer.Equals(const Left,
  Right: TMultimapKey): Boolean;
begin
  result := (Left.Number=Right.Number) and FKeyComparer.Equals(Left.Key, Right.Key);
end;

function TMultimapClass<TKey, TValue>.TMultimapKeyEqualityComparer.GetHashCode(
  const Value: TMultimapKey): Integer;
begin
  result := TDigests.Mix(FKeyComparer.GetHashCode(Value.Key), Value.Number);
end;

{ TMultimapClass<TKey, TValue>.TPairEnumerator }

constructor TMultimapClass<TKey, TValue>.TPairEnumerator.Create(
  const AMultimap: TMultimapClass<TKey, TValue>);
begin
  inherited Create;
  FMultimap := AMultimap;
  FCurrentKey := FMultimap.FCount.Keys.GetEnumerator;
end;

function TMultimapClass<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  if not FInEnumKey then
  begin
    result := FCurrentKey.MoveNext;
    if not result then
      Exit;
    FCurrentValue := FMultimap.Values[FCurrentKey.Current];
    result := FCurrentValue.MoveNext;
    Assert(result); // if key is here, then there must be at least one value
    FInEnumKey := True;
    Exit;
  end;
  result := FCurrentValue.MoveNext;
  if not result then
  begin
    FInEnumKey := False;
    result := MoveNext;
  end;
end;

function TMultimapClass<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  result.Key := FCurrentKey.Current;
  result.Value := FCurrentValue.Current;
end;

{ TMap<TKey, TValue> }

procedure TMap<TKey, TValue>.Init;
begin
  Self := Default(TMap<TKey, TValue>);
end;

procedure TMap<TKey, TValue>.Init(ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  Self := Default(TMap<TKey, TValue>);
  CreateMap(ACapacity, AComparer);
end;

procedure TMap<TKey, TValue>.Init(const V: array of TPair<TKey, TValue>; ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  Self := Default(TMap<TKey, TValue>);
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

procedure TMap<TKey, TValue>.Init(const V: TEnumerable<TPair<TKey, TValue>>; ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  Self := Default(TMap<TKey, TValue>);
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

procedure TMap<TKey, TValue>.Init(V: TMap<TKey, TValue>; ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  Self := Default(TMap<TKey, TValue>);
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

procedure TMap<TKey, TValue>.CreateMap(ACapacity: integer; AComparer: IEqualityComparer<TKey>);
var
  C: IEqualityComparer<TKey>;
begin
  if AComparer=nil
    then C := TComparerUtils.DefaultEqualityComparer<TKey>
    else C := AComparer;
  FMapInt := TInterfacedObject<TMapClass<TKey,TValue>>.Create( TMapClass<TKey,TValue>.Create(ACapacity, C) );
end;

function TMap<TKey, TValue>.GetReadonly: TMapClass<TKey, TValue>;
begin
  if FMapInt=nil then
    CreateMap;
  result := FMapInt.Data;
end;

function TMap<TKey, TValue>.GetReadWrite: TMapClass<TKey, TValue>;
var
  SrcMapInt: IInterfacedObject<TMapClass<TKey,TValue>>;
begin
  if FMapInt=nil then
    CreateMap
  else
    if FMapInt.GetRefCount<>1 then
    begin
      { Copy on write }
      SrcMapInt := FMapInt;
      CreateMap(SrcMapInt.Data.Count, SrcMapInt.Data.Comparer);
      FMapInt.Data.Add(SrcMapInt.Data);
      FMapInt.Data.OwnsKeys   := FMapInt.Data.OwnsKeys;
      FMapInt.Data.OwnsValues := FMapInt.Data.OwnsValues;
    end;
  result := FMapInt.Data;
end;

function TMap<TKey, TValue>.GetEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TMap<TKey, TValue>.GetEnumerator: TPairEnumerator;
begin
  result := Readonly.GetEnumerator;
end;

function TMap<TKey, TValue>.GetKeys: TKeyCollection;
begin
  result := ReadOnly.Keys;
end;

function TMap<TKey, TValue>.GetValues: TValueCollection;
begin
  result := ReadOnly.Values;
end;

procedure TMap<TKey, TValue>.Remove(const V: TKey);
begin
  ReadWrite.Remove(V);
end;

procedure TMap<TKey, TValue>.Remove(const V: TEnumerable<TKey>);
begin
  ReadWrite.Remove(V);
end;

procedure TMap<TKey, TValue>.Remove(const V: array of TKey);
begin
  ReadWrite.Remove(V);
end;

procedure TMap<TKey, TValue>.Add(const V: array of TPair<TKey, TValue>);
begin
  ReadWrite.Add(V);
end;

procedure TMap<TKey, TValue>.Add(const Pair: TPair<TKey, TValue>);
begin
  ReadWrite.Add(Pair.Key, Pair.Value);
end;

procedure TMap<TKey, TValue>.Add(const Key: TKey; Value: TValue);
begin
  ReadWrite.Add(Key, Value);
end;

procedure TMap<TKey, TValue>.Add(const V: TEnumerable<TPair<TKey, TValue>>);
begin
  ReadWrite.Add(V);
end;

procedure TMap<TKey, TValue>.Add(V: TMap<TKey, TValue>);
begin
  ReadWrite.Add(V.ReadOnly);
end;

procedure TMap<TKey, TValue>.AddOrSetValue(const V: array of TPair<TKey, TValue>);
begin
  ReadWrite.AddOrSetValue(V);
end;

procedure TMap<TKey, TValue>.AddOrSetValue(const Pair: TPair<TKey, TValue>);
begin
  ReadWrite.AddOrSetValue(Pair.Key, Pair.Value);
end;

procedure TMap<TKey, TValue>.AddOrSetValue(const Key: TKey; Value: TValue);
begin
  ReadWrite.AddOrSetValue(Key, Value);
end;

procedure TMap<TKey, TValue>.AddOrSetValue(const V: TEnumerable<TPair<TKey, TValue>>);
begin
  ReadWrite.AddOrSetValue(V);
end;

procedure TMap<TKey, TValue>.AddOrSetValue(V: TMap<TKey, TValue>);
begin
  ReadWrite.AddOrSetValue(V.ReadOnly);
end;

procedure TMap<TKey, TValue>.TrimExcess;
begin
  ReadWrite.TrimExcess;
end;

function TMap<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
begin
  result := ReadOnly.TryGetValue(Key, Value);
end;

function TMap<TKey, TValue>.GetValue(const Key: TKey): TValue;
begin
  if not ReadOnly.TryGetValue(Key, Result) then
    raise Exception.Create('Error');
end;

function TMap<TKey, TValue>.GetValueDef(const Key: TKey): TValue;
begin
  if not ReadOnly.TryGetValue(Key, Result) then
    result := default(TValue);
end;

class operator TMap<TKey, TValue>.Equal(A, B: TMap<TKey, TValue>): Boolean;
var
  Pair: TPair<TKey, TValue>;
  Value: TValue;
  ValueComparer: IComparer<TValue>;
begin
  if A.Count<>B.Count then
    Exit(False);
  ValueComparer := TComparerUtils.DefaultComparer<TValue>;
  for Pair in A do
    if not B.TryGetValue(Pair.Key, Value) or (ValueComparer.Compare(Pair.Value, Value) <> 0) then
      Exit(False);
  Result := True;
end;

class operator TMap<TKey, TValue>.NotEqual(A, B: TMap<TKey, TValue>): Boolean;
begin
  result := not (A=B);
end;

function TMap<TKey, TValue>.ExtractPair(const Key: TKey): TPair<TKey, TValue>;
begin
  result := ReadWrite.ExtractPair(Key);
end;

procedure TMap<TKey, TValue>.Clear;
begin
  ReadWrite.Clear;
end;

procedure TMap<TKey, TValue>.Release;
begin
  FMapInt := nil;
end;

function TMap<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  result := ReadOnly.ContainsKey(Key);
end;

function TMap<TKey, TValue>.Copy: TMap<TKey, TValue>;
begin
  if FMapInt=nil then
    result.Init
  else
  begin
    result.Init(Count, FMapInt.Data.Comparer);
    result.Add(Self);
  end;
end;

function TMap<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
begin
  result := ReadOnly.ToArray;
end;

function TMap<TKey, TValue>.ToString: string;
begin
  result := ReadOnly.ToString;
end;

function TMap<TKey, TValue>.ToText(const Delimiter: string = #13#10): string;
begin
  result := ReadOnly.ToText(Delimiter);
end;

function TMap<TKey, TValue>.GetCollection: TEnumerable<TPair<TKey, TValue>>;
begin
  result := Readonly;
end;

function TMap<TKey, TValue>.GetCount: integer;
begin
  result := ReadOnly.Count;
end;

function TMap<TKey, TValue>.GetItem(const Key: TKey): TValue;
begin
  result := ReadOnly[Key];
end;

procedure TMap<TKey, TValue>.SetItem(const Key: TKey; const Value: TValue);
begin
  ReadWrite[Key] := Value;
end;

function TMap<TKey, TValue>.GetOwnsKeys: boolean;
begin
  result := ReadOnly.OwnsKeys;
end;

function TMap<TKey, TValue>.GetOwnsValues: boolean;
begin
  result := ReadOnly.OwnsValues;
end;

procedure TMap<TKey, TValue>.SetOwnsKeys(const Value: boolean);
begin
  if Value<>ReadOnly.OwnsKeys then
    ReadWrite.OwnsKeys := Value;
end;

procedure TMap<TKey, TValue>.SetOwnsValues(const Value: boolean);
begin
  if Value<>ReadOnly.OwnsValues then
    ReadWrite.OwnsValues := Value;
end;

{ TAATree<TKey, TValue> }

constructor TAATree<TKey, TValue>.Create;
begin
  Create(IComparer<TKey>(nil));
end;

constructor TAATree<TKey, TValue>.Create(AComparer: IComparer<TKey>);
begin
  inherited Create;
  FComparer := AComparer;
  if FComparer=nil then
    FComparer := TComparerUtils.DefaultComparer<TKey>;
  FBottom := AllocNewItem;
  FBottom.Level := 0;
  FBottom.Left := FBottom;
  FBottom.Right := FBottom;
  FDeleted := FBottom;
  FRoot := FBottom;
  FLast := FBottom;
end;

constructor TAATree<TKey, TValue>.Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; AComparer: IComparer<TKey> = nil);
begin
  Create(AComparer);
  Add(ACollection);
end;

destructor TAATree<TKey, TValue>.Destroy;
begin
  Clear;
  ReleaseItem(FBottom);
  FBottom := nil;
  FDeleted := nil;
  FRoot := nil;
  FreeAndNil(FKeyCollection);
  FreeAndNil(FValueCollection);
  inherited;
end;

function TAATree<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  result := TPairEnumerator.Create(Self);
end;

function TAATree<TKey, TValue>.AllocNewItem: P_AATreeItem;
begin
  result := AllocMem(SizeOf(T_AATreeItem));
end;

procedure TAATree<TKey, TValue>.ReleaseItem(p: P_AATreeItem);
begin
  if p=nil then
    Exit;
  p.Data.Key := Default(TKey);
  p.Data.Value := Default(TValue);
  FreeMem(p);
end;

procedure TAATree<TKey, TValue>.KeyNotify(const Key: TKey; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, Key, Action);
end;

procedure TAATree<TKey, TValue>.ValueNotify(const Value: TValue; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);
end;

function TAATree<TKey, TValue>.PtrToHandle(p: P_AATreeItem): TItemHandle;
begin
  if (p=FBottom) or (p=nil) then
    Result := -1
  else
    Result := TItemHandle(p);
end;

function TAATree<TKey, TValue>.HandleToPtr(p: TItemHandle): P_AATreeItem;
begin
  if p=-1 then
    result := nil
  else
    result := P_AATreeItem(p);
end;

procedure TAATree<TKey, TValue>.Clear;
begin
  if FCount=0 then
    exit;
  treeClear(FRoot);
  FBottom.Level := 0;
  FBottom.Left := FBottom;
  FBottom.Right := FBottom;
  FDeleted := FBottom;
  FRoot := FBottom;
  FCount := 0;
end;

function TAATree<TKey, TValue>.treeAdd(p, aparent: P_AATreeItem; var Dst: P_AATreeItem): Boolean;
var
  r: integer;
begin
  if Dst = FBottom then
    with p^ do
    begin
      Parent := AParent;
      Left := FBottom;
      Right := FBottom;
      Level := 1;
      Dst := p;
      result := true;
      exit;
    end;
  r := FComparer.Compare(p.Data.Key, Dst.Data.Key);
  if r>0 then
    result := treeAdd(p, Dst, Dst.right)
  else
  if r<0 then
    result := treeAdd(p, Dst, Dst.left)
  else
    result := false;
  if not result then
    exit;
  treeSkew(Dst);
  treeSplit(Dst);
end;

procedure TAATree<TKey, TValue>.treeClear(p: P_AATreeItem);
begin
  if (p=nil) or (p=FBottom) then
    exit;
  KeyNotify(p.Data.Key, TCollectionNotification.cnRemoved);
  ValueNotify(p.Data.Value, TCollectionNotification.cnRemoved);
  treeClear(p.Left);
  treeClear(p.Right);
  releaseItem(p);
end;

function TAATree<TKey, TValue>.treeDelete(x: P_AATreeItem; var t: P_AATreeItem): boolean;
begin
  result := false;
  if (t=nil) or (t=FBottom) then
    exit;

  // search down the tree and set pointers last and deleted
  Flast := t;
  if FComparer.Compare(x.Data.Key, t.Data.Key) < 0 then
    result := treeDelete(x, t.Left)
  else
  begin
    FDeleted := t;
    result := treeDelete(x, t.Right);
  end;

  // At the bottom of the tree we remove the element (if it exists)
  if (t = FLast) and (FDeleted <> FBottom) and (FComparer.Compare(x.Data.Key, FDeleted.Data.Key)=0) then
  begin
    // We copy key, because it is necessary to rebalance the tree and move
    // FDeleted into right position (position for FLast).
    if FLast<>FDeleted then
      FDeleted.Data.Key := FLast.Data.Key;
    t.Right.Parent := t.Parent;
    t := t.Right;
    result := true;
  end
  else
    // On the way back, we rebalance
    if (t.Left.Level < t.Level-1) or (t.Right.Level < t.Level-1) then
    begin
      dec(t.Level);
      if t.Right.Level > t.Level then
        t.right.level := t.level;
      treeSkew(t);
      treeSkew(t.right);
      treeSkew(t.right.right);
      treeSplit(t);
      treeSplit(t.right);
    end;
end;

function TAATree<TKey, TValue>.treeFind(const AKey: TKey): P_AATreeItem;
var n: integer;
begin
  result := FRoot;
  while result<>FBottom do
  begin
    n := FComparer.Compare(AKey, result.Data.Key);
    if n<0 then
      result := result.Left
    else
      if n>0 then
        result := result.Right
      else
        exit;
  end;
end;

function TAATree<TKey, TValue>.treeFullHeight: integer;
begin
  result := treeGetHeight(FRoot);
end;

function TAATree<TKey, TValue>.treeGetHeight(p: P_AATreeItem): integer;
begin
  if p=FBottom then
    result := 0
  else
    result := Max(treeGetHeight(p.Left), treeGetHeight(p.Right)) + 1;
end;

function TAATree<TKey, TValue>.treeMax(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
  begin
    result := p;
    while result.Right <> FBottom do
      result := result.Right;
  end;
end;

function TAATree<TKey, TValue>.treeMin(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
  begin
    result := p;
    while result.Left <> FBottom do
      result := result.Left;
  end;
end;

procedure TAATree<TKey, TValue>.treeMove(Src, Dst: P_AATreeItem);
begin
  Dst.Data := Src.Data;
end;

function TAATree<TKey, TValue>.treePredecessor(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
    if p.Left <> FBottom then
      result := treeMax(p.Left)
    else
    begin
      result := p.Parent;
      while (result<>FBottom) and (p=result.Left) do
      begin
        p := result;
        result := result.Parent;
      end;
    end;
end;

function TAATree<TKey, TValue>.treeSuccessor(p: P_AATreeItem): P_AATreeItem;
begin
  if (p=nil) or (p=FBottom) then
    result := nil
  else
    if p.Right <> FBottom then
      result := treeMin(p.Right)
    else
    begin
      result := p.Parent;
      while (result<>FBottom) and (p=result.Right) do
      begin
        p := result;
        result := result.Parent;
      end;
    end;
end;

function TAATree<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
var
  h: TItemHandle;
begin
  h := Find(Key);
  result := h<>-1;
  if result then
    Value := Values[h];
end;

// replace position of Dst item in the tree with Src item
procedure TAATree<TKey, TValue>.treeReplace(Src, Dst: P_AATreeItem);
begin
  Src.Parent := Dst.Parent;
  Src.Left := Dst.Left;
  Src.Right := Dst.Right;
  Src.Level := Dst.Level;

  // root item has "parent=FBottom"
  // but FBottom.left/right MUST refer to FBottom
  if Src.Parent<>FBottom then
    if Src.Parent.Left=Dst then
      Src.Parent.Left := Src
    else
      Src.Parent.Right := Src;
  Src.Left.Parent := Src;
  Src.Right.Parent := Src;

  if FRoot=Dst then
    FRoot := Src;
end;

{
  Src: 1(p)   Dst:  2(p)
       / \          / \
     2(t) X        Y  1(t)
     / \              / \
    Y   Z            Z   X
}
procedure TAATree<TKey, TValue>.treeSkew(var p: P_AATreeItem);
var
  t: P_AATreeItem;
begin
  if (p.Left.Level = p.Level) then
  begin

    // change Right&Left links
    t := p;
    p := p.left;
    t.left := p.right;
    p.right := t;

    // change Parent links
    p.Parent := t.Parent;
    t.Parent := p;
    t.Left.Parent := t;
  end;
end;

{
  Src: 1(p)     Dst:  2(p)
       / \            /  \
     X  2(t)        1(t)  Z
        /  \        /  \
       Y    Z      X    Y
}
procedure TAATree<TKey, TValue>.treeSplit(var p: P_AATreeItem);
var
  t: P_AATreeItem;
begin
  if (p.Right.Right.Level = p.Level) then
  begin

    // change Right&Left links
    t := p;
    p := p.Right;
    t.Right := p.Left;
    p.Left := t;
    inc(p.Level);

    // change Parent links
    p.Parent := t.Parent;
    t.Parent := p;
    t.Right.Parent := t;
  end;
end;

function TAATree<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue): TItemHandle;
var
  p: P_AATreeItem;
begin
  p := AllocNewItem;
  p.Data.Key := AKey;
  p.Data.Value := AValue;
  if treeAdd(p, FBottom, FRoot) then
  begin
    result := TItemHandle(p);
    inc(FCount);
    KeyNotify(AKey, TCollectionNotification.cnAdded);
    ValueNotify(AValue, TCollectionNotification.cnAdded);
  end
  else
  begin
    ReleaseItem(p);
    result := -1;
  end;
end;

procedure TAATree<TKey, TValue>.Add(const ACollection: TEnumerable<TPair<TKey,TValue>>);
var
  Pair: TPair<TKey,TValue>;
begin
  for Pair in ACollection do
    Add(Pair.Key, Pair.Value);
end;

function TAATree<TKey, TValue>.AddOrSetValue(const AKey: TKey; const AValue: TValue): TItemHandle;
begin
  result := Find(AKey);
  if result=-1 then
    result := Add(AKey, AValue)
  else
    Values[result] := AValue;
end;

procedure TAATree<TKey, TValue>.Delete(AHandle: TItemHandle);
begin
  if AHandle=-1 then
    raise Exception.Create('Error');

  if (P_AATreeItem(AHandle)<>nil) and (P_AATreeItem(AHandle)<>FBottom) then
  begin
    KeyNotify(P_AATreeItem(AHandle).Data.Key, TCollectionNotification.cnRemoved);
    ValueNotify(P_AATreeItem(AHandle).Data.Value, TCollectionNotification.cnRemoved);
  end;

  if not treeDelete(P_AATreeItem(AHandle), FRoot) then
    exit;

  // content of FDeleted has cleaned & replaced with key from FLast
  // now we move FLast into position of FDeleted
  if FLast<>FDeleted then
  begin
    treeMove(FLast, FDeleted);
    treeReplace(FLast, FDeleted);
  end;

  // ItemDeleted for deleted item has called from treeDelete
  // so we must not call it again
  ReleaseItem(FDeleted);
  FDeleted := FBottom;
  FLast := FBottom;

  dec(FCount);
end;

function TAATree<TKey, TValue>.Find(const AKey: TKey): TItemHandle;
begin
  result := PtrToHandle( treeFind(AKey) );
end;

function TAATree<TKey, TValue>.FindMax: TItemHandle;
begin
  result := PtrToHandle( treeMax(FRoot) );
end;

function TAATree<TKey, TValue>.GetRoot(var ARoot: TItemHandle): Boolean;
begin
  ARoot := PtrToHandle(FRoot);
  result := ARoot<>-1
end;

function TAATree<TKey, TValue>.GetLeftChild(AItem: TItemHandle; var AChild: TItemHandle): Boolean;
begin
  result := AItem<>-1;
  if result then
  begin
    AChild := PtrToHandle(HandleToPtr(AItem).Left);
    result := AChild<>-1;
  end;
end;

function TAATree<TKey, TValue>.GetRightChild(AItem: TItemHandle; var AChild: TItemHandle): Boolean;
begin
  result := AItem<>-1;
  if result then
  begin
    AChild := PtrToHandle(HandleToPtr(AItem).Right);
    result := AChild<>-1;
  end;
end;

function TAATree<TKey, TValue>.GetParent(AItem: TItemHandle; var AParent: TItemHandle): Boolean;
begin
  result := AItem<>-1;
  if result then
  begin
    AParent := PtrToHandle(HandleToPtr(AItem).Parent);
    result := AParent<>-1;
  end;
end;

function TAATree<TKey, TValue>.FindMin: TItemHandle;
begin
  result := PtrToHandle( treeMin(FRoot) );
end;

function TAATree<TKey, TValue>.First(var AHandle: TItemHandle): Boolean;
begin
  AHandle := PtrToHandle( treeMin(FRoot) );
  result := AHandle<>-1;
end;

function TAATree<TKey, TValue>.Last(var AHandle: TItemHandle): Boolean;
begin
  AHandle := PtrToHandle( treeMax(FRoot) );
  result := AHandle<>-1;
end;

function TAATree<TKey, TValue>.Prev(var AHandle: TItemHandle): Boolean;
begin
  AHandle := PtrToHandle( treePredecessor(P_AATreeItem(AHandle)) );
  result := AHandle<>-1;
end;

function TAATree<TKey, TValue>.Next(var AHandle: TItemHandle): Boolean;
begin
  AHandle := PtrToHandle( treeSuccessor(P_AATreeItem(AHandle)) );
  result := AHandle<>-1;
end;

function TAATree<TKey, TValue>.GetEnumerator: TPairEnumerator;
begin
  result := TPairEnumerator.Create(Self);
end;

function TAATree<TKey, TValue>.GetKey(AHandle: TItemHandle): TKey;
begin
  result := P_AATreeItem(AHandle).Data.Key;
end;

function TAATree<TKey, TValue>.GetKeyCollection: TKeyCollection;
begin
  if FKeyCollection = nil then
    FKeyCollection := TKeyCollection.Create(Self);
  result := FKeyCollection;
end;

function TAATree<TKey, TValue>.GetValue(AHandle: TItemHandle): TValue;
begin
  if AHandle<>-1 then
    result := P_AATreeItem(AHandle).Data.Value
  else
    raise Exception.Create('Error');
end;

procedure TAATree<TKey, TValue>.SetValue(AHandle: TItemHandle; const AValue: TValue);
begin
  if AHandle<>-1 then
    P_AATreeItem(AHandle).Data.Value := AValue
  else
    raise Exception.Create('Error');
end;

function TAATree<TKey, TValue>.GetPair(AHandle: TItemHandle): TPair<TKEy,TValue>;
begin
  if AHandle<>-1 then
    result := P_AATreeItem(AHandle).Data
  else
    raise Exception.Create('Error');
end;

function TAATree<TKey, TValue>.GetValueByKey(const AKey: TKey): TValue;
begin
  result := Values[Find(AKey)];
end;

function TAATree<TKey, TValue>.GetValueCollection: TValueCollection;
begin
  if FValueCollection = nil then
    FValueCollection := TValueCollection.Create(Self);
  result := FValueCollection;
end;

procedure TAATree<TKey, TValue>.SetValueByKey(const AKey: TKey; const AValue: TValue);
var
  h: TItemHandle;
begin
  h := Find(AKey);
  if h=-1 then
    Add(AKey, AValue)
  else
    Values[h] := AValue;
end;

function TAATree<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  result := Find(Key)<>-1;
end;

function TAATree<TKey, TValue>.ContainsKeys(const AKeys: array of TKey;
  ASearchType: TSearchType): Boolean;
var
  i: Integer;
begin
  case ASearchType of
    stAll:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if not ContainsKey(AKeys[i]) then
            Exit(False);
        Exit(True);
      end;
    stAny:
      begin
        for i := Low(AKeys) to High(AKeys) do
          if ContainsKey(AKeys[i]) then
            Exit(True);
        Exit(False);
      end;
  end;
end;

function TAATree<TKey, TValue>.ContainsKeys(const AKeys: TEnumerable<TKey>;
  ASearchType: TSearchType): Boolean;
var
  Key: TKey;
begin
  case ASearchType of
    stAll:
      begin
        for Key in AKeys do
          if not ContainsKey(Key) then
            Exit(False);
        Exit(True);
      end;
    stAny:
      begin
        for Key in AKeys do
          if ContainsKey(Key) then
            Exit(True);
        Exit(False);
      end;
  end;
end;

function TAATree<TKey, TValue>.ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue> = nil): Boolean;
var
  h: TItemHandle;
begin
  if AEqualityComparer=nil then
    AEqualityComparer := TComparerUtils.DefaultEqualityComparer<TValue>;
  if not First(h) then
    result := False
  else
    repeat
      result := AEqualityComparer.Equals(Value, Values[h]);
    until result or not Next(h);
end;

function TAATree<TKey, TValue>.MinKey: TKey;
begin
  result := Keys[FindMin];
end;

function TAATree<TKey, TValue>.MaxKey: TKey;
begin
  result := Keys[FindMax];
end;

procedure TAATree<TKey, TValue>.Remove(const AKey: TKey);
begin
  Delete(Find(AKey));
end;

procedure TAATree<TKey, TValue>.Remove(const AKeys: TArray<TKey>);
var
  i: Integer;
begin
  for i := Low(AKeys) to High(AKeys) do
    Remove(AKeys[i]);
end;

procedure TAATree<TKey, TValue>.Remove(const AKeys: TEnumerable<TKey>);
var
  Key: TKey;
begin
  for Key in AKeys do
    Remove(Key);
end;

{ TAATree<TKey, TValue>.TPairEnumerator }

constructor TAATree<TKey, TValue>.TPairEnumerator.Create(const ATree: TAATree<TKey, TValue>);
begin
  FTree := ATree;
  FCurrentItem := 0;
end;

function TAATree<TKey, TValue>.TPairEnumerator.DoMoveNext: Boolean;
begin
  if FCurrentItem=0 then
    result := FTree.First(FCurrentItem)
  else
    result := (FCurrentItem<>-1) and FTree.Next(FCurrentItem);
end;

function TAATree<TKey, TValue>.TPairEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  result := FTree.Pairs[FCurrentItem];
end;

{ TAATree<TKey, TValue>.TKeyEnumerator }

constructor TAATree<TKey, TValue>.TKeyEnumerator.Create(const ATree: TAATree<TKey, TValue>);
begin
  FTree := ATree;
  FCurrentItem := 0;
end;

function TAATree<TKey, TValue>.TKeyEnumerator.DoGetCurrent: TKey;
begin
  result := FTree.Keys[FCurrentItem];
end;

function TAATree<TKey, TValue>.TKeyEnumerator.DoMoveNext: Boolean;
begin
  if FCurrentItem=0 then
    result := FTree.First(FCurrentItem)
  else
    result := (FCurrentItem<>-1) and FTree.Next(FCurrentItem);
end;

{ TAATree<TKey, TValue>.TValueEnumerator }

constructor TAATree<TKey, TValue>.TValueEnumerator.Create(const ATree: TAATree<TKey, TValue>);
begin
  FTree := ATree;
  FCurrentItem := 0;
end;

function TAATree<TKey, TValue>.TValueEnumerator.DoGetCurrent: TValue;
begin
  result := FTree.Values[FCurrentItem];
end;

function TAATree<TKey, TValue>.TValueEnumerator.DoMoveNext: Boolean;
begin
  if FCurrentItem=0 then
    result := FTree.First(FCurrentItem)
  else
    result := (FCurrentItem<>-1) and FTree.Next(FCurrentItem);
end;

{ TAATree<TKey, TValue>.TKeyCollection }

constructor TAATree<TKey, TValue>.TKeyCollection.Create(const ATree: TAATree<TKey, TValue>);
begin
  inherited Create;
  FTree := ATree;
end;

function TAATree<TKey, TValue>.TKeyCollection.DoGetEnumerator: TEnumerator<TKey>;
begin
  result := TKeyEnumerator.Create(FTree);
end;

{ TAATree<TKey, TValue>.TValueCollection }

constructor TAATree<TKey, TValue>.TValueCollection.Create(const ATree: TAATree<TKey, TValue>);
begin
  inherited Create;
  FTree := ATree;
end;

function TAATree<TKey, TValue>.TValueCollection.DoGetEnumerator: TEnumerator<TValue>;
begin
  result := TValueEnumerator.Create(FTree);
end;

{ TOrderedMapClass<TKey, TValue> }

procedure TOrderedMapClass<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  FTree.Add(AKey, AValue);
end;

procedure TOrderedMapClass<TKey, TValue>.Add(const ACollection: TEnumerable<TPair<TKey, TValue>>);
begin
  FTree.Add(ACollection);
end;

procedure TOrderedMapClass<TKey, TValue>.AddOrSetValue(const AKey: TKey; const AValue: TValue);
begin
  FTree.AddOrSetValue(AKey, AValue);
end;

procedure TOrderedMapClass<TKey, TValue>.Clear;
begin
  FTree.Clear;
end;

function TOrderedMapClass<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  result := FTree.ContainsKey(Key);
end;

function TOrderedMapClass<TKey, TValue>.ContainsKeys(const AKeys: array of TKey; ASearchType: TSearchType): Boolean;
begin
  result := FTree.ContainsKeys(AKeys, ASearchType);
end;

function TOrderedMapClass<TKey, TValue>.ContainsKeys(const AKeys: TEnumerable<TKey>; ASearchType: TSearchType): Boolean;
begin
  result := FTree.ContainsKeys(AKeys, ASearchType);
end;

function TOrderedMapClass<TKey, TValue>.ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue>): Boolean;
begin
  result := FTree.ContainsValue(Value, AEqualityComparer);
end;

constructor TOrderedMapClass<TKey, TValue>.Create;
begin
  Create(IComparer<TKey>(nil));
end;

constructor TOrderedMapClass<TKey, TValue>.Create(AComparer: IComparer<TKey>);
begin
  inherited Create;
  FTree := TAATree<TKey,TValue>.Create(AComparer);
  FTree.OnKeyNotify := treeOnKeyNotify;
  FTree.OnValueNotify := treeOnValueNotify;
end;

constructor TOrderedMapClass<TKey, TValue>.Create(
  const ACollection: TEnumerable<TPair<TKey, TValue>>;
        AComparer: IComparer<TKey>);
begin
  inherited Create;
  FTree := TAATree<TKey,TValue>.Create(ACollection, AComparer);
  FTree.OnKeyNotify := treeOnKeyNotify;
  FTree.OnValueNotify := treeOnValueNotify;
end;

destructor TOrderedMapClass<TKey, TValue>.Destroy;
begin
  FreeAndNil(FTree);
  inherited;
end;

function TOrderedMapClass<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  result := FTree.DoGetEnumerator;
end;

function TOrderedMapClass<TKey, TValue>.First(var AKey: TKey): Boolean;
var
  h: TItemHandle;
begin
  result := FTree.First(h);
  if result then
    AKey := FTree.Keys[h];
end;

function TOrderedMapClass<TKey, TValue>.GetCount: integer;
begin
  result := FTree.Count;
end;

function TOrderedMapClass<TKey, TValue>.GetItem(const AKey: TKey): TValue;
begin
  result := FTree.Items[AKey];
end;

function TOrderedMapClass<TKey, TValue>.GetKeyCollection: TKeyCollection;
begin
  result := FTree.GetKeyCollection;
end;

function TOrderedMapClass<TKey, TValue>.GetRoot(var Key: TKey): Boolean;
var
  h: TItemHandle;
begin
  result := FTree.GetRoot(h);
  if result then
    Key := FTree.Keys[h];
end;

function TOrderedMapClass<TKey, TValue>.GetTreeHeight: integer;
begin
  result := FTree.TreeHeight;
end;

function TOrderedMapClass<TKey, TValue>.GetValueCollection: TValueCollection;
begin
  result := FTree.GetValueCollection;
end;

function TOrderedMapClass<TKey, TValue>.Last(var AKey: TKey): Boolean;
var
  h: TItemHandle;
begin
  result := FTree.Last(h);
  if result then
    AKey := FTree.Keys[h];
end;

function TOrderedMapClass<TKey, TValue>.GetLeftChild(const AKey: TKey; var AChild: TKey): Boolean;
var
  h: TItemHandle;
begin
  result := FTree.GetLeftChild(FTree.Find(AKey), h);
  if result then
    AChild := FTree.Keys[h];
end;

function TOrderedMapClass<TKey, TValue>.Max: TPair<TKey, TValue>;
begin
  result := FTree.Pairs[FTree.FindMax];
end;

function TOrderedMapClass<TKey, TValue>.Min: TPair<TKey, TValue>;
begin
  result := FTree.Pairs[FTree.FindMin];
end;

function TOrderedMapClass<TKey, TValue>.FindKeyByIndex(Index: integer): TKey;
var
  h: TItemHandle;
begin
  h := FTree.FindMin;
  while Index > 0 do
  begin
    FTree.Next(h);
    dec(Index);
  end;
  result := FTree.Keys[h];
end;

function TOrderedMapClass<TKey, TValue>.Next(const AKey: TKey; var ANewKey: TKey): Boolean;
var
  h: TItemHandle;
begin
  h := FTree.Find(AKey);
  result := FTree.Next(h);
  if result then
    ANewKey := FTree.Keys[h];
end;

function TOrderedMapClass<TKey, TValue>.GetParent(const AKey: TKey; var AParent: TKey): Boolean;
var
  h: TItemHandle;
begin
  result := FTree.GetParent(FTree.Find(AKey), h);
  if result then
    AParent := FTree.Keys[h];
end;

function TOrderedMapClass<TKey, TValue>.Prev(const AKey: TKey; var ANewKey: TKey): Boolean;
var
  h: TItemHandle;
begin
  h := FTree.Find(AKey);
  result := FTree.Prev(h);
  if result then
    ANewKey := FTree.Keys[h];
end;

procedure TOrderedMapClass<TKey, TValue>.Remove(const AKeys: TArray<TKey>);
begin
  FTree.Remove(AKeys);
end;

procedure TOrderedMapClass<TKey, TValue>.Remove(const AKeys: TEnumerable<TKey>);
begin
  FTree.Remove(AKeys);
end;

procedure TOrderedMapClass<TKey, TValue>.Remove(const AKey: TKey);
begin
  FTree.Remove(AKey);
end;

function TOrderedMapClass<TKey, TValue>.GetRightChild(const AKey: TKey; var AChild: TKey): Boolean;
var
  h: TItemHandle;
begin
  result := FTree.GetRightChild(FTree.Find(AKey), h);
  if result then
    AChild := FTree.Keys[h];
end;

procedure TOrderedMapClass<TKey, TValue>.SetItem(const AKey: TKey; const Value: TValue);
begin
  FTree.Values[FTree.Find(AKey)] := Value;
end;

function TOrderedMapClass<TKey, TValue>.TryGetValue(const Key: TKey; out Value: TValue): Boolean;
begin
  result := FTree.TryGetValue(Key, Value);
end;

function TOrderedMapClass<TKey, TValue>.GetOwnsKeys: boolean;
begin
  result := doOwnsKeys in FOwnerships;
end;

function TOrderedMapClass<TKey, TValue>.GetOwnsValues: boolean;
begin
  result := doOwnsValues in FOwnerships;
end;

procedure TOrderedMapClass<TKey, TValue>.SetOwnsKeys(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TKey> then
    raise Exception.Create('Generic type is not a class.');
  if Value
    then include(FOwnerships, doOwnsKeys)
    else exclude(FOwnerships, doOwnsKeys);
end;

procedure TOrderedMapClass<TKey, TValue>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TValue> then
    raise Exception.Create('Generic type is not a class.');
  if Value
    then include(FOwnerships, doOwnsValues)
    else exclude(FOwnerships, doOwnsValues);
end;

procedure TOrderedMapClass<TKey, TValue>.treeOnKeyNotify(Sender: TObject; const Key: TKey; Action: TCollectionNotification);
begin
  if Assigned(FOnKeyNotify) then
    FOnKeyNotify(Self, Key, Action);
  if (Action = TCollectionNotification.cnRemoved) and (doOwnsKeys in FOwnerships) then
    PObject(@Key)^.DisposeOf;
end;

procedure TOrderedMapClass<TKey, TValue>.treeOnValueNotify(Sender: TObject; const Value: TValue; Action: TCollectionNotification);
begin
  if Assigned(FOnValueNotify) then
    FOnValueNotify(Self, Value, Action);
  if (Action = TCollectionNotification.cnRemoved) and (doOwnsValues in FOwnerships) then
    PObject(@Value)^.DisposeOf;
end;

end.
