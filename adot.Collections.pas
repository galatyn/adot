unit adot.Collections;

{ Definition of classes/record types:

  TAuto<T: class> = record
    Class wrapper. Inner object created automaticaly "on demand" and destroyed when wrapper goes out of scope.

  TAutoFree<T: class> = record
    Class wrapper. Inner object will be destroyed automatically when wrapper goes out of scope.

  TAutoFreeCollection = record
    Collection of objects to be destroyed automaticaly when collection goes out of scope.

  TBHeap<TKey, TValue> = record
    Low level heap operations on array.

  TBinaryHeapClass<TKey,TValue> = class
    Binary heap of pairs [Key;Value].

  TBinaryHeapClass<TKey> = class
    Binary heap with key only.

  TBinarySearchTree<TKey,TValue> = Class
    Binary search tree.

  TCache<TKey,TValue> = class
    Based on TDictionary, but automatically deletes data if it take more space than allowed.

  TComparerUtils = class
    Default comparer/equality comparer etc.

  TCompound<TypeA,TypeB,TypeC> = record
    Compound record with three fields. Provides constructor and comparers for use in collections.

  TCompound<TypeA,TypeB> = record
    Compound record with two fields. Provides constructor and comparers for use in collections.

  TCompoundComparer<TypeA,TypeB,TypeC> = class
    Comparer for compound of three fields.

  TCompoundComparer<TypeA,TypeB> = class
    Comparer for compound of two fields.

  TCompoundEqualityComparer<TypeA,TypeB,TypeC> = class
    Equality comparer for compound of three fields.

  TCompoundEqualityComparer<TypeA,TypeB> = class
    Equality comparer for compound of two fields.

  TMap<TKey,TValue> = record
    Class for map. Based on TDictionary and extends it with some features.

  TMapClass<TKey,TValue> = class
    Class for map. Based on TDictionary and extends it with some features.

  TMultimapClass<TKey,TValue> = class
    Multimap class. Supports multiple items sharing same key. Keeps items in efficient way.

  TOrderedMapClass<TKey,TValue> = Class
    Ordered map.

  TRingClass<T> = class
    Cyclic/circular buffer based on array. Add/delete items to head/tail.

  TSet<T> = record
    Record type for set. Support operators for all set operations and copy-on-write.

  TSetClass<TValue> = class
    Generic class for set.

  TVector<T> = record
    Wrapper for TArray<T> (array with Add/Delete functionality).

}
interface

uses
  adot.Types,
  adot.Tools.Rtti,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.TypInfo,
  System.StrUtils,
  System.SysUtils,
  System.Character,
  System.Contnrs,
  System.Math,
  System.Classes;

const
  RecordTypes = [tkInteger, tkChar, tkEnumeration, tkFloat, tkSet,
    tkMethod, tkWChar, tkRecord, tkInt64, tkPointer, tkProcedure];

type

  {  All objects placed in TAutoFreeCollection will be destroyed automatically
     when collection goes out of the scope. Items will be removed in reverse order.
     Example:
     var
       C: TAutoFreeCollection;
       A: TAutoFree<T1>;
       B: TAutoFree<T2>;
     begin
       A := C.Add( T1.Create );     // First adde, last destroyed.
       B := C.Add( T2.Create );     // Last added, first destroyed.
       [do something with A and B]
     End; }
  { Collection of objects to be destroyed automaticaly when collection goes out of scope }
  TAutoFreeCollection = record
  private
    type
      IAutoFreeCollection = interface(IUnknown)
        procedure Add(AObject: TObject);
        function Count: integer;
      end;

      TAutoFreeCollectionImpl = class(TInterfacedObject, IAutoFreeCollection)
      protected
        FList: TObjectList;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Add(AObject: TObject);
        function Count: integer;
      end;

    var
      FGuard: IAutoFreeCollection;

  public
    function Add<T: class>(AObject: T):T;
    procedure Clear;
    procedure Free; { same as clear, but more "compatible" with regular syntax in Delphi }
    function Empty: Boolean;
  end;

  { This generic type implements automatic destroying of inner object.
    It helps to avoid of try-finally sections in the code. Also can be used to destroy
    inner objects of some class (instead of explicit destroying in destructor).
    Example:

    var
      A: T1;
      B: T2;
    begin
      A := T1.Create;
      try
        B := T2.Create;
        try
          [ do something with A and B]
        finally
          B.Free;
        end;
      finally
        A.Free;
      end;
    End;

    can be rewritten as follow:

    var
      A: TAutoFree<T1>;
      B: TAutoFree<T2>;
    begin
      A := T1.Create;
      B := T2.Create;
      [ do something with A.Value and B.Value]
    End; }
  { Class wrapper. Inner object will be destroyed automatically when wrapper goes out of scope }
  TAutoFree<T: class> = record
  private
    type
      TAutoFreeImpl = class(TInterfacedObject, IUnknown)
      protected
        FObject: TObject;
      public
        constructor Create(AObject: TObject);
        destructor Destroy; override;
      end;

    var
      FValue: T;
      FGuard: IUnknown;

    procedure SetValue(const Value: T);
    function GetIsLink: boolean;
  public
    class function Create: TAutoFree<T>; overload; static;
    class function Create(const AValue: T): TAutoFree<T>; overload; static;
    class operator Equal(const ALeft, ARight: TAutoFree<T>): Boolean;
    class operator Equal(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
    class operator NotEqual(const ALeft, ARight: TAutoFree<T>): Boolean;
    class operator NotEqual(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
    class operator Implicit(const AValue: TAutoFree<T>): T;
    class operator Implicit(const AValue: T): TAutoFree<T>;

    procedure Clear;
    procedure Free; { same as clear, but more "compatible" with regular syntax in Delphi }
    procedure SetAsLink(const Value: T); { keep only link (don't free object)}

    property Value: T read FValue write SetValue;
    property IsLink: boolean read GetIsLink;
  end;

  { Main difference from TAutoFree is that TAuto automatically creates instance
    at first request. Class should have at least one supported constructor:
      Create();
      Create(ACapacity: integer);
      Create(AOwner: TComponent);
        It allows to use many classes without explicit creating/destroying:
    var
      Lines: TAuto<TStringList>;
    begin
      Lines.Value.Text := 'Test';
      Lines.Value.SaveToFile('test.txt');
    end; }
  { Class wrapper. Inner object created automaticaly "on demand" and destroyed when wrapper goes out of scope }
  TAuto<T: class> = record
  private
    FValue: T;
    FGuard: IUnknown;

    procedure SetValue(const Value: T);
    function CreateInstance: T;
    function GetValue: T;
  public
    class function Create: TAutoFree<T>; overload; static;
    class function Create(const AValue: T): TAutoFree<T>; overload; static;
    class operator Equal(const ALeft, ARight: TAuto<T>): Boolean;
    class operator Equal(const ALeft: TAuto<T>; const ARight: T): Boolean;
    class operator NotEqual(const ALeft, ARight: TAuto<T>): Boolean;
    class operator NotEqual(const ALeft: TAuto<T>; const ARight: T): Boolean;
    class operator Implicit(const AValue: TAuto<T>): T;
    class operator Implicit(const AValue: T): TAuto<T>;

    procedure Clear;
    procedure Free; {$IFNDEF DEBUG}inline;{$ENDIF} { Alias to "Clear", more compatible with regular syntax. }

    property Value: T read GetValue write SetValue;
  end;

  { Always use TCompoundEqualityComparer.Default for creating of compatible comparer
    It supports efficiently both - record types and managed types.
    There is also TCompoundComparer class.
    Example:
    type
      TKey = TCompound<String, TArray<Integer>>;
    var
      m: TMap<TKey, Double>;
    begin
      m := TMap<TKey, Double>.Create(TCompoundEqualityComparer<String, TArray<Integer>>.Default);
      m.Add(TKey.Create('test', TArrayHelper.Get<integer>([1,2,3])), 3.1415926);
    end; }
  { Compound record with two fields. Provides constructor and comparers for use in collections. }
  TCompound<TypeA,TypeB> = record
    A: TypeA;
    B: TypeB;
    constructor Create(const A: TypeA; const B: TypeB);
  end;

  { Compound record with three fields. Provides constructor and comparers for use in collections. }
  TCompound<TypeA,TypeB,TypeC> = record
    A: TypeA;
    B: TypeB;
    C: TypeC;
    constructor Create(const A: TypeA; const B: TypeB; const C: TypeC);
  end;

  { Equality comparer for compound of two fields. }
  TCompoundEqualityComparer<TypeA,TypeB> = class(TEqualityComparer<TCompound<TypeA,TypeB>>)
  protected
    var
      FComparerA: IEqualityComparer<TypeA>;
      FComparerB: IEqualityComparer<TypeB>;
    class var
      FOrdinalComparer: IEqualityComparer<TCompound<TypeA,TypeB>>;
  public
    class function Default: IEqualityComparer<TCompound<TypeA,TypeB>>; reintroduce;
    constructor Create(
      const ComparerA: IEqualityComparer<TypeA>;
      const ComparerB: IEqualityComparer<TypeB>
    );
    function Equals(const Left, Right: TCompound<TypeA,TypeB>): Boolean; overload; override;
    function GetHashCode(const Value: TCompound<TypeA,TypeB>): Integer; overload; override;
  end;

  { Equality comparer for compound of three fields. }
  TCompoundEqualityComparer<TypeA,TypeB,TypeC> = class(TEqualityComparer<TCompound<TypeA,TypeB,TypeC>>)
  protected
    var
      FComparerA: IEqualityComparer<TypeA>;
      FComparerB: IEqualityComparer<TypeB>;
      FComparerC: IEqualityComparer<TypeC>;
    class var
      FOrdinalComparer: IEqualityComparer<TCompound<TypeA,TypeB,TypeC>>;
  public
    class function Default: IEqualityComparer<TCompound<TypeA,TypeB,TypeC>>; reintroduce;
    constructor Create(
      const ComparerA: IEqualityComparer<TypeA>;
      const ComparerB: IEqualityComparer<TypeB>;
      const ComparerC: IEqualityComparer<TypeC>
    );
    function Equals(const Left, Right: TCompound<TypeA,TypeB,TypeC>): Boolean; overload; override;
    function GetHashCode(const Value: TCompound<TypeA,TypeB,TYpeC>): Integer; overload; override;
  end;

  { Comparer for compound of two fields. }
  TCompoundComparer<TypeA,TypeB> = class(TComparer<TCompound<TypeA,TypeB>>)
  protected
    var
      FComparerA: IComparer<TypeA>;
      FComparerB: IComparer<TypeB>;
    class var
      FOrdinalComparer: IComparer<TCompound<TypeA,TypeB>>;
  public
    class function Default: IComparer<TCompound<TypeA,TypeB>>; reintroduce;
    constructor Create(
      const ComparerA: IComparer<TypeA>;
      const ComparerB: IComparer<TypeB>
    );
    function Compare(const Left, Right: TCompound<TypeA,TypeB>): Integer; override;
  end;

  { Comparer for compound of three fields. }
  TCompoundComparer<TypeA,TypeB,TypeC> = class(TComparer<TCompound<TypeA,TypeB,TypeC>>)
  protected
    var
      FComparerA: IComparer<TypeA>;
      FComparerB: IComparer<TypeB>;
      FComparerC: IComparer<TypeC>;

    class var
      FOrdinalComparer: IComparer<TCompound<TypeA,TypeB,TypeC>>;
  public
    class function Default: IComparer<TCompound<TypeA,TypeB,TypeC>>; reintroduce;
    constructor Create(
      const ComparerA: IComparer<TypeA>;
      const ComparerB: IComparer<TypeB>;
      const ComparerC: IComparer<TypeC>
    );
    function Compare(const Left, Right: TCompound<TypeA,TypeB,TypeC>): Integer; override;
  end;

  TGuidInt = TCompound<TGUID, integer>;

  TSetOp = (soUnion, soIntersection, soDifference, soSymmetricDifference);

  { Generic class for set. }
  TSetClass<TValue> = class(TEnumerable<TValue>)
  public
    type
      TEnumerator = TDictionary<TValue, TEmptyRec>.TKeyEnumerator;

      TSetObjectDictionary<TSetDictKey,TSetDictValue> = class(TDictionary<TSetDictKey,TSetDictValue>)
      protected
        OwnsKeys: boolean;

        procedure KeyNotify(const Key: TSetDictKey; Action: System.Generics.Collections.TCollectionNotification); override;
      end;

  protected
    var
      FSet: TSetObjectDictionary<TValue, TEmptyRec>;
      FComparerCopy: IEqualityComparer<TValue>; { FSet.Comparer is hidden in private section, so we keep copy }

    function GetCount: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function DoGetEnumerator: TEnumerator<TValue>; override;
    function GetComparer: IEqualityComparer<TValue>;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetOwnsValues: boolean;

  public
    constructor Create(ACapacity: integer = 0; const AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AValues: array of TValue; const AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AValues: TEnumerable<TValue>; const AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AOperands: TArray<TSetClass<TValue>>; ASetOp: TSetOp; const AComparer: IEqualityComparer<TValue> = nil); overload;

    destructor Destroy; override;
    function GetEnumerator: TEnumerator; reintroduce;
    procedure Add(const AValue: TValue); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Add(const ASet: array of TValue); overload;
    procedure Add(const AValues: TEnumerable<TValue>); overload;
    procedure IncludeLogicalAnd(const A,B: TSetClass<TValue>);
    procedure IncludeLogicalOr(const A,B: TSetClass<TValue>);
    procedure IncludeLogicalXor(const A,B: TSetClass<TValue>);
    procedure Include(const AValue: TValue); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Include(const ASet: array of TValue); overload;
    procedure Include(const AValues: TEnumerable<TValue>); overload;
    procedure Remove(const AValue: TValue); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Remove(const ASet: array of TValue); overload;
    procedure Remove(const AValues: TEnumerable<TValue>); overload;
    function Contains(const AValue: TValue): boolean; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Contains(const ASet: array of TValue): boolean; overload;
    function Contains(const AValues: TEnumerable<TValue>): boolean; overload;
    procedure Clear; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Empty: Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    property Count: integer read GetCount;
    property Comparer: IEqualityComparer<TValue> read GetComparer;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  {  Example:
      var
        a,b,c: TSet<string>;
        s: string;
      begin
        a := ['Mandag', 'Tirsdag', 'Fredag'];
        b := ['Fredag', 'Lørdag'];
        c := a and b;                           // ['Fredag']
        c := a or b;                            // ['Mandag', 'Tirsdag', 'Fredag', 'Lørdag']
        c := a + b - ['Mandag', 'Tirsdag'];     // ['Fredag', 'Lørdag']
        if a xor b = TSet<string>.Create(['Mandag', 'Tirsdag', 'Lørdag']) then
          [...]
        if (b in a) or ('Fredag' in a) then
          [...]
        if a>b then     // "a" contains all items from "b" and at least one item extra
          [...]
        if a>=b then    // "a" contains all items from "b" and maybe some items extra
          [...]
        for s in c do   // enumerate all values from the set
          [...]
        c := ['En'];
        c.Add(['To', 'Tre']);
        c.Remove('To'); // ['En', 'Tre']
        Assert( ('En' in c) and ('en' in c) ); // default comparer for "string" type is case insensitive
        c := TSet<string>.Create(['En','To'], 0,TStringComparer.Ordinal);
        Assert( ('En' in c) and NOT ('en' in c) ); // now we used case sensitive comparer
      end; }
  { Record type for set. Support operators for all set operations and copy-on-write. }
  TSet<T> = record
  private
    FSetInt: IInterfacedObject<TSetClass<T>>;

    procedure CreateSet(ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil);

    function GetReadonly: TSetClass<T>;
    function GetReadWrite: TSetClass<T>;
    function GetAsString: string;
    function GetAsArray: TArray<T>;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(AOwnsValues: boolean);

    property ReadOnly: TSetClass<T> read GetReadonly;
    property ReadWrite: TSetClass<T> read GetReadWrite;
  public
    type
      TEnumerator = TSetClass<T>.TEnumerator;

    { Record type TSet<T> can be used without constructor, use constructor only if you
      need some customization: set Capacity, provide custom comparer etc. }
    constructor Create(ACapacity: integer; const AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: array of T; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: TEnumerable<T>; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: array of TEnumerable<T>; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(V: TSet<T>; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil); overload;

    function GetEnumerator: TEnumerator;

    procedure Add(const V: T); overload;
    procedure Add(const V: array of T); overload;
    procedure Add(const V: TEnumerable<T>); overload;
    procedure Add(const V: array of TEnumerable<T>); overload;
    procedure Add(V: TSet<T>); overload;

    procedure Remove(const V: T); overload;
    procedure Remove(const V: array of T); overload;
    procedure Remove(const V: TEnumerable<T>); overload;
    procedure Remove(const V: array of TEnumerable<T>); overload;
    procedure Remove(V: TSet<T>); overload;

    { It is prefered to use syntax "Item in SomSet" over "SomeSet.Contains(Item)", but in
      rare situations compiler can be confused and then "Contains" method is the only way to go }
    function Contains(const a: T) : Boolean; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Contains(const a: TEnumerable<T>) : Boolean; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Contains(a: TSet<T>) : Boolean; overload; {$IFNDEF DEBUG}inline;{$ENDIF}

    function Copy: TSet<T>;

    function Count: integer;
    procedure Clear;

    class operator In(const a: T; b: TSet<T>) : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    class operator In(const a: TEnumerable<T>; b: TSet<T>) : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    class operator In(a: TSet<T>; b: TSet<T>) : Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

    class operator Implicit(const a : T) : TSet<T>;
    class operator Implicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Implicit(const a : array of T) : TSet<T>;

    class operator Explicit(const a : T) : TSet<T>;
    class operator Explicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Explicit(const a : array of T) : TSet<T>;

    class operator Add(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator Add(a: TSet<T>; const b: T): TSet<T>;
    class operator Add(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
    class operator Add(a: TSet<T>; const b: array of T): TSet<T>;
    class operator Add(const a: T;              b: TSet<T>): TSet<T>;
    class operator Add(const a: array of T;     b: TSet<T>): TSet<T>;
    class operator Add(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;

    class operator Subtract(a: TSet<T>; const b: T): TSet<T>;
    class operator Subtract(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator Subtract(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
    class operator Subtract(a: TSet<T>; const b: array of T): TSet<T>;
    class operator Subtract(const a: T;              b: TSet<T>): TSet<T>;
    class operator Subtract(const a: array of T;     b: TSet<T>): TSet<T>;
    class operator Subtract(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;

    class operator Equal(a: TSet<T>;       b: TSet<T>) : Boolean;
    class operator Equal(a: TSet<T>; const b: TEnumerable<T>) : Boolean;

    class operator NotEqual(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator NotEqual(a: TSet<T>; const b: TEnumerable<T>) : Boolean;

    class operator GreaterThanOrEqual(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator GreaterThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator GreaterThan(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator GreaterThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator LessThan(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator LessThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator LessThanOrEqual(a: TSet<T>;       b: TSet<T>): Boolean;
    class operator LessThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;

    class operator LogicalAnd(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator LogicalAnd(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;

    class operator LogicalOr(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator LogicalOr(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;

    class operator LogicalXor(a: TSet<T>;       b: TSet<T>): TSet<T>;
    class operator LogicalXor(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;

    property AsString: string read GetAsString;
    property AsArray: TArray<T> read GetAsArray;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  { Class for map. Based on TDictionary and extends it with some features. }
  TMapClass<TKey,TValue> = class(TDictionary<TKey,TValue>)
  protected
    FComparerCopy: IEqualityComparer<TKey>; { FDictionary.Comparer is hidden in private section, so we keep copy }
    FOwnerships: TDictionaryOwnerships;

    procedure KeyNotify(const Key: TKey; Action: System.Generics.Collections.TCollectionNotification); override;
    procedure ValueNotify(const Value: TValue; Action: System.Generics.Collections.TCollectionNotification); override;
    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);
    function GetAsString: string;
    class function EscapeStrVal(const S: string): string; static;

  public
    constructor Create(ACapacity: integer = 0; const AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const AValues: array of TPair<TKey,TValue>; const AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const AValues: TEnumerable<TPair<TKey,TValue>>; const AComparer: IEqualityComparer<TKey> = nil); overload;

    procedure Add(const AValues: array of TPair<TKey,TValue>); overload;
    procedure Add(const AValues: TEnumerable<TPair<TKey,TValue>>); overload;

    procedure AddOrSetValue(const AValues: array of TPair<TKey,TValue>); overload;
    procedure AddOrSetValue(const AValues: TEnumerable<TPair<TKey,TValue>>); overload;

    procedure Remove(const AKeys: array of TKey); overload;
    procedure Remove(const AKeys: TEnumerable<TKey>); overload;

    function Empty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

    property Comparer: IEqualityComparer<TKey> read FComparerCopy;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property AsString:string read GetAsString;
  end;

  { Class for map. Based on TDictionary and extends it with some features. }
  TMap<TKey,TValue> = record
  public
    type
      TPairEnumerator  = TMapClass<TKey,TValue>.TPairEnumerator;
      TKeyEnumerator   = TMapClass<TKey,TValue>.TKeyEnumerator;
      TValueEnumerator = TMapClass<TKey,TValue>.TValueEnumerator;
      TKeyCollection   = TMapClass<TKey,TValue>.TKeyCollection;
      TValueCollection = TMapClass<TKey,TValue>.TValueCollection;

  private
    FMapInt: IInterfacedObject<TMapClass<TKey,TValue>>;

    procedure CreateMap(ACapacity: integer = 0; const AComparer: IEqualityComparer<TKey> = nil);

    function GetReadonly: TMapClass<TKey,TValue>;
    function GetReadWrite: TMapClass<TKey,TValue>;
    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);

    property ReadOnly: TMapClass<TKey,TValue> read GetReadonly;
    property ReadWrite: TMapClass<TKey,TValue> read GetReadWrite;

    function GetKeys: TKeyCollection;
    function GetValues: TValueCollection;
    function GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
    function GetCount: integer;
    function GetAsString: string;

  public

    { Record type TMap<TKey,TValue> can be used without constructor, use constructor only if you
      need some customization: set Capacity, provide custom comparer etc. }
    constructor Create(ACapacity: integer; const AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const V: array of TPair<TKey,TValue>; ACapacity: integer = 0; const AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const V: TEnumerable<TPair<TKey,TValue>>; ACapacity: integer = 0; const AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(V: TMap<TKey,TValue>; ACapacity: integer = 0; const AComparer: IEqualityComparer<TKey> = nil); overload;

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
    function ExtractPair(const Key: TKey): TPair<TKey,TValue>;
    procedure Clear;
    procedure TrimExcess;
    function ContainsKey(const Key: TKey): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property AsString:string read GetAsString;
  end;

  TContainsCheckType = (cctAll, cctAnyOf);

  { Multimap class. Supports multiple items sharing same key. Keeps items in efficient way.}
  TMultimapClass<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
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
      TPair = System.Generics.Collections.TPair<TKey,TValue>;

      {# Standard containers in Delphi use classes for enumerators.
         It is ok when we keep single instance of the class inside, but for multimap we
         will have to keep lot of instances (for every key where items enumerator has requested).
         To avoid of this we use record type instead of class type. }
      TValueEnumerator = record
      private
        FMultimap: TMultimapClass<TKey,TValue>;
        FMultimapKey: TMultimapKey;

        constructor Create(AMultimap: TMultimapClass<TKey,TValue>; const AKey: TKey);
        function GetCurrent: TValue;
        function GetKey: TKey;
      public
        function MoveNext: Boolean;
        property Current: TValue read GetCurrent;
        property Key: TKey read GetKey;

        { Does nothing, only for kind of "syntax compatibility" with classes. }
        procedure Free;

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
    constructor Create(const AComparer: IEqualityComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair>; const AComparer: IEqualityComparer<TKey> = nil); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const AKey: TKey; const AValues: array of TValue); overload;
    procedure Add(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    procedure Add(const ACollection: TEnumerable<TPair>); overload;

    { e := m.Values[Key];
      while e.MoveNext do
        if m.Current=10 then m.RemoveValue(e); }
    function Remove(const AKey: TKey):Boolean;
    procedure RemoveValue(const AEnum: TValueEnumerator);
    procedure RemoveValues(const AKey: TKey; const AValues: TSetClass<TValue>); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: array of TValue); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;

    function ContainsKey(const AKey: TKey): Boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    function ContainsKeys(const AKeys: array of TKey; AContainsCheckType: TContainsCheckType = cctAnyOf): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; AContainsCheckType: TContainsCheckType = cctAnyOf): Boolean; overload;

    function ContainsValue(const AKey: TKey; const AValue: TValue; AComparer: IEqualityComparer<TValue> = nil): Boolean;
    function ContainsValues(const AKey: TKey; const AValues: array of TValue; AContainsCheckType: TContainsCheckType = cctAnyOf;
      AComparer: IEqualityComparer<TValue> = nil): Boolean; overload;
    function ContainsValues(const AKey: TKey; const AValues: TEnumerable<TValue>; AContainsCheckType: TContainsCheckType = cctAnyOf;
      AComparer: IEqualityComparer<TValue> = nil): Boolean; overload;

    { Enumerator. Example:
      p: TPair<string, integer>;
        for p in m do
          [do something] p.Key / p.Value; }
    function GetEnumerator: TPairEnumerator; reintroduce;

    function Empty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

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

  { Wrapper for TArray<T> (array with Add/Delete functionality). }
  TVector<T> = record
  public
    { we define it before other field to make access more efficient }
    Items: TArray<T>;
  private
    FCount: integer;

    procedure SetCount(ACount: integer);
    procedure SetCapacity(ACapacity: integer);
    procedure Grow;
    function GetCapacity: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetItem(ItemIndex: integer): T; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetItem(ItemIndex: integer; const Value: T);
    function GetFirst: T; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetLast: T; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetFirst(const Value: T); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetLast(const Value: T); {$IFNDEF DEBUG}inline;{$ENDIF}

  public
    type
      TEnumerator = record
      private
        Items: TArray<T>;
        Len: integer;
        Pos: integer;

        function GetCurrent: T;
      public
        constructor Create(const Items: TArray<T>; ACount: integer);
        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;

    constructor Create(ACapacity: integer); overload;
    constructor Create(ADst: TArray<T>); overload;

    function Add: integer; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Add(const Value: T): integer; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Add(const Value: TArray<T>); overload;
    procedure Add(const Value: TEnumerable<T>); overload;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(StartIndex,FinishIndex: integer); overload;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    procedure DeleteLast; {$IFNDEF DEBUG}inline;{$ENDIF}
    function ExtractLast: T; {$IFNDEF DEBUG}inline;{$ENDIF}

    { Extract deletes the item, but doesn't free (even if OwnsValues=True) }
    function Extract(ItemIndex: integer): T;
    function ExtractAll: TArray<T>;

    function GetEnumerator: TEnumerator; reintroduce; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Clear; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure TrimExcess; {$IFNDEF DEBUG}inline;{$ENDIF}

    property Count: integer read FCount write SetCount;
    property Length: integer read FCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Elements[ItemIndex: integer]: T read GetItem write SetItem; default;
  end;

  { Low level heap operations on array.}
  TBHeap<TKey, TValue> = record
    class function GetLeft(ParentIdx: integer): integer; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function GetRight(ParentIdx: integer): integer; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function GetParent(ChildIdx: integer): integer; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class procedure MoveUp(var Items: TArray<TPair<TKey,TValue>>; ItemIndex: integer; Comparer: IComparer<TKey>); static;
    class procedure Delete(var Items: TArray<TPair<TKey,TValue>>; ItemIndex,Count: integer; Comparer: IComparer<TKey>); static;
    class function ExtractMin(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>):TPair<TKey,TValue>; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class procedure Build(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>); static;
    class procedure Sort(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>); static;
    class procedure Replace(var Items: TArray<TPair<TKey,TValue>>; ItemIndex,Count: integer;
      Comparer: IComparer<TKey>; const Pair:TPair<TKey,TValue>); static;
  end;

  { Binary heap of pairs [Key;Value]. }
  TBinaryHeapClass<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  public
    type
      TPairsEnumerator = TVector<TPair<TKey,TValue>>.TEnumerator;

      {# Enumerator for heap (we have to inherit from TEnumerator to be comatible with TEnumerable) }
      TBinaryHeapEnumerator = class(TEnumerator<TPair<TKey, TValue>>)
      protected
        PairEnumerator: TPairsEnumerator;

        function DoGetCurrent: TPair<TKey, TValue>; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const PairEnumerator: TPairsEnumerator);
      end;

      {# Enumerator of keys }
      TKeyEnumerator = record
      private
        PairEnumerator: TPairsEnumerator;

        function GetCurrent: TKey;
      public
        constructor Create(const PairEnumerator: TPairsEnumerator);
        function MoveNext: Boolean;
        property Current: TKey read GetCurrent;
      end;

      {# Collection of keys }
      TKeyCollection = record
      private
        PairEnumerator: TPairsEnumerator;
      public
        constructor Create(const PairEnumerator: TPairsEnumerator);
        function GetEnumerator: TKeyEnumerator;
      end;

  protected
    FItems: TVector<TPair<TKey,TValue>>;
    FComparer: IComparer<TKey>;
    FOwnerships: TDictionaryOwnerships;

    function GetCapacity: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetCount: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetCapacity(ACapacity: integer); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetValue(n: integer; const Value: TPair<TKey, TValue>);
    function GetValue(Index: integer): TPair<TKey,TValue>; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetKeyCollection: TKeyCollection;
    function DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>; override;
    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);
    procedure DisposeItem(n: integer);
    procedure DisposeAll;

    { this function is O(N), avoid of use it }
    function Find(const Key: TKey): integer;

  public
    constructor Create(ACapacity: integer = 0; const AComparer: IComparer<TKey> = nil); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; ACapacity: integer = 0; const AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    function Add(const Key: TKey; const Value: TValue): integer; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Add(const Pair: TPair<TKey,TValue>): integer; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Add(const Values: TArray<TPair<TKey,TValue>>); overload;
    procedure Add(const Values: TEnumerable<TPair<TKey,TValue>>); overload;

    function MinValue: TPair<TKey,TValue>; {$IFNDEF DEBUG}inline;{$ENDIF}   { O(1)      }
    function ExtractMin: TPair<TKey,TValue>; {$IFNDEF DEBUG}inline;{$ENDIF} { O(Log(n)) }
    procedure DeleteMin; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Delete(n: integer); {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure Clear; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure TrimExcess; {$IFNDEF DEBUG}inline;{$ENDIF}

    function Empty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Values[n: integer]: TPair<TKey,TValue> read GetValue write SetValue; default;
    property Keys: TKeyCollection read GetKeyCollection;
    property Comparer: IComparer<TKey> read FComparer;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  { Binary heap with key only. }
  TBinaryHeapClass<TKey> = class(TEnumerable<TKey>)
  protected
    type

      {# Enumerator for heap (we have to inherit from TEnumerator to be comatible with TEnumerable) }
      TBinaryHeapEnumerator = class(TEnumerator<TKey>)
      protected
        FPairEnumerator: TEnumerator<TPair<TKey, TEmptyRec>>;

        function DoGetCurrent: TKey; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(const PairEnumerator: TEnumerator<TPair<TKey, TEmptyRec>>);
        destructor Destroy; override;
      end;

    var
      FHeap: TBinaryHeapClass<TKey,TEmptyRec>;

    function DoGetEnumerator: TEnumerator<TKey>; override;
    function GetCapacity: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetCount: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetValue(n: integer): TKey; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetCapacity(const Value: integer); {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetValue(n: integer; const Value: TKey); {$IFNDEF DEBUG}inline;{$ENDIF}

    { this function is O(N), avoid of use it }
    function Find(const Key: TKey): integer; {$IFNDEF DEBUG}inline;{$ENDIF}

  public
    constructor Create(ACapacity: integer = 0; const AComparer: IComparer<TKey> = nil); overload;
    constructor Create(const ACollection: TEnumerable<TKey>; ACapacity: integer = 0; const AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    function Add(const Value: TKey): integer; overload; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Add(const Values: TArray<TKey>); overload;
    procedure Add(const Values: TEnumerable<TKey>); overload;

    function MinValue: TKey; {$IFNDEF DEBUG}inline;{$ENDIF}   { O(1)      }
    function ExtractMin: TKey; {$IFNDEF DEBUG}inline;{$ENDIF} { O(Log(n)) }

    procedure DeleteMin; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure Delete(n: integer); {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure Clear; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure TrimExcess; {$IFNDEF DEBUG}inline;{$ENDIF}
    function Empty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}

    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Values[n: integer]: TKey read GetValue write SetValue; default;
  end;

  { Cyclic/circular buffer based on array. Add/delete items to head/tail. }
  TRingClass<T> = class(TEnumerable<T>)
  protected
    { FValues.Items is inner array, Items is property to access elements of the ring by index }
    FValues: TVector<T>;
    FHead: integer;
    FCount: integer;
    FOwnsValues: Boolean;

    function GetItemFromHead(n: integer): T;
    procedure SetItemFromHead(n: integer; const Value: T);
    function GetItemFromTail(n: integer): T;
    procedure SetItemFromTail(n: integer; const Value: T);
    function GetCapacity: integer;
    procedure SetCapacity(ANewCapacity: integer);
    function GetHead: T;  {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetHead(const Value: T);  {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetTail: T;  {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetTail(const Value: T);  {$IFNDEF DEBUG}inline;{$ENDIF}
    function GetEmpty: boolean; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure SetOwnsValues(const Value: Boolean);
    function DoGetEnumerator: TEnumerator<T>; override;

    type
      {# Enumerator for the ring (we inherit from TEnumerator to be comatible with TEnumerable) }
      TRingEnumerator = class(TEnumerator<T>)
      protected
        [Weak] FRing: TRingClass<T>;
        FPos: integer;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
      public
        constructor Create(ARing: TRingClass<T>);
      end;

  public
    constructor Create(ACapacity: integer);
    destructor Destroy; override;
    procedure Clear;

    { add to head }
    procedure Add(const Value: T); overload;
    procedure Add(const Values: TArray<T>); overload;
    procedure Add(const AValues: TEnumerable<T>); overload;

    { add to tail }
    procedure AddToTail(const Value: T); overload;
    procedure AddToTail(const Values: TArray<T>); overload;
    procedure AddToTail(const AValues: TEnumerable<T>); overload;

    { from tail }
    function Extract: T;  {$IFNDEF DEBUG}inline;{$ENDIF}
    { from head }
    function ExtractHead: T;  {$IFNDEF DEBUG}inline;{$ENDIF}

    procedure Delete; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure DeleteHead;

    property Capacity: integer read GetCapacity write SetCapacity; { should have Count=0 to resize }
    property Count: integer read FCount write FCount;
    property Empty: boolean read GetEmpty;
    property Head: T read GetHead write SetHead;
    property Tail: T read GetTail write SetTail;
    property ItemsFromHead[n: integer]:T read GetItemFromHead write SetItemFromHead; default; { from Head to Tail }
    property ItemsFromTail[n: integer]:T read GetItemFromTail write SetItemFromTail;          { from Tail to Head }
    property OwnsValues: Boolean read FOwnsValues write SetOwnsValues;
  end;

  { Based on TDictionary, but automatically deletes data if it take more space than allowed. }
  TCache<TKey,TValue> = class
  protected
    const
      DefaultSize = 1024*1024;
    var
      Cache: TDictionary<TKey,TValue>;
      Size, MaxSize: longint;
  public
    constructor Create; overload;
    constructor Create(AMaxSize: longint); overload;
    destructor Destroy; override;
    procedure Add(K: TKey; V: TValue; ASize: longint);
    function TryGetValue(K: TKey; var V: TValue):boolean;
  end;

  {# Balanced binary search tree (BST). The performance of an AA tree is equivalent to the performance of a red-black tree.
    http://en.wikipedia.org/wiki/AA_tree }
  TSearchType = (stAll, stAny);
  TItemHandle = NativeInt;
  TAATree<TKey,TValue> = class(TEnumerable<TPair<TKey,TValue>>)
  private
  protected
    type
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

      TKeyCollection = class(TEnumerable<TKey>)
      protected
        [Weak] FTree: TAATree<TKey,TValue>;

        function DoGetEnumerator: TEnumerator<TKey>; override;
      public
        constructor Create(const ATree: TAATree<TKey,TValue>);
      end;

      TValueCollection = class(TEnumerable<TValue>)
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

    function AllocNewItem: P_AATreeItem; {$IFNDEF DEBUG}inline;{$ENDIF}
    procedure ReleaseItem(p: P_AATreeItem);
    function PtrToHandle(p: P_AATreeItem): TItemHandle; {$IFNDEF DEBUG}inline;{$ENDIF}
    function HandleToPtr(p: TItemHandle): P_AATreeItem; {$IFNDEF DEBUG}inline;{$ENDIF}
    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetKeyCollection: TKeyCollection;
    function GetValueCollection: TValueCollection;

    procedure treeSkew(var p: P_AATreeItem);
    procedure treeSplit(var p: P_AATreeItem);
    function treeAdd(p,aparent: P_AATreeItem; var Dst: P_AATreeItem): Boolean;
    function treeGetHeight(p: P_AATreeItem): integer;
    function treeFullHeight: integer; {$IFNDEF DEBUG}inline;{$ENDIF}
    function treeMin(p: P_AATreeItem): P_AATreeItem;
    function treeMax(p: P_AATreeItem): P_AATreeItem;
    function treeSuccessor(p: P_AATreeItem): P_AATreeItem;
    function treePredecessor(p: P_AATreeItem): P_AATreeItem;
    procedure treeClear(p: P_AATreeItem);
    function treeDelete(x: P_AATreeItem; var t: P_AATreeItem): boolean;
    function treeFind(const AKey: TKey): P_AATreeItem;
    procedure treeMove(Src, Dst: P_AATreeItem); {$IFNDEF DEBUG}inline;{$ENDIF}
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
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;
    function GetEnumerator: TPairEnumerator;

    procedure Clear;
    function Add(const AKey: TKey; const AValue: TValue): TItemHandle; overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    function AddOrSetValue(const AKey: TKey; const AValue: TValue): TItemHandle;
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Delete(AHandle: TItemHandle);
    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKeys: array of TKey); overload;
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
  end;

  { Binary search tree. }
  TBinarySearchTree<TKey,TValue> = Class(TAATree<TKey,TValue>);

  { We do not expose handle-related function here. That is why TAATree is slightly
    faster, but TOrderedMapClass has interface similar to TDictionary.
    Usually TMap is preferred over direct access to TAATree. }
  { Ordered map. }
  TOrderedMapClass<TKey,TValue> = Class(TEnumerable<TPair<TKey,TValue>>)
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

    function DoGetEnumerator: TEnumerator<TPair<TKey,TValue>>; override;
    function GetKeyCollection: TKeyCollection;
    function GetValueCollection: TValueCollection;

    function GetCount: integer;
    function GetItem(const AKey: TKey): TValue;
    function GetTreeHeight: integer;
    procedure SetItem(const AKey: TKey; const Value: TValue);

  public
    constructor Create; overload;
    constructor Create(AComparer: IComparer<TKey>); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; const AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure Add(const AKey: TKey; const AValue: TValue); overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;
    procedure AddOrSetValue(const AKey: TKey; const AValue: TValue);
    function TryGetValue(const Key: TKey; out Value: TValue): Boolean;
    procedure Remove(const AKey: TKey); overload;
    procedure Remove(const AKeys: array of TKey); overload;
    procedure Remove(const AKeys: TEnumerable<TKey>); overload;
    function ContainsKey(const Key: TKey): Boolean; overload;
    function ContainsKeys(const AKeys: array of TKey; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsKeys(const AKeys: TEnumerable<TKey>; ASearchType: TSearchType = stAll): Boolean; overload;
    function ContainsValue(const Value: TValue; AEqualityComparer: IEqualityComparer<TValue> = nil): Boolean;
    function Min: TPair<TKey, TValue>;
    function Max: TPair<TKey, TValue>;

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
  end;

  { Default comparer/equality comparer etc. }
  TComparerUtils = class
  public

    { For string type we use case insensitive comparer by default }
    class function DefaultComparer<T>: IComparer<T>; static;
    class function DefaultEqualityComparer<T>: IEqualityComparer<T>; static;

    class function CompoundComparer<A,B>: IComparer<TCompound<A,B>>; overload; static;
    class function CompoundComparer<A,B,C>: IComparer<TCompound<A,B,C>>; overload; static;
    class function CompoundEqualityComparer<A,B>: IEqualityComparer<TCompound<A,B>>; overload;  static;
    class function CompoundEqualityComparer<A,B,C>: IEqualityComparer<TCompound<A,B,C>>; overload;  static;
  end;

implementation

uses
  adot.Tools;

{ TCompound<TypeA, TypeB> }

constructor TCompound<TypeA, TypeB>.Create(const A: TypeA; const B: TypeB);
begin
  Self.A := A;
  Self.B := B;
end;

{ TCompound<TypeA, TypeB, TypeC> }

constructor TCompound<TypeA, TypeB, TypeC>.Create(const A: TypeA; const B: TypeB; const C: TypeC);
begin
  Self.A := A;
  Self.B := B;
  Self.C := C;
end;

{ TCompoundEqualityComparer<TypeA, TypeB> }

constructor TCompoundEqualityComparer<TypeA, TypeB>.Create(
  const ComparerA: IEqualityComparer<TypeA>;
  const ComparerB: IEqualityComparer<TypeB>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  if FComparerA=nil then
    FComparerA := TEqualityComparer<TypeA>.Default;
  if FComparerB=nil then
    FComparerB := TEqualityComparer<TypeB>.Default;
end;

class function TCompoundEqualityComparer<TypeA, TypeB>.Default: IEqualityComparer<TCompound<TypeA, TypeB>>;
var
  A,B: PTypeInfo;
begin
  if FOrdinalComparer=nil then
  begin
    A := TypeInfo(TypeA);
    if (A<>nil) and (A.Kind in RecordTypes) then
    begin
      B := TypeInfo(TypeB);
      if (B<>nil) and (B.Kind in RecordTypes) then
        Exit(inherited);
    end;
    FOrdinalComparer := TCompoundEqualityComparer<TypeA, TypeB>.Create(nil, nil);
  end;
  result := FOrdinalComparer;
end;

function TCompoundEqualityComparer<TypeA, TypeB>.Equals(const Left,Right: TCompound<TypeA, TypeB>): Boolean;
begin
  result :=
    FComparerA.Equals(Left.A, Right.A) and
    FComparerB.Equals(Left.B, Right.B);
end;

function TCompoundEqualityComparer<TypeA, TypeB>.GetHashCode(const Value: TCompound<TypeA, TypeB>): Integer;
begin
  result := THashes.Mix(FComparerA.GetHashCode(Value.A), FComparerB.GetHashCode(Value.B));
end;

{ TCompoundEqualityComparer<TypeA, TypeB, TypeC> }

constructor TCompoundEqualityComparer<TypeA, TypeB, TypeC>.Create(
  const ComparerA: IEqualityComparer<TypeA>;
  const ComparerB: IEqualityComparer<TypeB>;
  const ComparerC: IEqualityComparer<TypeC>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  FComparerC := ComparerC;
  if FComparerA=nil then
    FComparerA := TEqualityComparer<TypeA>.Default;
  if FComparerB=nil then
    FComparerB := TEqualityComparer<TypeB>.Default;
  if FComparerC=nil then
    FComparerC := TEqualityComparer<TypeC>.Default;
end;

class function TCompoundEqualityComparer<TypeA, TypeB, TypeC>.Default: IEqualityComparer<TCompound<TypeA, TypeB, TypeC>>;
var
  A,B,C: PTypeInfo;
begin
  if FOrdinalComparer=nil then
  begin
    A := TypeInfo(TypeA);
    if (A<>nil) and (A.Kind in RecordTypes) then
    begin
      B := TypeInfo(TypeB);
      if (B<>nil) and (B.Kind in RecordTypes) then
      begin
        C := TypeInfo(TypeC);
        if (C<>nil) and (C.Kind in RecordTypes) then
          Exit(inherited);
      end;
    end;
    FOrdinalComparer := TCompoundEqualityComparer<TypeA, TypeB, TypeC>.Create(nil, nil, nil);
  end;
  result := FOrdinalComparer;
end;

function TCompoundEqualityComparer<TypeA, TypeB, TypeC>.Equals(
  const Left,Right: TCompound<TypeA, TypeB, TypeC>): Boolean;
begin
  result :=
    FComparerA.Equals(Left.A, Right.A) and
    FComparerB.Equals(Left.B, Right.B) and
    FComparerC.Equals(Left.C, Right.C);
end;

function TCompoundEqualityComparer<TypeA, TypeB, TypeC>.GetHashCode(
  const Value: TCompound<TypeA, TypeB, TYpeC>): Integer;
begin
  result := THashes.Mix(FComparerA.GetHashCode(Value.A), FComparerB.GetHashCode(Value.B), FComparerC.GetHashCode(Value.C));
end;

{ TCompoundComparer<TypeA, TypeB> }

constructor TCompoundComparer<TypeA, TypeB>.Create(
  const ComparerA: IComparer<TypeA>; const ComparerB: IComparer<TypeB>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  if FComparerA=nil then
    FComparerA := TComparer<TypeA>.Default;
  if FComparerB=nil then
    FComparerB := TComparer<TypeB>.Default;
end;

class function TCompoundComparer<TypeA, TypeB>.Default: IComparer<TCompound<TypeA, TypeB>>;
var
  A,B: PTypeInfo;
begin
  if FOrdinalComparer=nil then
  begin
    A := TypeInfo(TypeA);
    if (A<>nil) and (A.Kind in RecordTypes) then
    begin
      B := TypeInfo(TypeB);
      if (B<>nil) and (B.Kind in RecordTypes) then
        Exit(inherited);
    end;
    FOrdinalComparer := TCompoundComparer<TypeA, TypeB>.Create(nil, nil);
  end;
  result := FOrdinalComparer;
end;

function TCompoundComparer<TypeA, TypeB>.Compare(const Left,Right: TCompound<TypeA, TypeB>): Integer;
begin
  Result := FComparerA.Compare(Left.A, Right.A);
  if Result=0 then
    Result := FComparerB.Compare(Left.B, Right.B);
end;

{ TCompoundComparer<TypeA, TypeB, TypeC> }

constructor TCompoundComparer<TypeA, TypeB, TypeC>.Create(
  const ComparerA: IComparer<TypeA>;
  const ComparerB: IComparer<TypeB>;
  const ComparerC: IComparer<TypeC>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  FComparerC := ComparerC;
  if FComparerA=nil then
    FComparerA := TComparer<TypeA>.Default;
  if FComparerB=nil then
    FComparerB := TComparer<TypeB>.Default;
  if FComparerC=nil then
    FComparerC := TComparer<TypeC>.Default;
end;

class function TCompoundComparer<TypeA, TypeB, TypeC>.Default: IComparer<TCompound<TypeA, TypeB, TypeC>>;
var
  A,B,C: PTypeInfo;
begin
  if FOrdinalComparer=nil then
  begin
    A := TypeInfo(TypeA);
    if (A<>nil) and (A.Kind in RecordTypes) then
    begin
      B := TypeInfo(TypeB);
      if (B<>nil) and (B.Kind in RecordTypes) then
      begin
        C := TypeInfo(TypeC);
        if (C<>nil) and (C.Kind in RecordTypes) then
          Exit(inherited);
      end;
    end;
    FOrdinalComparer := TCompoundComparer<TypeA, TypeB, TypeC>.Create(nil, nil, nil);
  end;
  result := FOrdinalComparer;
end;

function TCompoundComparer<TypeA, TypeB, TypeC>.Compare(
  const Left,Right: TCompound<TypeA, TypeB, TypeC>): Integer;
begin
  result := FComparerA.Compare(Left.A, Right.A);
  if result=0 then
  begin
    result := FComparerB.Compare(Left.B, Right.B);
    if result=0 then
      result := FComparerC.Compare(Left.C, Right.C);
  end;
end;

{ TSetClass<TValue>.TSetObjectDictionary<TSetDictKey, TSetDictValue> }

procedure TSetClass<TValue>.TSetObjectDictionary<TSetDictKey, TSetDictValue>.KeyNotify(
  const Key: TSetDictKey; Action: System.Generics.Collections.TCollectionNotification);
begin
  inherited;
  if OwnsKeys and (Action = cnRemoved) then
    PObject(@Key)^.DisposeOf;
end;

{ TSetClass<TValue> }

constructor TSetClass<TValue>.Create(ACapacity: integer = 0; const AComparer: IEqualityComparer<TValue> = nil);
begin
  inherited Create;
  FSet := TSetObjectDictionary<TValue, TEmptyRec>.Create(ACapacity, AComparer);
  FComparerCopy := AComparer;
end;

constructor TSetClass<TValue>.Create(const AValues: array of TValue; const AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TSetClass<TValue>.Create(const AValues: TEnumerable<TValue>; const AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TSetClass<TValue>.Create(const AOperands: TArray<TSetClass<TValue>>; ASetOp: TSetOp;
  const AComparer: IEqualityComparer<TValue>);
var
  FoundInAll: Boolean;
  Value: TValue;
  I,J: Integer;
  Found: Boolean;
begin
  Create(0, AComparer);
  case ASetOp of

    soUnion:
      for I := 0 to High(AOperands) do
        Include(AOperands[I]);

    soIntersection:
      begin
        if Length(AOperands)=0 then
          Exit;
        J := 0;
        for I := 1 to High(AOperands) do
          if AOperands[I].Count<AOperands[J].Count then
            J := I;
        for Value in AOperands[J] do
        begin
          FoundInAll := True;
          for I := 0 to High(AOperands) do
            if (I<>J) and not AOperands[I].Contains(Value) then
            begin
              FoundInAll := False;
              Break;
            end;
          if FoundInAll then
            Add(Value);
        end;
      end;

    soDifference:
      begin
        if Length(AOperands)>0 then
          Add(AOperands[0]);
        for I := 1 to High(AOperands) do
          Remove(AOperands[I]);
      end;

    soSymmetricDifference:
      for i := 0 to High(AOperands) do
        for Value in AOperands[i] do
        begin
          Found := False;
          for j := 0 to High(AOperands) do
            if (i<>j) and AOperands[j].Contains(Value) then
            begin
              Found := True;
              Break;
            end;
          if not Found then
            Add(Value);
        end;
  end;
end;

destructor TSetClass<TValue>.Destroy;
begin
  FreeAndNil(FSet);
  inherited;
end;

function TSetClass<TValue>.DoGetEnumerator: TEnumerator<TValue>;
begin
  Result := GetEnumerator;
end;

function TSetClass<TValue>.Empty: Boolean;
begin
  result := FSet.Count=0;
end;

function TSetClass<TValue>.GetComparer: IEqualityComparer<TValue>;
begin
  result := FComparerCopy;
end;

function TSetClass<TValue>.GetCount: integer;
begin
  result := FSet.Count;
end;

function TSetClass<TValue>.GetEnumerator: TEnumerator;
begin
  result := FSet.Keys.GetEnumerator;
end;

procedure TSetClass<TValue>.Remove(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Remove(ASet[i]);
end;

procedure TSetClass<TValue>.Remove(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Remove(Item);
end;

function TSetClass<TValue>.GetOwnsValues: boolean;
begin
  result := FSet.OwnsKeys;
end;

procedure TSetClass<TValue>.SetOwnsValues(AOwnsValues: boolean);
begin
  if AOwnsValues and not TRttiUtils.IsInstance<TValue> then
    raise Exception.Create('Generic type is not a class.');
  FSet.OwnsKeys := AOwnsValues;
end;

procedure TSetClass<TValue>.Add(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.Add(AValue, R);
end;

procedure TSetClass<TValue>.Add(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Add(ASet[i]);
end;

procedure TSetClass<TValue>.Add(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Add(Item);
end;

procedure TSetClass<TValue>.IncludeLogicalAnd(const A, B: TSetClass<TValue>);
var
  Value: TValue;
begin
  if A.Count<=B.Count then
  begin
    for Value in A do
      if B.Contains(Value) then
        Include(Value);
  end
  else
  begin
    for Value in B do
      if A.Contains(Value) then
        Include(Value);
  end
end;

procedure TSetClass<TValue>.IncludeLogicalOr(const A, B: TSetClass<TValue>);
var
  Value: TValue;
begin
  for Value in A do
    Include(Value);
  for Value in B do
    Include(Value);
end;

procedure TSetClass<TValue>.IncludeLogicalXor(const A, B: TSetClass<TValue>);
var
  Value: TValue;
begin
  for Value in A do
    if not B.Contains(Value) then
      Include(Value);
  for Value in B do
    if not A.Contains(Value) then
      Include(Value);
end;

procedure TSetClass<TValue>.Include(const AValue: TValue);
var R: TEmptyRec;
begin
  FSet.AddOrSetValue(AValue, R);
end;

procedure TSetClass<TValue>.Include(const ASet: array of TValue);
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    Include(ASet[i]);
end;

procedure TSetClass<TValue>.Include(const AValues: TEnumerable<TValue>);
var
  Item: TValue;
begin
  for Item in AValues do
    Include(Item);
end;

procedure TSetClass<TValue>.Clear;
begin
  FSet.Clear;
end;

function TSetClass<TValue>.Contains(const AValue: TValue): boolean;
begin
  result := FSet.ContainsKey(AValue);
end;

function TSetClass<TValue>.Contains(const ASet: array of TValue): boolean;
var
  i: Integer;
begin
  for i := Low(ASet) to High(ASet) do
    if not Contains(ASet[i]) then
      Exit(False);
  result := True;
end;

function TSetClass<TValue>.Contains(const AValues: TEnumerable<TValue>): boolean;
var
  Item: TValue;
begin
  for Item in AValues do
    if not Contains(Item) then
      Exit(False);
  result := True;
end;

procedure TSetClass<TValue>.Remove(const AValue: TValue);
begin
  FSet.Remove(AValue);
end;

{ TMapClass<TKey, TValue> }

constructor TMapClass<TKey, TValue>.Create(ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(ACapacity, AComparer);
  FComparerCopy := AComparer;
end;

constructor TMapClass<TKey, TValue>.Create(const AValues: array of TPair<TKey, TValue>; const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create(AComparer);
  FComparerCopy := AComparer;
  Add(AValues);
end;

constructor TMapClass<TKey, TValue>.Create(const AValues: TEnumerable<TPair<TKey, TValue>>; const AComparer: IEqualityComparer<TKey>);
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

function TMapClass<TKey, TValue>.GetAsString: string;
var
  Arr: TArray<TPair<TKey, TValue>>;
  KeyComparer: IComparer<TKey>;
  ValueComparer: IComparer<TValue>;
  Pair: TPair<TKey, TValue>;
  i: Integer;
begin
  Result := '';
  Arr := ToArray;
  KeyComparer := TComparer<TKey>.Default;
  ValueComparer := TComparer<TValue>.Default;
  TArray.Sort<TPair<TKey, TValue>>(Arr, TDelegatedComparer<TPair<TKey, TValue>>.Create(
    function (const L,R: TPair<TKey, TValue>): integer
    begin
      result := KeyComparer.Compare(L.Key, R.Key);
      if result=0 then
        result := ValueComparer.Compare(L.Value, R.Value);
    end));
  for Pair in Arr do
    Result := Result +
      IfThen(Result='','',' ') +
      '(' +
      EscapeStrVal(TRttiUtils.ValueAsString<TKey>(Pair.Key)) +
      ', ' +
      EscapeStrVal(TRttiUtils.ValueAsString<TValue>(Pair.Value)) +
      ')';
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
  if Value then
    include(FOwnerships, doOwnsKeys)
  else
    exclude(FOwnerships, doOwnsKeys);
end;

procedure TMapClass<TKey, TValue>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TValue> then
    raise Exception.Create('Generic type is not a class.');
  if Value then
    include(FOwnerships, doOwnsValues)
  else
    exclude(FOwnerships, doOwnsValues);
end;

procedure TMapClass<TKey, TValue>.KeyNotify(const Key: TKey; Action: System.Generics.Collections.TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsKeys in FOwnerships) then
    PObject(@Key)^.DisposeOf;
end;

procedure TMapClass<TKey, TValue>.ValueNotify(const Value: TValue; Action: System.Generics.Collections.TCollectionNotification);
begin
  inherited;
  if (Action = cnRemoved) and (doOwnsValues in FOwnerships) then
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

constructor TMultimapClass<TKey, TValue>.Create(const AComparer: IEqualityComparer<TKey>);
begin
  inherited Create;
  FCount := TDictionary<TKey, integer>.Create(AComparer);
  FValues := TDictionary<TMultimapKey, TValue>.Create(TMultimapKeyEqualityComparer.Create(AComparer));
end;

constructor TMultimapClass<TKey, TValue>.Create(const ACollection: TEnumerable<TPair>; const AComparer: IEqualityComparer<TKey> = nil);
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
    AComparer := TEqualityComparer<TValue>.Default;
  for V in Values[AKey] do
    if AComparer.Equals(AValue, V) then
      Exit(True);
  result := False;
end;

function TMultimapClass<TKey, TValue>.ContainsValues(const AKey: TKey; const AValues: array of TValue; AContainsCheckType: TContainsCheckType;
  AComparer: IEqualityComparer<TValue>): Boolean;
var
  ValueSet: TSetClass<TValue>;
  i: Integer;
begin
  if AComparer=nil then
    AComparer := TEqualityComparer<TValue>.Default;
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
    AComparer := TEqualityComparer<TValue>.Default;
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
  MKey.Key := AKey;
  if not FCount.TryGetValue(AKey, MKey.Number) then
    MKey.Number := 0;
  FValues.Add(MKey, AValue);
  inc(MKey.Number);
  FCount.AddOrSetValue(AKey, MKey.Number);
end;

procedure TMultimapClass<TKey, TValue>.Add(const AKey: TKey; const AValues: array of TValue);
var
  MKey: TMultimapKey;
  i: Integer;
begin
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

procedure TMultimapClass<TKey, TValue>.Add(const ACollection: TEnumerable<TPair>);
var
  item: TPair;
begin
  for item in ACollection do
    Add(item.Key, item.Value);
end;

function TMultimapClass<TKey, TValue>.Remove(const AKey: TKey): Boolean;
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

procedure TMultimapClass<TKey, TValue>.RemoveValue(const AEnum: TValueEnumerator);
var
  LastKey: TMultimapKey;
  LastValue: TValue;
begin
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

procedure TMultimapClass<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: TSetClass<TValue>);
var
  Enum: TValueEnumerator;
begin
  Enum := Values[AKey];
  while Enum.MoveNext do
    if AValues.Contains(Enum.Current) then
      RemoveValue(Enum);
end;

procedure TMultimapClass<TKey, TValue>.RemoveValues(const AKey: TKey; const AValues: array of TValue);
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
  result := TValueEnumerator.Create(Self, AKey);
end;

{ TMultimapClass<TKey, TValue>.TValueEnumerator }

constructor TMultimapClass<TKey, TValue>.TValueEnumerator.Create(AMultimap: TMultimapClass<TKey, TValue>; const AKey: TKey);
begin
  FMultimap := AMultimap;
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

procedure TMultimapClass<TKey, TValue>.TValueEnumerator.Free;
begin
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
    FKeyComparer := TEqualityComparer<TKey>.Default;
end;

function TMultimapClass<TKey, TValue>.TMultimapKeyEqualityComparer.Equals(const Left,
  Right: TMultimapKey): Boolean;
begin
  result := (Left.Number=Right.Number) and FKeyComparer.Equals(Left.Key, Right.Key);
end;

function TMultimapClass<TKey, TValue>.TMultimapKeyEqualityComparer.GetHashCode(
  const Value: TMultimapKey): Integer;
begin
  result := THashes.Mix(FKeyComparer.GetHashCode(Value.Key), Value.Number);
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

{ TSet<T> }

procedure TSet<T>.Add(const V: T);
begin
  ReadWrite.Include(V);
end;

procedure TSet<T>.Add(const V: TEnumerable<T>);
var
  D: TSetClass<T>;
  Value: T;
begin
  D := ReadWrite;
  for Value in V do
    D.Include(Value);
end;

procedure TSet<T>.Add(const V: array of T);
var
  D: TSetClass<T>;
  Value: T;
begin
  D := ReadWrite;
  for Value in V do
    D.Include(Value);
end;

procedure TSet<T>.Add(V: TSet<T>);
var
  S,D: TSetClass<T>;
  Value: T;
begin
  D := ReadWrite;
  S := V.ReadOnly;
  for Value in S do
    D.Include(Value);
end;

procedure TSet<T>.Add(const V: array of TEnumerable<T>);
var
  D: TSetClass<T>;
  Enum: TEnumerable<T>;
  Value: T;
begin
  D := ReadWrite;
  for Enum in V do
    for Value in Enum do
      D.Include(Value);
end;

function TSet<T>.GetEnumerator: TEnumerator;
begin
  result := Readonly.GetEnumerator;
end;

function TSet<T>.GetReadonly:TSetClass<T>;
begin
  if FSetInt=nil then
    CreateSet;
  result := FSetInt.Data;
end;

function TSet<T>.GetReadWrite: TSetClass<T>;
var
  SrcSetInt: IInterfacedObject<TSetClass<T>>;
begin
  if FSetInt=nil then
    CreateSet
  else
    if FSetInt.GetRefCount<>1 then
    begin
      { Copy on write }
      SrcSetInt := FSetInt;
      CreateSet(SrcSetInt.Data.Count, SrcSetInt.Data.Comparer);
      FSetInt.Data.Include(SrcSetInt.Data);
      FSetInt.Data.OwnsValues := SrcSetInt.Data.OwnsValues;
    end;
  result := FSetInt.Data;
end;

procedure TSet<T>.CreateSet(ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil);
var
  C: IEqualityComparer<T>;
begin
  if AComparer=nil then
    C := TComparerUtils.DefaultEqualityComparer<T>
  else
    C := AComparer;
  FSetInt := TInterfacedObject<TSetClass<T>>.Create( TSetClass<T>.Create(ACapacity, C) );
end;

function TSet<T>.GetAsArray: TArray<T>;
var
  S: TSetClass<T>;
  i: Integer;
  Value: T;
begin
  S := ReadOnly;
  SetLength(Result, S.Count);
  i := 0;
  for Value in S do
  begin
    Result[i] := Value;
    inc(i);
  end;
end;

function TSet<T>.GetOwnsValues: boolean;
begin
  result := ReadOnly.OwnsValues;
end;

procedure TSet<T>.SetOwnsValues(AOwnsValues: boolean);
begin
  if AOwnsValues<>OwnsValues then
    ReadWrite.OwnsValues := AOwnsValues;
end;

function TSet<T>.GetAsString: string;
var
  Arr: TArray<T>;
  Value: T;
begin
  Result := '';
  Arr := AsArray;
  TArray.Sort<T>(Arr);
  for Value in Arr do
    Result := Result + IfThen(Result='','',' ') + TRttiUtils.ValueAsString<T>(Value);
end;

function TSet<T>.Contains(const a: T) : Boolean;
begin
  result := ReadOnly.Contains(a);
end;

function TSet<T>.Contains(const a: TEnumerable<T>) : Boolean;
begin
  result := ReadOnly.Contains(a);
end;

function TSet<T>.Contains(a: TSet<T>) : Boolean;
begin
  result := ReadOnly.Contains(a.ReadOnly);
end;

class operator TSet<T>.In(const a: TEnumerable<T>; b: TSet<T>): Boolean;
begin
  result := b.ReadOnly.Contains(a);
end;

class operator TSet<T>.In(const a: T; b: TSet<T>): Boolean;
begin
  result := b.ReadOnly.Contains(a);
end;

class operator TSet<T>.In(a, b: TSet<T>): Boolean;
begin
  result := b.ReadOnly.Contains(a.ReadOnly);
end;

class operator TSet<T>.Implicit(const a: T): TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Implicit(const a: TEnumerable<T>): TSet<T>;
begin
  result.FSetInt := nil;
  if a<>nil then
    result.Add(a);
end;

class operator TSet<T>.Implicit(const a : array of T) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Explicit(const a : T) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Explicit(const a : TEnumerable<T>) : TSet<T>;
begin
  result.FSetInt := nil;
  if a<>nil then
    result.Add(a);
end;

class operator TSet<T>.Explicit(const a : array of T) : TSet<T>;
begin
  result.FSetInt := nil;
  result.Add(a);
end;

class operator TSet<T>.Add(a, b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b.ReadOnly);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.Add(a: TSet<T>; const b: array of T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.Add(const a: T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Include(b.ReadOnly);
end;

class operator TSet<T>.Add(const a: array of T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Include(b.ReadOnly);
end;

class operator TSet<T>.Add(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Include(b.ReadOnly);
end;

procedure TSet<T>.Clear;
begin
  ReadWrite.Clear;
end;

function TSet<T>.Count: integer;
begin
  result := ReadOnly.Count;
end;

constructor TSet<T>.Create(const V: TEnumerable<T>; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

constructor TSet<T>.Create(const V: array of T; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

constructor TSet<T>.Create(ACapacity: integer; const AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
end;

constructor TSet<T>.Create(V: TSet<T>; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

function TSet<T>.Copy: TSet<T>;
begin
  if FSetInt=nil then
    result.FSetInt := nil
  else
  begin
    result := TSet<T>.Create(Count, FSetInt.Data.Comparer);
    result.Add(Self);
  end;
end;

constructor TSet<T>.Create(const V: array of TEnumerable<T>; ACapacity: integer = 0; const AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

procedure TSet<T>.Remove(const V: TEnumerable<T>);
begin
  ReadWrite.Remove(V);
end;

procedure TSet<T>.Remove(const V: array of T);
begin
  ReadWrite.Remove(V);
end;

procedure TSet<T>.Remove(const V: T);
begin
  ReadWrite.Remove(V);
end;

procedure TSet<T>.Remove(V: TSet<T>);
begin
  ReadWrite.Remove(V.ReadOnly);
end;

procedure TSet<T>.Remove(const V: array of TEnumerable<T>);
var
  i: integer;
  D: TSetClass<T>;
begin
  D := ReadWrite;
  for i := 0 to High(v) do
    D.Remove(V[i]);
end;

class operator TSet<T>.Equal(a, b: TSet<T>): Boolean;
var
  S,D: TSetClass<T>;
begin
  S := a.ReadOnly;
  D := b.ReadOnly;
  result := (S.Count=D.Count) and D.Contains(S);
end;

class operator TSet<T>.Equal(a: TSet<T>; const b: TEnumerable<T>): Boolean;
var
  Value: T;
  D: TSetClass<T>;
  N: Integer;
begin
  D := a.ReadOnly;
  N := 0;
  for Value in b do
    if D.Contains(Value) then
      inc(N)
    else
      exit(False);
  result := N=D.Count;
end;

class operator TSet<T>.NotEqual(a, b: TSet<T>): Boolean;
begin
  result := not (a=b);
end;

class operator TSet<T>.NotEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := not (a=b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b);
end;

class operator TSet<T>.Subtract(a, b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b);
end;

class operator TSet<T>.Subtract(a: TSet<T>; const b: array of T): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Remove(b);
end;

class operator TSet<T>.Subtract(const a: T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.Subtract(const a: array of T; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.Subtract(const a: TEnumerable<T>; b: TSet<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a);
  D.Remove(b.ReadOnly);
end;

class operator TSet<T>.GreaterThanOrEqual(a, b: TSet<T>): Boolean;
begin
  result := b in a;
end;

class operator TSet<T>.GreaterThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := b in a;
end;

class operator TSet<T>.GreaterThan(a, b: TSet<T>): Boolean;
begin
  result := (a.Count>b.Count) and (b in a);
end;

class operator TSet<T>.GreaterThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := (a<>b) and (b in a);
end;

class operator TSet<T>.LessThan(a, b: TSet<T>): Boolean;
begin
  result := (a.Count<b.Count) and (a in b);
end;

class operator TSet<T>.LessThan(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := (a<>b) and (a in b);
end;

class operator TSet<T>.LessThanOrEqual(a, b: TSet<T>): Boolean;
begin
  result := a in b;
end;

class operator TSet<T>.LessThanOrEqual(a: TSet<T>; const b: TEnumerable<T>): Boolean;
begin
  result := a in b;
end;

class operator TSet<T>.LogicalAnd(a, b: TSet<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result.ReadWrite.IncludeLogicalAnd(a.ReadOnly, b.ReadOnly);
end;

class operator TSet<T>.LogicalAnd(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  S,R: TSetClass<T>;
  Value: T;
begin
  result.FSetInt := nil;
  S := a.ReadOnly;
  R := result.ReadWrite;
  for Value in b do
    if S.Contains(Value) then
      R.Include(Value);
end;

class operator TSet<T>.LogicalOr(a, b: TSet<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result.ReadWrite.IncludeLogicalOr(a.ReadOnly, b.ReadOnly);
end;

class operator TSet<T>.LogicalOr(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
var
  D: TSetClass<T>;
begin
  result.FSetInt := nil;
  D := result.ReadWrite;
  D.Include(a.ReadOnly);
  D.Include(b);
end;

class operator TSet<T>.LogicalXor(a, b: TSet<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result.ReadWrite.IncludeLogicalXor(a.ReadOnly, b.ReadOnly);
end;

class operator TSet<T>.LogicalXor(a: TSet<T>; const b: TEnumerable<T>): TSet<T>;
begin
  result.FSetInt := nil;
  result := a xor TSet<T>(b);
end;

{ TMap<TKey, TValue> }

constructor TMap<TKey, TValue>.Create(const V: array of TPair<TKey, TValue>; ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

constructor TMap<TKey, TValue>.Create(ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
end;

constructor TMap<TKey, TValue>.Create(V: TMap<TKey, TValue>; ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

constructor TMap<TKey, TValue>.Create(const V: TEnumerable<TPair<TKey, TValue>>; ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

procedure TMap<TKey, TValue>.CreateMap(ACapacity: integer; const AComparer: IEqualityComparer<TKey>);
var
  C: IEqualityComparer<TKey>;
begin
  if AComparer=nil then
    C := TComparerUtils.DefaultEqualityComparer<TKey>
  else
    C := AComparer;
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

function TMap<TKey, TValue>.ExtractPair(const Key: TKey): TPair<TKey, TValue>;
begin
  result := ReadWrite.ExtractPair(Key);
end;

procedure TMap<TKey, TValue>.Clear;
begin
  ReadWrite.Clear;
end;

function TMap<TKey, TValue>.ContainsKey(const Key: TKey): Boolean;
begin
  result := ReadOnly.ContainsKey(Key);
end;

function TMap<TKey, TValue>.Copy: TMap<TKey, TValue>;
begin
  if FMapInt=nil then
    result.FMapInt := nil
  else
  begin
    result := TMap<TKey, TValue>.Create(Count, FMapInt.Data.Comparer);
    result.Add(Self);
  end;
end;

function TMap<TKey, TValue>.ToArray: TArray<TPair<TKey, TValue>>;
begin
  result := ReadOnly.ToArray;
end;

function TMap<TKey, TValue>.GetAsString: string;
begin
  result := ReadOnly.AsString;
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

{ TAutoFreeCollection }

function TAutoFreeCollection.Add<T>(AObject: T): T;
begin
  if FGuard=nil then
    FGuard := TAutoFreeCollectionImpl.Create;
  FGuard.Add(AObject);
  result := AObject;
end;

procedure TAutoFreeCollection.Clear;
begin
  FGuard := nil;
end;

procedure TAutoFreeCollection.Free;
begin
  Clear;
end;

function TAutoFreeCollection.Empty: Boolean;
begin
  result := (FGuard=nil) or (FGuard.Count=0);
end;

{ TAutoFreeCollection.TAutoFreeCollectionImpl }

constructor TAutoFreeCollection.TAutoFreeCollectionImpl.Create;
begin
  inherited Create;
  FList := TObjectList.Create(True);
end;

destructor TAutoFreeCollection.TAutoFreeCollectionImpl.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TAutoFreeCollection.TAutoFreeCollectionImpl.Add(AObject: TObject);
begin
  FList.Add(AObject);
end;

function TAutoFreeCollection.TAutoFreeCollectionImpl.Count: integer;
begin
  result := FList.Count;
end;

{ TAutofree<T>.TAutoFreeImpl }

constructor TAutofree<T>.TAutoFreeImpl.Create(AObject: TObject);
begin
  FObject := AObject;
end;

destructor TAutofree<T>.TAutoFreeImpl.Destroy;
begin
  FreeAndNil(FObject);
  inherited;
end;

{ TAutofree<T> }

class function TAutoFree<T>.Create: TAutoFree<T>;
begin
  result.FValue := nil;
  result.FGuard := nil;
end;

procedure TAutoFree<T>.Clear;
begin
  FValue := nil;
  FGuard := nil;
end;

class function TAutoFree<T>.Create(const AValue: T): TAutoFree<T>;
begin
  result.Value := AValue;
end;

procedure TAutoFree<T>.SetAsLink(const Value: T);
begin
  FGuard := nil;
  FValue := Value;
end;

function TAutoFree<T>.GetIsLink: boolean;
begin
  result := (FGuard=nil) and (FValue<>nil);
end;

procedure TAutofree<T>.SetValue(const Value: T);
begin
  if FValue = Value then
    Exit;
  FValue := Value;
  FGuard := TAutoFreeImpl.Create(FValue);
end;

class operator TAutoFree<T>.Equal(const ALeft, ARight: TAutoFree<T>): Boolean;
begin
  result := ALeft.Value=ARight.Value;
end;

class operator TAutoFree<T>.Equal(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
begin
  result := ALeft.Value=ARight;
end;

procedure TAutoFree<T>.Free;
begin
  Clear;
end;

class operator TAutoFree<T>.Implicit(const AValue: TAutoFree<T>): T;
begin
  result := AValue.Value;
end;

class operator TAutoFree<T>.Implicit(const AValue: T): TAutoFree<T>;
begin
  result.Value := AValue;
end;

class operator TAutoFree<T>.NotEqual(const ALeft, ARight: TAutoFree<T>): Boolean;
begin
  result := ALeft.Value<>ARight.Value;
end;

class operator TAutoFree<T>.NotEqual(const ALeft: TAutoFree<T>; const ARight: T): Boolean;
begin
  result := ALeft.Value<>ARight;
end;

{ TAuto<T> }

procedure TAuto<T>.Clear;
begin
  FValue := nil;
  FGuard := nil;
end;

class function TAuto<T>.Create: TAutoFree<T>;
begin
  result.FValue := nil;
  result.FGuard := nil;
end;

class function TAuto<T>.Create(const AValue: T): TAutoFree<T>;
begin
  result.Value := AValue;
end;

class operator TAuto<T>.Equal(const ALeft: TAuto<T>; const ARight: T): Boolean;
begin
  result := ALeft.Value=ARight;
end;

class operator TAuto<T>.Equal(const ALeft, ARight: TAuto<T>): Boolean;
begin
  result := ALeft.Value=ARight.Value;
end;

procedure TAuto<T>.Free;
begin
  Clear;
end;

function TAuto<T>.CreateInstance: T;
begin
  result := TRttiUtils.CreateInstance<T>;
end;

function TAuto<T>.GetValue: T;
begin
  if FGuard=nil then
    Value := CreateInstance;
  result := FValue;
end;

class operator TAuto<T>.Implicit(const AValue: T): TAuto<T>;
begin
  result.Value := AValue;
end;

class operator TAuto<T>.Implicit(const AValue: TAuto<T>): T;
begin
  result := AValue.Value;
end;

class operator TAuto<T>.NotEqual(const ALeft: TAuto<T>; const ARight: T): Boolean;
begin
  result := ALeft.Value<>ARight;
end;

class operator TAuto<T>.NotEqual(const ALeft, ARight: TAuto<T>): Boolean;
begin
  result := ALeft.Value<>ARight.Value;
end;

procedure TAuto<T>.SetValue(const Value: T);
begin
  if FValue = Value then
    Exit;
  FValue := Value;
  FGuard := TAutofree<T>.TAutoFreeImpl.Create(FValue);
end;

{ TCache<TKey, TValue> }

constructor TCache<TKey, TValue>.Create;
begin
  Create(DefaultSize);
end;

constructor TCache<TKey, TValue>.Create(AMaxSize: Integer);
begin
  MaxSize := AMaxSize;
  Cache := TDictionary<TKey,TValue>.Create;
end;

destructor TCache<TKey, TValue>.Destroy;
begin
  FreeAndNil(Cache);
end;

procedure TCache<TKey, TValue>.Add(K: TKey; V: TValue; ASize: longint);
begin
  if Size>=MaxSize then
  begin
    Size := 0;
    Cache.Clear;
  end;
  Cache.Add(K,V);
  inc(Size, ASize);
end;

function TCache<TKey, TValue>.TryGetValue(K: TKey; var V: TValue): boolean;
begin
  result := Cache.TryGetValue(K,V);
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
    FComparer := TComparer<TKey>.Default;
  FBottom := AllocNewItem;
  FBottom.Level := 0;
  FBottom.Left := FBottom;
  FBottom.Right := FBottom;
  FDeleted := FBottom;
  FRoot := FBottom;
  FLast := FBottom;
end;

constructor TAATree<TKey, TValue>.Create(const ACollection: TEnumerable<TPair<TKey,TValue>>;
  const AComparer: IComparer<TKey> = nil);
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
    AEqualityComparer := TEqualityComparer<TValue>.Default;
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

procedure TAATree<TKey, TValue>.Remove(const AKeys: array of TKey);
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
end;

constructor TOrderedMapClass<TKey, TValue>.Create(
  const ACollection: TEnumerable<TPair<TKey, TValue>>;
  const AComparer: IComparer<TKey>);
begin
  inherited Create;
  FTree := TAATree<TKey,TValue>.Create(ACollection, AComparer);
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

procedure TOrderedMapClass<TKey, TValue>.Remove(const AKeys: array of TKey);
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

{ TComparerUtils }

class function TComparerUtils.CompoundComparer<A, B, C>: IComparer<TCompound<A, B, C>>;
begin
  result := TCompoundComparer<A,B,C>.Default;
end;

class function TComparerUtils.CompoundComparer<A, B>: IComparer<TCompound<A, B>>;
begin
  result := TCompoundComparer<A,B>.Default;
end;

class function TComparerUtils.CompoundEqualityComparer<A, B, C>: IEqualityComparer<TCompound<A, B, C>>;
begin
  result := TCompoundEqualityComparer<A,B,C>.Default;
end;

class function TComparerUtils.CompoundEqualityComparer<A, B>: IEqualityComparer<TCompound<A, B>>;
begin
  result := TCompoundEqualityComparer<A,B>.Default;
end;

class function TComparerUtils.DefaultComparer<T>: IComparer<T>;
begin
  if TypeInfo(T) = TypeInfo(string) then
    result := IComparer<T>( IComparer<string>(TIStringComparer.Ordinal) )
  else
    result := TComparer<T>.Default;
end;

class function TComparerUtils.DefaultEqualityComparer<T>: IEqualityComparer<T>;
begin
  if TypeInfo(T) = TypeInfo(string) then
    result := IEqualityComparer<T>( IEqualityComparer<string>(TIStringComparer.Ordinal) )
  else
    result := TEqualityComparer<T>.Default;
end;

{ TVector<T>.TEnumerator }

constructor TVector<T>.TEnumerator.Create(const Items: TArray<T>; ACount: integer);
begin
  Self.Items := Items;
  Len := ACount;
  Pos := 0;
end;

function TVector<T>.TEnumerator.GetCurrent: T;
begin
  result := Items[Pos-1];
end;

function TVector<T>.TEnumerator.MoveNext: Boolean;
begin
  result := Pos<Len;
  if result then
    inc(Pos);
end;

{ TVector<T> }

constructor TVector<T>.Create(ACapacity: integer);
begin
  Capacity := ACapacity;
end;

constructor TVector<T>.Create(ADst: TArray<T>);
begin
  Items := ADst;
end;

function TVector<T>.Add: integer;
begin
  if Count>=Capacity then
    Grow;
  result := FCount;
  inc(FCount);
end;

function TVector<T>.Add(const Value: T): integer;
begin
  result := Add;
  Items[result] := Value;
end;

procedure TVector<T>.Add(const Value: TArray<T>);
var
  I: Integer;
begin
  I := Count + System.Length(Value);
  if I > Capacity then
    Capacity := I;
  for I := Low(Value) to High(Value) do
    Add(Value[I]);
end;

procedure TVector<T>.Add(const Value: TEnumerable<T>);
var
  V: T;
begin
  for V in Value do
    Add(V);
end;

procedure TVector<T>.Clear;
begin
  SetLength(Items, 0);
  FCount := 0;
end;

procedure TVector<T>.Delete(ItemIndex: integer);
var
  I: Integer;
begin
  for I := ItemIndex to Count-2 do
    Items[I] := Items[I+1];
  Dec(FCount);
  Items[FCount] := Default(T);
end;

procedure TVector<T>.Delete(StartIndex, FinishIndex: integer);
var
  I,C: Integer;
 begin
  C := Count-(FinishIndex-StartIndex+1); { new Count }
  for I := StartIndex to C-1 do
    Items[I] := Items[FinishIndex + (I-StartIndex+1)];
  for I := C to Count-1 do
    Items[I] := Default(T);
  FCount := C;
end;

function TVector<T>.Extract(ItemIndex: integer): T;
begin
  result := Items[ItemIndex];
  Delete(ItemIndex);
end;

function TVector<T>.ExtractAll: TArray<T>;
var
  n: TArray<T>;
begin
  result := Items;
  SetLength(n, 0);
  Items := n;
  FCount := 0;
end;

procedure TVector<T>.DeleteLast;
begin
  Dec(FCount);
  Items[FCount] := Default(T);
end;

function TVector<T>.ExtractLast: T;
begin
  Dec(FCount);
  result := Items[FCount];
  Items[FCount] := Default(T);
end;

procedure TVector<T>.Grow;
begin
  if Capacity<8 then
    Capacity := Capacity+1
  else
  if Capacity<64 then
    Capacity := 64
  else
    Capacity := Capacity * 2;
end;

function TVector<T>.GetCapacity: integer;
begin
  result := System.Length(Items);
end;

procedure TVector<T>.SetCapacity(ACapacity: integer);
begin
  Assert(ACapacity>=Count);
  SetLength(Items, ACapacity);
end;

function TVector<T>.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(Items, Count);
end;

function TVector<T>.GetFirst: T;
begin
  Result := Items[0];
end;

procedure TVector<T>.SetFirst(const Value: T);
begin
  Items[0] := Value;
end;

function TVector<T>.GetLast: T;
begin
  Result := Items[Count-1];
end;

procedure TVector<T>.SetLast(const Value: T);
begin
  Items[Count-1] := Value;
end;

function TVector<T>.GetItem(ItemIndex: integer): T;
begin
  result := Items[ItemIndex];
end;

procedure TVector<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  Items[ItemIndex] := Value;
end;

procedure TVector<T>.TrimExcess;
begin
  Capacity := Count;
end;

procedure TVector<T>.SetCount(ACount: integer);
var
  I: Integer;
begin
  for I := ACount to Count-1 do
    Items[I] := Default(T);
  FCount := ACount;
  if ACount > Capacity then
    Capacity := ACount;
end;

{ TBHeap<TKey, TValue> }

class function TBHeap<TKey, TValue>.GetLeft(ParentIdx: integer): integer;
begin
  result := (ParentIdx shl 1) + 1;
end;

class function TBHeap<TKey, TValue>.GetRight(ParentIdx: integer): integer;
begin
  result := (ParentIdx shl 1) + 2;
end;

class function TBHeap<TKey, TValue>.GetParent(ChildIdx: integer): integer;
begin
  result := (ChildIdx - 1) shr 1;
end;

class procedure TBHeap<TKey, TValue>.MoveUp(var Items: TArray<TPair<TKey, TValue>>;
  ItemIndex: integer; Comparer: IComparer<TKey>);
var
  ParentIndex: Integer;
  Value: TPair<TKey, TValue>;
begin
  while ItemIndex > 0 do
  begin
    ParentIndex := GetParent(ItemIndex);
    if Comparer.Compare(Items[ParentIndex].Key, Items[ItemIndex].Key)<=0 then
      Break;
    Value := Items[ParentIndex];
    Items[ParentIndex] := Items[ItemIndex];
    Items[ItemIndex] := Value;
    ItemIndex := ParentIndex;
  end;
end;

class procedure TBHeap<TKey, TValue>.Replace(var Items: TArray<TPair<TKey, TValue>>; ItemIndex, Count: integer;
  Comparer: IComparer<TKey>; const Pair: TPair<TKey, TValue>);
begin
  Delete(Items, ItemIndex,Count, Comparer);
  Items[Count-1] := Pair;
  MoveUp(Items, Count-1, Comparer);
end;

class procedure TBHeap<TKey, TValue>.Delete(var Items: TArray<TPair<TKey, TValue>>;
  ItemIndex,Count: integer; Comparer: IComparer<TKey>);
var
  L,R: integer;
begin
  repeat
    L := GetLeft(ItemIndex);
    if L >= Count then
      Break;
    R := GetRight(ItemIndex);
    if R >= Count then
    begin
      Items[ItemIndex] := Items[L];
      ItemIndex := L;
      Break;
    end;
    if Comparer.Compare(Items[L].Key, Items[R].Key) < 0 then
    begin
      Items[ItemIndex] := Items[L];
      ItemIndex := L;
    end
    else
    begin
      Items[ItemIndex] := Items[R];
      ItemIndex := R;
    end;
  until False;
  if ItemIndex < Count-1 then
  begin
    Items[ItemIndex] := Items[Count-1];
    MoveUp(Items, ItemIndex, Comparer);
  end;
end;

class function TBHeap<TKey, TValue>.ExtractMin(var Items: TArray<TPair<TKey, TValue>>;
  Count: integer; Comparer: IComparer<TKey>): TPair<TKey, TValue>;
begin
  result := Items[0];
  Delete(Items, 0, Count, Comparer);
end;

class procedure TBHeap<TKey, TValue>.Build(var Items: TArray<TPair<TKey, TValue>>;
  Count: integer; Comparer: IComparer<TKey>);
var
  I: Integer;
begin
  for I := Low(Items)+1 to Count-1 do
    MoveUp(Items, I, Comparer);
end;

class procedure TBHeap<TKey, TValue>.Sort(var Items: TArray<TPair<TKey, TValue>>;
  Count: integer; Comparer: IComparer<TKey>);
var
  I: Integer;
begin
  Build(Items, Count, Comparer);
  for I := Length(Items)-1 downto 0 do
    Items[I] := ExtractMin(Items, I+1, Comparer);
  TArrayUtils.Inverse<TPair<TKey, TValue>>(Items);
end;

{ TBinaryHeapClass<TKey, TValue>.TEnumerator }

constructor TBinaryHeapClass<TKey, TValue>.TBinaryHeapEnumerator.Create(const PairEnumerator: TPairsEnumerator);
begin
  inherited Create;
  Self.PairEnumerator := PairEnumerator;
end;

function TBinaryHeapClass<TKey, TValue>.TBinaryHeapEnumerator.DoGetCurrent: TPair<TKey, TValue>;
begin
  result := PairEnumerator.Current;
end;

function TBinaryHeapClass<TKey, TValue>.TBinaryHeapEnumerator.DoMoveNext: Boolean;
begin
  result := PairEnumerator.MoveNext;
end;

{ TBinaryHeapClass<TKey, TValue>.TKeyEnumerator }

constructor TBinaryHeapClass<TKey, TValue>.TKeyEnumerator.Create(const PairEnumerator: TPairsEnumerator);
begin
  Self.PairEnumerator := PairEnumerator;
end;

function TBinaryHeapClass<TKey, TValue>.TKeyEnumerator.GetCurrent: TKey;
begin
  result := PairEnumerator.Current.Key;
end;

function TBinaryHeapClass<TKey, TValue>.TKeyEnumerator.MoveNext: Boolean;
begin
  result := PairEnumerator.MoveNext;
end;

{ TBinaryHeapClass<TKey, TValue>.TKeyCollection }

constructor TBinaryHeapClass<TKey, TValue>.TKeyCollection.Create(const PairEnumerator: TPairsEnumerator);
begin
  Self.PairEnumerator := PairEnumerator;
end;

function TBinaryHeapClass<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  result := TKeyEnumerator.Create(PairEnumerator);
end;

{ TBinaryHeapClass<TKey, TValue> }

constructor TBinaryHeapClass<TKey, TValue>.Create(ACapacity: integer; const AComparer: IComparer<TKey>);
begin
  inherited Create;
  Capacity := ACapacity;
  if AComparer=nil then
    FComparer := TComparerUtils.DefaultComparer<TKey>
  else
    FComparer := AComparer;
end;

constructor TBinaryHeapClass<TKey, TValue>.Create(const ACollection: TEnumerable<TPair<TKey, TValue>>;
  ACapacity: integer; const AComparer: IComparer<TKey>);
begin
  Create(ACapacity, AComparer);
  Add(ACollection);
end;

destructor TBinaryHeapClass<TKey, TValue>.Destroy;
begin
  Clear;
  FComparer := nil;
  inherited;
end;

procedure TBinaryHeapClass<TKey, TValue>.Clear;
begin
  DisposeAll;
  FItems.Clear;
end;

procedure TBinaryHeapClass<TKey, TValue>.DeleteMin;
begin
  Delete(0);
end;

procedure TBinaryHeapClass<TKey, TValue>.Delete(n: integer);
begin
  DisposeItem(n);
  TBHeap<TKey, TValue>.Delete(FItems.Items, n, Count, FComparer);
  FItems.Delete(Count-1);
end;

function TBinaryHeapClass<TKey, TValue>.Empty: boolean;
begin
  result := FItems.Count=0;
end;

function TBinaryHeapClass<TKey, TValue>.Find(const Key: TKey): integer;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
    if FComparer.Compare(FItems.Items[I].Key, Key)=0 then
      Exit(I);
  result := -1;
end;

function TBinaryHeapClass<TKey, TValue>.GetCapacity: integer;
begin
  result := FItems.Capacity;
end;

function TBinaryHeapClass<TKey, TValue>.GetCount: integer;
begin
  result := FItems.Count;
end;

function TBinaryHeapClass<TKey, TValue>.DoGetEnumerator: TEnumerator<TPair<TKey, TValue>>;
begin
  result := TBinaryHeapEnumerator.Create(FItems.GetEnumerator);
end;

function TBinaryHeapClass<TKey, TValue>.GetKeyCollection: TKeyCollection;
begin
  result := TKeyCollection.Create( FItems.GetEnumerator );
end;

function TBinaryHeapClass<TKey, TValue>.GetOwnsKeys: boolean;
begin
  result := doOwnsKeys in FOwnerships;
end;

function TBinaryHeapClass<TKey, TValue>.GetOwnsValues: boolean;
begin
  result := doOwnsValues in FOwnerships;
end;

procedure TBinaryHeapClass<TKey, TValue>.SetOwnsKeys(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TKey> then
    raise Exception.Create('Generic type is not a class.');
  if Value then
    include(FOwnerships, doOwnsKeys)
  else
    exclude(FOwnerships, doOwnsKeys);
end;

procedure TBinaryHeapClass<TKey, TValue>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TValue> then
    raise Exception.Create('Generic type is not a class.');
  if Value then
    include(FOwnerships, doOwnsValues)
  else
    exclude(FOwnerships, doOwnsValues);
end;

function TBinaryHeapClass<TKey, TValue>.GetValue(Index: integer): TPair<TKey, TValue>;
begin
  result := FItems.Items[Index];
end;

procedure TBinaryHeapClass<TKey, TValue>.SetCapacity(ACapacity: integer);
begin
  FItems.Capacity := ACapacity;
end;

procedure TBinaryHeapClass<TKey, TValue>.DisposeAll;
var
  I: Integer;
begin
  if FOwnerships<>[] then
    for I := 0 to FItems.Count-1 do
    begin
      if doOwnsKeys in FOwnerships then
        PObject(@FItems.Items[I].Key)^.DisposeOf;
      if doOwnsValues in FOwnerships then
        PObject(@FItems.Items[I].Value)^.DisposeOf;
    end;
end;

procedure TBinaryHeapClass<TKey, TValue>.DisposeItem(n: integer);
begin
  if FOwnerships<>[] then
  begin
    if doOwnsKeys in FOwnerships then
      PObject(@FItems.Items[n].Key)^.DisposeOf;
    if doOwnsValues in FOwnerships then
      PObject(@FItems.Items[n].Value)^.DisposeOf;
  end;
end;

procedure TBinaryHeapClass<TKey, TValue>.SetValue(n: integer; const Value: TPair<TKey, TValue>);
begin
  DisposeItem(n);
  TBHeap<TKey, TValue>.Replace(FItems.Items, n, Count, FComparer, Value);
end;

procedure TBinaryHeapClass<TKey, TValue>.TrimExcess;
begin
  FItems.TrimExcess;
end;

function TBinaryHeapClass<TKey, TValue>.Add(const Pair: TPair<TKey, TValue>): integer;
begin
  result := FItems.Add(Pair);
  TBHeap<TKey, TValue>.MoveUp(FItems.Items, result, FComparer);
end;

function TBinaryHeapClass<TKey, TValue>.Add(const Key: TKey; const Value: TValue): integer;
var
  P: TPair<TKey,TValue>;
begin
  P.Key := Key;
  P.Value := Value;
  result := FItems.Add(P);
  TBHeap<TKey, TValue>.MoveUp(FItems.Items, result, FComparer);
end;

procedure TBinaryHeapClass<TKey, TValue>.Add(const Values: TEnumerable<TPair<TKey, TValue>>);
var
  Pair: TPair<TKey, TValue>;
  I: Integer;
begin
  for Pair in Values do
  begin
    I := FItems.Add(Pair);
    TBHeap<TKey, TValue>.MoveUp(FItems.Items, I, FComparer);
  end;
end;

procedure TBinaryHeapClass<TKey, TValue>.Add(const Values: TArray<TPair<TKey, TValue>>);
var
  I,J: Integer;
begin
  for I := Low(Values) to High(Values) do
  begin
    J := FItems.Add(Values[I]);
    TBHeap<TKey, TValue>.MoveUp(FItems.Items, J, FComparer);
  end;
end;

function TBinaryHeapClass<TKey, TValue>.ExtractMin: TPair<TKey, TValue>;
begin
  result := FItems.Items[0];
  DeleteMin;
end;

function TBinaryHeapClass<TKey, TValue>.MinValue: TPair<TKey, TValue>;
begin
  result := FItems.Items[0];
end;

{ TBinaryHeapClass<TKey>.TEnumerator }

constructor TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.Create(const PairEnumerator: TEnumerator<TPair<TKey, TEmptyRec>>);
begin
  inherited Create;
  FPairEnumerator := PairEnumerator;
end;

destructor TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.Destroy;
begin
  FreeAndNil(FPairEnumerator);
  inherited;
end;

function TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.DoGetCurrent: TKey;
begin
  result := FPairEnumerator.Current.Key;
end;

function TBinaryHeapClass<TKey>.TBinaryHeapEnumerator.DoMoveNext: Boolean;
begin
  result := FPairEnumerator.MoveNext;
end;

{ TBinaryHeapClass<TKey> }

constructor TBinaryHeapClass<TKey>.Create(const ACollection: TEnumerable<TKey>; ACapacity: integer; const AComparer: IComparer<TKey>);
begin
  Create(ACapacity, AComparer);
  Add(ACollection);
end;

constructor TBinaryHeapClass<TKey>.Create(ACapacity: integer; const AComparer: IComparer<TKey>);
begin
  inherited Create;
  FHeap := TBinaryHeapClass<TKey,TEmptyRec>.Create(ACapacity, AComparer);
end;

procedure TBinaryHeapClass<TKey>.Clear;
begin
  FHeap.Clear;
end;

procedure TBinaryHeapClass<TKey>.Delete(n: integer);
begin
  FHeap.Delete(n);
end;

procedure TBinaryHeapClass<TKey>.DeleteMin;
begin
  FHeap.DeleteMin;
end;

destructor TBinaryHeapClass<TKey>.Destroy;
begin
  FreeAndNil(FHeap);
  inherited;
end;

function TBinaryHeapClass<TKey>.DoGetEnumerator: TEnumerator<TKey>;
begin
  result := TBinaryHeapEnumerator.Create(FHeap.GetEnumerator);
end;

function TBinaryHeapClass<TKey>.Add(const Value: TKey): integer;
var
  P: TPair<TKey,TEmptyRec>;
begin
  P.Key := Value;
  result := FHeap.Add(P);
end;

procedure TBinaryHeapClass<TKey>.Add(const Values: TArray<TKey>);
var
  P: TPair<TKey, TEmptyRec>;
  I: Integer;
begin
  I := Count + Length(Values);
  if I > Capacity then
    Capacity := I;
  for I := Low(Values) to High(Values) do
  begin
    P.Key := Values[I];
    FHeap.Add(P);
  end;
end;

procedure TBinaryHeapClass<TKey>.Add(const Values: TEnumerable<TKey>);
var
  P: TPair<TKey, TEmptyRec>;
  K: TKey;
begin
  for K in Values do
  begin
    P.Key := K;
    FHeap.Add(P);
  end;
end;

function TBinaryHeapClass<TKey>.MinValue: TKey;
begin
  result := FHeap.MinValue.Key;
end;

function TBinaryHeapClass<TKey>.Empty: boolean;
begin
  result := FHeap.Empty;
end;

procedure TBinaryHeapClass<TKey>.SetCapacity(const Value: integer);
begin
  FHeap.Capacity := Value;
end;

procedure TBinaryHeapClass<TKey>.SetValue(n: integer; const Value: TKey);
var
  p: TPair<TKey, TEmptyRec>;
begin
  p.Key := Value;
  FHeap.Values[n] := p;
end;

procedure TBinaryHeapClass<TKey>.TrimExcess;
begin
  FHeap.TrimExcess;
end;

function TBinaryHeapClass<TKey>.ExtractMin: TKey;
begin
  result := FHeap.ExtractMin.Key;
end;

function TBinaryHeapClass<TKey>.Find(const Key: TKey): integer;
begin
  result := FHeap.Find(Key);
end;

function TBinaryHeapClass<TKey>.GetCapacity: integer;
begin
  result := FHeap.Capacity;
end;

function TBinaryHeapClass<TKey>.GetCount: integer;
begin
  result := FHeap.Count;
end;

function TBinaryHeapClass<TKey>.GetValue(n: integer): TKey;
begin
  result := FHeap.Values[n].Key;
end;

{ TRingClass<T>.TRingEnumerator }

constructor TRingClass<T>.TRingEnumerator.Create(ARing: TRingClass<T>);
begin
  inherited Create;
  FRing := ARing;
  FPos := 0;
end;

function TRingClass<T>.TRingEnumerator.DoMoveNext: Boolean;
begin
  result := FPos < FRing.Count;
  if result then
    Inc(FPos);
end;

function TRingClass<T>.TRingEnumerator.DoGetCurrent: T;
begin
  result := FRing.ItemsFromHead[FPos-1];
end;

{ TRingClass<T> }

constructor TRingClass<T>.Create(ACapacity: integer);
begin
  Assert(ACapacity > 0);
  inherited Create;
  Capacity := ACapacity;
end;

destructor TRingClass<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TRingClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := TRingEnumerator.Create(Self);
end;

function TRingClass<T>.GetCapacity: integer;
begin
  result := FValues.Count;
end;

procedure TRingClass<T>.SetCapacity(ANewCapacity: integer);
var
  Temp: TArray<T>;
  I: Integer;
begin
  Assert((ANewCapacity >= Count) and (ANewCapacity>0));
  if Capacity=ANewCapacity then
    Exit;
  SetLength(Temp, Count);
  for I := 0 to Count-1 do
  begin
    Temp[I] := ItemsFromHead[I];
    ItemsFromHead[I] := Default(T);
  end;
  FValues.Count := ANewCapacity;
  FHead := 0;
  for I := 0 to Count-1 do
    FValues[I] := Temp[I];
end;

procedure TRingClass<T>.Clear;
var
  I: Integer;
begin
  if FOwnsValues then
    for I := 0 to Count-1 do
      ItemsFromHead[I] := Default(T);
  FHead  := 0;
  FCount := 0;
end;

function TRingClass<T>.GetItemFromHead(n: integer): T;
begin
  result := FValues.Items[(FHead + n) mod Capacity];
end;

procedure TRingClass<T>.SetItemFromHead(n: integer; const Value: T);
begin
  n := (FHead + n) mod Capacity;
  if FOwnsValues then
    PObject(@FValues.Items[n])^.DisposeOf;
  FValues.Items[n] := Value;
end;

function TRingClass<T>.GetItemFromTail(n: integer): T;
begin
  result := FValues.Items[(FHead + FCount - n - 1) mod Capacity];
end;

procedure TRingClass<T>.SetItemFromTail(n: integer; const Value: T);
begin
  n := (FHead + FCount - n - 1) mod Capacity;
  if FOwnsValues then
    PObject(@FValues.Items[n])^.DisposeOf;
  FValues.Items[n] := Value;
end;

procedure TRingClass<T>.SetOwnsValues(const Value: Boolean);
begin
  if Value and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := Value;
end;

function TRingClass<T>.GetHead: T;
begin
  result := ItemsFromHead[0];
end;

procedure TRingClass<T>.SetHead(const Value: T);
begin
  ItemsFromHead[0] := Value;
end;

function TRingClass<T>.GetTail: T;
begin
  result := ItemsFromTail[0];
end;

procedure TRingClass<T>.SetTail(const Value: T);
begin
  ItemsFromTail[0] := Value;
end;

procedure TRingClass<T>.Add(const Value: T);
begin
  if FCount < Capacity then
    inc(FCount);
  Dec(FHead);
  if FHead < 0 then
    Inc(FHead, Capacity);
  if FOwnsValues then
    PObject(@FValues.Items[FHead])^.DisposeOf;
  FValues.Items[FHead] := Value;
end;

procedure TRingClass<T>.AddToTail(const Value: T);
begin
  if FCount < Capacity then
    inc(FCount)
  else
  begin
    Inc(FHead);
    if FHead >= Capacity then
      FHead := 0;
  end;
  ItemsFromTail[0] := Value;
end;

procedure TRingClass<T>.Add(const Values: TArray<T>);
var
  i: Integer;
begin
  for i := 0 to High(Values) do
    Add(Values[i]);
end;

procedure TRingClass<T>.Add(const AValues: TEnumerable<T>);
var
  Value: T;
begin
  for Value in AValues do
    Add(Value);
end;

procedure TRingClass<T>.AddToTail(const Values: TArray<T>);
var
  i: Integer;
begin
  for i := 0 to High(Values) do
    AddToTail(Values[i]);
end;

procedure TRingClass<T>.AddToTail(const AValues: TEnumerable<T>);
var
  Value: T;
begin
  for Value in AValues do
    AddToTail(Value);
end;

function TRingClass<T>.GetEmpty: boolean;
begin
  result := FCount=0;
end;

function TRingClass<T>.Extract: T;
begin
  result := ItemsFromTail[0];
  Delete;
end;

function TRingClass<T>.ExtractHead: T;
begin
  result := ItemsFromHead[0];
  DeleteHead;
end;

procedure TRingClass<T>.Delete;
begin
  ItemsFromTail[0] := Default(T);
  Dec(FCount);
end;

procedure TRingClass<T>.DeleteHead;
begin
  ItemsFromHead[0] := Default(T);
  Dec(FCount);
  Inc(FHead);
  if (FHead >= Capacity) then
    FHead := 0;
end;

end.
