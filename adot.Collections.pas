unit adot.Collections;
{$OVERFLOWCHECKS OFF}

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

  TCacheClass<TKey,TValue> = class
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

  TArr<T> = record
    Wrapper for TArray<T> (array with Add/Delete functionality).

}
interface

uses
  adot.Types,
  adot.Tools.Rtti,
  adot.Arithmetic,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.TypInfo,
  System.StrUtils,
  System.SysUtils,
  System.Character,
  System.Contnrs,
  System.Math,
  System.Classes, 
  System.Hash;

const
  RecordTypes = [tkInteger, tkChar, tkEnumeration, tkFloat, tkSet,
    tkMethod, tkWChar, tkRecord, tkInt64, tkPointer, tkProcedure];

type

  { Delphi has two definitions of TCollectionNotification: System.Classes and System.Generics.Collections }
  TCollectionNotification = System.Generics.Collections.TCollectionNotification;

  EForbiddenOperation = class(Exception);

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
    class function Create: TAutoFreeCollection; static;
    function Add<T: class>(AObject: T):T;
    procedure Clear;
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
    class function Create: TAuto<T>; overload; static;
    class function Create(const AValue: T): TAuto<T>; overload; static;
    class operator Equal(const ALeft, ARight: TAuto<T>): Boolean;
    class operator Equal(const ALeft: TAuto<T>; const ARight: T): Boolean;
    class operator NotEqual(const ALeft, ARight: TAuto<T>): Boolean;
    class operator NotEqual(const ALeft: TAuto<T>; const ARight: T): Boolean;
    class operator Implicit(const AValue: TAuto<T>): T;
    class operator Implicit(const AValue: T): TAuto<T>;

    procedure Clear;

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
    procedure Init(const A: TypeA; const B: TypeB);
  end;

  { Compound record with three fields. Provides constructor and comparers for use in collections. }
  TCompound<TypeA,TypeB,TypeC> = record
    A: TypeA;
    B: TypeB;
    C: TypeC;

    constructor Create(const A: TypeA; const B: TypeB; const C: TypeC);
    procedure Init(const A: TypeA; const B: TypeB; const C: TypeC);
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
      ComparerA: IEqualityComparer<TypeA>;
      ComparerB: IEqualityComparer<TypeB>
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
      ComparerA: IEqualityComparer<TypeA>;
      ComparerB: IEqualityComparer<TypeB>;
      ComparerC: IEqualityComparer<TypeC>
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
    constructor Create(ComparerA: IComparer<TypeA>; ComparerB: IComparer<TypeB>);
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
      ComparerA: IComparer<TypeA>;
      ComparerB: IComparer<TypeB>;
      ComparerC: IComparer<TypeC>
    );
    function Compare(const Left, Right: TCompound<TypeA,TypeB,TypeC>): Integer; override;
  end;

  TGuidInt = TCompound<TGUID, integer>;

  TSetOp = (soUnion, soIntersection, soDifference, soSymmetricDifference);

  TEnumerableExt<T> = class(TEnumerable<T>)
  protected
    class procedure DoSaveToStream(Src: TEnumerable<T>; Dst: TStream; Encoding: TEncoding = nil); static;
    class procedure DoSaveToFile(Src: TEnumerable<T>; const FileName: string; Encoding: TEncoding = nil; MemStream: boolean = True); static;

  public
    procedure SaveToStream(Dst: TStream; Encoding: TEncoding = nil);
    procedure SaveToFile(const FileName: string; Encoding: TEncoding = nil; MemStream: boolean = True);
  end;

  { Generic class for set. }
  TSetClass<TValue> = class(TEnumerableExt<TValue>)
  public
    type
      TEnumerator = TDictionary<TValue, TEmptyRec>.TKeyEnumerator;

      TSetObjectDictionary<TSetDictKey,TSetDictValue> = class(TDictionary<TSetDictKey,TSetDictValue>)
      protected
        OwnsKeys: boolean;

        procedure KeyNotify(const Key: TSetDictKey; Action: TCollectionNotification); override;
      end;

  protected
    var
      FSet: TSetObjectDictionary<TValue, TEmptyRec>;
      FComparerCopy: IEqualityComparer<TValue>; { FSet.Comparer is hidden in private section, so we keep copy }

    function GetCount: integer;
    function DoGetEnumerator: TEnumerator<TValue>; override;
    function GetComparer: IEqualityComparer<TValue>;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetOwnsValues: boolean;
    function GetAsArray: TArray<TValue>;
    function GetAsString: string;

  public
    constructor Create(ACapacity: integer = 0; AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AValues: array of TValue; AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AValues: TEnumerable<TValue>; AComparer: IEqualityComparer<TValue> = nil); overload;
    constructor Create(const AOperands: TArray<TSetClass<TValue>>; ASetOp: TSetOp; AComparer: IEqualityComparer<TValue> = nil); overload;

    destructor Destroy; override;
    function GetEnumerator: TEnumerator; reintroduce;
    procedure Add(const AValue: TValue); overload;
    procedure Add(const ASet: array of TValue); overload;
    procedure Add(const AValues: TEnumerable<TValue>); overload;
    procedure IncludeLogicalAnd(const A,B: TSetClass<TValue>);
    procedure IncludeLogicalOr(const A,B: TSetClass<TValue>);
    procedure IncludeLogicalXor(const A,B: TSetClass<TValue>);
    procedure Include(const AValue: TValue); overload;
    procedure Include(const ASet: array of TValue); overload;
    procedure Include(const AValues: TEnumerable<TValue>); overload;
    procedure Remove(const AValue: TValue); overload;
    procedure Remove(const ASet: array of TValue); overload;
    procedure Remove(const AValues: TEnumerable<TValue>); overload;
    function Contains(const AValue: TValue): boolean; overload;
    function Contains(const ASet: array of TValue): boolean; overload;
    function Contains(const AValues: TEnumerable<TValue>): boolean; overload;
    procedure Clear;
    function Empty: Boolean;
    property Count: integer read GetCount;
    property Comparer: IEqualityComparer<TValue> read GetComparer;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property AsString: string read GetAsString;
    property AsArray: TArray<TValue> read GetAsArray;
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

    procedure CreateSet(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);

    function GetReadonly: TSetClass<T>;
    function GetReadWrite: TSetClass<T>;
    function GetAsString: string;
    function GetAsArray: TArray<T>;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetCount: integer;
    function GetEmpty: Boolean;
    function GetCollection: TEnumerable<T>;

    property ReadOnly: TSetClass<T> read GetReadonly;
    property ReadWrite: TSetClass<T> read GetReadWrite;
  public
    type
      TEnumerator = TSetClass<T>.TEnumerator;

    { Record type TSet<T> can be used without constructor, use constructor only if you
      need some customization: set Capacity, provide custom comparer etc. }
    constructor Create(ACapacity: integer; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: array of T; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(const V: array of TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;
    constructor Create(V: TSet<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil); overload;

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
    function Contains(const a: T) : Boolean; overload;
    function Contains(const a: TEnumerable<T>) : Boolean; overload;
    function Contains(a: TSet<T>) : Boolean; overload;

    function Copy: TSet<T>;

    procedure Clear;

    class operator In(const a: T; b: TSet<T>) : Boolean;
    class operator In(const a: TEnumerable<T>; b: TSet<T>) : Boolean;
    class operator In(a: TSet<T>; b: TSet<T>) : Boolean;

    class operator Implicit(const a : T) : TSet<T>;
    class operator Implicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Implicit(const a : array of T) : TSet<T>;
    class operator Implicit(const a : TArray<T>) : TSet<T>;

    class operator Explicit(const a : T) : TSet<T>;
    class operator Explicit(const a : TEnumerable<T>) : TSet<T>;
    class operator Explicit(const a : array of T) : TSet<T>;
    class operator Explicit(const a : TArray<T>) : TSet<T>;

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
    property Empty: Boolean read GetEmpty;
    property Count: integer read GetCount;
    property Collection: TEnumerable<T> read GetCollection;
  end;

  { Class for map. Based on TDictionary and extends it with some features. }
  TMapClass<TKey,TValue> = class(TDictionary<TKey,TValue>)
  protected
    FComparerCopy: IEqualityComparer<TKey>; { FDictionary.Comparer is hidden in private section, so we keep copy }
    FOwnerships: TDictionaryOwnerships;

    procedure KeyNotify(const Key: TKey; Action: TCollectionNotification); override;
    procedure ValueNotify(const Value: TValue; Action: TCollectionNotification); override;
    function GetOwnsKeys: boolean;
    function GetOwnsValues: boolean;
    procedure SetOwnsKeys(const Value: boolean);
    procedure SetOwnsValues(const Value: boolean);
    function GetAsString: string;
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

    property Comparer: IEqualityComparer<TKey> read FComparerCopy;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property AsString:string read GetAsString;
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
    function GetAsString: string;

  public

    { Record type TMap<TKey,TValue> can be used without constructor, use constructor only if you
      need some customization: set Capacity, provide custom comparer etc. }
    constructor Create(ACapacity: integer; AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const V: array of TPair<TKey,TValue>; ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(const V: TEnumerable<TPair<TKey,TValue>>; ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil); overload;
    constructor Create(V: TMap<TKey,TValue>; ACapacity: integer = 0; AComparer: IEqualityComparer<TKey> = nil); overload;

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
    procedure Release; { release underlying object }
    procedure TrimExcess;
    function ContainsKey(const Key: TKey): Boolean;
    function ToArray: TArray<TPair<TKey,TValue>>;

    class operator Equal(A,B: TMap<TKey,TValue>): Boolean;
    class operator NotEqual(A,B: TMap<TKey,TValue>): Boolean;

    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    property Empty: Boolean read GetEmpty;
    property Keys: TKeyCollection read GetKeys;
    property Values: TValueCollection read GetValues;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property AsString:string read GetAsString;
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

        constructor Create(AMultimap: TMultimapClass<TKey,TValue>; const AKey: TKey);
        function GetCurrent: TValue;
        function GetKey: TKey;
      public
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
    procedure Add(const AKey: TKey; const AValues: array of TValue); overload;
    procedure Add(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;
    procedure Add(const ACollection: TEnumerable<TPair<TKey,TValue>>); overload;

    { e := m.Values[Key];
      while e.MoveNext do
        if m.Current=10 then m.RemoveValue(e); }
    function Remove(const AKey: TKey):Boolean;
    procedure RemoveValue(const AEnum: TValueEnumerator);
    procedure RemoveValues(const AKey: TKey; const AValues: TSetClass<TValue>); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: array of TValue); overload;
    procedure RemoveValues(const AKey: TKey; const AValues: TEnumerable<TValue>); overload;

    function ContainsKey(const AKey: TKey): Boolean;
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

  { Creates sorted index for quick (binary) search of elements
    without sorting of array itself }
  TBinarySearchIndexClass<T> = class
  private
    FValues: TArray<T>;
    FIndex: TArray<integer>;
    FComparer: IComparer<T>;

    function GetIndex(IndexedPos: integer): integer;
    function GetValue(IndexedPos: integer): T;
    function GetCount: integer;

  public
    constructor Create(const AValues: TArray<T>; AComparer: IComparer<T> = nil);
    function Find(const Value: T): integer;
    procedure Clear;

    property Indices[IndexedPos: integer]: integer read GetIndex; default;
    property Values[IndexedPos: integer]: T read GetValue;
    property Count: integer read GetCount;
  end;

  TSortFunc<T> = reference to function(const Left,Right: T): integer;

  TVectorClass<T> = class(TEnumerableExt<T>)
  public
    type
      TRemoveFilter = reference to function(const Value: T; Index: integer): boolean;

  protected
    type
      TVectorEnumerator = class(TEnumerator<T>)
      protected
        FItems: TArray<T>;
        FCount: integer;
        FCurrentIndex: integer;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;

      public
        constructor Create(AVector: TVectorClass<T>);
      end;

  private
    function GetCapacity: integer;
    function GetEmpty: boolean;
    function GetFirst: T;
    function GetItem(ItemIndex: integer): T;
    function GetLast: T;
    function GetTotalSizeBytes: int64;
    procedure SetCapacity(const Value: integer);
    procedure SetCount(const Value: integer);
    procedure SetFirst(const Value: T);
    procedure SetItem(ItemIndex: integer; const Value: T);
    procedure SetLast(const Value: T);
    procedure Grow;
    function ContainsAll(V: TArray<T>): boolean;
    function GetItemsArray: TArray<T>;
    procedure SetItemsArray(const Value: TArray<T>);
    procedure FindComparer(var AComparer: IComparer<T>);
    procedure SetOwnsValues(const Value: boolean);

  protected
    FItems: TArray<T>;
    FCount: integer;
    FComparer: IComparer<T>;
    FOwnsValues: boolean;

    function DoGetEnumerator: TEnumerator<T>; override;

  public
    constructor Create(AComparer: IComparer<T> = nil); overload;
    constructor Create(AComparer: TComparison<T>); overload;
    constructor Create(ACapacity: integer; AComparer: IComparer<T> = nil); overload;
    constructor Create(AItems: TArray<T>; AComparer: IComparer<T> = nil); overload;
    constructor Create(AItems: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    destructor Destroy; override;

    procedure Clear;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Value: TArray<T>); overload;
    procedure Add(const Value: TArray<T>; AStartIndex,ACount: integer); overload;
    procedure Add(const Value: TEnumerable<T>); overload;

    { Get new instance with copy of the data }
    function Copy: TVectorClass<T>;

    function Insert(Index: integer; const Value: T): integer;

    function Sorted: boolean; overload;
    function Sorted(AComparer: IComparer<T>): boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparer: IComparer<T>): boolean; overload;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(AStartIndex,ACount: integer); overload;
    procedure Delete(const AIndices: TArray<integer>); overload;
    procedure Delete(AIndices: TSet<integer>); overload;
    procedure DeleteLast;

    procedure Remove(const V: T; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(AFilter: TRemoveFilter); overload;

    { Removes item from the vector. Unlike Delete it returns the value and will not free the item }
    function Extract(ItemIndex: integer): T;
    function ExtractAll: TArray<T>;
    function ExtractLast: T;

    procedure Move(SrcIndex, DstIndex: integer);

    function IndexOf(const Value: T): integer; overload;
    function IndexOf(const Value: T; AComparer: IComparer<T>): integer; overload;
    function FindFirst(const Value: T; var Index: integer): boolean; overload;
    function FindFirst(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean; overload;
    function FindNext(const Value: T; var Index: integer): boolean; overload;
    function FindNext(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean; overload;

    function Contains(const Value: T): boolean; overload;
    function Contains(const Values: TArray<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>): boolean; overload;

    procedure Exchange(Index1,Index2: integer);
    { Reverse( [1,2,3,4,5], 1, 3 ) = [1, 4,3,2, 5] }
    procedure Reverse; overload;
    procedure Reverse(AStartIndex,ACount: integer); overload;
    { RotateLeft( [1,2,3,4,5], 1, 3, 1 ) = [1, 3,4,2, 5]
      RotateLeft( [1,2,3,4,5], 1, 3,-1 ) = [1, 4,2,3, 5] }
    procedure RotateLeft(Index1,Index2,Shift: integer);
    { RotateRight( [1,2,3,4,5], 1, 3, 1 ) = [1, 4,2,3, 5]
      RotateRight( [1,2,3,4,5], 1, 3,-1 ) = [1, 3,4,2, 5] }
    procedure RotateRight(Index1,Index2,Shift: integer);
    { Shuffle items of the range in random order }
    procedure Shuffle; overload;
    procedure Shuffle(AStartIndex,ACount: integer); overload;
    { Generate all permutations. Permutations of [2,1,3]:
        [1,2,3] [1,3,2] [2,1,3] [2,3,1] [3,1,2] [3,2,1] }
    procedure FirstPermutation;
    function NextPermutation: boolean;
    function PrevPermutation: boolean;

    procedure TrimExcess;

    procedure Sort(AComparer: IComparer<T> = nil); overload;
    procedure Sort(AIndex, ACount: Integer; AComparer: IComparer<T> = nil); overload;
    procedure Sort(AComparer: TSortFunc<T>); overload;
    procedure Sort(AIndex, ACount: Integer; AComparer: TSortFunc<T>); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer; AComparer: IComparer<T> = nil): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; AStartIndex,ACount: Integer; AComparer: IComparer<T> = nil): Boolean; overload;

    { TArray }
    function Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(const B: TArray<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;
    { TEnumerable }
    function Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(B: TEnumerable<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;

    function Equal(const B: TArray<T>): boolean; overload;
    function Equal(B: TEnumerable<T>): boolean; overload;

    { get copy of data }
    function ToArray: TArray<T>; overload; override;
    { Readonly=False : get copy of data
      Readonly=True  : get pointer to stored data }
    function ToArray(Readonly: boolean): TArray<T>; reintroduce; overload;
    function ToString: string; reintroduce; overload;
    function ToString(const ValueSeparator: string; SepAfterLastValue: boolean): string; reintroduce; overload;
    function ToText: string;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Count: integer read FCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Items[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
    property Comparer: IComparer<T> read FComparer write FComparer;
    property OwnsValues: boolean read FOwnsValues write SetOwnsValues;
    property ItemsArray: TArray<T> read GetItemsArray write SetItemsArray;
  end;

  TVector<T> = record
  private
    FVectorInt: IInterfacedObject<TVectorClass<T>>;

    procedure CreateVector(ACapacity: integer = 0; AComparer: IComparer<T> = nil);

    function GetRO: TVectorClass<T>;
    function GetRW: TVectorClass<T>;
    function GetItemsArray: TArray<T>;
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(AOwnsValues: boolean);
    function GetCount: integer;
    function GetEmpty: Boolean;
    function GetCollection: TEnumerable<T>;
    function GetCapacity: integer;
    function GetFirst: T;
    function GetItem(ItemIndex: integer): T;
    function GetLast: T;
    function GetTotalSizeBytes: int64;
    procedure SetCapacity(const Value: integer);
    procedure SetCount(const Value: integer);
    procedure SetFirst(const Value: T);
    procedure SetItem(ItemIndex: integer; const Value: T);
    procedure SetLast(const Value: T);
    function GetComparer: IComparer<T>;
    procedure SetComparer(Value: IComparer<T>);
    procedure SetItemsArray(const Value: TArray<T>);

    property RO: TVectorClass<T> read GetRO;
    property RW: TVectorClass<T> read GetRW;
  public
    type
      TRemoveFilter = TVectorClass<T>.TRemoveFilter;

    { Record type can be used without constructor, use constructor only if you
      need some customization: set Capacity, provide custom comparer etc.
      Delphi doesn't allow parameterless constructor, but fine with function }
    class function Create(AComparer: IComparer<T> = nil): TVector<T>; overload; static;
    constructor Create(AComparer: TComparison<T>); overload;
    constructor Create(ACapacity: integer; AComparer: IComparer<T> = nil); overload;
    constructor Create(const Values: TArray<T>; AComparer: IComparer<T> = nil); overload;
    constructor Create(const Values: TEnumerable<T>; ACapacity: integer = 0; AComparer: IComparer<T> = nil); overload;

    function GetEnumerator: TEnumerator<T>;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Values: TArray<T>); overload;
    procedure Add(const Values: TArray<T>; AStartIndex,ACount: integer); overload;
    procedure Add(const Values: TEnumerable<T>); overload;
    procedure Add(Values: TVector<T>); overload;

    { Normally it is not necessary to use Copy, TVector supports copy-on-write }
    function Copy: TVector<T>;
    procedure Clear;

    function Insert(Index: integer; const Value: T): integer;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(AStartIndex,ACount: integer); overload;
    procedure Delete(const AIndices: TArray<integer>); overload;
    procedure Delete(AIndices: TSet<integer>); overload;
    procedure DeleteLast;

    procedure Remove(const V: T; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(AFilter: TRemoveFilter); overload;

    { get & delete }
    function Extract(ItemIndex: integer): T;
    function ExtractAll: TArray<T>;
    function ExtractLast: T;

    procedure Move(SrcIndex, DstIndex: integer);

    function IndexOf(const Value: T): integer; overload;
    function IndexOf(const Value: T; AComparer: IComparer<T>): integer; overload;
    function FindFirst(const Value: T; var Index: integer): boolean; overload;
    function FindFirst(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean; overload;
    function FindNext(const Value: T; var Index: integer): boolean; overload;
    function FindNext(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean; overload;

    function Contains(const Value: T): boolean; overload;
    function Contains(const Values: TArray<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>): boolean; overload;

    procedure Exchange(Index1,Index2: integer);
    { Reverse( [1,2,3,4,5], 1, 3 ) = [1, 4,3,2, 5] }
    procedure Reverse; overload;
    procedure Reverse(AStartIndex,ACount: integer); overload;
    { RotateLeft( [1,2,3,4,5], 1, 3, 1 ) = [1, 3,4,2, 5]
      RotateLeft( [1,2,3,4,5], 1, 3,-1 ) = [1, 4,2,3, 5] }
    procedure RotateLeft(Index1,Index2,Shift: integer);
    { RotateRight( [1,2,3,4,5], 1, 3, 1 ) = [1, 4,2,3, 5]
      RotateRight( [1,2,3,4,5], 1, 3,-1 ) = [1, 3,4,2, 5] }
    procedure RotateRight(Index1,Index2,Shift: integer);
    { Shuffle items of the range in random order }
    procedure Shuffle; overload;
    procedure Shuffle(AStartIndex,ACount: integer); overload;
    { Generate all permutations. Permutations of [2,1,3]:
      [1,2,3] [1,3,2] [2,1,3] [2,3,1] [3,1,2] [3,2,1 }
    procedure FirstPermutation;
    function NextPermutation: boolean;
    function PrevPermutation: boolean;

    procedure TrimExcess;

    procedure Sort(AComparer: IComparer<T> = nil); overload;
    procedure Sort(AIndex, ACount: Integer; AComparer: IComparer<T> = nil); overload;
    procedure Sort(AComparer: TSortFunc<T>); overload;
    procedure Sort(AIndex, ACount: Integer; AComparer: TSortFunc<T>); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer; AComparer: IComparer<T> = nil): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; AStartIndex,ACount: Integer; AComparer: IComparer<T> = nil): Boolean; overload;

    { TArray }
    function Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(const B: TArray<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;
    { TEnumerable }
    function Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(B: TEnumerable<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;

    function Equal(const B: TArray<T>): boolean; overload;
    function Equal(B: TEnumerable<T>): boolean; overload;

    { Readonly=False : get copy of data
      Readonly=True  : get pointer to stored data }
    function ToArray(ReadOnly: boolean = False): TArray<T>;
    function ToString: string; overload;
    function ToString(const ValueSeparator: string; SepAfterLastValue: boolean = False): string; overload;
    function ToText: string;

    procedure SaveToStream(Dst: TStream; Encoding: TEncoding = nil);
    procedure SaveToFile(const FileName: string; Encoding: TEncoding = nil; MemStream: boolean = True);

    class operator In(const a: T; b: TVector<T>) : Boolean;
    class operator In(a: TVector<T>; b: TVector<T>) : Boolean;
    class operator In(const a: TArray<T>; b: TVector<T>) : Boolean;
    class operator In(const a: TEnumerable<T>; b: TVector<T>) : Boolean;

    class operator Implicit(const a : T) : TVector<T>;
    class operator Implicit(const a : TArray<T>) : TVector<T>;
    class operator Implicit(const a : TEnumerable<T>) : TVector<T>;
    { We don't want to have both conversions: ->TArray and ->TEnumerable,
      because in many cases it will create ambiguity (many methods support both as input).
      We support TEnumerable because it is safe. If someone needs TArray, he can use
      wither AsArray (for readobly access) or ToArray }
    class operator Implicit(a : TVector<T>) : TEnumerable<T>;

    class operator Explicit(const a : T) : TVector<T>;
    class operator Explicit(const a : TArray<T>) : TVector<T>;
    class operator Explicit(const a : TEnumerable<T>) : TVector<T>;
    { see comments for Implicit(a : TVector<T>) : TEnumerable<T>; }
    class operator Explicit(a : TVector<T>) : TEnumerable<T>;

    class operator Add(a: TVector<T>; const b: T): TVector<T>;
    class operator Add(a: TVector<T>;       b: TVector<T>): TVector<T>;
    class operator Add(a: TVector<T>; const b: TArray<T>): TVector<T>;
    class operator Add(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
    class operator Add(const a: T;              b: TVector<T>): TVector<T>;
    class operator Add(const a: TArray<T>;      b: TVector<T>): TVector<T>;
    class operator Add(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;

    class operator Subtract(a: TVector<T>; const b: T): TVector<T>;
    class operator Subtract(a: TVector<T>;       b: TVector<T>): TVector<T>;
    class operator Subtract(a: TVector<T>; const b: TArray<T>): TVector<T>;
    class operator Subtract(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
    class operator Subtract(const a: T;              b: TVector<T>): TVector<T>;
    class operator Subtract(const a: TArray<T>;      b: TVector<T>): TVector<T>;
    class operator Subtract(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;

    class operator Equal(a: TVector<T>;       b: TVector<T>) : Boolean;
    class operator Equal(a: TVector<T>; const b: TArray<T>) : Boolean;
    class operator Equal(a: TVector<T>; const b: TEnumerable<T>) : Boolean;
    class operator Equal(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator Equal(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator NotEqual(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator NotEqual(a: TVector<T>; const b: TArray<T>) : Boolean;
    class operator NotEqual(a: TVector<T>; const b: TEnumerable<T>) : Boolean;
    class operator NotEqual(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator NotEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator GreaterThanOrEqual(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator GreaterThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator GreaterThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator GreaterThanOrEqual(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator GreaterThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator GreaterThan(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator GreaterThan(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator GreaterThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator GreaterThan(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator GreaterThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator LessThan(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator LessThan(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator LessThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator LessThan(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator LessThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    class operator LessThanOrEqual(a: TVector<T>;       b: TVector<T>): Boolean;
    class operator LessThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
    class operator LessThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
    class operator LessThanOrEqual(const b: TArray<T>;      a: TVector<T>): Boolean;
    class operator LessThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Count: integer read GetCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Items[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
    property ItemsArray: TArray<T> read GetItemsArray write SetItemsArray;
    property Collection: TEnumerable<T> read GetCollection;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
    property Comparer: IComparer<T> read GetComparer write SetComparer;
  end;

  { Wrapper for TArray<T> (array with Add/Delete functionality). Example:
       function GetFiltered(const Src: TArray<integer>; Filter: TFunc<integer, boolean>): TArray<integer>;
       var
         V: TArr<integer>;
         I: integer;
       begin
         V.Clear;
         for I := 0 to High(Src) do
           if Filter(Src[I]) then
             V.Add(Src[I]); // more efficient than resizing TArray<> every time
         Result := V.ToArray; // there is no copying of data here, we get array pointer only
       end; }
  TArr<T> = record
  public
    { we define it before other field to make access more efficient }
    Items: TArray<T>;
  private
    FCount: integer;

    procedure SetCount(ACount: integer);
    procedure SetCapacity(ACapacity: integer);
    procedure Grow;
    function GetCapacity: integer;
    function GetItem(ItemIndex: integer): T;
    procedure SetItem(ItemIndex: integer; const Value: T);
    function GetFirst: T;
    function GetLast: T;
    procedure SetFirst(const Value: T);
    procedure SetLast(const Value: T);
    function GetEmpty: Boolean;
    function GetTotalSizeBytes: int64;

  public
    type
      TEnumerator = record
      private
        Items: TArray<T>;
        Count: integer;
        Pos: integer;

        function GetCurrent: T;
      public
        constructor Create(const Items: TArray<T>; ACount: integer);
        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;

    constructor Create(ACapacity: integer); overload;
    constructor Create(ADst: TArray<T>); overload;
    procedure Init;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Value: TArray<T>); overload;
    procedure Add(const Value: TEnumerable<T>); overload;
    procedure Add(const Value: TArr<T>); overload;

    { Dynamic arrays in Delphi do not support copy-on-write.
      TArr is wrapper for TArray and doesn't support COW too. }
    function Copy: TArr<T>;

    function Insert(Index: integer; const Value: T): integer;
    procedure Delete(ItemIndex: integer); overload;
    procedure Move(SrcIndex, DstIndex: integer);
    procedure Exchange(Index1,Index2: integer);
    procedure DeleteLast;
    function ExtractLast: T;

    function IndexOf(const Value: T): integer; overload;
    function IndexOf(const Value: T; Comparer: IComparer<T>): integer; overload;
    function FindFirst(const Value: T; var Index: integer): boolean; overload;
    function FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;
    function FindNext(const Value: T; var Index: integer): boolean; overload;
    function FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;

    function ToArray: TArray<T>;

    function GetEnumerator: TEnumerator; reintroduce;
    procedure Clear;
    procedure TrimExcess;

    procedure Sort; overload;
    procedure Sort(Comparer: IComparer<T>); overload;
    procedure Sort(Comparer: IComparer<T>; AIndex, ACount: Integer); overload;
    procedure Sort(Comparer: TFunc<T,T,integer>); overload;
    procedure Sort(Comparer: TFunc<T,T,integer>; AIndex, ACount: Integer); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean; overload;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Count: integer read FCount write SetCount;
    property Length: integer read FCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Elements[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
  end;

  { Dynamic 2-dimensional array }
  TVector2D<T> = record
  public
    Rows: TArr<TArr<T>>;

    type

      TEnumerator = class(TEnumerator<T>)
      protected
        Rows: TArr<TArr<T>>;
        X,Y: integer;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;

      public
        constructor Create(const Rows: TArr<TArr<T>>);
      end;

      TCollection = class(TEnumerableExt<T>)
      protected
        Rows: TArr<TArr<T>>;

        function DoGetEnumerator: TEnumerator<T>; override;

      public
        constructor Create(const Rows: TArr<TArr<T>>);
      end;

  private
    function GetValue(x,y: integer): T;
    procedure SetValue(x, y: integer; const Value: T);
    function GetRowCount: integer;
    procedure SetRowCount(const Value: integer);
    function GetWidth(y: integer): integer;
    procedure SetWidth(y: integer; const Value: integer);

  public
    constructor Create(Width, Height: integer);
    procedure Clear;

    function AddRow: integer;
    function Add(y: integer): integer; overload;
    function Add(y: integer; const Value: T): integer; overload;
    function Add(y: integer; const Values: TEnumerable<T>): integer; overload;
    function Add(y: integer; const Values: TArray<T>): integer; overload;

    { Syntax:
        for Value in Vec2d.Collection.Data do }
    function Collection: IInterfacedObject<TEnumerable<T>>;

    property Elements[x,y: integer]: T read GetValue write SetValue; default;
    property RowCount: integer read GetRowCount write SetRowCount;
    property Count[y: integer]:integer read GetWidth write SetWidth;
  end;

  { Low level heap operations on array.}
  TBHeap<TKey, TValue> = class
    class function GetLeft(ParentIdx: integer): integer; static;
    class function GetRight(ParentIdx: integer): integer; static;
    class function GetParent(ChildIdx: integer): integer; static;
    class procedure MoveUp(var Items: TArray<TPair<TKey,TValue>>; ItemIndex: integer; Comparer: IComparer<TKey>); static;
    class procedure Delete(var Items: TArray<TPair<TKey,TValue>>; ItemIndex,Count: integer; Comparer: IComparer<TKey>); static;
    class function ExtractMin(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>):TPair<TKey,TValue>; static;
    class procedure Build(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>); static;
    class procedure Sort(var Items: TArray<TPair<TKey,TValue>>; Count: integer; Comparer: IComparer<TKey>); static;
    class procedure Replace(var Items: TArray<TPair<TKey,TValue>>; ItemIndex,Count: integer;
      Comparer: IComparer<TKey>; const Pair:TPair<TKey,TValue>); static;
  end;

  { Binary heap of pairs [Key;Value]. }
  TBinaryHeapClass<TKey,TValue> = class(TEnumerableExt<TPair<TKey,TValue>>)
  public
    type
      TPairsEnumerator = TArr<TPair<TKey,TValue>>.TEnumerator;

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
    FItems: TArr<TPair<TKey,TValue>>;
    FComparer: IComparer<TKey>;
    FOwnerships: TDictionaryOwnerships;

    function GetCapacity: integer;
    function GetCount: integer;
    procedure SetCapacity(ACapacity: integer);
    procedure SetValue(n: integer; const Value: TPair<TKey, TValue>);
    function GetValue(Index: integer): TPair<TKey,TValue>;
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
    constructor Create(ACapacity: integer = 0; AComparer: IComparer<TKey> = nil); overload;
    constructor Create(const ACollection: TEnumerable<TPair<TKey,TValue>>; ACapacity: integer = 0; AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    function Add(const Key: TKey; const Value: TValue): integer; overload;
    function Add(const Pair: TPair<TKey,TValue>): integer; overload;
    procedure Add(const Values: TArray<TPair<TKey,TValue>>); overload;
    procedure Add(const Values: TEnumerable<TPair<TKey,TValue>>); overload;

    function MinValue: TPair<TKey,TValue>;    { O(1)      }
    function ExtractMin: TPair<TKey,TValue>;  { O(Log(n)) }
    procedure DeleteMin;
    procedure Delete(n: integer);

    procedure Clear;
    procedure TrimExcess;

    function Empty: boolean;

    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Values[n: integer]: TPair<TKey,TValue> read GetValue write SetValue; default;
    property Keys: TKeyCollection read GetKeyCollection;
    property Comparer: IComparer<TKey> read FComparer;
    property OwnsKeys: boolean read GetOwnsKeys write SetOwnsKeys;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  { Binary heap with key only. }
  TBinaryHeapClass<TKey> = class(TEnumerableExt<TKey>)
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
    function GetCapacity: integer;
    function GetCount: integer;
    function GetValue(n: integer): TKey;
    procedure SetCapacity(const Value: integer);
    procedure SetValue(n: integer; const Value: TKey);

    { this function is O(N), avoid of use it }
    function Find(const Key: TKey): integer;

  public
    constructor Create(ACapacity: integer = 0; AComparer: IComparer<TKey> = nil); overload;
    constructor Create(const ACollection: TEnumerable<TKey>; ACapacity: integer = 0; AComparer: IComparer<TKey> = nil); overload;
    destructor Destroy; override;

    function Add(const Value: TKey): integer; overload;
    procedure Add(const Values: TArray<TKey>); overload;
    procedure Add(const Values: TEnumerable<TKey>); overload;

    function MinValue: TKey;    { O(1)      }
    function ExtractMin: TKey;  { O(Log(n)) }

    procedure DeleteMin;
    procedure Delete(n: integer);

    procedure Clear;
    procedure TrimExcess;
    function Empty: boolean;

    property Count: integer read GetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Values[n: integer]: TKey read GetValue write SetValue; default;
  end;

  { Cyclic/circular buffer based on array. Add/delete items to head/tail. }
  TRingClass<T> = class(TEnumerableExt<T>)
  protected
    { FValues.Items is inner array, Items is property to access elements of the ring by index }
    FValues: TArr<T>;
    FHead: integer;
    FCount: integer;
    FOwnsValues: Boolean;

    function GetItemFromHead(n: integer): T;
    procedure SetItemFromHead(n: integer; const Value: T);
    function GetItemFromTail(n: integer): T;
    procedure SetItemFromTail(n: integer; const Value: T);
    function GetCapacity: integer;
    procedure SetCapacity(ANewCapacity: integer);
    function GetHead: T;
    procedure SetHead(const Value: T);
    function GetTail: T;
    procedure SetTail(const Value: T);
    function GetEmpty: boolean;
    procedure SetOwnsValues(const Value: Boolean);
    function DoGetEnumerator: TEnumerator<T>; override;
    function GetIsFull: boolean;

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
    function Extract: T;
    function ExtractTail: T;
    { from head }
    function ExtractHead: T;

    procedure Delete;
    procedure DeleteHead;

    property Capacity: integer read GetCapacity write SetCapacity; { should have Count=0 to resize }
    property Count: integer read FCount write FCount;
    property Empty: boolean read GetEmpty;
    property Head: T read GetHead write SetHead;
    property Tail: T read GetTail write SetTail;
    property ItemsFromHead[n: integer]:T read GetItemFromHead write SetItemFromHead; default; { from Head to Tail }
    property ItemsFromTail[n: integer]:T read GetItemFromTail write SetItemFromTail;          { from Tail to Head }
    property OwnsValues: Boolean read FOwnsValues write SetOwnsValues;
    property IsFull: boolean read GetIsFull;
  end;

  { Based on TDictionary, but automatically deletes data if it take more space than allowed. }
  TCacheClass<TKey,TValue> = class
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

  { Default comparer/equality comparer etc. }
  TComparerUtils = class
  private
    type
      TEqualityByComparer<T> = class(TEqualityComparer<T>)
      private
        FComparer: IComparer<T>;
      public
        constructor Create(AComparer: IComparer<T>);
        function Equals(const Left, Right: T): Boolean; override;
        function GetHashCode(const Value: T): Integer; override;
      end;
  public

    { For string type we use case insensitive comparer by default }
    class function DefaultComparer<T>: IComparer<T>; static;
    class function DefaultEqualityComparer<T>: IEqualityComparer<T>; static;
    class function DefaultArithmetic<T>: IArithmetic<T>; static;
    class function FindEqualityComparer<T>(Comparer: IComparer<T>): IEqualityComparer<T>; static;

    class function CompoundComparer<A,B>: IComparer<TCompound<A,B>>; overload; static;
    class function CompoundComparer<A,B,C>: IComparer<TCompound<A,B,C>>; overload; static;
    class function CompoundEqualityComparer<A,B>: IEqualityComparer<TCompound<A,B>>; overload;  static;
    class function CompoundEqualityComparer<A,B,C>: IEqualityComparer<TCompound<A,B,C>>; overload;  static;

    class function Equal<T>(A,B: TEnumerable<T>): boolean; overload; static;
    class function Equal<T>(A,B: TEnumerable<T>; Comparer: IComparer<T>): boolean; overload; static;
    class function Equal<T>(A: TEnumerable<T>; const B: TArray<T>): boolean; overload; static;
    class function Equal<T>(A: TEnumerable<T>; const B: TArray<T>; Comparer: IComparer<T>): boolean; overload; static;

    class function Compare<T>(const Left, Right: TList<T>; ItemComparer: IComparer<T>): integer; overload; static;
    class function Compare<T>(const Left, Right: TArray<T>; ItemComparer: IComparer<T>): integer; overload; static;
    class function Compare(const Left, Right: string; CaseInsensitive: boolean = True): integer; overload; static;
    class function Compare(const Left, Right: integer): integer; overload; static;
    class function Compare(const Left, Right: double): integer; overload; static;
    class function Compare(const Left, Right: boolean): integer; overload; static;
  end;

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
        constructor Create(const ANodes: TArr<TNode>);
        function MoveNext: Boolean;
        property Current: integer read GetCurrent;
      end;

      { Same as TEnumerator but returns value instead of index }
      TValuesEnumerator = record
      private
        Enum: TEnumerator;

        function GetCurrent: T;
      public
        constructor Create(const ANodes: TArr<TNode>);
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
        constructor Create(const ANodes: TArr<TNode>; ARoot: integer);
        function MoveNext: Boolean;
        property Current: integer read CurrentNode;
      end;

      { Enumerates all values }
      TValuesCollection = record
        Nodes: TArr<TNode>;

        constructor Create(const ANodes: TArr<TNode>);
        function GetEnumerator: TValuesEnumerator;
      end;

      { Enumerable subtree starting from specified node }
      TSubtreeCollection = record
        Nodes: TArr<TNode>;
        Root: integer;

        constructor Create(const ANodes: TArr<TNode>; ARoot: integer);
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

  { Integer range (pair [min_value; max_value]).

    Example 1:
      var R: TRange; // we will generate ranges for array of item numbers
      for R in TArrayUtils.Ranges([9, 1, 7, 2, 5, 8]) do  // R     : [1-2] [5]   [7-9]
        DeleteItems(R.Start, R.Length);                   // Params: (1,2) (5,1) (7,3)

    Example 2:
      var X,Y: TRange;
      X.Clear; Y.Clear; // we will find min rectange containing all controls
      for I := 0 to ControlCount-1 do
      begin
        X.AddOrSet(Controls[I].Left, Controls[I].Left + Controls[I].Width);
        Y.AddOrSet(Controls[I].Top, Controls[I].Top + Controls[I].Height);
      end;
      R := TRectangle.Create(X.ValueMin,Y.ValueMin,X.ValueMax,Y.ValueMax);
  }
  TRange = record
  private
    FValueMin,FValueMax: integer;

    type
      TEnumerator = record
      private
        FCurValue, FMaxValue: integer;
      public
        constructor Create(const R: TRange);
        function MoveNext: boolean;
        function GetCurrentValue: integer;

        property Current: integer read GetCurrentValue;
      end;

    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetEmpty: boolean;
    function GetLength: integer;

  public

    constructor Create(AValueMin,AValueMax: integer); overload;
    function GetEnumerator: TEnumerator;

    { assign new range (Range[1,3] = Range[3,1]) }
    procedure Init(AValueMin,AValueMax: integer);

    { assign empty range (with no elements) }
    procedure Clear;

    { Assign new range or extend existing one (Range[1,3] = Range[3,1]) }
    procedure AddOrSet(AValueMin,AValueMax: integer);

    { Extend range (Range[1,3] = Range[3,1]) }
    procedure Add(AValueMin,AValueMax: integer);

    { Check if range has intersection with A }
    function Overlaps(const a: TRange): boolean;

    { Check if B contains all elements from A }
    class operator In(const a: TRange; b: TRange) : Boolean;
    class operator In(const a: integer; b: TRange) : Boolean;

    { [1,3] + [5,7] -> [1,7]}
    class operator Add(const a,b: TRange) : TRange;

    { Subtract may produce two ranges, it can't be implemented with single TRange as result
      class operator Subtract(const a,b: TRange) : TRange; }

    { [1,5] AND [4,7] = [4,5] }
    class operator LogicalAnd(const a,b: TRange) : TRange;

    { Similar to ADD, but returns empty range if A and B has no intersection }
    class operator LogicalOr(const a,b: TRange) : TRange;

    { XOR may produce two ranges, it can't be implemented with single TRange as result
      class operator LogicalXor(const a,b: TRange) : TRange; }

    { [1,5] -> [2,6] }
    class operator Inc(const a: TRange) : TRange;
    { [1,5] -> [0,4] }
    class operator Dec(const a: TRange) : TRange;

    class operator Equal(const a,b: TRange) : Boolean;
    class operator NotEqual(const a,b: TRange) : Boolean;
    class operator GreaterThanOrEqual(const a,b: TRange) : Boolean;
    class operator GreaterThan(const a,b: TRange) : Boolean;
    class operator LessThan(const a,b: TRange) : Boolean;
    class operator LessThanOrEqual(const a,b: TRange) : Boolean;

    { [1,3] -> "[1,3]" }
    property AsString: string read GetAsString write SetAsString;
    property Empty: boolean read GetEmpty;
    property Length: integer read GetLength;

    { synonyms to FValueMin/FValueMax }
    property Start: integer read FValueMin write FValueMin;
    property Finish: integer read FValueMax write FValueMax;
    property Left: integer read FValueMin write FValueMin;
    property Right: integer read FValueMax write FValueMax;
    property ValueMin: integer read FValueMin write FValueMin;
    property ValueMax: integer read FValueMax write FValueMax;
  end;

  TRangeEnumerator = record
  private
    Src: TArray<integer>;
    Cur: integer;

    function GetCurrent: TRange;
  public
    constructor Create(Src: TArray<integer>);
    function MoveNext: boolean;

    property Current: TRange read GetCurrent;
  end;

  TRangeEnumerable = record
  private
    Src: TArray<integer>;
  public
    constructor Create(Src: TArray<integer>);
    function GetEnumerator: TRangeEnumerator;
  end;

  TRangeComparer = class(TComparer<TRange>)
  protected
    class var
      FOrdinalComparer: IComparer<TRange>;
  public
    class function Ordinal: IComparer<TRange>; reintroduce;
    function Compare(const Left, Right: TRange): Integer; override;
  end;

  { Generic singleton pattern. Creates instance on first request. Example:
      Type
        TOptions = class(TSingleton<TStringList>)
        protected
          class function CreateInstance: TStringList; override;
        end;

        class function TOptions.CreateInstance: TStringList;
        begin
          result := TStringList.Create;
        end;

      procedure Test;
      begin
        TOptions.Ordinal.Add('test');
      end;
  }
  TSingleton<T: class> = class abstract
  protected
    class var
      FOrdinal: T;

    { must be overridden by descendant class }
    class function CreateInstance: T; virtual; abstract;

    class destructor DestroyClass;

  public

    { We can't use "property Ordinal" here, because Delphi (10.2 Seattle at least) allows to map
      class properties only to static class methods. But static method doesn't have access to class info
      and can not call correct virtual method (we need to call CreateInstance from GetOrdinal).
      That is why we have to use function Ordinal + procedure SetOrdinal instead of one property Ordinal }
    class function Ordinal: T;
    class procedure SetOrdinal(const Value: T); static;
  end;

implementation

uses
  adot.Strings,
  adot.Tools;

{ TCompound<TypeA, TypeB> }

constructor TCompound<TypeA, TypeB>.Create(const A: TypeA; const B: TypeB);
begin
  Init(A,B);
end;

procedure TCompound<TypeA, TypeB>.Init(const A: TypeA; const B: TypeB);
begin
  Self := Default(TCompound<TypeA, TypeB>);
  Self.A := A;
  Self.B := B;
end;

{ TCompound<TypeA, TypeB, TypeC> }

constructor TCompound<TypeA, TypeB, TypeC>.Create(const A: TypeA; const B: TypeB; const C: TypeC);
begin
  Init(A,B,C);
end;

procedure TCompound<TypeA, TypeB, TypeC>.Init(const A: TypeA; const B: TypeB; const C: TypeC);
begin
  Self := Default(TCompound<TypeA, TypeB, TypeC>);
  Self.A := A;
  Self.B := B;
  Self.C := C;
end;

{ TCompoundEqualityComparer<TypeA, TypeB> }

constructor TCompoundEqualityComparer<TypeA, TypeB>.Create(
  ComparerA: IEqualityComparer<TypeA>;
  ComparerB: IEqualityComparer<TypeB>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  if FComparerA=nil then
    FComparerA := TComparerUtils.DefaultEqualityComparer<TypeA>;
  if FComparerB=nil then
    FComparerB := TComparerUtils.DefaultEqualityComparer<TypeB>;
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
        Exit(inherited Default);
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
  result := THashUtils.Mix(FComparerA.GetHashCode(Value.A), FComparerB.GetHashCode(Value.B));
end;

{ TCompoundEqualityComparer<TypeA, TypeB, TypeC> }

constructor TCompoundEqualityComparer<TypeA, TypeB, TypeC>.Create(
  ComparerA: IEqualityComparer<TypeA>;
  ComparerB: IEqualityComparer<TypeB>;
  ComparerC: IEqualityComparer<TypeC>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  FComparerC := ComparerC;
  if FComparerA=nil then
    FComparerA := TComparerUtils.DefaultEqualityComparer<TypeA>;
  if FComparerB=nil then
    FComparerB := TComparerUtils.DefaultEqualityComparer<TypeB>;
  if FComparerC=nil then
    FComparerC := TComparerUtils.DefaultEqualityComparer<TypeC>;
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
          Exit(inherited Default);
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
  result := THashUtils.Mix(
    FComparerA.GetHashCode(Value.A),
    FComparerB.GetHashCode(Value.B),
    FComparerC.GetHashCode(Value.C)
  );
end;

{ TCompoundComparer<TypeA, TypeB> }

constructor TCompoundComparer<TypeA, TypeB>.Create(ComparerA: IComparer<TypeA>; ComparerB: IComparer<TypeB>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  if FComparerA=nil then
    FComparerA := TComparerUtils.DefaultComparer<TypeA>;
  if FComparerB=nil then
    FComparerB := TComparerUtils.DefaultComparer<TypeB>;
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
        Exit(inherited Default);
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
  ComparerA: IComparer<TypeA>;
  ComparerB: IComparer<TypeB>;
  ComparerC: IComparer<TypeC>);
begin
  FComparerA := ComparerA;
  FComparerB := ComparerB;
  FComparerC := ComparerC;
  if FComparerA=nil then
    FComparerA := TComparerUtils.DefaultComparer<TypeA>;
  if FComparerB=nil then
    FComparerB := TComparerUtils.DefaultComparer<TypeB>;
  if FComparerC=nil then
    FComparerC := TComparerUtils.DefaultComparer<TypeC>;
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
          Exit(inherited Default);
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
  const Key: TSetDictKey; Action: TCollectionNotification);
begin
  inherited;
  if OwnsKeys and (Action = TCollectionNotification.cnRemoved) then
    PObject(@Key)^.DisposeOf;
end;

{ TSetClass<TValue> }

constructor TSetClass<TValue>.Create(ACapacity: integer = 0; AComparer: IEqualityComparer<TValue> = nil);
begin
  inherited Create;
  FSet := TSetObjectDictionary<TValue, TEmptyRec>.Create(ACapacity, AComparer);
  FComparerCopy := AComparer;
end;

constructor TSetClass<TValue>.Create(const AValues: array of TValue; AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TSetClass<TValue>.Create(const AValues: TEnumerable<TValue>; AComparer: IEqualityComparer<TValue> = nil);
begin
  Create(0, AComparer);
  Add(AValues);
end;

constructor TSetClass<TValue>.Create(const AOperands: TArray<TSetClass<TValue>>; ASetOp: TSetOp;
  AComparer: IEqualityComparer<TValue>);
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

function TSetClass<TValue>.GetAsArray: TArray<TValue>;
var
  i: Integer;
  Value: TValue;
begin
  SetLength(Result, Count);
  i := 0;
  for Value in Self do
  begin
    Result[i] := Value;
    inc(i);
  end;
end;

function TSetClass<TValue>.GetAsString: string;
var
  Arr: TArray<TValue>;
  Value: TValue;
  Buf: TStringBuffer;
begin
  Arr := AsArray;
  TArray.Sort<TValue>(Arr);
  Buf.Clear;
  for Value in Arr do
    Buf.Write(IfThen(Buf.Empty,'',' ') + TRttiUtils.ValueAsString<TValue>(Value));
  Result := Buf.Text;
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

function TMapClass<TKey, TValue>.GetAsString: string;
var
  Arr: TArray<TPair<TKey, TValue>>;
  KeyComparer: IComparer<TKey>;
  ValueComparer: IComparer<TValue>;
  PairComparer: IComparer<TPair<TKey, TValue>>;
  Pair: TPair<TKey, TValue>;
  i: Integer;
  Buf: TStringBuffer;
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
  Buf.Clear;
  for Pair in Arr do
    Buf.Write(
      IfThen(Buf.Empty,'',' ') +
      '(' +
      EscapeStrVal(TRttiUtils.ValueAsString<TKey>(Pair.Key)) +
      ', ' +
      EscapeStrVal(TRttiUtils.ValueAsString<TValue>(Pair.Value)) +
      ')'
    );
  Result := Buf.Text;
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

function TMultimapClass<TKey, TValue>.ContainsValues(const AKey: TKey; const AValues: array of TValue; AContainsCheckType: TContainsCheckType;
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

procedure TMultimapClass<TKey, TValue>.Add(const AKey: TKey; const AValues: array of TValue);
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
  result := THashUtils.Mix(FKeyComparer.GetHashCode(Value.Key), Value.Number);
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

function TSet<T>.GetEmpty: Boolean;
begin
  result := Count = 0;
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

procedure TSet<T>.CreateSet(ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
var
  C: IEqualityComparer<T>;
begin
  if AComparer=nil
    then C := TComparerUtils.DefaultEqualityComparer<T>
    else C := AComparer;
  FSetInt := TInterfacedObject<TSetClass<T>>.Create( TSetClass<T>.Create(ACapacity, C) );
end;

function TSet<T>.GetAsArray: TArray<T>;
begin
  Result := Readonly.AsArray;
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
begin
  Result := Readonly.AsString;
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

class operator TSet<T>.Implicit(const a : TArray<T>) : TSet<T>;
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

class operator TSet<T>.Explicit(const a : TArray<T>) : TSet<T>;
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
  Self := Default(TSet<T>);
end;

function TSet<T>.GetCollection: TEnumerable<T>;
begin
  result := Readonly;
end;

function TSet<T>.GetCount: integer;
begin
  result := ReadOnly.Count;
end;

constructor TSet<T>.Create(const V: TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

constructor TSet<T>.Create(const V: array of T; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
  Add(v);
end;

constructor TSet<T>.Create(ACapacity: integer; AComparer: IEqualityComparer<T> = nil);
begin
  CreateSet(ACapacity, AComparer);
end;

constructor TSet<T>.Create(V: TSet<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
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

constructor TSet<T>.Create(const V: array of TEnumerable<T>; ACapacity: integer = 0; AComparer: IEqualityComparer<T> = nil);
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

constructor TMap<TKey, TValue>.Create(const V: array of TPair<TKey, TValue>; ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

constructor TMap<TKey, TValue>.Create(ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
end;

constructor TMap<TKey, TValue>.Create(V: TMap<TKey, TValue>; ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

constructor TMap<TKey, TValue>.Create(const V: TEnumerable<TPair<TKey, TValue>>; ACapacity: integer; AComparer: IEqualityComparer<TKey>);
begin
  CreateMap(ACapacity, AComparer);
  Add(V);
end;

procedure TMap<TKey, TValue>.CreateMap(ACapacity: integer; AComparer: IEqualityComparer<TKey>);
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

{ TAutoFreeCollection }

class function TAutoFreeCollection.Create: TAutoFreeCollection;
begin
  result := Default(TAutoFreeCollection);
end;

procedure TAutoFreeCollection.Clear;
begin
  Self := Default(TAutoFreeCollection);
end;

function TAutoFreeCollection.Add<T>(AObject: T): T;
begin
  if FGuard=nil then
    FGuard := TAutoFreeCollectionImpl.Create;
  FGuard.Add(AObject);
  result := AObject;
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
  result := Default(TAutoFree<T>);
end;

class function TAutoFree<T>.Create(const AValue: T): TAutoFree<T>;
begin
  result := Default(TAutoFree<T>);
  result.Value := AValue;
end;

procedure TAutoFree<T>.Clear;
begin
  Self := Default(TAutoFree<T>);
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

class function TAuto<T>.Create: TAuto<T>;
begin
  result := Default(TAuto<T>);
end;

class function TAuto<T>.Create(const AValue: T): TAuto<T>;
begin
  result := Default(TAuto<T>);
  result.Value := AValue;
end;

procedure TAuto<T>.Clear;
begin
  Self := Default(TAuto<T>);
end;

function TAuto<T>.CreateInstance: T;
begin
  result := TRttiUtils.CreateInstance<T>;
end;

class operator TAuto<T>.Equal(const ALeft: TAuto<T>; const ARight: T): Boolean;
begin
  result := ALeft.Value=ARight;
end;

class operator TAuto<T>.Equal(const ALeft, ARight: TAuto<T>): Boolean;
begin
  result := ALeft.Value=ARight.Value;
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

{ TCacheClass<TKey, TValue> }

constructor TCacheClass<TKey, TValue>.Create;
begin
  Create(DefaultSize);
end;

constructor TCacheClass<TKey, TValue>.Create(AMaxSize: Integer);
begin
  MaxSize := AMaxSize;
  Cache := TDictionary<TKey,TValue>.Create;
end;

destructor TCacheClass<TKey, TValue>.Destroy;
begin
  FreeAndNil(Cache);
end;

procedure TCacheClass<TKey, TValue>.Add(K: TKey; V: TValue; ASize: longint);
begin
  if Size>=MaxSize then
  begin
    Size := 0;
    Cache.Clear;
  end;
  Cache.Add(K,V);
  inc(Size, ASize);
end;

function TCacheClass<TKey, TValue>.TryGetValue(K: TKey; var V: TValue): boolean;
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

{ TComparerUtils }

class function TComparerUtils.Compare(const Left, Right: integer): integer;
begin
  if Left < Right then result := -1 else
    if Left > Right then result := 1 else
      result := 0;
end;

class function TComparerUtils.Compare(const Left, Right: double): integer;
begin
  if Left < Right then result := -1 else
    if Left > Right then result := 1 else
      result := 0;
end;

class function TComparerUtils.Compare(const Left, Right: boolean): integer;
begin
  if Left then
    if Right
      then result := 0
      else result := 1
  else
    if Right
      then result := -1
      else result := 0;
end;

class function TComparerUtils.Compare<T>(const Left, Right: TArray<T>; ItemComparer: IComparer<T>): integer;
var
  I: Integer;
begin
  result := Length(Left) - Length(Right);
  for I := 0 to Length(Left)-1 do
    if result = 0
      then result := ItemComparer.Compare(Left[I], Right[I])
      else Break;
end;

class function TComparerUtils.Compare<T>(const Left, Right: TList<T>; ItemComparer: IComparer<T>): integer;
var
  I: Integer;
begin
  if Left = Right then result := 0 else
    if Left = nil then result := -1 else
      if Right = nil then result := 1 else
      begin
        result := Left.Count - Right.Count;
        for I := 0 to Left.Count-1 do
          if result = 0
            then result := ItemComparer.Compare(Left[I], Right[I])
            else Break;
      end;
end;

class function TComparerUtils.Compare(const Left, Right: string; CaseInsensitive: boolean = True): integer;
begin
  if CaseInsensitive
    then result := CompareText(Left, Right)
    else result := CompareStr(Left, Right);
end;

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

class function TComparerUtils.DefaultArithmetic<T>: IArithmetic<T>;
begin
  result := TArithmeticUtils<T>.DefaultArithmetic;
end;

class function TComparerUtils.DefaultComparer<T>: IComparer<T>;
begin
  if TypeInfo(T) = TypeInfo(string) then
    result := IComparer<T>( IComparer<string>(TIStringComparer.Ordinal) )
  else
  if TypeInfo(T) = TypeInfo(TRange) then
    result := IComparer<T>( IComparer<TRange>(TRangeComparer.Ordinal) )
  else
    result := TComparer<T>.Default;
end;

class function TComparerUtils.DefaultEqualityComparer<T>: IEqualityComparer<T>;
begin
  if TypeInfo(T) = TypeInfo(string) then
    result := IEqualityComparer<T>( IEqualityComparer<string>(TIStringComparer.Ordinal) )
  { default equality comparer is ok for TRange
  else
  if TypeInfo(T) = TypeInfo(TRange) then
    result := IEqualityComparer<T>( IEqualityComparer<TRange>(TRangeComparer.Ordinal) )}
  else
    result := TEqualityComparer<T>.Default;
end;

class function TComparerUtils.Equal<T>(A: TEnumerable<T>; const B: TArray<T>): boolean;
begin
  result := Equal<T>(A, B, DefaultComparer<T>);
end;

class function TComparerUtils.Equal<T>(A: TEnumerable<T>; const B: TArray<T>; Comparer: IComparer<T>): boolean;
var
  Enum: TEnumerator<T>;
  I: Integer;
begin
  result := True;
  Enum := A.GetEnumerator;
  for I := Low(B) to High(B) do
    begin
      result := Enum.MoveNext and (Comparer.Compare(Enum.Current, B[I]) = 0);
      if not result then
        Break;
    end;
  if result then
    result := not Enum.MoveNext;
  Sys.FreeAndNil(Enum);
end;

class function TComparerUtils.FindEqualityComparer<T>(Comparer: IComparer<T>): IEqualityComparer<T>;
begin
  if Comparer=nil
    then result := DefaultEqualityComparer<T>
    else result := TEqualityByComparer<T>.Create(Comparer);
end;

class function TComparerUtils.Equal<T>(A, B: TEnumerable<T>): boolean;
begin
  result := Equal<T>(A,B, DefaultComparer<T>);
end;

class function TComparerUtils.Equal<T>(A, B: TEnumerable<T>; Comparer: IComparer<T>): boolean;
var
  EnumA, EnumB: TEnumerator<T>;
begin
  EnumA := A.GetEnumerator;
  EnumB := B.GetEnumerator;
  while True do
    if EnumA.MoveNext then
      if EnumB.MoveNext and (Comparer.Compare(EnumA.Current, EnumB.Current) = 0) then
        Continue
      else
      begin
        result := False;
        Break;
      end
    else
    begin
      result := not EnumB.MoveNext;
      Break;
    end;
  Sys.FreeAndNil(EnumA);
  Sys.FreeAndNil(EnumB);
end;

{ TArr<T>.TEnumerator }

constructor TArr<T>.TEnumerator.Create(const Items: TArray<T>; ACount: integer);
begin
  Self := Default(TEnumerator);
  Self.Items := Items;
  Self.Count := ACount;
  Self.Pos := 0;
end;

function TArr<T>.TEnumerator.GetCurrent: T;
begin
  result := Items[Pos-1];
end;

function TArr<T>.TEnumerator.MoveNext: Boolean;
begin
  result := Pos < Count;
  if result then
    inc(Pos);
end;

{ TArr<T> }

constructor TArr<T>.Create(ACapacity: integer);
begin
  Self := Default(TArr<T>);
  Capacity := ACapacity;
end;

constructor TArr<T>.Create(ADst: TArray<T>);
begin
  Self := Default(TArr<T>);
  Items := ADst;
  Count := High(Items)-Low(Items)+1;
end;

function TArr<T>.Add: integer;
begin
  if Count>=Capacity then
    Grow;
  result := FCount;
  inc(FCount);
end;

function TArr<T>.Add(const Value: T): integer;
begin
  result := Add;
  Items[result] := Value;
end;

procedure TArr<T>.Add(const Value: TArray<T>);
var
  I: Integer;
begin
  I := Count + System.Length(Value);
  if I > Capacity then
    Capacity := I;
  for I := Low(Value) to High(Value) do
    Add(Value[I]);
end;

procedure TArr<T>.Add(const Value: TEnumerable<T>);
var
  V: T;
begin
  for V in Value do
    Add(V);
end;

procedure TArr<T>.Clear;
begin
  Self := Default(TArr<T>);
end;

function TArr<T>.Copy: TArr<T>;
begin
  result.Clear;
  result.Items := TArrayUtils.Copy<T>(Items, 0, Count);
  result.FCount := Count;
end;

procedure TArr<T>.Delete(ItemIndex: integer);
var
  I: Integer;
begin
  Assert((ItemIndex>=0) and (ItemIndex<FCount));
  for I := ItemIndex to Count-2 do
    Items[I] := Items[I+1];
  Dec(FCount);
  Items[FCount] := Default(T);
end;

procedure TArr<T>.Exchange(Index1, Index2: integer);
var Value: T;
begin
  Value := Items[Index1];
  Items[Index1] := Items[Index2];
  Items[Index2] := Value;
end;

function TArr<T>.ToArray: TArray<T>;
begin
  TrimExcess;
  result := Items;
end;

procedure TArr<T>.DeleteLast;
begin
  Assert(FCount>=0);
  Dec(FCount);
  Items[FCount] := Default(T);
end;

function TArr<T>.ExtractLast: T;
begin
  Assert(FCount>=0);
  Dec(FCount);
  result := Items[FCount];
  Items[FCount] := Default(T);
end;

procedure TArr<T>.Grow;
begin
  if Capacity < 4 then
    Capacity := Capacity+1
  else
  if Capacity < 64 then
    Capacity := 64
  else
    Capacity := Capacity * 2;
end;

function TArr<T>.IndexOf(const Value: T; Comparer: IComparer<T>): integer;
begin
  if not FindFirst(Value, Result, Comparer) then
    result := -1;
end;

procedure TArr<T>.Init;
begin
  Self := Default(TArr<T>);
end;

function TArr<T>.IndexOf(const Value: T): integer;
begin
  if not FindFirst(Value, Result) then
    result := -1;
end;

function TArr<T>.FindFirst(const Value: T; var Index: integer): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, TComparerUtils.DefaultComparer<T>);
end;

function TArr<T>.FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, Comparer);
end;

function TArr<T>.FindNext(const Value: T; var Index: integer): boolean;
begin
  result := FindNext(Value, Index, TComparerUtils.DefaultComparer<T>);
end;

function TArr<T>.FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
var
  I: Integer;
begin
  if Comparer = nil then
    Comparer := TComparerUtils.DefaultComparer<T>;
  for I := Index+1 to Count-1 do
    if Comparer.Compare(Items[I], Value)=0 then
    begin
      Index := I;
      Exit(True);
    end;
  result := False;
end;

function TArr<T>.Insert(Index: integer; const Value: T): integer;
begin
  for result := Add downto Index+1 do
    Items[result] := Items[result-1];
  result := Index;
  Items[result] := Value;
end;

procedure TArr<T>.Move(SrcIndex, DstIndex: integer);
var
  I: integer;
  Value: T;
begin
  if SrcIndex < DstIndex then
  begin
    {      src   dst
      1 2 [3] 4 [5] 6 7 }
    Value := Items[SrcIndex];
    for I := SrcIndex to DstIndex-1 do
      Items[I] := Items[I+1];
    Items[DstIndex] := Value;
  end
  else
  if SrcIndex > DstIndex then
  begin
    {      dst   src
      1 2 [3] 4 [5] 6 7 }
    Value := Items[SrcIndex];
    for I := SrcIndex downto DstIndex+1 do
      Items[I] := Items[I-1];
    Items[DstIndex] := Value;
  end;
end;

function TArr<T>.GetCapacity: integer;
begin
  result := System.Length(Items);
end;

procedure TArr<T>.SetCapacity(ACapacity: integer);
begin
  Assert(ACapacity>=Count);
  SetLength(Items, ACapacity);
end;

function TArr<T>.GetEmpty: Boolean;
begin
  Result := FCount <= 0;
end;

function TArr<T>.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(Items, Count);
end;

function TArr<T>.GetFirst: T;
begin
  Result := Items[0];
end;

procedure TArr<T>.SetFirst(const Value: T);
begin
  Items[0] := Value;
end;

function TArr<T>.GetLast: T;
begin
  Result := Items[Count-1];
end;

function TArr<T>.GetTotalSizeBytes: int64;
begin
  result := (High(Items)-Low(Items)+1)*SizeOf(T);
end;

procedure TArr<T>.SetLast(const Value: T);
begin
  Items[Count-1] := Value;
end;

function TArr<T>.GetItem(ItemIndex: integer): T;
begin
  {$IF Defined(Debug)} Assert((ItemIndex >= 0) and (ItemIndex < Count)); {$ENDIF}
  result := Items[ItemIndex];
end;

procedure TArr<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  {$IF Defined(Debug)} Assert((ItemIndex >= 0) and (ItemIndex < Count)); {$ENDIF}
  Items[ItemIndex] := Value;
end;

procedure TArr<T>.TrimExcess;
begin
  if Capacity>Count then
    Capacity := Count;
end;

procedure TArr<T>.SetCount(ACount: integer);
var
  I: Integer;
begin
  for I := ACount to Count-1 do
    Items[I] := Default(T);
  FCount := ACount;
  if ACount > Capacity then
    Capacity := ACount;
end;

procedure TArr<T>.Sort;
begin
  TArray.Sort<T>(Items, TComparerUtils.DefaultComparer<T>, 0,Count);
end;

procedure TArr<T>.Sort(Comparer: IComparer<T>);
begin
  Sort(Comparer, 0, Count);
end;

procedure TArr<T>.Sort(Comparer: IComparer<T>; AIndex, ACount: Integer);
begin
  if Comparer=nil then
    Comparer := TComparerUtils.DefaultComparer<T>;
  TArray.Sort<T>(Items, Comparer, AIndex, ACount);
end;

procedure TArr<T>.Sort(Comparer: TFunc<T, T, integer>);
begin
  Sort(Comparer, 0, Count);
end;

procedure TArr<T>.Sort(Comparer: TFunc<T, T, integer>; AIndex, ACount: Integer);
var
  C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(
    function (const A,B: T): integer
    begin
      result := Comparer(A,B);
    end);
  TArray.Sort<T>(Items, C, AIndex, ACount);
end;

function TArr<T>.BinarySearch(const Item: T; out FoundIndex: Integer): Boolean;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, TComparerUtils.DefaultComparer<T>, 0, Count);
end;

function TArr<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, Comparer, 0, Count);
end;

function TArr<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, Comparer, AIndex,ACount);
end;

procedure TArr<T>.Add(const Value: TArr<T>);
var
  I: Integer;
begin
  for I := 0 to Value.Count-1 do
    Items[Add] := Value[I];
end;

{ TEnumerableExt<T> }

class procedure TEnumerableExt<T>.DoSaveToFile(Src: TEnumerable<T>; const FileName: string; Encoding: TEncoding; MemStream: boolean);
var
  S: TStream;
begin
  if MemStream
    then S := TMemoryStream.Create
    else S := TFileStream.Create(FileName, System.Classes.fmCreate);
  try
    DoSaveToStream(Src, S, Encoding);
    if MemStream then
      TMemoryStream(S).SaveToFile(FileName);
  finally
    Sys.FreeAndNil(S);
  end;
end;

class procedure TEnumerableExt<T>.DoSaveToStream(Src: TEnumerable<T>; Dst: TStream; Encoding: TEncoding);
var
  I: Integer;
  S: string;
  B: TArray<byte>;
  V: T;
  N: boolean;
begin
  if Encoding = nil then
    Encoding := TEncoding.UTF8;
  B := Encoding.GetPreamble;
  Dst.WriteBuffer(B, System.Length(B));
  N := False;
  for V in Src do
  begin
    S := IfThen(N,#13#10,'') + TRttiUtils.ValueAsString<T>(V);
    B := Encoding.GetBytes(S);
    Dst.WriteBuffer(B, System.Length(B));
    N := True;
  end;
end;

procedure TEnumerableExt<T>.SaveToFile(const FileName: string; Encoding: TEncoding; MemStream: boolean);
begin
  DoSaveToFile(Self, Filename, Encoding, MemStream);
end;

procedure TEnumerableExt<T>.SaveToStream(Dst: TStream; Encoding: TEncoding);
begin
  DoSaveTostream(Self, dst, Encoding);
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
  Self := Default(TKeyEnumerator);
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
  Self := Default(TKeyCollection);
  Self.PairEnumerator := PairEnumerator;
end;

function TBinaryHeapClass<TKey, TValue>.TKeyCollection.GetEnumerator: TKeyEnumerator;
begin
  result := TKeyEnumerator.Create(PairEnumerator);
end;

{ TBinaryHeapClass<TKey, TValue> }

constructor TBinaryHeapClass<TKey, TValue>.Create(ACapacity: integer; AComparer: IComparer<TKey>);
begin
  inherited Create;
  Capacity := ACapacity;
  if AComparer=nil then
    FComparer := TComparerUtils.DefaultComparer<TKey>
  else
    FComparer := AComparer;
end;

constructor TBinaryHeapClass<TKey, TValue>.Create(const ACollection: TEnumerable<TPair<TKey, TValue>>;
  ACapacity: integer; AComparer: IComparer<TKey>);
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
  if Value
    then include(FOwnerships, doOwnsKeys)
    else exclude(FOwnerships, doOwnsKeys);
end;

procedure TBinaryHeapClass<TKey, TValue>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<TValue> then
    raise Exception.Create('Generic type is not a class.');
  if Value
    then include(FOwnerships, doOwnsValues)
    else exclude(FOwnerships, doOwnsValues);
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

constructor TBinaryHeapClass<TKey>.Create(const ACollection: TEnumerable<TKey>; ACapacity: integer; AComparer: IComparer<TKey>);
begin
  Create(ACapacity, AComparer);
  Add(ACollection);
end;

constructor TBinaryHeapClass<TKey>.Create(ACapacity: integer; AComparer: IComparer<TKey>);
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

function TRingClass<T>.GetIsFull: boolean;
begin
  result := Count = Capacity;
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

function TRingClass<T>.ExtractTail: T;
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
  Values.Clear;
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

{ TTreeArrayClass<T>.TEnumerator }

constructor TTreeArrayClass<T>.TEnumerator.Create(const ANodes: TArr<TNode>);
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
  result := TEnumerator.Create(Nodes);
end;

function TTreeArrayClass<T>.GetSubtreeCollection(StaringNode: integer): TSubtreeCollection;
begin
  result := TSubtreeCollection.Create(Nodes, StaringNode);
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
  result := TValuesCollection.Create(Nodes);
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

constructor TTreeArrayClass<T>.TSubtreeEnumerator.Create(const ANodes: TArr<TNode>; ARoot: integer);
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

constructor TTreeArrayClass<T>.TSubtreeCollection.Create(const ANodes: TArr<TNode>; ARoot: integer);
begin
  Self := Default(TSubtreeCollection);
  Nodes := ANodes;
  Root := ARoot;
end;

function TTreeArrayClass<T>.TSubtreeCollection.GetEnumerator: TSubtreeEnumerator;
begin
  result := TSubtreeEnumerator.Create(Nodes, Root);
end;

{ TTreeArrayClass<T>.TValuesEnumerator }

constructor TTreeArrayClass<T>.TValuesEnumerator.Create(const ANodes: TArr<TNode>);
begin
  Self := Default(TValuesEnumerator);
  Enum := TEnumerator.Create(ANodes);
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

constructor TTreeArrayClass<T>.TValuesCollection.Create(const ANodes: TArr<TNode>);
begin
  Self := Default(TValuesCollection);
  Nodes := ANodes;
end;

function TTreeArrayClass<T>.TValuesCollection.GetEnumerator: TValuesEnumerator;
begin
  result := TValuesEnumerator.Create(Nodes);
end;

{ TVector2D<T>.TCollectionEnumerator }

constructor TVector2D<T>.TEnumerator.Create(const Rows: TArr<TArr<T>>);
begin
  inherited Create;
  Self.Rows := Rows;
  X := -1;
  Y := 0;
end;

function TVector2D<T>.TEnumerator.DoMoveNext: Boolean;
begin
  if Y >= Rows.Count then
    Exit(False);

  inc(X);
  if X < Rows[Y].Count then
    Exit(True);

  repeat
    inc(Y);
    if Y >= Rows.Count then
      Exit(False);
  until Rows[Y].Count > 0;
  X := 0;
  Result := True;
end;

function TVector2D<T>.TEnumerator.DoGetCurrent: T;
begin
  result := Rows[Y][X];
end;

{ TVector2D<T>.TCollection }

constructor TVector2D<T>.TCollection.Create(const Rows: TArr<TArr<T>>);
begin
  inherited Create;
  Self.Rows := Rows;
end;

function TVector2D<T>.TCollection.DoGetEnumerator: TEnumerator<T>;
begin
  result := TEnumerator.Create(Rows);
end;

{ TVector2D<T> }

constructor TVector2D<T>.Create(Width, Height: integer);
var
  Y: integer;
  V: TArr<T>;
begin
  Self := Default(TVector2D<T>);
  Rows := TArr<TArr<T>>.Create(Height);
  Rows.Count := Height;
  for Y := 0 to Height-1 do
  begin
    Rows[Y] := TArr<T>.Create(Width);
    Rows.Items[Y].Count := Width;
  end;
end;

procedure TVector2D<T>.Clear;
begin
  Self := Default(TVector2D<T>);
end;

function TVector2D<T>.Collection: IInterfacedObject<TEnumerable<T>>;
begin
  result := TInterfacedObject<TEnumerable<T>>.Create(TCollection.Create(Rows));
end;

function TVector2D<T>.Add(y: integer): integer;
begin
  result := Rows.Items[y].Add;
end;

function TVector2D<T>.Add(y: integer; const Value: T): integer;
begin
  result := Rows.Items[y].Add(Value);
end;

function TVector2D<T>.Add(y: integer; const Values: TEnumerable<T>): integer;
var
  Value: T;
begin
  result := -1;
  for Value in Values do
    result := Self.Rows.Items[y].Add(Value);
end;

function TVector2D<T>.Add(y: integer; const Values: TArray<T>): integer;
var
  I: Integer;
begin
  result := -1;
  for I := Low(Values) to High(Values) do
    result := Self.Rows.Items[y].Add(Values[I]);
end;

function TVector2D<T>.AddRow: integer;
begin
  result := Rows.Add;
end;

function TVector2D<T>.GetValue(x, y: integer): T;
begin
  result := Rows.Items[y].Items[x];
end;

procedure TVector2D<T>.SetValue(x, y: integer; const Value: T);
begin
  Rows.Items[y].Items[x] := Value;
end;

function TVector2D<T>.GetRowCount: integer;
begin
  result := Rows.Count;
end;

procedure TVector2D<T>.SetRowCount(const Value: integer);
begin
  Rows.Count := Value;
end;

function TVector2D<T>.GetWidth(y: integer): integer;
begin
  result := Rows.Items[y].Count;
end;

procedure TVector2D<T>.SetWidth(y: integer; const Value: integer);
begin
  Rows.Items[y].Count := Value;
end;

{ TRange }

constructor TRange.Create(AValueMin, AValueMax: integer);
begin
  Init(AValueMin, AValueMax);
end;

procedure TRange.Init(AValueMin, AValueMax: integer);
begin
  Self := Default(TRange);
  if AValueMin <= AValueMax then
  begin
    ValueMin := AValueMin;
    ValueMax := AValueMax;
  end
  else
  begin
    ValueMin := AValueMax;
    ValueMax := AValueMin;
  end;
end;

procedure TRange.Add(AValueMin, AValueMax: integer);
begin
  if AValueMin <= AValueMax then
  begin
    ValueMin := Min(ValueMin, AValueMin);
    ValueMax := Max(ValueMax, AValueMax);
  end
  else
  begin
    ValueMin := Min(ValueMin, AValueMax);
    ValueMax := Max(ValueMax, AValueMin);
  end
end;

procedure TRange.AddOrSet(AValueMin, AValueMax: integer);
begin
  if Empty
    then Init(AValueMin, AValueMax)
    else Add(AValueMin, AValueMax);
end;

procedure TRange.Clear;
begin
  Self := Default(TRange);
  dec(FValueMax);
end;

function TRange.GetEmpty: boolean;
begin
  result := ValueMax < ValueMin;
end;

function TRange.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(Self);
end;

function TRange.GetLength: integer;
begin
  if ValueMax >= ValueMin then
    result := ValueMax-ValueMin+1
  else
    result := 0;
end;

function TRange.GetAsString: string;
begin
  result := format('[%d,%d]', [ValueMin,ValueMax]);
end;

procedure TRange.SetAsString(const Value: string);
var
  a,b,c,i,j: integer;
begin
  a := Value.IndexOf('[');
  if a >= 0 then
  begin
    b := Value.IndexOf(',', a);
    if b >= 0 then
    begin
      c := Value.IndexOf(']', b);
      if (c >= 0) and TryStrToInt(Value.Substring(a+1,b-a-1), i) and TryStrToInt(Value.Substring(b,c-b-1), j) then
      begin
        Init(i, j);
        Exit;
      end;
    end;
  end;
  Clear;
end;

class operator TRange.Inc(const a: TRange): TRange;
begin
  result := TRange.Create(a.ValueMin+1, a.ValueMax+1);
end;

class operator TRange.Dec(const a: TRange): TRange;
begin
  result := TRange.Create(a.ValueMin-1, a.ValueMax-1);
end;

class operator TRange.Add(const a, b: TRange): TRange;
begin
  result := TRange.Create(Min(a.ValueMin, b.ValueMin), Max(a.ValueMax, b.ValueMax));
end;

class operator TRange.LogicalAnd(const a, b: TRange): TRange;
begin
  { [...XXX]
       [XXX...]   -> [XXX] }
  if a.Overlaps(b) then
    result.Init(Max(a.ValueMin, b.ValueMin), Min(a.ValueMax, b.ValueMax))
  else
    result.Clear;
end;

class operator TRange.LogicalOr(const a, b: TRange): TRange;
begin
  { [...XXX]
       [XXX...]   -> [...XXX...]
    If a and b are overlapped, then A or B = A + B, otherwise A or B = empty unlike A + B }
  if a.Overlaps(b) then
    result.Init(Min(a.ValueMin, b.ValueMin), Max(a.ValueMax, b.ValueMax))
  else
    result.Clear;
end;

function TRange.Overlaps(const a: TRange): boolean;
begin
  result := (ValueMin <= a.ValueMax) and (a.ValueMin <= ValueMax);
end;

class operator TRange.In(const a: TRange; b: TRange): Boolean;
begin
  result := (a.ValueMin >= b.ValueMin) and (a.ValueMax <= b.ValueMax);
end;

class operator TRange.In(const a: integer; b: TRange): Boolean;
begin
  result := (a >= b.ValueMin) and (a <= b.ValueMax);
end;

class operator TRange.Equal(const a, b: TRange): Boolean;
begin
  result := (a.ValueMin=b.ValueMin) and (a.ValueMax=b.ValueMax);
end;

class operator TRange.NotEqual(const a, b: TRange): Boolean;
begin
  result := not (a=b);
end;

class operator TRange.LessThan(const a, b: TRange): Boolean;
begin
  if a.ValueMin < b.ValueMin then
    result := True
  else
  if a.ValueMin > b.ValueMin then
    result := False
  else
    result := a.Length < b.Length;
end;

class operator TRange.GreaterThanOrEqual(const a, b: TRange): Boolean;
begin
  result := not (a < b);
end;

class operator TRange.GreaterThan(const a, b: TRange): Boolean;
begin
  if a.ValueMin > b.ValueMin then
    result := True
  else
  if a.ValueMin < b.ValueMin then
    result := False
  else
    result := a.Length > b.Length;
end;

class operator TRange.LessThanOrEqual(const a, b: TRange): Boolean;
begin
  result := not (a > b);
end;

{ TRange.TEnumerator }

constructor TRange.TEnumerator.Create(const R: TRange);
begin
  FCurValue := R.ValueMin;
  FMaxValue := R.ValueMax;
end;

function TRange.TEnumerator.MoveNext: boolean;
begin
  result := FCurValue <= FMaxValue;
  if result then
    inc(FCurValue);
end;

function TRange.TEnumerator.GetCurrentValue: integer;
begin
  result := FCurValue-1;
end;

{ TRangeEnumerator }

constructor TRangeEnumerator.Create(Src: TArray<integer>);
begin
  Self := Default(TRangeEnumerator);
  Self.Src := Src;
  TArray.Sort<integer>(Self.Src);
end;

function TRangeEnumerator.MoveNext: boolean;
var
  L: Integer;
begin
  L := Length(Src);
  result := Cur < L;
  if result then
    repeat
      inc(Cur);
    until (Cur >= L) or (Src[Cur] <> Src[Cur-1]+1);
end;

function TRangeEnumerator.GetCurrent: TRange;
var
  I: integer;
begin
  I := Cur-1;
  while (I > 0) and (Src[I]=Src[I-1]+1) do
    dec(I);
  result.Init(Src[I], Src[Cur-1]);
end;

{ TRangeEnumerable }

constructor TRangeEnumerable.Create(Src: TArray<integer>);
begin
  Self := Default(TRangeEnumerable);
  Self.Src := Src;
end;

function TRangeEnumerable.GetEnumerator: TRangeEnumerator;
begin
  result := TRangeEnumerator.Create(Src);
end;

{ TRangeComparer }

class function TRangeComparer.Ordinal: IComparer<TRange>;
begin
  if FOrdinalComparer = nil then
    FOrdinalComparer := TRangeComparer.Create;
end;

function TRangeComparer.Compare(const Left, Right: TRange): Integer;
begin
  if Left < Right then
    result := -1
  else
  if Left = Right then
    result := 0
  else
    result := 1;
end;

{ TSingleton<T> }

class destructor TSingleton<T>.DestroyClass;
begin
  FreeAndNil(FOrdinal);
end;

class function TSingleton<T>.Ordinal: T;
begin
  if FOrdinal = nil then
    FOrdinal := CreateInstance;
  result := FOrdinal;
end;

class procedure TSingleton<T>.SetOrdinal(const Value: T);
begin
  if Value = FOrdinal then
    Exit;
  FreeAndNil(FOrdinal);
  FOrdinal := Value;
end;

{ TVectorClass<T>.TVectorEnumerator<T> }

constructor TVectorClass<T>.TVectorEnumerator.Create(AVector: TVectorClass<T>);
begin
  inherited Create;
  FItems := AVector.FItems;
  FCount := AVector.Count;
end;

function TVectorClass<T>.TVectorEnumerator.DoMoveNext: Boolean;
begin
  result := FCurrentIndex < FCount;
  if result then
    inc(FCurrentIndex);
end;

function TVectorClass<T>.TVectorEnumerator.DoGetCurrent: T;
begin
  result := FITems[FCurrentIndex-1];
end;

{ TVectorClass<T> }

constructor TVectorClass<T>.Create(AComparer: IComparer<T>);
begin
  inherited Create;
  FComparer := AComparer;
end;

constructor TVectorClass<T>.Create(ACapacity: integer; AComparer: IComparer<T> = nil);
begin
  Create(AComparer);
  Capacity := ACapacity;
end;

constructor TVectorClass<T>.Create(AItems: TArray<T>; AComparer: IComparer<T> = nil);
begin
  Create(AComparer);
  Add(AItems);
end;

constructor TVectorClass<T>.Create(AItems: TEnumerable<T>; AComparer: IComparer<T> = nil);
begin
  Create(AComparer);
  Add(AItems);
end;

constructor TVectorClass<T>.Create(AComparer: TComparison<T>);
begin
  Create(TDelegatedComparer<T>.Create(AComparer));
end;

procedure TVectorClass<T>.Add(const Value: TArray<T>);
begin
  Add(Value, 0, System.Length(Value));
end;

procedure TVectorClass<T>.Add(const Value: TArray<T>; AStartIndex,ACount: integer);
var
  I: Integer;
begin
  I := Count + ACount;
  if I > Capacity then
    Capacity := I;
  for I := AStartIndex to AStartIndex+ACount-1 do
    Add(Value[I]);
end;

procedure TVectorClass<T>.Add(const Value: TEnumerable<T>);
var
  V: T;
begin
  for V in Value do
    Add(V);
end;

function TVectorClass<T>.Add: integer;
begin
  if Count>=Capacity then
    Grow;
  result := FCount;
  inc(FCount);
end;

function TVectorClass<T>.Add(const Value: T): integer;
begin
  result := Add;
  FItems[result] := Value;
end;

procedure TVectorClass<T>.Grow;
begin
  if Capacity < 4 then
    Capacity := Capacity+1
  else
  if Capacity < 64 then
    Capacity := 64
  else
    Capacity := Capacity * 2;
end;

function TVectorClass<T>.BinarySearch(const Item: T; out FoundIndex: Integer; AComparer: IComparer<T> = nil): Boolean;
begin
  result := BinarySearch(Item, FoundIndex, 0, Count, AComparer);
end;

function TVectorClass<T>.BinarySearch(const Item: T; out FoundIndex: Integer; AStartIndex,ACount: Integer; AComparer: IComparer<T> = nil): Boolean;
begin
  FindComparer(AComparer);
  result := TArray.BinarySearch<T>(FItems, Item, FoundIndex, AComparer, AStartIndex, ACount);
end;

procedure TVectorClass<T>.Clear;
var
  I: Integer;
begin
  if FOwnsValues then
    for I := FCount-1 downto 0 do
      PObject(@FItems[I])^.DisposeOf;
  SetLength(FItems, 0);
  FCount := 0;
end;

function TVectorClass<T>.Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer;
begin
  if Count = System.Length(B) then
    result := Compare(B,0,0,Count, AComparer)
  else
    if Count < System.Length(B)
      then result := -1
      else result := 1;
end;

function TVectorClass<T>.Compare(const B: TArray<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T>): integer;
var
  I: Integer;
begin
  FindComparer(AComparer);
  Assert((ACount=0) or (ACount>0) and (AStartIndex>=0) and (AStartIndex+ACount-1<Count));
  Assert((ACount=0) or (ACount>0) and (BStartIndex>=0) and (BStartIndex+ACount-1<System.Length(B)));
  if ACount <= 0 then
    result := 0
  else
    for I := 0 to ACount-1 do
    begin
      result := AComparer.Compare(FItems[I+AStartIndex], B[I+BStartIndex]);
      if result <> 0 then
        Break;
    end;
end;

function TVectorClass<T>.Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer;
var
  Value: T;
  BItemsCount: integer;
begin
  BItemsCount := 0;
  for Value in B do
    inc(BItemsCount);
  if Count = BItemsCount then
    result := Compare(B,0,0,Count, AComparer)
  else
    if Count < BItemsCount
      then result := -1
      else result := 1;
end;

function TVectorClass<T>.Compare(B: TEnumerable<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T>): integer;
var
  Value: T;
  BItemsCount: integer;
begin
  FindComparer(AComparer);
  BItemsCount := 0;
  for Value in B do
    inc(BItemsCount);
  Assert((ACount=0) or (ACount>0) and (AStartIndex>=0) and (AStartIndex+ACount-1<Count));
  Assert((ACount=0) or (ACount>0) and (BStartIndex>=0) and (BStartIndex+ACount-1<BItemsCount));
  result := 0;
  for Value in B do
    if BStartIndex > 0 then
      dec(BStartIndex)
    else
    begin
      result := AComparer.Compare(FItems[AStartIndex], Value);
      inc(AStartIndex);
      dec(ACount);
      if (result <> 0) or (ACount <= 0) then
        break;
    end;
end;

function TVectorClass<T>.ContainsAll(V: TArray<T>): boolean;
var
  C: IComparer<T>;
  B: TArray<boolean>;
  I,J,N: Integer;
begin
  if (Length(V) = 0) or (Count = 0) then
    Exit(False);

  { We can't use TSet, because it needs IEqualityComparer.GetHash,
    IComparer can not be tranformed into IEqualityComparer hasher }
  if Comparer=nil
    then C := TComparerUtils.DefaultComparer<T>
    else C := Comparer;
  TArray.Sort<T>(V, C);

  { remove duplicates }
  J := 0;
  for I := 1 to High(V) do
    if C.Compare(V[I], V[J])<>0 then
    begin
      inc(J);
      FItems[J] := FItems[I];
    end;
  SetLength(V, J+1);

  { find items in sorted version of Values }
  SetLength(B, Length(V));
  N := Length(V);
  for I := 0 to FCount-1 do
    if TArray.BinarySearch<T>(V, FItems[I], J, C) and not B[J] then
    begin
      B[J] := True;
      dec(N);
      if N <= 0 then
        Exit(True);
    end;
  result := False;
end;

function TVectorClass<T>.Contains(const Values: TArray<T>): boolean;
begin
  result := ContainsAll(TArrayUtils.Copy<T>(Values));
end;

function TVectorClass<T>.Contains(const Values: TEnumerable<T>): boolean;
begin
  result := ContainsAll(Values.ToArray);
end;

function TVectorClass<T>.Contains(const Value: T): boolean;
begin
  result := IndexOf(Value)>=0;
end;

function TVectorClass<T>.Copy: TVectorClass<T>;
begin
  result := TVectorClass<T>.Create;
  result.FItems := TArrayUtils.Copy<T>(FItems, 0, Count);
  result.FCount := Count;
end;

procedure TVectorClass<T>.Delete(AStartIndex, ACount: integer);
var
  I,C: Integer;
begin
  Assert((AStartIndex>=0) and (AStartIndex<Count) and (ACount>=0) and (AStartIndex+ACount-1<Count));
  if FOwnsValues then
    for I := AStartIndex to AStartIndex+ACount-1 do
      PObject(@FItems[I])^.DisposeOf;
  C := Count-ACount; { new Count }
  for I := AStartIndex to C-1 do
    FItems[I] := FItems[I+ACount];
  for I := C to Count-1 do
    FItems[I] := Default(T);
  FCount := C;
end;

procedure TVectorClass<T>.Delete(ItemIndex: integer);
var
  I: Integer;
begin
  Assert((ItemIndex>=0) and (ItemIndex<FCount));
  if FOwnsValues then
    PObject(@FItems[ItemIndex])^.DisposeOf;
  for I := ItemIndex to Count-2 do
    FItems[I] := FItems[I+1];
  Dec(FCount);
  FItems[FCount] := Default(T);
end;

procedure TVectorClass<T>.Delete(const AIndices: TArray<integer>);
var
  S,D,I,V: Integer;
begin
  if System.Length(AIndices) = 0 then
    Exit;

  { if AIndices is not sorted or has equal indices, then we transform AIndices to set and use another method }
  for I := 0 to High(AIndices)-1 do
    if AIndices[I] >= AIndices[I+1] then
    begin
      Delete(TSet<integer>.Create(AIndices));
      Exit;
    end;

  Assert((AIndices[0] >= 0) and (AIndices[High(AIndices)] < Count));
  S := 0;
  V := AIndices[0];
  D := V;
  for I := AIndices[0] to Count-1 do
    if I = V then
    begin
      if FOwnsValues then
        PObject(@FItems[I])^.DisposeOf;
      if S >= High(AIndices) then
        V := -1
      else
      begin
        inc(S);
        V := AIndices[S];
      end
    end
    else
    begin
      FItems[D] := FItems[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    FItems[I] := Default(T);
  FCount := D;
end;

procedure TVectorClass<T>.Delete(AIndices: TSet<integer>);
var
  I,S,D: Integer;
begin
  S := 0;
  D := 0;
  for I := 0 to Count-1 do
    if I in AIndices then
    begin
      if FOwnsValues then
        PObject(@FItems[I])^.DisposeOf;
      inc(S);
      dec(FCount);
    end
    else
    begin
      FItems[D] := FItems[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    FItems[I] := Default(T);
  FCount := D;
end;

procedure TVectorClass<T>.DeleteLast;
begin
  Assert(FCount>0);
  Dec(FCount);
  if FOwnsValues then
    PObject(@FItems[FCount])^.DisposeOf;
  FItems[FCount] := Default(T);
end;

destructor TVectorClass<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TVectorClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := TVEctorEnumerator.Create(Self);
end;

function TVectorClass<T>.Equal(const B: TArray<T>): boolean;
begin
  result := Compare(B) = 0;
end;

function TVectorClass<T>.Equal(B: TEnumerable<T>): boolean;
begin
  result := Compare(B) = 0;
end;

procedure TVectorClass<T>.Exchange(Index1, Index2: integer);
var Value: T;
begin
  Assert((Index1>=0) and (Index1<Count) and (Index2>=0) and (Index2<Count));
  Value := FItems[Index1];
  FItems[Index1] := FItems[Index2];
  FItems[Index2] := Value;
end;

function TVectorClass<T>.Extract(ItemIndex: integer): T;
begin
  Assert((ItemIndex>=0) and (ItemIndex<Count));
  result := FItems[ItemIndex];
  FItems[ItemIndex] := Default(T);
  Delete(ItemIndex);
end;

function TVectorClass<T>.ExtractAll: TArray<T>;
begin
  TrimExcess;
  result := FItems;
  SetLength(FItems, 0);
  FCount := 0;
end;

function TVectorClass<T>.ExtractLast: T;
begin
  Assert(FCount>=0);
  Dec(FCount);
  result := FItems[FCount];
  FItems[FCount] := Default(T);
end;

function TVectorClass<T>.FindFirst(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, AComparer);
end;

function TVectorClass<T>.FindFirst(const Value: T; var Index: integer): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, FComparer);
end;

function TVectorClass<T>.FindNext(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean;
var
  I: Integer;
begin
  FindComparer(AComparer);
  for I := Index+1 to Count-1 do
    if AComparer.Compare(FItems[I], Value)=0 then
    begin
      Index := I;
      Exit(True);
    end;
  result := False;
end;

function TVectorClass<T>.FindNext(const Value: T; var Index: integer): boolean;
begin
  result := FindNext(Value, Index, FComparer);
end;

procedure TVectorClass<T>.FirstPermutation;
begin
  Sort;
end;

function TVectorClass<T>.NextPermutation: boolean;
var
  i,x,n: integer;
  C: IComparer<T>;
begin
  C := Comparer;
  FindComparer(C);

  { find max N where A[N] < A[N+1] }
  n := -1;
  for i := Count-2 downto 0 do
    if C.Compare(FItems[i], FItems[i+1]) < 0 then
    begin
      n := i;
      break;
    end;

  { if A[N] > A[N+1] for any N then there is no more permutations }
  result := n<>-1;
  if not result then
    exit;

  { let's order range [N+1; FCount-1]
    now it has reverse order so just call .reverse }
  Reverse(n+1,FCount-n-1);

  { find value next to A[N] in range [N+1; Count-1]
    such value exists because at least original A[N+1] > A[N] }
  x := -1;
  for i := N+1 to Count-1 do
    if C.Compare(FItems[i], FItems[N]) > 0 then
    begin
      x := i;
      break;
    end;

  { swap A[N] and A[X] }
  Exchange(n, x);

  { change position of A[X] to make range [N+1; FCoun-1] ordered again }
  i := x;
  while (i > n+1) and (C.Compare(FItems[i-1], FItems[x]) > 0) do
    dec(i);
  while (i < Count-1) and (C.Compare(FItems[x], FItems[i+1]) > 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

function TVectorClass<T>.PrevPermutation: boolean;
var
  i,x,n: integer;
  C: IComparer<T>;
begin
  C := Comparer;
  FindComparer(C);

  { find max N where A[N] > A[N+1] }
  n := -1;
  for i := FCount-2 downto 0 do
    if C.Compare(FItems[i], FItems[i+1]) > 0 then
    begin
      n := i;
      break;
    end;

  { if A[N] > A[N+1] for any N then there is no more permutations }
  result := n<>-1;
  if not result then
    exit;

  { let's order range [N+1; FCoun-1]
    now it has reverse order so just call .reverse }
  reverse(n+1,FCount-n-1);

  { find value previous to A[N] in range [N+1; FCount-1]
    such value exists because at least original A[N+1] < A[N] }
  x := -1;
  for i := N+1 to FCount-1 do
    if C.Compare(FItems[i], FItems[N]) < 0 then
    begin
      x := i;
      break;
    end;

  { swap A[N] and A[X] }
  Exchange(n,x);

  { change position of A[X] to make range [N+1; FCoun-1] back ordered again }
  i := x;
  while (i > n+1) and (C.Compare(FItems[i-1], FItems[x]) < 0) do
    dec(i);
  while (i < FCount-1) and (C.Compare(FItems[x], FItems[i+1]) < 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

function TVectorClass<T>.GetItemsArray: TArray<T>;
begin
  TrimExcess;
  result := FItems;
end;

function TVectorClass<T>.GetCapacity: integer;
begin
  result := System.Length(FItems);
end;

function TVectorClass<T>.GetEmpty: boolean;
begin
  Result := FCount <= 0;
end;

function TVectorClass<T>.GetFirst: T;
begin
  Assert(FCount>0);
  Result := FItems[0];
end;

function TVectorClass<T>.GetItem(ItemIndex: integer): T;
begin
  Assert((ItemIndex >= 0) and (ItemIndex < Count));
  result := FItems[ItemIndex];
end;

function TVectorClass<T>.GetLast: T;
begin
  Assert(FCount>0);
  Result := FItems[Count-1];
end;

function TVectorClass<T>.GetTotalSizeBytes: int64;
begin
  result := (High(FItems)-Low(FItems)+1)*SizeOf(T);
end;

function TVectorClass<T>.IndexOf(const Value: T; AComparer: IComparer<T>): integer;
begin
  if not FindFirst(Value, Result, AComparer) then
    result := -1;
end;

function TVectorClass<T>.IndexOf(const Value: T): integer;
begin
  if not FindFirst(Value, Result) then
    result := -1;
end;

function TVectorClass<T>.Insert(Index: integer; const Value: T): integer;
begin
  Assert((Index>=0) and (Index<=Count));
  for result := Add downto Index+1 do
    FItems[result] := FItems[result-1];
  result := Index;
  FItems[result] := Value;
end;

procedure TVectorClass<T>.Move(SrcIndex, DstIndex: integer);
var
  I: integer;
  Value: T;
begin
  Assert((SrcIndex>=0) and (SrcIndex<Count) and (DstIndex>=0) and (DstIndex<Count));
  if SrcIndex < DstIndex then
  begin
    {      src   dst
      1 2 [3] 4 [5] 6 7 }
    Value := FItems[SrcIndex];
    for I := SrcIndex to DstIndex-1 do
      FItems[I] := FItems[I+1];
    FItems[DstIndex] := Value;
  end
  else
  if SrcIndex > DstIndex then
  begin
    {      dst   src
      1 2 [3] 4 [5] 6 7 }
    Value := FItems[SrcIndex];
    for I := SrcIndex downto DstIndex+1 do
      FItems[I] := FItems[I-1];
    FItems[DstIndex] := Value;
  end;
end;

procedure TVectorClass<T>.FindComparer(var AComparer: IComparer<T>);
begin
  if AComparer = nil then
    if FComparer <> nil
      then AComparer := FComparer
      else AComparer := TComparerUtils.DefaultComparer<T>;
end;

procedure TVectorClass<T>.Remove(const V: T; AComparer: IComparer<T> = nil);
var
  I,D: Integer;
begin
  FindComparer(AComparer);
  D := 0;
  for I := 0 to FCount-1 do
    if AComparer.Compare(FItems[I], V) = 0 then
    begin
      if FOwnsValues then
        PObject(@FItems[I])^.DisposeOf;
    end
    else
    begin
      FItems[D] := FItems[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    FItems[I] := Default(T);
  FCount := D;
end;

procedure TVectorClass<T>.Remove(AFilter: TRemoveFilter);
var
  I,D: Integer;
begin
  D := 0;
  for I := 0 to FCount-1 do
    if AFilter(FItems[I], I) then
    begin
      if FOwnsValues then
        PObject(@FItems[I])^.DisposeOf;
    end
    else
    begin
      FItems[D] := FItems[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    FItems[I] := Default(T);
  FCount := D;
end;

procedure TVectorClass<T>.Remove(const V: TArray<T>; AComparer: IComparer<T> = nil);
var
  S: TBinarySearchIndexClass<T>;
  I,D: integer;
begin
  if (Length(V)=0) or (Count=0) then
    Exit;
  FindComparer(AComparer);
  S := TBinarySearchIndexClass<T>.Create(V, AComparer);
  try
    D := 0;
    for I := 0 to FCount-1 do
      if S.Find(FItems[I]) >= 0 then
      begin
        if FOwnsValues then
          PObject(@FItems[I])^.DisposeOf;
      end
      else
      begin
        FItems[D] := FItems[I];
        inc(D);
      end;
    for I := D to FCount-1 do
      FItems[I] := Default(T);
    FCount := D;
  finally
    Sys.FreeAndNil(S);
  end;
end;

procedure TVectorClass<T>.Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil);
begin
  Remove(V.ToArray, AComparer);
end;

procedure TVectorClass<T>.Reverse;
begin
  Reverse(0, Count);
end;

procedure TVectorClass<T>.Reverse(AStartIndex,ACount: integer);
var
  I: Integer;
  Value: T;
begin
  if ACount <= 0 then
    Exit;
  Assert((AStartIndex >= 0) and (AStartIndex + ACount <= Count));
  for I := 0 to (ACount shr 1) - 1 do
  begin
    Value := FItems[AStartIndex+I];
    FItems[AStartIndex+I] := FItems[AStartIndex+ACount-1-I];
    FItems[AStartIndex+ACount-1-I] := Value;
  end;
end;

procedure TVectorClass<T>.RotateLeft(Index1, Index2, Shift: integer);
var
  I: integer;
begin
  Assert((Index1>=0) and (Index1<Count) and (Index2>=0) and (Index2<Count));
  if Index2 < Index1 then
  begin
    I := Index1;
    Index1 := Index2;
    Index2 := I;
  end;
  I := Index2-Index1+1;
  Shift := (I - (Shift mod I)) mod I;
  if Shift <= 0 then
    if Shift < 0 then
      Inc(Shift, I)
    else
      Exit;
  Reverse(Index1, Index2-Index1+1);
  Reverse(Index1, Shift);
  Reverse(Index1+Shift, Index2-Index1+1-Shift);
end;

procedure TVectorClass<T>.RotateRight(Index1, Index2, Shift: integer);
var
  I: integer;
begin
  Assert((Index1>=0) and (Index1<Count) and (Index2>=0) and (Index2<Count));
  if Index2 < Index1 then
  begin
    I := Index1;
    Index1 := Index2;
    Index2 := I;
  end;
  I := Index2-Index1+1;
  Shift := Shift mod I;
  if Shift <= 0 then
    if Shift < 0 then
      Inc(Shift, I)
    else
      Exit;
  Reverse(Index1, Index2-Index1+1);
  Reverse(Index1, Shift);
  Reverse(Index1+Shift, Index2-Index1+1-Shift);
end;

procedure TVectorClass<T>.SetItemsArray(const Value: TArray<T>);
begin
  Clear; { to properly destroy items }
  FItems := Value;
  FCount := System.Length(FItems);
end;

procedure TVectorClass<T>.SetCapacity(const Value: integer);
begin
  Assert(Value>=Count);
  SetLength(FItems, Value);
end;

procedure TVectorClass<T>.SetCount(const Value: integer);
var
  I: Integer;
begin
  Assert(Value>=0);
  if not FOwnsValues then
    for I := Value to Count-1 do
      FItems[I] := Default(T)
  else
    for I := Value to Count-1 do
    begin
      PObject(@FItems[I])^.DisposeOf;
      FItems[I] := Default(T)
    end;
  FCount := Value;
  if Value > Capacity then
    Capacity := Value;
end;

procedure TVectorClass<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  Assert((ItemIndex >= 0) and (ItemIndex < Count));
  if FOwnsValues then
    PObject(@FItems[ItemIndex])^.DisposeOf;
  FItems[ItemIndex] := Value;
end;

procedure TVectorClass<T>.SetFirst(const Value: T);
begin
  Assert(Count>0);
  if FOwnsValues then
    PObject(@FItems[0])^.DisposeOf;
  FItems[0] := Value;
end;

procedure TVectorClass<T>.SetLast(const Value: T);
begin
  Assert(Count>0);
  if FOwnsValues then
    PObject(@FItems[Count-1])^.DisposeOf;
  FItems[Count-1] := Value;
end;

procedure TVectorClass<T>.SetOwnsValues(const Value: boolean);
begin
  if Value and not TRttiUtils.IsInstance<T> then
    raise Exception.Create('Generic type is not a class.');
  FOwnsValues := Value;
end;

procedure TVectorClass<T>.Shuffle;
begin
  Shuffle(0, Count);
end;

procedure TVectorClass<T>.Shuffle(AStartIndex,ACount: integer);
var
  I: Integer;
begin
  if ACount <= 1 then
    Exit;
  Assert((AStartIndex >= 0) and (AStartIndex + ACount <= Count));
  for I := ACount-1 downto 1 do
    Exchange(I+AStartIndex, Random(I+1)+AStartIndex);
end;

procedure TVectorClass<T>.Sort(AComparer: IComparer<T> = nil);
begin
  Sort(0, Count, AComparer);
end;

procedure TVectorClass<T>.Sort(AIndex, ACount: Integer; AComparer: IComparer<T> = nil);
begin
  FindComparer(AComparer);
  TArray.Sort<T>(FItems, AComparer, AIndex, ACount);
end;

procedure TVectorClass<T>.Sort(AComparer: TSortFunc<T>);
begin
  Sort(0, Count, AComparer);
end;

procedure TVectorClass<T>.Sort(AIndex, ACount: Integer; AComparer: TSortFunc<T>);
var
  C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(
    function (const A,B: T): integer
    begin
      result := AComparer(A,B);
    end);
  TArray.Sort<T>(FItems, C, AIndex, ACount);
end;

function TVectorClass<T>.Sorted: boolean;
begin
  result := Sorted(0, Count, FComparer);
end;

function TVectorClass<T>.Sorted(AComparer: IComparer<T>): boolean;
begin
  result := Sorted(0, Count, AComparer);
end;

function TVectorClass<T>.Sorted(AStartIndex, ACount: integer; AComparer: IComparer<T>): boolean;
begin
  FindComparer(AComparer);
  result := TArrayUtils.Sorted<T>(FItems, AStartIndex, ACount, AComparer);
end;

function TVectorClass<T>.ToArray: TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, FCount);
  for I := 0 to FCount-1 do
    Result[I] := FItems[I];
end;

function TVectorClass<T>.ToArray(Readonly: boolean): TArray<T>;
begin
  if not Readonly then
    result := ToArray
  else
  begin
    TrimExcess;
    result := FItems;
  end;
end;

function TVectorClass<T>.ToString: string;
begin
  result := ToString(' ', False);
end;

function TVectorClass<T>.ToString(const ValueSeparator: string; SepAfterLastValue: boolean): string;
var
  Buf: TStringBuffer;
  I: Integer;
begin
  Buf.Clear;
  if Count > 0 then
    Buf.Write(TRttiUtils.ValueAsString<T>(FItems[0]));
  for I := 1 to Count-1 do
    Buf.Write(ValueSeparator + TRttiUtils.ValueAsString<T>(FItems[I]));
  if (Count > 0) and SepAfterLastValue then
    Buf.Write(ValueSeparator);
  Result := Buf.Text;
end;

function TVectorClass<T>.ToText: string;
begin
  result := ToString(#13#10, False);
end;

procedure TVectorClass<T>.TrimExcess;
begin
  if Capacity>Count then
    Capacity := Count;
end;

{ TVector<T> }

class operator TVector<T>.Add(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a: TVector<T>; const b: TArray<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a, b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Add(B);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; AComparer: IComparer<T>): Boolean;
begin
  result := RO.BinarySearch(Item, FoundIndex, AComparer);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; AStartIndex, ACount: Integer;
  AComparer: IComparer<T>): Boolean;
begin
  result := RO.BinarySearch(Item, FoundIndex, AStartIndex, ACount, AComparer);
end;

class operator TVector<T>.Add(const a: TArray<T>; b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: T; b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Add(B);
end;

procedure TVector<T>.Add(const Values: TArray<T>);
begin
  RW.Add(Values);
end;

procedure TVector<T>.Add(const Values: TArray<T>; AStartIndex,ACount: integer);
begin
  RW.Add(Values,AStartIndex,ACount);
end;

function TVector<T>.Add(const Value: T): integer;
begin
  result := RW.Add(Value);
end;

function TVector<T>.Add: integer;
begin
  result := RW.Add;
end;

class operator TVector<T>.Add(a: TVector<T>; const b: T): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Add(B);
end;

procedure TVector<T>.Add(Values: TVector<T>);
var
  Src: TVectorClass<T>;
begin
  if Values.FVectorInt = nil then
    Exit;
  Src := Values.RO;
  RW.Add(Src.FItems, 0, Src.Count);
end;

procedure TVector<T>.Add(const Values: TEnumerable<T>);
begin
  RW.Add(Values);
end;

procedure TVector<T>.Clear;
begin
  RW.Clear;
end;

function TVector<T>.Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer;
begin
  result := RO.Compare(B, AComparer);
end;

function TVector<T>.Compare(const B: TArray<T>; AStartIndex, BStartIndex, ACount: integer;
  AComparer: IComparer<T>): integer;
begin
  result := RO.Compare(B, AStartIndex, BStartIndex, ACount, AComparer);
end;

function TVector<T>.Compare(B: TEnumerable<T>; AStartIndex, BStartIndex, ACount: integer;
  AComparer: IComparer<T>): integer;
begin
  result := RO.Compare(B, AStartIndex, BStartIndex, ACount, AComparer);
end;

function TVector<T>.Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer;
begin
  result := RO.Compare(B, AComparer);
end;

function TVector<T>.Contains(const Values: TArray<T>): boolean;
begin
  result := RO.Contains(Values);
end;

function TVector<T>.Contains(const Values: TEnumerable<T>): boolean;
begin
  result := RO.Contains(Values);
end;

function TVector<T>.Contains(const Value: T): boolean;
begin
  result := RO.Contains(Value);
end;

function TVector<T>.Copy: TVector<T>;
begin
  result := TVector<T>.Create(Comparer);
  result.Add(Self);
end;

class function TVector<T>.Create(AComparer: IComparer<T>): TVector<T>;
begin
  result.Clear;
  result.CreateVector(0, AComparer);
end;

constructor TVector<T>.Create(ACapacity: integer; AComparer: IComparer<T>);
begin
  CreateVector(ACapacity, AComparer);
end;

constructor TVector<T>.Create(AComparer: TComparison<T>);
begin
  CreateVector(0, TDelegatedComparer<T>.Create(AComparer));
end;

constructor TVector<T>.Create(const Values: TEnumerable<T>; ACapacity: integer; AComparer: IComparer<T>);
begin
  CreateVector(ACapacity, AComparer);
  Add(Values);
end;

constructor TVector<T>.Create(const Values: TArray<T>; AComparer: IComparer<T>);
begin
  CreateVector(System.Length(Values), AComparer);
  Add(Values);
end;

procedure TVector<T>.CreateVector(ACapacity: integer; AComparer: IComparer<T>);
begin
  FVectorInt := TInterfacedObject<TVectorClass<T>>.Create(
    TVectorClass<T>.Create(ACapacity, AComparer)
  );
end;

procedure TVector<T>.Delete(const AIndices: TArray<integer>);
begin
  RW.Delete(AIndices);
end;

procedure TVector<T>.Delete(AIndices: TSet<integer>);
begin
  RW.Delete(AIndices);
end;

procedure TVector<T>.Delete(ItemIndex: integer);
begin
  RW.Delete(ItemIndex);
end;

procedure TVector<T>.Delete(AStartIndex, ACount: integer);
begin
  RW.Delete(AStartIndex, ACount);
end;

procedure TVector<T>.DeleteLast;
begin
  RW.DeleteLast;
end;

class operator TVector<T>.Equal(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Equal(B);
end;

class operator TVector<T>.Equal(a, b: TVector<T>): Boolean;
begin
  result := A.Equal(B.Collection);
end;

function TVector<T>.Equal(B: TEnumerable<T>): boolean;
begin
  result := RO.Equal(B);
end;

class operator TVector<T>.Equal(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Equal(B);
end;

function TVector<T>.Equal(const B: TArray<T>): boolean;
begin
  result := RO.Equal(B);
end;

procedure TVector<T>.Exchange(Index1, Index2: integer);
begin
  RW.Exchange(Index1, Index2);
end;

class operator TVector<T>.Explicit(const a: TArray<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
end;

class operator TVector<T>.Explicit(const a: T): TVector<T>;
begin
  result.Clear;
  result.Add(A);
end;

class operator TVector<T>.Explicit(const a: TEnumerable<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
end;

function TVector<T>.Extract(ItemIndex: integer): T;
begin
  result := RW.Extract(ItemIndex);
end;

function TVector<T>.ExtractAll: TArray<T>;
begin
  result := RW.ExtractAll;
end;

function TVector<T>.ExtractLast: T;
begin
  result := RW.ExtractLast;
end;

function TVector<T>.FindFirst(const Value: T; var Index: integer): boolean;
begin
  result := RO.FindFirst(Value, Index);
end;

function TVector<T>.FindFirst(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean;
begin
  result := RO.FindFirst(Value, Index, AComparer);
end;

function TVector<T>.FindNext(const Value: T; var Index: integer): boolean;
begin
  result := RO.FindNext(Value, Index);
end;

function TVector<T>.FindNext(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean;
begin
  result := RO.FindNext(Value, Index, AComparer);
end;

procedure TVector<T>.FirstPermutation;
begin
  RW.FirstPermutation;
end;

function TVector<T>.GetItemsArray: TArray<T>;
begin
  result := RW.ItemsArray;
end;

function TVector<T>.GetCapacity: integer;
begin
  result := RO.Capacity;
end;

function TVector<T>.GetCollection: TEnumerable<T>;
begin
  result := RO;
end;

function TVector<T>.GetComparer: IComparer<T>;
begin
  result := RO.Comparer;
end;

function TVector<T>.GetCount: integer;
begin
  result := RO.Count;
end;

function TVector<T>.GetEmpty: Boolean;
begin
  result := RO.Empty;
end;

function TVector<T>.GetEnumerator: TEnumerator<T>;
begin
  result := RO.GetEnumerator;
end;

function TVector<T>.GetFirst: T;
begin
  result := RO.First;
end;

function TVector<T>.GetItem(ItemIndex: integer): T;
begin
  result := RO[ItemIndex];
end;

function TVector<T>.GetLast: T;
begin
  result := RO.Last;
end;

function TVector<T>.GetOwnsValues: boolean;
begin
  result := RO.OwnsValues;
end;

function TVector<T>.GetRO: TVectorClass<T>;
begin
  if FVectorInt=nil then
    CreateVector;
  result := FVectorInt.Data;
end;

function TVector<T>.GetRW: TVectorClass<T>;
var
  SrcVectorInt: IInterfacedObject<TVectorClass<T>>;
begin
  if FVectorInt=nil then
    CreateVector
  else
    if FVectorInt.GetRefCount<>1 then
    begin
      { Copy on write }
      SrcVectorInt := FVectorInt;
      CreateVector(SrcVectorInt.Data.Count, SrcVectorInt.Data.Comparer);
      FVectorInt.Data.Add(SrcVectorInt.Data);
      FVectorInt.Data.OwnsValues := SrcVectorInt.Data.OwnsValues;
    end;
  result := FVectorInt.Data;
end;

function TVector<T>.GetTotalSizeBytes: int64;
begin
  result := RO.TotalSizeBytes;
end;

class operator TVector<T>.GreaterThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThan(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) > 0;
end;

class operator TVector<T>.GreaterThan(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) >= 0;
end;

class operator TVector<T>.Implicit(const a: TEnumerable<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
end;

class operator TVector<T>.Implicit(const a: T): TVector<T>;
begin
  result.Clear;
  result.Add(A);
end;

class operator TVector<T>.Implicit(const a: TArray<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
end;

class operator TVector<T>.In(const a: TArray<T>; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.In(const a: TEnumerable<T>; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.In(const a: T; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.In(a, b: TVector<T>): Boolean;
begin
  result := B.Contains(A.Collection);
end;

function TVector<T>.IndexOf(const Value: T; AComparer: IComparer<T>): integer;
begin
  result := RO.IndexOf(Value, AComparer);
end;

function TVector<T>.IndexOf(const Value: T): integer;
begin
  result := RO.IndexOf(Value);
end;

function TVector<T>.Insert(Index: integer; const Value: T): integer;
begin
  result := RW.Insert(Index, Value);
end;

class operator TVector<T>.LessThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) < 0;
end;

class operator TVector<T>.LessThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThan(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.LessThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.LessThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.LessThanOrEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

procedure TVector<T>.Move(SrcIndex, DstIndex: integer);
begin
  RW.Move(SrcIndex, DstIndex);
end;

function TVector<T>.NextPermutation: boolean;
begin
  result := RW.NextPermutation;
end;

class operator TVector<T>.NotEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := not A.Equal(B);
end;

class operator TVector<T>.NotEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := not A.Equal(B);
end;

class operator TVector<T>.NotEqual(a, b: TVector<T>): Boolean;
begin
  result := not A.Equal(B.Collection);
end;

class operator TVector<T>.NotEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := not A.Equal(B);
end;

class operator TVector<T>.NotEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := not A.Equal(B);
end;

function TVector<T>.PrevPermutation: boolean;
begin
  result := RW.PrevPermutation;
end;

procedure TVector<T>.Remove(const V: T; AComparer: IComparer<T> = nil);
begin
  RW.Remove(V, AComparer);
end;

procedure TVector<T>.Remove(const V: TArray<T>; AComparer: IComparer<T> = nil);
begin
  RW.Remove(V, AComparer);
end;

procedure TVector<T>.Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil);
begin
  RW.Remove(V, AComparer);
end;

procedure TVector<T>.Reverse;
begin
  RW.Reverse;
end;

procedure TVector<T>.Reverse(AStartIndex,ACount: integer);
begin
  RW.Reverse(AStartIndex,ACount);
end;

procedure TVector<T>.RotateLeft(Index1, Index2, Shift: integer);
begin
  RW.RotateLeft(Index1, Index2, Shift);
end;

procedure TVector<T>.RotateRight(Index1, Index2, Shift: integer);
begin
  RW.RotateRight(Index1, Index2, Shift);
end;

procedure TVector<T>.SaveToFile(const FileName: string; Encoding: TEncoding; MemStream: boolean);
begin
  RO.SaveToFile(FileName, Encoding, MemStream);
end;

procedure TVector<T>.SaveToStream(Dst: TStream; Encoding: TEncoding);
begin
  RO.SaveToStream(Dst, Encoding);
end;

procedure TVector<T>.SetCapacity(const Value: integer);
begin
  RW.Capacity := Value;
end;

procedure TVector<T>.SetComparer(Value: IComparer<T>);
begin
  RW.Comparer := Value;
end;

procedure TVector<T>.SetCount(const Value: integer);
begin
  RW.Count := Value;
end;

procedure TVector<T>.SetFirst(const Value: T);
begin
  RW.First := Value;
end;

procedure TVector<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  RW[ItemIndex] := Value;
end;

procedure TVector<T>.SetItemsArray(const Value: TArray<T>);
begin
  RW.ItemsArray := Value;
end;

procedure TVector<T>.SetLast(const Value: T);
begin
  RW.Last := Value;
end;

procedure TVector<T>.SetOwnsValues(AOwnsValues: boolean);
begin
  RW.OwnsValues := AOwnsValues;
end;

procedure TVector<T>.Shuffle;
begin
  RW.Shuffle;
end;

procedure TVector<T>.Shuffle(AStartIndex,ACount: integer);
begin
  RW.Shuffle(AStartIndex,ACount);
end;

procedure TVector<T>.Sort(AIndex, ACount: Integer; AComparer: IComparer<T>);
begin
  RW.Sort(AIndex, ACount, AComparer);
end;

procedure TVector<T>.Sort(AComparer: IComparer<T>);
begin
  RW.Sort(AComparer);
end;

procedure TVector<T>.Sort(AIndex, ACount: Integer; AComparer: TSortFunc<T>);
begin
  RW.Sort(AIndex, ACount, AComparer);
end;

procedure TVector<T>.Sort(AComparer: TSortFunc<T>);
begin
  RW.Sort(AComparer);
end;

class operator TVector<T>.Subtract(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Remove(B.Collection);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: TArray<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(a, b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Remove(B.Collection);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: T): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(const a: TArray<T>; b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Remove(B.Collection);
end;

class operator TVector<T>.Subtract(const a: T; b: TVector<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Remove(B.Collection);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
begin
  result.Clear;
  result.Add(A);
  result.Remove(B);
end;

function TVector<T>.ToArray(ReadOnly: boolean): TArray<T>;
begin
  result := RO.ToArray(ReadOnly);
end;

function TVector<T>.ToString: string;
begin
  result := RO.ToString;
end;

function TVector<T>.ToString(const ValueSeparator: string; SepAfterLastValue: boolean): string;
begin
  result := RO.ToString(ValueSeparator, SepAfterLastValue);
end;

function TVector<T>.ToText: string;
begin
  result := RO.ToText;
end;

procedure TVector<T>.TrimExcess;
begin
  RW.TrimExcess;
end;

class operator TVector<T>.Implicit(a: TVector<T>): TEnumerable<T>;
begin
  result := a.Collection;
end;

class operator TVector<T>.Explicit(a: TVector<T>): TEnumerable<T>;
begin
  result := a.Collection;
end;

class operator TVector<T>.Equal(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Equal(B);
end;

class operator TVector<T>.Equal(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Equal(B);
end;

class operator TVector<T>.GreaterThanOrEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.GreaterThan(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.GreaterThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

procedure TVector<T>.Remove(AFilter: TRemoveFilter);
begin
  RW.Remove(AFilter);
end;

{ TComparerUtils.TEqualityByComparer<T> }

constructor TComparerUtils.TEqualityByComparer<T>.Create(AComparer: IComparer<T>);
begin
  inherited Create;
  Assert(AComparer<>nil);
  FComparer := AComparer;
end;

function TComparerUtils.TEqualityByComparer<T>.Equals(const Left, Right: T): Boolean;
begin
  result := FComparer.Compare(Left, Right) = 0;
end;

function TComparerUtils.TEqualityByComparer<T>.GetHashCode(const Value: T): Integer;
var H: THashBobJenkins;
begin
  H.Reset(0);
  H.Update(Value, SizeOf(Value));
  result := H.HashAsInteger;
end;

{ TBinarySearchIndexClass<T> }

procedure TBinarySearchIndexClass<T>.Clear;
begin
  SetLength(FValues, 0);
  SetLength(FIndex, 0);
  FComparer := nil;
end;

constructor TBinarySearchIndexClass<T>.Create(const AValues: TArray<T>; AComparer: IComparer<T>);
var
  I: Integer;
  C: IComparer<integer>;
begin
  Clear;
  FValues := AValues;
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  FComparer := AComparer;

  { build sorted index}
  SetLength(FIndex, Length(FValues));
  for I := 0 to High(FIndex) do
    FIndex[I] := I;
  C := TDelegatedComparer<integer>.Create(
    function(const L,R: integer): integer
    begin
      result := AComparer.Compare(AValues[L], AValues[R]);
    end);
  TArray.Sort<integer>(FIndex, C);

end;

function TBinarySearchIndexClass<T>.Find(const Value: T): integer;
var L,R,C: integer;
begin
  if Length(FValues)=0 then
    Exit(-1);
  L := 0;
  R := High(FIndex);
  while (R-L>1) do
  begin
    Result := (L+R) div 2;
    C := FComparer.Compare(FValues[FIndex[Result]], Value);
    if C < 0 then L := Result else
      if C > 0 then R := Result else
        Exit;
  end;
  if FComparer.Compare(FValues[FIndex[L]], Value) = 0 then Result := L else
    if FComparer.Compare(FValues[FIndex[R]], Value) = 0 then Result := R else
      Result := -1;
end;

function TBinarySearchIndexClass<T>.GetCount: integer;
begin
  result := Length(FIndex);
end;

function TBinarySearchIndexClass<T>.GetIndex(IndexedPos: integer): integer;
begin
  {$IF Defined(Debug)}
    Assert((IndexedPos>=0) and (IndexedPos<=High(FIndex)) and (Length(FIndex)=Length(FValues)));
  {$EndIf}
  result := FIndex[IndexedPos];
end;

function TBinarySearchIndexClass<T>.GetValue(IndexedPos: integer): T;
begin
  {$IF Defined(Debug)}
    Assert((IndexedPos>=0) and (IndexedPos<=High(FIndex)) and (Length(FIndex)=Length(FValues)));
  {$EndIf}
  result := FValues[FIndex[IndexedPos]];
end;

end.
