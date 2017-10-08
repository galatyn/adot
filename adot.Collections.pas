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
  adot.Collections.Types,
  adot.Collections.Vectors,
  adot.Collections.Sets,
  adot.Collections.Maps,
  adot.Collections.Heap,
  adot.Collections.Rings,
  adot.Collections.Lists,
  adot.Collections.Trees,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.TypInfo,
  System.StrUtils,
  System.SysUtils,
  System.Character,
  //System.Contnrs,
  System.Math,
  System.Classes, 
  System.Hash;

type

  { adot.Collections.Maps.pas }
  TMapClass<TKey,TValue>         = class(adot.Collections.Maps.TMapClass<TKey,TValue>);
  TMultimapClass<TKey,TValue>    = class(adot.Collections.Maps.TMultimapClass<TKey,TValue>);
  TBinarySearchTree<TKey,TValue> = class(adot.Collections.Maps.TBinarySearchTree<TKey,TValue>);
  TOrderedMapClass<TKey,TValue>  = class(adot.Collections.Maps.TOrderedMapClass<TKey,TValue>);

  { adot.Collections.Sets.pas }
  TSetClass<T>  = class(adot.Collections.Sets.TSetClass<T>);

  { adot.Collections.Heap }
  TBHeap<TKey, TValue> = class(adot.Collections.Heap.TBHeap<TKey, TValue>);
  TBinaryHeapClass<TKey,TValue> = class(adot.Collections.Heap.TBinaryHeapClass<TKey, TValue>);
  TBinaryHeapClass<TKey> = class(adot.Collections.Heap.TBinaryHeapClass<TKey>);

  { adot.Collections.Rings }
  TRingClass<T> = class(adot.Collections.Rings.TRingClass<T>);

  { adot.Collections.Lists }
  TDoublyLinkedListClass<T> = class(adot.Collections.Lists.TDoublyLinkedListClass<T>);

  { adot.Collections.Trees }
  TTreeArrayClass<T> = class(adot.Collections.Trees.TTreeArrayClass<T>);

  { Delphi has two definitions of TCollectionNotification: System.Classes and System.Generics.Collections }
  TCollectionNotification = System.Generics.Collections.TCollectionNotification;

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
        FList: TObjectList<TObject>;
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

  { Default comparer/equality comparer etc. }
  TComparerUtils = class
  public

    { For string type we use case insensitive comparer by default }
    class function DefaultComparer<T>: IComparer<T>; static;
    class function DefaultEqualityComparer<T>: IEqualityComparer<T>; static;
    class function DefaultArithmetic<T>: IArithmetic<T>; static;

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
  FList := TObjectList<TObject>.Create(True);
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

{ TCacheClass<TKey, TValue> }

constructor TCacheClass<TKey, TValue>.Create;
begin
  Create(DefaultSize);
end;

constructor TCacheClass<TKey, TValue>.Create(AMaxSize: Longint);
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

end.



