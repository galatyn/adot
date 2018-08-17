unit adot.Collections.Vectors;

interface
{
  Most useful classes:
    TVector<T>
      managed, very light and very fast wrapper for TArray<T>
    TCVector<T>
      Managed, supports copy-on-write and has property "OwnsValues",
      but it costs some performance penalty in comparing with TVector<T>

  Full list of classes:
                                                                        managed   copy-on-write
    TVector<T>       generic record, light & fast wrapper for TArray<T>    +            -
    TVectorClass<T>  generic class, base for TCVector<T>                   -            -
    TCVector<T>      generic record, supports copy-on-write                +            +
    T2DVector<T>     generic record, wrapper for 2-dimensional array       +            -
}

uses
  adot.Types,
  adot.Collections.Iterators,
  adot.Collections.Types,
  adot.Collections.Slices,
  adot.Collections.Sets,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.Classes,
  System.Math;

type
  TArrayEnumeratorClass<T> = class(TEnumerator<T>)
  private
    FItems: TArray<T>;
    FCount: integer;
    FCurrentIndex: integer;

  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;

  public
    constructor Create(AItems: TArray<T>; ACount: integer);
  end;

  TArrayEnumeratorRec<T> = record
  private
    FItems: TArray<T>;
    FCount: integer;
    FCurrentIndex: integer;

    function GetCurrent: T;

  public
    procedure Init(AItems: TArray<T>; ACount: integer);
    function MoveNext: Boolean;
    property Current: T read GetCurrent;
  end;

  { Most efficient vector-like container, but doesn't support copy-on-write.
    Wrapper for TArray<T> (array with Add/Delete functionality). Example:
       function GetFiltered(const Src: TArray<integer>; Filter: TFunc<integer, boolean>): TArray<integer>;
       var
         V: TVector<integer>;
         I: integer;
       begin
         V.Init; // just init before first use, no need to free
         for I := 0 to High(Src) do
           if Filter(Src[I]) then
             V.Add(Src[I]); // more efficient than resizing TArray<T> every time
         Result := V.ToArray; // there is no copying of data here
       end;
   }
  TVector<T> = record
  public
    { we define it before other field to make access more efficient }
    Items: TArray<T>;
  private
    FCount: NativeInt;

    procedure SetCount(ACount: NativeInt);
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
    function ContainsAll(V: TArray<T>; C: IComparer<T>): boolean;

  public
    type
      TEnumerator = TArrayEnumeratorRec<T>;

    { Init is preferred over Create, it is obviously distinguished from classes.
      Class-like Create can be useful when new instance is param of some routine }
    procedure Init; overload;
    procedure Init(ACapacity: integer); overload;
    procedure Init(AItems: TArray<T>); overload;
    procedure Init(AItems: TEnumerable<T>; ACapacity: integer); overload;

    class function Create: TVector<T>; overload; static;
    class function Create(ACapacity: integer): TVector<T>; overload; static;
    class function Create(AItems: TArray<T>): TVector<T>; overload; static;
    class function Create(AItems: TEnumerable<T>; ACapacity: integer): TVector<T>; overload; static;

    { Creates empty slice on underlying array }
    function GetSlice: TSlice<T>; overload;
    { Creates slice from range of underlying array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates slice on the array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

    procedure Clear;
    procedure TrimExcess;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Value: TArray<T>); overload;
    procedure Add(const Value: TArray<T>; AStartIndex,ACount: integer); overload;
    procedure Add(const Value: TEnumerable<T>); overload;
    procedure Add(const Value: TVector<T>); overload;

    { Dynamic arrays in Delphi do not support copy-on-write.
      TVector is wrapper for TArray and doesn't support COW too. }
    function Copy: TVector<T>;

    function Insert(Index: integer; const Value: T): integer;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(AStartIndex,ACount: integer); overload;
    procedure Delete(const AIndices: TArray<integer>); overload;
    procedure Delete(AIndices: TSet<integer>); overload;
    procedure DeleteLast;

    procedure Remove(const V: T; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(AFilter: TFuncFilterValueIndex<T>); overload;

    function Extract(ItemIndex: integer): T;
    function ExtractAll: TArray<T>;
    function ExtractLast: T;

    procedure Move(SrcIndex, DstIndex: integer);

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

    function Contains(const Value: T): boolean; overload;
    function Contains(const Value: T; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TArray<T>): boolean; overload;
    function Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean; overload;

    function Sorted: boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparer: IComparer<T>): boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparison: TComparison<T>): boolean; overload;

    function IndexOf(const Value: T): integer; overload;
    function IndexOf(const Value: T; Comparer: IComparer<T>): integer; overload;
    function FindFirst(const Value: T; var Index: integer): boolean; overload;
    function FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;
    function FindNext(const Value: T; var Index: integer): boolean; overload;
    function FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;

    procedure Sort; overload;
    procedure Sort(AIndex, ACount: Integer); overload;
    procedure Sort(Comparer: IComparer<T>); overload;
    procedure Sort(Comparer: IComparer<T>; AIndex, ACount: Integer); overload;
    procedure Sort(Comparison: TComparison<T>); overload;
    procedure Sort(Comparison: TComparison<T>; AIndex, ACount: Integer); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex,ACount: Integer): Boolean; overload;

    { TArray }
    function Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(const B: TArray<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;
    { TEnumerable }
    function Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(B: TEnumerable<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;

    { Trims and returns Items }
    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;
    function ToArray: TArray<T>;

    { for-in support }
    function GetEnumerator: TEnumerator;         { record (default) }
    function GetEnumeratorClass: TEnumerator<T>; { class (if compatibility with TEnumerator<T> is required) }

    { iterators }
    function Start: TArrayIterator<T>;
    function Finish: TArrayIterator<T>;
    function RStart: TArrayRIterator<T>;
    function RFinish: TArrayRIterator<T>;

    class operator In(const a: T; b: TVector<T>) : Boolean;
    class operator In(a: TVector<T>; b: TVector<T>) : Boolean;
    class operator In(const a: TArray<T>; b: TVector<T>) : Boolean;
    class operator In(const a: TEnumerable<T>; b: TVector<T>) : Boolean;

    class operator Implicit(const a : T) : TVector<T>;
    class operator Implicit(const a : TArray<T>) : TVector<T>;
    class operator Implicit(const a : TEnumerable<T>) : TVector<T>;
    class operator Implicit(a : TVector<T>) : TArray<T>;

    class operator Explicit(const a : T) : TVector<T>;
    class operator Explicit(const a : TArray<T>) : TVector<T>;
    class operator Explicit(const a : TEnumerable<T>) : TVector<T>;
    class operator Explicit(a : TVector<T>) : TArray<T>;

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
    property Count: NativeInt read FCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Elements[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
  end;

  TVectorClass<T> = class(TEnumerableExt<T>)
  private
    Arr: TVector<T>;
    FOwnsValues: boolean;

    function GetCapacity: integer;
    function GetCount: NativeInt;
    function GetEmpty: boolean;
    function GetFirst: T;
    function GetItem(ItemIndex: integer): T;
    function GetLast: T;
    function GetTotalSizeBytes: int64;
    procedure SetCapacity(const Value: integer);
    procedure SetCount(const Value: NativeInt);
    procedure SetFirst(const Value: T);
    procedure SetItem(ItemIndex: integer; const Value: T);
    procedure SetLast(const Value: T);

  protected
    function DoGetEnumerator: TEnumerator<T>; override;

  public
    constructor Create; overload;
    constructor Create(ACapacity: integer); overload;
    constructor Create(AItems: TArray<T>); overload;

    destructor Destroy; override;

    { Creates empty slice on underlying array }
    function GetSlice: TSlice<T>; overload;
    { Creates slice from range of underlying array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates slice on the array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

    procedure Clear;
    procedure TrimExcess;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Value: TArray<T>); overload;
    procedure Add(const Value: TArray<T>; AStartIndex,ACount: integer); overload;
    procedure Add(const Value: TEnumerable<T>); overload;
    procedure Add(const Value: TVector<T>); overload;

    { Dynamic arrays in Delphi do not support copy-on-write.
      TVector is wrapper for TArray and doesn't support COW too. }
    function Copy: TVectorClass<T>;

    function Insert(Index: integer; const Value: T): integer;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(AStartIndex,ACount: integer); overload;
    procedure Delete(const AIndices: TArray<integer>); overload;
    procedure Delete(AIndices: TSet<integer>); overload;
    procedure DeleteLast;

    procedure Remove(const V: T; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(AFilter: TFuncFilterValueIndex<T>); overload;

    function Extract(ItemIndex: integer): T;
    function ExtractAll: TArray<T>;
    function ExtractLast: T;

    procedure Move(SrcIndex, DstIndex: integer);

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

    function Contains(const Value: T): boolean; overload;
    function Contains(const Value: T; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TArray<T>): boolean; overload;
    function Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean; overload;

    function Sorted: boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparer: IComparer<T>): boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparison: TComparison<T>): boolean; overload;

    function IndexOf(const Value: T): integer; overload;
    function IndexOf(const Value: T; Comparer: IComparer<T>): integer; overload;
    function FindFirst(const Value: T; var Index: integer): boolean; overload;
    function FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;
    function FindNext(const Value: T; var Index: integer): boolean; overload;
    function FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean; overload;

    procedure Sort; overload;
    procedure Sort(AIndex, ACount: Integer); overload;
    procedure Sort(Comparer: IComparer<T>); overload;
    procedure Sort(Comparer: IComparer<T>; AIndex, ACount: Integer); overload;
    procedure Sort(Comparison: TComparison<T>); overload;
    procedure Sort(Comparison: TComparison<T>; AIndex, ACount: Integer); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex,ACount: Integer): Boolean; overload;

    { TArray }
    function Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(const B: TArray<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;
    { TEnumerable }
    function Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(B: TEnumerable<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;

    { Trims and returns Items }
    function ToString: string; override;
    function ToText(const ValuesDelimiter: string = #13#10): string;
    function ToArray: TArray<T>; override;

    { iterators }
    function Start: TArrayIterator<T>;
    function Finish: TArrayIterator<T>;
    function RStart: TArrayRIterator<T>;
    function RFinish: TArrayRIterator<T>;

    { operator overloading is not allowed for classes yet }
    function Equal(const Values: TArray<T>): boolean; overload;
    function Equal(const Values: TEnumerable<T>): boolean; overload;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Count: NativeInt read GetCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Elements[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
    property OwnsValues: boolean read FOwnsValues write FOwnsValues;
  end;

  { Managed vector with copy-on-write support }
  TCVector<T> = record
  private
    FVectorInt: IInterfacedObject<TVectorClass<T>>;

    procedure CreateVector(ACapacity: integer = 0);

    function GetRO: TVectorClass<T>;
    function GetRW: TVectorClass<T>;
    function GetItemsArray: TArray<T>;
    function GetCount: NativeInt;
    function GetEmpty: Boolean;
    function GetCollection: TEnumerable<T>;
    function GetCapacity: integer;
    function GetFirst: T;
    function GetItem(ItemIndex: integer): T;
    function GetLast: T;
    function GetTotalSizeBytes: int64;
    procedure SetCapacity(const Value: integer);
    procedure SetCount(const Value: NativeInt);
    procedure SetFirst(const Value: T);
    procedure SetItem(ItemIndex: integer; const Value: T);
    procedure SetLast(const Value: T);
    function GetOwnsValues: boolean;
    procedure SetOwnsValues(const Value: boolean);

    property RO: TVectorClass<T> read GetRO;
    property RW: TVectorClass<T> read GetRW;
  public

    procedure Init; overload;
    procedure Init(ACapacity: integer); overload;
    procedure Init(const Values: TArray<T>); overload;
    procedure Init(const Values: TEnumerable<T>; ACapacity: integer = 0); overload;

    { 1. Delphi doesn't allow parameterless constructor
      2. Delphi creates here strange / not optimal code for Linux, but there is no problems with
         static functions. Followin example fails with constructor, but works as expected with function:
           A := TCVector<integer>.Create(10);
           A.Add([2,1,3]);
           Check(A.Capacity=10); }
    class function Create: TCVector<T>; overload; static;
    class function Create(ACapacity: integer): TCVector<T>; overload; static;
    class function Create(const Values: TArray<T>): TCVector<T>; overload; static;
    class function Create(const Values: TEnumerable<T>; ACapacity: integer = 0): TCVector<T>; overload; static;

    { Creates empty slice on underlying array }
    function GetSlice: TSlice<T>; overload;
    { Creates slice from range of underlying array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates slice on the array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

    procedure Clear;
    procedure TrimExcess;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Values: TArray<T>); overload;
    procedure Add(const Values: TArray<T>; AStartIndex,ACount: integer); overload;
    procedure Add(const Values: TEnumerable<T>); overload;
    procedure Add(Values: TCVector<T>); overload;

    { Normally it is not necessary to use Copy, TCVector supports copy-on-write }
    function Copy: TCVector<T>;

    function Insert(Index: integer; const Value: T): integer;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(AStartIndex,ACount: integer); overload;
    procedure Delete(const AIndices: TArray<integer>); overload;
    procedure Delete(AIndices: TSet<integer>); overload;
    procedure DeleteLast;

    procedure Remove(const V: T; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(AFilter: TFuncFilterValueIndex<T>); overload;

    { get & delete }
    function Extract(ItemIndex: integer): T;
    function ExtractAll: TArray<T>;
    function ExtractLast: T;

    procedure Move(SrcIndex, DstIndex: integer);

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

    function Contains(const Value: T): boolean; overload;
    function Contains(const Value: T; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TArray<T>): boolean; overload;
    function Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>): boolean; overload;
    function Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean; overload;

    function Sorted: boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparer: IComparer<T>): boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparison: TComparison<T>): boolean; overload;

    function IndexOf(const Value: T): integer; overload;
    function IndexOf(const Value: T; AComparer: IComparer<T>): integer; overload;
    function FindFirst(const Value: T; var Index: integer): boolean; overload;
    function FindFirst(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean; overload;
    function FindNext(const Value: T; var Index: integer): boolean; overload;
    function FindNext(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean; overload;

    procedure Sort; overload;
    procedure Sort(AIndex, ACount: Integer); overload;
    procedure Sort(Comparer: IComparer<T>); overload;
    procedure Sort(Comparer: IComparer<T>; AIndex, ACount: Integer); overload;
    procedure Sort(Comparison: TComparison<T>); overload;
    procedure Sort(Comparison: TComparison<T>; AIndex, ACount: Integer); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex,ACount: Integer): Boolean; overload;

    { TArray }
    function Compare(const B: TArray<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(const B: TArray<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;
    { TEnumerable }
    function Compare(B: TEnumerable<T>; AComparer: IComparer<T> = nil): integer; overload;
    function Compare(B: TEnumerable<T>; AStartIndex,BStartIndex,ACount: integer; AComparer: IComparer<T> = nil): integer; overload;

    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;
    function ToArray: TArray<T>;

    function GetEnumerator: TEnumerator<T>;

    procedure SaveToStream(Dst: TStream; Encoding: TEncoding = nil);
    procedure SaveToFile(const FileName: string; Encoding: TEncoding = nil; MemStream: boolean = True);

    { iterators }
    function Start: TArrayIterator<T>;
    function Finish: TArrayIterator<T>;
    function RStart: TArrayRIterator<T>;
    function RFinish: TArrayRIterator<T>;

    { operators }
    class operator In(const a: T; b: TCVector<T>) : Boolean;
    class operator In(a: TCVector<T>; b: TCVector<T>) : Boolean;
    class operator In(const a: TArray<T>; b: TCVector<T>) : Boolean;
    class operator In(const a: TEnumerable<T>; b: TCVector<T>) : Boolean;

    class operator Implicit(const a : T) : TCVector<T>;
    class operator Implicit(const a : TArray<T>) : TCVector<T>;
    class operator Implicit(const a : TEnumerable<T>) : TCVector<T>;
    { We don't want to have both conversions: ->TArray and ->TEnumerable,
      because in many cases it will create ambiguity (many methods support both as input).
      We support TEnumerable because it is safe. If someone needs TArray, he can use
      wither AsArray (for readonly access) or ToArray }
    class operator Implicit(a : TCVector<T>) : TEnumerable<T>;

    class operator Explicit(const a : T) : TCVector<T>;
    class operator Explicit(const a : TArray<T>) : TCVector<T>;
    class operator Explicit(const a : TEnumerable<T>) : TCVector<T>;
    { see comments for Implicit(a : TCVector<T>) : TEnumerable<T>; }
    class operator Explicit(a : TCVector<T>) : TEnumerable<T>;

    class operator Add(a: TCVector<T>; const b: T): TCVector<T>;
    class operator Add(a: TCVector<T>;       b: TCVector<T>): TCVector<T>;
    class operator Add(a: TCVector<T>; const b: TArray<T>): TCVector<T>;
    class operator Add(a: TCVector<T>; const b: TEnumerable<T>): TCVector<T>;
    class operator Add(const a: T;              b: TCVector<T>): TCVector<T>;
    class operator Add(const a: TArray<T>;      b: TCVector<T>): TCVector<T>;
    class operator Add(const a: TEnumerable<T>; b: TCVector<T>): TCVector<T>;

    class operator Subtract(a: TCVector<T>; const b: T): TCVector<T>;
    class operator Subtract(a: TCVector<T>;       b: TCVector<T>): TCVector<T>;
    class operator Subtract(a: TCVector<T>; const b: TArray<T>): TCVector<T>;
    class operator Subtract(a: TCVector<T>; const b: TEnumerable<T>): TCVector<T>;
    class operator Subtract(const a: T;              b: TCVector<T>): TCVector<T>;
    class operator Subtract(const a: TArray<T>;      b: TCVector<T>): TCVector<T>;
    class operator Subtract(const a: TEnumerable<T>; b: TCVector<T>): TCVector<T>;

    class operator Equal(a: TCVector<T>;       b: TCVector<T>) : Boolean;
    class operator Equal(a: TCVector<T>; const b: TArray<T>) : Boolean;
    class operator Equal(a: TCVector<T>; const b: TEnumerable<T>) : Boolean;
    class operator Equal(const b: TArray<T>;      a: TCVector<T>): Boolean;
    class operator Equal(const b: TEnumerable<T>; a: TCVector<T>): Boolean;

    class operator NotEqual(a: TCVector<T>;       b: TCVector<T>): Boolean;
    class operator NotEqual(a: TCVector<T>; const b: TArray<T>) : Boolean;
    class operator NotEqual(a: TCVector<T>; const b: TEnumerable<T>) : Boolean;
    class operator NotEqual(const b: TArray<T>;      a: TCVector<T>): Boolean;
    class operator NotEqual(const b: TEnumerable<T>; a: TCVector<T>): Boolean;

    class operator GreaterThanOrEqual(a: TCVector<T>;       b: TCVector<T>): Boolean;
    class operator GreaterThanOrEqual(a: TCVector<T>; const b: TArray<T>): Boolean;
    class operator GreaterThanOrEqual(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
    class operator GreaterThanOrEqual(const b: TArray<T>;      a: TCVector<T>): Boolean;
    class operator GreaterThanOrEqual(const b: TEnumerable<T>; a: TCVector<T>): Boolean;

    class operator GreaterThan(a: TCVector<T>;       b: TCVector<T>): Boolean;
    class operator GreaterThan(a: TCVector<T>; const b: TArray<T>): Boolean;
    class operator GreaterThan(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
    class operator GreaterThan(const b: TArray<T>;      a: TCVector<T>): Boolean;
    class operator GreaterThan(const b: TEnumerable<T>; a: TCVector<T>): Boolean;

    class operator LessThan(a: TCVector<T>;       b: TCVector<T>): Boolean;
    class operator LessThan(a: TCVector<T>; const b: TArray<T>): Boolean;
    class operator LessThan(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
    class operator LessThan(const b: TArray<T>;      a: TCVector<T>): Boolean;
    class operator LessThan(const b: TEnumerable<T>; a: TCVector<T>): Boolean;

    class operator LessThanOrEqual(a: TCVector<T>;       b: TCVector<T>): Boolean;
    class operator LessThanOrEqual(a: TCVector<T>; const b: TArray<T>): Boolean;
    class operator LessThanOrEqual(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
    class operator LessThanOrEqual(const b: TArray<T>;      a: TCVector<T>): Boolean;
    class operator LessThanOrEqual(const b: TEnumerable<T>; a: TCVector<T>): Boolean;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Count: NativeInt read GetCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Items[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
    property ItemsArray: TArray<T> read GetItemsArray;
    property Collection: TEnumerable<T> read GetCollection;
    property OwnsValues: boolean read GetOwnsValues write SetOwnsValues;
  end;

  { Copy-on-write doesn't make much sense for TObject* class,
    because there is no way to copy objects }
  //TCObjectVector<T: class> = record

  { Dynamic 2-dimensional array }
  T2DVector<T> = record
  public
    Rows: TVector<TVector<T>>;

    type

      TEnumerator = class(TEnumerator<T>)
      protected
        Rows: TVector<TVector<T>>;
        X,Y: integer;

        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;

      public
        constructor Create(const Rows: TVector<TVector<T>>);
      end;

      TCollection = class(TEnumerableExt<T>)
      protected
        Rows: TVector<TVector<T>>;

        function DoGetEnumerator: TEnumerator<T>; override;

      public
        constructor Create(const Rows: TVector<TVector<T>>);
      end;

  private
    function GetValue(x,y: integer): T;
    procedure SetValue(x, y: integer; const Value: T);
    function GetRowCount: integer;
    procedure SetRowCount(const Value: integer);
    function GetWidth(y: integer): integer;
    procedure SetWidth(y: integer; const Value: integer);

  public
    procedure Init; overload;
    procedure Init(Width, Height: integer); overload;
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

implementation

uses
  adot.Collections,
  adot.Tools,
  adot.Tools.RTTI,
  adot.Strings;

{ TVector<T> }

class function TVector<T>.Create: TVector<T>;
begin
  result.Init;
end;

class function TVector<T>.Create(ACapacity: integer): TVector<T>;
begin
  result.Init(ACapacity);
end;

class function TVector<T>.Create(AItems: TArray<T>): TVector<T>;
begin
  result.Init(AItems);
end;

class function TVector<T>.Create(AItems: TEnumerable<T>; ACapacity: integer): TVector<T>;
begin
  result.Init(AItems, ACapacity);
end;

procedure TVector<T>.Init;
begin
  Self := Default(TVector<T>);
end;

procedure TVector<T>.Init(ACapacity: integer);
begin
  Self := Default(TVector<T>);
  Capacity := ACapacity;
end;

procedure TVector<T>.Init(AItems: TArray<T>);
begin
  Self := Default(TVector<T>);
  Items := AItems;
  FCount := High(AItems)-Low(AItems)+1;
end;

procedure TVector<T>.Init(AItems: TEnumerable<T>; ACapacity: integer);
begin
  Self := Default(TVector<T>);
  Capacity := ACapacity;
  Add(AItems);
end;

function TVector<T>.Add: integer;
begin
  if Count >= Capacity then
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
begin
  Add(Value, 0, System.Length(Value));
end;

procedure TVector<T>.Add(const Value: TArray<T>; AStartIndex,ACount: integer);
var
  I: Integer;
begin
  if ACount <= 0 then
    Exit;
  I := Count + ACount;
  if I > Capacity then
    Capacity := I;
  for I := AStartIndex to AStartIndex+ACount-1 do
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
  Self := Default(TVector<T>);
end;

procedure TVector<T>.TrimExcess;
begin
  if Capacity>Count then
    Capacity := Count;
end;

function TVector<T>.ContainsAll(V: TArray<T>; C: IComparer<T>): boolean;
var
  B: TArray<boolean>;
  I,J,N: Integer;
begin
  if (System.Length(V) = 0) or (Count = 0) then
    Exit(False);

  { We can't use TSet, because it needs IEqualityComparer.GetHash,
    IComparer can not be tranformed into IEqualityComparer hasher }
  TArray.Sort<T>(V, C);

  { remove duplicates }
  J := 0;
  for I := 1 to High(V) do
    if C.Compare(V[I], V[J])<>0 then
    begin
      inc(J);
      Items[J] := Items[I];
    end;
  SetLength(V, J+1);

  { find items in sorted version of Values }
  SetLength(B, System.Length(V));
  N := System.Length(V);
  for I := 0 to FCount-1 do
    if TArray.BinarySearch<T>(V, Items[I], J, C) and not B[J] then
    begin
      B[J] := True;
      dec(N);
      if N <= 0 then
        Exit(True);
    end;
  result := False;
end;

function TVector<T>.Contains(const Value: T): boolean;
begin
  result := IndexOf(Value)>=0;
end;

function TVector<T>.Contains(const Value: T; Comparer: IComparer<T>): boolean;
begin
  result := IndexOf(Value, Comparer)>=0;
end;

function TVector<T>.Contains(const Values: TArray<T>): boolean;
begin
  result := ContainsAll(TArrayUtils.Copy<T>(Values), TComparerUtils.DefaultComparer<T>);
end;

function TVector<T>.Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean;
begin
  result := ContainsAll(TArrayUtils.Copy<T>(Values), Comparer);
end;

function TVector<T>.Compare(B: TEnumerable<T>; AComparer: IComparer<T>): integer;
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

function TVector<T>.Compare(const B: TArray<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
var
  I: Integer;
begin
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  Assert((ACount=0) or (ACount>0) and (AStartIndex>=0) and (AStartIndex+ACount-1<Count));
  Assert((ACount=0) or (ACount>0) and (BStartIndex>=0) and (BStartIndex+ACount-1<System.Length(B)));
  if ACount <= 0 then
    result := 0
  else
    for I := 0 to ACount-1 do
    begin
      result := AComparer.Compare(Items[I+AStartIndex], B[I+BStartIndex]);
      if result <> 0 then
        Break;
    end;
end;

function TVector<T>.Compare(const B: TArray<T>; AComparer: IComparer<T>): integer;
begin
  if Count = System.Length(B) then
    result := Compare(B,0,0,Count, AComparer)
  else
    if Count < System.Length(B)
      then result := -1
      else result := 1;
end;

function TVector<T>.Compare(B: TEnumerable<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
var
  Value: T;
  BItemsCount: integer;
begin
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
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
      result := AComparer.Compare(Items[AStartIndex], Value);
      inc(AStartIndex);
      dec(ACount);
      if (result <> 0) or (ACount <= 0) then
        break;
    end;
end;

function TVector<T>.Contains(const Values: TEnumerable<T>): boolean;
begin
  result := ContainsAll(Values.ToArray, TComparerUtils.DefaultComparer<T>);
end;

function TVector<T>.Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean;
begin
  result := ContainsAll(Values.ToArray, Comparer);
end;

function TVector<T>.Copy: TVector<T>;
begin
  result.Clear;
  result.Items := TArrayUtils.Copy<T>(Items, 0, Count);
  result.FCount := Count;
end;

procedure TVector<T>.Delete(ItemIndex: integer);
var
  I: Integer;
begin
  Assert((ItemIndex>=0) and (ItemIndex<FCount));
  for I := ItemIndex to Count-2 do
    Items[I] := Items[I+1];
  Dec(FCount);
  Items[FCount] := Default(T);
end;

procedure TVector<T>.Delete(AStartIndex,ACount: integer);
var
  I,C: Integer;
begin
  Assert((AStartIndex>=0) and (AStartIndex<Count) and (ACount>=0) and (AStartIndex+ACount-1<Count));
  C := Count-ACount; { new Count }
  for I := AStartIndex to C-1 do
    Items[I] := Items[I+ACount];
  for I := C to Count-1 do
    Items[I] := Default(T);
  FCount := C;
end;

procedure TVector<T>.Delete(const AIndices: TArray<integer>);
begin
  Delete(TSet<integer>.Create(AIndices));
end;

procedure TVector<T>.Delete(AIndices: TSet<integer>);
var
  S,D,I: Integer;
begin
  if (AIndices.Count = 0) or (Count = 0) then
    Exit;
  S := 0;
  D := 0;
  for I := 0 to Count-1 do
    if not (I in AIndices) then
    begin
      Items[D] := Items[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    Items[I] := Default(T);
  FCount := D;
end;

procedure TVector<T>.DeleteLast;
begin
  Assert(FCount>0);
  Dec(FCount);
  Items[FCount] := Default(T);
end;

class operator TVector<T>.Equal(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TVector<T>.Equal(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

procedure TVector<T>.Exchange(Index1, Index2: integer);
var Value: T;
begin
  Assert((Index1>=0) and (Index1<Count) and (Index2>=0) and (Index2<Count));
  Value := Items[Index1];
  Items[Index1] := Items[Index2];
  Items[Index2] := Value;
end;

class operator TVector<T>.Explicit(const a: T): TVector<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TVector<T>.Explicit(const a: TArray<T>): TVector<T>;
begin
  result.Init(a);
end;

class operator TVector<T>.Explicit(const a: TEnumerable<T>): TVector<T>;
begin
  result.Init(a, 0);
end;

class operator TVector<T>.Explicit(a: TVector<T>): TArray<T>;
begin
  result := a.ToArray;
end;

function TVector<T>.ToString: string;
begin
  result := ToText(' ');
end;

function TVector<T>.ToText(const ValuesDelimiter: string = #13#10): string;
var
  S: TStringBuilder;
  I: Integer;
begin
  S := TStringBuilder.Create;
  for I := 0 to FCount-1 do
  begin
    if S.Length > 0 then
      S.Append(ValuesDelimiter);
    S.Append(TRttiUtils.ValueAsString<T>(Items[I]));
  end;
  result := S.ToString;
end;

function TVector<T>.ToArray: TArray<T>;
begin
  TrimExcess;
  result := Items;
end;

function TVector<T>.Extract(ItemIndex: integer): T;
begin
  Assert((ItemIndex>=0) and (ItemIndex<Count));
  result := Items[ItemIndex];
  Items[ItemIndex] := Default(T);
  Delete(ItemIndex);
end;

function TVector<T>.ExtractAll: TArray<T>;
var L: TArray<T>;
begin
  TrimExcess;
  result := Items;
  SetLength(L, 0);
  Items := L;
  FCount := 0;
end;

function TVector<T>.ExtractLast: T;
begin
  Assert(FCount>0);
  Dec(FCount);
  result := Items[FCount];
  Items[FCount] := Default(T);
end;

class operator TVector<T>.GreaterThanOrEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.GreaterThanOrEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.GreaterThan(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThan(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.GreaterThan(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.GreaterThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.GreaterThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

procedure TVector<T>.Grow;
begin
  if Capacity < 4 then
    Capacity := Capacity+1
  else
  if Capacity < 64 then
    Capacity := 64
  else
    Capacity := Capacity * 2;
end;

class operator TVector<T>.In(const a: T; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.In(a, b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.In(const a: TArray<T>; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TVector<T>.Implicit(const a: T): TVector<T>;
begin
  result.Init;
  result.Add(A);
end;

class operator TVector<T>.Implicit(const a: TArray<T>): TVector<T>;
begin
  result.Init(a);
end;

class operator TVector<T>.Implicit(const a: TEnumerable<T>): TVector<T>;
begin
  result.Init(a, 0);
end;

class operator TVector<T>.Implicit(a: TVector<T>): TArray<T>;
begin
  result := a.ToArray;
end;

class operator TVector<T>.In(const a: TEnumerable<T>; b: TVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

function TVector<T>.IndexOf(const Value: T; Comparer: IComparer<T>): integer;
begin
  if not FindFirst(Value, Result, Comparer) then
    result := -1;
end;

function TVector<T>.IndexOf(const Value: T): integer;
begin
  if not FindFirst(Value, Result) then
    result := -1;
end;

function TVector<T>.FindFirst(const Value: T; var Index: integer): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, TComparerUtils.DefaultComparer<T>);
end;

function TVector<T>.FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
begin
  Index := -1;
  result := FindNext(Value, Index, Comparer);
end;

function TVector<T>.FindNext(const Value: T; var Index: integer): boolean;
begin
  result := FindNext(Value, Index, TComparerUtils.DefaultComparer<T>);
end;

function TVector<T>.FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
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

procedure TVector<T>.FirstPermutation;
begin
  Sort;
end;

function TVector<T>.Insert(Index: integer; const Value: T): integer;
begin
  for result := Add downto Index+1 do
    Items[result] := Items[result-1];
  result := Index;
  Items[result] := Value;
end;

class operator TVector<T>.LessThan(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TVector<T>.LessThan(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.LessThan(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TVector<T>.LessThanOrEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TVector<T>.LessThanOrEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TVector<T>.LessThanOrEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

procedure TVector<T>.Move(SrcIndex, DstIndex: integer);
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

function TVector<T>.NextPermutation: boolean;
var
  i,x,n: integer;
  C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;

  { find max N where A[N] < A[N+1] }
  n := -1;
  for i := Count-2 downto 0 do
    if C.Compare(Items[i], Items[i+1]) < 0 then
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
    if C.Compare(Items[i], Items[N]) > 0 then
    begin
      x := i;
      break;
    end;

  { swap A[N] and A[X] }
  Exchange(n, x);

  { change position of A[X] to make range [N+1; FCoun-1] ordered again }
  i := x;
  while (i > n+1) and (C.Compare(Items[i-1], Items[x]) > 0) do
    dec(i);
  while (i < Count-1) and (C.Compare(Items[x], Items[i+1]) > 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

class operator TVector<T>.NotEqual(a: TVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(a, b: TVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(a: TVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(const b: TArray<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TVector<T>.NotEqual(const b: TEnumerable<T>; a: TVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

function TVector<T>.PrevPermutation: boolean;
var
  i,x,n: integer;
  C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;

  { find max N where A[N] > A[N+1] }
  n := -1;
  for i := FCount-2 downto 0 do
    if C.Compare(Items[i], Items[i+1]) > 0 then
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
    if C.Compare(Items[i], Items[N]) < 0 then
    begin
      x := i;
      break;
    end;

  { swap A[N] and A[X] }
  Exchange(n,x);

  { change position of A[X] to make range [N+1; FCoun-1] back ordered again }
  i := x;
  while (i > n+1) and (C.Compare(Items[i-1], Items[x]) < 0) do
    dec(i);
  while (i < FCount-1) and (C.Compare(Items[x], Items[i+1]) < 0) do
    inc(i);
  if i<>x then
    Move(x,i);
end;

procedure TVector<T>.Remove(const V: T; AComparer: IComparer<T>);
var
  I,D: Integer;
begin
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  D := 0;
  for I := 0 to FCount-1 do
    if AComparer.Compare(Items[I], V) <> 0 then
    begin
      Items[D] := Items[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    Items[I] := Default(T);
  FCount := D;
end;

procedure TVector<T>.Remove(const V: TArray<T>; AComparer: IComparer<T>);
var
  S: TVector<T>;
  I,J,D: integer;
begin
  if (System.Length(V)=0) or (Count=0) then
    Exit;
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  S.Init(TArrayUtils.Copy<T>(V));
  S.Sort(AComparer);
  D := 0;
  for I := 0 to FCount-1 do
    if not S.BinarySearch(Items[I],J,AComparer) then
    begin
      Items[D] := Items[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    Items[I] := Default(T);
  FCount := D;
end;

procedure TVector<T>.Remove(const V: TEnumerable<T>; AComparer: IComparer<T>);
begin
  Remove(V.ToArray, AComparer);
end;

procedure TVector<T>.Remove(AFilter: TFuncFilterValueIndex<T>);
var
  I,D: Integer;
begin
  D := 0;
  for I := 0 to FCount-1 do
    if not AFilter(Items[I], I) then
    begin
      Items[D] := Items[I];
      inc(D);
    end;
  for I := D to FCount-1 do
    Items[I] := Default(T);
  FCount := D;
end;

procedure TVector<T>.Reverse;
begin
  Reverse(0, Count);
end;

procedure TVector<T>.Reverse(AStartIndex, ACount: integer);
var
  I: Integer;
  Value: T;
begin
  if ACount <= 0 then
    Exit;
  Assert((AStartIndex >= 0) and (AStartIndex + ACount <= Count));
  for I := 0 to (ACount shr 1) - 1 do
  begin
    Value := Items[AStartIndex+I];
    Items[AStartIndex+I] := Items[AStartIndex+ACount-1-I];
    Items[AStartIndex+ACount-1-I] := Value;
  end;
end;

function TVector<T>.RFinish: TArrayRIterator<T>;
begin
  result.Init(@Items[0]);
  dec(result);
end;

procedure TVector<T>.RotateLeft(Index1, Index2, Shift: integer);
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

procedure TVector<T>.RotateRight(Index1, Index2, Shift: integer);
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

function TVector<T>.RStart: TArrayRIterator<T>;
begin
  result.Init(@Items[0]);
  inc(result, FCount-1);
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

function TVector<T>.GetEmpty: Boolean;
begin
  Result := FCount <= 0;
end;

function TVector<T>.GetEnumerator: TEnumerator;
begin
  result.Init(Items, Count);
end;

function TVector<T>.GetEnumeratorClass: TEnumerator<T>;
begin
  result := TArrayEnumeratorClass<T>.Create(Items, Count);
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

function TVector<T>.GetSlice: TSlice<T>;
begin
  result.Init(Items, FCount);
end;

function TVector<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
begin
  result.Init(Items, FCount);
  result.Add(AStartSliceIndexIncl, AEndSliceIndexExcl);
end;

function TVector<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
var
  I: Integer;
begin
  result.Init(Items, FCount);
  for I := 0 to FCount-1 do
    if AFilter(Items[I], I) then
      result.Add(I);
end;

function TVector<T>.GetTotalSizeBytes: int64;
begin
  result := (High(Items)-Low(Items)+1)*SizeOf(T);
end;

procedure TVector<T>.SetLast(const Value: T);
begin
  Items[Count-1] := Value;
end;

procedure TVector<T>.Shuffle(AStartIndex, ACount: integer);
var
  I: Integer;
begin
  if ACount <= 1 then
    Exit;
  Assert((AStartIndex >= 0) and (AStartIndex + ACount <= Count));
  for I := ACount-1 downto 1 do
    Exchange(I+AStartIndex, Random(I+1)+AStartIndex);
end;

procedure TVector<T>.Shuffle;
begin
  Shuffle(0, Count);
end;

function TVector<T>.GetItem(ItemIndex: integer): T;
begin
  Assert((ItemIndex >= 0) and (ItemIndex < Count));
  result := Items[ItemIndex];
end;

procedure TVector<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  Assert((ItemIndex >= 0) and (ItemIndex < Count));
  Items[ItemIndex] := Value;
end;

procedure TVector<T>.SetCount(ACount: NativeInt);
var
  I: Integer;
begin
  for I := ACount to Count-1 do
    Items[I] := Default(T);
  FCount := ACount;
  if ACount > Capacity then
    Capacity := ACount;
end;

procedure TVector<T>.Sort;
begin
  TArray.Sort<T>(Items, TComparerUtils.DefaultComparer<T>, 0,Count);
end;

procedure TVector<T>.Sort(AIndex, ACount: Integer);
begin
  TArray.Sort<T>(Items, TComparerUtils.DefaultComparer<T>, AIndex, ACount);
end;

procedure TVector<T>.Sort(Comparer: IComparer<T>);
begin
  Sort(Comparer, 0, Count);
end;

procedure TVector<T>.Sort(Comparer: IComparer<T>; AIndex, ACount: Integer);
begin
  TArray.Sort<T>(Items, Comparer, AIndex, ACount);
end;

procedure TVector<T>.Sort(Comparison: TComparison<T>);
begin
  Sort(Comparison, 0, Count);
end;

procedure TVector<T>.Sort(Comparison: TComparison<T>; AIndex, ACount: Integer);
var
  C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(Comparison);
  TArray.Sort<T>(Items, C, AIndex, ACount);
end;

function TVector<T>.Sorted: boolean;
var C: IComparer<T>;
begin
  C := TComparerUtils.DefaultComparer<T>;
  result := TArrayUtils.Sorted<T>(Items, 0, Count, C);
end;

function TVector<T>.Sorted(AStartIndex, ACount: integer; AComparer: IComparer<T>): boolean;
begin
  result := TArrayUtils.Sorted<T>(Items, AStartIndex, ACount, AComparer);
end;

function TVector<T>.Sorted(AStartIndex, ACount: integer; AComparison: TComparison<T>): boolean;
var C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(AComparison);
  result := TArrayUtils.Sorted<T>(Items, AStartIndex, ACount, C);
end;

function TVector<T>.Start: TArrayIterator<T>;
begin
  result.Init(@Items[0]);
end;

function TVector<T>.Finish: TArrayIterator<T>;
begin
  result.Init(@Items[0]);
  inc(result, FCount);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: TArray<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(a, b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: T): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(const a: TArray<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TVector<T>.Subtract(const a: T; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer): Boolean;
var C: IComparer<T>;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, TComparerUtils.DefaultComparer<T>, 0, Count);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, Comparer, 0, Count);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean;
begin
  result := TArray.BinarySearch<T>(Items, Item, FoundIndex, Comparer, AIndex,ACount);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex, ACount: Integer): Boolean;
begin
  result := BinarySearch(Item, FoundIndex, TDelegatedComparer<T>.Create(Comparison), AIndex, ACount);
end;

function TVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean;
begin
  result := BinarySearch(Item, FoundIndex, TDelegatedComparer<T>.Create(Comparison), 0, Count);
end;

procedure TVector<T>.Add(const Value: TVector<T>);
var
  I: Integer;
begin
  for I := 0 to Value.Count-1 do
    Items[Add] := Value[I];
end;

{ T2DVector<T>.TCollectionEnumerator }

constructor T2DVector<T>.TEnumerator.Create(const Rows: TVector<TVector<T>>);
begin
  inherited Create;
  Self.Rows := Rows;
  X := -1;
  Y := 0;
end;

function T2DVector<T>.TEnumerator.DoMoveNext: Boolean;
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

function T2DVector<T>.TEnumerator.DoGetCurrent: T;
begin
  result := Rows[Y][X];
end;

class operator TVector<T>.Add(a: TVector<T>; const b: TArray<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a, b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a: TVector<T>; const b: T): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(a: TVector<T>; const b: TEnumerable<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: TEnumerable<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: TArray<T>; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TVector<T>.Add(const a: T; b: TVector<T>): TVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

{ T2DVector<T>.TCollection }

constructor T2DVector<T>.TCollection.Create(const Rows: TVector<TVector<T>>);
begin
  inherited Create;
  Self.Rows := Rows;
end;

function T2DVector<T>.TCollection.DoGetEnumerator: TEnumerator<T>;
begin
  result := TEnumerator.Create(Rows);
end;

{ T2DVector<T> }

procedure T2DVector<T>.Init;
begin
  Self := Default(T2DVector<T>);
end;

procedure T2DVector<T>.Init(Width, Height: integer);
var
  Y: integer;
  V: TVector<T>;
begin
  Init;
  Rows.Init;
  Rows.Count := Height;
  for Y := 0 to Height-1 do
  begin
    Rows[Y].Init;
    Rows.Items[Y].Count := Width;
  end;
end;

procedure T2DVector<T>.Clear;
begin
  Init;
end;

function T2DVector<T>.Collection: IInterfacedObject<TEnumerable<T>>;
begin
  result := TInterfacedObject<TEnumerable<T>>.Create(TCollection.Create(Rows));
end;

function T2DVector<T>.Add(y: integer): integer;
begin
  result := Rows.Items[y].Add;
end;

function T2DVector<T>.Add(y: integer; const Value: T): integer;
begin
  result := Rows.Items[y].Add(Value);
end;

function T2DVector<T>.Add(y: integer; const Values: TEnumerable<T>): integer;
var
  Value: T;
begin
  result := -1;
  for Value in Values do
    result := Self.Rows.Items[y].Add(Value);
end;

function T2DVector<T>.Add(y: integer; const Values: TArray<T>): integer;
var
  I: Integer;
begin
  result := -1;
  for I := Low(Values) to High(Values) do
    result := Self.Rows.Items[y].Add(Values[I]);
end;

function T2DVector<T>.AddRow: integer;
begin
  result := Rows.Add;
end;

function T2DVector<T>.GetValue(x, y: integer): T;
begin
  result := Rows.Items[y].Items[x];
end;

procedure T2DVector<T>.SetValue(x, y: integer; const Value: T);
begin
  assert((y>=0) and (y<Rows.FCount));
  assert((x>=0) and (x<Rows.Items[y].FCount));
  Rows.Items[y].Items[x] := Value;
end;

function T2DVector<T>.GetRowCount: integer;
begin
  result := Rows.Count;
end;

procedure T2DVector<T>.SetRowCount(const Value: integer);
begin
  Rows.Count := Value;
end;

function T2DVector<T>.GetWidth(y: integer): integer;
begin
  result := Rows.Items[y].Count;
end;


procedure T2DVector<T>.SetWidth(y: integer; const Value: integer);
begin
  Rows.Items[y].Count := Value;
end;

{ TArrayEnumeratorClass<T> }

constructor TArrayEnumeratorClass<T>.Create(AItems: TArray<T>; ACount: integer);
begin
  FItems := AItems;
  FCount := ACount;
end;

function TArrayEnumeratorClass<T>.DoGetCurrent: T;
begin
  result := FITems[FCurrentIndex-1];
end;

function TArrayEnumeratorClass<T>.DoMoveNext: Boolean;
begin
  result := FCurrentIndex < FCount;
  if result then
    inc(FCurrentIndex);
end;

{ TArrayEnumeratorRec<T> }

procedure TArrayEnumeratorRec<T>.Init(AItems: TArray<T>; ACount: integer);
begin
  Self := Default(TArrayEnumeratorRec<T>);
  FItems := AItems;
  FCount := ACount;
end;

function TArrayEnumeratorRec<T>.GetCurrent: T;
begin
  result := FITems[FCurrentIndex-1];
end;

function TArrayEnumeratorRec<T>.MoveNext: Boolean;
begin
  result := FCurrentIndex < FCount;
  if result then
    inc(FCurrentIndex);
end;

{ TVectorClass<T> }

procedure TVectorClass<T>.Add(const Value: TArray<T>; AStartIndex, ACount: integer);
begin
  Arr.Add(Value, AStartIndex, ACount);
end;

procedure TVectorClass<T>.Add(const Value: TEnumerable<T>);
begin
  Arr.Add(Value);
end;

procedure TVectorClass<T>.Add(const Value: TVector<T>);
begin
  Arr.Add(Value);
end;

function TVectorClass<T>.Add: integer;
begin
  result := Arr.Add;
end;

function TVectorClass<T>.Add(const Value: T): integer;
begin
  result := Arr.Add(Value);
end;

procedure TVectorClass<T>.Add(const Value: TArray<T>);
begin
  Arr.Add(Value);
end;

function TVectorClass<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean;
begin
  result := Arr.BinarySearch(Item, FoundIndex, Comparer);
end;

function TVectorClass<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean;
begin
  result := Arr.BinarySearch(Item, FoundIndex, Comparison);
end;

function TVectorClass<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex, ACount: Integer): Boolean;
begin
  result := Arr.BinarySearch(Item, FoundIndex, Comparer, AIndex, ACount);
end;

function TVectorClass<T>.BinarySearch(const Item: T; out FoundIndex: Integer): Boolean;
begin
  result := Arr.BinarySearch(Item, FoundIndex);
end;

function TVectorClass<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex, ACount: Integer): Boolean;
begin
  result := Arr.BinarySearch(Item, FoundIndex, Comparison, AIndex, ACount);
end;

procedure TVectorClass<T>.Clear;
var
  I: Integer;
begin
  if FOwnsValues then
    for I := Arr.FCount-1 downto 0 do
      PObject(@Arr.Items[I])^.DisposeOf;
  Arr.Clear;
end;

function TVectorClass<T>.Compare(B: TEnumerable<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
begin
  result := Arr.Compare(B, AStartIndex, BStartIndex, ACount, AComparer);
end;

function TVectorClass<T>.Compare(B: TEnumerable<T>; AComparer: IComparer<T>): integer;
begin
  result := Arr.Compare(B, AComparer);
end;

function TVectorClass<T>.Compare(const B: TArray<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
begin
  result := Arr.Compare(B, AStartIndex, BStartIndex, ACount, AComparer);
end;

function TVectorClass<T>.Compare(const B: TArray<T>; AComparer: IComparer<T>): integer;
begin
  result := Arr.Compare(B, AComparer);
end;

function TVectorClass<T>.Contains(const Values: TEnumerable<T>): boolean;
begin
  result := Arr.Contains(Values);
end;

function TVectorClass<T>.Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean;
begin
  result := Arr.Contains(Values, Comparer);
end;

function TVectorClass<T>.Contains(const Values: TArray<T>): boolean;
begin
  result := Arr.Contains(Values);
end;

function TVectorClass<T>.Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean;
begin
  result := Arr.Contains(Values, Comparer);
end;

function TVectorClass<T>.Contains(const Value: T): boolean;
begin
  result := Arr.Contains(Value);
end;

function TVectorClass<T>.Contains(const Value: T; Comparer: IComparer<T>): boolean;
begin
  result := Arr.Contains(Value, Comparer);
end;

function TVectorClass<T>.Copy: TVectorClass<T>;
begin
  result := TVectorClass<T>.Create;
  result.Add(Arr.Items, 0, Arr.Count);
end;

constructor TVectorClass<T>.Create;
begin
  Arr.Init;
end;

constructor TVectorClass<T>.Create(AItems: TArray<T>);
begin
  Arr.Init(AItems);
end;

constructor TVectorClass<T>.Create(ACapacity: integer);
begin
  Arr.Init(ACapacity);
end;

procedure TVectorClass<T>.Delete(AIndices: TSet<integer>);
var
  I: Integer;
begin
  if FOwnsValues then
    for I := 0 to Count-1 do
      if I in AIndices then
        PObject(@Arr.Items[I])^.DisposeOf;
  Arr.Delete(AIndices);
end;

procedure TVectorClass<T>.Delete(const AIndices: TArray<integer>);
begin
  Delete(TSet<integer>.Create(AIndices));
end;

procedure TVectorClass<T>.Delete(ItemIndex: integer);
begin
  if FOwnsValues then
    PObject(@Arr.Items[ItemIndex])^.DisposeOf;
  Arr.Delete(ItemIndex);
end;

procedure TVectorClass<T>.Delete(AStartIndex, ACount: integer);
var
  I: Integer;
begin
  if FOwnsValues then
    for I := AStartIndex to AStartIndex+ACount-1 do
      PObject(@Arr.Items[I])^.DisposeOf;
  Arr.Delete(AStartIndex, ACount);
end;

procedure TVectorClass<T>.DeleteLast;
begin
  Assert(Arr.FCount>0);
  if FOwnsValues then
    PObject(@Arr.Items[Arr.FCount-1])^.DisposeOf;
  Arr.DeleteLast;
end;

destructor TVectorClass<T>.Destroy;
begin
  Clear;
  inherited;
end;

function TVectorClass<T>.DoGetEnumerator: TEnumerator<T>;
begin
  result := TArrayEnumeratorClass<T>.Create(Arr.Items, Arr.Count);
end;

function TVectorClass<T>.Equal(const Values: TEnumerable<T>): boolean;
begin
  result := Compare(Values) = 0;
end;

function TVectorClass<T>.Equal(const Values: TArray<T>): boolean;
begin
  result := Compare(Values) = 0;
end;

procedure TVectorClass<T>.Exchange(Index1, Index2: integer);
begin
  Arr.Exchange(Index1, Index2);
end;

function TVectorClass<T>.Extract(ItemIndex: integer): T;
begin
  result := Arr.Extract(ItemIndex);
end;

function TVectorClass<T>.ExtractAll: TArray<T>;
begin
  result := Arr.ExtractAll;
end;

function TVectorClass<T>.ExtractLast: T;
begin
  result := Arr.ExtractLast;
end;

function TVectorClass<T>.FindFirst(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
begin
  result := Arr.FindFirst(Value, Index, Comparer);
end;

function TVectorClass<T>.FindFirst(const Value: T; var Index: integer): boolean;
begin
  result := Arr.FindFirst(Value, Index);
end;

function TVectorClass<T>.FindNext(const Value: T; var Index: integer; Comparer: IComparer<T>): boolean;
begin
  result := Arr.FindNext(Value, Index, Comparer);
end;

function TVectorClass<T>.FindNext(const Value: T; var Index: integer): boolean;
begin
  result := Arr.FindNext(Value, Index);
end;

procedure TVectorClass<T>.FirstPermutation;
begin
  Arr.FirstPermutation;
end;

function TVectorClass<T>.GetCapacity: integer;
begin
  result := Arr.Capacity;
end;

function TVectorClass<T>.GetCount: NativeInt;
begin
  result := Arr.Count;
end;

function TVectorClass<T>.GetEmpty: boolean;
begin
  result := Arr.Empty;
end;

function TVectorClass<T>.GetFirst: T;
begin
  result := Arr.First;
end;

function TVectorClass<T>.GetItem(ItemIndex: integer): T;
begin
  result := Arr[ItemIndex];
end;

function TVectorClass<T>.GetLast: T;
begin
  result := Arr.Last;
end;

function TVectorClass<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
begin
  result := Arr.GetSlice(AFilter);
end;

function TVectorClass<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
begin
  result := Arr.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl);
end;

function TVectorClass<T>.GetSlice: TSlice<T>;
begin
  result := Arr.GetSlice;
end;

function TVectorClass<T>.GetTotalSizeBytes: int64;
begin
  result := Arr.TotalSizeBytes;
end;

function TVectorClass<T>.IndexOf(const Value: T; Comparer: IComparer<T>): integer;
begin
  result := Arr.IndexOf(Value, Comparer);
end;

function TVectorClass<T>.IndexOf(const Value: T): integer;
begin
  result := Arr.IndexOf(Value);
end;

function TVectorClass<T>.Insert(Index: integer; const Value: T): integer;
begin
  result := Arr.Insert(Index, Value);
end;

procedure TVectorClass<T>.Move(SrcIndex, DstIndex: integer);
begin
  Arr.Move(SrcIndex, DstIndex);
end;

function TVectorClass<T>.NextPermutation: boolean;
begin
  result := Arr.NextPermutation;
end;

function TVectorClass<T>.PrevPermutation: boolean;
begin
  result := Arr.PrevPermutation;
end;

procedure TVectorClass<T>.Remove(AFilter: TFuncFilterValueIndex<T>);
var
  I,D: Integer;
begin
  {
    We do not use Arr.Remove here, to avoid double call of AFilter
    (it may return another result)
  }
  D := 0;
  for I := 0 to Arr.FCount-1 do
    if AFilter(Arr.Items[I], I) then
    begin
      if FOwnsValues then
        PObject(@Arr.Items[I])^.DisposeOf;
    end
    else
    begin
      Arr.Items[D] := Arr.Items[I];
      inc(D);
    end;
  for I := D to Arr.FCount-1 do
    Arr.Items[I] := Default(T);
  Arr.FCount := D;
end;

procedure TVectorClass<T>.Remove(const V: TEnumerable<T>; AComparer: IComparer<T>);
begin
  Remove(V.ToArray, AComparer);
end;

procedure TVectorClass<T>.Remove(const V: TArray<T>; AComparer: IComparer<T>);
var
  S: TVector<T>;
  I,J,D: integer;
begin
  {
    We do not use Arr.Remove here, to avoid ordering of array V several times.
  }
  if (Length(V)=0) or (Count=0) then
    Exit;
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  S.Init(TArrayUtils.Copy<T>(V));
  S.Sort(AComparer);
  D := 0;
  for I := 0 to Arr.FCount-1 do
    if S.BinarySearch(Arr.Items[I],J,AComparer) then
    begin
      if FOwnsValues then
        PObject(@Arr.Items[I])^.DisposeOf;
    end
    else
    begin
      Arr.Items[D] := Arr.Items[I];
      inc(D);
    end;
  for I := D to Arr.FCount-1 do
    Arr.Items[I] := Default(T);
  Arr.FCount := D;
end;

procedure TVectorClass<T>.Remove(const V: T; AComparer: IComparer<T>);
var
  I: Integer;
begin
  if AComparer = nil then
    AComparer := TComparerUtils.DefaultComparer<T>;
  if FOwnsValues then
    for I := 0 to Arr.FCount-1 do
      if AComparer.Compare(Arr.Items[I], V) = 0 then
        PObject(@Arr.Items[I])^.DisposeOf;
  Arr.Remove(V, AComparer);
end;

procedure TVectorClass<T>.Reverse(AStartIndex, ACount: integer);
begin
  Arr.Reverse(AStartIndex, ACount);
end;

procedure TVectorClass<T>.Reverse;
begin
  Arr.Reverse;
end;

procedure TVectorClass<T>.RotateLeft(Index1, Index2, Shift: integer);
begin
  Arr.RotateLeft(Index1, Index2, Shift);
end;

procedure TVectorClass<T>.RotateRight(Index1, Index2, Shift: integer);
begin
  Arr.RotateRight(Index1, Index2, Shift);
end;

procedure TVectorClass<T>.SetCapacity(const Value: integer);
begin
  Arr.Capacity := Value;
end;

procedure TVectorClass<T>.SetCount(const Value: NativeInt);
var
  I: Integer;
begin
  if FOwnsValues then
    for I := Value to Arr.Count-1 do
      PObject(@Arr.Items[I])^.DisposeOf;
  Arr.Count := Value;
end;

procedure TVectorClass<T>.SetFirst(const Value: T);
begin
  Assert(Arr.FCount>0);
  if FOwnsValues then
    PObject(@Arr.Items[0])^.DisposeOf;
  Arr.First := Value;
end;

procedure TVectorClass<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  Assert((ItemIndex >= 0) and (ItemIndex < Arr.FCount));
  if FOwnsValues then
    PObject(@Arr.Items[ItemIndex])^.DisposeOf;
  Arr.Items[ItemIndex] := Value;
end;

procedure TVectorClass<T>.SetLast(const Value: T);
begin
  Assert(Arr.FCount>0);
  if FOwnsValues then
    PObject(@Arr.Items[Arr.FCount-1])^.DisposeOf;
  Arr.Last := Value;
end;

procedure TVectorClass<T>.Shuffle;
begin
  Arr.Shuffle;
end;

procedure TVectorClass<T>.Shuffle(AStartIndex, ACount: integer);
begin
  Arr.Shuffle(AStartIndex, ACount);
end;

procedure TVectorClass<T>.Sort(Comparer: IComparer<T>);
begin
  Arr.Sort(Comparer);
end;

procedure TVectorClass<T>.Sort(Comparison: TComparison<T>; AIndex, ACount: Integer);
begin
  Arr.Sort(Comparison, AIndex, ACount);
end;

procedure TVectorClass<T>.Sort(Comparer: IComparer<T>; AIndex, ACount: Integer);
begin
  Arr.Sort(Comparer, AIndex, ACount);
end;

procedure TVectorClass<T>.Sort;
begin
  Arr.Sort;
end;

procedure TVectorClass<T>.Sort(AIndex, ACount: Integer);
begin
  Arr.Sort(AIndex, ACount);
end;

procedure TVectorClass<T>.Sort(Comparison: TComparison<T>);
begin
  Arr.Sort(Comparison);
end;

function TVectorClass<T>.Sorted(AStartIndex, ACount: integer; AComparer: IComparer<T>): boolean;
begin
  result := Arr.Sorted(AStartIndex, ACount, AComparer);
end;

function TVectorClass<T>.Sorted: boolean;
begin
  result := Arr.Sorted;
end;

function TVectorClass<T>.Sorted(AStartIndex, ACount: integer; AComparison: TComparison<T>): boolean;
begin
  result := Arr.Sorted(AStartIndex, ACount, AComparison);
end;

function TVectorClass<T>.Start: TArrayIterator<T>;
begin
  result := Arr.Start;
end;

function TVectorClass<T>.Finish: TArrayIterator<T>;
begin
  result := Arr.Finish;
end;

function TVectorClass<T>.RStart: TArrayRIterator<T>;
begin
  result := Arr.RStart;
end;

function TVectorClass<T>.RFinish: TArrayRIterator<T>;
begin
  result := Arr.RFinish;
end;

function TVectorClass<T>.ToArray: TArray<T>;
begin
  result := Arr.ToArray;
end;

function TVectorClass<T>.ToString: string;
begin
  result := Arr.ToString;
end;

function TVectorClass<T>.ToText(const ValuesDelimiter: string): string;
begin
  result := Arr.ToText(ValuesDelimiter);
end;

procedure TVectorClass<T>.TrimExcess;
begin
  Arr.TrimExcess;
end;

{ TCVector<T> }

class operator TCVector<T>.Add(a: TCVector<T>; const b: TEnumerable<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TCVector<T>.Add(a: TCVector<T>; const b: TArray<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TCVector<T>.Add(a, b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TCVector<T>.Add(const a: TEnumerable<T>; b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

function TCVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer): Boolean;
begin
  result := RO.BinarySearch(Item, FoundIndex);
end;

function TCVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean;
begin
  result := RO.BinarySearch(Item, FoundIndex, Comparer);
end;

function TCVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex, ACount: Integer): Boolean;
begin
  result := RO.BinarySearch(Item, FoundIndex, Comparer, AIndex, ACount);
end;

function TCVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex, ACount: Integer): Boolean;
begin
  result := RO.BinarySearch(Item, FoundIndex, Comparison, AIndex, ACount);
end;

function TCVector<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean;
begin
  result := RO.BinarySearch(Item, FoundIndex, Comparison);
end;

class operator TCVector<T>.Add(const a: TArray<T>; b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TCVector<T>.Add(const a: T; b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

class operator TCVector<T>.Add(a: TCVector<T>; const b: T): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Add(B);
end;

procedure TCVector<T>.Add(const Values: TArray<T>);
begin
  RW.Add(Values);
end;

function TCVector<T>.Add(const Value: T): integer;
begin
  result := RW.Add(Value);
end;

function TCVector<T>.Add: integer;
begin
  result := RW.Add;
end;

procedure TCVector<T>.Add(Values: TCVector<T>);
var
  Src: TVectorClass<T>;
begin
  if Values.FVectorInt = nil then
    Exit;
  Src := Values.RO;
  RW.Add(Src.Arr.Items, 0, Src.Count);
end;

procedure TCVector<T>.Add(const Values: TEnumerable<T>);
begin
  RW.Add(Values);
end;

procedure TCVector<T>.Add(const Values: TArray<T>; AStartIndex, ACount: integer);
begin
  RW.Add(Values, AStartIndex, ACount);
end;

procedure TCVector<T>.Clear;
begin
  RW.Clear;
end;

function TCVector<T>.Compare(const B: TArray<T>; AComparer: IComparer<T>): integer;
begin
  result := RO.Compare(B, AComparer);
end;

function TCVector<T>.Compare(const B: TArray<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
begin
  result := RO.Compare(B, AStartIndex, BStartIndex, ACount, AComparer);
end;

function TCVector<T>.Compare(B: TEnumerable<T>; AStartIndex, BStartIndex, ACount: integer; AComparer: IComparer<T>): integer;
begin
  result := RO.Compare(B, AStartIndex, BStartIndex, ACount, AComparer);
end;

function TCVector<T>.Compare(B: TEnumerable<T>; AComparer: IComparer<T>): integer;
begin
  result := RO.Compare(B, AComparer);
end;

function TCVector<T>.Contains(const Value: T): boolean;
begin
  result := RO.Contains(Value);
end;

function TCVector<T>.Contains(const Value: T; Comparer: IComparer<T>): boolean;
begin
  result := RO.Contains(Value, Comparer);
end;

function TCVector<T>.Contains(const Values: TArray<T>): boolean;
begin
  result := RO.Contains(Values);
end;

function TCVector<T>.Contains(const Values: TArray<T>; Comparer: IComparer<T>): boolean;
begin
  result := RO.Contains(Values, Comparer);
end;

function TCVector<T>.Contains(const Values: TEnumerable<T>): boolean;
begin
  result := RO.Contains(Values);
end;

function TCVector<T>.Contains(const Values: TEnumerable<T>; Comparer: IComparer<T>): boolean;
begin
  result := RO.Contains(Values, Comparer);
end;

function TCVector<T>.Copy: TCVector<T>;
begin
  result.Init;
  result.Add(Self);
end;

class function TCVector<T>.Create(ACapacity: integer): TCVector<T>;
begin
  result.Init(ACapacity);
end;

class function TCVector<T>.Create(const Values: TArray<T>): TCVector<T>;
begin
  result.Init(Values);
end;

class function TCVector<T>.Create(const Values: TEnumerable<T>; ACapacity: integer): TCVector<T>;
begin
  result.Init(Values, ACapacity);
end;

class function TCVector<T>.Create: TCVector<T>;
begin
  result.Init;
end;

procedure TCVector<T>.CreateVector(ACapacity: integer);
begin
  FVectorInt := TInterfacedObject<TVectorClass<T>>.Create( TVectorClass<T>.Create );
  FVectorInt.Data.Capacity := ACapacity;
end;

procedure TCVector<T>.Delete(AStartIndex, ACount: integer);
begin
  RW.Delete(AStartIndex, ACount);
end;

procedure TCVector<T>.Delete(ItemIndex: integer);
begin
  RW.Delete(ItemIndex);
end;

procedure TCVector<T>.Delete(const AIndices: TArray<integer>);
begin
  RW.Delete(AIndices);
end;

procedure TCVector<T>.Delete(AIndices: TSet<integer>);
begin
  RW.Delete(AIndices);
end;

procedure TCVector<T>.DeleteLast;
begin
  RW.DeleteLast;
end;

class operator TCVector<T>.Equal(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TCVector<T>.Equal(a, b: TCVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) = 0;
end;

class operator TCVector<T>.Equal(const b: TEnumerable<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TCVector<T>.Equal(a: TCVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

class operator TCVector<T>.Equal(const b: TArray<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) = 0;
end;

procedure TCVector<T>.Exchange(Index1, Index2: integer);
begin
  RW.Exchange(Index1, Index2);
end;

class operator TCVector<T>.Explicit(const a: TEnumerable<T>): TCVector<T>;
begin
  result.Init(a);
end;

class operator TCVector<T>.Explicit(const a: TArray<T>): TCVector<T>;
begin
  result.Init(a);
end;

class operator TCVector<T>.Explicit(const a: T): TCVector<T>;
begin
  result.Init;
  result.Add(a);
end;

class operator TCVector<T>.Explicit(a: TCVector<T>): TEnumerable<T>;
begin
  result := a.Collection;
end;

function TCVector<T>.Extract(ItemIndex: integer): T;
begin
  result := RW.Extract(ItemIndex);
end;

function TCVector<T>.ExtractAll: TArray<T>;
begin
  result := RW.ExtractAll;
end;

function TCVector<T>.ExtractLast: T;
begin
  result := RW.ExtractLast;
end;

function TCVector<T>.FindFirst(const Value: T; var Index: integer): boolean;
begin
  result := RO.FindFirst(Value, Index);
end;

function TCVector<T>.FindFirst(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean;
begin
  result := RO.FindFirst(Value, Index, AComparer);
end;

function TCVector<T>.FindNext(const Value: T; var Index: integer; AComparer: IComparer<T>): boolean;
begin
  result := RO.FindNext(Value, Index, AComparer);
end;

function TCVector<T>.FindNext(const Value: T; var Index: integer): boolean;
begin
  result := RO.FindNext(Value, Index);
end;

procedure TCVector<T>.FirstPermutation;
begin
  RW.FirstPermutation;
end;

function TCVector<T>.GetCapacity: integer;
begin
  result := RO.Capacity;
end;

function TCVector<T>.GetCollection: TEnumerable<T>;
begin
  result := RO;
end;

function TCVector<T>.GetCount: NativeInt;
begin
  result := RO.Count;
end;

function TCVector<T>.GetEmpty: Boolean;
begin
  result := RO.Empty;
end;

function TCVector<T>.GetEnumerator: TEnumerator<T>;
begin
  result := RO.GetEnumerator;
end;

function TCVector<T>.GetFirst: T;
begin
  result := RO.First;
end;

function TCVector<T>.GetItem(ItemIndex: integer): T;
begin
  result := RO[ItemIndex];
end;

function TCVector<T>.GetItemsArray: TArray<T>;
begin
  result := RW.Arr.Items;
end;

function TCVector<T>.GetLast: T;
begin
  result := RO.Last;
end;

function TCVector<T>.GetOwnsValues: boolean;
begin
  result := RW.OwnsValues;
end;

function TCVector<T>.GetRO: TVectorClass<T>;
begin
  if FVectorInt=nil then
    CreateVector;
  result := FVectorInt.Data;
end;

function TCVector<T>.GetRW: TVectorClass<T>;
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
      CreateVector(SrcVectorInt.Data.Count);
      FVectorInt.Data.Add(SrcVectorInt.Data);
    end;
  result := FVectorInt.Data;
end;

function TCVector<T>.GetSlice: TSlice<T>;
begin
  result := RO.GetSlice;
end;

function TCVector<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
begin
  result := RO.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl);
end;

function TCVector<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
begin
  result := RO.GetSlice(AFilter);
end;

function TCVector<T>.GetTotalSizeBytes: int64;
begin
  result := RO.TotalSizeBytes;
end;

class operator TCVector<T>.GreaterThan(a: TCVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TCVector<T>.GreaterThan(a, b: TCVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) > 0;
end;

class operator TCVector<T>.GreaterThan(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TCVector<T>.GreaterThan(const b: TArray<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TCVector<T>.GreaterThan(const b: TEnumerable<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TCVector<T>.GreaterThanOrEqual(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TCVector<T>.GreaterThanOrEqual(const b: TArray<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TCVector<T>.GreaterThanOrEqual(a: TCVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TCVector<T>.GreaterThanOrEqual(a, b: TCVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) >= 0;
end;

class operator TCVector<T>.GreaterThanOrEqual(const b: TEnumerable<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TCVector<T>.Implicit(const a: T): TCVector<T>;
begin
  result.Init;
  result.Add(A);
end;

class operator TCVector<T>.Implicit(const a: TEnumerable<T>): TCVector<T>;
begin
  result.Init(a);
end;

class operator TCVector<T>.Implicit(const a: TArray<T>): TCVector<T>;
begin
  result.Init(a);
end;

class operator TCVector<T>.Implicit(a: TCVector<T>): TEnumerable<T>;
begin
  result := a.Collection;
end;

class operator TCVector<T>.In(a, b: TCVector<T>): Boolean;
begin
  result := B.Contains(A.Collection);
end;

class operator TCVector<T>.In(const a: TArray<T>; b: TCVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TCVector<T>.In(const a: TEnumerable<T>; b: TCVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

class operator TCVector<T>.In(const a: T; b: TCVector<T>): Boolean;
begin
  result := B.Contains(A);
end;

function TCVector<T>.IndexOf(const Value: T): integer;
begin
  result := RO.IndexOf(Value);
end;

function TCVector<T>.IndexOf(const Value: T; AComparer: IComparer<T>): integer;
begin
  result := RO.IndexOf(Value, AComparer);
end;

procedure TCVector<T>.Init(ACapacity: integer);
begin
  Clear;
  Self := Default(TCVector<T>);
  if (ACapacity > 0) then
    CreateVector(ACapacity);
end;

procedure TCVector<T>.Init(const Values: TArray<T>);
begin
  Clear;
  Self := Default(TCVector<T>);
  Add(Values);
end;

procedure TCVector<T>.Init(const Values: TEnumerable<T>; ACapacity: integer);
begin
  Clear;
  Self := Default(TCVector<T>);
  CreateVector(ACapacity);
  Add(Values);
end;

procedure TCVector<T>.Init;
begin
  Clear;
  Self := Default(TCVector<T>);
end;

function TCVector<T>.Insert(Index: integer; const Value: T): integer;
begin
  result := RW.Insert(Index, Value);
end;

class operator TCVector<T>.LessThan(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TCVector<T>.LessThan(const b: TEnumerable<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TCVector<T>.LessThan(const b: TArray<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) > 0;
end;

class operator TCVector<T>.LessThan(a: TCVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) < 0;
end;

class operator TCVector<T>.LessThan(a, b: TCVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) < 0;
end;

class operator TCVector<T>.LessThanOrEqual(const b: TEnumerable<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

class operator TCVector<T>.LessThanOrEqual(a, b: TCVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) <= 0;
end;

class operator TCVector<T>.LessThanOrEqual(a: TCVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TCVector<T>.LessThanOrEqual(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) <= 0;
end;

class operator TCVector<T>.LessThanOrEqual(const b: TArray<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) >= 0;
end;

procedure TCVector<T>.Move(SrcIndex, DstIndex: integer);
begin
  RW.Move(SrcIndex, DstIndex);
end;

function TCVector<T>.NextPermutation: boolean;
begin
  result := RW.NextPermutation;
end;

class operator TCVector<T>.NotEqual(const b: TArray<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TCVector<T>.NotEqual(const b: TEnumerable<T>; a: TCVector<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TCVector<T>.NotEqual(a, b: TCVector<T>): Boolean;
begin
  result := A.Compare(B.Collection) <> 0;
end;

class operator TCVector<T>.NotEqual(a: TCVector<T>; const b: TArray<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

class operator TCVector<T>.NotEqual(a: TCVector<T>; const b: TEnumerable<T>): Boolean;
begin
  result := A.Compare(B) <> 0;
end;

function TCVector<T>.PrevPermutation: boolean;
begin
  result := RW.PrevPermutation;
end;

procedure TCVector<T>.Remove(AFilter: TFuncFilterValueIndex<T>);
begin
  RW.Remove(AFilter);
end;

procedure TCVector<T>.Remove(const V: TEnumerable<T>; AComparer: IComparer<T>);
begin
  RW.Remove(V, AComparer);
end;

procedure TCVector<T>.Remove(const V: TArray<T>; AComparer: IComparer<T>);
begin
  RW.Remove(V, AComparer);
end;

procedure TCVector<T>.Remove(const V: T; AComparer: IComparer<T>);
begin
  RW.Remove(V, AComparer);
end;

procedure TCVector<T>.Reverse;
begin
  RW.Reverse;
end;

procedure TCVector<T>.Reverse(AStartIndex, ACount: integer);
begin
  RW.Reverse(AStartIndex,ACount);
end;

procedure TCVector<T>.RotateLeft(Index1, Index2, Shift: integer);
begin
  RW.RotateLeft(Index1, Index2, Shift);
end;

procedure TCVector<T>.RotateRight(Index1, Index2, Shift: integer);
begin
  RW.RotateRight(Index1, Index2, Shift);
end;

procedure TCVector<T>.SaveToFile(const FileName: string; Encoding: TEncoding; MemStream: boolean);
begin
  RO.SaveToFile(FileName, Encoding, MemStream);
end;

procedure TCVector<T>.SaveToStream(Dst: TStream; Encoding: TEncoding);
begin
  RO.SaveToStream(Dst, Encoding);
end;

procedure TCVector<T>.SetCapacity(const Value: integer);
begin
  RW.Capacity := Value;
end;

procedure TCVector<T>.SetCount(const Value: NativeInt);
begin
  RW.Count := Value;
end;

procedure TCVector<T>.SetFirst(const Value: T);
begin
  RW.First := Value;
end;

procedure TCVector<T>.SetItem(ItemIndex: integer; const Value: T);
begin
  RW[ItemIndex] := Value;
end;

procedure TCVector<T>.SetLast(const Value: T);
begin
  RW.Last := Value;
end;

procedure TCVector<T>.SetOwnsValues(const Value: boolean);
begin
  RW.OwnsValues := Value;
end;

procedure TCVector<T>.Shuffle(AStartIndex, ACount: integer);
begin
  RW.Shuffle(AStartIndex,ACount);
end;

procedure TCVector<T>.Sort(Comparer: IComparer<T>);
begin
  RW.Sort(Comparer);
end;

procedure TCVector<T>.Sort;
begin
  RW.Sort;
end;

procedure TCVector<T>.Sort(AIndex, ACount: Integer);
begin
  RW.Sort(AIndex, ACount);
end;

procedure TCVector<T>.Sort(Comparer: IComparer<T>; AIndex, ACount: Integer);
begin
  RW.Sort(Comparer, AIndex, ACount);
end;

procedure TCVector<T>.Sort(Comparison: TComparison<T>);
begin
  RW.Sort(Comparison);
end;

procedure TCVector<T>.Sort(Comparison: TComparison<T>; AIndex, ACount: Integer);
begin
  RW.Sort(Comparison, AIndex, ACount);
end;

function TCVector<T>.Sorted: boolean;
begin
  result := RO.Sorted;
end;

function TCVector<T>.Sorted(AStartIndex, ACount: integer; AComparer: IComparer<T>): boolean;
begin
  result := RO.Sorted(AStartIndex, ACount, AComparer);
end;

function TCVector<T>.Sorted(AStartIndex, ACount: integer; AComparison: TComparison<T>): boolean;
begin
  result := RO.Sorted(AStartIndex, ACount, AComparison);
end;

function TCVector<T>.Start: TArrayIterator<T>;
begin
  result := RW.Start;
end;

function TCVector<T>.Finish: TArrayIterator<T>;
begin
  result := RW.Finish;
end;

function TCVector<T>.RStart: TArrayRIterator<T>;
begin
  result := RW.RStart;
end;

function TCVector<T>.RFinish: TArrayRIterator<T>;
begin
  result := RW.RFinish;
end;

procedure TCVector<T>.Shuffle;
begin
  RW.Shuffle;
end;

class operator TCVector<T>.Subtract(const a: TArray<T>; b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B.Collection);
end;

class operator TCVector<T>.Subtract(const a: TEnumerable<T>; b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B.Collection);
end;

class operator TCVector<T>.Subtract(a: TCVector<T>; const b: TArray<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TCVector<T>.Subtract(a, b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B.Collection);
end;

class operator TCVector<T>.Subtract(a: TCVector<T>; const b: T): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TCVector<T>.Subtract(a: TCVector<T>; const b: TEnumerable<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B);
end;

class operator TCVector<T>.Subtract(const a: T; b: TCVector<T>): TCVector<T>;
begin
  result.Init;
  result.Add(A);
  result.Remove(B.Collection);
end;

function TCVector<T>.ToArray: TArray<T>;
begin
  result := RW.ToArray;
end;

function TCVector<T>.ToString: string;
begin
  result := RO.ToString;
end;

function TCVector<T>.ToText(const ValuesDelimiter: string = #13#10): string;
begin
  result := RO.ToText(ValuesDelimiter);
end;

procedure TCVector<T>.TrimExcess;
begin
  RW.TrimExcess;
end;

end.
