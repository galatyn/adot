unit adot.Collections.Vectors;

interface
{
                                                                       managed  copy-on-write
  TArr<T>          generic record,  wrapper for TArray<T>                +            -
  TArr2D<T>        generic record, wrapper for 2-dimensional array       +            -
  TVector<T>       generic record, supports copy-on-write                +            +
  TVectorClass<T>  generic class, base for TVector<T>                    -            -
}

uses
  adot.Types,
  adot.Collections.Types,
  adot.Collections.Slices,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  System.Classes,
  System.Math;

type
  { Most efficient vector-like container, but doesn't support copy-on-write.
    Wrapper for TArray<T> (array with Add/Delete functionality). Example:
       function GetFiltered(const Src: TArray<integer>; Filter: TFunc<integer, boolean>): TArray<integer>;
       var
         V: TArr<integer>;
         I: integer;
       begin
         V.Init;
         for I := 0 to High(Src) do
           if Filter(Src[I]) then
             V.Add(Src[I]); // more efficient than resizing TArray<> every time
         Result := V.ToArray; // there is no copying of data here, we get array pointer only
       end;
   }
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
        procedure Init(const Items: TArray<T>; ACount: integer);

        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;

    { Init is preferred over Create, it is obviously distinguished from classes.
      Class-like Create can be useful when new instance is param of some routine }
    procedure Init; overload;
    procedure Init(ACapacity: integer); overload;
    procedure Init(AItems: TArray<T>); overload;

    class function Create: TArr<T>; overload; static;
    class function Create(ACapacity: integer): TArr<T>; overload; static;
    class function Create(AItems: TArray<T>): TArr<T>; overload; static;

    { Creates empty slice on underlying array }
    function GetSlice: TSlice<T>; overload;
    { Creates slice from range of underlying array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates slice on the array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

    function Add: integer; overload;
    function Add(const Value: T): integer; overload;
    procedure Add(const Value: TArray<T>); overload;
    procedure Add(const Value: TArray<T>; AStartIndex,ACount: integer); overload;
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

    { Trims and returns Items }
    function ToString: string;
    function ToText(const ValuesDelimiter: string = #13#10): string;
    function ToArray: TArray<T>;

    function GetEnumerator: TEnumerator; reintroduce;
    procedure Clear;
    procedure TrimExcess;

    procedure Sort; overload;
    procedure Sort(Comparer: IComparer<T>); overload;
    procedure Sort(Comparer: IComparer<T>; AIndex, ACount: Integer); overload;
    procedure Sort(Comparison: TComparison<T>); overload;
    procedure Sort(Comparison: TComparison<T>; AIndex, ACount: Integer); overload;

    function BinarySearch(const Item: T; out FoundIndex: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparer: IComparer<T>; AIndex,ACount: Integer): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean; overload;
    function BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex,ACount: Integer): Boolean; overload;

    class operator Explicit(const a : TArr<T>) : TArray<T>;
    class operator Explicit(const a : TArray<T>) : TArr<T>;
    class operator Implicit(const a : TArr<T>) : TArray<T>;
    class operator Implicit(const a : TArray<T>) : TArr<T>;

    property First: T read GetFirst write SetFirst;
    property Last: T read GetLast write SetLast;
    property Count: integer read FCount write SetCount;
    property Length: integer read FCount write SetCount;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Elements[ItemIndex: integer]: T read GetItem write SetItem; default;
    property Empty: boolean read GetEmpty;
    property TotalSizeBytes: int64 read GetTotalSizeBytes;
  end;

  TVectorClass<T> = class(TEnumerableExt<T>)
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
    procedure SetComparison(AComparison: TComparison<T>);
    procedure SetComparer(AComparer: IComparer<T>);

  protected
    FItems: TArray<T>;
    FCount: integer;
    FComparer: IComparer<T>;
    FOwnsValues: boolean;

    function DoGetEnumerator: TEnumerator<T>; override;

  public
    constructor Create; overload;
    constructor Create(AComparer: IComparer<T>); overload;
    constructor Create(AComparison: TComparison<T>); overload;

    constructor Create(const AItems: TArray<T>); overload;
    constructor Create(const AItems: TArray<T>; AComparer: IComparer<T>); overload;
    constructor Create(const AItems: TArray<T>; AComparison: TComparison<T>); overload;

    destructor Destroy; override;

    { Creates empty slice on underlying array }
    function GetSlice: TSlice<T>; overload;
    { Creates slice from range of underlying array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates slice on the array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

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
    function Sorted(AStartIndex,ACount: integer; AComparer: IComparer<T>): boolean; overload;
    function Sorted(AStartIndex,ACount: integer; AComparison: TComparison<T>): boolean; overload;

    procedure Delete(ItemIndex: integer); overload;
    procedure Delete(AStartIndex,ACount: integer); overload;
    procedure Delete(const AIndices: TArray<integer>); overload;
    procedure DeleteLast;

    procedure Remove(const V: T; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(const V: TEnumerable<T>; AComparer: IComparer<T> = nil); overload;
    procedure Remove(AFilter: TFuncFilterValueIndex<T>); overload;

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
    procedure Sort(AComparer: TFuncCompareValues<T>); overload;
    procedure Sort(AIndex, ACount: Integer; AComparer: TFuncCompareValues<T>); overload;

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
    property Comparer: IComparer<T> read FComparer write SetComparer;
    property Comparison: TComparison<T> write SetComparison;
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

    procedure Init(AComparer: IComparer<T> = nil); overload;
    procedure Init(AComparer: TComparison<T>); overload;
    procedure Init(ACapacity: integer; AComparer: IComparer<T> = nil); overload;
    procedure Init(const Values: TArray<T>; AComparer: IComparer<T> = nil); overload;
    procedure Init(const Values: TEnumerable<T>; ACapacity: integer = 0; AComparer: IComparer<T> = nil); overload;

    { 1. Delphi doesn't allow parameterless constructor
      2. Delphi creates here strange / not optimal code for Linux, but there is no problems with
         static functions. Followin example fails with constructor, but works as expected with function:
           A := TVector<integer>.Create(10);
           A.Add([2,1,3]);
           Check(A.Capacity=10); }
    class function Create(AComparer: IComparer<T> = nil): TVector<T>; overload; static;
    class function Create(AComparer: TComparison<T>): TVector<T>; overload; static;
    class function Create(ACapacity: integer; AComparer: IComparer<T> = nil): TVector<T>; overload; static;
    class function Create(const Values: TArray<T>; AComparer: IComparer<T> = nil): TVector<T>; overload; static;
    class function Create(const Values: TEnumerable<T>; ACapacity: integer = 0; AComparer: IComparer<T> = nil): TVector<T>; overload; static;

    { Creates empty slice on underlying array }
    function GetSlice: TSlice<T>; overload;
    { Creates slice from range of underlying array }
    function GetSlice(AStartSliceIndexIncl,AEndSliceIndexExcl: integer): TSlice<T>; overload;
    { Creates slice on the array for items accepted by filter }
    function GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>; overload;

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
    procedure Sort(AComparer: TFuncCompareValues<T>); overload;
    procedure Sort(AIndex, ACount: Integer; AComparer: TFuncCompareValues<T>); overload;

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
      wither AsArray (for readonly access) or ToArray }
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

  { Dynamic 2-dimensional array }
  TArr2D<T> = record
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
    procedure Init(Width, Height: integer);
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
  adot.Strings, adot.Collections.Sets;

{ TArr<T>.TEnumerator }

procedure TArr<T>.TEnumerator.Init(const Items: TArray<T>; ACount: integer);
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

class function TArr<T>.Create: TArr<T>;
begin
  result.Init;
end;

class function TArr<T>.Create(ACapacity: integer): TArr<T>;
begin
  result.Init(ACapacity);
end;

class function TArr<T>.Create(AItems: TArray<T>): TArr<T>;
begin
  result.Init(AItems);
end;

procedure TArr<T>.Init;
begin
  Self := Default(TArr<T>);
end;

procedure TArr<T>.Init(ACapacity: integer);
begin
  Self := Default(TArr<T>);
  Capacity := ACapacity;
end;

procedure TArr<T>.Init(AItems: TArray<T>);
begin
  Self := Default(TArr<T>);
  Items := AItems;
  FCount := High(AItems)-Low(AItems)+1;
end;

function TArr<T>.Add: integer;
begin
  if Count >= Capacity then
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
begin
  Add(Value, 0, System.Length(Value));
end;

procedure TArr<T>.Add(const Value: TArray<T>; AStartIndex,ACount: integer);
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

class operator TArr<T>.Implicit(const a: TArr<T>): TArray<T>;
begin
  result := a.ToArray;
end;

class operator TArr<T>.Implicit(const a: TArray<T>): TArr<T>;
begin
  result.Init(a);
end;

class operator TArr<T>.Explicit(const a: TArr<T>): TArray<T>;
begin
  result := a.ToArray;
end;

class operator TArr<T>.Explicit(const a: TArray<T>): TArr<T>;
begin
  result.Init(a);
end;

function TArr<T>.ToString: string;
begin
  result := ToText(' ');
end;

function TArr<T>.ToText(const ValuesDelimiter: string = #13#10): string;
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
  result.Init(Items, Count);
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

function TArr<T>.GetSlice: TSlice<T>;
begin
  result.Init(Items, FCount);
end;

function TArr<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
begin
  result.Init(Items, FCount);
  result.Add(AStartSliceIndexIncl, AEndSliceIndexExcl);
end;

function TArr<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
var
  I: Integer;
begin
  result.Init(Items, FCount);
  for I := 0 to FCount-1 do
    if AFilter(Items[I], I) then
      result.Add(I);
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

procedure TArr<T>.Sort(Comparison: TComparison<T>);
begin
  Sort(Comparison, 0, Count);
end;

procedure TArr<T>.Sort(Comparison: TComparison<T>; AIndex, ACount: Integer);
var
  C: IComparer<T>;
begin
  C := TDelegatedComparer<T>.Create(Comparison);
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

function TArr<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>; AIndex, ACount: Integer): Boolean;
begin
  result := BinarySearch(Item, FoundIndex, TDelegatedComparer<T>.Create(Comparison), AIndex, ACount);
end;

function TArr<T>.BinarySearch(const Item: T; out FoundIndex: Integer; Comparison: TComparison<T>): Boolean;
begin
  result := BinarySearch(Item, FoundIndex, TDelegatedComparer<T>.Create(Comparison), 0, Count);
end;

procedure TArr<T>.Add(const Value: TArr<T>);
var
  I: Integer;
begin
  for I := 0 to Value.Count-1 do
    Items[Add] := Value[I];
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

constructor TVectorClass<T>.Create;
begin
  inherited Create;
  FComparer := TComparerUtils.DefaultComparer<T>;
end;

constructor TVectorClass<T>.Create(AComparer: IComparer<T>);
begin
  inherited Create;
  Comparer := AComparer;
end;

constructor TVectorClass<T>.Create(AComparison: TComparison<T>);
begin
  inherited Create;
  Comparison := AComparison;
end;

constructor TVectorClass<T>.Create(const AItems: TArray<T>);
begin
  Create;
  Add(AItems);
end;

constructor TVectorClass<T>.Create(const AItems: TArray<T>; AComparer: IComparer<T>);
begin
  Create(AComparer);
  Add(AItems);
end;

constructor TVectorClass<T>.Create(const AItems: TArray<T>; AComparison: TComparison<T>);
begin
  Create(AComparison);
  Add(AItems);
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
  S,D,I: Integer;
  IndicesSet: TSet<integer>;
begin
  if (Length(AIndices) = 0) or (Count = 0) then
    Exit;
  IndicesSet.Init(AIndices);
  S := 0;
  D := 0;
  for I := 0 to Count-1 do
    if I in IndicesSet then
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

function TVectorClass<T>.GetSlice: TSlice<T>;
begin
  result.Init(FItems, FCount);
end;

function TVectorClass<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
begin
  result.Init(FItems, FCount);
  result.Add(AStartSliceIndexIncl, AEndSliceIndexExcl);
end;

function TVectorClass<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
var
  I: Integer;
begin
  result.Init(FItems, FCount);
  for I := 0 to FCount-1 do
    if AFilter(FItems[I], I) then
      result.Add(I);
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

procedure TVectorClass<T>.Remove(AFilter: TFuncFilterValueIndex<T>);
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
  S: TArr<T>;
  I,J,D: integer;
begin
  if (Length(V)=0) or (Count=0) then
    Exit;
  FindComparer(AComparer);
  S.Init(TArrayUtils.Copy<T>(V));
  S.Sort(AComparer);
  D := 0;
  for I := 0 to FCount-1 do
    if S.BinarySearch(FItems[I],J,AComparer) then
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

procedure TVectorClass<T>.SetComparer(AComparer: IComparer<T>);
begin
  if AComparer=nil
    then FComparer := TComparerUtils.DefaultComparer<T>
    else FComparer := AComparer;
end;

procedure TVectorClass<T>.SetComparison(AComparison: TComparison<T>);
begin
  if not Assigned(AComparison)
    then FComparer := TComparerUtils.DefaultComparer<T>
    else FComparer := TDelegatedComparer<T>.Create(AComparison);
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

procedure TVectorClass<T>.Sort(AComparer: TFuncCompareValues<T>);
begin
  Sort(0, Count, AComparer);
end;

procedure TVectorClass<T>.Sort(AIndex, ACount: Integer; AComparer: TFuncCompareValues<T>);
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

function TVectorClass<T>.Sorted(AStartIndex, ACount: integer; AComparison: TComparison<T>): boolean;
begin
  if Assigned(AComparison)
    then result := Sorted(AStartIndex, ACount, TDelegatedComparer<T>.Create(AComparison))
    else result := Sorted(AStartIndex, ACount, FComparer);
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
  Buf: TStringBuilder;
  I: Integer;
begin
  Buf := TStringBuilder.Create;
  if Count > 0 then
    Buf.Append(TRttiUtils.ValueAsString<T>(FItems[0]));
  for I := 1 to Count-1 do
    Buf.Append(ValueSeparator + TRttiUtils.ValueAsString<T>(FItems[I]));
  if (Count > 0) and SepAfterLastValue then
    Buf.Append(ValueSeparator);
  Result := Buf.ToString;
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

class function TVector<T>.Create(AComparer: IComparer<T> = nil): TVector<T>;
begin
  Result.Init(AComparer);
end;

class function TVector<T>.Create(ACapacity: integer; AComparer: IComparer<T>): TVector<T>;
begin
  Result.Init(ACapacity, AComparer);
end;

class function TVector<T>.Create(AComparer: TComparison<T>): TVector<T>;
begin
  Result.Init(AComparer);
end;

class function TVector<T>.Create(const Values: TEnumerable<T>; ACapacity: integer; AComparer: IComparer<T>): TVector<T>;
begin
  Result.Init(Values, ACapacity, AComparer);
end;

class function TVector<T>.Create(const Values: TArray<T>; AComparer: IComparer<T>): TVector<T>;
begin
  Result.Init(Values, AComparer);
end;

procedure TVector<T>.Init(AComparer: IComparer<T>);
begin
  Self := Default(TVector<T>);
  if AComparer<>nil then
    CreateVector(0, AComparer);
end;

procedure TVector<T>.Init(ACapacity: integer; AComparer: IComparer<T>);
begin
  Self := Default(TVector<T>);
  if (ACapacity > 0) or (AComparer<>nil) then
    CreateVector(ACapacity, AComparer);
end;

procedure TVector<T>.Init(AComparer: TComparison<T>);
begin
  Self := Default(TVector<T>);
  CreateVector(0, TDelegatedComparer<T>.Create(AComparer));
end;

procedure TVector<T>.Init(const Values: TEnumerable<T>; ACapacity: integer; AComparer: IComparer<T>);
begin
  Self := Default(TVector<T>);
  CreateVector(ACapacity, AComparer);
  Add(Values);
end;

procedure TVector<T>.Init(const Values: TArray<T>; AComparer: IComparer<T>);
begin
  Self := Default(TVector<T>);
  CreateVector(System.Length(Values), AComparer);
  Add(Values);
end;

procedure TVector<T>.CreateVector(ACapacity: integer; AComparer: IComparer<T>);
begin
  FVectorInt := TInterfacedObject<TVectorClass<T>>.Create( TVectorClass<T>.Create(AComparer) );
  FVectorInt.Data.Capacity := ACapacity;
end;

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
  result.Init(Comparer);
  result.Add(Self);
end;

procedure TVector<T>.Delete(const AIndices: TArray<integer>);
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

function TVector<T>.GetSlice: TSlice<T>;
begin
  result := RO.GetSlice;
end;

function TVector<T>.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl: integer): TSlice<T>;
begin
  result := RO.GetSlice(AStartSliceIndexIncl, AEndSliceIndexExcl);
end;

function TVector<T>.GetSlice(AFilter: TFuncFilterValueIndex<T>): TSlice<T>;
begin
  result := RO.GetSlice(AFilter);
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

procedure TVector<T>.Sort(AIndex, ACount: Integer; AComparer: TFuncCompareValues<T>);
begin
  RW.Sort(AIndex, ACount, AComparer);
end;

procedure TVector<T>.Sort(AComparer: TFuncCompareValues<T>);
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

procedure TVector<T>.Remove(AFilter: TFuncFilterValueIndex<T>);
begin
  RW.Remove(AFilter);
end;

{ TArr2D<T>.TCollectionEnumerator }

constructor TArr2D<T>.TEnumerator.Create(const Rows: TArr<TArr<T>>);
begin
  inherited Create;
  Self.Rows := Rows;
  X := -1;
  Y := 0;
end;

function TArr2D<T>.TEnumerator.DoMoveNext: Boolean;
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

function TArr2D<T>.TEnumerator.DoGetCurrent: T;
begin
  result := Rows[Y][X];
end;

{ TArr2D<T>.TCollection }

constructor TArr2D<T>.TCollection.Create(const Rows: TArr<TArr<T>>);
begin
  inherited Create;
  Self.Rows := Rows;
end;

function TArr2D<T>.TCollection.DoGetEnumerator: TEnumerator<T>;
begin
  result := TEnumerator.Create(Rows);
end;

{ TArr2D<T> }

procedure TArr2D<T>.Init(Width, Height: integer);
var
  Y: integer;
  V: TArr<T>;
begin
  Self := Default(TArr2D<T>);
  Rows := TArr<TArr<T>>.Create(Height);
  Rows.Count := Height;
  for Y := 0 to Height-1 do
  begin
    Rows[Y] := TArr<T>.Create(Width);
    Rows.Items[Y].Count := Width;
  end;
end;

procedure TArr2D<T>.Clear;
begin
  Self := Default(TArr2D<T>);
end;

function TArr2D<T>.Collection: IInterfacedObject<TEnumerable<T>>;
begin
  result := TInterfacedObject<TEnumerable<T>>.Create(TCollection.Create(Rows));
end;

function TArr2D<T>.Add(y: integer): integer;
begin
  result := Rows.Items[y].Add;
end;

function TArr2D<T>.Add(y: integer; const Value: T): integer;
begin
  result := Rows.Items[y].Add(Value);
end;

function TArr2D<T>.Add(y: integer; const Values: TEnumerable<T>): integer;
var
  Value: T;
begin
  result := -1;
  for Value in Values do
    result := Self.Rows.Items[y].Add(Value);
end;

function TArr2D<T>.Add(y: integer; const Values: TArray<T>): integer;
var
  I: Integer;
begin
  result := -1;
  for I := Low(Values) to High(Values) do
    result := Self.Rows.Items[y].Add(Values[I]);
end;

function TArr2D<T>.AddRow: integer;
begin
  result := Rows.Add;
end;

function TArr2D<T>.GetValue(x, y: integer): T;
begin
  result := Rows.Items[y].Items[x];
end;

procedure TArr2D<T>.SetValue(x, y: integer; const Value: T);
begin
  Rows.Items[y].Items[x] := Value;
end;

function TArr2D<T>.GetRowCount: integer;
begin
  result := Rows.Count;
end;

procedure TArr2D<T>.SetRowCount(const Value: integer);
begin
  Rows.Count := Value;
end;

function TArr2D<T>.GetWidth(y: integer): integer;
begin
  result := Rows.Items[y].Count;
end;

procedure TArr2D<T>.SetWidth(y: integer; const Value: integer);
begin
  Rows.Items[y].Count := Value;
end;

end.


