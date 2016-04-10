unit adot.Test;

interface

uses
  adot.Types, System.Math;

implementation

uses
  adot.Collections,
  adot.Strings,
  adot.Tools,
  adot.Tools.Rtti,
  adot.Variants,
  Vcl.Forms,
  Vcl.StdCtrls,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Types,
  System.Variants,
  System.Classes;

type
  TOwnsValuesTestClass = class
    P: PInteger;

    constructor Create(AInt: PInteger);
    destructor Destroy; override;
  end;

  TTest_adot_Collections = class
  private
    class procedure Test_Heap_Search; static;
    class procedure Test_Heap_Search2; static;
    type
      TTest_TSet = class
      private
        class procedure LogicalOperators; static;
        class procedure Add; static;
        class procedure General; static;
        class procedure OwnsValues; static;

        class procedure Run; static;
      end;

      TTest_TMap= class
      private
        class procedure General; static;
        class procedure OwnsValues; static;

        class procedure Run; static;
      end;

    class procedure Test_Multimap; static;
    class procedure Test_Heap; static;
    class procedure Test_AA; static;
    class procedure Test_ring; static;
    class procedure Test_TOrderedMapClass_vs_TSet; static;

    class procedure Run; static;
  end;

  TTest_adot_Strings = class
  private
    type
      CTokCustom = class of TTokCustom;

    { helpers }
    class procedure CheckTokenizer(T: TTokCustom; const Text: string; const R: array of string); static;
    class procedure CheckTokenizerClass(TokenizerClass: CTokCustom; const Text: string; const R: array of string); static;

    { testcases }
    class procedure test_TTokNumbers; static;
    class procedure test_TTokLines; static;
    class procedure test_TTokCharDelimitedLines; static;
    class procedure test_TTokLettersOrDigits; static;
    class procedure test_TTokDigits; static;
    class procedure test_TTokLiterals; static;
    class procedure test_TTokComments; static;

    class procedure Run; static;
  end;

  TTest_TAutoFreeCollection = class
  class var
    InstCount: integer;
  var
    data: string;
  private
    class procedure Create_TAutoFreeCollection(n: integer); static;
    class procedure Create_TAuto(n: integer); static;
    class procedure Create_TAutoFree(n: integer); static;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Test_TAutoFreeCollection; static;
    class procedure Test_TAuto; static;
    class procedure Test_TAutoFree; static;
  end;

  TTest_adot_Tools = class
  private

    { testcases }
    class procedure test_Hex; static;
    class procedure Test_THex; static;
    class procedure Test_TNullable; static;
    class procedure Test_TCompound; static;
    class procedure Test_TAutoFreeCollection; static;
    class procedure Test_TAuto; static;
    class procedure Test_TAutoFree; static;

    class procedure Run; static;
  end;

  TTest_adot_Tools_Rtti = class
  private
    type
      T1 = class
      private
        v: integer;

      public
        constructor Create;
      end;

      T2 = class(T1)
      public
        constructor Create(ACapaCITY: integer);
      end;

      TEn = (en1, en2, en3);

    class procedure Run; static;
  end;

  TTest_adot_Variants = class
  private
    class procedure Run; static;
    class procedure Test_TVar; static;
  end;

{ TTest_adot_Collections }

class procedure TTest_adot_Collections.Run;
begin
  Test_TOrderedMapClass_vs_TSet;
  Test_ring;
  TTest_TSet.Run;
  TTest_TMap.Run;
  Test_Multimap;
  Test_Heap;
  Test_AA;
end;

{ TTest_TSet }

class procedure TTest_adot_Collections.TTest_TSet.Add;
var
  a,b,c: TSet<integer>;
  s,q: TSet<string>;
  i: integer;
  t: string;
begin
  // a := a + b
  a := [1];
  b := [2];
  a := a + b;
  Assert(a.AsString='1 2');
  c := [3];
  a := a + c;
  Assert(a.AsString='1 2 3');
  t := a.AsString;
  for i := 4 to 7 do
    a := a + [i];
  Assert(a.AsString='1 2 3 4 5 6 7');
  for i := 8 to 9 do
  begin
    b := [i];
    a := a + b;
  end;
  Assert(a.AsString='1 2 3 4 5 6 7 8 9');
  a := [1];
  for i := 2 to 3 do
    a := a + i;
  Assert(a.AsString='1 2 3');

  s := ['1'];
  q := ['2'];
  s := s + q;
  Assert(s.AsString='1 2');
  q := ['3'];
  s := s + q;
  Assert(s.AsString='1 2 3');
  t := s.AsString;
  for i := 4 to 7 do
    s := s + [inttostr(i)];
  Assert(s.AsString='1 2 3 4 5 6 7');
  for i := 8 to 9 do
  begin
    q := [inttostr(i)];
    s := s + q;
  end;
  Assert(s.AsString='1 2 3 4 5 6 7 8 9');
  s := ['1'];
  for i := 2 to 3 do
    s := s + inttostr(i);
  Assert(s.AsString='1 2 3');

  // a := b + a
  a := [1];
  b := [2];
  a := b + a;
  Assert(a.AsString='1 2');
  b := [3];
  a := b + a;
  Assert(a.AsString='1 2 3');
  t := a.AsString;
  for i := 4 to 7 do
    a := [i] + a;
  Assert(a.AsString='1 2 3 4 5 6 7');
  for i := 8 to 9 do
  begin
    b := [i];
    a := b + a;
  end;
  Assert(a.AsString='1 2 3 4 5 6 7 8 9');
  a := [1];
  for i := 2 to 3 do
    a := i + a;
  Assert(a.AsString='1 2 3');
end;

class procedure TTest_adot_Collections.TTest_TSet.General;
var
  a,b,c: TSet<integer>;
  s,q: TSet<string>;
  i: integer;
  t: string;
begin
  i := 0;
  for t in s do inc(i);
  for t in q do inc(i);
  assert(i=0);
  a := [1,2,3,4,5];
  b := [3,4];
  assert(b.AsString = '3 4');
  c := 5;
  assert((b.Count=2) and (3 in b) and (4 in b) and not (1 in b) and (b in a) and not (a in b));
  assert((b<a) and (b<=a) and (a>b) and (a>=b) and not (b>a) and not (b>=a));
  Assert(c = TSet<integer>.Create([5]));

  b := [1,2,3];
  c := b + [3,4] - [1];
  Assert(c = TSet<integer>.Create([2,3,4]));
  c := [1,2,3,4,5];
  Assert(TSet<integer>(c) in a);
  Assert(a in c);
  Assert(not (c in b));
  Assert((a=c) and (a<>b));

  c := a+b; { result is not empty here (inside of ADD operator) }
  assert((a in c) and (b in c));
  a := a+b;
  a := a+c;

  s := ['Mandag', 'Tirsdag', 'Fredag'];
  assert('Fredag' in s);
  s := s + 'Lørdag';
  assert('Fredag' in s);
  assert(not ('Søndag' in s));
  assert(TSet<string>.Create(['Fredag','Lørdag']) in s);

  s := ['Mandag', 'Tirsdag', 'Fredag'];
  q := s + ['Lørdag'];
  assert((s<q) and (s in q));

  s := ['Mandag', 'Tirsdag', 'Fredag'];
  q := ['Fredag', 'Lørdag'];
  assert(q.AsString = 'Fredag Lørdag');
  assert(s and q = TSet<string>.Create(['Fredag']));
  assert(s  or q = TSet<string>.Create(['Mandag', 'Tirsdag', 'Fredag', 'Lørdag']));
  assert(s xor q = TSet<string>.Create(['Mandag', 'Tirsdag', 'Lørdag']));
  assert(s < (s or q));
  assert(s > (s and q));
  Assert(not (q in s));
  i := 0;
  for t in s do
  begin
    inc(i);
    assert(t in TSet<string>.Create(['Mandag', 'Tirsdag', 'Fredag']));
  end;
  assert(i=3);
end;

class procedure TTest_adot_Collections.TTest_TSet.LogicalOperators;
var
  a,b,c: TSet<string>;
  s: string;
begin
  a := ['1'];
  b := a;
  a.add('2');
  Assert((a.Count=2) and (b.Count=1));
  a := ['Mandag', 'Tirsdag', 'Fredag'];
  b := ['Fredag', 'Lørdag'];
  c := a and b;                           // ['Fredag']
  c := a or b;                            // ['Mandag', 'Tirsdag', 'Fredag', 'Lørdag']
  c := a + b - ['Mandag', 'Tirsdag'];     // ['Fredag', 'Lørdag']
  Assert(a xor b = TSet<string>.Create(['Mandag', 'Tirsdag', 'Lørdag']));
  Assert((b in a) or ('Fredag' in a));
  c := a + b;
  Assert(c>a);
  Assert(c>=a);
  for s in c do
    a.Add(s);
  c := ['En'];
  c.Add(['To', 'Tre']);
  c.Remove('To'); // ['En', 'Tre']
  Assert( ('En' in c) and ('en' in c) ); // default comparer for "string" type is case insensitive
  c := TSet<string>.Create(['En','To'], 0,TStringComparer.Ordinal);
  Assert( ('En' in c) and NOT ('en' in c) ); // now we used case sensitive comparer
end;

class procedure TTest_adot_Collections.TTest_TSet.OwnsValues;
type
  TC = class of TObject;
var
  N: integer;
  V: TOwnsValuesTestClass;
  A,B: TSet<TOwnsValuesTestClass>;
  C: TSet<TC>;
begin
  Assert(not C.OwnsValues);
  C.Add(TObject);
  Assert(not C.OwnsValues);
  C.Clear;

  N := 0;
  A.Add(TOwnsValuesTestClass.Create(@N));
  assert((N=1) and not A.OwnsValues);
  A.OwnsValues := True;
  Assert(A.OwnsValues);
  A.Clear;
  assert(N=0);

  A.Add(TOwnsValuesTestClass.Create(@N));
  B := A;
  Assert(A.OwnsValues and B.OwnsValues);
  B.OwnsValues := False;
  Assert(A.OwnsValues and not B.OwnsValues);
  V := TOwnsValuesTestClass.Create(@N);
  B.Add(V);
  assert(N=2);
  A.Clear;
  assert(N=1);
  B.Clear;
  assert(N=1);
  FreeAndNil(V);
  assert(N=0);

  A.Add(TOwnsValuesTestClass.Create(@N));
  B := A;
  B.OwnsValues := False;
  V := TOwnsValuesTestClass.Create(@N);
  B.Add(V);
  assert(N=2);
  A.Clear;
  assert(N=1);
  B.Clear;
  assert(N=1);
  FreeAndNil(V);
  assert(N=0);

end;

class procedure TTest_adot_Collections.TTest_TSet.Run;
begin
  General;
  Add;
  LogicalOperators;
  OwnsValues;
end;

class procedure TTest_adot_Collections.Test_AA;
var
  aa: TAATree<integer, integer>;
  h: TItemHandle;
begin
  aa := TAATree<integer, integer>.Create;
  try
    aa.Add(1,10);
    aa.Add(2,20);
    aa.Add(3,30);
    assert(aa.Count=3);
    assert(aa.ContainsKeys([1,2,3], stAll));
    assert(not aa.ContainsKeys([0,4], stAny));
    aa.Delete(aa.Find(2));
    assert(aa.Count=2);
    assert(aa.ContainsKeys([1,3], stAll));
    assert(not aa.ContainsKeys([0,2,4], stAny));
    aa.Delete(aa.Find(1));
    assert(aa.Count=1);
    assert(aa.ContainsKeys([3], stAll));
    assert(not aa.ContainsKeys([0,1,2,4], stAny));
    aa.Delete(aa.Find(3));
    assert(aa.Count=0);
    assert(not aa.ContainsKeys([0,1,2,3,4], stAny));

    aa.Add(1,10);
    aa.Add(2,20);
    aa.Add(3,30);
    assert(aa.ContainsKeys([1,2,3], stAll));
    assert(not aa.ContainsKeys([0,4], stAny));
    aa.Clear;
    assert(not aa.ContainsKeys([0,1,2,3,4], stAny));

    aa.Add(1,10);
    aa.AddOrSetValue(3,20);
    aa.AddOrSetValue(3,10);
    aa.Add(2,20);
    aa.Add(5,50);
    aa.Add(4,40);
    assert(aa[3]=10);
    assert(aa.Count=5);
    assert(aa.MinKey=1);
    assert(aa.MaxKey=5);
    assert(aa.First(h) and (aa.Keys[h]=1));
    assert(aa.Next(h) and (aa.Keys[h]=2));
    assert(aa.Next(h) and (aa.Keys[h]=3));
    assert(aa.Next(h) and (aa.Keys[h]=4));
    assert(aa.Next(h) and (aa.Keys[h]=5));
    assert(not aa.Next(h));
  finally
    aa.Free;
  end;

end;

type
  THackHeap<K,V> = class(TBinaryHeapClass<K,V>);
  THackHeap<K> = class(TBinaryHeapClass<K>);

class procedure TTest_adot_Collections.Test_Heap_Search;
var
  h: THackHeap<integer,string>;
  v: TArray<TPair<integer,string>>;
  i,j: Integer;
  s: TSet<integer>;
  p: TPair<integer,string>;
begin
  h := THackHeap<integer,string>.Create;
  try
    j := 0;
    setlength(v, 100);
    for i := 0 to High(v) do
    begin
      inc(j, random(10)+1);
      v[i].Key := j;
      v[i].Value := 'num ' + j.ToString;
    end;
    //v := [1,2,3,4,5];
    TArrayUtils.Randomize<TPair<integer,string>>(v);
    h.Add(v);
    for i := 0 to High(v) do
    begin
      j := h.Find(v[i].Key);
      assert(j>=0);
      Assert( h[j].Key = v[i].Key );
    end;
    TArray.Sort<TPair<integer,string>>(v, TDelegatedComparer<TPair<integer,string>>.Create(
      function(const A,B: TPair<integer,string>): integer
      begin
        result := A.Key-B.Key;
      end));
    for I := 0 to High(v) do
      Assert(v[i].Key=h.ExtractMin.Key);
    Assert(h.Count=0);
    h.Add(v);
    for I := 0 to High(v) do
    begin
      Assert(v[i].Key=h.MinValue.Key);
      h.DeleteMin;
    end;
    Assert(h.Count=0);

    h.Add(v);
    TArrayUtils.Randomize<TPair<integer,string>>(v);
    for i := 0 to High(v) div 2 do
    begin
      h.Delete(h.Find(v[i].Key));
      v[i].Key := high(integer);
    end;
    assert(h.count=High(v)-(High(v) div 2+1)+1);
    for i := High(v) div 2+1 to High(v) do
      assert( h[ h.Find(v[i].Key) ].Key = v[i].Key );
    TArray.Sort<TPair<integer,string>>(v, TDelegatedComparer<TPair<integer,string>>.Create(
      function(const A,B: TPair<integer,string>): integer
      begin
        result := A.Key-B.Key;
      end));
    j := 0;
    s.Clear;
    for i := High(v) div 2+1 to High(v) do
    begin
      assert(v[j].Key=h.ExtractMin.Key);
      inc(j);
    end;
    Assert(h.Count=0);

    j := 0;
    for i := 0 to High(v) do
    begin
      inc(j, random(10)+1);
      v[i].Key := j;
      v[i].Value := 'value='+j.ToString;
    end;
    h.Add(v);
    for i := 0 to High(v) do
      s.Add(v[i].Key);
    assert(h.Count=s.Count);
    j := 0;
    for i in h.Keys do
    begin
      Assert(i in s);
      inc(j);
    end;
    assert(j=Length(v));
    j := 0;
    for p in h do
    begin
      Assert(p.Key in s);
      inc(j);
    end;
    assert(j=Length(v));
    Assert(h.Count=Length(v));
    Assert(h.Capacity>=h.Count);
    h.Clear;
    Assert(h.Count=0);
    h.TrimExcess;
    Assert(h.Capacity=0);

  finally
    h.Free;
  end;
end;

class procedure TTest_adot_Collections.Test_Heap_Search2;
var
  h: THackHeap<integer>;
  v: TArray<integer>;
  i,j: Integer;
  s: TSet<integer>;
begin
  h := THackHeap<integer>.Create;
  try
    j := 0;
    setlength(v, 100);
    for i := 0 to High(v) do
    begin
      inc(j, random(10)+1);
      v[i] := j;
    end;
    //v := [1,2,3,4,5];
    TArrayUtils.Randomize<integer>(v);
    h.Add(v);
    for i := 0 to High(v) do
    begin
      j := h.Find(v[i]);
      assert(j>=0);
      Assert( h[j] = v[i] );
    end;
    TArray.Sort<integer>(v);
    for I := 0 to High(v) do
      Assert(v[i]=h.ExtractMin);
    Assert(h.Count=0);
    h.Add(v);
    for I := 0 to High(v) do
    begin
      Assert(v[i]=h.MinValue);
      h.DeleteMin;
    end;
    Assert(h.Count=0);

    h.Add(v);
    TArrayUtils.Randomize<integer>(v);
    for i := 0 to High(v) div 2 do
    begin
      h.Delete(h.Find(v[i]));
      v[i] := high(integer);
    end;
    assert(h.count=High(v)-(High(v) div 2+1)+1);
    for i := High(v) div 2+1 to High(v) do
      assert( h[ h.Find(v[i]) ] = v[i] );
    TArray.Sort<integer>(v);
    j := 0;
    s.Clear;
    for i := High(v) div 2+1 to High(v) do
    begin
      assert(v[j]=h.ExtractMin);
      inc(j);
    end;
    Assert(h.Count=0);

    j := 0;
    for i := 0 to High(v) do
    begin
      inc(j, random(10)+1);
      v[i] := j;
    end;
    h.Add(v);
    s.Add(v);
    assert(h.Count=s.Count);
    j := 0;
    for i in h do
    begin
      Assert(i in s);
      inc(j);
    end;
    assert(j=Length(v));
    j := 0;
    for i in h do
    begin
      Assert(i in s);
      inc(j);
    end;
    assert(j=Length(v));
    Assert(h.Count=Length(v));
    Assert(h.Capacity>=h.Count);
    h.Clear;
    Assert(h.Count=0);
    h.TrimExcess;
    Assert(h.Capacity=0);

  finally
    h.Free;
  end;
end;

class procedure TTest_adot_Collections.Test_Heap;
const
  TotalAdd = 10*1000;
  AvgInHeap = TotalAdd div 100;
var

  h: TBinaryHeapClass<string>;
  l: TStringList;
  rep: TStringList;
  MinTestSize: integer;
  i,j,m,n: Integer;
  s: string;

  procedure CheckHeap;
  var i: integer;
  begin
    for i := 0 to h.Count-1 do
      Assert(h[i] >= h.MinValue);
  end;

  procedure CheckHeapStrong(h: TBinaryHeapClass<string>; l: TStringList);
  var
    i: integer;
    s: TStringList;
  begin
    CheckHeap;
    s := TStringList.Create;
    try
      if random<0.1 then
      begin
        while not h.Empty do
          s.Add(h.ExtractMin);
        for i := 0 to s.Count-1 do
          h.Add(s[i]);
      end
      else
        for i := 0 to h.Count-1 do
          s.Add(h[i]);
      s.Sort;
      try
        assert(s.Count=l.Count);
      except
        assert(s.Count=l.Count);
      end;
      for i := 0 to l.Count-1 do
        try
          Assert(s[i]=l[i]);
        except
          Assert(s[i]=l[i]);
        end;
    finally
      s.Free;
    end;
  end;

begin
  Test_Heap_Search2;
  Test_Heap_Search;
  MinTestSize := high(MinTestSize);
  repeat
    h := nil;
    l := nil;
    rep := nil;
    try
      rep := TStringList.Create;
      h := TBinaryHeapClass<string>.Create(0, TDelegatedComparer<string>.Create(
        function(const a,b: string):integer
        begin
          if a<b then
            result := -1
          else
            if a=b then
              result := 0
            else
              result := 1;
        end));

      h.Add(['omp', 'yxx']);
      CheckHeap;
      rep.Clear;
      for s in h do
        rep.Add(s);
      rep.Sort;
      assert(rep[0]='omp');
      assert(rep[1]='yxx');
      h.Clear;

      try
        l := TStringList.Create;
        l.Sorted := True;
        l.Duplicates := dupAccept;
        for i := 1 to TotalAdd do
        begin
          CheckHeapStrong(h,l);
          Assert(l.Count=h.Count);
          s := TStr.RandomString(3);
          l.Add(s);
          h.Add(s);
          rep.Add(s);

          if (l.Count>AvgInHeap) and (random(AvgInHeap)<10) then
          begin
            j := AvgInHeap-random(AvgInHeap);
            while l.Count>j do
            begin
              if random(100)<30 then
                m := 0
              else
                m := Random(l.Count);
              n := h.Find(l[m]);
              Assert(n>=0);
              l.Delete(m);
              h.Delete(n);
              CheckHeap;
            end;
          end;

        end;
        Break;
      except
        if rep.Count<MinTestSize then
        begin
          MinTestSize := rep.Count;
          rep.SaveToFile(changefileext(paramstr(0), '.Test_Heap.txt'));
        end;
      end;

    finally
      FreeAndNil(rep);
      FreeAndNil(h);
      FreeAndNil(l);
    end;

  until False;
end;

class procedure TTest_adot_Collections.Test_Multimap;
var
  m: TMultimapClass<string, integer>;
  e: TMultimapClass<string, integer>.TValueEnumerator;
  s: TSet<integer>;
  Key: string;
  i,v: Integer;
  p: TMultimapClass<string, integer>.TPair;
begin

  // test default constructor
  m := TMultimapClass<string, integer>.Create;
  m.Free;

  m := nil;
  try
    m := TMultimapClass<string, integer>.Create;
    m.Add('1.1', [11,12,13]);
    m.Add('2',2);
    m.Add('2',2);
    m.Add('3',3);
    m.Add('',[5,5]);
    m.Add('2',3);

    Assert(m.TotalValuesCount=9);
    Assert(m.ValuesCount['1.1']=3);
    Assert(m.ValuesCount['2']=3);
    Assert(m.ValuesCount['3']=1);
    Assert(m.ValuesCount['']=2);
    Assert(m.ContainsKey('1.1'));
    Assert(m.ContainsKeys(['1.1', '2', '3', '']));
    Assert(not m.ContainsKey('4'));
    Assert(not m.ContainsKeys(['1.1', '2', '3', '', '4'], cctAll));
    Assert(m.ContainsKeys(['1.1', '2', '3', '', '4'], cctAnyOf));
    Assert(m.ContainsKeys(['1.1', '2', '3', ''], cctAll));

    i := 0;
    for p in m do
      i := i + p.Value;
    assert(i=56);

    i := 0;
    for key in m.Keys do
      for v in m[key] do
        inc(i, v);
    assert(i=56);

    e := m.Values['1.1'];
    s := TSet<integer>.Create([11,12,13]);
    Assert(e.MoveNext and s.Contains(e.Current));
    Assert(e.MoveNext and s.Contains(e.Current));
    Assert(e.MoveNext and s.Contains(e.Current));
    Assert(not e.MoveNext);

    Assert(s.Count=3);
    for i in s do
      Assert(i in [11,12,13]);
    s.Clear;

    m.Remove('2');
    Assert(m.TotalValuesCount=6);
    Assert(not m.ContainsKey('2'));

    m.RemoveValues('', [6]);
    Assert(m.TotalValuesCount=6);
    m.RemoveValues('', [5]);
    Assert(m.TotalValuesCount=4);
    e := m.Values[''];
    Assert(not e.MoveNext);

    i := 0;
    for Key in m.Keys do
    begin
      e := m.Values[Key];
      while e.MoveNext do
        i := i + e.Current;
    end;
    assert(i=39);

    for Key in m.Keys do
    begin
      e := m.Values[Key];
      while e.MoveNext do
        m.RemoveValue(e);
    end;
    Assert(m.TotalValuesCount=0);

  finally
    FreeAndNil(m);
  end;
end;

type
  TRingSim<T> = class
  protected
    FItems: TList<T>;
    FCapacity: integer;

    constructor Create(Cap: integer);
    destructor Destroy; override;

    procedure AddToHead(Value: T);
    procedure AddToTail(Value: T);
    function FromHead(n: integer): T;
    function FromTail(n: integer): T;
    function ExtractHead: T;
    function ExtractTail: T;
    function EqualTo(const R: TRingClass<T>): boolean;
    procedure ChangeCapacity(nc: integer);

    class procedure RandomOperation(A: TRingSim<integer>; B: TRingClass<integer>); static;
  end;

{ TRingSim<T> }

constructor TRingSim<T>.Create(Cap: integer);
begin
  FItems := TList<T>.Create;
  FItems.Capacity := Cap;
  FCapacity := Cap;
end;

destructor TRingSim<T>.Destroy;
begin
  FReeAndNil(FItems);
  FCapacity := 0;
  inherited;
end;

procedure TRingSim<T>.ChangeCapacity(nc: integer);
begin
  assert(nc >= FItems.Count);
  FCapacity := nc;
end;

procedure TRingSim<T>.AddToHead(Value: T);
begin
  FItems.Add(Value);
  if FItems.Count>FCapacity then
    FItems.Delete(0);
end;

procedure TRingSim<T>.AddToTail(Value: T);
begin
  FItems.Insert(0, Value);
  if FItems.Count>FCapacity then
    FItems.Delete(FItems.Count-1);
end;

function TRingSim<T>.ExtractHead: T;
begin
  result := FItems[FItems.Count-1];
  FItems.Delete(FItems.Count-1);
end;

function TRingSim<T>.ExtractTail: T;
begin
  result := FItems[0];
  FItems.Delete(0);
end;

function TRingSim<T>.FromHead(n: integer): T;
begin
  result := FItems[FItems.Count-1-n];
end;

function TRingSim<T>.FromTail(n: integer): T;
begin
  result := FItems[n];
end;

function TRingSim<T>.EqualTo(const R: TRingClass<T>): boolean;
var
  I: Integer;
  C: IEqualityComparer<T>;
  V: T;
begin
  C := TComparerUtils.DefaultEqualityComparer<T>;
  result := R.Count=FItems.Count;
  if not result then
    Exit;
  if R.Count>0 then
    result := C.Equals(R.Head, FromHead(0)) and C.Equals(R.Tail, FromTail(0));
  if not result then
    Exit;
  for I := 0 to FItems.Count-1 do
    if not C.Equals(FromHead(I), R.ItemsFromHead[I]) or not C.Equals(FromTail(I), R.ItemsFromTail[I]) then
      Exit(False);
  I := 0;
  for V in R do
  begin
    Assert(C.Equals(FromHead(I), V));
    inc(I);
  end;
  Assert(I=R.Count);
end;

class procedure TRingSim<T>.RandomOperation(A: TRingSim<integer>; B: TRingClass<integer>);
var
  C, I,N: Integer;
  V: TArray<integer>;
begin
  C := Random(6);
  case C of
    0,1:
      begin
        TArrayUtils.FillRandom(V, Random(10)+1, -100,100);
        for I := 0 to High(V) do
          case C of
            0:
              begin
                A.AddToHead(V[I]);
                B.Add(V[I]);
              end;
            1:
              begin
                A.AddToTail(V[I]);
                B.AddToTail(V[I]);
              end;
          end;
      end;
    2,3:
      begin
        N := Random(10) + 1;
        for I := 0 to N-1 do
          if A.FItems.Count=0 then
            Break
          else
          case C of
            2: Assert(A.ExtractHead = B.ExtractHead);
            3: Assert(A.ExtractTail = B.Extract);
          end;
      end;
    4:
      if Random(100)<20 then
      begin
        A.FItems.Clear;
        B.Clear;
      end;
    5:
      begin
        i := random(5)+1;
        A.ChangeCapacity(A.FItems.Count + i);
        B.Capacity := B.Count + i;
      end;
  end;
end;

class procedure TTest_adot_Collections.Test_ring;
var
  r: TRingClass<integer>;
  s: TRingSim<integer>;
  i,j: Integer;
begin
  r := TRingClass<integer>.create(5);
  try
    r.Add([1,2,3]);
    Assert(r.Count=3);
    Assert(r.Head=3);
    Assert(r.ItemsFromHead[0]=3);
    Assert(r.ItemsFromHead[1]=2);
    Assert(r.ItemsFromHead[2]=1);
    Assert(r.Tail=1);
    Assert(r.ItemsFromTail[0]=1);
    Assert(r.ItemsFromTail[1]=2);
    Assert(r.ItemsFromTail[2]=3);
    r.Add([4,5]);
    Assert(r.Count=5);
    Assert(r.ItemsFromHead[0]=5);
    Assert(r.ItemsFromHead[4]=1);
    Assert(r.ItemsFromTail[0]=1);
    Assert(r.ItemsFromTail[4]=5);
    r.Add(6);
    Assert(r.Count=5);
    Assert(r.ItemsFromHead[0]=6);
    Assert(r.ItemsFromHead[4]=2);
    Assert(r.ItemsFromTail[0]=2);
    Assert(r.ItemsFromTail[4]=6);
    r.Add(7);
    Assert(r.Count=5);
    Assert(r.ItemsFromHead[0]=7);
    Assert(r.ItemsFromHead[4]=3);
    Assert(r.ItemsFromTail[0]=3);
    Assert(r.ItemsFromTail[4]=7);
  finally
    FreeAndNil(r);
  end;

  r := TRingClass<integer>.Create(2);
  s := TRingSim<integer>.Create(2);
  try
    r.AddToTail(73);
    r.AddToTail(35);
    r.AddToTail(-36);
    assert(r.Count=2);
    assert(r.Tail=-36);
    assert(r.Head=35);

    r.Clear;
    r.AddToTail([73, 35, -36, -68]);
    Assert(r.ItemsFromTail[0]=-68);
    Assert(r.ItemsFromTail[1]=-36);
    Assert(r.ItemsFromHead[0]=-36);
    Assert(r.ItemsFromHead[1]=-68);

    s.AddToTail(73);
    s.AddToTail(35);
    s.AddToTail(-36);
    s.AddToTail(-68);
    Assert(s.FromTail(0)=-68);
    Assert(s.FromTail(1)=-36);
    Assert(s.FromHead(0)=-36);
    Assert(s.FromHead(1)=-68);

    r.Clear;
    r.AddToTail([1,2]);
    Assert(r.ItemsFromTail[0]=2);
    Assert(r.ItemsFromTail[1]=1);
    Assert(r.ItemsFromHead[0]=1);
    Assert(r.ItemsFromHead[1]=2);

  finally
    FReeandNil(r);
    FReeandNil(s);
  end;

  for j := 1 to 21 do
  begin
    r := TRingClass<integer>.Create(j);
    s := TRingSim<integer>.Create(j);
    try
      for i := 0 to 99 do
      begin
        s.RandomOperation(s,r);
        Assert(s.EqualTo(r));
      end;
    finally
      FReeAndNil(r);
      FReeAndNil(s);
    end;
  end;
end;

function EqualSets(a: TOrderedMapClass<integer, string>; b: TSet<integer>): Boolean;
var
  d: TDictionary<integer, TEmptyRec>;
  e: TEmptyRec;
  I: integer;
begin
  d := TDictionary<integer, TEmptyRec>.create;
  try
    for I in B do
      d.Add(I, e);
    result := (a.Count=b.Count) and (a.Count=d.Count);
    if not result then
      Exit;
    for I in a.KeyCollection do
      d.Remove(I);
    result := d.Count=0;
  finally
    d.Free;
  end;
end;

class procedure TTest_adot_Collections.Test_TOrderedMapClass_vs_TSet;
var
  a: TOrderedMapClass<integer, string>;
  s: TSet<integer>;
  v: TArray<integer>;
  I,J,N: Integer;
begin
  a := TOrderedMapClass<integer, string>.Create;
  try
    TArrayUtils.Fill(v, 1000, 0, 3);
    TArrayUtils.Randomize<integer>(v);
    for I := 0 to High(v) do
    begin
      J := Random(10);
      case J of
        0..3:
          begin
            A.Add(V[I], V[I].ToString);
            S.Add(V[I]);
          end;
        4..7:
          begin
            N := Random(I);
            A.Add(V[N], V[N].ToString);
            S.Add(V[N]);
          end;
        8:
          begin
            N := Max(High(v), I + 1 + Random(High(v)-I));
            if A.ContainsKey(V[N]) then
              A.Remove(V[N]);
            S.Remove(V[N]);
          end;
        9:
          begin
            N := Random(I);
            if A.ContainsKey(V[N]) then
              A.Remove(V[N]);
            S.Remove(V[N]);
          end;
      end;
      Assert(EqualSets(a,s));
    end;
  finally
    FreeAndNil(a);
  end;
end;

{ TOwnsValuesTestClass }

constructor TOwnsValuesTestClass.Create(AInt: PInteger);
begin
  P := AInt;
  inc(p^);
end;

destructor TOwnsValuesTestClass.Destroy;
begin
  if p<>nil then dec(p^);
  inherited;
end;

{ TTest_adot_Collections.TTest_TMap }

class procedure TTest_adot_Collections.TTest_TMap.General;
var
  A,B: TMap<string, integer>;
begin
  A.Add('One', 1);
  A.Add('Two', 2);
  Assert(A.AsString='("One", 1) ("Two", 2)' );
  B := A;
  B.Add('Three', 3);
  Assert((A.Count=2) and (B.Count=3));
  Assert((A['One']=1) and (A['Two']=2) and (B['Three']=3));
  B.Clear;
  Assert(B.Count=0);
  B.Add(A);
  Assert((B['One']=1) and (B['Two']=2));
  Assert(A.ContainsKey('One') and not A.ContainsKey('qqq'));
end;

class procedure TTest_adot_Collections.TTest_TMap.OwnsValues;
type
  TC = class of TObject;
var
  N: integer;
  V: TOwnsValuesTestClass;
  A,B: TMap<integer, TOwnsValuesTestClass>;
begin

  N := 0;
  A.Add(1, TOwnsValuesTestClass.Create(@N));
  assert((N=1) and not A.OwnsValues and not A.OwnsKeys);
  A.OwnsValues := True;
  Assert(A.OwnsValues);
  A.Clear;
  assert(N=0);

  A.Add(1, TOwnsValuesTestClass.Create(@N));
  Assert(not B.OwnsValues); { default is False }
  B := A; { B has same object as A! }
  Assert(A.OwnsValues and B.OwnsValues);
  B.OwnsValues := False;
  Assert(A.OwnsValues and not B.OwnsValues);
  V := TOwnsValuesTestClass.Create(@N);
  B.Add(2, V);
  assert(N=2);
  A.Clear;
  assert(N=1);
  B.Clear;
  assert(N=1);
  FreeAndNil(V);
  assert(N=0);

  A.Add(1, TOwnsValuesTestClass.Create(@N));
  B := A; { B has same object as A! }
  B.OwnsValues := False;
  V := TOwnsValuesTestClass.Create(@N);
  B.Add(2, V);
  assert(N=2);
  A.Clear;
  assert(N=1);
  B.Clear;
  assert(N=1);
  FreeAndNil(V);
  assert(N=0);

end;

class procedure TTest_adot_Collections.TTest_TMap.Run;
begin
  General;
  OwnsValues;
end;

{ TTest_adot_Strings }

class procedure TTest_adot_Strings.test_TTokNumbers;
begin
  CheckTokenizerClass(TTokNumbers, '1', ['1']);
  CheckTokenizerClass(TTokNumbers, '11', ['11']);
  CheckTokenizerClass(TTokNumbers, '+1', ['+1']);
  CheckTokenizerClass(TTokNumbers, '+11', ['+11']);
  CheckTokenizerClass(TTokNumbers, '-1', ['-1']);
  CheckTokenizerClass(TTokNumbers, '-11', ['-11']);
  CheckTokenizerClass(TTokNumbers, '1.1', ['1.1']);
  CheckTokenizerClass(TTokNumbers, '11.1', ['11.1']);
  CheckTokenizerClass(TTokNumbers, '1.11', ['1.11']);
  CheckTokenizerClass(TTokNumbers, '-1.1', ['-1.1']);
  CheckTokenizerClass(TTokNumbers, '-11.1', ['-11.1']);
  CheckTokenizerClass(TTokNumbers, '-1.11', ['-1.11']);
  CheckTokenizerClass(TTokNumbers, '+1.1', ['+1.1']);
  CheckTokenizerClass(TTokNumbers, '+11.1', ['+11.1']);
  CheckTokenizerClass(TTokNumbers, '+1.11', ['+1.11']);

  CheckTokenizerClass(TTokNumbers, 'q1', ['1']);
  CheckTokenizerClass(TTokNumbers, 'qq11', ['11']);
  CheckTokenizerClass(TTokNumbers, '+-+1', ['+1']);
  CheckTokenizerClass(TTokNumbers, 'a++11', ['+11']);
  CheckTokenizerClass(TTokNumbers, '--1', ['-1']);
  CheckTokenizerClass(TTokNumbers, 'a-11', ['-11']);
  CheckTokenizerClass(TTokNumbers, 'a1.1', ['1.1']);
  CheckTokenizerClass(TTokNumbers, 'a11.1', ['11.1']);
  CheckTokenizerClass(TTokNumbers, 'a1.11', ['1.11']);
  CheckTokenizerClass(TTokNumbers, 'a-1.1', ['-1.1']);
  CheckTokenizerClass(TTokNumbers, 'a-11.1', ['-11.1']);
  CheckTokenizerClass(TTokNumbers, 'a-1.11', ['-1.11']);
  CheckTokenizerClass(TTokNumbers, 'a+1.1', ['+1.1']);
  CheckTokenizerClass(TTokNumbers, 'a+11.1', ['+11.1']);
  CheckTokenizerClass(TTokNumbers, 'a+1.11', ['+1.11']);

  CheckTokenizerClass(TTokNumbers, '', []);
  CheckTokenizerClass(TTokNumbers, 'a', []);
  CheckTokenizerClass(TTokNumbers, '+', []);
  CheckTokenizerClass(TTokNumbers, '-', []);
  CheckTokenizerClass(TTokNumbers, '+-', []);
  CheckTokenizerClass(TTokNumbers, '-+', []);
  CheckTokenizerClass(TTokNumbers, 'a+-', []);
  CheckTokenizerClass(TTokNumbers, 'a-+', []);
  CheckTokenizerClass(TTokNumbers, '+-a', []);
  CheckTokenizerClass(TTokNumbers, '-+a', []);

  CheckTokenizerClass(TTokNumbers, '1+', ['1']);
  CheckTokenizerClass(TTokNumbers, '11+', ['11']);
  CheckTokenizerClass(TTokNumbers, '+1+', ['+1']);
  CheckTokenizerClass(TTokNumbers, '+11+', ['+11']);
  CheckTokenizerClass(TTokNumbers, '-1+', ['-1']);
  CheckTokenizerClass(TTokNumbers, '-11+', ['-11']);
  CheckTokenizerClass(TTokNumbers, '1.1-', ['1.1']);
  CheckTokenizerClass(TTokNumbers, '11.1-', ['11.1']);
  CheckTokenizerClass(TTokNumbers, '1.11q', ['1.11']);
  CheckTokenizerClass(TTokNumbers, '-1.1q', ['-1.1']);
  CheckTokenizerClass(TTokNumbers, '-11.1q', ['-11.1']);
  CheckTokenizerClass(TTokNumbers, '-1.11q', ['-1.11']);
  CheckTokenizerClass(TTokNumbers, '+1.1q', ['+1.1']);
  CheckTokenizerClass(TTokNumbers, '+11.1q', ['+11.1']);
  CheckTokenizerClass(TTokNumbers, '+1.11q', ['+1.11']);

  CheckTokenizerClass(TTokNumbers, 'q1+', ['1']);
  CheckTokenizerClass(TTokNumbers, 'qq11+', ['11']);
  CheckTokenizerClass(TTokNumbers, 'qqq+1+', ['+1']);
  CheckTokenizerClass(TTokNumbers, 'q+11+', ['+11']);
  CheckTokenizerClass(TTokNumbers, 'qq-1+', ['-1']);
  CheckTokenizerClass(TTokNumbers, 'qqq-11+', ['-11']);
  CheckTokenizerClass(TTokNumbers, 'q1.1-', ['1.1']);
  CheckTokenizerClass(TTokNumbers, 'qq11.1-', ['11.1']);
  CheckTokenizerClass(TTokNumbers, 'qqq1.11q', ['1.11']);
  CheckTokenizerClass(TTokNumbers, 'q-1.1q', ['-1.1']);
  CheckTokenizerClass(TTokNumbers, 'qq-11.1q', ['-11.1']);
  CheckTokenizerClass(TTokNumbers, 'qqq-1.11q', ['-1.11']);
  CheckTokenizerClass(TTokNumbers, 'q+1.1q', ['+1.1']);
  CheckTokenizerClass(TTokNumbers, 'qq+11.1q', ['+11.1']);
  CheckTokenizerClass(TTokNumbers, 'qqq+1.11q', ['+1.11']);

  CheckTokenizerClass(TTokNumbers, '1.a', ['1.']);
end;

class procedure TTest_adot_Strings.CheckTokenizer(T: TTokCustom; const Text: string; const R: array of string);
var
  I: integer;
  S: string;
begin
  T.Reset(PChar(Text), Length(Text));
  I := 0;
  while T.Next(S) do
  begin
    assert(S=R[I]);
    inc(I);
  end;
  Assert(I=Length(R));
end;

class procedure TTest_adot_Strings.CheckTokenizerClass(TokenizerClass: CTokCustom; const Text: string; const R: array of string);
var
  T: TTokCustom;
begin
  T := TokenizerClass.Create(Text);
  try
    CheckTokenizer(T, Text, R);
  finally
    T.Free;
  end;
end;

class procedure TTest_adot_Strings.test_TTokLines;
const
  EOL: array of string = [#$000A, #$000B, #$000C, #$000D, #$0085, #$2028, #$2029, #13#10, #10#13];
var
  i: Integer;
begin
  CheckTokenizerClass(TTokLines, '', []);
  for i := Low(EOL) to High(EOL) do
  begin
    CheckTokenizerClass(TTokLines, EOL[i], ['']);
    CheckTokenizerClass(TTokLines, EOL[i]+EOL[i], ['','']);
    CheckTokenizerClass(TTokLines, EOL[i]+'1'+EOL[i], ['','1']);
    CheckTokenizerClass(TTokLines, '1'+EOL[i]+'1'+EOL[i], ['1','1']);
    CheckTokenizerClass(TTokLines, '1'+EOL[i], ['1']);

    CheckTokenizerClass(TTokLines, EOL[i]+'q', ['','q']);
    CheckTokenizerClass(TTokLines, EOL[i]+EOL[i]+'q', ['','','q']);
    CheckTokenizerClass(TTokLines, EOL[i]+'1'+EOL[i]+'q', ['','1','q']);
    CheckTokenizerClass(TTokLines, '1'+EOL[i]+'1'+EOL[i]+'q', ['1','1','q']);
  end;
end;

class procedure TTest_adot_Strings.test_TTokCharDelimitedLines;
var
  T: TTokCharDelimitedLines;

  procedure test;
  begin
    CheckTokenizer(T, '', []);
    CheckTokenizer(T, '1200', ['1200']);
    CheckTokenizer(T, '1200;1300', ['1200','1300']);
    CheckTokenizer(T, '1;2;3', ['1','2','3']);
    CheckTokenizer(T, '1;2;', ['1','2']);
  end;

begin
  T := TTokCharDelimitedLines.Create('', ';', True);
  try
    test;
  finally
    T.Free;
  end;
  T := TTokCharDelimitedLines.Create('', ';', False);
  try
    test;
  finally
    T.Free;
  end;
end;

class procedure TTest_adot_Strings.test_TTokLettersOrDigits;
begin
  CheckTokenizerClass(TTokLettersOrDigits, '', []);
  CheckTokenizerClass(TTokLettersOrDigits, '1200', ['1200']);
  CheckTokenizerClass(TTokLettersOrDigits, '1200 ;1300', ['1200','1300']);
  CheckTokenizerClass(TTokLettersOrDigits, '1;2;3', ['1','2','3']);
  CheckTokenizerClass(TTokLettersOrDigits, '1;2; ', ['1','2']);
  CheckTokenizerClass(TTokLettersOrDigits, 'a1;2b; ccc', ['a1','2b','ccc']);
end;

class procedure TTest_adot_Strings.test_TTokDigits;
begin
  CheckTokenizerClass(TTokDigits, '', []);
  CheckTokenizerClass(TTokDigits, '1200', ['1200']);
  CheckTokenizerClass(TTokDigits, '1200 ;1300', ['1200','1300']);
  CheckTokenizerClass(TTokDigits, '1;2;3', ['1','2','3']);
  CheckTokenizerClass(TTokDigits, '1;2; ', ['1','2']);
  CheckTokenizerClass(TTokDigits, 'a1;2b; ccc', ['1','2']);
end;

class procedure TTest_adot_Strings.test_TTokLiterals;

  function S(const A: string): string; overload;
  begin
    result := A.Replace('"', '''');
  end;

  function S(const A: array of string): TArray<string>; overload;
  var
    i: Integer;
  begin
    setlength(result, length(A));
    for i := 0 to High(A) do
      result[i] := S(A[i]);
  end;

begin
  CheckTokenizerClass(TTokLiterals, S(''), S([]));
  CheckTokenizerClass(TTokLiterals, S('qq11'), S([]));
  CheckTokenizerClass(TTokLiterals, S('""'), S(['""']));
  CheckTokenizerClass(TTokLiterals, S('"a"'), S(['"a"']));
  CheckTokenizerClass(TTokLiterals, S('1"a"'), S(['"a"']));
  CheckTokenizerClass(TTokLiterals, S('"a"2'), S(['"a"']));
  CheckTokenizerClass(TTokLiterals, S('1"a"2'), S(['"a"']));
  CheckTokenizerClass(TTokLiterals, S('"a""b"'), S(['"a""b"']));
  CheckTokenizerClass(TTokLiterals, S('"a" "b"'), S(['"a"', '"b"']));
  CheckTokenizerClass(TTokLiterals, S('"a"#13#10"b"'), S(['"a"', '"b"']));
  CheckTokenizerClass(TTokLiterals, S('"a"ccc"b"'), S(['"a"', '"b"']));
  CheckTokenizerClass(TTokLiterals, S('1"a"ccc"b"2'), S(['"a"', '"b"']));
end;

class procedure TTest_adot_Strings.test_TTokComments;
var
  N: TTokNot;
begin
  CheckTokenizerClass(TTokComments, '', []);
  CheckTokenizerClass(TTokComments, #13#10, []);
  CheckTokenizerClass(TTokComments, '//dd''sdsadsa{(*'#13#10'b', ['//dd''sdsadsa{(*']);
  CheckTokenizerClass(TTokComments, 'a//dd''sdsadsa{(*'#13#10'b', ['//dd''sdsadsa{(*']);
  CheckTokenizerClass(TTokComments, 'a{fds''(*ds*)}b', ['{fds''(*ds*)}']);
  CheckTokenizerClass(TTokComments, 'a(*fds''{ds}*)b', ['(*fds''{ds}*)']);
  CheckTokenizerClass(TTokComments, 'a{}b(**)c', ['{}', '(**)']);
  CheckTokenizerClass(TTokComments, 'a{}b(**)c//', ['{}', '(**)', '//']);
  CheckTokenizerClass(TTokComments, 'a{}b(**)c//'#13#10, ['{}', '(**)', '//']);
  CheckTokenizerClass(TTokComments, 'a{}b(**)c//'#13#10'd{jjj}', ['{}', '(**)', '//', '{jjj}']);
  CheckTokenizerClass(TTokComments, 'a{}b(**)c//'#13#10'd{jjj}e', ['{}', '(**)', '//', '{jjj}']);
  CheckTokenizerClass(TTokComments, 'a{'#13#10'}', ['{'#13#10'}']);
  CheckTokenizerClass(TTokComments, 'a{'#13#10'}b', ['{'#13#10'}']);
  CheckTokenizerClass(TTokComments, 'a''{(*//''{}b', ['{}']);
  CheckTokenizerClass(TTokComments, 'a''bcd', []);
  CheckTokenizerClass(TTokComments, 'a{bcd', ['{bcd']);
  CheckTokenizerClass(TTokComments, 'a(*bcd', ['(*bcd']);
  CheckTokenizerClass(TTokComments, 'a//', ['//']);
  CheckTokenizerClass(TTokComments, 'a/', []);
  CheckTokenizerClass(TTokComments, #13#10, []);
  CheckTokenizerClass(TTokComments, #13#10'a', []);
  CheckTokenizerClass(TTokComments, #13#10#13#10, []);
  CheckTokenizerClass(TTokComments, #13#10#13#10'a', []);
  CheckTokenizerClass(TTokComments, '''{aaa''{b', ['{b']);
  CheckTokenizerClass(TTokComments, '//aaa'#13#10'//bbb'#13#10'//ccc', ['//aaa', '//bbb', '//ccc']);
  CheckTokenizerClass(TTokComments, '//aa{a'#13#10'//bbb'#13#10'//cc}c', ['//aa{a', '//bbb', '//cc}c']);
  CheckTokenizerClass(TTokComments, 'aa{a'#13#10'//bbb'#13#10'//cc}c', ['{a'#13#10'//bbb'#13#10'//cc}']);
  CheckTokenizerClass(TTokComments, 'aa''{(*//bbb''', []);
  CheckTokenizerClass(TTokComments, 'aa''{(*//bbb''{}c', ['{}']);

  N := TTokNot.Create(TTokComments.Create, True);
  try
    CheckTokenizer(N, '', []);
    CheckTokenizer(N, #13#10, [#13#10]);
    CheckTokenizer(N, '//dd''sdsadsa{(*'#13#10'b', [#13#10'b']);
    CheckTokenizer(N, 'a//dd''sdsadsa{(*'#13#10'b', ['a', #13#10'b']);
    CheckTokenizer(N, 'a{fds''(*ds*)}b', ['a', 'b']);
    CheckTokenizer(N, 'a(*fds''{ds}*)b', ['a', 'b']);
    CheckTokenizer(N, 'a{}b(**)c', ['a','b','c']);
    CheckTokenizer(N, 'a{}b(**)c//', ['a','b','c']);
    CheckTokenizer(N, 'a{}b(**)c//'#13#10, ['a','b','c',#13#10]);
    CheckTokenizer(N, 'a{}b(**)c//'#13#10'd{jjj}', ['a','b','c',#13#10'd']);
    CheckTokenizer(N, 'a{}b(**)c//'#13#10'd{jjj}e', ['a','b','c',#13#10'd','e']);
    CheckTokenizer(N, 'a{'#13#10'}', ['a']);
    CheckTokenizer(N, 'a{'#13#10'}b', ['a','b']);
    CheckTokenizer(N, 'a''{(*//''{}b', ['a''{(*//''','b']);
    CheckTokenizer(N, 'a''bcd', ['a''bcd']);
    CheckTokenizer(N, 'a{bcd', ['a']);
    CheckTokenizer(N, 'a(*bcd', ['a']);
    CheckTokenizer(N, 'a//', ['a']);
    CheckTokenizer(N, 'a/', ['a/']);
    CheckTokenizer(N, #13#10, [#13#10]);
    CheckTokenizer(N, #13#10'a', [#13#10'a']);
    CheckTokenizer(N, #13#10#13#10, [#13#10#13#10]);
    CheckTokenizer(N, #13#10#13#10'a', [#13#10#13#10'a']);
    CheckTokenizer(N, '//aaa'#13#10'//bbb'#13#10'//ccc', [#13#10, #13#10]);
    CheckTokenizer(N, '//aa{a'#13#10'//bbb'#13#10'//cc}c', [#13#10, #13#10]);
    CheckTokenizer(N, 'aa{a'#13#10'//bbb'#13#10'//cc}c', ['aa', 'c']);
    CheckTokenizer(N, 'aa''{(*//bbb''', ['aa''{(*//bbb''']);
    CheckTokenizer(N, 'aa''{(*//bbb''{}c', ['aa''{(*//bbb''', 'c']);
  finally
    FreeAndNil(N);
  end;
end;

class procedure TTest_adot_Strings.Run;
begin
  test_TTokNumbers;
  test_TTokLines;
  test_TTokCharDelimitedLines;
  test_TTokLettersOrDigits;
  test_TTokDigits;
  test_TTokLiterals;
  test_TTokComments; {TTokNot}
end;

{ TTest_adot_Tools }

class procedure TTest_adot_Tools.Run;
begin
  test_Hex;
  Test_THex;
  Test_TNullable;
  Test_TCompound;
  Test_TAutoFreeCollection;
  Test_TAuto;
  Test_TAutoFree;
end;

class procedure TTest_adot_Tools.test_Hex;
var
  b: TArray<byte>;
  c: TArray<byte>;
begin
  Assert(THEx.EncodeAnsiString('')='');
  Assert(THEx.Encode(b, 0)='');
  Assert(THEx.Encode('', False)='');
  Assert(THEx.Encode('', True)='');
  Assert(THEx.Encode('')='');
  Assert(THEx.Encode(b)='');

  Assert(THEx.DecodedSizeBytes(0)=0);
  Assert(THEx.DecodeString('')='');
  Assert(THEx.DecodeString('', False)='');
  Assert(THEx.DecodeString('', True)='');
  THEx.Decode('',b);
  Assert(THEx.DecodeAnsiString('')='');

  SetLength(b, 3);
  SetLength(c, 4);
  TArrayUtils.Fill(b, -1, 0,1);

  { class function Encode(const Buf; ByteBufSize: integer): String; overload; static; }
  Assert(THex.Encode(b[0], length(b)) = '000102');
  { class function Encode<T: Record>(const Value: T): String; overload; static; }
  Assert((THex.Encode<byte>(b[0]) = '00') and (THex.Encode<byte>(b[1]) = '01') and (THex.Encode<byte>(b[2]) = '02'));
  {  class function Encode(s: TBytes):String; overload; static; }
  Assert(THex.Encode(b) = '000102');
  { class function Encode(const s: string): string; overload; static;
    class function Encode(const s: string; utf8: boolean): string; overload; static; }
  Assert(THex.Encode('')='');
  Assert(THex.Encode('a')='6100');
  Assert(THex.Encode('ab')='61006200');
  Assert(THex.Encode('',false)='');
  Assert(THex.Encode('a',false)='6100');
  Assert(THex.Encode('ab',false)='61006200');
  Assert(THex.Encode('', true)='');
  Assert(THex.Encode('a', true)='61');
  Assert(THex.Encode('ab', true)='6162');
  { class function EncodeAnsiString(const s: AnsiString):String; static; }
  Assert(THex.EncodeAnsiString('')='');
  Assert(THex.EncodeAnsiString('a')='61');
  Assert(THex.EncodeAnsiString('ab')='6162');

  c := [5,5,5,5];
  THex.Decode( THex.Encode(b[0], length(b)), c[0]);
  assert((c[0]=b[0]) and (c[1]=b[1]) and (c[2]=b[2]) and (c[3]=5));

  c := [5,5,5,5];
  c[0] := THex.Decode<Byte>( THex.Encode<byte>(b[0]) );
  c[1] := THex.Decode<Byte>( THex.Encode<byte>(b[1]) );
  c[2] := THex.Decode<Byte>( THex.Encode<byte>(b[2]) );
  assert((c[0]=b[0]) and (c[1]=b[1]) and (c[2]=b[2]) and (c[3]=5));

  Assert(TArrayUtils.Equal<byte>(b, THex.DecodeBytes(THex.Encode(b))) );

  Assert( THex.DecodeString(THex.Encode('')) = '');
  Assert( THex.DecodeString(THex.Encode('a')) = 'a');
  Assert( THex.DecodeString(THex.Encode('ab')) = 'ab');
  Assert( THex.DecodeString(THex.Encode('',false), false) = '');
  Assert( THex.DecodeString(THex.Encode('a',false), false) = 'a');
  Assert( THex.DecodeString(THex.Encode('ab',false), false) = 'ab');
  Assert( THex.DecodeString(THex.Encode('',True), true) = '');
  Assert( THex.DecodeString(THex.Encode('a',True), true) = 'a');
  Assert( THex.DecodeString(THex.Encode('ab',True), true) = 'ab');
  Assert( THex.DecodeAnsiString(THex.EncodeAnsiString(''))='');
  Assert( THex.DecodeAnsiString(THex.EncodeAnsiString('a'))='a');
  Assert( THex.DecodeAnsiString(THex.EncodeAnsiString('ab'))='ab');
end;

class procedure TTest_adot_Tools.Test_TAuto;
begin
  TTest_TAutoFreeCollection.Test_TAuto;
end;

class procedure TTest_adot_Tools.Test_TAutoFree;
begin
  TTest_TAutoFreeCollection.Test_TAutoFree;
end;

{ TTest_TAutoFreeCollection }

constructor TTest_TAutoFreeCollection.Create;
begin
  inc(InstCount);
end;

destructor TTest_TAutoFreeCollection.Destroy;
begin
  dec(InstCount);
  inherited;
end;

class procedure TTest_TAutoFreeCollection.Create_TAuto(n: integer);
var
  Arr: array of TAuto<TTest_TAutoFreeCollection>;
  i: Integer;
begin
  InstCount := 0;
  setlength(Arr, n);
  for i := 0 to High(Arr) do
    Arr[i].Value.Data := 'test' + IntToStr(i);
  Assert(InstCount=Length(Arr));
end;

class procedure TTest_TAutoFreeCollection.Create_TAutoFree(n: integer);
var
  Arr: array of TAutoFree<TTest_TAutoFreeCollection>;
  i: Integer;
begin
  InstCount := 0;
  setlength(Arr, n);
  for i := 0 to High(Arr) do
  begin
    Arr[i] := TTest_TAutoFreeCollection.Create;
    Arr[i].Value.Data := 'test' + IntToStr(i);
  end;
  Assert(InstCount=Length(Arr));
end;

class procedure TTest_TAutoFreeCollection.Create_TAutoFreeCollection(n: integer);
var
  AutoFreeCollection: TAutoFreeCollection;
  Arr: array of TTest_TAutoFreeCollection;
  i: Integer;
begin
  InstCount := 0;
  setlength(Arr, n);
  for i := 0 to High(Arr) do
    Arr[i] := AutoFreeCollection.Add( TTest_TAutoFreeCollection.Create );
  Assert(InstCount=Length(Arr));
end;

class procedure TTest_TAutoFreeCollection.Test_TAutoFreeCollection;
begin
  Create_TAutoFreeCollection(12);
  Assert(InstCount=0);
end;

class procedure TTest_TAutoFreeCollection.Test_TAuto;
var
  Form: TAuto<TForm>;
  Memo: TAuto<TMemo>;
  d: TAuto<TDictionary<string, integer>>;
  l: TAuto<TList<string>>;
begin
  Create_TAuto(12);
  Assert(InstCount=0);

  Memo.Value.Parent := Form.Value;
  Memo.Value.Lines.Text := 'line1'#13#10'line2';
  Assert(Memo.Value.Lines.Count=2);
  Assert(Memo.Value.Lines[0]='line1');
  Assert(Memo.Value.Lines[1]='line2');
  Memo.Clear;
  Assert(Memo.Value.Lines.Count=0);

  d.Value.Add('key1',1);
  d.Value.Add('key2',2);
  d.Value.Add('key3',3);
  assert(d.Value.ContainsKey('key1'));

  l.Value.Add('line1');
  l.Value.Add('line2');
  assert(l.Value.Count=2);
  assert(l.Value[0]='line1');
  l.Free;
  assert(l.Value.Count=0);

end;

class procedure TTest_TAutoFreeCollection.Test_TAutoFree;
begin
  Create_TAutoFree(12);
  Assert(InstCount=0);
end;

class procedure TTest_adot_Tools.Test_TAutoFreeCollection;
begin
  TTest_TAutoFreeCollection.Test_TAutoFreeCollection;
end;

class procedure TTest_adot_Tools.Test_TCompound;
type
  TId = TCompound<integer, byte>;
var
  m: TMap<TId, string>;
begin
  m.Add(TId.Create(1,0), '1');
  m.Add(TId.Create(1,1), '2');
  m.Add(TId.Create(0,1), '3');
  assert(m.ContainsKey(TId.Create(1,0)));
  assert(m.ContainsKey(TId.Create(1,1)));
  assert(m.ContainsKey(TId.Create(0,1)));
  assert(not m.ContainsKey(TId.Create(0,0)));
  assert(not m.ContainsKey(TId.Create(2,0)));
  assert(not m.ContainsKey(TId.Create(0,2)));
end;

class procedure TTest_adot_Tools.Test_THex;
var
  b1,b2: TBytes;
  i: Integer;
  s: string;
begin
  setlength(b1, 7);
  for i := 1 to 5 do
    b1[i] := i;
  s := THex.Encode(b1);
  assert(AnsiLowerCase(s)='00010203040500');
  b2 := THex.DecodeBytes(s);
  Assert((length(b1)=length(b2)) and  CompareMem(@b1[0], @b2[0], length(b1)));
  Assert( THex.Decode<byte>( THex.Encode<byte>(255) ) = 255 );
  Assert( THex.Decode<integer>( THex.Encode<integer>(1000000000) ) = 1000000000 );
  Assert( THex.HexToPointer( THex.PointerToHex(PChar(s)) ) = PChar(s) );
end;

class procedure TTest_adot_Tools.Test_TNullable;
var
  a: TNullable<string>;
  b: TNullable<string>;
  c: string;
  v: variant;
begin
  c := 'test';
  a.Value := c;
  assert(not (a=b));
  assert(a<>b);
  assert(a=c);
  assert(not (a<>c));
  b := a;
  assert(a=b);
  assert(not (a<>b));
  assert(b=c);
  a := v;
  assert(a.IsNull and not a.HasValue);

  v := 'qqq';
  a := v;
  assert(not a.IsNull and a.HasValue and (a='qqq') and (a=v));
  a.HasValue := False;
  assert(a.IsNull and not a.HasValue);

  a := &c;
  assert(not a.IsNull and a.HasValue and (a='test') and (a=c));
  a.HasValue := False;
  assert(a.IsNull and not a.HasValue);
  a := @c;
  assert(not a.IsNull and a.HasValue and (a='test') and (a=c));
  a.HasValue := False;
  assert(a.IsNull and not a.HasValue);
  a := addr(c);
  assert(not a.IsNull and a.HasValue and (a='test') and (a=c));

  b := 'qwerty';
  assert(a<>b);
  b.Value := a;
  assert(a=b);
  b.HasValue := false;
  assert(a<>b);
  a := b;
  assert(a=b);

  a := 'test2';
  c := a;
  assert(c='test2');

end;

{ TTest_adot_Tools_Rtti.T1 }

constructor TTest_adot_Tools_Rtti.T1.Create;
begin
  v := 1;
end;

{ TTest_adot_Tools_Rtti.T2 }

constructor TTest_adot_Tools_Rtti.T2.Create(ACapaCITY: integer);
begin
  inherited create;
  assert(v=1);
  v := 2;
end;

{ TTest_adot_Tools_Rtti }

class procedure TTest_adot_Tools_Rtti.Run;
var
  p: T1;
begin
  Assert(TRttiUtils.IsInstance<TObject>);
  Assert(TRttiUtils.IsInstance<TTest_adot_Tools_Rtti>);
  Assert(not TRttiUtils.IsInstance<integer>);
  Assert(not TRttiUtils.IsInstance<pointer>);
  Assert(not TRttiUtils.IsInstance<IUnknown>);
  Assert(not TRttiUtils.IsInstance<TRect>);
  Assert(TRttiUtils.ValueAsString<integer>(-10)='-10');
  p := TRttiUtils.CreateInstance<T1>;
  assert(p.v=1);
  FreeAndNil(p);
  p := TRttiUtils.CreateInstance<T2>;
  assert(p.v=2);
  FreeAndNil(p);
  Assert(TEnumeration<TEn>.ToString(en1)='en1');
  Assert(TEnumeration<TEn>.ToString(en2)='en2');
  Assert(TEnumeration<TEn>.ToString(en3)='en3');
  Assert(TEnumeration<TEn>.FromString('en1')=en1);
  Assert(TEnumeration<TEn>.FromString('en2')=en2);
  Assert(TEnumeration<TEn>.FromString('en3')=en3);
  Assert(TEnumeration<TEn>.MinValue=integer(en1));
  Assert(TEnumeration<TEn>.MaxValue=integer(en3));
end;

{ TTest_adot_Variants }

class procedure TTest_adot_Variants.Test_TVar;
var
  v: variant;
  s: string;
  i: integer;
begin
  v := VarArrayCreate([0,2], varInteger);
  v[0] := 1;
  v[1] := 2;
  v[2] := 3;
  s := TVar.VarOrArrayToStr(v, ' ');
  assert(s='1 2 3');
  assert(not TVar.TryToInteger(Unassigned, i));
  assert(not TVar.TryToInteger(Null, i));
  assert(TVar.TryToInteger(5, i) and (i=5));
end;

class procedure TTest_adot_Variants.Run;
begin
  Test_TVar;
end;

procedure RunTestcases;
begin
  TTest_adot_Collections.Run;
  TTest_adot_Strings.Run;
  TTest_adot_Tools.Run;
  TTest_adot_Tools_Rtti.Run;
  TTest_adot_Variants.Run;
end;

initialization
  RunTestcases;

end.
