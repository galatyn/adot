unit CrossPlatform.Generics.Containers.Test;

interface

uses
  CrossPlatform.Generics.Collections, System.SysUtils, System.Classes,
  CrossPlatform.Tools;

type
  TTests = class
  private
    class procedure Test_Multimap; static;
    class procedure Test_Heap; static;
  end;

procedure Run;

implementation

uses
  CrossPlatform.Strings, System.Generics.Collections, System.Generics.Defaults;

procedure Run;
begin
  TTests.Test_Multimap;
  TTests.Test_Heap;
end;

{ TTests }

class procedure TTests.Test_Multimap;
var
  m: TMultimap<string, integer>;
  e: TMultimap<string, integer>.TValueEnumerator;
  s: TSet<integer>;
  Key: string;
  i: Integer;
  p: TMultimap<string, integer>.TPair;
begin
  m := nil;
  s := nil;
  try
    m := TMultimap<string, integer>.Create;
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
    Assert(not m.ContainsKeys(['1.1', '2', '3', '', '4']));

    i := 0;
    for p in m do
      i := i + p.Value;
    assert(i=56);

    e := m.Values['1.1'];
    s := TSet<integer>.Create([11,12,13]);
    try
      Assert(e.MoveNext and s.Contains(e.Current));
      Assert(e.MoveNext and s.Contains(e.Current));
      Assert(e.MoveNext and s.Contains(e.Current));
      Assert(not e.MoveNext);

      Assert(s.Count=3);
      for i in s do
        Assert(i in [11,12,13]);
    finally
      FreeAndNil(s);
    end;

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
    FreeAndNil(s);
    FreeAndNil(m);
  end;
end;

class procedure TTests.Test_Heap;
const
  TotalAdd = 10*1000;
  AvgInHeap = TotalAdd div 100;
var

  h: THeap<string>;
  l: TStringList;
  rep: TStringList;
  MinTestSize: integer;
  i,j,m,n: Integer;
  s: string;

  procedure CheckHeap;
  var i: integer;
  begin
    for i := 0 to h.Count-1 do
      Assert(h[i]>=h.MinValue);
  end;

  procedure CheckHeapStrong;
  var
    i: integer;
    s: TStringList;
  begin
    CheckHeap;
    s := TStringList.Create;
    try
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
  MinTestSize := high(MinTestSize);
  repeat
    h := nil;
    l := nil;
    rep := nil;
    try
      rep := TStringList.Create;
      h := THeap<string>.Create(TDelegatedComparer<string>.Create(
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
          CheckHeapStrong;
          Assert(l.Count=h.Count);
          s := RandomString(3);
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

end.
