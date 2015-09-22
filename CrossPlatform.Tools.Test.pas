unit CrossPlatform.Tools.Test;

interface

uses
  CrossPlatform.Tools, System.SysUtils, System.Generics.Collections,
  System.Generics.Defaults;

type
  TTests = class
  private
    class procedure Test_THex; static;
    class procedure Test_Multimap; static;
  public
    class procedure QuickTest; static;
  end;

procedure RunTestSet;

implementation

{ TTests }

class procedure TTests.Test_THex;
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

class procedure TTests.Test_Multimap;
var
  m: TMultimap<string, integer>;
  e: TMultimap<string, integer>.TValueEnumerator;
  s: TSet<integer>;
  Key: string;
  i: Integer;
  p: TPair<string, integer>;
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

class procedure TTests.QuickTest;
begin
  Test_THex;
  Test_Multimap;
end;

procedure RunTestSet;
begin
  TTests.QuickTest;
end;

end.
