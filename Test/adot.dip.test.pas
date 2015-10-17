unit adot.dip.test;

interface

uses
  adot.dip, System.SysUtils, System.Classes, System.Math,
  System.JSON, System.JSONConsts, Xml.XMLDoc, Xml.XMLIntf;

type
  TTestCases = class
  protected
    const EE = 0.0000001;
    class procedure TestIntValue(Value: int64); static;
    class procedure TestIntRange(Value: int64; RangeEveryDirection: int64 = 1000); static;
  private
    class procedure TestValue(Src: TDIPValue); static;
    class procedure FillMap(m: TDIPDictionary; maxdepth: integer); static;
    class procedure TestArray; static;
    class procedure FillArray(m: TDIPArray; maxdepth: integer); static;
    class procedure TestJSON; static;
    class procedure TestXML; static;
  protected
    class procedure TestInt; static;
    class procedure TestMap; static;
  public
    class procedure Run; static;
    class procedure QuickTest; static;
  end;

implementation

class procedure TTestCases.TestIntValue(Value: int64);
var
  m: TMemoryStream;
  v: TDIPInteger;
begin
  m := nil;
  v := nil;
  try
    m := TMemoryStream.Create;
    v := TDIPInteger.Create(Value);
    v.SaveToStream(m);
    FreeAndNil(v);

    m.Position := 0;
    v := v.LoadFromStream(m) as TDIPInteger;
    Assert(v.Value=Value);
  finally
    FreeandNil(v);
    FreeandNil(m);
  end;
end;

class procedure TTestCases.TestIntRange(Value: int64; RangeEveryDirection: int64 = 1000);
var
  i: Integer;
begin
  Dec(Value, RangeEveryDirection);
  for i := 0 to RangeEveryDirection*2 do
  begin
    TestIntValue(Value);
    inc(Value);
  end;
end;

class procedure TTestCases.TestInt;
begin
  TestIntValue(0);
  TestIntValue(1);
  TestIntValue(-1);
  TestIntValue(High(Int64)-1);
  TestIntValue(High(Int64));
  TestIntValue(Low(Int64)+1);
  TestIntValue(Low(Int64));
  TestIntRange(0);
  TestIntRange(65535+255+130);
  TestIntRange(int64(high(longword))+65535+255+131);
  TestIntRange(-(65535+257));
  TestIntRange(-(int64(high(Longword))+65535+257));
  TestIntRange(int64($0000000000000000));
  TestIntRange(int64($00000000000000FF));
  TestIntRange(int64($000000000000FFFF));
  TestIntRange(int64($0000000000FFFFFF));
  TestIntRange(int64($00000000FFFFFFFF));
  TestIntRange(int64($000000FFFFFFFFFF));
  TestIntRange(int64($0000FFFFFFFFFFFF));
  TestIntRange(int64($00FFFFFFFFFFFFFF));
  TestIntRange(int64($FFFFFFFFFFFFFFFF));
  TestIntRange(-int64($0000000000000000));
  TestIntRange(-int64($00000000000000FF));
  TestIntRange(-int64($000000000000FFFF));
  TestIntRange(-int64($0000000000FFFFFF));
  TestIntRange(-int64($00000000FFFFFFFF));
  TestIntRange(-int64($000000FFFFFFFFFF));
  TestIntRange(-int64($0000FFFFFFFFFFFF));
  TestIntRange(-int64($00FFFFFFFFFFFFFF));
  TestIntRange(-int64($FFFFFFFFFFFFFFFF));
end;

class procedure TTestCases.Run;
begin
  TestInt;
  TestArray;
  TestMap;
  TestJSON;
  TestXML;
end;

class procedure TTestCases.QuickTest;
begin
  TestInt;
  TestArray;
  TestMap;
end;

class procedure TTestCases.TestValue(Src: TDIPValue);
var
  m: TMemoryStream;
  Dst: TDIPValue;
begin
  Assert(Src.IsConsistent);
  m := TMemoryStream.Create;
  try
    src.SaveToStream(m);
    m.Position := 0;
    dst := src.LoadFromStream(m);
    try
      Assert(Dst.IsConsistent);
      Assert(m.Position=m.Size);
      Assert(Src.EqualTo(Dst));
    finally
      FreeAndNil(dst);
    end;
  finally
    FreeAndNil(m);
  end;
end;

class procedure TTestCases.FillMap(m: TDIPDictionary; maxdepth: integer);
var
  b: TBytes;
  n: TDIPDictionary;
  a: TDIPArray;
  i,j: Integer;
begin
  m.AddOrSetInteger('int', 1000);
  Assert(m['int'].AsInteger=1000);

  m.AddOrSetSingle('pi', 3.1415926);
  Assert(abs(m['pi'].AsSingle-3.1415926)<EE);

  m.AddOrSetDouble('pi100', 3.1415926/10);
  Assert(abs(m['pi100'].AsDouble-3.1415926/10)<EE);

  m.AddOrSetCurrency('cur', 200.40);
  Assert(abs(m['cur'].AsCurrency-200.40)<EE);

  m.AddOrSetDateTime('time', Now);
  m.AddOrSetString('str', 'øæ some текст');
  m.AddOrSetBoolean('b', True);
  setlength(b, 3);
  b[0] := 12; b[1] := 9; b[2] := 7;
  m.AddOrSetBytes('bin', b);
  Assert(m.IndexOf('pi')=1);
  m.Exchange(m.IndexOf('pi'), m.IndexOf('cur'));
  Assert(m.IndexOf('pi')=3);
  Assert(m.IndexOf('cur')=1);
  Assert(m.IndexOf('pi100')=2);
  i := m.Count;
  m.KeyByIndex[1] := m.AsKey('cur2');
  m.ReplaceKey('cur2', 'cur777');
  Assert(abs(m['cur777'].AsCurrency-200.40)<EE);
  Assert(i = m.Count);

  for i := 0 to 100 do
    m.AddOrSetInteger('int'+inttostr(random(100)), random(high(integer))-2*random(high(integer)));
  if maxdepth>0 then
    for i := 0 to 3 do
    begin

      n := TDIPDictionary.Create;
      j := random(5);
      try
        m.AddOrSetValue('map'+inttostr(j), n);
      except
        m.AddOrSetValue('map'+inttostr(j), n);
      end;
      FillMap(n, maxdepth-1);

      a := TDIPArray.Create;
      FillArray(a, maxdepth-1);
      j := random(5);
      try
        m.AddOrSetValue('array'+inttostr(j), a);
      except
        m.AddOrSetValue('array'+inttostr(j), a);
      end;

    end;

end;

class procedure TTestCases.FillArray(m: TDIPArray; maxdepth: integer);
var
  b: TBytes;
  n: TDIPArray;
  i: Integer;
begin
  m.Add(1000);
  Assert(m[0].AsInteger=1000);
  m.Add(TDIPSingle.Create(3.1415926));
  Assert(abs(m[1].AsSingle-3.1415926)<EE);
  m.Add(3.1415926/10);
  Assert(abs(m[2].AsDouble-3.1415926/10)<EE);
  m.Add(TDIPCurrency.Create(200.40));
  Assert(abs(m[3].AsCurrency-200.40)<EE);
  m.Add(TDIPDateTime.Create(Now));
  m.Add('øæ some текст');
  Assert(m[5].AsUnicodeString='øæ some текст');
  m.Add(True);
  Assert(m[6].AsBoolean=True);
  setlength(b, 3);
  b[0] := 12; b[1] := 9; b[2] := 7;
  m.Add(b);
  m.Exchange(0,6);
  Assert(m[0].AsBoolean=True);

  for i := 0 to 100 do
    m.Add(random(high(integer))-2*random(high(integer)));
  if maxdepth>0 then
    for i := 0 to 2 do
    begin
      n := TDIPArray.Create;
      m.Add(n);
      FillArray(n, maxdepth-1);
    end;
end;

class procedure TTestCases.TestMap;
var
  m,m2: TDIPDictionary;
begin
  m := nil;
  try
    m := TDIPDictionary.Create;
    TestValue(m);
    m.Clear;
    FillMap(m, 5);
    m2 := TDIPDictionary.Create;
    FillMap(m2, 5);
    m.AddOrSetValue('submap', m2);
    TestValue(m);
  finally
    FreeandNil(m);
  end;
end;

class procedure TTestCases.TestArray;
var
  m,m2: TDIPArray;
  b: TBytes;
begin
  m := nil;
  try
    m := TDIPArray.Create;
    TestValue(m);
    m.AddInteger(0);
    TestValue(m);
    assert(m[0].AsInteger=0);
    m[0].AsInteger := 100;
    assert(m[0].AsInteger=100);
    m[0].AsInteger := 200;
    assert(m[0].AsInteger=200);
    m.AddString('øæ');
    assert(m[1].AsUnicodeString='øæ');
    m.AddSingle(12);
    assert(m[2].AsSingle=12);
    m.AddDouble(13);
    assert(m[3].AsSingle=13);
    m.AddCurrency(14);
    assert(m[4].AsSingle=14);
    assert(m[4].AsDouble=14);
    assert(m[4].AsCurrency=14);
    m.AddDateTime(15.5);
    assert(m[5].AsDateTime=15.5);
    m.AddString('я есть Грут!');
    assert(m[6].AsString='я есть Грут!');
    m.AddBoolean(True);
    assert(m[7].AsBoolean);
    m.AddBoolean(False);
    assert(not m[8].AsBoolean);
    setlength(b, 3); b[0] := 10; b[1] := 20; b[2] := 30;
    m.AddBytes(b);
    assert(m[9].AsBytes[0]=10);
    assert(m[9].AsBytes[1]=20);
    assert(m[9].AsBytes[2]=30);
    m.Add(TDIPInteger.Create(1));
    assert(m[10].AsInteger=1);
    m.Add(TDIPInteger.Create(high(int64)));
    assert(m[11].AsInteger=high(int64));
    assert(m[11].AsInt64=high(int64));
    m.Add(TDIPSingle.Create(100.2));
    assert(SameValue(m[12].AsSingle, 100.2, 1e-5));
    m.Add(TDIPDouble.Create(100.3));
    assert(SameValue(m[13].AsDouble, 100.3, 1e-5));
    m.Add(TDIPCurrency.Create(100.4));
    assert(SameValue(m[14].AsCurrency, 100.4, 1e-5));
    m.Add(TDIPDateTime.Create(100.5));
    assert(SameValue(m[15].AsDateTime, 100.5, 1e-5));
    TestValue(m);
    m.Clear;
    m.AddInteger(10);
    m.AddInteger(-5);
    m.AddInteger(2);
    m.Sort(function(const a,b: TDIPValue): integer
    begin
      result := a.AsInteger-b.AsInteger;
    end);
    assert(m[0].AsInteger=-5);
    assert(m[1].AsInteger=2);
    assert(m[2].AsInteger=10);
    m.Values[1] := TDIPString.Create('qwqwq');
    assert(m[1].AsString='qwqwq');
    TestValue(m);


    m.Clear;
    FillArray(m, 5);
    m2 := TDIPArray.Create;
    FillArray(m2, 5);
    m.Add(m2);
    TestValue(m);
  finally
    FreeandNil(m);
  end;
end;

class procedure TTestCases.TestJSON;
var
  j: TJSONValue;
  s: string;
  m: TDIPDictionary;
  t: TMemoryStream;
  UTF8Str: TUTF8String;
  dip: TDIPValue;
  v: TDIPValue;
begin
  m := TDIPDictionary.Create;
  try
    FillMap(m, 2);
    m.SaveToFile(changefileext(paramstr(0), '.json'));
    m.SaveToFile(changefileext(paramstr(0), '.dip'));
  finally
    FreeAndNil(m);
  end;

  v := TDIPValue.LoadFromFile('d:\DB\JSON.examples\AllSets-x.json');
  try
    v.SaveToFile('d:\DB\JSON.examples\AllSets-x.dip');
  finally
    FreeAndNil(v);
  end;

  t := TMemoryStream.Create;
  try
    // load test JSON
    t.LoadFromFile(ExtractFilePath(paramstr(0))+'..\..\..\src\demo\speed-test.json');
    t.Position := 0;
    setlength(UTF8Str, t.Size);
    t.Read(UTF8Str[Low(UTF8Str)], Length(UTF8Str));
    s := UTF8ToString(UTF8Str);

    // load JSON from string (parsing in memory)
    j := TJSONObject.ParseJSONValue(s);
    try

      // JSON -> dip
      dip := TDIPValue.LoadFromJSON(j);
      try
        t.Clear;
        dip.SaveToStream(t);
        t.SaveToFile(ExtractFilePath(paramstr(0))+'..\..\..\src\demo\speed-test.dip');
      finally
        FreeAndNil(dip);
      end;

    finally
      FreeandNil(j);
    end;

    // load dip from memory stream (parsing in memory)
    t.Position := 0;
    dip := TDIPValue.LoadFromStream(t);
    try
      {MessageDlg(format(
        'Loading speed-test.json'#13#10+
        'JSON: %d ms'#13#10+
        'dip: %d ms (%.1f times faster)',
        [t1, t2, t1/max(1,t2)]), mtInformation, [mbOK], 0);}
    finally
      FreeAndNil(dip);
    end;

  finally
    FreeandNil(t);
  end;

{  j := TJSONArray.Create;
  try
    (j as TJSONArray).Add(100);
    (j as TJSONArray).Add('øæ');
    s := (j as TJSONArray).ToString;
  finally
    FreeAndNil(j);
  end;}

end;

class procedure TTestCases.TestXML;
const
  xml: array[1..4] of string = (
    //'<?xml version="1.0" encoding="UTF-8"?>',

    '<rss version="2.0">'#13#10+
	  '    <title>RAD Studio 入門</title>'#13#10+
    '</rss>',

    '<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">'#13#10+
    '  <Import Condition="Exists(''$(BDS)\bin\CodeGear.Deployment.targets'')" Project="$(BDS)\bin\CodeGear.Deployment.targets"/>'#13#10+
    '</Project>',

    '<rss version="2.0">'#13#10+
    '  text 1'#13#10+
    '	<title>RAD</title>'#13#10+
    '  text 2'#13#10+
    '	<title>Studio</title>'#13#10+
    '</rss>',

    '<script>'#13#10+
    '<![CDATA['#13#10+
    'function matchwo(a,b)'#13#10+
    '{'#13#10+
    'if (a < b && a < 0) then'#13#10+
    '  {'#13#10+
    '    return 1;'#13#10+
    '  }'#13#10+
    'else'#13#10+
    '  {'#13#10+
    '    return 0;'#13#10+
    '  }'#13#10+
    '}'#13#10+
    ']]>'#13#10+
    '</script>'
  );

var
  d: TDIPArray;
  i: Integer;
begin

  for i := Low(xml) to High(xml) do
  begin
    d := TDIPValue.LoadFromXML(xml[i]);
    try
      d.SaveToFile(format('e:\Work\SimpleBinaryProtocol\src\demo\test.xml.%d.dip', [i]));
      d.SaveToFile(format('e:\Work\SimpleBinaryProtocol\src\demo\test.xml.%d.json', [i]));
    finally
      FreeAndNil(d);
    end;
  end;

{  d := TDIPValue.LoadFromXMLFile('e:\Work\SimpleBinaryProtocol\src\demo\test1.xml');
  try
    d.SaveToFile('e:\Work\SimpleBinaryProtocol\src\demo\test1.dip');
    d.SaveToFile('e:\Work\SimpleBinaryProtocol\src\demo\test1.json');
  finally
    FreeAndNil(d);
  end;}

{  d := TDIPValue.LoadFromXMLFile('c:\E\Work\SimpleBinaryProtocol\src\demo\EPAXMLDownload.xml');
  try
    d.SaveToFile('c:\E\Work\SimpleBinaryProtocol\src\demo\EPAXMLDownload.dip');
    d.SaveToFile('c:\E\Work\SimpleBinaryProtocol\src\demo\EPAXMLDownload.json');
  finally
    FreeAndNil(d);
  end;}
end;

end.
