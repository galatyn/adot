unit adot.Tools.Test;

interface

uses
  adot.Tools, System.SysUtils, System.Generics.Collections,
  System.Generics.Defaults, adot.Generics.Collections;

type
  TTests = class
  private
    class procedure Test_THex; static;
    class procedure Test_TNullable; static;
  end;

procedure Run;

implementation

procedure Run;
begin
  TTests.Test_THex;
  TTests.Test_TNullable;
end;

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

class procedure TTests.Test_TNullable;
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

end.
