unit CrossPlatform.Tools_Test;

interface

uses
  CrossPlatform.Tools, System.SysUtils;

type
  TTests = class
  public
    class procedure QuickTest; static;
  end;

implementation

{ TTests }

class procedure TTests.QuickTest;
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
{  with TSyncLog.Create(ChangeFileExt(PAramStr(0), '.log')) do
    try
      Log('test 1');
      Flush;
      Log('test 2');
      Log('test 3');
    finally
      Free;
    end;
  with TLog.Create(ChangeFileExt(PAramStr(0), '.log')) do
    try
      Log('test 1');
      Flush;
      sleep(7000);
      Log('test 2');
      sleep(20000);
      Log('test 3');
    finally
      Free;
    end;}
end;

end.
