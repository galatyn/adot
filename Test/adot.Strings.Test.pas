unit adot.Strings.Test;

interface

type
  TTests = class
  private
    class procedure Test_TextWords; static;
  end;

procedure Run;

implementation

procedure Run;
begin
  TTests.Test_TextWords;
end;

{ TTests }

class procedure TTests.Test_TextWords;
begin

end;

end.
