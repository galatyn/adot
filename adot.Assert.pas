unit adot.Assert;

{$If Defined(DEBUG)}
  {$Define HookAssert}
{$EndIf}

interface

{$If Defined(HookAssert)}
uses
  Winapi.Windows;

procedure Assert(Condition: Boolean; const Message: String = '');
{$EndIf}

implementation

{$If Defined(HookAssert)}
procedure Assert(Condition: Boolean; const Message: String = '');
begin
  {$If Defined(MSWindows)}
    { At Windows platform we set breakpoint here automatically and then we continue
      execution (to be able to inspect all conditions leading to the assertion) }
    if not Condition then
      DebugBreak;
  {$Else}
    { For non-Windows platform break point can be set here
      manually to skip System.Assert and continue execution }
    if not Condition then
      System.Assert(Condition, Message);
  {$EndIf}
end;
{$EndIf}

end.
