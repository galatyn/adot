unit adot.DUnit.Runner;

{ CLR is not supported }
{$IF Defined(CLR)}
  {$MESSAGE ERROR 'DotNet is not supported by DUnit framework' }
{$ENDIF}

{ non Windows and NextGen compilers are supported for console mode only }
{$IF not Defined(MSWINDOWS) or Defined(NEXTGEN)}
  {$IF not Defined(CONSOLE_TESTRUNNER)}
    {$MESSAGE ERROR 'Only VCL is supported in graphical UI mode' }
  {$ENDIF}
{$ENDIF}

interface

uses
  DUnitConsts,
  TestFramework,
  {$IF Defined(CONSOLE_TESTRUNNER)}
    TextTestRunner,
  {$ELSE}
    GuiTestRunner,
    Vcl.Forms,
  {$ENDIF}
  System.SysUtils;

type
  { Default runner from DUnitTestRunner.pas does not provide access to results, we create custom runner
    to see tests results (.TestResult is available after .Run call in console mode) }
  TUnitTestingRunner = class
  private
    FTestResult: TTestResult;

  public
    destructor Destroy; override;
    procedure Run(AExitBehavior: TRunnerExitBehavior = rxbContinue);

    property TestResult: TTestResult read FTestResult;
  end;

implementation

{ TUnitTestingRunner }

destructor TUnitTestingRunner.Destroy;
begin
  FreeAndNil(FTestResult);
  inherited;
end;

procedure TUnitTestingRunner.Run(AExitBehavior: TRunnerExitBehavior = rxbContinue);
begin
  FreeAndNil(FTestResult);

  {$IF Defined(CONSOLE_TESTRUNNER)}

    { console mode }
    FTestResult := TestFramework.RunTest(registeredTests, [TTextTestListener.Create]);
    {$IF not Defined(NEXTGEN)}
      case AExitBehavior of
        rxbPause:
          try
            writeln(sPressReturn);
            readln;
          except
          end;
        rxbHaltOnFailures:
          if not FTestResult.WasSuccessful then
            System.Halt(FTestResult.ErrorCount+FTestResult.FailureCount);
      end;
    {$ENDIF NEXTGEN}

  {$ELSE}

    { graphical mode, show modal form with test results etc }
    Application.Initialize;
    GuiTestRunner.RunRegisteredTests;

  {$ENDIF}
end;

end.
