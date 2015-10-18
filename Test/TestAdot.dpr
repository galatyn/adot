program TestAdot;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {FormMain},
  adot.PEG.Test in 'adot.PEG.Test.pas',
  adot.Tools.Test in 'adot.Tools.Test.pas',
  adot.Log in '..\adot.Log.pas',
  adot.VCL.Log in '..\adot.VCL.Log.pas',
  adot.Tools in '..\adot.Tools.pas',
  adot.Generics.Collections in '..\adot.Generics.Collections.pas',
  adot.Generics.Containers.Test in 'adot.Generics.Containers.Test.pas',
  adot.Strings in '..\adot.Strings.pas',
  adot.Strings.Test in 'adot.Strings.Test.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
