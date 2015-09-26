program TestAdot;

uses
  Vcl.Forms,
  MainFrm in 'MainFrm.pas' {FormMain},
  CrossPlatform.PEG.Test in 'CrossPlatform.PEG.Test.pas',
  CrossPlatform.Tools.Test in 'CrossPlatform.Tools.Test.pas',
  CrossPlatform.Log in '..\CrossPlatform.Log.pas',
  VCL.Log in '..\VCL.Log.pas',
  CrossPlatform.Tools in '..\CrossPlatform.Tools.pas',
  CrossPlatform.Generics.Collections in '..\CrossPlatform.Generics.Collections.pas',
  CrossPlatform.Generics.Containers.Test in 'CrossPlatform.Generics.Containers.Test.pas',
  CrossPlatform.Strings in '..\CrossPlatform.Strings.pas',
  CrossPlatform.Strings.Test in 'CrossPlatform.Strings.Test.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
