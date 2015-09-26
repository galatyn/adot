unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CrossPlatform.PEG.Test,
  CrossPlatform.Log, VCL.Log, CrossPlatform.Tools.Test,
  CrossPlatform.Containers.Test, CrossPlatform.Strings.Test;

type
  TFormMain = class(TForm)
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  AppLog := TMixLog.Create([
    TVCLStringsLog.Create(MemoLog.Lines),
    TSyncFileLog.Create(ChangeFileExt(ParamStr(0), '.log'))
  ]);
  CrossPlatform.Tools.Test.Run;
  CrossPlatform.Containers.Test.Run;
  CrossPlatform.Strings.Test.Run;
  CrossPlatform.PEG.Test.Run;
end;

end.
