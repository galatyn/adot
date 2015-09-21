unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CrossPlatform.PEG.Test,
  CrossPlatform.Log, VCL.Log, CrossPlatform.Tools.Test;

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
  //CrossPlatform.PEG.Test.RunTestSet;
  CrossPlatform.Tools.Test.RunTestSet;
end;

end.
