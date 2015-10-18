unit MainFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, adot.PEG.Test,
  adot.Log, adot.VCL.Log, adot.Tools.Test,
  adot.Generics.Containers.Test, adot.Strings.Test;

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
  Top := Screen.WorkAreaTop;
  Height := Screen.WorkAreaHeight;
  Left := Screen.WorkAreaWidth-Width;

  AppLog := TMixLog.Create([
    TVCLStringsLog.Create(MemoLog.Lines),
    TSyncFileLog.Create(ChangeFileExt(ParamStr(0), '.log'))
  ]);
  adot.Tools.Test.Run;
  adot.Generics.Containers.Test.Run;
  adot.Strings.Test.Run;
  adot.PEG.Test.Run;
  //adot.dip.test.TTestCases.Run;
end;

end.
