unit frmSHA1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm;

type

  { TFormSHA1 }

  TFormSHA1 = class(TForm)
    btnGo: TButton;
    etSrc: TEdit;
    lbDest: TLabel;
    procedure btnGoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormSHA1: TFormSHA1;

implementation

{$R *.lfm}

procedure TFormSHA1.btnGoClick(Sender: TObject);
var
  ori: string;
  enc: string;
begin
  ori := etSrc.Text;
  enc := string(mSha1EncryptString(PChar(ori)));
  lbDest.Caption:= enc;
end;

initialization
  RegisterClass(TFormSHA1);

end.

