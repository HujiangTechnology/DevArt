unit frmMD5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm;

type

  { TFormMD5 }

  TFormMD5 = class(TForm)
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
  FormMD5: TFormMD5;

implementation

{$R *.lfm}

{ TFormMD5 }

procedure TFormMD5.btnGoClick(Sender: TObject);
var
  ori: string;
  enc: string;
begin
  ori := etSrc.Text;
  enc := string(mMd5EncryptString(PChar(ori)));
  lbDest.Caption:= enc;
end;

initialization
  RegisterClass(TFormMD5);

end.

