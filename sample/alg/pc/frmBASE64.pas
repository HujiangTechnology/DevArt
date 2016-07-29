unit frmBASE64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm;

type

  { TFormBASE64 }

  TFormBASE64 = class(TForm)
    btnEncGo: TButton;
    btnDecGo: TButton;
    etEncSrc: TEdit;
    etDecSrc: TEdit;
    lbEncDest: TLabel;
    lbDecTitle: TLabel;
    lbDecDest: TLabel;
    lblEncTitle: TLabel;
    procedure btnDecGoClick(Sender: TObject);
    procedure btnEncGoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormBASE64: TFormBASE64;

implementation

{$R *.lfm}

{ TFormBASE64 }

procedure TFormBASE64.btnEncGoClick(Sender: TObject);
var
  ori: string;
  enc: string;
begin
  ori := etEncSrc.Text;
  enc := string(mBase64EncryptString(PChar(ori)));
  lbEncDest.Caption:= enc;
  etDecSrc.Text:= enc;
end;

procedure TFormBASE64.btnDecGoClick(Sender: TObject);
var
  ori: string;
  dec: string;
begin
  ori := etDecSrc.Text;
  dec:= string(mBase64DecryptString(PChar(ori)));
  lbDecDest.Caption:= dec;
end;

initialization
  RegisterClass(TFormBASE64);

end.

