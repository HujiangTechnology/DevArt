unit frmAES;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm;

type

  { TFormAES }

  TFormAES = class(TForm)
    btnEncGo: TButton;
    btnDecGo: TButton;
    etEncSrc: TEdit;
    etEncKey: TEdit;
    etDecSrc: TEdit;
    etDecKey: TEdit;
    lbEncTitle: TLabel;
    lbEncDest: TLabel;
    lbDecTitle: TLabel;
    lbDecDest: TLabel;
    procedure btnDecGoClick(Sender: TObject);
    procedure btnEncGoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormAES: TFormAES;

implementation

{$R *.lfm}

{ TFormAES }

procedure TFormAES.btnDecGoClick(Sender: TObject);
var
  ori: string;
  key: string;
  dec: string;
begin
  ori := etDecSrc.Text;
  key := etDecKey.Text;
  dec := string(mAesDecryptECB128(PChar(key), PChar(ori)));
  lbDecDest.Caption:= dec;
end;

procedure TFormAES.btnEncGoClick(Sender: TObject);
var
  ori: string;
  key: string;
  enc: string;
begin
  ori := etEncSrc.Text;
  key := etEncKey.Text;
  enc:= string(mAesEncryptECB128(PChar(key), PChar(ori)));
  lbEncDest.Caption:= enc;
  etDecKey.Text:= key;
  etDecSrc.Text:= enc;
end;

initialization
  RegisterClass(TFormAES);

end.

