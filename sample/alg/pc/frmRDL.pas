unit frmRDL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm;

type

  { TFormRDL }

  TFormRDL = class(TForm)
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
  FormRDL: TFormRDL;

implementation

{$R *.lfm}

{ TFormRDL }

procedure TFormRDL.btnEncGoClick(Sender: TObject);
var
  ori: string;
  key: string;
  enc: string;
begin
  ori := etEncSrc.Text;
  key := etEncKey.Text;
  enc:= string(mRdlEncryptString(0, 1, PChar(key), PChar(ori)));
  lbEncDest.Caption:= enc;
  etDecKey.Text:= key;
  etDecSrc.Text:= enc;
end;

procedure TFormRDL.btnDecGoClick(Sender: TObject);
var
  ori: string;
  key: string;
  dec: string;
begin
  ori := etDecSrc.Text;
  key := etDecKey.Text;
  dec := string(mRdlDecryptString(0, 1, PChar(key), PChar(ori)));
  lbDecDest.Caption:= dec;
end;

initialization
  RegisterClass(TFormRDL);

end.

