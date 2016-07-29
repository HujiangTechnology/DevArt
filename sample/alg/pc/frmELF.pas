unit frmELF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm;

type

  { TFormELF }

  TFormELF = class(TForm)
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
  FormELF: TFormELF;

implementation

{$R *.lfm}

{ TFormELF }

procedure TFormELF.btnGoClick(Sender: TObject);
var
  ori: string;
  enc: string;
begin
  ori := etSrc.Text;
  enc := string(mElfEncryptString(PChar(ori)));
  lbDest.Caption:= enc;
end;

initialization
  RegisterClass(TFormELF);

end.

