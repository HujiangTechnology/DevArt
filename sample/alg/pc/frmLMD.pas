unit frmLMD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, Dialogs, NativeAlgorithm;

type

  { TFormLMD }

  TFormLMD = class(TForm)
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
  FormLMD: TFormLMD;

implementation

{$R *.lfm}

{ TFormLMD }

procedure TFormLMD.btnGoClick(Sender: TObject);
var
  ori: string;
  enc: string;
begin
  ori := etSrc.Text;
  enc := string(mLmdEncryptString(PChar(ori)));
  lbDest.Caption:= enc;
end;

initialization
  RegisterClass(TFormLMD);

end.

