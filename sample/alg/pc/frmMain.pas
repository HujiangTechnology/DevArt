unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    lstAlg: TListBox;
    procedure lstAlgDblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.lstAlgDblClick(Sender: TObject);
var
  n: string;
  frm: TFormClass;
begin
  n := lstAlg.Items[lstAlg.ItemIndex];
  frm := TFormClass(FindClass('TForm' + n));
  with frm.Create(nil) do begin
    ShowModal;
    Free;
  end;
end;

end.

