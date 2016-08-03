unit frmMD5;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm, LCLType, LCLIntf, LMessages, Messages, unt_msgconst;

type

  { TThreadMD5 }

  TThreadMD5 = class(TThread)
  private
    FEncryptString: string;
    FHandle: HWND;
    FOri: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string);
  public
    property EncryptString: string read FEncryptString write FEncryptString;
  end;

  { TFormMD5 }

  TFormMD5 = class(TForm)
    btnGo: TButton;
    etSrc: TEdit;
    lbDest: TLabel;
    procedure btnGoClick(Sender: TObject);
  private
    FEncrypt: TThreadMD5;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormMD5: TFormMD5;

implementation

{$R *.lfm}

{ TThreadMD5 }

procedure TThreadMD5.Execute;
begin
  FEncryptString:= string(mMd5EncryptString(PChar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 0);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

constructor TThreadMD5.Create(AHandle: HWND; AOri: string);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FreeOnTerminate:= True;
end;

{ TFormMD5 }

procedure TFormMD5.btnGoClick(Sender: TObject);
begin
  btnGo.Enabled:= False;
  FEncrypt := TThreadMD5.Create(self.Handle, etSrc.Text);
  FEncrypt.Start;
end;

procedure TFormMD5.OnHandleMessage(var msg: TMessage);
begin
  case msg.WParam of
  MSG_UI: lbDest.Caption:= FEncrypt.EncryptString;
  MSG_BTN: btnGo.Enabled:= True;
  end;
end;

initialization
  RegisterClass(TFormMD5);

end.

