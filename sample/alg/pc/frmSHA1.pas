unit frmSHA1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm, LCLType, LCLIntf, LMessages, Messages, unt_msgconst;

type

  { TThreadSHA1 }

  TThreadSHA1 = class(TThread)
  private
    FEncryptString: string;
    FHandle: HWND;
    FOri: string;
    procedure SendUI;
    procedure SendBtn;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string);
  public
    property EncryptString: string read FEncryptString write FEncryptString;
  end;

  { TFormSHA1 }

  TFormSHA1 = class(TForm)
    btnGo: TButton;
    etSrc: TEdit;
    lbDest: TLabel;
    procedure btnGoClick(Sender: TObject);
  private
    FEncrypt: TThreadSHA1;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormSHA1: TFormSHA1;

implementation

{$R *.lfm}

{ TThreadSHA1 }

procedure TThreadSHA1.SendUI;
begin
  SendMessage(FHandle, LM_MSG, MSG_UI, 0);
end;

procedure TThreadSHA1.SendBtn;
begin
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

procedure TThreadSHA1.Execute;
begin
  FEncryptString:= string(mSha1EncryptString(PChar(FOri)));
  Synchronize(@SendUI);
  Synchronize(@SendBtn);
end;

constructor TThreadSHA1.Create(AHandle: HWND; AOri: string);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FreeOnTerminate:= True;
end;

procedure TFormSHA1.btnGoClick(Sender: TObject);
begin
  btnGo.Enabled:= False;
  FEncrypt := TThreadSHA1.Create(self.Handle, etSrc.Text);
  FEncrypt.Start;
end;

procedure TFormSHA1.OnHandleMessage(var msg: TMessage);
begin
  case msg.WParam of
  MSG_BTN: btnGo.Enabled:= True;
  MSG_UI: lbDest.Caption:= FEncrypt.EncryptString;
  end;
end;

initialization
  RegisterClass(TFormSHA1);

end.

