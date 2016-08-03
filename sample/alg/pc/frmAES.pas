unit frmAES;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm, unt_msgconst, LCLType, LCLIntf, LMessages, Messages;

type

  { TThreadEncrypt }

  TThreadEncrypt = class(TThread)
  private
    FEncryptString: string;
    FHandle: HWND;
    FOri: string;
    FKey: string;
    procedure SendUI;
    procedure SendBtn;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string; AKey: string);
  public
    property EncryptString: string read FEncryptString write FEncryptString;
  end;

  { TThreadDecrypt }

  TThreadDecrypt = class(TThread)
  private
    FDecryptString: string;
    FHandle: HWND;
    FOri: string;
    FKey: string;
    procedure SendUI;
    procedure SendBtn;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string; AKey: string);
  public
    property DecryptString: string read FDecryptString write FDecryptString;
  end;

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
    FEncrypt: TThreadEncrypt;
    FDecrypt: TThreadDecrypt;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormAES: TFormAES;

implementation

{$R *.lfm}

{ TThreadDecrypt }

procedure TThreadDecrypt.SendUI;
begin
  SendMessage(FHandle, LM_MSG, MSG_UI, 1);
end;

procedure TThreadDecrypt.SendBtn;
begin
  SendMessage(FHandle, LM_MSG, MSG_BTN, 1);
end;

procedure TThreadDecrypt.Execute;
begin
  FDecryptString:= string(mAesDecryptECB128(PChar(FKey), PChar(FOri)));
  Synchronize(@SendUI);
  Synchronize(@SendBtn);
end;

constructor TThreadDecrypt.Create(AHandle: HWND; AOri: string; AKey: string);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FOri := AOri;
  FKey:= AKey;
  FreeOnTerminate:= True;
end;

{ TThreadEncrypt }

procedure TThreadEncrypt.SendUI;
begin
  SendMessage(FHandle, LM_MSG, MSG_UI, 0);
end;

procedure TThreadEncrypt.SendBtn;
begin
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

procedure TThreadEncrypt.Execute;
begin
  FEncryptString:= string(mAesEncryptECB128(PChar(FKey), PChar(FOri)));
  Synchronize(@SendUI);
  Synchronize(@SendBtn);
end;

constructor TThreadEncrypt.Create(AHandle: HWND; AOri: string; AKey: string);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FOri := AOri;
  FKey:= AKey;
  FreeOnTerminate:= True;
end;

{ TFormAES }

procedure TFormAES.btnDecGoClick(Sender: TObject);
begin
  btnDecGo.Enabled:= False;
  FDecrypt := TThreadDecrypt.Create(self.Handle, etDecSrc.Text, etDecKey.Text);
  FDecrypt.Start;
end;

procedure TFormAES.btnEncGoClick(Sender: TObject);
begin
  etDecKey.Text:= etEncKey.Text;
  btnEncGo.Enabled:= False;
  FEncrypt := TThreadEncrypt.Create(self.Handle, etEncSrc.Text, etEncKey.Text);
  FEncrypt.Start;
end;

procedure TFormAES.OnHandleMessage(var msg: TMessage);
begin
  case msg.LParam of
  0:
    begin
      case msg.WParam of
      MSG_UI:
        begin
          lbEncDest.Caption:= FEncrypt.EncryptString;
          etDecSrc.Text:= FEncrypt.EncryptString;
        end;
      MSG_BTN: btnEncGo.Enabled:= True;
      end;
    end;
  1:
    begin
      case msg.WParam of
      MSG_UI: lbDecDest.Caption:= FDecrypt.DecryptString;
      MSG_BTN: btnDecGo.Enabled:= True;
      end;
    end;
  end;
end;

initialization
  RegisterClass(TFormAES);

end.

