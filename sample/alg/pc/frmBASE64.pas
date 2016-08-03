unit frmBASE64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm, LCLType, LCLIntf, LMessages, Messages, unt_msgconst;

type

  { TThreadEncrypt }

  TThreadEncrypt = class(TThread)
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

  { TThreadDecrypt }

  TThreadDecrypt = class(TThread)
  private
    FDecryptString: string;
    FHandle: HWND;
    FOri: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string);
  public
    property DecryptString: string read FDecryptString write FDecryptString;
  end;

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
    FEncrypt: TThreadEncrypt;
    FDecrypt: TThreadDecrypt;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormBASE64: TFormBASE64;

implementation

{$R *.lfm}

{ TThreadDecrypt }

procedure TThreadDecrypt.Execute;
begin
  FDecryptString:= string(mBase64DecryptString(Pchar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 1);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 1);
end;

constructor TThreadDecrypt.Create(AHandle: HWND; AOri: string);
begin
  inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FreeOnTerminate:= True;
end;

{ TThreadEncrypt }

procedure TThreadEncrypt.Execute;
begin
  FEncryptString:= string(mBase64EncryptString(Pchar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 0);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

constructor TThreadEncrypt.Create(AHandle: HWND; AOri: string);
begin
  inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FreeOnTerminate:= True;
end;

{ TFormBASE64 }

procedure TFormBASE64.btnEncGoClick(Sender: TObject);
begin
  btnEncGo.Enabled:= False;
  FEncrypt := TThreadEncrypt.Create(self.Handle, etEncSrc.Text);
  FEncrypt.Start;
end;

procedure TFormBASE64.OnHandleMessage(var msg: TMessage);
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
      MSG_UI:
        begin
          lbDecDest.Caption:= FDecrypt.DecryptString;
        end;
      MSG_BTN: btnDecGo.Enabled:= True;
      end;
    end;
  end;
end;

procedure TFormBASE64.btnDecGoClick(Sender: TObject);
begin
  btnDecGo.Enabled:= False;
  FDecrypt := TThreadDecrypt.Create(Self.Handle, etDecSrc.Text);
  FDecrypt.Start;
end;

initialization
  RegisterClass(TFormBASE64);

end.

