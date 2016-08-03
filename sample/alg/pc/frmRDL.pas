unit frmRDL;

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
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string; AKey: string);
  public
    property DecryptString: string read FDecryptString write FDecryptString;
  end;

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
    FEncrypt: TThreadEncrypt;
    FDecrypt: TThreadDecrypt;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormRDL: TFormRDL;

implementation

{$R *.lfm}

{ TThreadDecrypt }

procedure TThreadDecrypt.Execute;
begin
  FDecryptString:= string(mRdlDecryptString(0, 1, PChar(FKey), PChar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 1);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 1);
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

procedure TThreadEncrypt.Execute;
begin
  FEncryptString:= string(mRdlEncryptString(0, 1, PChar(FKey), PChar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 0);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

constructor TThreadEncrypt.Create(AHandle: HWND; AOri: string; AKey: string);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FOri := AOri;
  FKey:= AKey;
  FreeOnTerminate:= True;
end;

{ TFormRDL }

procedure TFormRDL.btnEncGoClick(Sender: TObject);
begin
  btnEncGo.Enabled:= False;
  etDecKey.Text:= etEncKey.Text;
  FEncrypt := TThreadEncrypt.Create(self.Handle, etEncSrc.Text, etEncKey.Text);
  FEncrypt.Start;
end;

procedure TFormRDL.OnHandleMessage(var msg: TMessage);
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

procedure TFormRDL.btnDecGoClick(Sender: TObject);
begin
  btnDecGo.Enabled:= False;
  FDecrypt := TThreadDecrypt.Create(self.Handle, etDecSrc.Text, etDecKey.Text);
  FDecrypt.Start;
end;

initialization
  RegisterClass(TFormRDL);

end.

