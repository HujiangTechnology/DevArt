unit frmRSA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm, LMessages, LCLType, LCLIntf, Messages, strutils;

const
  LM_MSG = LM_USER + 1;

  MSG_STATUS = 0;
  MSG_UI = 1;
  MSG_BTN = 2;

type

  { TThreadKeyPair }

  TThreadKeyPair = class(TThread)
  private
    FHandle: HWND;
    FPubPass: string;
    FPubPath: string;
    FPrivPass: string;
    FPrivPath: string;
    FStatus: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; APubPass: string; APrivPass: string; APubPath: string; APrivPath: string);
  public
    property Status: string read FStatus write FStatus;
  end;

  { TThreadEncrypt }

  TThreadEncrypt = class(TThread)
  private
    FHandle: HWND;
    FEncryptedString: string;
    FOri: string;
    FPubPass: string;
    FPubPath: string;
    FStatus: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string; APubPass: string; APubPath: string);
  public
    property EncryptedString: string read FEncryptedString write FEncryptedString;
    property Status: string read FStatus write FStatus;
  end;

  { TThreadDecrypt }

  TThreadDecrypt = class(TThread)
  private
    FHandle: HWND;
    FDecryptedString: string;
    Fori: string;
    FPrivPass: string;
    FPrivPath: string;
    FStatus: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string; APrivPass: string; APrivPath: string);
  public
    property DecryptedString: string read FDecryptedString write FDecryptedString;
    property Status: string read FStatus write FStatus;
  end;

  { TFormRSA }

  TFormRSA = class(TForm)
    btnKeyPair: TButton;
    btnEncGo: TButton;
    btnDecGo: TButton;
    etEncSrc: TEdit;
    etDecSrc: TEdit;
    lbEncTitle: TLabel;
    lbEncDest: TLabel;
    lbDecTitle: TLabel;
    lbDecDest: TLabel;
    lbStatus: TLabel;
    procedure btnDecGoClick(Sender: TObject);
    procedure btnEncGoClick(Sender: TObject);
    procedure btnKeyPairClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPubPass: string;
    FPrivPass: string;
    FPubPath: string;
    FPrivPath: string;
    FKeyPair: TThreadKeyPair;
    FEncrypt: TThreadEncrypt;
    FDecrypt: TThreadDecrypt;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormRSA: TFormRSA;

implementation

{$R *.lfm}

{ TThreadKeyPair }

procedure TThreadKeyPair.Execute;
var
  ret: Integer;
begin
  FStatus:= 'Generating Key Pair ...';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 0);
  ret := mRsaGenerateKeys(0, PChar(FPubPass), PChar(FPrivPass), PChar(FPubPath), PChar(FPrivPath));
  FStatus:= IfThen(ret = 0, 'Generate OK', 'Generate Fail');
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 0);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

constructor TThreadKeyPair.Create(AHandle: HWND; APubPass: string;
  APrivPass: string; APubPath: string; APrivPath: string);
begin
  inherited Create(True);
  FHandle:= AHandle;
  FPubPass:= APubPass;
  FPrivPass:= APrivPass;
  FPubPath:= APubPath;
  FPrivPath:= APrivPath;
  FreeOnTerminate:= True;
end;

{ TFormRSA }

procedure TFormRSA.btnKeyPairClick(Sender: TObject);
begin
  btnKeyPair.Enabled:= False;
  FKeyPair := TThreadKeyPair.Create(self.Handle, FPubPass, FPrivPass, FPubPath, FPrivPath);
  FKeyPair.Start;
end;

procedure TFormRSA.btnEncGoClick(Sender: TObject);
begin
  btnEncGo.Enabled:= False;
  FEncrypt := TThreadEncrypt.Create(self.Handle, etEncSrc.Text, FPubPass, FPubPath);
  FEncrypt.Start;
end;

procedure TFormRSA.btnDecGoClick(Sender: TObject);
begin
  btnDecGo.Enabled:= False;
  FDecrypt := TThreadDecrypt.Create(self.Handle, etDecSrc.Text, FPrivPass, FPrivPath);
  FDecrypt.Start;
end;

procedure TFormRSA.FormCreate(Sender: TObject);
begin
  FPubPass:= 'hujiang';
  FPrivPass:= 'hujiang';
  FPubPath:= ExtractFilePath(ParamStr(0)) + 'rsa.pub';
  ShowMessage(FPubPath);
  FPrivPath:= ExtractFilePath(ParamStr(0)) + 'rsa.priv';
end;

procedure TFormRSA.OnHandleMessage(var msg: TMessage);
begin
  case msg.LParam of
  0:
    begin
      case msg.WParam of
        MSG_STATUS: lbStatus.Caption:= FKeyPair.Status;
        MSG_BTN: btnKeyPair.Enabled:= True;
      end;
    end;
  1:
    begin
      case msg.WParam of
      MSG_STATUS: lbStatus.Caption := FEncrypt.Status;
      MSG_UI:
        begin
          lbEncDest.Caption:= FEncrypt.EncryptedString;
          etDecSrc.Text:= FEncrypt.EncryptedString;
        end;
      MSG_BTN: btnEncGo.Enabled:= True;
      end;
    end;
  2:
    begin
      case msg.WParam of
      MSG_STATUS: lbStatus.Caption:= FDecrypt.Status;
      MSG_UI:
        begin
          lbDecDest.Caption:= FDecrypt.DecryptedString;
        end;
      MSG_BTN: btnDecGo.Enabled:= True;
      end;
    end;
  end;
end;

{ TThreadDecrypt }

procedure TThreadDecrypt.Execute;
begin
  FStatus:= 'Decrypting ...';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 2);
  FDecryptedString:= string(mRsaDecryptString(0, PChar(FPrivPass), PChar(FPrivPath), PChar(Fori)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 2);
  FStatus:= 'Decrypt Completed';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 2);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 2);
end;

constructor TThreadDecrypt.Create(AHandle: HWND; AOri: string;
  APrivPass: string; APrivPath: string);
begin
  inherited Create(True);
  FHandle:= AHandle;
  Fori:= AOri;
  FPrivPass:= APrivPass;
  FPrivPath:= APrivPath;
  FreeOnTerminate:= True;
end;

{ TThreadEncrypt }

procedure TThreadEncrypt.Execute;
begin
  FStatus:= 'Encrypting ...';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 1);
  FEncryptedString:= string(mRsaEncryptString(0, PChar(FPubPass), PChar(FPubPath), PChar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 1);
  FStatus:= 'Encrypt Completed';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 1);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 1);
end;

constructor TThreadEncrypt.Create(AHandle: HWND; AOri: string;
  APubPass: string; APubPath: string);
begin
  inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FPubPass:= APubPass;
  FPubPath:= APubPath;
  FreeOnTerminate:=True;
end;

initialization
  RegisterClass(TFormRSA);

end.

