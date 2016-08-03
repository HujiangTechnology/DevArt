unit frmDSA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm, LMessages, Messages, LCLType, LCLIntf, strutils, unt_msgconst;

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
    FPrivPass: string;
    FPrivPath: string;
    FStatus: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; AOri: string; APrivPass: string; APrivPath: string);
  public
    property EncryptedString: string read FEncryptedString write FEncryptedString;
    property Status: string read FStatus write FStatus;
  end;

  { TThreadVerify }

  TThreadVerify = class(TThread)
  private
    FHandle: HWND;
    FSrc: String;
    FOri: string;
    FPubPass: string;
    FPubPath: string;
    FStatus: string;
    FVerifyString: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; ASrc: string; AOri: string; APubPass: string; APubPath: string);
  public
    property VerifyString: string read FVerifyString write FVerifyString;
    property Status: string read FStatus write FStatus;
  end;

  { TFormDSA }

  TFormDSA = class(TForm)
    btnKeyPair: TButton;
    btnEncGo: TButton;
    btnVerifyGo: TButton;
    etEncSrc: TEdit;
    etVerifySrc: TEdit;
    etVerifyOri: TEdit;
    lbStatus: TLabel;
    lbEncTitle: TLabel;
    lbEncDest: TLabel;
    lbVerifyTitle: TLabel;
    lbVerifyDest: TLabel;
    procedure btnEncGoClick(Sender: TObject);
    procedure btnKeyPairClick(Sender: TObject);
    procedure btnVerifyGoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FPubPass: string;
    FPrivPass: string;
    FPubPath: string;
    FPrivPath: string;
    FKeyPair: TThreadKeyPair;
    FEncrypt: TThreadEncrypt;
    FVerify: TThreadVerify;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormDSA: TFormDSA;

implementation

{$R *.lfm}

{ TThreadVerify }

procedure TThreadVerify.Execute;
var
  ret: Integer;
begin
  FStatus:= 'Verifying ...';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 2);
  ret := mDsaVerifyString(0, PChar(FPubPass), PChar(FPubPath), PChar(FSrc), PChar(Fori));
  FVerifyString:= IfThen(ret = 0, 'TRUE', 'FALSE');
  SendMessage(FHandle, LM_MSG, MSG_UI, 2);
  FStatus:= 'Verify Completed';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 2);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 2);
end;

constructor TThreadVerify.Create(AHandle: HWND; ASrc: string; AOri: string;
  APubPass: string; APubPath: string);
begin
  inherited Create(True);
  FHandle:= AHandle;
  FSrc:= ASrc;
  FOri:= AOri;
  FPubPass:= APubPass;
  FPubPath:= APubPath;
  FreeOnTerminate:= True;
end;

{ TThreadEncrypt }

procedure TThreadEncrypt.Execute;
begin
  FStatus:= 'Encrypting ...';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 1);
  FEncryptedString:= string(mDsaSignString(0, PChar(FPrivPass), PChar(FPrivPath), PChar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 1);
  FStatus:= 'Encrypt Completed';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 1);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 1);
end;

constructor TThreadEncrypt.Create(AHandle: HWND; AOri: string;
  APrivPass: string; APrivPath: string);
begin
  inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FPrivPass:= APrivPass;
  FPrivPath:= APrivPath;
  FreeOnTerminate:=True;
end;

{ TFormDSA }

procedure TFormDSA.btnKeyPairClick(Sender: TObject);
begin
  btnKeyPair.Enabled:= False;
  FKeyPair := TThreadKeyPair.Create(self.Handle, FPubPass, FPrivPass, FPubPath, FPrivPath);
  FKeyPair.Start;
end;

procedure TFormDSA.btnVerifyGoClick(Sender: TObject);
begin
  btnVerifyGo.Enabled:= False;
  FVerify := TThreadVerify.Create(self.Handle, etVerifySrc.Text, etVerifyOri.Text, FPubPass, FPubPath);
  FVerify.Start;
end;

procedure TFormDSA.btnEncGoClick(Sender: TObject);
begin
  etVerifyOri.Text:= etEncSrc.Text;
  btnEncGo.Enabled:= False;
  FEncrypt := TThreadEncrypt.Create(self.Handle, etEncSrc.Text, FPrivPass, FPrivPath);
  FEncrypt.Start;
end;

procedure TFormDSA.FormCreate(Sender: TObject);
begin
  FPubPass:= 'hujiang';
  FPrivPass:= 'hujiang';
  FPubPath:= ExtractFilePath(ParamStr(0)) + 'dsa.pub';
  FPrivPath:= ExtractFilePath(ParamStr(0)) + 'dsa.priv';
end;

procedure TFormDSA.OnHandleMessage(var msg: TMessage);
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
            etVerifySrc.Text:= FEncrypt.EncryptedString;
          end;
        MSG_BTN: btnEncGo.Enabled:= True;
        end;
      end;
    2:
      begin
        case msg.WParam of
        MSG_STATUS: lbStatus.Caption:= FVerify.Status;
        MSG_UI:
          begin
            lbVerifyDest.Caption:= FVerify.VerifyString;
          end;
        MSG_BTN: btnVerifyGo.Enabled:= True;
        end;
      end;
    end;
end;

{ TThreadKeyPair }

procedure TThreadKeyPair.Execute;
var
  ret: Integer;
begin
  FStatus:= 'Generating Key Pair ...';
  SendMessage(FHandle, LM_MSG, MSG_STATUS, 0);
  ret := mDsaGenerateKeys(0, PChar(FPubPass), PChar(FPrivPass), PChar(FPubPath), PChar(FPrivPath));
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

initialization
  RegisterClass(TFormDSA);

end.

