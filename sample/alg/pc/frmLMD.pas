unit frmLMD;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, StdCtrls, Dialogs, NativeAlgorithm, LCLType, LCLIntf, LMessages, Messages, unt_msgconst;

type

  { TThreadLMD }

  TThreadLMD = class(TThread)
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

  { TFormLMD }

  TFormLMD = class(TForm)
    btnGo: TButton;
    etSrc: TEdit;
    lbDest: TLabel;
    procedure btnGoClick(Sender: TObject);
  private
    FEncrypt: TThreadLMD;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormLMD: TFormLMD;

implementation

{$R *.lfm}

{ TThreadLMD }

procedure TThreadLMD.Execute;
begin
  FEncryptString:= string(mLmdEncryptString(PChar(FOri)));
  SendMessage(FHandle, LM_MSG, MSG_UI, 0);
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

constructor TThreadLMD.Create(AHandle: HWND; AOri: string);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FreeOnTerminate:= True;
end;

{ TFormLMD }

procedure TFormLMD.btnGoClick(Sender: TObject);
begin
  btnGo.Enabled:= False;
  FEncrypt := TThreadLMD.Create(self.Handle, etSrc.Text);
  FEncrypt.Start;
end;

procedure TFormLMD.OnHandleMessage(var msg: TMessage);
begin
  case msg.WParam of
  MSG_BTN: btnGo.Enabled:= True;
  MSG_UI: lbDest.Caption:= FEncrypt.EncryptString;
  end;
end;

initialization
  RegisterClass(TFormLMD);

end.

