unit frmELF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, NativeAlgorithm, LCLType, LCLIntf, LMessages, Messages, unt_msgconst;

type

  { TThreadELF }

  TThreadELF = class(TThread)
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

  { TFormELF }

  TFormELF = class(TForm)
    btnGo: TButton;
    etSrc: TEdit;
    lbDest: TLabel;
    procedure btnGoClick(Sender: TObject);
  private
    FEncrypt: TThreadELF;
  public
    procedure OnHandleMessage(var msg: TMessage); message LM_MSG;
  end;

var
  FormELF: TFormELF;

implementation

{$R *.lfm}

{ TThreadELF }

procedure TThreadELF.SendUI;
begin
  SendMessage(FHandle, LM_MSG, MSG_UI, 0);
end;

procedure TThreadELF.SendBtn;
begin
  SendMessage(FHandle, LM_MSG, MSG_BTN, 0);
end;

procedure TThreadELF.Execute;
begin
  FEncryptString:= string(mElfEncryptString(PChar(FOri)));
  Synchronize(@SendUI);
  Synchronize(@SendBtn);
end;

constructor TThreadELF.Create(AHandle: HWND; AOri: string);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FOri:= AOri;
  FreeOnTerminate:= True;
end;

{ TFormELF }

procedure TFormELF.btnGoClick(Sender: TObject);
begin
  btnGo.Enabled:= False;
  FEncrypt := TThreadELF.Create(self.Handle, etSrc.Text);
  FEncrypt.Start;
end;

procedure TFormELF.OnHandleMessage(var msg: TMessage);
begin
  case msg.WParam of
  MSG_BTN: btnGo.Enabled:= True;
  MSG_UI: lbDest.Caption:= FEncrypt.EncryptString;
  end;
end;

initialization
  RegisterClass(TFormELF);

end.

