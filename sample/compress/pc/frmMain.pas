unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, strutils, NativeCompress, LMessages, Messages, LCLIntf, LCLType;

const
  MSG_HANDLEMSG = LM_USER + 1;

type
  { TFormatInfo }

  TFormatInfo = class
  private
    FFileType: Integer;
    FFormat: string;
  public
    constructor Create(AFormat: string; AFileType: Integer);
  public
    property Format: string read FFormat write FFormat;
    property FileType: Integer read FFileType write FFileType;
  end;

  { TThreadZip }

  TThreadZip = class(TThread)
  private
    FHandle: HWND;
    FSrc: string;
    FDest: string;
    FInfo: TFormatInfo;
    FMsgEnd: string;
    FMsgStart: string;
    FStatus: TCompressStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; ASrc: string; ADest: string; info: TFormatInfo);
    destructor Destroy; override;
  public
    property MsgStart: string read FMsgStart;
    property MsgEnd: string read FMsgEnd;
    property Status: TCompressStatus read FStatus;
  end;

  { TThreadUnzip }

  TThreadUnzip = class(TThread)
  private
    FHandle: HWND;
    FSrc: string;
    FDest: string;
    FInfo: TFormatInfo;
    FMsgEnd: string;
    FMSgStart: string;
    FStatus: TUncompressStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(AHandle: HWND; ASrc: string; ADest: string; info: TFormatInfo);
    destructor Destroy; override;
  public
    property MsgStart: string read FMSgStart;
    property MsgEnd: string read FMsgEnd;
    property Status: TUncompressStatus read FStatus;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    btnZip: TButton;
    btnUnzip: TButton;
    btnGo: TButton;
    cbFormat: TComboBox;
    edtSrc: TEdit;
    edtDest: TEdit;
    lblSrc: TLabel;
    lblDest: TLabel;
    lblFormat: TLabel;
    lblFormatHint: TLabel;
    mmLog: TMemo;
    procedure btnGoClick(Sender: TObject);
    procedure btnUnzipClick(Sender: TObject);
    procedure btnZipClick(Sender: TObject);
    procedure cbFormatSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    ZIP_TYPE: TList;
    IsUnzip: Boolean;
    ThreadZip: TThreadZip;
    ThreadUnzip: TThreadUnzip;
    procedure RefreshData;
    procedure HandleMessage(var msg: TMessage); message MSG_HANDLEMSG;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TThreadUnzip }

procedure TThreadUnzip.Execute;
var
  _start, _end: QWord;
begin
  // TODO: unzip
  _start := GetTickCount64;
  FMsgStart:= Format('[unzip-start] %d', [_start]);
  SendMessage(FHandle, MSG_HANDLEMSG, 1, 0);
  mUncompress(PChar(FSrc), PChar(FDest));
  _end := GetTickCount64;
  FMsgEnd:= Format('[unzip-end] %d', [_end]);
  SendMessage(FHandle, MSG_HANDLEMSG, 2, 0);
  FStatus := GetUnCompressStatus(FSrc);
  SendMessage(FHandle, MSG_HANDLEMSG, 3, 0);
  SendMessage(FHandle, MSG_HANDLEMSG, 4, 0);
end;

constructor TThreadUnzip.Create(AHandle: HWND; ASrc: string; ADest: string;
  info: TFormatInfo);
begin
  inherited Create(True);
  FHandle:= AHandle;
  FSrc:= ASrc;
  FDest:= ADest;
  FInfo := info;
  FreeOnTerminate:= True;
end;

destructor TThreadUnzip.Destroy;
begin
  if FStatus <> nil then begin
    FStatus.Free;
  end;
  inherited Destroy;
end;

{ TThreadZip }

procedure TThreadZip.Execute;
var
  _start, _end: QWord;
begin
 // TODO: zip
  _start := GetTickCount64;
  FMsgStart:= Format('[zip-start] %d', [_start]);
  SendMessage(FHandle, MSG_HANDLEMSG, 1, 0);
  mCompress(PChar(FDest), PChar(FSrc));
  _end := GetTickCount64;
  FMsgEnd:= Format('[zip-end] %d', [_end]);
  SendMessage(FHandle, MSG_HANDLEMSG, 2, 0);
  FStatus := GetCompressStatus(FDest);
  SendMessage(FHandle, MSG_HANDLEMSG, 3, 0);
  SendMessage(FHandle, MSG_HANDLEMSG, 4, 0);
end;

constructor TThreadZip.Create(AHandle: HWND; ASrc: string; ADest: string;
  info: TFormatInfo);
begin
  Inherited Create(True);
  FHandle:= AHandle;
  FSrc:= ASrc;
  FDest:= ADest;
  FInfo := info;
  FreeOnTerminate:= True;
end;

destructor TThreadZip.Destroy;
begin
  if FStatus <> nil then begin
    FStatus.Free;
  end;
  inherited Destroy;
end;

{ TFormatInfo }

constructor TFormatInfo.Create(AFormat: string; AFileType: Integer);
begin
  FFormat:= AFormat;
  FFileType:= AFileType;
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  IsUnzip := False;
  ZIP_TYPE := TList.Create;
  ZIP_TYPE.Add(TFormatInfo.Create('.hjz', 0));
  ZIP_TYPE.Add(TFormatInfo.Create('.hjp', 0));
  ZIP_TYPE.Add(TFormatInfo.Create('.zip', 0));
  ZIP_TYPE.Add(TFormatInfo.Create('.jar', 0));
  ZIP_TYPE.Add(TFormatInfo.Create('.tar', 0));
  ZIP_TYPE.Add(TFormatInfo.Create('.gz', 1));
  ZIP_TYPE.Add(TFormatInfo.Create('.gzip', 1));
  ZIP_TYPE.Add(TFormatInfo.Create('.tgz', 0));
  ZIP_TYPE.Add(TFormatInfo.Create('.tar.gz', 0));
  cbFormat.Items.Clear;
  for i := 0 to ZIP_TYPE.Count - 1 do begin
    cbFormat.Items.Add(TFormatInfo(ZIP_TYPE.Items[i]).Format);
  end;
  cbFormat.ItemIndex:= 0;
  RefreshData();
end;

procedure TFormMain.cbFormatSelect(Sender: TObject);
begin
  RefreshData;
end;

procedure TFormMain.btnZipClick(Sender: TObject);
begin
  IsUnzip:= False;
  RefreshData;
end;

procedure TFormMain.btnUnzipClick(Sender: TObject);
begin
  IsUnzip:= True;
  RefreshData;
end;

procedure TFormMain.btnGoClick(Sender: TObject);
begin
  // gogogo
  btnGo.Enabled:= False;
  if IsUnzip then begin
    ThreadUnzip := TThreadUnzip.Create(self.Handle, edtSrc.Text, edtDest.Text, TFormatInfo(ZIP_TYPE.Items[cbFormat.ItemIndex]));
    ThreadUnzip.Start;
  end else begin
    ThreadZip := TThreadZip.Create(self.Handle, edtSrc.Text, edtDest.Text, TFormatInfo(ZIP_TYPE.Items[cbFormat.ItemIndex]));
    ThreadZip.Start;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ZIP_TYPE.Count - 1 do begin
    TFormatInfo(ZIP_TYPE.Items[i]).Free;
  end;
  ZIP_TYPE.Free;
end;

procedure TFormMain.RefreshData;
var
  info: TFormatInfo;
begin
  info := TFormatInfo(ZIP_TYPE.Items[cbFormat.ItemIndex]);
  Caption:= Format('ZipSample (%s Mode)', [IfThen(IsUnzip, 'Unzip', 'Zip')]);
  lblFormatHint.Caption:= IfThen(info.FileType = 0, 'Multi Files', 'Single File');
end;

procedure TFormMain.HandleMessage(var msg: TMessage);
var
  w: Cardinal;
  compressStat: TCompressStatus;
  uncompressStat: TUncompressStatus;
begin
  w := msg.WParam;
  if IsUnzip then begin
    case w of
      1: mmLog.Lines.Add(ThreadUnzip.MsgStart);
      2: mmLog.Lines.Add(ThreadUnzip.MsgEnd);
      3:
        begin
          uncompressStat := ThreadUnzip.Status;
          mmLog.Lines.Add(Format('[unzip-status] err: %d', [uncompressStat.ErrorCode]));
          mmLog.Lines.Add(Format('[unzip-status] msg: %s', [uncompressStat.ErrorMessage]));
          mmLog.Lines.Add(Format('[unzip-status] filePath: %s', [uncompressStat.FilePath]));
          mmLog.Lines.Add(Format('[unzip-status] fileCount: %d', [uncompressStat.FileCount]));
          mmLog.Lines.Add(Format('[unzip-status] uncompressedCount: %d', [uncompressStat.UncompressCount]));
        end;
      4: btnGo.Enabled:= True;
    end;
  end else begin
    case w of
      1: mmLog.Lines.Add(ThreadZip.MsgStart);
      2: mmLog.Lines.Add(ThreadZip.MsgEnd);
      3:
        begin
          compressStat := ThreadZip.Status;
          mmLog.Lines.Add(Format('[zip-status] err: %d', [compressStat.ErrorCode]));
          mmLog.Lines.Add(Format('[zip-status] msg: %s', [compressStat.ErrorMessage]));
          mmLog.Lines.Add(Format('[zip-status] filePath: %s', [compressStat.FilePath]));
          mmLog.Lines.Add(Format('[zip-status] fileCount: %d', [compressStat.FileCount]));
          mmLog.Lines.Add(Format('[zip-status] compressedCount: %d', [compressStat.CompressCount]));
        end;
      4: btnGo.Enabled:= True;
    end;
  end;

end;

end.

