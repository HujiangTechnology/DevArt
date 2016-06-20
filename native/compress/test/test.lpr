program test;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  dynlibs, Classes, sysutils, Forms, Interfaces;

type
  TGetFileSize = function (path: PChar): PChar; cdecl;
  TUncompress = function (filePath: PChar; dest: PChar): Integer; cdecl;
  TGetCompressErrorCode = function (filePath: PChar): Integer; cdecl;
  TGetUncompressErrorCode = function (filePath: PChar): Integer; cdecl;

  { TDemoThread }

  TDemoThread = class(TThread)
  private
    FFilePath: string;
    FOutPath: string;
    procedure threadTerminate(Sender: TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(filePath: string; outPath: string);
  end;

var
  libPath: string;
  hLib: TLibHandle;

  mGetFileSize: TGetFileSize;
  mUncompress: TUncompress;
  mGetCompressErrorCode: TGetCompressErrorCode;
  mGetUncompressErrorCode: TGetUncompressErrorCode;

  hjpPath: string;
  hjpPath2: string;
  outPath: string;
  outPath2: string;

  termCount: Integer = 0;

{ TDemoThread }

procedure TDemoThread.threadTerminate(Sender: TObject);
begin
  termCount += 1;
end;

procedure TDemoThread.Execute;
var
  ret: Integer;
begin
  ret := mUncompress(Pchar(FFilePath), Pchar(FOutPath));
  WriteLn(ret);
end;

constructor TDemoThread.Create(filePath: string; outPath: string);
begin
  inherited Create(True);
  FFilePath:= filePath;
  FOutPath:= outPath;
  OnTerminate:=@threadTerminate;
  FreeOnTerminate:= True;
end;


var
  t1: TDemoThread;
  t2: TDemoThread;

  err1: Integer;
  err2: Integer;
begin
  hjpPath:= ExtractFilePath(ParamStr(0)) + 'file1.hjp';
  hjpPath2:= ExtractFilePath(ParamStr(0)) + 'file2.hjp';
  outPath:= ExtractFilePath(ParamStr(0)) + 'out/';
  outPath2:= ExtractFilePath(ParamStr(0)) + 'out2/';
  libPath:= ExtractFilePath(ParamStr(0)) + 'libhjz.so';
  hLib:= LoadLibrary(libPath);
  mGetFileSize:= TGetFileSize(GetProcAddress(hLib, 'getFileSize'));
  mUncompress:= TUncompress(GetProcAddress(hLib, 'uncompress'));
  mGetCompressErrorCode := TGetCompressErrorCode(GetProcAddress(hLib, 'getCompressErrorCode'));
  mGetUncompressErrorCode := TGetUncompressErrorCode(GetProcAddress(hLib, 'getUncompressErrorCode'));

  // call uncompress
  t1 := TDemoThread.Create(hjpPath, outPath);
  t2 := TDemoThread.Create(hjpPath2, outPath2);
  t1.Start;
  t2.Start;

  while termCount <> 2 do begin
    Application.ProcessMessages;
  end;

  err1:= mGetUncompressErrorCode(PChar(hjpPath));
  err2:= mGetUncompressErrorCode(PChar(hjpPath2));
  WriteLn(Format('ErrCode: 1 => %d, 2 => %d', [err1, err2]));
end.

