program test;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  dynlibs, Classes, sysutils;

type
  TGetFileSize = function (path: PChar): PChar; cdecl;
  TUncompress = function (filePath: PChar; dest: PChar): Integer; cdecl;

const
  FILEPATH = '1464316237242-f34cda048240430ba22111c2f084843b.hjp';

var
  libPath: string;
  hLib: TLibHandle;

  mGetFileSize: TGetFileSize;
  mUncompress: TUncompress;

  hjpPath: string;
  outPath: string;

  size: string;
  unRet: Integer;

begin
  hjpPath:= ExtractFilePath(ParamStr(0)) + FILEPATH;
  outPath:= ExtractFilePath(ParamStr(0)) + 'out/';
  libPath:= ExtractFilePath(ParamStr(0)) + 'libhjz.so';
  hLib:= LoadLibrary(libPath);
  mGetFileSize:= TGetFileSize(GetProcAddress(hLib, 'getFileSize'));
  mUncompress:= TUncompress(GetProcAddress(hLib, 'uncompress'));

  // call get file size
  size:= string(mGetFileSize(PChar(hjpPath)));
  WriteLn(size);

  // call uncompress
  unRet:= mUncompress(PChar(hjpPath), PChar(outPath));
  WriteLn(unRet);

  // call get file size
  size := string(mGetFileSize(PChar(hjpPath)));
  WriteLn(size);

  mGetFileSize:= nil;
  mUncompress:= nil;
  FreeLibrary(hLib);
end.

