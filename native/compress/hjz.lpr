{.$DEFINE DEBUG}

{$IFDEF DEBUG}
program hjz;
{$ELSE}
library hjz;
{$ENDIF}

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  jni2, jni_utils,
  Classes, sysutils, strutils, unt_zip, unt_error, unt_tar,
  unt_gz, unt_bz2, unt_hjz, unt_targz, unt_tarbz, unt_files;

const
  ZIP_FORMAT: array[0..11] of string = (
    '.hjz',   // 0
    '.hjp',   //
    '.zip',   // 1
    '.bz2',   // 2
    '.jar',   // 3
    '.tar',   // 4
    '.gz',    // 5
    '.gzip',   // 6
    '.tgz',   // 7
    '.tbz',   // 8
    '.tar.gz', // 9
    '.tar.bz2' // 10
    );

function extractFileRealExt(AFileName: string): string;
var
  i: Integer;
begin
  AFileName:= LowerCase(AFileName);
  for i:= 0 to Length(ZIP_FORMAT) - 1 do begin
    if AnsiEndsText(ZIP_FORMAT[i], AFileName) then begin
       Result := ZIP_FORMAT[i];
    end;
  end;
end;

// filePath: zip file to uncompress
// destPath: folder to save uncompressed files
// return:
//        -1: unsupported file format
//        -2: uncompress error
//        >=0: uncompressed file count
function _uncompress(filePath: PChar; dest: PChar): Integer;
var
  strPath: string;
  strDest: string;
  ext: string;
begin
  strPath:= string(filePath);
  strDest:= string(dest);
  ext := extractFileRealExt(strPath);
  WriteLn(Format('file format => %s', [ext]));
  Result := -1;
  ERROR_CODE:= ERROR_FORMAT_NOT_SUPPORT;
  ERROR_MESSAGE:= ERRMSG_FORMAT_NOT_SUPPORT;
  if (ext = '.zip') or (ext = '.jar') or (ext = '.hjp') then begin
    Result := DoUnzip(strPath, strDest);
  end else if (ext = '.bz2') then begin
    Result := DoUnbz2(strPath, strDest);
  end else if (ext = '.tar') then begin
    Result := DoUntar(strPath, strDest);
  end else if (ext = '.tgz') or (ext = '.tar.gz') then begin
    Result := DoUnTarGz(strPath, strDest);
  end else if (ext = '.gz') or (ext = '.gzip') then begin
    Result := DoUnGz(strPath, strDest);
  end else if (ext = '.tbz') or (ext = '.tar.bz2') then begin
    Result := DoUnTarBz(strPath, strDest);
  end else if (ext = '.hjz') then begin
    Result := DoUnhjz(strPath, strDest);
  end;
end;

function _compress(filePath: PChar; src: PChar): Integer;
var
  strPath: string;
  strSrc: string;
  ext: string;
begin
  strPath := string(filePath);
  strSrc := string(src);
  ext := extractFileRealExt(strPath);
  Result := -1;
  ERROR_CODE:= ERROR_FORMAT_NOT_SUPPORT;
  ERROR_MESSAGE:= ERRMSG_FORMAT_NOT_SUPPORT;
  if (ext = '.zip') or (ext = '.jar') or (ext = '.hjp') then begin
    Result := DoZip(strPath, strSrc);
  end else if (ext = '.bz2') then begin
    Result := DoBz2(strPath, strSrc);
  end else if (ext = '.tar') then begin
    Result := DoTar(strPath, strSrc);
  end else if (ext = '.tgz') or (ext = '.tar.gz') then begin
    Result := DoTarGz(strPath, strSrc);
  end else if (ext = '.gz') or (ext = '.gzip') then begin
    Result := DoGz(strPath, strSrc);
  end else if (ext = '.tbz') or (ext = '.tar.bz2') then begin
    Result := DoTarBz(strPath, strSrc);
  end else if (ext = '.hjz') then begin
    Result := DoHjz(strPath, strSrc);
  end;
end;

function uncompress(filePath: PChar; dest: PChar): Integer; cdecl;
begin
  Result := _uncompress(filePath, dest);
end;

function compress(filePath: PChar; src: PChar): Integer; cdecl;
begin
  Result := _compress(filePath, src);
end;

// Java_com_hujiang_devart_utils_ZipUtils

function Java_com_hujiang_devart_utils_ZipUtils_uncompress(env: PJNIEnv; obj:jobject; filePath: jstring; dest: jstring): jint; stdcall;
var
  strFilePath: string;
  strDest: string;
begin
  strFilePath:= jstringToString(env, filePath);
  strDest:= jstringToString(env, dest);
  Result := _uncompress(PChar(strFilePath), PChar(strDest));
end;

function Java_com_hujiang_devart_utils_ZipUtils_compress(env: PJNIEnv; obj: jobject; filePath: jstring; src: jstring): jint; stdcall;
var
  strFilePath: string;
  strSrc: string;
begin
  strFilePath:= jstringToString(env, filePath);
  strSrc:= jstringToString(env, src);
  Result := _compress(PChar(strFilePath), PChar(strSrc));
end;

function _getLastError(): Integer;
begin
  Result := ERROR_CODE;
end;

function getLastError(): Integer; cdecl;
begin
  Result := _getLastError();
end;

function Java_com_hujiang_devart_utils_ZipUtils_getLastError(env: PJNIEnv; obj: jobject): jint; stdcall;
begin
  Result := _getLastError();
end;

function _getLastErrorMessage(): PChar;
begin
  Result := StrAlloc(Length(ERROR_MESSAGE));
  strcopy(Result, Pchar(ERROR_MESSAGE));
end;

function getLastErrorMessage(): PChar; cdecl;
begin
  Result := _getLastErrorMessage();
end;

function Java_com_hujiang_devart_utils_ZipUtils_getLastErrorMessage(env: PJNIEnv; obj: jobject): jstring; stdcall;
var
  str: string;
begin
  str := string(_getLastErrorMessage());
  Result := stringToJString(env, str);
end;

function _getHelp(): PChar;
var
  str: string;
begin
  str := 'Usage: hjz <switcher> <parameters ...>'#13#10;
  str += ''#13#10;
  str += '    -c <dest file> <source dir|file>'#13#10;
  str += '        compress source dir or file to dest file'#13#10;
  str += '    -d <source file> <dest dir|file>'#13#10;
  str += '        decompress source file to dest dir or file'#13#10;
  str += ''#13#10;
  Result := StrAlloc(Length(str));
  strcopy(Result, PChar(str));
end;

function getHelp(): PChar; cdecl;
begin
  Result := _getHelp();
end;

function Java_com_hujiang_devart_utils_ZipUtils_getHelp(env: PJNIEnv; obj: jobject): jstring; stdcall;
var
  ret: string;
begin
  ret := string(_getHelp());
  Result := stringToJString(env, ret);
end;

function _getFileSize(path: PChar): PChar;
var
  pathStr: string;
  size: Int64;
  ret: string;
  fx: THandle;
begin
  pathStr:= string(path);
  ret := '0';
  if FileExists(pathStr) and (not DirectoryExists(pathStr)) then
  begin
    fx := FileOpen(pathStr, 0);
    size:= FileSeek(fx, 0, 2);
    FileClose(fx);
    ret := IntToStr(size);
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function getFileSize(path: PChar): PChar;
begin
  Result := _getFileSize(path);
end;

function Java_com_hujiang_devart_utils_ZipUtils_getFileSize(env: PJNIEnv; obj: jobject; path: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _getFileSize(PChar(jstringToString(env, path)));
  Result := stringToJString(env, string(ret));
end;

exports
  uncompress,
  compress,
  getLastError,
  getLastErrorMessage,
  getHelp,
  getFileSize,
  Java_com_hujiang_devart_utils_ZipUtils_uncompress,
  Java_com_hujiang_devart_utils_ZipUtils_compress,
  Java_com_hujiang_devart_utils_ZipUtils_getLastError,
  Java_com_hujiang_devart_utils_ZipUtils_getLastErrorMessage,
  Java_com_hujiang_devart_utils_ZipUtils_getHelp,
  Java_com_hujiang_devart_utils_ZipUtils_getFileSize;


{$IFDEF DEBUG}
var
  pCount: Integer;
  funType: string;
  ret: Integer;
  retStr: PChar;
  p1: string;
  p2: string;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  pCount:= ParamCount;
  if (pCount = 1) and (ParamStr(1) = '-h') then begin
    WriteLn(string(_getHelp()));
    Exit;
  end else if (pCount <> 3) then begin
    WriteLn(string(_getHelp()));
    Exit;
  end;
  funType:= ParamStr(1);
  p1 := ParamStr(2);
  p2 := ParamStr(3);
  if (funType = '-c') then begin
    ret := _compress(PChar(p1), PChar(p2));
    WriteLn(Format('Compress %s to %s => %d', [p2, p1, ret]));
    WriteLn(Format('Error Code => %d', [_getLastError()]));
    WriteLn(Format('Error Message => %s', [string(_getLastErrorMessage())]));
  end else if (funType = '-d') then begin
    ret := _uncompress(PChar(p1), PChar(p2));
    WriteLn(Format('Decompress %s to %s => %d', [p1, p2, ret]));
    WriteLn(Format('Error Code => %d', [_getLastError()]));
    WriteLn(Format('Error Message => %s', [string(_getLastErrorMessage())]));
  end else if (funType = '-s') then begin
    retStr:= _getFileSize(PChar(p1));
    WriteLn(Format('File Size => %s', [string(retStr)]));
  end;
  {$ENDIF}

end.

