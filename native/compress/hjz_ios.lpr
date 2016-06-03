library hjz;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, sysutils, strutils, unt_zip, unt_error, unt_tar,
  unt_gz, unt_bz2, unt_hjz, unt_targz, unt_tarbz;

const
  ZIP_FORMAT: array[0..10] of string = (
    '.hjz',   // 0
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
function uncompress(filePath: PChar; dest: PChar): Integer; cdecl;
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
  if (ext = '.zip') or (ext = '.jar') then begin
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

function compress(filePath: PChar; src: PChar): Integer; cdecl;
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
  if (ext = '.zip') or (ext = '.jar') then begin
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

function getLastError(): Integer; cdecl;
begin
  Result := ERROR_CODE;
end;

function getLastErrorMessage(): PChar; cdecl;
begin
  Result := StrAlloc(Length(ERROR_MESSAGE));
  strcopy(Result, Pchar(ERROR_MESSAGE));
end;

function getHelp(): PChar; cdecl;
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

exports
  uncompress,
  compress,
  getLastError,
  getLastErrorMessage,
  getHelp;

begin

end.

