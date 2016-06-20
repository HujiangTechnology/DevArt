unit unt_tarbz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,unt_tar, unt_bz2, unt_error, unt_status;

function DoTarBz(filePath: string; srcDir: string): Integer;
function DoUnTarBz(filePath: string; destDir: string): Integer;

implementation

function DoTarBz(filePath: string; srcDir: string): Integer;
var
  tmpFile: string;
  count: Integer;
  errCode: Integer = 0;
  errMsg: string = '';
  bzCount: Integer = 0;
begin
  // tarbz
  tmpFile:= filePath + '.tmp';
  count := DoTar(tmpFile, srcDir);
  bzCount := count;
  if (count > 0) then begin
    count := DoBz2(filePath, tmpFile);
    bzCount += count;
  end;
  DeleteFile(tmpFile);
  errCode:= ERROR_NONE;
  errMsg:= ERRMSG_NONE;
  if (bzCount = 0) then begin
    errCode:= ERROR_COMPRESS;
    errMsg:= ERRMSG_COMPRESS;
  end;
  AddCompressStatus(filePath, bzCount, bzCount, errCode, errMsg);
  Result := errCode;
end;

function DoUnTarBz(filePath: string; destDir: string): Integer;
var
  tmpFile: string;
  count: Integer;
  errCode: Integer = 0;
  errMsg: string = '';
  unbzCount: Integer = 0;
begin
  // untarbz
  tmpFile := filePath + '.tmp';
  count := DoUnbz2(filePath, tmpFile);
  unbzCount := count;
  if (count > 0) then begin
    count := DoUntar(tmpFile, destDir);
    unbzCount += count;
  end;
  DeleteFile(tmpFile);
  errCode:= ERROR_NONE;
  errMsg:= ERRMSG_NONE;
  if (unbzCount = 0) then begin
    errCode:= ERROR_UNCOMPRESS;
    errMsg:= ERRMSG_UNCOMPRESS;
  end;
  AddUncompressStatus(filePath, unbzCount, unbzCount, errCode, errMsg);
  Result := errCode;
end;

end.

