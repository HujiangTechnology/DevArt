unit unt_targz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_tar, unt_gz, unt_error, unt_status;

function DoTarGz(filePath: string; srcDir: string): Integer;
function DoUnTarGz(filePath: string; destDir: string): Integer;

implementation

function DoTarGz(filePath: string; srcDir: string): Integer;
var
  count: Integer;
  tmpFile: string;
  tarCount: Integer = 0;
  errCode: Integer = 0;
  errMsg: string = '';
begin
  // tar
  tmpFile:= filePath + '.tmp';
  count := DoTar(tmpFile, srcDir);
  tarCount := count;
  if (count > 0) then begin
    count := DoGz(filePath, tmpFile);
    tarCount += count;
  end;
  DeleteFile(tmpFile);
  errCode:= ERROR_NONE;
  errMsg:= ERRMSG_NONE;
  if (tarCount = 0) then begin
    errCode:= ERROR_COMPRESS;
    errMsg:= ERRMSG_COMPRESS;
  end;
  AddCompressStatus(filePath, tarCount, tarCount, errCode, errMsg);
  Result := errCode;
end;

function DoUnTarGz(filePath: string; destDir: string): Integer;
var
  count: Integer;
  tmpFile: string;
  errCode: Integer = 0;
  errMsg: string = '';
  untarCount: Integer = 0;
begin
  // untargz
  tmpFile:= filePath + '.tmp';
  count := DoUnGz(filePath, tmpFile);
  untarCount := count;
  if (count > 0) then begin
    count := DoUntar(tmpFile, destDir);
    untarCount += count;
  end;
  DeleteFile(tmpFile);
  errCode:= ERROR_NONE;
  errMsg:= ERRMSG_NONE;
  if (untarCount = 0) then begin
    errCode:= ERROR_UNCOMPRESS;
    errMsg:= ERRMSG_UNCOMPRESS;
  end;
  AddUncompressStatus(filePath, untarCount, untarCount, errCode, errMsg);
  Result := errCode;
end;

end.

