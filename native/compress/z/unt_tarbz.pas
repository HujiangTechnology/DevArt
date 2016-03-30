unit unt_tarbz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,unt_tar, unt_bz2, unt_error;

function DoTarBz(filePath: string; srcDir: string): Integer;
function DoUnTarBz(filePath: string; destDir: string): Integer;

implementation

function DoTarBz(filePath: string; srcDir: string): Integer;
var
  tmpFile: string;
  count: Integer;
begin
  // tarbz
  tmpFile:= filePath + '.tmp';
  count := DoTar(tmpFile, srcDir);
  Result := count;
  if (count > 0) then begin
    count := DoBz2(filePath, tmpFile);
    Result += count;
  end;
  DeleteFile(tmpFile);
end;

function DoUnTarBz(filePath: string; destDir: string): Integer;
var
  tmpFile: string;
  count: Integer;
begin
  // untarbz
  tmpFile := filePath + '.tmp';
  count := DoUnbz2(filePath, tmpFile);
  Result := count;
  if (count > 0) then begin
    count := DoUntar(tmpFile, destDir);
    Result += count;
  end;
  DeleteFile(tmpFile);
end;

end.

