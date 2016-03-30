unit unt_targz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_tar, unt_gz, unt_error;

function DoTarGz(filePath: string; srcDir: string): Integer;
function DoUnTarGz(filePath: string; destDir: string): Integer;

implementation

function DoTarGz(filePath: string; srcDir: string): Integer;
var
  count: Integer;
  tmpFile: string;
begin
  // tar
  tmpFile:= filePath + '.tmp';
  count := DoTar(tmpFile, srcDir);
  Result := count;
  if (count > 0) then begin
    count := DoGz(filePath, tmpFile);
    Result += count;
  end;
  DeleteFile(tmpFile);
end;

function DoUnTarGz(filePath: string; destDir: string): Integer;
var
  count: Integer;
  tmpFile: string;
begin
  // untargz
  tmpFile:= filePath + '.tmp';
  count := DoUnGz(filePath, tmpFile);
  Result := count;
  if (count > 0) then begin
    count := DoUntar(tmpFile, destDir);
    Result += count;
  end;
  DeleteFile(tmpFile);
end;

end.

