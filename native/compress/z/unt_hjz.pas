unit unt_hjz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tplHjzUnit, unt_tar, unt_error;

function DoHjz(filePath: string; srcDir: string): Integer;
function DoUnhjz(filePath: string; destDir: string): integer;

implementation

function DoHjz(filePath: string; srcDir: string): Integer;
var
  tmpFile: string;
  count: Integer;
  hjz: TplLzmaCompress;
  ofile: TFileStream;

begin
  Result := 0;
  // hjz
  tmpFile:= filePath + '.tmp';
  count := DoTar(tmpFile, srcDir);
  Result := count;
  if (count > 0) then begin
    try
      ofile := TFileStream.Create(filePath, fmCreate);
      hjz := TplLzmaCompress.Create(nil);
      try
        hjz.OutStream := ofile;
        hjz.InputFiles.Clear;
        hjz.InputFiles.Add(tmpFile);
        if hjz.CreateArchive then begin
          Result := 1;
        end;
        ERROR_CODE:= ERROR_NONE;
        ERROR_MESSAGE:= ERRMSG_NONE;
      except
        Result := -2;
        ERROR_CODE:= ERROR_COMPRESS;
        ERROR_MESSAGE:= ERRMSG_COMPRESS;
      end;
    finally
      hjz.Free;
      ofile.Free;
    end;
  end;
  DeleteFile(tmpFile);
end;

function DoUnhjz(filePath: string; destDir: string): integer;
var
  tmpFile: string;
  count: Integer;
  ulzma: TplLzmaUnCompress;
  ofile: TFileStream;
  dfile: TFileStream;
begin
  tmpFile:= filePath + '.tmp';
  count := 0;
  Result := 0;
  try
    ofile := TFileStream.Create(filePath, fmOpenRead);
    ulzma := TplLzmaUnCompress.Create(nil);
    ulzma.InStream := ofile;
    dfile := TFileStream.Create(tmpFile, fmCreate);
    try
      try
        ulzma.ExtractFileToStream(ulzma.FilesInArchive[0].FileName, dfile);
        count := 1;
        Result := 1;
        ERROR_CODE := ERROR_NONE;
        ERROR_MESSAGE := ERRMSG_NONE;
      except
        Result := -2;
        ERROR_CODE := ERROR_UNCOMPRESS;
        ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
      end;
    finally
      dfile.Free;
      ulzma.Free;
      ofile.Free;
    end;
  except
    Result := -2;
    ERROR_CODE := ERROR_UNCOMPRESS;
    ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
  end;
  if (count > 0) then begin
    count := DoUntar(tmpFile, destDir);
    Result += count;
  end;
  DeleteFile(tmpFile);
end;

end.

