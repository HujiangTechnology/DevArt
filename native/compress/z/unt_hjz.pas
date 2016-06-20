unit unt_hjz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tplHjzUnit, unt_tar, unt_error, unt_status;

function DoHjz(filePath: string; srcDir: string): Integer;
function DoUnhjz(filePath: string; destDir: string): integer;

implementation

function DoHjz(filePath: string; srcDir: string): Integer;
var
  tmpFile: string;
  count: Integer;
  hjz: TplLzmaCompress;
  ofile: TFileStream;
  errCode: Integer = 0;
  errMsg: string = '';
  hjzCount: Integer = 0;
begin
  // hjz
  tmpFile:= filePath + '.tmp';
  count := DoTar(tmpFile, srcDir);
  hjzCount := count;
  if (count > 0) then begin
    try
      ofile := TFileStream.Create(filePath, fmCreate);
      hjz := TplLzmaCompress.Create(nil);
      try
        hjz.OutStream := ofile;
        hjz.InputFiles.Clear;
        hjz.InputFiles.Add(tmpFile);
        if hjz.CreateArchive then begin
          hjzCount += 1;
        end;
        errCode:= ERROR_NONE;
        errMsg:= ERRMSG_NONE;
      except
        errCode:= ERROR_COMPRESS;
        errMsg:= ERRMSG_COMPRESS;
      end;
    finally
      hjz.Free;
      ofile.Free;
    end;
  end;
  DeleteFile(tmpFile);
  AddCompressStatus(filePath, hjzCount, hjzCount, errCode, errMsg);
  Result := errCode;
end;

function DoUnhjz(filePath: string; destDir: string): integer;
var
  tmpFile: string;
  count: Integer;
  ulzma: TplLzmaUnCompress;
  ofile: TFileStream;
  dfile: TFileStream;
  errCode: Integer = 0;
  errMsg: string = '';
  unhjzCount: Integer = 0;
begin
  tmpFile:= filePath + '.tmp';
  count := 0;
  try
    ofile := TFileStream.Create(filePath, fmOpenRead);
    ulzma := TplLzmaUnCompress.Create(nil);
    ulzma.InStream := ofile;
    dfile := TFileStream.Create(tmpFile, fmCreate);
    try
      try
        ulzma.ExtractFileToStream(ulzma.FilesInArchive[0].FileName, dfile);
        count := 1;
        unhjzCount := 1;
        errCode := ERROR_NONE;
        errMsg := ERRMSG_NONE;
      except
        errCode := ERROR_UNCOMPRESS;
        errMsg := ERRMSG_UNCOMPRESS;
      end;
    finally
      dfile.Free;
      ulzma.Free;
      ofile.Free;
    end;
  except
    errCode := ERROR_UNCOMPRESS;
    errMsg := ERRMSG_UNCOMPRESS;
  end;
  if (count > 0) then begin
    count := DoUntar(tmpFile, destDir);
    unhjzCount += count;
  end;
  DeleteFile(tmpFile);
  AddUncompressStatus(filePath, unhjzCount, unhjzCount, errCode, errMsg);
  Result := errCode;
end;

end.

