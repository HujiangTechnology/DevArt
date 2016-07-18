unit unt_tar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_files, LibTar, unt_error, unt_status;

function DoTar(filePath: string; srcDir: string): Integer;
function DoUntar(filePath: string; destDir: string): Integer;

implementation

function DoTar(filePath: string; srcDir: string): Integer;
var
  tar: TTarWriter;
  fileList: TStringList;
  i: Integer;
  errCode: Integer = 0;
  errMsg: String = '';
  tarCount: Integer = 0;
  fileCount: Integer = 0;
begin
  // tar
  try
    tar := TTarWriter.Create(filePath);
    try
      fileList := TStringList.Create;
      try
        FindAllFiles(srcDir, fileList);
        fileCount:= fileList.Count;
        for i := 0 to fileList.Count - 1 do begin
          tar.AddFile(fileList[i], CreateRelativePath(fileList[i], srcDir));
        end;
        tarCount := fileCount;
      finally
        fileList.Free;
      end;
      tar.Finalize;
      errCode := ERROR_NONE;
      errMsg := ERRMSG_NONE;
      if (tarCount = 0) then begin
        errCode := ERROR_COMPRESS;
        errMsg := ERRMSG_COMPRESS;
      end;
    except
      errCode := ERROR_COMPRESS;
      errMsg := ERRMSG_COMPRESS;
    end;
  finally
    tar.Free;
  end;
  AddCompressStatus(filePath, fileCount, tarCount, errCode, errMsg);
  Result := errCode;
end;

function DoUntar(filePath: string; destDir: string): Integer;
var
  untar: TTarArchive;
  drec: TTarDirRec;
  filename: string;
  dir: string;
  errCode: Integer = 0;
  errMsg: string = '';
  fileCount: Integer = 0;
  untarCount: Integer = 0;
begin
  try
    untar := TTarArchive.Create(filePath);
    try
      try
        while untar.FindNext(drec) do begin
          filename:= destDir + drec.Name;
          if (drec.FileType = ftNormal) then begin
            dir := ExtractFilePath(filename);
          end else if (drec.FileType = ftDirectory) then begin
            dir := filename;
          end;
          ForceDirectories(dir);
          if (drec.FileType = ftNormal) then begin
            untar.ReadFile(filename);
            untarCount += 1;
          end;
        end;
        fileCount:= untarCount;
        errCode := ERROR_NONE;
        errMsg := ERRMSG_NONE;
        if (untarCount = 0) then begin
          errCode := ERROR_UNCOMPRESS;
          errMsg := ERRMSG_UNCOMPRESS;
        end;
      except
        errCode := ERROR_UNCOMPRESS;
        errMsg := ERRMSG_UNCOMPRESS;
      end;
    finally
      untar.Free;
    end;
  except
    errCode := ERROR_UNCOMPRESS;
    errMsg := ERRMSG_UNCOMPRESS;
  end;
  AddUncompressStatus(filePath, fileCount, untarCount, errCode, errMsg);
  Result := errCode;
end;

end.

