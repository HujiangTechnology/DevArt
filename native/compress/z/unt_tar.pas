unit unt_tar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, unt_files, LibTar, unt_error;

function DoTar(filePath: string; srcDir: string): Integer;
function DoUntar(filePath: string; destDir: string): Integer;

implementation

function DoTar(filePath: string; srcDir: string): Integer;
var
  tar: TTarWriter;
  fileList: TStringList;
  i: Integer;
begin
  // tar
  Result := 0;
  try
    tar := TTarWriter.Create(filePath);
    try
      try
        FindAllFiles(srcDir, fileList);
        for i := 0 to fileList.Count - 1 do begin
          tar.AddFile(fileList[i], CreateRelativePath(fileList[i], srcDir));
        end;
        Result := fileList.Count;
      finally
        fileList.Free;
      end;
      tar.Finalize;
      ERROR_CODE := ERROR_NONE;
      ERROR_MESSAGE := ERRMSG_NONE;
    except
      Result := -2;
      ERROR_CODE := ERROR_UNCOMPRESS;
      ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
    end;
  finally
    tar.Free;
  end;
end;

function DoUntar(filePath: string; destDir: string): Integer;
var
  untar: TTarArchive;
  drec: TTarDirRec;
  filename: string;
  dir: string;
begin
  Result := 0;
  try
    untar := TTarArchive.Create(filePath);
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
          Result += 1;
        end;
      end;
      ERROR_CODE := ERROR_NONE;
      ERROR_MESSAGE := ERRMSG_NONE;
    except
      Result := -2;
      ERROR_CODE := ERROR_UNCOMPRESS;
      ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
    end;
  finally
    untar.Free;
  end;

end;

end.

