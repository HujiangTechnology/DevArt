unit unt_zip;

{$mode objfpc}{$H+}

{$Warnings off}

interface

uses
  Classes, SysUtils, FileUtil, strutils, zipper, unt_error, android;

function DoZip(filePath: string; srcDir: string): integer;
function DoUnzip(filePath: string; destDir: string): integer;

implementation

function DoZip(filePath: string; srcDir: string): integer;
var
  zip: TZipper;
  zentries: TZipFileEntries;
  i: integer;
  fileList: TStringList;
  relaPath: string;
begin
  Result := 0;
  try
    zip := TZipper.Create;
    try
      with zip do begin
        FileName := filePath;
        Clear;
        zentries := TZipFileEntries.Create(TZipFileEntry);
        if DirectoryExists(srcDir) then begin
          if not (AnsiEndsText('/', srcDir)) then begin
            srcDir += '/';
          end;
          fileList := TStringList.Create;

          try
            fileList := FindAllFiles(srcDir);
            LOGE(PChar('fileList => ' + fileList.Text));
            for i := 0 to fileList.Count - 1 do begin
              relaPath:= CreateRelativePath(fileList[i], srcDir);
              LOGE(PChar('relapath => ' + relaPath));
              zentries.AddFileEntry(fileList[i], relaPath);
            end;
          finally
            fileList.Free;
          end;
        end;
        if (zentries.Count > 0) then begin
          ZipFiles(zentries);
        end;
        Result := zentries.Count;
        ERROR_CODE := ERROR_NONE;
        ERROR_MESSAGE := ERRMSG_NONE;
      end;
    except
      Result := -2;
      ERROR_CODE := ERROR_COMPRESS;
      ERROR_MESSAGE := ERRMSG_COMPRESS;
    end;
  finally
    if (zentries <> nil) then begin
      FreeAndNil(zentries);
    end;
    zip.Free;
  end;
end;

function DoUnzip(filePath: string; destDir: string): integer;
var
  unzip: TUnZipper;
begin
  Result := 0;
  try
    unzip := TUnZipper.Create;
    try
      with unzip do begin
        FileName := filePath;
        OutputPath := destDir;
        Examine;
        UnZipAllFiles;
        Result := Entries.Count;
      end;
      ERROR_CODE := ERROR_NONE;
      ERROR_MESSAGE := ERRMSG_NONE;
    except
      Result := -2;
      ERROR_CODE := ERROR_UNCOMPRESS;
      ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
    end;
  finally
    unzip.Free;
  end;
end;

end.

