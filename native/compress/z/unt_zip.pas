unit unt_zip;

{$mode objfpc}{$H+}

{$Warnings off}

interface

uses
  Classes, SysUtils, strutils, zipper, unt_error, unt_files, unt_status;



function DoZip(filePath: string; srcDir: string): integer;
function DoUnzip(filePath: string; destDir: string): integer;

implementation

function DoZip(filePath: string; srcDir: string): integer;
var
  zip: TZipper;
  zentries: TZipFileEntries;
  relaPath: string;
  i: integer;
  fileList: TStringList;
  fileCount: Integer = 0;
  zipCount: Integer = 0;
  errCode: Integer = 0;
  errMsg: string = '';
begin
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
            FindAllFiles(srcDir, fileList);
            WriteLn('fileList => ' + fileList.Text);
            for i := 0 to fileList.Count - 1 do begin
              relaPath:= CreateRelativePath(fileList[i], srcDir);
              zentries.AddFileEntry(fileList[i], relaPath);
            end;
          finally
            fileList.Free;
          end;
        end;
        fileCount:= zentries.Count;
        if (zentries.Count > 0) then begin
          ZipFiles(zentries);
        end;
        zipCount := fileCount;
        errCode := ERROR_NONE;
        errMsg := ERRMSG_NONE;
        if (zipCount = 0) then begin
          errCode:= ERROR_COMPRESS;
          errMsg:= ERRMSG_COMPRESS;
        end;
      end;
    except
      errCode := ERROR_COMPRESS;
      errMsg := ERRMSG_COMPRESS;
    end;
  finally
    if (zentries <> nil) then begin
      FreeAndNil(zentries);
    end;
    zip.Free;
  end;
  AddCompressStatus(filePath, fileCount, zipCount, errCode, errMsg);
  Result := errCode;
end;

function DoUnzip(filePath: string; destDir: string): integer;
var
  unzip: TUnZipper;
  fileCount: Integer = 0;
  unzipCount: Integer = 0;
  errCode: Integer = 0;
  errMsg: string = '';
begin
  try
    unzip := TUnZipper.Create;
    try
      with unzip do begin
        FileName := filePath;
        OutputPath := destDir;
        Examine;
        fileCount:= Entries.Count;
        UnZipAllFiles;
        unzipCount := fileCount;
      end;
      errCode := ERROR_NONE;
      errMsg := ERRMSG_NONE;
      if (unzipCount = 0) then begin
        errCode:= ERROR_UNCOMPRESS;
        errMsg:= ERRMSG_UNCOMPRESS;
      end;
    except
      errCode := ERROR_UNCOMPRESS;
      errMsg := ERRMSG_UNCOMPRESS;
    end;
  finally
    unzip.Free;
  end;
  AddUncompressStatus(filePath, fileCount, unzipCount, errCode, errMsg);
  Result := errCode;
end;

end.

