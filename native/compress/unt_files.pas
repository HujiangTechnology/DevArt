unit unt_files;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

procedure FindAllFiles(srcPath: string; list: TStringList = nil);
function CreateRelativePath(path: string; base: string): string;

implementation

function combinePath(base: string; sub: string): string;
begin
  if not (AnsiEndsStr('/', base)) then begin
    base += '/';
  end;
  Result := base + sub;
end;

procedure FindAllFiles(srcPath: string; list: TStringList);
var
  src: TSearchRec;
  p: string;
begin
  if (list = nil) then begin
    list := TStringList.Create;
  end;
  if not (AnsiEndsStr('/', srcPath)) then begin
    srcPath += '/';
  end;
  if (FindFirst(srcPath + '*', faAnyFile, src) = 0) then begin
    repeat
      if (src.Name <> '.') and (src.Name <> '..') then begin
        p := combinePath(srcPath, src.Name);
        if (DirectoryExists(p)) then begin
          FindAllFiles(p, list);
        end else begin
          list.Add(p);
        end;
      end;
    until FindNext(src) <> 0;
    FindClose(src);
  end;
end;

function CreateRelativePath(path: string; base: string): string;
begin
  // base => base => /sdcard/test
  Result := path;
  if (AnsiStartsStr(base, path)) then begin
    Result := Copy(path, Length(base) + 1, Length(path) - Length(base) + 1);
    if (AnsiStartsStr('/', Result)) then begin
       Result := RightStr(Result, Length(Result) - 1);
    end;
  end;
end;

end.

