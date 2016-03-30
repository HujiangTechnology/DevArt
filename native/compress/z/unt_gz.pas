unit unt_gz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream, unt_error;

function DoGz(filePath: string; src: string): Integer;
function DoUnGz(filePath: string; dest: string): integer;

implementation

function DoGz(filePath: string; src: string): Integer;
var
  gfile: TGZFileStream;
  ofile: TFileStream;
  count: Integer;
  buf: array[0..1023] of Byte;
begin
  // gz
  Result := 0;
  try
    try
      gfile := TGZFileStream.create(filePath, gzopenwrite);
      ofile := TFileStream.Create(src, fmOpenRead);
      try
        repeat
          count := ofile.Read(buf, SizeOf(buf));
          gfile.write(buf, count);
        until count < SizeOf(buf);
        Result := 1;
        ERROR_CODE := ERROR_NONE;
        ERROR_MESSAGE := ERRMSG_NONE;
      except
        Result := -2;
        ERROR_CODE := ERROR_UNCOMPRESS;
        ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
      end;
    finally
      ofile.Free;
      gfile.Free;
    end;
  except
    Result := -2;
    ERROR_CODE := ERROR_UNCOMPRESS;
    ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
  end;
end;

function DoUnGz(filePath: string; dest: string): integer;
var
  gfile: TGZFileStream;
  unfile: TFileStream;
  Count: integer;
  buf: array[0..1023] of byte;
begin
  Result := 0;
  try
    try
      gfile := TGZFileStream.Create(filePath, gzopenread);
      unfile := TFileStream.Create(dest, fmCreate);
      try
        repeat
          Count := gfile.Read(buf, SizeOf(buf));
          unfile.Write(buf, Count);
        until Count < SizeOf(buf);
        Result := 1;
        ERROR_CODE := ERROR_NONE;
        ERROR_MESSAGE := ERRMSG_NONE;
      except
        Result := -2;
        ERROR_CODE := ERROR_UNCOMPRESS;
        ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
      end;
    finally
      unfile.Free;
      gfile.Free;
    end;
  except
    Result := -2;
    ERROR_CODE := ERROR_UNCOMPRESS;
    ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
  end;
end;

end.

