unit unt_bz2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bzip2lib, unt_error;

function DoBz2(filePath: string; src: string): Integer;
function DoUnbz2(filePath: string; dest: string): integer;

implementation

function DoBz2(filePath: string; src: string): Integer;
var
  ofile: TFileStream;
  bfile: TBzip2CompressStream;
  infile: TFileStream;
  count: Integer;
  buf: array[0..1023] of Byte;
begin
  Result := 0;
  try
    infile := TFileStream.Create(filePath, fmCreate);
    bfile := TBzip2CompressStream.Create(infile);
    ofile := TFileStream.Create(src, fmOpenRead);
    try
      repeat
        count := ofile.Read(buf, SizeOf(buf));
        bfile.Write(buf, count);
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
    bfile.Free;
    infile.Free;
  end;
end;

function DoUnbz2(filePath: string; dest: string): integer;
var
  ofile: TFileStream;
  bfile: TBzip2DecompressStream;
  unfile: TFileStream;
  Count: integer;
  buf: array[0..1023] of byte;
begin
  Result := 0;
  try
    try
      ofile := TFileStream.Create(filePath, fmOpenRead);
      bfile := TBzip2DecompressStream.Create(ofile);
      unfile := TFileStream.Create(dest, fmCreate);
      try
        repeat
          Count := bfile.Read(buf, SizeOf(buf));
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
      bfile.Free;
      ofile.Free;
    end;
  except
    Result := -2;
    ERROR_CODE := ERROR_UNCOMPRESS;
    ERROR_MESSAGE := ERRMSG_UNCOMPRESS;
  end;
end;

end.

