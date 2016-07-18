unit unt_gz;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zstream, unt_error, unt_status;

function DoGz(filePath: string; src: string): Integer;
function DoUnGz(filePath: string; dest: string): integer;

implementation

function DoGz(filePath: string; src: string): Integer;
var
  gfile: TGZFileStream;
  ofile: TFileStream;
  count: Integer;
  buf: array[0..1023] of Byte;
  errCode: Integer = 0;
  errMsg: string = '';
  gzCount: Integer = 0;
begin
  // gz
  try
    gfile := TGZFileStream.create(filePath, gzopenwrite);
    ofile := TFileStream.Create(src, fmOpenRead);
    try
      try
        repeat
          count := ofile.Read(buf, SizeOf(buf));
          gfile.write(buf, count);
        until count < SizeOf(buf);
        gzCount := 1;
        errCode := ERROR_NONE;
        errMsg := ERRMSG_NONE;
      except
        errCode := ERROR_COMPRESS;
        errMsg := ERRMSG_COMPRESS;
      end;
    finally
      ofile.Free;
      gfile.Free;
    end;
  except
    errCode := ERROR_COMPRESS;
    errMsg := ERRMSG_COMPRESS;
  end;
  AddCompressStatus(filePath, 1, gzCount, errCode, errMsg);
  Result := errCode;
end;

function DoUnGz(filePath: string; dest: string): integer;
var
  gfile: TGZFileStream;
  unfile: TFileStream;
  Count: integer;
  buf: array[0..1023] of byte;
  ungzCount: Integer = 0;
  errCode: Integer = 0;
  errMsg: string = '';
begin
  try
    gfile := TGZFileStream.Create(filePath, gzopenread);
    unfile := TFileStream.Create(dest, fmCreate);
    try
      try
        repeat
          Count := gfile.Read(buf, SizeOf(buf));
          unfile.Write(buf, Count);
        until Count < SizeOf(buf);
        ungzCount:= 1;
        errCode := ERROR_NONE;
        errMsg := ERRMSG_NONE;
      except
        errCode := ERROR_UNCOMPRESS;
        errMsg := ERRMSG_UNCOMPRESS;
      end;
    finally
      unfile.Free;
      gfile.Free;
    end;
  except
    errCode := ERROR_UNCOMPRESS;
    errMsg := ERRMSG_UNCOMPRESS;
  end;
  AddUncompressStatus(filePath, 1, ungzCount, errCode, errMsg);
  Result := errCode;
end;

end.

