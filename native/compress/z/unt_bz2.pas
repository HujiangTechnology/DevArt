unit unt_bz2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, bzip2lib, unt_error, unt_status;

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
  errCode: Integer = 0;
  errMsg: string = '';
  bzCount: Integer = 0;
begin
  try
    infile := TFileStream.Create(filePath, fmCreate);
    bfile := TBzip2CompressStream.Create(infile);
    ofile := TFileStream.Create(src, fmOpenRead);
    try
      repeat
        count := ofile.Read(buf, SizeOf(buf));
        bfile.Write(buf, count);
      until count < SizeOf(buf);
      bzCount:= 1;
      errCode := ERROR_NONE;
      errMsg := ERRMSG_NONE;
    except
      errCode := ERROR_UNCOMPRESS;
      errMsg := ERRMSG_UNCOMPRESS;
    end;
  finally
    ofile.Free;
    bfile.Free;
    infile.Free;
  end;
  AddCompressStatus(filePath, 1, bzCount, errCode, errMsg);
  Result := errCode;
end;

function DoUnbz2(filePath: string; dest: string): integer;
var
  ofile: TFileStream;
  bfile: TBzip2DecompressStream;
  unfile: TFileStream;
  Count: integer;
  buf: array[0..1023] of byte;
  errCode: Integer = 0;
  errMsg: string = '';
  unbzCount: Integer = 0;
begin
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
        unbzCount:= 1;
        errCode := ERROR_NONE;
        errMsg := ERRMSG_NONE;
      except
        errCode := ERROR_UNCOMPRESS;
        errMsg := ERRMSG_UNCOMPRESS;
      end;
    finally
      unfile.Free;
      bfile.Free;
      ofile.Free;
    end;
  except
    errCode := ERROR_UNCOMPRESS;
    errMsg := ERRMSG_UNCOMPRESS;
  end;
  AddUncompressStatus(filePath, 1, unbzCount, errCode, errMsg);
  Result := errCode;
end;

end.

