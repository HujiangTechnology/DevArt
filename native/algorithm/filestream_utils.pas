unit filestream_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function openFileStream(AFilePath: String): TStream;
procedure closeFileStream(AStream: TStream);

implementation

function openFileStream(AFilePath: String): TStream;
begin
  try
    Result := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  except
    Result := nil;
  end;
end;

procedure closeFileStream(AStream: TStream);
begin
  try
    AStream.Free;
  except
  end;
end;

end.

