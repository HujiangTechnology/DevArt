unit NativeCompress;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs;

const
  LIB_NAME = {$IFDEF WINDOWS}'hjz.dll'{$ELSE}{$IFDEF DARWIN}'libhjz.dylib'{$ELSE}'libhjz.so'{$ENDIF}{$ENDIF};

type
  TExUncompress = function (filePath: PChar; dest: PChar): Integer; cdecl;
  TExCompress = function (filePath: PChar; src: PChar): Integer; cdecl;
  TExGetFileSize = function (path: PChar): PChar; cdecl;
  TExGetCompressErrorCode = function (filePath: PChar): Integer; cdecl;
  TExGetCompressErrorMessage = function (filePath: PChar): PChar; cdecl;
  TExGetCompressFileCount = function(filePath: PChar): Integer; cdecl;
  TExGetCompressedCount = function (filePath: PChar): Integer; cdecl;
  TExGetUncompressErrorCode = function (filePath: PChar): Integer; cdecl;
  TExGetUncompressErrorMessage = function (filePath: PChar): PChar; cdecl;
  TExGetUncompressFileCount = function (filePath: PChar): Integer; cdecl;
  TExGetUncompressedCount = function (filePath: PChar): Integer; cdecl;

var
  mUncompress: TExUncompress;
  mCompress: TExCompress;
  mGetFileSize: TExGetFileSize;
  mGetCompressErrorCode: TExGetCompressErrorCode;
  mGetCompressErrorMessage: TExGetCompressErrorMessage;
  mGetCompressFileCount: TExGetCompressFileCount;
  mGetCompressedCount: TExGetCompressedCount;
  mGetUncompressErrorCode: TExGetUncompressErrorCode;
  mGetUncompressErrorMessage: TExGetUncompressErrorMessage;
  mGetUncompressFileCount: TExGetUncompressFileCount;
  mGetUncompressedCount: TExGetUncompressedCount;

type

  { TCompressStatus }

  TCompressStatus = class
  private
    FCompressCount: Integer;
    FErrorCode: Integer;
    FErrorMessage: string;
    FFileCount: Integer;
    FFilePath: string;
  public
    property FilePath: string read FFilePath;
    property FileCount: Integer read FFileCount;
    property CompressCount: Integer read FCompressCount;
    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read FErrorMessage;
  end;

  { TUncompressStatus }

  TUncompressStatus = class
  private
    FErrorCode: Integer;
    FErrorMessage: string;
    FFileCount: Integer;
    FFilePath: string;
    FUncompressCount: Integer;
  public
    property FilePath: string read FFilePath;
    property FileCount: Integer read FFileCount;
    property UncompressCount: Integer read FUncompressCount;
    property ErrorCode: Integer read FErrorCode;
    property ErrorMessage: string read FErrorMessage;
  end;

function GetCompressStatus(filePath: string): TCompressStatus;
function GetUnCompressStatus(filePath: string): TUncompressStatus;

implementation

var
  hLib: TLibHandle;
  libPath: string;

function GetCompressStatus(filePath: string): TCompressStatus;
begin
  // TODO:
  Result := TCompressStatus.Create;
  Result.FFilePath:= filePath;
  Result.FErrorCode := mGetCompressErrorCode(PChar(filePath));
  Result.FErrorMessage:= string(mGetCompressErrorMessage(PChar(filePath)));
  Result.FCompressCount:= mGetCompressedCount(PChar(filePath));
  Result.FFileCount:= mGetCompressFileCount(PChar(filePath));
end;

function GetUnCompressStatus(filePath: string): TUncompressStatus;
begin
  // TODO:
  Result := TUncompressStatus.Create;
  Result.FFilePath:= filePath;
  Result.FErrorCode:= mGetUncompressErrorCode(PChar(filePath));
  Result.FErrorMessage:= string(mGetUncompressErrorMessage(PChar(filePath)));
  Result.FUncompressCount:= mGetUncompressedCount(PChar(filePath));
  Result.FFileCount:= mGetUncompressFileCount(PChar(filePath));
end;

initialization
  libPath:= ExtractFilePath(ParamStr(0)) + LIB_NAME;
  if FileExists(libPath) then begin
    hLib := LoadLibrary(libPath);
    if hLib <> 0 then begin
      mUncompress:= TExUncompress(GetProcAddress(hLib, 'uncompress'));
      mCompress:= TExCompress(GetProcAddress(hLib, 'compress'));
      mGetFileSize:= TExGetFileSize(GetProcAddress(hLib, 'getFileSize'));
      mGetCompressErrorCode:= TExGetCompressErrorCode(GetProcAddress(hLib, 'getCompressErrorCode'));
      mGetCompressErrorMessage:= TExGetCompressErrorMessage(GetProcAddress(hLib, 'getCompressErrorMessage'));
      mGetCompressFileCount:= TExGetCompressFileCount(GetProcAddress(hLib, 'getCompressFileCount'));
      mGetCompressedCount:= TExGetCompressedCount(GetProcAddress(hLib, 'getCompressedCount'));
      mGetUncompressErrorCode:= TExGetUncompressErrorCode(GetProcAddress(hLib, 'getUncompressErrorCode'));
      mGetUncompressErrorMessage:= TExGetUncompressErrorMessage(GetProcAddress(hLib, 'getUncompressErrorMessage'));
      mGetUncompressFileCount:= TExGetUncompressFileCount(GetProcAddress(hLib, 'getUncompressFileCount'));
      mGetUncompressedCount:= TExGetUncompressedCount(GetProcAddress(hLib, 'getUncompressedCount'));
    end;
  end;

finalization
  mUncompress := nil;
  mCompress:= nil;
  mGetFileSize:= nil;
  mGetCompressErrorCode:= nil;
  mGetCompressErrorMessage:= nil;
  mGetCompressFileCount:= nil;
  mGetCompressedCount:= nil;
  mGetUncompressErrorCode:= nil;
  mGetUncompressErrorMessage:= nil;
  mGetUncompressFileCount:= nil;
  mGetUncompressedCount:= nil;

  if (hLib <> 0) then begin
    FreeLibrary(hLib);
  end;

end.
