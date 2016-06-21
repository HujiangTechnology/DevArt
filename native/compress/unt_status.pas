unit unt_status;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2, jni_utils;

type
  TCompressStatus = class
  public
    filePath: string;
    fileCount: Integer;
    fileCompressed: Integer;
    errorCode: Integer;
    errorMessage: string;
  end;

  TUncompressStatus =  class
  public
    filePath: string;
    fileCount: Integer;
    fileUncompressed: Integer;
    errorCode: Integer;
    errorMessage: string;
  end;

var
  compressList: TList;
  uncompressList: TList;

// compress
procedure AddCompressStatus(filePath: string; fileCount: Integer; fileCompressed: Integer; errorCode: Integer; errorMessage: string);
procedure CleanCompressStatus;
function _GetCompressIndex(filePath: string): Integer;
function _GetCompressStatus(filePath: string): TCompressStatus;


// uncompress
procedure AddUncompressStatus(filePath: string; fileCount: Integer; fileUncompressed: Integer; errorCode: Integer; errorMessage: string);
procedure CleanUncompressStatus;
function _GetUncompressIndex(filePath: string): Integer;
function _GetUncompressStatus(filePath: string): TUncompressStatus;

// export
function getCompressErrorCode(filePath: PChar): Integer; cdecl;
function getCompressErrorMessage(filePath: PChar): PChar; cdecl;
function getCompressFileCount(filePath: PChar): Integer; cdecl;
function getCompressedCount(filePath: PChar): Integer; cdecl;
function getUncompressErrorCode(filePath: PChar): Integer; cdecl;
function getUncompressErrorMessage(filePath: PChar): PChar; cdecl;
function getUncompressFileCount(filePath: PChar): Integer; cdecl;
function getUncompressedCount(filePath: PChar): Integer; cdecl;

// export jni
function Java_com_hujiang_devart_utils_ZipUtils_getCompressErrorCode(env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_utils_ZipUtils_getCompressErrorMessage(env: PJNIEnv; obj: jobject; filePath: jstring): jstring; stdcall;
function Java_com_hujiang_devart_utils_ZipUtils_getCompressFileCount(env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_utils_ZipUtils_getCompressedCount(env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_utils_ZipUtils_getUncompressErrorCode(env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_utils_ZipUtils_getUncompressErrorMessage(env: PJNIEnv; obj: jobject; filePath: jstring): jstring; stdcall;
function Java_com_hujiang_devart_utils_ZipUtils_getUncompressFileCount(env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_utils_ZipUtils_getUncompressedCount(env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;


implementation

procedure AddCompressStatus(filePath: string; fileCount: Integer;
  fileCompressed: Integer; errorCode: Integer; errorMessage: string);
var
  idx: Integer;
  item: TCompressStatus;
begin
  idx := _GetCompressIndex(filePath);
  item := TCompressStatus.Create;
  item.filePath:= filePath;
  item.fileCount:= fileCount;
  item.fileCompressed:= fileCompressed;
  item.errorCode:= errorCode;
  item.errorMessage:= errorMessage;
  if (idx = -1) then begin
    compressList.Add(item);
  end else begin
    TCompressStatus(compressList.Items[idx]).Free;
    compressList.Delete(idx);
    compressList.Add(item);
  end;
end;

procedure CleanCompressStatus;
var
  i: Integer;
begin
  for i:= compressList.Count - 1 downto 0 do begin
    TCompressStatus(compressList.Items[i]).Free;
  end;
  compressList.Clear;
end;

function _GetCompressIndex(filePath: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to compressList.Count - 1 do begin
    if (TCompressStatus(compressList.Items[i]).filePath = filePath) then begin
      Result := i;
      Break;
    end;
  end;

end;

function _GetCompressStatus(filePath: string): TCompressStatus;
var
  i: Integer;
begin
  Result := nil;
  for i:= 0 to compressList.Count - 1 do begin
    if (TCompressStatus(compressList.Items[i]).filePath = filePath) then begin
      Result := TCompressStatus(compressList.Items[i]);
      Break;
    end;
  end;
end;

procedure AddUncompressStatus(filePath: string; fileCount: Integer;
  fileUncompressed: Integer; errorCode: Integer; errorMessage: string);
var
  idx: Integer;
  item: TUncompressStatus;
begin
  idx := _GetUncompressIndex(filePath);
  item := TUncompressStatus.Create;
  item.filePath:= filePath;
  item.fileCount:= fileCount;
  item.fileUncompressed:= fileUncompressed;
  item.errorCode:= errorCode;
  item.errorMessage:= errorMessage;
  if (idx = -1) then begin
    uncompressList.Add(item);
  end else begin
    TUncompressStatus(uncompressList.Items[idx]).Free;
    uncompressList.Delete(idx);
    uncompressList.Add(item);
  end;



end;

procedure CleanUncompressStatus;
var
  i: Integer;
begin
  for i:= uncompressList.Count - 1 downto 0 do begin
    TUncompressStatus(uncompressList.Items[i]).Free;
  end;
  uncompressList.Clear;
end;

function _GetUncompressIndex(filePath: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to uncompressList.Count - 1 do begin
    if (TUncompressStatus(uncompressList.Items[i]).filePath = filePath) then begin
      Result := i;
      Break;
    end;
  end;
end;

function _GetUncompressStatus(filePath: string): TUncompressStatus;
var
  i: Integer;
begin
  Result := nil;
  for i:= 0 to uncompressList.Count - 1 do begin
    if (TUncompressStatus(uncompressList.Items[i]).filePath = filePath) then begin
      Result := TUncompressStatus(uncompressList.Items[i]);
      Break;
    end;
  end;
end;

function getCompressErrorCode(filePath: PChar): Integer; cdecl;
var
  item: TCompressStatus;
begin
  Result := 0;
  item := _GetCompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := item.errorCode;
  end;
end;

function getCompressErrorMessage(filePath: PChar): PChar; cdecl;
var
  item: TCompressStatus;
begin
  Result := '';
  item := _GetCompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := StrAlloc(Length(item.errorMessage));
    strcopy(Result, PChar(item.errorMessage));
  end;
end;

function getCompressFileCount(filePath: PChar): Integer; cdecl;
var
  item: TCompressStatus;
begin
  Result := 0;
  item := _GetCompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := item.fileCount;
  end;
end;

function getCompressedCount(filePath: PChar): Integer; cdecl;
var
  item: TCompressStatus;
begin
  Result := 0;
  item := _GetCompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := item.fileCompressed;
  end;
end;

function getUncompressErrorCode(filePath: PChar): Integer; cdecl;
var
  item: TUncompressStatus;
begin
  Result := 0;
  item := _GetUncompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := item.errorCode;
  end;
end;

function getUncompressErrorMessage(filePath: PChar): PChar; cdecl;
var
  item: TUncompressStatus;
begin
  Result := '';
  item := _GetUncompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := StrAlloc(Length(item.errorMessage));
    strcopy(Result, PChar(item.errorMessage));
  end;
end;

function getUncompressFileCount(filePath: PChar): Integer; cdecl;
var
  item: TUncompressStatus;
begin
  Result := 0;
  item := _GetUncompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := item.fileCount;
  end;
end;

function getUncompressedCount(filePath: PChar): Integer; cdecl;
var
  item: TUncompressStatus;
begin
  Result := 0;
  item := _GetUncompressStatus(string(filePath));
  if (item <> nil) then begin
    Result := item.fileUncompressed;
  end;
end;

function Java_com_hujiang_devart_utils_ZipUtils_getCompressErrorCode(
  env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
var
  item: TCompressStatus;
begin
  Result := 0;
  item := _GetCompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    Result := item.errorCode;
  end;
end;

function Java_com_hujiang_devart_utils_ZipUtils_getCompressErrorMessage(
  env: PJNIEnv; obj: jobject; filePath: jstring): jstring; stdcall;
var
  item: TCompressStatus;
  ret: string;
begin
  ret := '';
  item := _GetCompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    ret := item.errorMessage;
  end;
  Result := stringToJString(env, ret);
end;

function Java_com_hujiang_devart_utils_ZipUtils_getCompressFileCount(
  env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
var
  item: TCompressStatus;
begin
  Result := 0;
  item := _GetCompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    Result := item.fileCount;
  end;
end;

function Java_com_hujiang_devart_utils_ZipUtils_getCompressedCount(
  env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
var
  item: TCompressStatus;
begin
  Result := 0;
  item := _GetCompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    Result := item.fileCompressed;
  end;
end;

function Java_com_hujiang_devart_utils_ZipUtils_getUncompressErrorCode(
  env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
var
  item: TUncompressStatus;
begin
  Result := 0;
  item := _GetUncompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    Result := item.errorCode;
  end;
end;

function Java_com_hujiang_devart_utils_ZipUtils_getUncompressErrorMessage(
  env: PJNIEnv; obj: jobject; filePath: jstring): jstring; stdcall;
var
  item: TUncompressStatus;
  ret: string;
begin
  ret := '';
  item := _GetUncompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    ret := item.errorMessage;
  end;
  Result := stringToJString(env, ret);
end;

function Java_com_hujiang_devart_utils_ZipUtils_getUncompressFileCount(
  env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
var
  item: TUncompressStatus;
begin
  Result := 0;
  item := _GetUncompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    Result := item.fileCount;
  end;
end;

function Java_com_hujiang_devart_utils_ZipUtils_getUncompressedCount(
  env: PJNIEnv; obj: jobject; filePath: jstring): jint; stdcall;
var
  item: TUncompressStatus;
begin
  Result := 0;
  item := _GetUncompressStatus(jstringToString(env, filePath));
  if (item <> nil) then begin
    Result := item.fileUncompressed;
  end;
end;

initialization
  compressList := TList.Create;
  uncompressList := TList.Create;

finalization
  CleanCompressStatus;
  CleanUncompressStatus;
  compressList.Free;
  uncompressList.Free;

end.

