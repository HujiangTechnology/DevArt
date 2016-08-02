unit sec_base64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2, jni_utils, base64;

function base64EncryptString(str: PChar): PChar; cdecl;
function base64DecryptString(str: Pchar): PChar; cdecl;
function Java_com_hujiang_devart_security_AlgorithmUtils_base64EncryptString(env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_base64DecryptString(env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;

implementation

function _base64EncryptString(str: PChar): PChar;
var
  ret: string;
begin
  try
    ret := EncodeStringBase64(string(str));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function _base64DecryptString(str: Pchar): PChar;
var
  ret: string;
begin
  try
    ret := DecodeStringBase64(string(str));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function base64EncryptString(str: PChar): PChar; cdecl;
begin
  Result := _base64EncryptString(str);
end;

function base64DecryptString(str: Pchar): PChar; cdecl;
begin
  Result := _base64DecryptString(str);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_base64EncryptString(
  env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _base64EncryptString(PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_base64DecryptString(
  env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _base64DecryptString(PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

end.

