unit sec_base64;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2, jni_utils, base64;

function _base64EncryptString(str: PChar): PChar; stdcall;
function _base64DecryptString(str: Pchar): PChar; stdcall;
function base64EncryptString(str: PChar): PChar; stdcall;
function base64DecryptString(str: Pchar): PChar; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_base64EncryptString(env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_base64DecryptString(env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;

implementation

function _base64EncryptString(str: PChar): PChar; stdcall;
var
  ret: string;
begin
  ret := EncodeStringBase64(string(str));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function _base64DecryptString(str: Pchar): PChar; stdcall;
var
  ret: string;
begin
  ret := DecodeStringBase64(string(str));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function base64EncryptString(str: PChar): PChar; stdcall;
begin
  Result := _base64DecryptString(str);
end;

function base64DecryptString(str: Pchar): PChar; stdcall;
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

