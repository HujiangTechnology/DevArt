unit sec_elf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2, jni_utils, lbcipher, lbutils, filestream_utils;

function _elfEncryptString(str: PChar): PChar; stdcall;
function elfEncryptString(str: PChar): PChar; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_elfEncryptString(env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;

implementation

function _elfEncryptString(str: PChar): PChar; stdcall;
var
  d: LongInt;
  ret: string;
begin
  StringHashELF(d, string(str));
  ret := BufferToHex(d, SizeOf(d));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function elfEncryptString(str: PChar): PChar; stdcall;
begin
  Result := _elfEncryptString(str);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_elfEncryptString(
  env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret  := _elfEncryptString(PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

end.

