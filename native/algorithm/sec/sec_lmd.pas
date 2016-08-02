unit sec_lmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2, jni_utils, lockbox, filestream_utils;

function lmdEncryptString(str: PChar): PChar; cdecl;
function lmdEncryptFile(filePath: PChar): PChar; cdecl;
function Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptString(env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptFile(env: PJNIEnv; obj: jobject; filePath: jstring): jstring; stdcall;

implementation

var
  Buffer: array[0..1023] of Byte;

function _lmdEncryptString(str: PChar): PChar;
var
  d: LongInt;
  ret: string;
begin
  try
    StringHashLMD(d, SizeOf(d), string(str));
    ret := BufferToHex(d, SizeOf(d));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function _lmdEncryptFile(filePath: PChar): PChar;
var
  s: TStream;
  c: TLMDContext;
  d: LongInt;
  ret: string;
  bs: Int64;
begin
  try
    ret := '';
    s := openFileStream(string(filePath));
    if Assigned(s) then begin
      InitLMD(c);
      bs := s.Read(Buffer, SizeOf(Buffer));
      while (bs > 0) do begin
        UpdateLMD(c, Buffer, bs);
        bs := s.Read(Buffer, SizeOf(Buffer));
      end;
      FinalizeLMD(c, d, SizeOf(d));
      closeFileStream(s);
      ret := BufferToHex(d, SizeOf(d));
    end;
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function lmdEncryptString(str: PChar): PChar; cdecl;
begin
  Result := _lmdEncryptString(str);
end;

function lmdEncryptFile(filePath: PChar): PChar; cdecl;
begin
  Result := _lmdEncryptFile(filePath);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptString(
  env: PJNIEnv; obj: jobject; str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret  := _lmdEncryptString(PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptFile(
  env: PJNIEnv; obj: jobject; filePath: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret  := _lmdEncryptFile(PChar(jstringToString(env, filePath)));
  Result := stringToJString(env, string(ret));

end;

end.

