unit sec_aes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2, jni_utils, aes;


// AES Encrypt ECB
function aesEncryptECB128(key: PChar; src: PChar): PChar; cdecl;
function aesEncryptECB192(key: PChar; src: PChar): PChar; cdecl;
function aesEncryptECB256(key: PChar; src: PChar): PChar; cdecl;
// AES Encrypt ECB Exp
function aesEncryptECB128Exp(key: PChar; src: PChar): PChar; cdecl;
function aesEncryptECB192Exp(key: PChar; src: PChar): PChar; cdecl;
function aesEncryptECB256Exp(key: PChar; src: PChar): PChar; cdecl;
// AES Encrypt CBC
function aesEncryptCBC128(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesEncryptCBC192(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesEncryptCBC256(init: PChar; key: PChar; src: PChar): PChar; cdecl;
// AES Encrypt CBC Exp
function aesEncryptCBC128Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesEncryptCBC192Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesEncryptCBC256Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
// AES Decrypt ECB
function aesDecryptECB128(key: PChar; src: PChar): PChar; cdecl;
function aesDecryptECB192(key: PChar; src: PChar): PChar; cdecl;
function aesDecryptECB256(key: PChar; src: PChar): PChar; cdecl;
// AES Decrypt ECB Exp
function aesDecryptECB128Exp(key: PChar; src: PChar): PChar; cdecl;
function aesDecryptECB192Exp(key: PChar; src: PChar): PChar; cdecl;
function aesDecryptECB256Exp(key: PChar; src: PChar): PChar; cdecl;
// AES Decrypt CBC
function aesDecryptCBC128(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesDecryptCBC192(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesDecryptCBC256(init: PChar; key: PChar; src: PChar): PChar; cdecl;
// AES Decrypt CBC Exp
function aesDecryptCBC128Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesDecryptCBC192Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
function aesDecryptCBC256Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;


// AES Encrypt ECB
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB128(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB192(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB256(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
// AES Encrypt ECB Exp
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB128Exp(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB192Exp(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB256Exp(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
// AES Encrypt CBC
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC128(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC192(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC256(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
// AES Encrypt CBC Exp
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC128Exp(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC192Exp(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC256Exp(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
// AES Decrypt ECB
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB128(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB192(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB256(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
// AES Decrypt ECB Exp
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB128Exp(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB192Exp(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB256Exp(env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
// AES Decrypt CBC
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC128(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC192(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC256(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
// AES Decrypt CBC Exp
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC128Exp(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC192Exp(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC256Exp(env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring): jstring; stdcall;

implementation

function aesEncryptECB128(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptECB(string(key), 128, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptECB192(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptECB(string(key), 192, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptECB256(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptECB(string(key), 256, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptECB128Exp(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptECB(string(key), 128, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptECB192Exp(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptECB(string(key), 192, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptECB256Exp(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptECB(string(key), 256, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptCBC128(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptCBC(string(init), string(key), 128, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptCBC192(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptCBC(string(init), string(key), 192, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptCBC256(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptCBC(string(init), string(key), 256, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptCBC128Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptCBC(string(init), string(key), 128, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptCBC192Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptCBC(string(init), string(key), 192, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesEncryptCBC256Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESEncryptCBC(string(init), string(key), 256, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptECB128(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptECB(string(key), 128, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptECB192(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptECB(string(key), 192, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptECB256(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptECB(string(key), 256, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptECB128Exp(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptECB(string(key), 128, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptECB192Exp(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptECB(string(key), 192, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptECB256Exp(key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptECB(string(key), 256, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptCBC128(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptCBC(string(init), string(key), 128, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptCBC192(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptCBC(string(init), string(key), 192, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptCBC256(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptCBC(string(init), string(key), 256, False, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptCBC128Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptCBC(string(init), string(key), 128, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptCBC192Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptCBC(string(init), string(key), 192, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function aesDecryptCBC256Exp(init: PChar; key: PChar; src: PChar): PChar; cdecl;
var
  ret: string;
begin
  try
    ret := _AESDecryptCBC(string(init), string(key), 256, True, string(src));
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

// ==========================================================================

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB128(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptECB128(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB192(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptECB192(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB256(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptECB256(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB128Exp(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptECB128Exp(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB192Exp(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptECB192Exp(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptECB256Exp(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptECB256Exp(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC128(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptCBC128(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC192(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptCBC192(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC256(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptCBC256(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC128Exp(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptCBC128Exp(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC192Exp(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptCBC192Exp(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesEncryptCBC256Exp(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesEncryptCBC256Exp(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB128(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptECB128(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB192(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptECB192(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB256(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptECB256(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB128Exp(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptECB128Exp(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB192Exp(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptECB192Exp(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptECB256Exp(
  env: PJNIEnv; obj: jobject; key: jstring; src: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptECB256Exp(PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC128(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptCBC128(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC192(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptCBC192(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC256(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptCBC256(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC128Exp(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptCBC128Exp(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC192Exp(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptCBC192Exp(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_aesDecryptCBC256Exp(
  env: PJNIEnv; obj: jobject; init: jstring; key: jstring; src: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := aesDecryptCBC256Exp(PChar(jstringToString(env, init)), PChar(jstringToString(env, key)), PChar(jstringToString(env, src)));
  Result := stringToJString(env, string(ret));
end;


end.

