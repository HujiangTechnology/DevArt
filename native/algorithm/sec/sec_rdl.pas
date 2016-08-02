unit sec_rdl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lockbox, jni2, jni_utils;

// keySize: (ks128 = 0, ks192 = 1, ks256 = 2);
// cipherMode: (cmECB = 0, cmCBC = 1);
function rdlEncryptString(keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; cdecl;
function rdlEncryptFile(keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; cdecl;
function rdlDecryptString(keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; cdecl;
function rdlDecryptFile(keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; cdecl;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptString(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptFile(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; filePath: jstring; outFilePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptString(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptFile(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; filePath: jstring; outFilePath: jstring): jint; stdcall;

implementation

function _rdlEncryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar;
var
  rdl: TLbRijndael;
  ret: string = '';
begin
  try
    rdl := TLbRijndael.Create(nil);
    try
      rdl.KeySize:= TLbKeySizeRDL(keySize);
      rdl.CipherMode:= TLbCipherMode(cipherMode);
      rdl.GenerateKey(string(key));
      ret  := rdl.EncryptString(string(str));
    finally
      rdl.Free;
    end;
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function _rdlEncryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer;
var
  rdl: TLbRijndael;
begin
  Result := -1;
  try
    rdl := TLbRijndael.Create(nil);
    try
      rdl.KeySize:= TLbKeySizeRDL(keySize);
      rdl.CipherMode:= TLbCipherMode(cipherMode);
      rdl.GenerateKey(string(key));
      rdl.EncryptFile(string(filePath), string(outFilePath));
      Result := 0;
    finally
      rdl.Free;
    end;
  except
  end;
end;

function _rdlDecryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar;
var
  rdl: TLbRijndael;
  ret: string = '';
begin
  try
    rdl := TLbRijndael.Create(nil);
    try
      rdl.KeySize:= TLbKeySizeRDL(keySize);
      rdl.CipherMode:= TLbCipherMode(cipherMode);
      rdl.GenerateKey(string(key));
      ret  := rdl.DecryptString(string(str));
    finally
      rdl.Free;
    end;
    Result := StrAlloc(Length(ret));
    strcopy(Result, PChar(ret));
  except
    Result := '';
  end;
end;

function _rdlDecryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer;
var
  rdl: TLbRijndael;
begin
  Result := -1;
  try
    rdl := TLbRijndael.Create(nil);
    try
      rdl.KeySize:= TLbKeySizeRDL(keySize);
      rdl.CipherMode:= TLbCipherMode(cipherMode);
      rdl.GenerateKey(string(key));
      rdl.DecryptFile(string(filePath), string(outFilePath));
      Result := 0;
    finally
      rdl.Free;
    end;
  except
  end;
end;

function rdlEncryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; cdecl;
begin
  Result := _rdlEncryptString(keySize, 1, key, str);
end;

function rdlEncryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; cdecl;
begin
  Result := _rdlEncryptFile(keySize, 1, key, filePath, outFilePath);
end;

function rdlDecryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; cdecl;
begin
  Result := _rdlDecryptString(keySize, 1, key, str);
end;

function rdlDecryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; cdecl;
begin
  Result := _rdlDecryptFile(keySize, 1, key, filePath, outFilePath);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptString(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _rdlEncryptString(keySize, 1, PChar(jstringToString(env, key)), PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptFile(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  filePath: jstring; outFilePath: jstring): jint; stdcall;
begin
  Result := _rdlEncryptFile(keySize, 1, PChar(jstringToString(env, key)), PChar(jstringToString(env, filePath)), PChar(jstringToString(env, outFilePath)));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptString(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _rdlDecryptString(keySize, 1, PChar(jstringToString(env, key)), PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptFile(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  filePath: jstring; outFilePath: jstring): jint; stdcall;
begin
  Result := _rdlDecryptFile(keySize, 1, PChar(key), PChar(filePath), PChar(outFilePath));
end;

end.

