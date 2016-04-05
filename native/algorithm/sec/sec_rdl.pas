unit sec_rdl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LbClass, LbCipher, jni2, jni_utils;

// keySize: (ks128 = 0, ks192 = 1, ks256 = 2);
// cipherMode: (cmECB = 0, cmCBC = 1);
function _rdlEncryptString(keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; stdcall;
function _rdlEncryptFile(keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; stdcall;
function _rdlDecryptString(keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; stdcall;
function _rdlDecryptFile(keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; stdcall;
function rdlEncryptString(keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; stdcall;
function rdlEncryptFile(keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; stdcall;
function rdlDecryptString(keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; stdcall;
function rdlDecryptFile(keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptString(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptFile(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; filePath: jstring; outFilePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptString(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptFile(env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring; filePath: jstring; outFilePath: jstring): jint; stdcall;

implementation

function _rdlEncryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; stdcall;
var
  rdl: TLbRijndael;
  ret: string = '';
begin
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
end;

function _rdlEncryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; stdcall;
var
  rdl: TLbRijndael;
begin
  Result := -1;
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
end;

function _rdlDecryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; stdcall;
var
  rdl: TLbRijndael;
  ret: string = '';
begin
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
end;

function _rdlDecryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; stdcall;
var
  rdl: TLbRijndael;
begin
  Result := -1;
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
end;

function rdlEncryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; stdcall;
begin
  Result := _rdlEncryptString(keySize, cipherMode, key, str);
end;

function rdlEncryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; stdcall;
begin
  Result := _rdlEncryptFile(keySize, cipherMode, key, filePath, outFilePath);
end;

function rdlDecryptString(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; stdcall;
begin
  Result := _rdlDecryptString(keySize, cipherMode, key, str);
end;

function rdlDecryptFile(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; stdcall;
begin
  Result := _rdlDecryptFile(keySize, cipherMode, key, filePath, outFilePath);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptString(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _rdlEncryptString(keySize, cipherMode, PChar(jstringToString(env, key)), PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptFile(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  filePath: jstring; outFilePath: jstring): jint; stdcall;
begin
  Result := _rdlEncryptFile(keySize, cipherMode, PChar(jstringToString(env, key)), PChar(jstringToString(env, filePath)), PChar(jstringToString(env, outFilePath)));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptString(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _rdlDecryptString(keySize, cipherMode, PChar(jstringToString(env, key)), PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptFile(
  env: PJNIEnv; obj: jobject; keySize: jint; cipherMode: jint; key: jstring;
  filePath: jstring; outFilePath: jstring): jint; stdcall;
begin
  Result := _rdlDecryptFile(keySize, cipherMode, PChar(key), PChar(filePath), PChar(outFilePath));
end;



end.

