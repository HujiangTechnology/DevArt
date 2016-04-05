unit sec_rsassa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LbRSA, LbCipher, LbAsym, LbUtils, math, jni_utils, jni2;

type

  { TRSASSABlockEvent }

  TRSASSABlockEvent = class
  private
    FSig: string;
  public
    constructor Create(sig: string);
    procedure evtGetSignatre(Sender: TObject; var Sig: TRSASignatureBlock);
  end;

function _rsassaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar; pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
function _rsassaSignString(keySize: Integer; hashMethod: Integer; privPass: PChar; privPath: PChar; str: PChar): PChar; stdcall;
function _rsassaSignFile(keySize: Integer; hashMethod: Integer; privPass: PChar; privPath: PChar; filePath: PChar): PChar; stdcall;
function _rsassaVerifyString(keySize: Integer; hashMethod: Integer; pubPass: PChar; pubPath: PChar; sig: PChar; str: PChar): Integer; stdcall;
function _rsassaVerifyFile(keySize: Integer; hashMethod: Integer; pubPass: PChar; pubPath: PChar; sig: PChar; filePath: PChar): Integer; stdcall;
function rsassaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar; pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
function rsassaSignString(keySize: Integer; hashMethod: Integer; privPass: PChar; privPath: PChar; str: PChar): PChar; stdcall;
function rsassaSignFile(keySize: Integer; hashMethod: Integer; privPass: PChar; privPath: PChar; filePath: PChar): PChar; stdcall;
function rsassaVerifyString(keySize: Integer; hashMethod: Integer; pubPass: PChar; pubPath: PChar; sig: PChar; str: PChar): Integer; stdcall;
function rsassaVerifyFile(keySize: Integer; hashMethod: Integer; pubPass: PChar; pubPath: PChar; sig: PChar; filePath: PChar): Integer; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaGenerateKeys(env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring; privPass: jstring; pubSavePath: jstring;  privSavePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaSignString(env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint; privPass: jstring; privPath: jstring; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaSignFile(env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint; privPass: jstring; privPath: jstring; filePath: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaVerifyString(env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint; pubPass: jstring; pubPath: jstring; sig: jstring; str: jstring): jint; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaVerifyFile(env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint; pubPass: jstring; pubPath: jstring; sig: jstring; filePath: jstring): jint; stdcall;

implementation

function _rsassaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar;
  pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
var
  rsassa: TLbRSASSA;
begin
  Result := -1;
  rsassa := TLbRSASSA.Create(nil);
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.GenerateKeyPair;
    rsassa.PublicKey.Passphrase:= string(pubPass);
    rsassa.PublicKey.StoreToFile(string(pubSavePath));
    rsassa.PrivateKey.Passphrase:= string(privPass);
    rsassa.PrivateKey.StoreToFile(string(privSavePath));
    Result := 0;
  finally
    rsassa.Free;
  end;
end;

function _rsassaSignString(keySize: Integer; hashMethod: Integer;
  privPass: PChar; privPath: PChar; str: PChar): PChar; stdcall;
var
  rsassa: TLbRSASSA;
  ret: string = '';
begin
  rsassa := TLbRSASSA.Create(nil);
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.PrivateKey.Passphrase:= string(privPass);
    rsassa.PrivateKey.LoadFromFile(string(privPath));
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.SignString(string(str));
    ret := rsassa.Signature.IntStr;
  finally
    rsassa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function _rsassaSignFile(keySize: Integer; hashMethod: Integer;
  privPass: PChar; privPath: PChar; filePath: PChar): PChar; stdcall;
var
  rsassa: TLbRSASSA;
  ret: string = '';
begin
  rsassa := TLbRSASSA.Create(nil);
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.PrivateKey.Passphrase:= string(privPass);
    rsassa.PrivateKey.LoadFromFile(string(privPath));
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.SignFile(string(filePath));
    ret := rsassa.Signature.IntStr
  finally
    rsassa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function _rsassaVerifyString(keySize: Integer; hashMethod: Integer;
  pubPass: PChar; pubPath: PChar; sig: PChar; str: PChar): Integer; stdcall;
var
  rsassa: TLbRSASSA;
  evt: TRSASSABlockEvent;
  ret: Boolean;
begin
  Result := -1;
  rsassa := TLbRSASSA.Create(nil);
  evt := TRSASSABlockEvent.Create(string(sig));
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.PublicKey.Passphrase:= string(pubPass);
    rsassa.PublicKey.LoadFromFile(string(pubPath));
    rsassa.OnGetSignature:= @evt.evtGetSignatre;
    ret := rsassa.VerifyString(string(str));
    Result := ifthen(ret, 0, 1);
  finally
    rsassa.Free;
    evt.Free;
  end;
end;

function _rsassaVerifyFile(keySize: Integer; hashMethod: Integer;
  pubPass: PChar; pubPath: PChar; sig: PChar; filePath: PChar): Integer;
  stdcall;
var
  rsassa: TLbRSASSA;
  evt: TRSASSABlockEvent;
  ret: Boolean;
begin
  Result := -1;
  rsassa := TLbRSASSA.Create(nil);
  evt := TRSASSABlockEvent.Create(string(sig));
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.PublicKey.Passphrase:= string(pubPass);
    rsassa.PublicKey.LoadFromFile(string(pubPath));
    rsassa.OnGetSignature:= @evt.evtGetSignatre;
    ret := rsassa.VerifyFile(string(filePath));
    Result := ifthen(ret, 0, 1);
  finally
    rsassa.Free;
    evt.Free;
  end;
end;

function rsassaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar;
  pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
begin
  Result := _rsassaGenerateKeys(keySize, pubPass, privPass, pubSavePath,privSavePath);
end;

function rsassaSignString(keySize: Integer; hashMethod: Integer;
  privPass: PChar; privPath: PChar; str: PChar): PChar; stdcall;
begin
  Result := _rsassaSignString(keySize, hashMethod, privPass, privPath, str);
end;

function rsassaSignFile(keySize: Integer; hashMethod: Integer; privPass: PChar;
  privPath: PChar; filePath: PChar): PChar; stdcall;
begin
  Result := _rsassaSignFile(keySize, hashMethod, privPass, privPath, filePath);
end;

function rsassaVerifyString(keySize: Integer; hashMethod: Integer;
  pubPass: PChar; pubPath: PChar; sig: PChar; str: PChar): Integer; stdcall;
begin
  Result := _rsassaVerifyString(keySize, hashMethod, pubPass, pubPath, sig, str);
end;

function rsassaVerifyFile(keySize: Integer; hashMethod: Integer;
  pubPass: PChar; pubPath: PChar; sig: PChar; filePath: PChar): Integer;
  stdcall;
begin
  Result := _rsassaVerifyFile(keySize, hashMethod, pubPass, pubPath, sig, filePath);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaGenerateKeys(
  env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring;
  privPass: jstring; pubSavePath: jstring; privSavePath: jstring): jint;
  stdcall;
begin
  Result := _rsassaGenerateKeys(keySize, PChar(jstringToString(env, pubPass)), PChar(jstringToString(env, privPass)), PChar(jstringToString(env, pubSavePath)), PChar(jstringToString(env, privSavePath)));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaSignString(
  env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint;
  privPass: jstring; privPath: jstring; str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _rsassaSignString(keySize, hashMethod, PChar(privPass), PChar(privPath), PChar(str));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaSignFile(
  env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint;
  privPass: jstring; privPath: jstring; filePath: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _rsassaSignFile(keySize, hashMethod, PChar(privPass), PChar(privPath), PChar(filePath));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaVerifyString(
  env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint;
  pubPass: jstring; pubPath: jstring; sig: jstring; str: jstring): jint;
  stdcall;
begin
  Result := _rsassaVerifyString(keySize, hashMethod, PChar(jstringToString(env, pubPass)), PChar(jstringToString(env, pubPath)), PChar(jstringToString(env, sig)), PChar(jstringToString(env, str)));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_rsassaVerifyFile(
  env: PJNIEnv; obj: jobject; keySize: jint; hashMethod: jint;
  pubPass: jstring; pubPath: jstring; sig: jstring; filePath: jstring): jint;
  stdcall;
begin
  Result := _rsassaVerifyFile(keySize, hashMethod, PChar(jstringToString(env, pubPass)), PChar(jstringToString(env, pubPath)), PChar(jstringToString(env, sig)), PChar(jstringToString(env, filePath)));
end;

{ TRSASSABlockEvent }

constructor TRSASSABlockEvent.Create(sig: string);
begin
  FSig:= sig;
end;

procedure TRSASSABlockEvent.evtGetSignatre(Sender: TObject;
  var Sig: TRSASignatureBlock);
begin
  HexToBuffer(FSig, Sig, SizeOf(Sig));
end;

end.

