unit sec_dsa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni2, jni_utils, LbDSA, LbCipher, LbAsym, LbUtils, math;

type

  { TDsaBlockEvent }

  TDsaBlockEvent = class
  private
    FDsaR: string;
    FDSaS: string;
  public
    constructor Create(dsaRS: string);
    procedure evtDsaGetR(Sender: TObject; var Block: TLbDSABlock);
    procedure evtDsaGetS(Sender: TObject; var Block: TLbDSABlock);
  end;

// keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
// return Integer: succ = 0, other = error code
function _dsaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar; pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
function _dsaSignString(keySize: Integer; privPass: PChar; privPath: PChar; str: PChar): PChar; stdcall;
function _dsaSignFile(keySize: Integer; privPass: PChar; privPath: PChar; filePath: PChar): PChar; stdcall;
function _dsaVerifyString(keySize: Integer; pubPass: PChar; pubPath: PChar; rs: PChar; str: PChar): Integer; stdcall;
function _dsaVerifyFile(keySize: Integer; pubPass: PChar; pubPath: PChar; rs: PChar; filePath: PChar): Integer; stdcall;
function _dsaGetPubkeyQPGY(keySize: Integer; pubPass: PChar; pubPath: PChar): PChar; stdcall;
function _dsaGetPrivkeyQPGX(keySize: Integer; privPass: PChar; privPath: PChar): PChar; stdcall;
function dsaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar; pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
function dsaSignString(keySize: Integer; privPass: PChar; privPath: PChar; str: PChar): PChar; stdcall;
function dsaSignFile(keySize: Integer; privPass: PChar; privPath: PChar; filePath: PChar): PChar; stdcall;
function dsaVerifyString(keySize: Integer; pubPass: PChar; pubPath: PChar; rs: PChar; str: PChar): Integer; stdcall;
function dsaVerifyFile(keySize: Integer; pubPass: PChar; pubPath: PChar; rs: PChar; filePath: PChar): Integer; stdcall;
function dsaGetPubkeyQPGY(keySize: Integer; pubPass: PChar; pubPath: PChar): PChar; stdcall;
function dsaGetPrivkeyQPGX(keySize: Integer; privPass: PChar; privPath: PChar): PChar; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_dsaGenerateKeys(env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring; privPass: jstring; pubSavePath: jstring; privSavePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_dsaSignString(env: PJNIEnv; obj: jobject; keySize: jint; privPass: jstring; privPath: jstring; str: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_dsaSignFile(env: PJNIEnv; obj: jobject; keySize: jint; privPass: jstring; privPath: jstring; filePath: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_dsaVerifyString(env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring; pubPath: jstring; rs: PChar; str: jstring): jint; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_dsaVerifyFile(env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring; pubPath: jstring; rs: PChar; filePath: jstring): jint; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_dsaGetPubkeyQPGY(env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring; pubPath: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_dsaGetPrivkeyQPGX(env: PJNIEnv; obj: jobject; keySize: jint; privPass: jstring; privPath: jstring): jstring; stdcall;

implementation

function _dsaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar;
  pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
var
  dsa: TLbDSA;
begin
  Result := -1;
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.GenerateKeyPair;
    dsa.PublicKey.Passphrase:= String(pubPass);
    dsa.PublicKey.StoreToFile(string(pubSavePath));
    dsa.PrivateKey.Passphrase:= String(privPass);
    dsa.PrivateKey.StoreToFile(string(privSavePath));
    Result := 0;
  finally
    dsa.Free;;
  end;
end;

function _dsaSignString(keySize: Integer; privPass: PChar; privPath: PChar;
  str: PChar): PChar; stdcall;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PrivateKey.Passphrase:= string(privPass);
    dsa.PrivateKey.LoadFromFile(string(privPath));
    dsa.SignString(string(str));
    ret := Format('%s|%s', [dsa.SignatureR.IntStr, dsa.SignatureS.IntStr]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function _dsaSignFile(keySize: Integer; privPass: PChar; privPath: PChar;
  filePath: PChar): PChar; stdcall;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PrivateKey.Passphrase:= string(privPass);
    dsa.PrivateKey.LoadFromFile(string(privPath));
    dsa.SignFile(string(filePath));
    ret := Format('%s|%s', [dsa.SignatureR.IntStr, dsa.SignatureS.IntStr]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function _dsaVerifyString(keySize: Integer; pubPass: PChar; pubPath: PChar;
  rs: PChar; str: PChar): Integer; stdcall;
var
  dsa: TLbDSA;
  evt: TDsaBlockEvent;
  ret: Boolean;
begin
  Result := -1;
  dsa := TLbDSA.Create(nil);
  evt := TDsaBlockEvent.Create(string(rs));
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PublicKey.Passphrase:= string(pubPass);
    dsa.PublicKey.LoadFromFile(pubPath);
    dsa.OnGetR:= @evt.evtDsaGetR;
    dsa.OnGetS:= @evt.evtDsaGetS;
    ret := dsa.VerifyString(string(str));
    Result := ifthen(ret, 0, 1);
  finally
    dsa.Free;
    evt.Free;
  end;
end;

function _dsaVerifyFile(keySize: Integer; pubPass: PChar; pubPath: PChar;
  rs: PChar; filePath: PChar): Integer; stdcall;
var
  dsa: TLbDSA;
  evt: TDsaBlockEvent;
  ret: Boolean;
begin
  Result := -1;
  dsa := TLbDSA.Create(nil);
  evt := TDsaBlockEvent.Create(string(rs));
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PublicKey.Passphrase:= string(pubPass);
    dsa.PublicKey.LoadFromFile(pubPath);
    dsa.OnGetR:= @evt.evtDsaGetR;
    dsa.OnGetS:= @evt.evtDsaGetS;
    ret := dsa.VerifyFile(string(filePath));
    Result := ifthen(ret, 0, 1);
  finally
    dsa.Free;
    evt.Free;
  end;

end;

function _dsaGetPubkeyQPGY(keySize: Integer; pubPass: PChar; pubPath: PChar
  ): PChar; stdcall;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PublicKey.Passphrase:= string(pubPass);
    dsa.PublicKey.LoadFromFile(pubPath);
    ret := Format('%s|%s|%s|%s', [dsa.PublicKey.QAsString, dsa.PublicKey.PAsString, dsa.PublicKey.GAsString, dsa.PublicKey.YAsString]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function _dsaGetPrivkeyQPGX(keySize: Integer; privPass: PChar; privPath: PChar
  ): PChar; stdcall;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PrivateKey.Passphrase:= string(privPass);
    dsa.PrivateKey.LoadFromFile(privPath);
    ret := Format('%s|%s|%s|%s', [dsa.PrivateKey.QAsString, dsa.PrivateKey.PAsString, dsa.PrivateKey.GAsString, dsa.PrivateKey.XAsString]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function dsaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar;
  pubSavePath: PChar; privSavePath: PChar): Integer; stdcall;
begin
  Result := _dsaGenerateKeys(keySize, pubPass, privPass, pubSavePath, privSavePath);
end;

function dsaSignString(keySize: Integer; privPass: PChar; privPath: PChar;
  str: PChar): PChar; stdcall;
begin
  Result := _dsaSignString(keySize, privPass, privPath, str);
end;

function dsaSignFile(keySize: Integer; privPass: PChar; privPath: PChar;
  filePath: PChar): PChar; stdcall;
begin
  Result := _dsaSignFile(keySize, privPass, privPath, filePath);
end;

function dsaVerifyString(keySize: Integer; pubPass: PChar; pubPath: PChar;
  rs: PChar; str: PChar): Integer; stdcall;
begin
  Result := _dsaVerifyString(keySize, pubPass, pubPath, rs, str);
end;

function dsaVerifyFile(keySize: Integer; pubPass: PChar; pubPath: PChar;
  rs: PChar; filePath: PChar): Integer; stdcall;
begin
  Result := _dsaVerifyFile(keySize, pubPass, pubPath, rs, filePath);
end;

function dsaGetPubkeyQPGY(keySize: Integer; pubPass: PChar; pubPath: PChar
  ): PChar; stdcall;
begin
  Result := _dsaGetPubkeyQPGY(keySize, pubPass, pubPath);
end;

function dsaGetPrivkeyQPGX(keySize: Integer; privPass: PChar; privPath: PChar
  ): PChar; stdcall;
begin
  Result := _dsaGetPrivkeyQPGX(keySize, privPass, privPath);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_dsaGenerateKeys(
  env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring;
  privPass: jstring; pubSavePath: jstring; privSavePath: jstring): jint;
  stdcall;
begin
  Result := _dsaGenerateKeys(keySize, PChar(jstringToString(env, pubPass)), PChar(jstringToString(env, privPass)), PChar(jstringToString(env, pubSavePath)), PChar(jstringToString(env, privSavePath)));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_dsaSignString(
  env: PJNIEnv; obj: jobject; keySize: jint; privPass: jstring;
  privPath: jstring; str: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _dsaSignString(keySize, PChar(jstringToString(env, privPass)), PChar(jstringToString(env, privPath)), PChar(jstringToString(env, str)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_dsaSignFile(
  env: PJNIEnv; obj: jobject; keySize: jint; privPass: jstring;
  privPath: jstring; filePath: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _dsaSignFile(keySize, PChar(jstringToString(env, privPass)), PChar(jstringToString(env, privPath)), PChar(jstringToString(env, filePath)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_dsaVerifyString(
  env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring;
  pubPath: jstring; rs: PChar; str: jstring): jint; stdcall;
begin
  Result := _dsaVerifyString(keySize, PChar(jstringToString(env, pubPass)), PChar(jstringToString(env, pubPath)), PChar(jstringToString(env, rs)), PChar(jstringToString(env, str)));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_dsaVerifyFile(
  env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring;
  pubPath: jstring; rs: PChar; filePath: jstring): jint; stdcall;
begin
  Result := _dsaVerifyFile(keySize, PChar(jstringToString(env, pubPass)), PChar(jstringToString(env, pubPath)), PChar(jstringToString(env, rs)), PChar(jstringToString(env, filePath)));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_dsaGetPubkeyQPGY(
  env: PJNIEnv; obj: jobject; keySize: jint; pubPass: jstring; pubPath: jstring
  ): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _dsaGetPubkeyQPGY(keySize, PChar(jstringToString(env, pubPass)), PChar(jstringToString(env, pubPath)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_dsaGetPrivkeyQPGX(
  env: PJNIEnv; obj: jobject; keySize: jint; privPass: jstring;
  privPath: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _dsaGetPrivkeyQPGX(keySize, PChar(jstringToString(env, privPass)), PChar(jstringToString(env, privPath)));
  Result := stringToJString(env, string(ret));
end;

{ TDsaBlockEvent }

constructor TDsaBlockEvent.Create(dsaRS: string);
var
  p: Integer;
begin
  p := Pos('|', dsaRS);
  FDsaR:= LeftStr(dsaRS, p - 1);
  FDSaS:= Copy(dsaRS, p + 1, Length(dsaRS) - p);
end;

procedure TDsaBlockEvent.evtDsaGetR(Sender: TObject; var Block: TLbDSABlock);
begin
  HexToBuffer(FDsaR, Block, SizeOf(Block));
end;

procedure TDsaBlockEvent.evtDsaGetS(Sender: TObject; var Block: TLbDSABlock);
begin
  HexToBuffer(FDsaS, Block, SizeOf(Block));
end;

end.

