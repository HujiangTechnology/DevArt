unit NativeAlgorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dynlibs;

const
  LIB_NAME = {$IFDEF WINDOWS}'alg.dll'{$ELSE}{$IFDEF DARWIN}'libalg.dylib'{$ELSE}'libalg.so'{$ENDIF}{$ENDIF};

type
  // Hash ==============================================================================
  TExMd5EncryptString = function (str: PChar): PChar; cdecl;
  TExMd5EncryptFile = function (filePath: PChar): PChar; cdecl;
  TExSha1EncryptString = function (str: PChar): PChar; cdecl;
  TExSha1EncryptFile = function (filePath: PChar): PChar; cdecl;
  TExLmdEncryptString = function (str: PChar): PChar; cdecl;
  TExLmdEncryptFile = function (filePath: PChar): PChar; cdecl;
  TExElfEncryptString = function (str: PChar): PChar; cdecl;

  // Base64 ==============================================================================
  TExBase64EncryptString = function (str: PChar): PChar; cdecl;
  TExBase64DecryptString = function (str: PChar): PChar; cdecl;

  // RSA ==============================================================================
  // keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
  // return Integer: succ = 0, other = error code
  TExRsaGenerateKeys = function (keySize: Integer; pubPass: PChar; privPass: PChar; pubSavePath: PChar; privSavePath: PChar): Integer; cdecl;
  TExRsaEncryptString = function (keySize: Integer; pubPass: PChar; pubPath: PChar; str: PChar): PChar; cdecl;
  TExRsaEncryptFile = function (keySize: Integer; pubPass: PChar; pubPath: PChar; filePath: PChar; outFilePath: PChar): Integer; cdecl;
  TExRsaDecryptString = function (keySize: Integer; privPass: PChar; privPath: PChar; str: PChar): PChar; cdecl;
  TExRsaDecryptFile = function (keySize: Integer; privPass: PChar; privPath: PChar; filePath: PChar; outFilePath: PChar): Integer; cdecl;
  TExRsaGetPubkeyModules = function (keySize: Integer; pubPass: PChar; pubPath: PChar): PChar; cdecl;
  TExRsaGetPrivkeyModules = function (keySize: Integer; privPass: PChar; privPath: PChar): PChar; cdecl;

  // DSA ==============================================================================
  // keysize:  (aks128 = 0, aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
  // return Integer: succ = 0, other = error code
  TExDsaGenerateKeys = function (keySize: Integer; pubPass: PChar; privPass: PChar; pubSavePath: PChar; privSavePath: PChar): Integer; cdecl;
  TExDsaSignString = function (keySize: Integer; privPass: PChar; privPath: PChar; str: PChar): PChar; cdecl;
  TExDsaSignFile = function (keySize: Integer; privPass: PChar; privPath: PChar; filePath: PChar): PChar; cdecl;
  TExDsaVerifyString = function (keySize: Integer; pubPass: PChar; pubPath: PChar; rs: PChar; str: PChar): Integer;cdecl;
  TExDsaVerifyFile = function (keySize: Integer; pubPass: PChar; pubPath: PChar; rs: PChar; filePath: PChar): Integer; cdecl;
  TExDsaGetPubkeyQPGY = function (keySize: Integer; pubPass: PChar; pubPath: PChar): PChar; cdecl;
  TExDsaGetPrivkeyQPGX = function (keySize: Integer; privPass: PChar; privPath: PChar): PChar; cdecl;

  // RDL ==============================================================================
  // keySize: (ks128 = 0, ks192 = 1, ks256 = 2);
  // cipherMode: (cmECB = 0, cmCBC = 1);
  // return Integer: succ = 0, other = error code
  TExRdlEncryptString = function (keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; cdecl;
  TExRdlEncryptFile = function (keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; cdecl;
  TExRdlDecryptString = function (keySize: Integer; cipherMode: Integer; key: PChar; str: PChar): PChar; cdecl;
  TExRdlDecryptFile = function (keySize: Integer; cipherMode: Integer; key: PChar; filePath: PChar; outFilePath: PChar): Integer; cdecl;

  // RSASSA ==============================================================================
  // keysize:  (aks256 = 1, aks512 = 2, aks768 = 3, aks1024 = 4)
  // hashMethod : (md5 = 0, sha1 = 1)
  // return Integer: succ = 0, other = error code
  TExRsassaGenerateKeys = function (keySize: Integer; pubPass: PChar; privPass: PChar; pubSavePath: PChar; privSavePath: PChar): Integer; cdecl;
  TExRsassaSignString = function (keySize: Integer; hashMethod: Integer; privPass: PChar; privPath: PChar; str: PChar): PChar; cdecl;
  TExRsassaSignFile = function (keySize: Integer; hashMethod: Integer; privPass: PChar; privPath: PChar; filePath: PChar): PChar; cdecl;
  TExRsassaVerifyString = function (keySize: Integer; hashMethod: Integer; pubPass: PChar; pubPath: PChar; sig: PChar; str: PChar): Integer; cdecl;
  TExRsassaVerifyFile = function (keySize: Integer; hashMethod: Integer; pubPass: PChar; pubPath: PChar; sig: PChar; filePath: PChar): Integer; cdecl;

  // AES ================================================================================
  TExAesEncryptECB128 = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptECB192 = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptECB256 = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptECB128Exp = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptECB192Exp = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptECB256Exp = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptCBC128 = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptCBC192 = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptCBC256 = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptCBC128Exp = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptCBC192Exp = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesEncryptCBC256Exp = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptECB128 = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptECB192 = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptECB256 = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptECB128Exp = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptECB192Exp = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptECB256Exp = function (key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptCBC128 = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptCBC192 = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptCBC256 = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptCBC128Exp = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptCBC192Exp = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;
  TExAesDecryptCBC256Exp = function (init: PChar; key: PChar; src: PChar): PChar; cdecl;

var
  // Hash ==============================================================================
  mMd5EncryptString: TExMd5EncryptString;
  mMd5EncryptFile: TExMd5EncryptFile;
  mSha1EncryptString: TExSha1EncryptString;
  mSha1EncryptFile: TExSha1EncryptFile;
  mLmdEncryptString: TExLmdEncryptString;
  mLmdEncryptFile: TExLmdEncryptFile;
  mElfEncryptString: TExElfEncryptString;

  // Base64 ==============================================================================
  mBase64EncryptString: TExBase64EncryptString;
  mBase64DecryptString: TExBase64DecryptString;

  // RSA ==============================================================================
  mRsaGenerateKeys: TExRsaGenerateKeys;
  mRsaEncryptString: TExRsaEncryptString;
  mRsaEncryptFile: TExRsaEncryptFile;
  mRsaDecryptString: TExRsaDecryptString;
  mRsaDecryptFile: TExRsaDecryptFile;
  mRsaGetPubkeyModules: TExRsaGetPubkeyModules;
  mRsaGetPrivkeyModules: TExRsaGetPrivkeyModules;

  // DSA ==============================================================================
  mDsaGenerateKeys: TExDsaGenerateKeys;
  mDsaSignString: TExDsaSignString;
  mDsaSignFile: TExDsaSignFile;
  mDsaVerifyString: TExDsaVerifyString;
  mDsaVerifyFile: TExDsaVerifyFile;
  mDsaGetPubkeyQPGY: TExDsaGetPubkeyQPGY;
  mDsaGetPrivkeyQPGX: TExDsaGetPrivkeyQPGX;

  // RDL ==============================================================================
  // keySize: (ks128 = 0, ks192 = 1, ks256 = 2);
  // cipherMode: (cmECB = 0, cmCBC = 1);
  // return Integer: succ = 0, other = error code
  mRdlEncryptString: TExRdlEncryptString;
  mRdlEncryptFile: TExRdlEncryptFile;
  mRdlDecryptString: TExRdlDecryptString;
  mRdlDecryptFile: TExRdlDecryptFile;

  // RSASSA ==============================================================================
  mRsassaGenerateKeys: TExRsassaGenerateKeys;
  mRsassaSignString: TExRsassaSignString;
  mRsassaSignFile: TExRsassaSignFile;
  mRsassaVerifyString: TExRsassaVerifyString;
  mRsassaVerifyFile: TExRsassaVerifyFile;

  // AES ================================================================================
  mAesEncryptECB128: TExAesEncryptECB128;
  mAesEncryptECB192: TExAesEncryptECB192;
  mAesEncryptECB256: TExAesEncryptECB256;
  mAesEncryptECB128Exp: TExAesEncryptECB128Exp;
  mAesEncryptECB192Exp: TExAesEncryptECB192Exp;
  mAesEncryptECB256Exp: TExAesEncryptECB256Exp;
  mAesEncryptCBC128: TExAesEncryptCBC128;
  mAesEncryptCBC192: TExAesEncryptCBC192;
  mAesEncryptCBC256: TExAesEncryptCBC256;
  mAesEncryptCBC128Exp: TExAesEncryptCBC128Exp;
  mAesEncryptCBC192Exp: TExAesEncryptCBC192Exp;
  mAesEncryptCBC256Exp: TExAesEncryptCBC256Exp;
  mAesDecryptECB128: TExAesDecryptECB128;
  mAesDecryptECB192: TExAesDecryptECB192;
  mAesDecryptECB256: TExAesDecryptECB256;
  mAesDecryptECB128Exp: TExAesDecryptECB128Exp;
  mAesDecryptECB192Exp: TExAesDecryptECB192Exp;
  mAesDecryptECB256Exp: TExAesDecryptECB256Exp;
  mAesDecryptCBC128: TExAesDecryptCBC128;
  mAesDecryptCBC192: TExAesDecryptCBC192;
  mAesDecryptCBC256: TExAesDecryptCBC256;
  mAesDecryptCBC128Exp: TExAesDecryptCBC128Exp;
  mAesDecryptCBC192Exp: TExAesDecryptCBC192Exp;
  mAesDecryptCBC256Exp: TExAesDecryptCBC256Exp;

implementation

var
  hLib: TLibHandle;
  libPath: string;

initialization
  libPath := ExtractFilePath(ParamStr(0)) + LIB_NAME;
  if FileExists(libPath) then begin
    hLib:= LoadLibrary(libPath);
    if hLib <> 0 then begin
      mMd5EncryptString:= TExMd5EncryptString(GetProcAddress(hLib, 'md5EncryptString'));
      mMd5EncryptFile:= TExMd5EncryptFile(GetProcAddress(hLib, 'md5EncryptFile'));
      mSha1EncryptString:= TExSha1EncryptString(GetProcAddress(hLib, 'sha1EncryptString'));
      mSha1EncryptFile:= TExSha1EncryptFile(GetProcAddress(hLib, 'sha1EncryptFile'));
      mLmdEncryptString:= TExLmdEncryptString(GetProcAddress(hLib, 'lmdEncryptString'));
      mLmdEncryptFile:= TExLmdEncryptFile(GetProcAddress(hLib, 'lmdEncryptFile'));
      mElfEncryptString:= TExElfEncryptString(GetProcAddress(hLib, 'elfEncryptString'));

      mBase64EncryptString:= TExBase64EncryptString(GetProcAddress(hLib, 'base64EncryptString'));
      mBase64DecryptString:= TExBase64DecryptString(GetProcAddress(hLib, 'base64DecryptString'));

      mRsaGenerateKeys:= TExRsaGenerateKeys(GetProcAddress(hLib, 'rsaGenerateKeys'));
      mRsaEncryptString:= TExRsaEncryptString(GetProcAddress(hLib, 'rsaEncryptString'));
      mRsaEncryptFile:= TExRsaEncryptFile(GetProcAddress(hLib, 'rsaEncryptFile'));
      mRsaDecryptString:= TExRsaDecryptString(GetProcAddress(hLib, 'rsaDecryptString'));
      mRsaDecryptFile:= TExRsaDecryptFile(GetProcAddress(hLib, 'rsaDecryptFile'));
      mRsaGetPubkeyModules:= TExRsaGetPubkeyModules(GetProcAddress(hLib, 'rsaGetPubkeyModules'));
      mRsaGetPrivkeyModules:= TExRsaGetPrivkeyModules(GetProcAddress(hLib, 'rsaGetPrivkeyModules'));

      mDsaGenerateKeys:= TExDsaGenerateKeys(GetProcAddress(hLib, 'dsaGenerateKeys'));
      mDsaSignString:= TExDsaSignString(GetProcAddress(hLib, 'dsaSignString'));
      mDsaSignFile:= TExDsaSignFile(GetProcAddress(hLib, 'dsaSignFile'));
      mDsaVerifyString:= TExDsaVerifyString(GetProcAddress(hLib, 'dsaVerifyString'));
      mDsaVerifyFile:= TExDsaVerifyFile(GetProcAddress(hLib, 'dsaVerifyFile'));
      mDsaGetPubkeyQPGY:= TExDsaGetPubkeyQPGY(GetProcAddress(hLib, 'dsaGetPubkeyQPGY'));
      mDsaGetPrivkeyQPGX:= TExDsaGetPrivkeyQPGX(GetProcAddress(hLib, 'dsaGetPrivkeyQPGX'));

      mRdlEncryptString:= TExRdlEncryptString(GetProcAddress(hLib, 'rdlEncryptString'));
      mRdlEncryptFile:= TExRdlEncryptFile(GetProcAddress(hLib, 'rdlEncryptFile'));
      mRdlDecryptString:= TExRdlDecryptString(GetProcAddress(hLib, 'rdlDecryptString'));
      mRdlDecryptFile:= TExRdlDecryptFile(GetProcAddress(hLib, 'rdlDecryptFile'));

      mRsassaGenerateKeys:= TExRsassaGenerateKeys(GetProcAddress(hLib, 'rsassaGenerateKeys'));
      mRsassaSignString:= TExRsassaSignString(GetProcAddress(hLib, 'rsassaSignString'));
      mRsassaSignFile:= TExRsassaSignFile(GetProcAddress(hLib, 'rsassaSignFile'));
      mRsassaVerifyString:= TExRsassaVerifyString(GetProcAddress(hLib, 'rsassaVerifyString'));
      mRsassaVerifyFile:= TExRsassaVerifyFile(GetProcAddress(hLib, 'rsassaVerifyFile'));

      mAesEncryptECB128:= TExAesEncryptECB128(GetProcAddress(hLib, 'aesEncryptECB128'));
      mAesEncryptECB192:= TExAesEncryptECB192(GetProcAddress(hLib, 'aesEncryptECB192'));
      mAesEncryptECB256:= TExAesEncryptECB256(GetProcAddress(hLib, 'aesEncryptECB256'));
      mAesEncryptECB128Exp:= TExAesEncryptECB128Exp(GetProcAddress(hLib, 'aesEncryptECB128Exp'));
      mAesEncryptECB192Exp:= TExAesEncryptECB192Exp(GetProcAddress(hLib, 'aesEncryptECB192Exp'));
      mAesEncryptECB256Exp:= TExAesEncryptECB256Exp(GetProcAddress(hLib, 'aesEncryptECB256Exp'));
      mAesEncryptCBC128:= TExAesEncryptCBC128(GetProcAddress(hLib, 'aesEncryptCBC128'));
      mAesEncryptCBC192:= TExAesEncryptCBC192(GetProcAddress(hLib, 'aesEncryptCBC192'));
      mAesEncryptCBC256:= TExAesEncryptCBC256(GetProcAddress(hLib, 'aesEncryptCBC256'));
      mAesEncryptCBC128Exp:= TExAesEncryptCBC128Exp(GetProcAddress(hLib, 'aesEncryptCBC128Exp'));
      mAesEncryptCBC192Exp:= TExAesEncryptCBC192Exp(GetProcAddress(hLib, 'aesEncryptCBC192Exp'));
      mAesEncryptCBC256Exp:= TExAesEncryptCBC256Exp(GetProcAddress(hLib, 'aesEncryptCBC256Exp'));
      mAesDecryptECB128:= TExAesDecryptECB128(GetProcAddress(hLib, 'aesDecryptECB128'));
      mAesDecryptECB192:= TExAesDecryptECB192(GetProcAddress(hLib, 'aesDecryptECB192'));
      mAesDecryptECB256:= TExAesDecryptECB256(GetProcAddress(hLib, 'aesDecryptECB256'));
      mAesDecryptECB128Exp:= TExAesDecryptECB128Exp(GetProcAddress(hLib, 'aesDecryptECB128Exp'));
      mAesDecryptECB192Exp:= TExAesDecryptECB192Exp(GetProcAddress(hLib, 'aesDecryptECB192Exp'));
      mAesDecryptECB256Exp:= TExAesDecryptECB256Exp(GetProcAddress(hLib, 'aesDecryptECB256Exp'));
      mAesDecryptCBC128:= TExAesDecryptCBC128(GetProcAddress(hLib, 'aesDecryptCBC128'));
      mAesDecryptCBC192:= TExAesDecryptCBC192(GetProcAddress(hLib, 'aesDecryptCBC192'));
      mAesDecryptCBC256:= TExAesDecryptCBC256(GetProcAddress(hLib, 'aesDecryptCBC256'));
      mAesDecryptCBC128Exp:= TExAesDecryptCBC128Exp(GetProcAddress(hLib, 'aesDecryptCBC128Exp'));
      mAesDecryptCBC192Exp:= TExAesDecryptCBC192Exp(GetProcAddress(hLib, 'aesDecryptCBC192Exp'));
      mAesDecryptCBC256Exp:= TExAesDecryptCBC256Exp(GetProcAddress(hLib, 'aesDecryptCBC256Exp'));

    end;

  end;

finalization
  mMd5EncryptString := nil;
  mMd5EncryptFile:= nil;
  mSha1EncryptString:= nil;
  mSha1EncryptFile:= nil;
  mLmdEncryptString:= nil;
  mLmdEncryptFile:= nil;
  mElfEncryptString:= nil;

  mBase64EncryptString:= nil;
  mBase64DecryptString:= nil;

  mRsaGenerateKeys:= nil;
  mRsaEncryptString:= nil;
  mRsaEncryptFile:= nil;
  mRsaDecryptString:= nil;
  mRsaDecryptFile:= nil;
  mRsaGetPubkeyModules:= nil;
  mRsaGetPrivkeyModules:= nil;

  mDsaGenerateKeys:= nil;
  mDsaSignString:= nil;
  mDsaSignFile:= nil;
  mDsaVerifyString:= nil;
  mDsaVerifyFile:= nil;
  mDsaGetPubkeyQPGY:= nil;
  mDsaGetPrivkeyQPGX:= nil;

  mRdlEncryptString:= nil;
  mRdlEncryptFile:= nil;
  mRdlDecryptString:= nil;
  mRdlDecryptFile:= nil;

  mRsassaGenerateKeys:= nil;
  mRsassaSignString:= nil;
  mRsassaSignFile:= nil;
  mRsassaVerifyString:= nil;
  mRsassaVerifyFile:= nil;

  mAesEncryptECB128:= nil;
  mAesEncryptECB192:= nil;
  mAesEncryptECB256:= nil;
  mAesEncryptECB128Exp:= nil;
  mAesEncryptECB192Exp:= nil;
  mAesEncryptECB256Exp:= nil;
  mAesEncryptCBC128:= nil;
  mAesEncryptCBC192:= nil;
  mAesEncryptCBC256:= nil;
  mAesEncryptCBC128Exp:= nil;
  mAesEncryptCBC192Exp:= nil;
  mAesEncryptCBC256Exp:= nil;
  mAesDecryptECB128:= nil;
  mAesDecryptECB192:= nil;
  mAesDecryptECB256:= nil;
  mAesDecryptECB128Exp:= nil;
  mAesDecryptECB192Exp:= nil;
  mAesDecryptECB256Exp:= nil;
  mAesDecryptCBC128:= nil;
  mAesDecryptCBC192:= nil;
  mAesDecryptCBC256:= nil;
  mAesDecryptCBC128Exp:= nil;
  mAesDecryptCBC192Exp:= nil;
  mAesDecryptCBC256Exp:= nil;

  if (hLib <> 0) then begin
    FreeLibrary(hLib);
  end;

end.

