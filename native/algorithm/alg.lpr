{.$DEFINE DEBUG}

{$IFDEF DEBUG}
program alg;
{$ELSE}
library alg;
{$ENDIF}

{$mode objfpc}{$H+}

uses
  Classes, sysutils, sec_md5, sec_sha1, sec_lmd, sec_elf, sec_des, sec_base64,
  sec_rsa, sec_dsa, sec_rdl, sec_rsassa;

exports
// for iOS
  _md5EncryptString,
  _md5EncryptFile,
  _sha1EncryptString,
  _sha1EncryptFile,
  _lmdEncryptString,
  _lmdEncryptFile,
  _elfEncryptString,
  _desEncryptString,
  _desDecryptString,
  _base64EncryptString,
  _base64DecryptString,
  _rsaGenerateKeys,
  _rsaEncryptString,
  _rsaEncryptFile,
  _rsaDecryptString,
  _rsaDecryptFile,
  _rsaGetPubkeyModules,
  _rsaGetPrivkeyModules,
  _dsaGenerateKeys,
  _dsaSignString,
  _dsaSignFile,
  _dsaVerifyString,
  _dsaVerifyFile,
  _dsaGetPubkeyQPGY,
  _dsaGetPrivkeyQPGX,
  _rdlEncryptString,
  _rdlEncryptFile,
  _rdlDecryptString,
  _rdlDecryptFile,
  _rsassaGenerateKeys,
  _rsassaSignString,
  _rsassaSignFile,
  _rsassaVerifyString,
  _rsassaVerifyFile,

// for cross
  md5EncryptString,
  md5EncryptFile,
  sha1EncryptString,
  sha1EncryptFile,
  lmdEncryptString,
  lmdEncryptFile,
  elfEncryptString,
  desEncryptString,
  desDecryptString,
  base64EncryptString,
  base64DecryptString,
  rsaGenerateKeys,
  rsaEncryptString,
  rsaEncryptFile,
  rsaDecryptString,
  rsaDecryptFile,
  rsaGetPubkeyModules,
  rsaGetPrivkeyModules,
  dsaGenerateKeys,
  dsaSignString,
  dsaSignFile,
  dsaVerifyString,
  dsaVerifyFile,
  dsaGetPubkeyQPGY,
  dsaGetPrivkeyQPGX,
  rdlEncryptString,
  rdlEncryptFile,
  rdlDecryptString,
  rdlDecryptFile,
  rsassaGenerateKeys,
  rsassaSignString,
  rsassaSignFile,
  rsassaVerifyString,
  rsassaVerifyFile,

// for jni
  Java_com_hujiang_devart_security_AlgorithmUtils_md5EncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_md5EncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_sha1EncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_sha1EncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_elfEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_desEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_desDecryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_base64EncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_base64DecryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsaGenerateKeys,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsaEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsaEncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsaDecryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsaDecryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsaGetPubkeyModules,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsaGetPrivkeyModules,
  Java_com_hujiang_devart_security_AlgorithmUtils_dsaGenerateKeys,
  Java_com_hujiang_devart_security_AlgorithmUtils_dsaSignString,
  Java_com_hujiang_devart_security_AlgorithmUtils_dsaSignFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_dsaVerifyString,
  Java_com_hujiang_devart_security_AlgorithmUtils_dsaVerifyFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_dsaGetPubkeyQPGY,
  Java_com_hujiang_devart_security_AlgorithmUtils_dsaGetPrivkeyQPGX,
  Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_rdlEncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_rdlDecryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsassaGenerateKeys,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsassaSignString,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsassaSignFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsassaVerifyString,
  Java_com_hujiang_devart_security_AlgorithmUtils_rsassaVerifyFile;

{$IFDEF DEBUG}
var
  m: string = '';    // method: md5, sha1, etc... (-md5, -sha1, etc...)
  t: string = '';    // type: string, file (-f, -s)  || encrypt string (-es), file(-ef), decrypt string(-ds), file(-df)
  p: string = '';    //  object to be operated (string or filepath)
  ret: string;

  // RSA
  keySize: Integer;
  pubPass: string;
  pubPath: string;
  privPass: string;
  privPath: string;
  rsaStr: string;
  rsaFile: string;
  rsaOutfile: string;

  // DSA
  dsaStr: string;
  dsaRS: string;
  dsaFile: string;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  try
    m := ParamStr(1);
    t := ParamStr(2);
    p := ParamStr(3);

    if (m = '-md5') then begin
      if (t = '-s') then begin
        ret := string(md5EncryptString(PChar(p)));
      end else if (t = '-f') then begin
        ret := String(md5EncryptFile(PChar(p)));
      end;
    end else if (m = '-sha1') then begin
      if (t = '-s') then begin
        ret  := string(sha1EncryptString(PChar(p)));
      end else if (t = '-f') then begin
        ret := string(sha1EncryptFile(PChar(p)));
      end;
    end else if (m = '-lmd') then begin
      if (t = '-s') then begin
        ret := String(lmdEncryptString(PChar(p)));
      end else if (t = '-f') then begin
        ret := string(lmdEncryptFile(PChar(p)));
      end;
    end else if (m = '-elf') then begin
      if (t = '-s') then begin
        ret  := string(elfEncryptString(PChar(p)));
      end;
    end else if (m = '-rsa') then begin
      keySize:= StrToInt(RightStr(ParamStr(3), 1));
      if (t = '-g') then begin
        pubPass:= ParamStr(4);
        pubPath:= ParamStr(5);
        privPass:= ParamStr(6);
        privPath:= ParamStr(7);
        ret := IntToStr(rsaGenerateKeys(keySize, PChar(pubPass), PChar(privPass), PChar(pubPath), PChar(privPath)));
      end else if (t = '-es') then begin
        pubPass:= ParamStr(4);
        pubPath:= ParamStr(5);
        rsaStr:= ParamStr(6);
        ret := string(rsaEncryptString(keySize, PChar(pubPass), PChar(pubPath), PChar(rsaStr)));
      end else if (t = '-ef') then begin
        pubPass:= ParamStr(4);
        pubPath:= ParamStr(5);
        rsaFile:= ParamStr(6);
        rsaOutfile:= ParamStr(7);
        ret := IntToStr(rsaEncryptFile(keySize, PChar(pubPass), PChar(pubPath), PChar(rsaFile), PChar(rsaOutfile)));
      end else if (t = '-ds') then begin
        privPass:= ParamStr(4);
        privPath:= ParamStr(5);
        rsaStr:= ParamStr(6);
        ret := string(rsaDecryptString(keySize, PChar(privPass), PChar(privPath), PChar(rsaStr)));
      end else if (t = '-df') then begin
          privPass:= ParamStr(4);
          privPath:= ParamStr(5);
          rsaFile:= ParamStr(6);
          rsaOutfile:= ParamStr(7);
          ret := IntToStr(rsaDecryptFile(keySize, PChar(privPass), PChar(privPath), PChar(rsaFile), PChar(rsaOutfile)));
      end;
    end else if (m = '-dsa') then begin
      keySize:= StrToInt(RightStr(ParamStr(3), 1));
      if (t = '-g') then begin
        pubPass:= ParamStr(4);
        pubPath:= ParamStr(5);
        privPass:= ParamStr(6);
        privPath:= ParamStr(7);
        ret := IntToStr(dsaGenerateKeys(keySize, PChar(pubPass), PChar(privPass), PChar(pubPath), PChar(privPath)));
      end else if (t = '-es') then begin
        privPass:= ParamStr(4);
        privPath:= ParamStr(5);
        dsaStr:= ParamStr(6);
        ret := string(dsaSignString(keySize, PChar(privPass), PChar(privPath), PChar(dsaStr)));
      end else if (t = '-ef') then begin
        privPass:= ParamStr(4);
        privPath:= ParamStr(5);
        dsaFile:= ParamStr(6);
        ret := string(dsaSignFile(keySize, PChar(privPass), PChar(privPath), PChar(dsaFile)));
      end else if (t = '-ds') then begin
        pubPass:= ParamStr(4);
        pubPath:= ParamStr(5);
        dsaRS:= ParamStr(6);
        dsaStr:= ParamStr(7);
        ret := IntToStr(dsaVerifyString(keySize, PChar(pubPass), PChar(pubPath), PChar(dsaRS), PChar(dsaStr)));
      end else if (t = '-df') then begin
        pubPass:= ParamStr(4);
        pubPath:= ParamStr(5);
        dsaRS:= ParamStr(6);
        dsaFile:= ParamStr(7);
        ret := IntToStr(dsaVerifyFile(keySize, PChar(pubPass), PChar(pubPath), PChar(dsaRS), PChar(dsaFile)));
      end;
    end;
    WriteLn(ret);

  except
  end;
  {$ENDIF}
end.

