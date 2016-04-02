{$DEFINE DEBUG}

{$IFDEF DEBUG}
program alg;
{$ELSE}
library alg;
{$ENDIF}

{$mode objfpc}{$H+}

uses
  Classes, sysutils, sec_md5, sec_sha1, sec_lmd, sec_elf, sec_des;

exports
  _md5EncryptString,
  _md5EncryptFile,
  _sha1EncryptString,
  _sha1EncryptFile,
  _lmdEncryptString,
  _lmdEncryptFile,
  _elfEncryptString,
  _desEncryptString,
  _desDecryptString,
  md5EncryptString,
  md5EncryptFile,
  sha1EncryptString,
  sha1EncryptFile,
  lmdEncryptString,
  lmdEncryptFile,
  elfEncryptString,
  desEncryptString,
  desDecryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_md5EncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_md5EncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_sha1EncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_sha1EncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_lmdEncryptFile,
  Java_com_hujiang_devart_security_AlgorithmUtils_elfEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_desEncryptString,
  Java_com_hujiang_devart_security_AlgorithmUtils_desDecryptString;

{$IFDEF DEBUG}
var
  m: string = '';    // method: md5, sha1, etc... (-md5, -sha1, etc...)
  t: string = '';    // type: string, file (-f, -s)  || encrypt string (-es), file(-ef), decrypt string(-ds), file(-df)
  p: string = '';    //  object to be operated (string or filepath)
  ret: string;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  if ParamCount <> 3 then Exit;
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
  end;
  WriteLn(ret);
  {$ENDIF}
end.

