unit sec_des;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lockbox, jni2, jni_utils;

function _desEncryptString(str: PChar; key: PChar): PChar; stdcall;
function _desDecryptString(str: PChar; key: PChar): PChar; stdcall;
function desEncryptString(str: PChar; key: PChar): PChar; stdcall;
function desDecryptString(str: PChar; key: PChar): PChar; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_desEncryptString(env: PJNIEnv; obj: jobject; str: jstring; key: jstring): jstring; stdcall;
function Java_com_hujiang_devart_security_AlgorithmUtils_desDecryptString(env: PJNIEnv; obj: jobject; str: jstring; key: jstring): jstring; stdcall;

implementation

type
  TDataBytes = array of Byte;

function StringToBytes(rida:string):TDataBytes;
var
  vaherida:string;
  arv:integer;
  pikkus:integer;
  vahe:integer;
  vahealgus:integer;
  Mitmes:integer;
begin
  vahe:=AnsiPos(';',rida) ;
  if vahe>0 then begin
    rida:=copy(rida,0,vahe-1);
  end;
  vahe:=AnsiPos('/',rida) ;
  if vahe>0 then begin
    rida:=copy(rida,0,vahe-1);
  end;
  vahe:=AnsiPos(':',rida) ;
  if vahe>0 then begin
    rida:=copy(rida,vahe+1,length(rida));
  end;
  vahe:=0 ;
  vahealgus:=0;
  pikkus:=Length(rida);
  mitmes:=0;
  setlength(result,pikkus);
  while vahe<=pikkus do begin
    if IsDelimiter(#32,rida,vahe)=true then begin
      vaherida:=trim(copy(rida,vahealgus,vahe-vahealgus));
      vahealgus:=vahe;
      if length(vaherida)>0 then begin
        if TryStrToInt(vaherida,arv)=false then
          raise exception.Create('Error convert TryStrToIntExt "'+vaherida+'"');
        result[mitmes]:=arv;
        inc(mitmes);
      end;
    end else begin
      if vahe=pikkus then begin
        vaherida:=trim(copy(rida,vahealgus,vahe-vahealgus+1));
        if length(vaherida)>0 then begin
          if TryStrToInt(vaherida,arv)=false then
            raise exception.Create('Error convert TryStrToIntExt "'+vaherida+'"');
          result[mitmes]:=lo(arv);
          inc(mitmes);
        end;
      end;
    end;
    inc(vahe);
  end;
  setlength(result,mitmes);
end;

procedure setDesKey(des: TLb3DES; key: PChar);
var
  aKey: TKey128;
  tmpArr: TDataBytes;
  i: integer;
begin
  tmpArr := StringToBytes(string(key));
  for I := 0 to Length(TmpArr) - 1 do begin
    aKey[I] := tmpArr[I];
  end;
  des.SetKey(aKey);
end;

function _desEncryptString(str: PChar; key: PChar): PChar; stdcall;
var
  des: TLb3DES;
  inBuffer: TDataBytes;
  outBuffer: TDataBytes;
  I: Integer;
  dataLen: Integer;
  S: String = '';
begin
  des := TLb3DES.Create(nil);
  try
    setDesKey(des, key);
    inBuffer := StringToBytes(string(str));
    SetLength(outBuffer, Length(inBuffer) *4);
    dataLen := des.EncryptBuffer(inBuffer[0], Length(InBuffer), outBuffer[0]);
    SetLength(outBuffer, dataLen);
    for I := 0 to Length(outBuffer) - 1 do begin
      S := S + '0x' + IntToHex(outBuffer[I],2) + ' ';
    end;
  finally
    des.Free;
  end;
  Result := StrAlloc(Length(S));
  strcopy(Result, PChar(S));
end;



function _desDecryptString(str: PChar; key: PChar): PChar; stdcall;
var
  des: TLb3DES;
  inBuffer: TDataBytes;
  outBuffer: TDataBytes;
  I, dataLen: Integer;
  S: String = '';
begin
  des := TLb3DES.Create(nil);
  try
    setDesKey(des, key);
    inBuffer := StringToBytes(string(str));
    SetLength(outBuffer, Length(inBuffer) *2);
      dataLen := des.DecryptBuffer(inBuffer[0], Length(InBuffer), outBuffer[0]);
      SetLength(outBuffer, dataLen);
      for I := 0 to Length(outBuffer) - 1 do begin
        S := S + '0x' +IntToHex(outBuffer[I],2) + ' ';
      end;
  finally
    des.Free;
  end;
  Result := StrAlloc(Length(S));
  strcopy(Result, PChar(S));
end;

function desEncryptString(str: PChar; key: PChar): PChar; stdcall;
begin
  Result := _desEncryptString(str, key);
end;

function desDecryptString(str: PChar; key: PChar): PChar; stdcall;
begin
  Result := _desDecryptString(str, key);
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_desEncryptString(
  env: PJNIEnv; obj: jobject; str: jstring; key: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret  := _desEncryptString(PChar(jstringToString(env, str)), PChar(jstringToString(env, key)));
  Result := stringToJString(env, string(ret));
end;

function Java_com_hujiang_devart_security_AlgorithmUtils_desDecryptString(
  env: PJNIEnv; obj: jobject; str: jstring; key: jstring): jstring; stdcall;
var
  ret: PChar;
begin
  ret := _desDecryptString(PChar(jstringToString(env, str)), PChar(jstringToString(env, key)));
  Result := stringToJString(env, string(ret));
end;

end.

