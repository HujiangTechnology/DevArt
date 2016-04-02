{.$DEFINE DEBUG}

{$IFDEF DEBUG}
program blacktech;
{$ELSE}
library blacktech;
{$ENDIF}

{$mode objfpc}{$H+}

uses
  Classes, sysutils, jni2, jni_utils, process, strutils;

function parse(line1, line2: String): String;
var
  p: Integer;
  lName: string;
  lAttr: string;
  lMac: string;
begin
  p := Pos(':', line1);
  line1 := Trim(Copy(line1, p + 1, Length(line1) - p));
  p := Pos(':', line1);
  lName:= LeftStr(line1, p - 1);
  line1:= Trim(Copy(line1, p + 1, Length(line1) - p));
  p := Pos('>', line1);
  lAttr:= LeftStr(line1, p -1);
  lAttr:= Trim(StringReplace(lAttr, '<', '', [rfIgnoreCase, rfReplaceAll]));
  line2:= Trim(line2);
  p := Pos(' ', line2);
  if (p <= 0) then begin
    Result := '';
  end else begin
    line2 := Trim(Copy(line2, p + 1, Length(line2) - p));
    if (line2 = '') then begin
      Result := '';
    end else begin
      p := Pos('brd', line2);
      if (p <= 0) then begin
        lMac:= line2;
      end else begin
        lMac:= Trim(LeftStr(line2, p - 1));
      end;
      Result := Format('%s|%s|%s^', [lName, lAttr, lMac]);
    end;
  end;
end;

function _blackGetMacAddress(): PChar; stdcall;
var
  ret: string = '';
  outStr: string;
  i: Integer = 0;
begin
  if (RunCommand('ip', ['link'], outstr)) then begin
    with TStringList.Create do begin
      Text:= outStr;
      while i < Count - 1 do begin
        ret += parse(Strings[i], Strings[i + 1]);
        i += 2;
      end;
      Free;
    end;
  end;
  if (ret <> '') then begin
    ret := LeftStr(ret, Length(ret) - 1);
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function blackGetMacAddress(): PChar; stdcall;
begin
  Result := _blackGetMacAddress();
end;

function Java_com_hujiang_devart_utils_BlackTechnology_blackGetMacAddress(env: PJNIEnv; obj: jobject): jstring; stdcall;
var
  ret: string;
begin
  ret := string(_blackGetMacAddress());
  Result := stringToJString(env, ret);
end;

exports
  _blackGetMacAddress,
  blackGetMacAddress,
  Java_com_hujiang_devart_utils_BlackTechnology_blackGetMacAddress;

begin

  {$IFDEF DEBUG}
  WriteLn(string(blackGetMacAddress()));
  {$ENDIF}

end.

