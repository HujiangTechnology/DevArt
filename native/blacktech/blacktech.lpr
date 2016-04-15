{.$DEFINE DEBUG}

{$IFDEF DEBUG}
program blacktech;
{$ELSE}
library blacktech;
{$ENDIF}

{$mode objfpc}{$H+}

uses
  cthreads, Classes, sysutils, jni2, jni_utils, process, strutils, android;

procedure parse(line1, line2: String; out AName, AAttr, AMac: string);
var
  p: Integer;
  lName: string;
  lAttr: string;
  lMac: string;
begin
  AName:= '';
  AAttr:= '';
  AMac:= '';
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
    Exit;
  end else begin
    line2 := Trim(Copy(line2, p + 1, Length(line2) - p));
    if (line2 = '') then begin
      Exit;
    end else begin
      p := Pos('brd', line2);
      if (p <= 0) then begin
        lMac:= line2;
      end else begin
        lMac:= Trim(LeftStr(line2, p - 1));
      end;
      AName:= lName;
      AAttr:= lAttr;
      AMac:= lMac;
    end;
  end;
end;

procedure blackGetMacAddress(env: PJNIEnv; obj: jobject; methodAdd: jmethodID);
var
  outStr: string;
  i: Integer = 0;
  AName, AAttr, AMac: string;
  clsMac: jclass;
  midMac: jmethodID;
  objMac: jobject;
  pval: Pjvalue;
begin
  clsMac:= env^^.FindClass(env, 'com/hujiang/devart/utils/BlackTechnology$MacAddress');
  midMac:= env^^.GetMethodID(env, clsMac, '<init>', '(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V');
  LOGE('Prepare MacAddress class');
  if (RunCommand('ip', ['link'], outstr, [poWaitOnExit])) then begin
    LOGE('RUN IP LINK');
    with TStringList.Create do begin
      Text:= outStr;
      while i < Count - 1 do begin
        LOGE('PARSE MAC ADDRESS');
        parse(Strings[i], Strings[i + 1], AName, AAttr, AMac);
        if (AMac <> '') then begin
          pval := argsToJValues(env, [AName, AAttr, AMac]);
          objMac:= env^^.NewObjectA(env, clsMac, midMac, pval);
          env^^.CallBooleanMethodA(env, obj, methodAdd, argsToJValues(env, [objMac]));
          LOGE('ADD MACADDRESS TO ARRAYLIST');
        end;
        i += 2;
      end;
      Free;
    end;
  end;
end;

function Java_com_hujiang_devart_utils_BlackTechnology_blackGetMacAddress(env: PJNIEnv; obj: jobject): jobject; stdcall;
var
  cls: jclass;
  midList: jmethodID;
  objList: jobject;
  midAdd: jmethodID;
begin
  LOGE('Java_com_hujiang_devart_utils_BlackTechnology_blackGetMacAddress');
  cls := env^^.FindClass(env, 'java/util/ArrayList');
  midList := env^^.GetMethodID(env, cls, '<init>', '()V');
  objList := env^^.NewObjectA(env, cls, midList, nil);
  LOGE('NEW ARRAYLIST OBJECT');
  midAdd := env^^.GetMethodID(env, cls, 'add', '(Ljava/lang/Object;)Z');
  blackGetMacAddress(env, objList, midAdd);
  LOGE('RETURN READY');
  Result := objList;
end;

exports
  Java_com_hujiang_devart_utils_BlackTechnology_blackGetMacAddress;

begin

  {$IFDEF DEBUG}
  WriteLn(string(blackGetMacAddress()));
  {$ENDIF}

end.

