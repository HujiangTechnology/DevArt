unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jni_utils, jni2, android, stdlib, BaseUnix, unistd;

function getVersion(): integer;
procedure javaCallback(env: PJNIEnv; obj: jobject; methodName: PChar);
function strStitching(str1: PChar; str2: PChar; str3: PChar): PChar;
function getContext(env: PJNIEnv; obj: jobject): jobject;
function getPackageName(env: PJNIEnv; jobj: jobject): PChar;
procedure startService(packageName: PChar; serviceName: PChar);

implementation

procedure startService(packageName: PChar; serviceName: PChar);
var
  pid: pid_t;
  version: integer;
  pkgSvcName: PChar;
begin
  pid := FpFork();
  if (pid < 0) then begin
    LOGE('startService error, do nothing...');
  end else if (pid = 0) then begin
    if (packageName = nil) or (serviceName = nil) then begin
      FpExit(EXIT_SUCCESS);
    end;
    version := getVersion();
    pkgSvcName := strStitching(packageName, '/', serviceName);
    if (version >= 17) or (version = 0) then begin
      execlp('am', 'am', 'startservice', '--user', '0', '-n', pkgSvcName, PChar(nil));
    end else begin
      execlp('am', 'am', 'startservice', '-n', pkgSvcName, PChar(nil));
    end;
    FpExit(EXIT_SUCCESS);
  end else begin
    FpWaitPid(pid, nil, 0);
  end;
end;

function getPackageName(env: PJNIEnv; jobj: jobject): PChar;
var
  contextObj: jobject;
  contextCls: jclass;
  m: jmethodID;
  pname: jstring;
begin
  contextObj := getContext(env, jobj);
  contextCls := env^^.GetObjectClass(env, contextObj);
  m := env^^.GetMethodID(jobj, contextCls, 'getPackageName', '()Ljava/lang/String;');
  pname := jstring(env^^.CallObjectMethod(env, contextObj, m));
  Result := PChar(jstringToString(env, pname));
end;

function getVersion(): integer;
var
  Value: array[0..7] of char;
begin
  __system_property_get('ro.build.version.sdk', Value);
  Result := atoi(Value);
end;

procedure javaCallback(env: PJNIEnv; obj: jobject; methodName: PChar);
var
  cls: jclass;
  cbMethod: jmethodID;
begin
  cls := env^^.GetObjectClass(env, obj);
  cbMethod := env^^.GetMethodID(env, cls, methodName, '()V');
  env^^.CallVoidMethod(env, obj, cbMethod);
end;

function strStitching(str1: PChar; str2: PChar; str3: PChar): PChar;
begin
  Result := StrAlloc(strlen(str1) + strlen(str2) + StrLen(str3) + 1);
  strcopy(Result, str1);
  strcat(Result, str2);
  strcat(Result, str3);
end;

function getContext(env: PJNIEnv; obj: jobject): jobject;
var
  thisCls: jclass;
  contentField: jfieldID;
begin
  thisCls := env^^.GetObjectClass(env, obj);
  contentField := env^^.GetFieldID(env, thisCls, '_context', 'Landroid/content/Context;');
  Result := env^^.GetObjectField(env, obj, contentField);
end;


end.

