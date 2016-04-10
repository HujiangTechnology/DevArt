library term;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, sysutils, jni2, jni_utils, BaseUnix, signal, termio, stdio, stdlib ,_strings;

const
  IUTF8 = &0040000;

type
  UnionJNIEnvToVoid = record
    env: PJNIEnv;
    venv: Pointer;
  end;

  char16_t = ShortInt;
  Pchar16_t = ^char16_t;

  String8 = class
  private
    FString: PChar;
  public
    constructor Create;
    destructor Destroy; override;
    function &String(): PChar;
    procedure &set(o: Pchar16_t; numChars: size_t);
  end;

  constructor String8.Create;
  begin
    FString:= nil;
  end;

destructor String8.Destroy;
begin
  if (FString <> nil) then begin
    Freemem(FString);
  end;
end;

function String8.&String(): PChar;
begin
  Result := FString;
end;

procedure String8.&set(o: Pchar16_t; numChars: size_t);
var
  i: size_t;
begin
  if (FString <> nil) then begin
    Freemem(FString);
  end;
  FString := StrAlloc(numChars + 1);
  if (FString = nil) then begin
    Exit;
  end;
  for i := 0 to numChars - 1 do begin
    FString[i] := Char(o[i]);
  end;
  FString[numChars] := Char(#0);
end;

var
  classFileDescriptor: jclass;
  fieldFileDescriptorDescriptor: jfieldID;
  methodFileDescriptorInit: jmethodID;

function throwOutOfMemoryError(env: PJNIEnv; message: PChar): Integer;
const
  className = 'java/lang/OutOfMemoryError';
var
  exClass: jclass;
begin
  exClass := env^^.FindClass(env, className);
  Result := env^^.ThrowNew(env, exClass, message);
end;

function createSubprocess(cmd: PChar; argv: PPChar; envp: PPChar; pProcessId: PInteger): Integer;
var
  devname: PChar;
  ptm: Integer;
  pid: pid_t;
  pts: Integer;
begin
  Result := -1;
  ptm := FpOpen('/dev/ptmx', O_RDWR);
  if(ptm < 0) then begin
    Exit;
  end;
  FpFcntl(ptm, F_SETFD, FD_CLOEXEC);
  devname:= ptsname(ptm);
  if (grantpt(ptm) <> 0) or (unlockpt(ptm) <> 0) or (devname = nil) then begin
    Exit;
  end;
  pid := FpFork();
  if(pid < 0) then begin
    Exit;
  end;
  if(pid = 0) then begin
    FpClose(ptm);
    FpSetsid();
    pts := FpOpen(devname, O_RDWR);
    if(pts < 0) then begin
      FpExit(-1);
      Exit;
    end;
    FpDup2(pts, 0);
    FpDup2(pts, 1);
    FpDup2(pts, 2);
    if (envp <> nil) then begin
      repeat
        Inc(envp);
        putenv(envp^);
      until envp^ = nil
    end;
    FpExecv(cmd, argv);
    FpExit(-1);
  end else begin
    pProcessId^ := pid;
    Result := ptm;
  end;
end;


function androidOsExecCreateSubProcess(env: PJNIEnv; clazz: jobject; cmd: jstring; args: jobjectArray; envVars: jobjectArray; processIdArray: jintArray): jobject;
var
  str: Pjchar = nil;
  isCopy: jboolean = JNI_FALSE;
  cmd8: String8;
  tmp8: String8;
  argv: PPChar = nil;
  envp: PPChar = nil;
  size: jsize = 0;
  i: Integer;
  arg: jstring;
  &var: jstring;
  procId: Integer;
  ptm: Integer;
  tmp: PPChar;
  procIdLen: Integer;
  pProcId: PInteger;
begin
  if (cmd <> nil) then begin
    str := env^^.GetStringCritical(env, cmd, isCopy);
  end;
  if (str <> nil) then begin
    cmd8 := String8.Create;
    cmd8.&set(Pchar16_t(str), env^^.GetStringLength(env, cmd));
    env^^.ReleaseStringCritical(env, cmd, str);
  end;
  if (args <> nil) then begin
    size := env^^.GetArrayLength(env, args);
  end;
  if (size > 0) then begin
    argv := PPChar(GetMem((size + 1)*sizeof(PChar)));
    if (argv = nil) then begin
      throwOutOfMemoryError(env, 'Couldn''t allocate argv array');
      Result := nil;
      if (Assigned(cmd8)) then begin
        cmd8.Free;
      end;
      Exit;
    end;
    for i := 0 to size - 1 do begin
      arg := jstring(env^^.GetObjectArrayElement(env, args, i));
      str := env^^.GetStringCritical(env, arg, isCopy);
      if (str = nil) then begin
        throwOutOfMemoryError(env, 'Couldn''t get argument from array');
        Result := nil;
        if (Assigned(cmd8)) then begin
          cmd8.Free;
        end;
        Exit;
      end;
      tmp8 := String8.Create;
      tmp8.&set(Pchar16_t(str), env^^.GetStringLength(env, arg));
      env^^.ReleaseStringCritical(env, arg, str);
      argv[i] := strdup(tmp8.&String());
      tmp8.Free;
    end;
    argv[size] := nil;
  end;
  size:= 0;
  if (envVars <> nil) then begin
    size := env^^.GetArrayLength(env, envVars);
  end;
  if (size > 0) then begin
    envp := PPChar(GetMem((size + 1) * sizeof(PChar)));
    if (envp = nil) then begin
      throwOutOfMemoryError(env, 'Couldn''t allocate envp array');
      Result := nil;
      if (Assigned(cmd8)) then begin
        cmd8.Free;
      end;
      Exit;
    end;
    for i := 0 to size - 1 do begin
      &var := jstring(env^^.GetObjectArrayElement(env, envVars, i));
      str := env^^.GetStringCritical(env, &var, isCopy);
      if (str = nil) then begin
        throwOutOfMemoryError(env, 'Couldn''t get env var from array');
        Result := nil;
        if (Assigned(cmd8)) then begin
          cmd8.Free;
        end;
        Exit;
      end;
      tmp8 := String8.Create;
      tmp8.&set(Pchar16_t(str), env^^.GetStringLength(env, &var));
      env^^.ReleaseStringCritical(env, &var, str);
      envp[i] := strdup(tmp8.&String());
      tmp8.Free;
    end;
    envp[size] := nil;
  end;

  ptm := createSubprocess(cmd8.&String(), argv, envp, @procId);

  if (argv <> nil) then begin
    tmp := argv;
    while (tmp^ <> nil) do begin
      Freemem(tmp^);
      Inc(tmp);
    end;
    Freemem(argv);
  end;
  if (envp <> nil) then begin
    tmp := envp;
    while (tmp^ <> nil) do begin
      Freemem(tmp^);
      Inc(tmp);
    end;
    Freemem(envp);
  end;
  if (processIdArray <> nil) then begin
    procIdLen := env^^.GetArrayLength(env, processIdArray);
    if (procIdLen > 0) then begin
      pProcId := PInteger(env^^.GetPrimitiveArrayCritical(env, processIdArray, isCopy));
      if (pProcId <> nil) then begin
        pProcId^ := procId;
        env^^.ReleasePrimitiveArrayCritical(env, processIdArray, pProcId, 0);
      end;
    end;
  end;
  Result := env^^.NewObject(env, classFileDescriptor, methodFileDescriptorInit);
  if (Result <> nil) then begin
    env^^.SetIntField(env, Result, fieldFileDescriptorDescriptor, ptm);
  end;
end;

procedure androidOsExecSetPtyWindowSize(env: PJNIEnv; clazz: jobject; fileDescriptor: jobject; row: jint; col: jint; xpixel: jint; ypixel: jint);
var
  fd: Integer;
  sz: winsize;
begin
  fd := env^^.GetIntField(env, fileDescriptor, fieldFileDescriptorDescriptor);
  if (env^^.ExceptionOccurred(env) <> nil) then begin
    Exit;
  end;
  sz.ws_row := row;
  sz.ws_col := col;
  sz.ws_xpixel := xpixel;
  sz.ws_ypixel := ypixel;
  FpIOCtl(fd, TIOCSWINSZ, @sz);
end;

procedure androidOsExecSetPtyUTF8Mode(env: PJNIEnv; clazz: jobject; fileDescriptor: jobject; utf8Mode: jboolean);
var
  fd: Integer;
  tios: termios;
begin
  fd := env^^.GetIntField(env, fileDescriptor, fieldFileDescriptorDescriptor);
  if (env^^.ExceptionOccurred(env) <> nil) then begin
    Exit;
  end;
  tcgetattr(fd, tios);
  if (utf8Mode = JNI_TRUE) then begin
    tios.c_iflag := tios.c_iflag or IUTF8;
  end else begin
    tios.c_iflag := tios.c_iflag and (not IUTF8);
  end;
  tcsetattr(fd, TCSANOW, &tios);
end;

function androidOsExecWaitFor(env: PJNIEnv; clazz: jobject; procId: jint): Integer;
var
  status: Integer;
begin
  FpWaitPid(procId, status, 0);
  Result := 0;
  if (WIFEXITED(status)) then begin
    Result := WEXITSTATUS(status);
  end;
end;

procedure androidOsExecClose(env: PJNIEnv; clazz: jobject; fileDescriptor: jobject);
var
  fd: Integer;
begin
  fd := env^^.GetIntField(env, fileDescriptor, fieldFileDescriptorDescriptor);
  if (env^^.ExceptionOccurred(env) <> nil) then begin
    Exit;
  end;
  FpClose(fd);
end;

procedure androidOsExecHangupProcessGroup(env: PJNIEnv; clazz: jobject; procId: jint);
begin
  FpKill(procId, SIGHUP);
end;

const
  classPathName = 'com/hujiang/devart/component/emulator/Proc';
  methodTable: array[0..5] of JNINativeMethod = (
    (name: 'createSubprocess'; signature: '(Ljava/lang/String;[Ljava/lang/String;[Ljava/lang/String;[I)Ljava/io/FileDescriptor;'; fnPtr: @androidOsExecCreateSubProcess),
    (name: 'setPtyWindowSize'; signature:'(Ljava/io/FileDescriptor;IIII)V'; fnPtr: @androidOsExecSetPtyWindowSize),
    (name: 'setPtyUTF8Mode'; signature:'(Ljava/io/FileDescriptor;Z)V'; fnPtr: @androidOsExecSetPtyUTF8Mode),
    (name: 'waitFor'; signature:'(I)I'; fnPtr: @androidOsExecWaitFor),
    (name: 'close'; signature:'(Ljava/io/FileDescriptor;)V'; fnPtr: @androidOsExecClose),
    (name: 'hangupProcessGroup'; signature:'(I)V'; fnPtr: @androidOsExecHangupProcessGroup)
  );

  function registerNativeMethods(env: PJNIEnv; className: PChar; gMethods: PJNINativeMethod; numMethods: Integer): Integer;
  var
    clazz: jclass;
  begin
    clazz := env^^.FindClass(env, className);
    if (clazz = nil)  then begin
        Result := JNI_FALSE;
        Exit;
    end;
    if (env^^.RegisterNatives(env, clazz, gMethods, numMethods) < 0) then begin
        Result := JNI_FALSE;
        Exit;
    end;
    Result := JNI_TRUE;
  end;

function registerFileDescriptor(env: PJNIEnv): Integer;
var
  localRefClassFileDescriptor: jclass;
begin
  Result := -1;
  localRefClassFileDescriptor := env^^.FindClass(env, 'java/io/FileDescriptor');
  if (localRefClassFileDescriptor = nil) then begin
    Exit;
  end;
  classFileDescriptor := jclass(env^^.NewGlobalRef(env, localRefClassFileDescriptor));
  env^^.DeleteLocalRef(env, localRefClassFileDescriptor);
  if (classFileDescriptor = nil) then begin
    Exit;
  end;
  fieldFileDescriptorDescriptor := env^^.GetFieldID(env, classFileDescriptor, 'descriptor', 'I');
  if (fieldFileDescriptorDescriptor = nil) then begin
    Exit;
  end;
  methodFileDescriptorInit := env^^.GetMethodID(env, classFileDescriptor, '<init>', '()V');
  if (methodFileDescriptorInit = nil) then begin
    Exit;
  end;
  Result := 0;
end;

function init_Exec(env: PJNIEnv): Integer;
begin
  if (registerFileDescriptor(env) < 0) then begin
    Result := JNI_FALSE;
    Exit;
  end;
  if (registerNativeMethods(env, classPathName, methodTable, sizeof(methodTable) div sizeof(methodTable[0])) <> JNI_TRUE) then begin
    Result := JNI_FALSE;
    Exit;
  end;
  Result := JNI_TRUE;
end;

function JNI_OnLoad(vm:PJavaVM;reserved:pointer):jint;{$ifdef mswindows}stdcall;{$else}cdecl;{$endif}
var
  uenv: UnionJNIEnvToVoid;
  env: PJNIEnv = nil;
begin
  uenv.venv := nil;
  Result := -1;
  if (vm^^.GetEnv(vm, @uenv.venv, JNI_VERSION_1_4) <> JNI_OK) then begin
    Exit;
  end;
  env := uenv.env;
  if (init_Exec(env) <> JNI_TRUE) then begin
    Exit;
  end;
  result := JNI_VERSION_1_4;
end;

exports
  JNI_OnLoad;

begin
end.

