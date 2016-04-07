library daemon20;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes,
  SysUtils,
  constant,
  jni2,
  jni_utils,
  android,
  BaseUnix,
  stdio, stdlib,
  common, unistd;

  function findPidByName(pidName: PChar; pidList: PInteger): integer;
  var
    _dir: PDIR;
    _next: Pdirent;
    i: integer = 0;
    _status: PFILE;
    procFileName: array[0..BUFFER_SIZE - 1] of char;
    buffer: array[0..BUFFER_SIZE] of char;
    processName: array[0..BUFFER_SIZE] of char;
  begin
    LOGE('findPidByName');
    pidList[0] := 0;
     _dir := FpOpendir('/proc');
    if (_dir = nil) then begin
      Result := 0;
      Exit;
    end;
    repeat
      _next := fpreaddir(_dir^);
      if string(_next^.d_name) = '..' then begin
        Continue;
      end;
      if (isdigit(Ord(_next^.d_name[0])) = 0) then begin
        Continue;
      end;
      sprintf(procFileName, '/proc/%s/cmdline', _next^.d_name);
      if (_status <> fopen(procFileName, 'r')) then begin
        Continue;
      end;
      if (fgets(buffer, BUFFER_SIZE - 1, _status) = nil) then begin
        fclose(_status);
        Continue;
      end;
      fclose(_status);
      sscanf(buffer, '%[^-]', processName);
      if string(processName) = string(pidName) then begin
        pidList[i] := atoi(_next^.d_name);
        Inc(i);
      end;
    until _next = nil;
    if (pidList <> nil) then begin
      pidList[i] := 0;
    end;
    fpclosedir(_dir^);
    Result := i;
  end;

  procedure kill_zombie_process(zombieName: PChar);
  var
    pidList: array[0..199] of integer;
    totalNum: integer;
    i: integer;
    retval: integer;
    daemonPid: TPid;
  begin
    LOGE('kill_zombie_process');
    totalNum := findPidByName(zombieName, pidList);
    LOGE(PChar(Format('zombie process name is %s, and number is %d, killing...', [zombieName, totalNum])));
    for i := 0 to totalNum - 1 do begin
      retval := 0;
      daemonPid := pidList[i];
      if (daemonPid > 1) and (daemonPid <> FpGetpid()) and (daemonPid <> FpGetpid()) then begin
        retval := FpKill(daemonPid, SIGTERM);
        if (retval = 0) then begin
           LOGE(PChar(Format('kill zombie successfully, zombie`s pid = %d', [daemonPid])));
        end else begin
           LOGE(PChar(Format('kill zombie failed, zombie`s pid = %d', [daemonPid])));
        end;
      end;
    end;
  end;


  // com.hujiang.devart.component.daemon.NativeDaemon20
  // doDaemon
  procedure Java_com_hujiang_devart_component_daemon_NativeDaemon20_doDaemon(env: PJNIEnv; obj: jobject; pkgName: jstring;
    svcName: jstring; daemonPath: jstring); stdcall;
  var
    _pkgName: PChar;
    _svcName: PChar;
    _daemonPath: PChar;
    pipeFd1: array[0..1] of integer;
    pipeFd2: array[0..1] of integer;
    pid: pid_t;
    rBuf: array[0..99] of char;
    rNum: integer;
    strP1r: array[0..9] of char;
    strP1w: array[0..9] of char;
    strP2r: array[0..9] of char;
    strP2w: array[0..9] of char;
  begin
    LOGE('Java_com_hujiang_devart_component_daemon_NativeDaemon20_doDaemon');
    if (pkgName = nil) or (svcName = nil) or (daemonPath = NULL) then begin
       LOGE('native doDaemon parameters cannot be NULL !');
      Exit;
    end;
    _pkgName := PChar(jstringToString(env, pkgName));
    _svcName := PChar(jstringToString(env, svcName));
    _daemonPath := PChar(jstringToString(env, daemonPath));
     kill_zombie_process(NATIVE_DAEMON_NAME);
    FillChar(rBuf, SizeOf(rBuf), #0);
    if (FpPipe(pipeFd1) < 0) then begin
       LOGE('pipe1 create error');
      Exit;
    end;
    if (FpPipe(pipeFd2) < 0) then begin
       LOGE('pipe2 create error');
      Exit;
    end;
    sprintf(strP1r, '%d', pipeFd1[0]);
    sprintf(strP1w, '%d', pipeFd1[1]);
    sprintf(strP2r, '%d', pipeFd2[0]);
    sprintf(strP2w, '%d', pipeFd2[1]);

    pid := FpFork();
    if (pid = 0) then begin
      execlp(_daemonPath,
        NATIVE_DAEMON_NAME,
        PARAM_PKG_NAME, _pkgName,
        PARAM_SVC_NAME, _svcName,
        PARAM_PIPE_1_READ, strP1r,
        PARAM_PIPE_1_WRITE, strP1w,
        PARAM_PIPE_2_READ, strP2r,
        PARAM_PIPE_2_WRITE, strP2w,
        PChar(nil));
    end else if (pid > 0) then begin
      FpClose(pipeFd1[1]);
      FpClose(pipeFd2[0]);
      rNum := fpread(pipeFd1[0], rBuf, 100);
       LOGE('Watch >>>>CHILD<<<< Dead !!!');
       javaCallback(env, obj, DAEMON_CALLBACK_NAME);
    end;

  end;

exports
  Java_com_hujiang_devart_component_daemon_NativeDaemon20_doDaemon;

begin

end.
