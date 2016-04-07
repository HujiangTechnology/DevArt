library daemon21;

{$mode objfpc}{$H+}

uses
  cthreads,
  constant,
  Classes,
  SysUtils,
  common,
  jni2,
  jni_utils,
  BaseUnix,
  stdio,
  stdlib,
  android,
  unistd,
  Linux;

  procedure waitforSelfObserver(observerFilePath: PChar);
  var
    lockFileDescriptor: integer;
    pBuf: Pointer;
    maskStrLength: integer;
    pMaskStr: PChar;
    fileDescriptor: integer;
    watchDescriptor: integer;
    readBytes: size_t;
  begin
    LOGE('waitforSelfObserver');
    lockFileDescriptor := FpOpen(observerFilePath, O_RDONLY);
    if (lockFileDescriptor = -1) then begin
      LOGE('Watched >>>>OBSERVER<<<< has been ready before watching...');
      Exit;
    end;
    pBuf := GetMem(SizeOf(inotify_event));
    if (pBuf = nil) then begin
      LOGE('malloc failed !!!');
      Exit;
    end;
    maskStrLength := 7 + 10 + 1;
    pMaskStr := StrAlloc(maskStrLength);
    if (pMaskStr = nil) then begin
      freemem(pBuf);
      LOGE('malloc failed !!!');
      Exit;
    end;
    fileDescriptor := inotify_init();
    if (fileDescriptor < 0) then begin
      Freemem(pBuf);
      Freemem(pMaskStr);
      LOGE('inotify_init failed !!!');
      Exit;
    end;
    watchDescriptor := inotify_add_watch(fileDescriptor, observerFilePath, IN_ALL_EVENTS);
    if (watchDescriptor < 0) then begin
      Freemem(pBuf);
      Freemem(pMaskStr);
      LOGE('inotify_add_watch failed !!!');
      Exit;
    end;

    while True do begin
      readBytes := fpread(fileDescriptor, pBuf, sizeof(inotify_event));
      if (Pinotify_event(pBuf)^.mask = 4) then begin
        LOGE('Watched >>>>OBSERVER<<<< has been ready...');
        Freemem(pMaskStr);
        Freemem(pBuf);
        Exit;
      end;
    end;
  end;

  procedure notifyDaemonObserver(isPersistent: char; observerFilePath: PChar);
  var
    lockFileDescriptor: integer;
  begin
    LOGE('notifyDaemonObserver');
    if (isPersistent = #0) then begin
      lockFileDescriptor := FpOpen(observerFilePath, O_RDONLY);
      while (lockFileDescriptor = -1) do begin
        lockFileDescriptor := FpOpen(observerFilePath, O_RDONLY);
      end;
    end;
    remove(observerFilePath);
  end;

  procedure notifyAndWaitfor(observerSelfPath: PChar; observerDaemonPath: PChar);
  var
    observerSelfDescriptor: integer;
    observerDaemonDescriptor: integer;
  begin
    LOGE('notifyAndWaitfor');
    observerSelfDescriptor := FpOpen(observerSelfPath, O_RDONLY);
    if (observerSelfDescriptor = -1) then begin
      observerSelfDescriptor := FpOpen(observerSelfPath, O_CREAT, S_IRUSR or S_IWUSR);
    end;
    observerDaemonDescriptor := FpOpen(observerDaemonPath, O_RDONLY);
    while (observerDaemonDescriptor = -1) do begin
      usleep(1000);
      observerDaemonDescriptor := FpOpen(observerDaemonPath, O_RDONLY);
    end;
    remove(observerDaemonPath);
    LOGE('Watched >>>>OBSERVER<<<< has been ready...');
  end;

  function lockFile(lockFilePath: PChar): integer;
  var
    lockFileDescriptor: integer;
    lockRet: integer;
  begin
    LOGE('lockFile');
    LOGE(PChar(Format('start try to lock file >> %s <<', [lockFilePath])));
    lockFileDescriptor := fpopen(lockFilePath, O_RDONLY);
    if (lockFileDescriptor = -1) then begin
      lockFileDescriptor := fpopen(lockFilePath, O_CREAT, S_IRUSR);
    end;
    lockRet := __flock(lockFileDescriptor, LOCK_EX);
    if (lockRet = -1) then begin
      LOGE(PChar(Format('lock file failed >> %s <<', [lockFilePath])));
      Result := 0;
    end else begin
      LOGE(PChar(Format('lock file success  >> %s <<', [lockFilePath])));
      Result := 1;
    end;
  end;

  procedure Java_com_hujiang_devart_component_daemon_NativeDaemon21_doDaemon(env: PJNIEnv; obj: jobject;
    indicatorSelfPath: jstring; indicatorDaemonPath: jstring; observerSelfPath: jstring; observerDaemonPath: jstring); stdcall;
  var
    _indicatorSelfPath: PChar;
    _indicatorDaemonPath: PChar;
    _observerSelfPath: PChar;
    _observerDaemonPath: PChar;
    _lockStatus: integer = 0;
    _tryTime: integer = 0;
  begin
    LOGE('Java_com_hujiang_devart_component_daemon_NativeDaemon21_doDaemon');
    if (indicatorSelfPath = nil) or (indicatorDaemonPath = nil) or (observerSelfPath = nil) or (observerDaemonPath = nil) then begin
      LOGE('parameters cannot be NULL !');
      Exit;
    end;
    _indicatorSelfPath := PChar(jstringToString(env, indicatorSelfPath));
    _indicatorDaemonPath := PChar(jstringToString(env, indicatorDaemonPath));
    _observerSelfPath := PChar(jstringToString(env, observerSelfPath));
    _observerDaemonPath := PChar(jstringToString(env, observerDaemonPath));
    while (_tryTime < 3) and (_lockStatus <> lockFile(_indicatorSelfPath)) do begin
      Inc(_tryTime);
      LOGE(PChar(Format('Persistent lock myself failed and try again as %d times', [_tryTime])));
      usleep(10000);
    end;
    if (_lockStatus = 0) then begin
      LOGE('Persistent lock myself failed and exit');
      Exit;
    end;
    notifyAndWaitfor(_observerSelfPath, _observerDaemonPath);
    _lockStatus := lockFile(_indicatorDaemonPath);
    if (_lockStatus <> 0) then begin
      LOGE('Watch >>>>DAEMON<<<<< Daed !!');
      remove(_observerSelfPath);
      javaCallback(env, obj, DAEMON_CALLBACK_NAME);
    end;
  end;

exports
  Java_com_hujiang_devart_component_daemon_NativeDaemon21_doDaemon;

begin
end.
