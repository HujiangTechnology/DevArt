program daemon;

{$mode objfpc}{$H+}

uses
  cthreads,
  Classes, sysutils, common, BaseUnix, android, constant, stdlib, unistd;


var
  pid: pid_t;
  pipeFd1: array[0..1] of Integer;
  pipeFd2: array[0..1] of Integer;
  pkgName: PChar;
  svcName: PChar;
  i: Integer;
  rBuf: array[0..99] of Char;
  rNum: Integer;
  count: Integer = 0;
begin
  pid := FpFork();
  if(pid = 0) then begin
      FpSetsid();
  		if(ParamCount < 13) then begin
  			LOGE('daemon parameters error');
  			Exit;
  		end;
  		for i := 0 to ParamCount - 1 do begin
  			if(ParamStr(i) = '') then begin
  				Continue;
  			end;
  			if (PARAM_PKG_NAME = ParamStr(i)) then begin
  				pkgName := PChar(ParamStr(i + 1));
        end else if (PARAM_SVC_NAME = ParamStr(i)) then begin
  				svcName := PChar(ParamStr(i + 1));
        end else if (PARAM_PIPE_1_READ = ParamStr(i)) then begin
  				// char* p1r = argv[i + 1];
  				pipeFd1[0] := atoi(PChar(ParamStr(i + 1)));
        end else if (PARAM_PIPE_1_WRITE = ParamStr(i)) then begin
  				// char* p1w = argv[i + 1];
  				pipeFd1[1] := atoi(PChar(ParamStr(i + 1)));
        end else if (PARAM_PIPE_2_READ = ParamStr(i))	then begin
  				// char* p2r = argv[i + 1];
  				pipeFd2[0] := atoi(PChar(ParamStr(i + 1)));
        end else if (PARAM_PIPE_2_WRITE = ParamStr(i)) then begin
  				// char* p2w = argv[i + 1];
  				pipeFd2[1] := atoi(PChar(ParamStr(i + 1)));
        end;
  		end;
  		FpClose(pipeFd1[0]);
  		FpClose(pipeFd2[1]);
      FillChar(rBuf, SizeOf(rBuf), #0);
  		rNum := fpread(pipeFd2[0], rBuf, 100);
  		LOGE('Watch >>>>PARENT<<<< Dead !!');
  		while(count < 50) do begin
  			startService(pkgName, svcName);
  			usleep(100000);
        Inc(count);
      end;
  	end else begin
  		FpExit(EXIT_SUCCESS);
  	end;
end.

