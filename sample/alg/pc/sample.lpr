program sample;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, frmMain, frmMD5, frmSHA1, frmLMD, frmELF, frmBASE64,
  frmRSA, frmDSA, frmRDL, frmRSASSA, frmAES, NativeAlgorithm;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

