program sample;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, frmMain, frmMD5, frmSHA1, frmLMD, frmELF, frmBASE64,
  frmRSA, frmDSA, frmRDL, frmRSASSA, frmAES, NativeAlgorithm, Dialogs;

{$R *.res}

{$IFDEF WINDOWS}
var
   path: string;
   i: Integer;
   overAscii: Boolean;
{$ENDIF}

begin
  {$IFDEF WINDOWS}
  overAscii:= False;
  path := Application.ExeName;
  for i:= 1 to Length(path) do begin
    if (Ord(path[i]) > 127) then begin
      overAscii:= True;
      Break;
    end;
  end;
  if (overAscii) then begin
    ShowMessage('You must put the app into a non-Chinese named path.');
    Exit;
  end;
  {$ENDIF}
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

