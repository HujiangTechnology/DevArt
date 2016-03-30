{**********************************************************************
 Package pl_ExCompress
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}

unit ULZMACommon;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$Hints off}

interface

uses Classes;

type TLZMAProgressAction=(LPAMax,LPAPos);
     TLZMAProgress=procedure (const Action:TLZMAProgressAction;const Value:int64) of object;

function ReadByte(const stream:TStream):byte;
procedure WriteByte(const stream:TStream;const b:byte);

const CodeProgressInterval = 50;//approx. number of times an OnProgress event will be fired during coding

implementation

function ReadByte(const stream:TStream):byte;
begin
stream.Read(result,1);
end;

procedure WriteByte(const stream:TStream;const b:byte);
begin
stream.Write(b,1);
end;

end.
