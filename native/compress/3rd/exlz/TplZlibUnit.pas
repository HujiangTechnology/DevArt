{**********************************************************************
 Package pl_ExCompress
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}

unit TplZlibUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, md5, compressbase,
  paszlib;

const
 ZLibFileType:  IDFileType = ('Z','A','R', #0);

 ZAR_MAJOR_VERSION  : LongInt = 1;
 ZAR_MINOR_VERSION  : LongInt = 0;
 ZAR_MICRO_VERSION  : LongInt = 0;

type

TplZlibCompress = class(TpCustomCompress)
  private
  protected
    procedure WriteHeader(AHeader: TplArchiveHeader); override;
    function  InternalCompressStream(FileIndex: Integer; aInStream: TStream; aOutStream: TStream): Integer; override;
  public
    function CreateArchive: boolean; override;
  published
  end;

TplZLibUnCompress = class(TpCustomUnCompress)
  private
  protected
    procedure VerifyFile; override;
    procedure ReadTOC; override;
    function  InternalExtractStream(aInStream: TStream;var aOutStream: TStream): Integer; override;
  public
  published
  end;


function CompressStreamZLib(InStream: TStream; OutStream: TStream): Longint; //returns size of compressed file
function ExtractStreamZLib(InStream: TStream; OutStream: TStream): Longint;

implementation


{*******************************************************************************
*   This decompresses the data from InStream and Writes the decompressed data  *
*   to OutputStream                                                            *
*******************************************************************************}
function ExtractStreamZLib(InStream: TStream; OutStream: TStream): Longint;
var
  err : integer;
  z : TZstream;
const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;
  if InStream=nil  then exit;
  if OutStream=nil then exit;
  //........

  FillChar(z, SizeOf(z), 0);

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := inflateInit(z);
  InStream.Position := 0;

  while InStream.Position < InStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := InStream.Read(input_buffer, MAX_IN_BUF_SIZE);

    // wouldn't work for files > 2GB
    //z.next_in := TMemoryStream(InStream).Memory;
    //z.avail_in := InStream.Size;

    if InStream.Position = InStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_SYNC_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;

      err := inflate(z, FlushType);
      Result += OutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
      if err = Z_STREAM_END then Break;
      until Z.avail_out > 0;
    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;
  err := inflateEnd(z);
end;

{*******************************************************************************
*   This compresses the data from InStream and Writes the compressed data      *
*   to OutputStream                                                            *
*******************************************************************************}
function CompressStreamZLib(InStream: TStream; OutStream: TStream): Longint;
var
  err : integer;
  z : TZstream;

const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;
  if InStream=nil  then exit;
  if OutStream=nil then exit;
  //........

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := deflateInit(z, -1); //default
  InStream.Position := 0;

  while InStream.Position < InStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := InStream.Read(input_buffer, MAX_IN_BUF_SIZE);

    if InStream.Position = InStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_NO_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;
      err := deflate(z, FlushType);
      Result += OutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
    until Z.avail_out > 0;

    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;

  err := deflateEnd(z);
end;


//=============== TplZlibCompress ===================================

procedure TplZlibCompress.WriteHeader(AHeader: TplArchiveHeader);
var
X: Integer;
CompressedTOCStream: TMemoryStream;
TOCStream: TMemoryStream;
Position: Int64;
TplOCEntry: TTplOCEntry;
FileInfo:TplFileInfo;
begin
  try
  CheckStreamAssigned;
  TOCStream := TMemoryStream.Create;
  Position := 0;
  OutStream.Position := 0;
  for X := 0 to fInputFiles.Count-1 do begin
    TplOCEntry.FileName := fInputFiles.FileName[X];
    TplOCEntry.FilePath := fInputFiles.Path[X];
    TplOCEntry.Position := Position;
    FileInfo := fInputFiles.FileInfo[X];
    TplOCEntry.CompressedSize := FileInfo.CompressedSize;
    TplOCEntry.Md5sum := FileInfo.Md5Sum;
    WriteTplOCEntry(TplOCEntry, TOCStream);
    Position += TplOCEntry.CompressedSize;
  end;
  CompressedTOCStream:= TMemoryStream.Create;
  CompressStreamZLib(TOCStream, CompressedTOCStream);
  CompressedTOCStream.Position := 0;
  AHeader.TOCLength := NtoLE(CompressedTOCStream.Size);
  AHeader.TOCMd5Sum := StreamMd5(TOCStream);
  OutStream.Write(AHeader, SizeOf(TplArchiveHeader));
  OutStream.CopyFrom(CompressedTOCStream, CompressedTOCStream.Size);
  finally
  TOCStream.Free;
  CompressedTOCStream.Free;
  end;
end;

function TplZlibCompress.CreateArchive: boolean;
var
X: Integer;
AHeader: TplArchiveHeader;
TmpStream: TMemoryStream; // this holds all the compressed files temporarily
TmpFile: TFileStream; // this holds the current file to be added to TmpStream
FileInfo: TplFileInfo;
begin
  Result := False;
  try
  CheckStreamAssigned;
  TmpStream := TMemoryStream.Create;

  AHeader.FileType := ZLibFileType;
  AHeader.MajorVersion := NtoLE(ZAR_MAJOR_VERSION);
  AHeader.MinorVersion := NtoLE(ZAR_MINOR_VERSION);
  AHeader.MicroVersion := NtoLE(ZAR_MICRO_VERSION);

  for X := 0 to fInputFiles.Count-1 do begin
    if FileExists(fInputFiles.FileName[X]) then begin
      try
      TmpFile := TFileStream.Create(fInputFiles.FileName[X],fmOpenRead or fmShareDenyNone);
      FileInfo.CompressedSize := InternalCompressStream(X, TmpFile, TmpStream);
      FileInfo.Md5Sum := StreamMD5(TmpFile);
      fInputFiles.FileInfo[X] := FileInfo;//records the compressed length/size
      finally
        TmpFile.Free;
      end;
    end;
  end;
  //Write file header and Table of contents
  WriteHeader(AHeader);
  //WriteFiles
  TmpStream.Position := 0;

  OutStream.CopyFrom(TmpStream, TmpStream.Size);

  Result := True;
  finally
  TmpStream.Free;
  end;
end;

function TplZlibCompress.InternalCompressStream(FileIndex: Integer; aInStream: TStream; aOutStream: TStream): Integer;
var
  err : integer;
  z : TZstream;

const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;

  FillChar(z, 0 , SizeOf(z));

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := deflateInit(z, -1); //default
  aInStream.Position := 0;

  while aInStream.Position < aInStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := aInStream.Read(input_buffer, MAX_IN_BUF_SIZE);

    if aInStream.Position = aInStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_NO_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;
      err := deflate(z, FlushType);
      Result += aOutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
    until Z.avail_out > 0;
    if fOnCompress <> nil then fOnCompress(Self, FileIndex, aInStream.Size, aInStream.Position);
    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;

  err := deflateEnd(z);
end;

//============= TplZLibUnCompress ==================================

procedure TplZLibUnCompress.VerifyFile;
begin
  if (fHeader.FileType <> ZLibFileType) then DoError(ERR_CORRUPT_FILE,'corrupt file or not a correct file type');
end;

procedure TplZLibUnCompress.ReadTOC;
var
Entry: PTplOCEntry;
PositionOffset: Int64;
fChar: Char;
TmpStream: TMemoryStream; // used to temporarily hold the compressed TOC
TOCStream: TMemoryStream; // the TOC will be extracted into this
begin
  TmpStream:= TMemoryStream.Create;
  TOCStream := TMemoryStream.Create;
  try
  fStream.Position := SizeOf(fHeader);
  //Read The Compressed TOC into the TmpStream from the main fStream
  TmpStream.CopyFrom(fStream, fHeader.TOCLength);
  //Decompress TOC into TOCStream
  ExtractStreamZLib(TmpStream, TOCStream);
  if MD5Match(fHeader.TOCMD5Sum, StreamMd5(TOCStream)) = False then DoError(ERR_CORRUPT_TOC, 'corrupted table of contents');

  TOCStream.Position := 0;
  PositionOffset := fHeader.TOCLength + SizeOf(fHeader);
  while TOCStream.Position <> TOCStream.Size do begin
    Entry := New(pTplOCEntry);
    fTOCList.Add(Entry);
    // Read FileName
    fChar := Char(TOCStream.ReadByte);
    while fChar <> #0 do begin
      Entry^.FileName += fChar;
      fChar := Char(TOCStream.ReadByte);
    end;
    //Read FilePath
    fChar := Char(TOCStream.ReadByte);
    while fChar <> #0 do begin
      Entry^.FilePath += fChar;
      fChar := Char(TOCStream.ReadByte);
    end;
    //Read Position
    TOCStream.Read(Entry^.Position, SizeOf(Int64));
    Entry^.Position := LEtoN(Entry^.Position) + PositionOffset;
    //Read Compressed Size
    TOCStream.Read(Entry^.CompressedSize, SizeOf(Int64));
    Entry^.CompressedSize := LEtoN(Entry^.CompressedSize);
    //Read Md5sum
    TOCStream.Read(Entry^.Md5sum, SizeOf(TMD5Digest));
  end;
  finally
  TmpStream.Free;
  end;
end;

function TplZLibUnCompress.InternalExtractStream(aInStream: TStream; var aOutStream: TStream): Integer;
var
  err : integer;
  z : TZstream;
const
  MAX_IN_BUF_SIZE = 1024;
  MAX_OUT_BUF_SIZE = 1024;
var
  input_buffer : array[0..MAX_IN_BUF_SIZE-1] of byte;
  output_buffer : array[0..MAX_OUT_BUF_SIZE-1] of byte;
  FlushType: LongInt;
begin
  Result := 0;

  FillChar(z, 0 , SizeOf(z));

  FillChar(input_buffer, SizeOf(input_buffer), 0);
  err := inflateInit(z);
  aInStream.Position := 0;
  while aInStream.Position < aInStream.Size do
  begin
    z.next_in := @input_buffer;
    z.avail_in := aInStream.Read(input_buffer, MAX_IN_BUF_SIZE);

    if aInStream.Position = aInStream.Size then
      FlushType := Z_FINISH
    else
      FlushType :=  Z_SYNC_FLUSH;
    repeat
      z.next_out := @output_buffer;
      z.avail_out := MAX_OUT_BUF_SIZE;

      err := inflate(z, FlushType);
      Result += aOutStream.Write(output_buffer, MAX_OUT_BUF_SIZE - z.avail_out);
      if err = Z_STREAM_END then Break;
    until Z.avail_out > 0;
    if fOnExtract <> nil then fOnExtract(Self, aInStream.Size, aInStream.Position);
    if (err <> Z_OK) and (err <> Z_BUF_ERROR) then begin
      break;
    end;
  end;
  err := inflateEnd(z);
end;


end.



