unit tplHjzUnit;

{$mode objfpc}{$H+}
{$Hints off}


interface

uses
  Classes, SysUtils, md5, compressbase,
  ULZMAEncoder, ULZMADecoder, UBufferedFS, ULZMACommon;

const
 LzmaFileType: IDFileType = ('H','J','Z', #0);

 ZMA_MAJOR_VERSION  : LongInt = 1;
 ZMA_MINOR_VERSION  : LongInt = 0;
 ZMA_MICRO_VERSION  : LongInt = 0;

type

TplLzmaOptions=Record
  EOS:boolean;
  Algorithm:integer;
  NumBenchMarkPasses:integer;
  DictionarySize:integer;
  Lc:integer;
  Lp:integer;
  Pb:integer;
  Fb:integer;
  MatchFinder:integer;
end;

TplLzmaCompress = class(TpCustomCompress)
  private
    FLzmaOptions:TplLzmaOptions;
    FDictionarySize:integer;
  protected
    Procedure SetOptEOS(const Val:boolean);
    Function  GetOptEOS:boolean;
    Procedure SetOptAlgorithm(const Val:integer);
    Function  GetOptAlgorithm:integer;
    Procedure SetOptNumBenchMarkPasses(const Val:integer);
    Function  GetOptNumBenchMarkPasses:integer;
    Procedure SetOptDictionarySize(const Val:integer);
    Function  GetOptDictionarySize:integer;
    Procedure SetOptMatchFinder(const Val:integer);
    Function  GetOptMatchFinder:integer;
    Procedure SetOptNumLiteralContext(const Val:integer);
    Function  GetOptNumLiteralContext:integer;
    Procedure SetOptNumLiteralPosBits(const Val:integer);
    Function  GetOptNumLiteralPosBits:integer;
    Procedure SetOptNumPosBits(const Val:integer);
    Function  GetOptNumPosBits:integer;
    Procedure SetOptNumFastBytes(const Val:integer);
    Function  GetOptNumFastBytes:integer;

    //......
    procedure WriteHeader(AHeader: TplArchiveHeader); override;
    function  InternalCompressStream(FileIndex: Integer; aInStream: TStream; aOutStream: TStream): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    Procedure SetDefaultOptions;
    function CreateArchive: boolean; override;
  published
    //write End Of Stream marker default: false
    property OptEOS :boolean read GetOptEOS write SetOptEOS default false;
    //Compress Algorithm : [0, 2], default: 2
    property OptAlgorithm :integer read GetOptAlgorithm write SetOptAlgorithm default 2;
    //NumBenchMarkPasses- [0,10], default: 10
    property OptNumBenchMarkPasses :integer read GetOptNumBenchMarkPasses write SetOptNumBenchMarkPasses default 10;
    //dictionarySize - [0,28], default: 23 (8MB)
    property OptDictionarySize :integer read GetOptDictionarySize write SetOptDictionarySize default 23;
    //Match Finder: [0, 1], default: 1
    property OptMatchFinder :integer read GetOptMatchFinder write SetOptMatchFinder default 1;
    //number of literal context bits - [0, 8], default: 3
    property OptNumLiteralContext :integer read GetOptNumLiteralContext write SetOptNumLiteralContext default 3;
    //number of literal pos bits - [0, 4], default: 0
    property OptNumLiteralPosBits :integer read GetOptNumLiteralPosBits write SetOptNumLiteralPosBits default 0;
    //number of pos bits - [0, 4], default: 2
    property OptNumPosBits :integer read GetOptNumPosBits write SetOptNumPosBits default 2;
    //number of fast bytes - [5, 273], default: 128
    property OptNumFastBytes :integer read GetOptNumFastBytes write SetOptNumFastBytes default 128;
  end;


TplLzmaUnCompress = class(TpCustomUnCompress)
  private
  protected
    procedure VerifyFile; override;
    procedure ReadTOC; override;
    function  InternalExtractStream(aInStream: TStream;var aOutStream: TStream): Integer; override;
  public
  published
  end;

function CompressStreamLzma(InStream: TStream; OutStream: TStream; Const AOptions:TplLzmaOptions): Longint;
function ExtractStreamLzma(InStream: TStream; OutStream: TStream): Longint;

implementation

{*******************************************************************************
*   This decompresses the data from InStream and Writes the decompressed data  *
*   to OutputStream                                                            *
*******************************************************************************}
function ExtractStreamLzma(InStream: TStream; OutStream: TStream): Longint;
const
     xpropertiessize=5;
 var decoder :TLZMADecoder;
     xproperties:array[0..4] of byte;
     i:integer;
     v:byte;
begin
  Result := 0;
  if InStream=nil  then exit;
  if OutStream=nil then exit;
  //........
  inStream.Position:=0;

  try
  if inStream.read(xproperties, xpropertiesSize) <> xpropertiesSize then
                raise Exception.Create('input file is too short');

  decoder := TLZMADecoder.Create;

  if not decoder.SetDecoderProperties(xproperties) then
                raise Exception.Create('Incorrect stream properties');

  for i := 0 to 7 do
   begin
     v := {shortint}(ReadByte(inStream));
     if v < 0 then
       raise Exception.Create('Can''t read stream size');
     Result := Result or v shl (8 * i);
   end;

  if not decoder.Code(inStream, outStream, Result)  then
     raise Exception.Create('Error in data stream');


  finally
    decoder.Free;
  end;

end;

{*******************************************************************************
*   This compresses the data from InStream and Writes the compressed data      *
*   to OutputStream                                                            *
*******************************************************************************}
function CompressStreamLzma(InStream: TStream; OutStream: TStream; Const AOptions:TplLzmaOptions): Longint;
 var encoder :TLZMAEncoder;
     xfilesize:int64;
     i:integer;
begin
  Result := 0;
  if InStream=nil  then exit;
  if OutStream=nil then exit;
  //........
  inStream.Position:=0;

  encoder:=TLZMAEncoder.Create;
  try

    if not encoder.SetAlgorithm(AOptions.Algorithm) then
              raise Exception.Create('Incorrect compression mode');

    if not encoder.SetDictionarySize(AOptions.DictionarySize) then
              raise Exception.Create('Incorrect dictionary size');

    if not encoder.SeNumFastBytes(AOptions.Fb) then
              raise Exception.Create('Incorrect -fb value');

    if not encoder.SetMatchFinder(AOptions.MatchFinder) then
              raise Exception.Create('Incorrect -mf value');

    if not encoder.SetLcLpPb(AOptions.Lc, AOptions.Lp, AOptions.Pb) then
              raise Exception.Create('Incorrect -lc or -lp or -pb value');

    encoder.SetEndMarkerMode(AOptions.eos);
    encoder.WriteCoderProperties(outStream);

    if AOptions.eos then
      xfileSize := -1 else
      xfileSize := inStream.Size;

    for i := 0 to 7 do
      WriteByte(outStream,(xfileSize shr (8 * i)) and $FF);

    encoder.Code(inStream, outStream, -1, -1);

    result:=outStream.Size;

  finally
    encoder.free;
  end;
end;

//=============== TplLzmaCompress ===================================
constructor TplLzmaCompress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  SetDefaultOptions;
end;

Procedure TplLzmaCompress.SetDefaultOptions;
begin
  FDictionarySize:=23;
  FLzmaOptions.Eos:= false;
  FLzmaOptions.Algorithm:= 2;
  FLzmaOptions.NumBenchMarkPasses:=10;
  FLzmaOptions.DictionarySize:=1 shl FDictionarySize;
  FLzmaOptions.Lc:= 3;
  FLzmaOptions.Lp:= 0;
  FLzmaOptions.Pb:= 2;
  FLzmaOptions.Fb:= 128;
  FLzmaOptions.MatchFinder:= 1;
end;

Procedure TplLzmaCompress.SetOptEOS(const Val:boolean);
begin
  FLzmaOptions.EOS:=Val;
end;
Function  TplLzmaCompress.GetOptEOS:boolean;
begin
  Result:=FLzmaOptions.EOS;
end;

Procedure TplLzmaCompress.SetOptDictionarySize(const Val:integer);
begin
  if (Val<0) or (Val>28) then exit;
  FDictionarySize:=Val;
  FLzmaOptions.DictionarySize:=1 shl FDictionarySize;
end;
Function  TplLzmaCompress.GetOptDictionarySize:integer;
begin
  Result:=FDictionarySize;
end;

Procedure TplLzmaCompress.SetOptAlgorithm(const Val:integer);
begin
  if (Val<0) or (Val>2) then exit;
  FLzmaOptions.Algorithm:=Val;
end;
Function  TplLzmaCompress.GetOptAlgorithm:integer;
begin
  Result:=FLzmaOptions.Algorithm;
end;

Procedure TplLzmaCompress.SetOptNumBenchMarkPasses(const Val:integer);
begin
  if (Val<0) or (Val>10) then exit;
  FLzmaOptions.NumBenchMarkPasses:=Val;
end;
Function  TplLzmaCompress.GetOptNumBenchMarkPasses:integer;
begin
  Result:=FLzmaOptions.NumBenchMarkPasses;
end;

Procedure TplLzmaCompress.SetOptMatchFinder(const Val:integer);
begin
  if (Val<0) or (Val>1) then exit;
  FLzmaOptions.MatchFinder:=Val;
end;
Function  TplLzmaCompress.GetOptMatchFinder:integer;
begin
  Result:=FLzmaOptions.MatchFinder;
end;

Procedure TplLzmaCompress.SetOptNumLiteralContext(const Val:integer);
begin
  if (Val<0) or (Val>8) then exit;
  FLzmaOptions.Lc:=Val;
end;
Function  TplLzmaCompress.GetOptNumLiteralContext:integer;
begin
  Result:=FLzmaOptions.Lc;
end;

Procedure TplLzmaCompress.SetOptNumLiteralPosBits(const Val:integer);
begin
  if (Val<0) or (Val>4) then exit;
  FLzmaOptions.Lp:=Val;
end;
Function  TplLzmaCompress.GetOptNumLiteralPosBits:integer;
begin
  Result:=FLzmaOptions.Lp;
end;

Procedure TplLzmaCompress.SetOptNumPosBits(const Val:integer);
begin
  if (Val<0) or (Val>4) then exit;
  FLzmaOptions.Pb:=Val;
end;
Function  TplLzmaCompress.GetOptNumPosBits:integer;
begin
  Result:=FLzmaOptions.Pb;
end;

Procedure TplLzmaCompress.SetOptNumFastBytes(const Val:integer);
begin
  if (Val<5) or (Val>273) then exit;
  FLzmaOptions.Fb:=Val;
end;
Function  TplLzmaCompress.GetOptNumFastBytes:integer;
begin
  Result:=FLzmaOptions.Fb;
end;

procedure TplLzmaCompress.WriteHeader(AHeader: TplArchiveHeader);
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
  CompressStreamLzma(TOCStream, CompressedTOCStream,FLzmaOptions);
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

function TplLzmaCompress.CreateArchive: boolean;
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

  AHeader.FileType := LzmaFileType;
  AHeader.MajorVersion := NtoLE(ZMA_MAJOR_VERSION);
  AHeader.MinorVersion := NtoLE(ZMA_MINOR_VERSION);
  AHeader.MicroVersion := NtoLE(ZMA_MICRO_VERSION);

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

function TplLzmaCompress.InternalCompressStream(FileIndex: Integer; aInStream: TStream; aOutStream: TStream): Integer;
begin
  result:=CompressStreamLzma(aInStream,aOutStream,FLzmaOptions);
end;

//============= TplLzmaUnCompress ==================================

procedure TplLzmaUnCompress.VerifyFile;
begin
  if (fHeader.FileType <> LzmaFileType) then DoError(ERR_CORRUPT_FILE,'corrupt file or not a correct file type');
end;

procedure TplLzmaUnCompress.ReadTOC;
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
  ExtractStreamLzma(TmpStream, TOCStream);
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

function TplLzmaUnCompress.InternalExtractStream(aInStream: TStream; var aOutStream: TStream): Integer;
begin
  result:=ExtractStreamLzma(aInStream,aOutStream);
end;


end.




