{**********************************************************************
 Package pl_ExCompress
 This unit is part of CodeTyphon Project (http://www.pilotlogic.com/)
***********************************************************************}

unit compressbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, md5;

const

 ERR_CORRUPT_TOC        = 1;
 ERR_CORRUPT_FILE       = 2;
 ERR_CHECKSUM_FAILED    = 3;
 ERR_FILENAME_NOT_FOUND = 4;
 ERR_UNKOWN_ERROR       = 6;

type

  IDFileType= array [0..3] of char;

  PplFileInfo = ^TplFileInfo;
  TplFileInfo = record
    CompressedSize: Int64;
    Md5Sum: TMD5Digest;
  end;

  TplArchiveHeader = packed record
    FileType: IDFileType;
    MajorVersion,
    MinorVersion,
    MicroVersion: LongInt;
    TOCLength: Int64;
    TOCMD5Sum: TMD5Digest;
  end;

  PTplOCEntry = ^TTplOCEntry;
  TTplOCEntry = packed record
    FileName: String;
    FilePath: String;
    Position: Int64;
    CompressedSize: Int64;
    Md5sum: TMD5Digest;
  end;

{
  A TOC Entry will stored like so:
  1.  Write File Name to be in archive
  2   Write Empty Byte.
  3.  Write File Path
  4.  Write Empty Byte
  5.  Write File Position (Int64) 8 bytes
  6.  Write File Compressed Size(Int64)
  7.  Write md5sum (TMD5Digest)
   Repeat 1-4
}


TplCompressProgProc = procedure (Sender: TObject; FileIndex: Integer; FileSize, FilePos: Int64) of object;
TplExtractProgProc = procedure (Sender: TObject; FileSize, FilePos: Int64) of object;
TplErrorProc = procedure(Sender: TObject; var ErrorCode: Integer; ErrorStr: String) of object;


TplCompressFilesList = class(TObject)
  private
    FFileList: TFpList;
    FPathList: TFpList;
    FFileInfoList: TFpList;
    function  GetFileName(AIndex: Integer): String;
    function  GetPath(AIndex: Integer): String;
    procedure SetFileName(AIndex: Integer; AFIleName: String);
    procedure SetPath(AIndex: Integer; APath: String);
    function  GeTplFileInfo(AIndex: Integer): TplFileInfo;
    procedure SeTplFileInfo(AIndex: Integer; AFileInfo: TplFileInfo);
  public
    constructor Create;
    destructor Destroy; override;
  public
    function  Add(AFileName: String): Integer;
    function  AddPath(AFileName: String; APath: String): Integer;
    function  Insert(AIndex: Integer; AFileName: String): Integer;
    function  InsertPath(AIndex: Integer; AFileName: String; APath: String): Integer;
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function  Count: Integer;
  public
    // this contains the full path to the file name on the current filesystem
    property FileName[Index: Integer]: String read GetFileName write SetFileName;
    // this is the relative path that you would like the file extracted to
    property Path[Index: Integer]: String read GetPath write SetPath;
    // used internally
    property FileInfo[Index: Integer]: TplFileInfo read GeTplFileInfo write SeTplFileInfo;
  end;


TpCustomCompress = class(TComponent)
  private
  protected
    fInputFiles: TplCompressFilesList;
    fOnCompress: TplCompressProgProc;
    fOnError: TplErrorProc;
    fStream: TStream;
    fStreamOwner: Boolean;
    procedure WriteTplOCEntry(Entry: TTplOCEntry; TOCStream: TStream);
    procedure CheckStreamAssigned;
    procedure SetStream(AValue: TStream);
    procedure DoError(ErrorCode: Integer; ErrorString: String);
    //.....
    procedure WriteHeader(AHeader: TplArchiveHeader); Virtual;
    function  InternalCompressStream(FileIndex: Integer; aInStream: TStream; aOutStream: TStream): Integer; Virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateArchive: boolean; Virtual;

    property OutStream: TStream read fStream write SetStream;
    property InputFiles: TplCompressFilesList read fInputFiles;
  published
    property OnError: TplErrorProc read fOnError write fOnError;
    property OnCompress : TplCompressProgProc read fOnCompress write fOnCompress;
  end;


TpCustomUnCompress = class(TComponent)
  private
  protected
    fTOCList: TFpList;
    fOnError: TplErrorProc;
    fOnExtract: TplExtractProgProc;
    fHeader: TplArchiveHeader;
    fStream: TStream;
    fFileName: String;
    procedure SetStream(AStream: TStream);
    procedure FreeTOC;
    function  GetCount: Integer;
    function  GetTplOCEntry(AIndex: Integer): TTplOCEntry;
    function  FindFilePath(const AFilePath: String): Integer;
    procedure DoError(ErrorCode: Integer; ErrorString: String);
    //.....
    procedure VerifyFile; Virtual;
    procedure ReadTOC;  Virtual;
    function  InternalExtractStream(aInStream: TStream;var aOutStream: TStream): Integer; Virtual;
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; InStream: TStream); overload;
    destructor  Destroy; override;
    procedure ExtractFileToStream(AIndex: Integer; Stream: TStream); overload;
    procedure ExtractFileToStream(const AFileName: String; Stream: TStream); overload;
    procedure ExtractFileToStream(const AFileName , APath: String; Stream: TStream); overload;

    property Header: TplArchiveHeader read fHeader write fHeader;
    property FilesInArchive[Index: Integer]: TTplOCEntry read GetTplOCEntry;
    property Count: Integer read GetCount;
    property InStream: TStream read fStream write SetStream;
  published
    property OnError: TplErrorProc read fOnError write fOnError;
    property OnExtract: TplExtractProgProc read fOnExtract write fOnExtract;
  end;

function StreamMD5(Stream: TStream): TMD5Digest;   // creates a md5digest from a tstream

implementation

{*******************************************************************************
*   This performs an md5 sum on the file and returns it as a TMD5Digest        *
*                                                                              *
*******************************************************************************}

function StreamMD5(Stream: TStream): TMD5Digest;
var
  Buf : array [0..1023] of byte;
  Context: TMD5Context;
  Count : Longint;
begin
  Stream.Position := 0;
  MD5Init(Context);
  repeat
    Count := 1024;
    Count := Stream.Read(Buf, Count);
    If (Count>0) then
      MD5Update(Context, Buf, Count);
  until (Count<1024);
  MD5Final(Context, Result);
end;

//=================== TplCompressFilesList =================================

constructor TplCompressFilesList.Create;
begin
  FFileList := TFpList.Create;
  FPathList := TFpList.Create;
  FFileInfoList := TFpList.Create;
end;

destructor TplCompressFilesList.Destroy;
begin
  Clear;
  Inherited Destroy;
end;

function TplCompressFilesList.GetFileName(AIndex: Integer): String;
begin
   Result := PString(FFileList.Items[AIndex])^;
end;

function TplCompressFilesList.GetPath(AIndex: Integer): String;
begin
   Result := PString(FPathList.Items[AIndex])^;
end;

procedure TplCompressFilesList.SetFileName(AIndex: Integer; AFIleName: String);
begin
  PString(FFileList.Items[AIndex])^ := AFileName;
end;

procedure TplCompressFilesList.SetPath(AIndex: Integer; APath: String);
begin
  PString(FPathList.Items[AIndex])^ := APath;
end;

function TplCompressFilesList.GeTplFileInfo(AIndex: Integer): TplFileInfo;
begin
  Result := PplFileInfo(FFileInfoList.Items[AIndex])^;
end;

procedure TplCompressFilesList.SeTplFileInfo(AIndex: Integer; AFileInfo: TplFileInfo);
begin
    PplFileInfo(FFileInfoList.Items[AIndex])^ := AFileInfo;
end;

function TplCompressFilesList.Add(AFileName: String): Integer;
begin
  Result := InsertPath(Count, AFileName,'/');
end;

function TplCompressFilesList.AddPath(AFileName: String; APath: String): Integer;
begin
  Result := InsertPath(Count, AFileName, APath);
end;

function TplCompressFilesList.Insert(AIndex: Integer; AFileName: String): Integer;
begin
  Result := InsertPath(AIndex, AFileName, '/');
end;

function TplCompressFilesList.InsertPath(AIndex: Integer; AFileName: String;
  APath: String): Integer;
var
FFile,
FPath: PString;
FFileInfo: PplFileInfo;
begin
  Result := 0;
  if AIndex > Count then Result := Count
  else Result := AIndex;

  FFile := New(PString);
  FPath := New(PString);
  FFileInfo := New(PplFileInfo);

  FFile^ := AFileName;
  FPath^ := APAth;

  FFileList.Insert(AIndex, FFile);
  FPathList.Insert(AIndex, FPath);
  FFileInfoList.Insert(AIndex, FFileInfo);
end;

procedure TplCompressFilesList.Delete(AIndex: Integer);
begin
  Dispose(PString(FFileList.Items[AIndex]));
  Dispose(PString(FPathList.Items[AIndex]));
  Dispose(PplFileInfo(FFileInfoList.Items[AIndex]));

  FFileList.Delete(AIndex);
  FPathList.Delete(AIndex);
  FFileInfoList.Delete(AIndex);
end;

procedure TplCompressFilesList.Clear;
begin
  While Count > 0 do
    Delete(Count-1);
end;

function TplCompressFilesList.Count: Integer;
begin
  Result := FFileList.Count;
end;

//=============== TpCustomCompress ===================================

constructor TpCustomCompress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fInputFiles := TplCompressFilesList.Create;
  fStream := nil;
end;

destructor TpCustomCompress.Destroy;
begin
  fInputFiles.Free;

  if fStreamOwner then
    if fStream<>nil then fStream.Free;

  inherited Destroy;
end;

procedure TpCustomCompress.WriteTplOCEntry(Entry: TTplOCEntry; TOCStream: TStream);
var
Str: array [0..255] of char;
EmptyByte: Byte =0;
begin
   Str := ExtractFileName(Entry.FileName);
   TOCStream.Write(Str, Length(Trim(Str)));
   TOCStream.WriteByte(EmptyByte);
   Str := Entry.FilePath;
   TOCStream.Write(Str, Length(Trim(Str)));
   TOCStream.WriteByte(EmptyByte);
   Entry.Position := NtoLE(Entry.Position);
   TOCStream.Write(Entry.Position, SizeOf(Int64));
   Entry.CompressedSize := NtoLE(Entry.CompressedSize);
   TOCStream.Write(Entry.CompressedSize, SizeOf(Int64));
   TOCStream.Write(Entry.Md5sum, SizeOf(TMD5Digest));
end;


procedure TpCustomCompress.CheckStreamAssigned;
begin
  if fStream = nil then begin
    fStream := TMemoryStream.Create;
    fStreamOwner := True;
  end;
end;

procedure TpCustomCompress.SetStream(AValue: TStream);
begin
  if AValue <> fStream then
  begin

    if fStreamOwner=true then
     if fStream<>nil then fStream.Free;

    fStreamOwner := False;
  end;
  fStream := AValue;
end;

procedure TpCustomCompress.DoError(ErrorCode: Integer; ErrorString: String);
var fErrCode: Integer;
begin
  fErrCode := ErrorCode;
  if Assigned(fOnError) then fOnError(Self, fErrCode, ErrorString);
  if fErrCode <> 0 then Raise Exception.Create('CompressError('+IntToStr(fErrCode)+') '+ErrorString);
end;

function TpCustomCompress.CreateArchive: boolean;
begin
  //.....
end;

procedure TpCustomCompress.WriteHeader(AHeader: TplArchiveHeader);
begin
  //.....
end;

function TpCustomCompress.InternalCompressStream(FileIndex: Integer; aInStream: TStream; aOutStream: TStream): Integer;
begin
  //.....
end;

//============= TpCustomUnCompress ==================================

constructor TpCustomUnCompress.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fStream := TMemoryStream.Create;
  fFileName := '';
  fTOCList := TFpList.Create;
end;

constructor TpCustomUnCompress.Create(AOwner: TComponent; InStream: TStream);
begin
  Create(AOwner);
  SetStream(InStream);
end;

destructor TpCustomUnCompress.Destroy;
begin
  FreeTOC;
  fTOCList.Free;
  inherited Destroy;
end;

procedure TpCustomUnCompress.SetStream(AStream: TStream);
begin
  fStream := AStream;
  if AStream <> nil then begin
    fStream.Position := 0;
    fStream.Read(fHeader,SizeOf(TplArchiveHeader));
    fHeader.MajorVersion := LEtoN(Header.MajorVersion);
    fHeader.MinorVersion := LEtoN(Header.MinorVersion);
    fHeader.MicroVersion := LetoN(Header.MicroVersion);
    fHeader.TOCLength := LEtoN(Header.TOCLength);
    VerifyFile;
    ReadTOC;
  end
  else begin
    FreeTOC;
  end;
end;

procedure TpCustomUnCompress.FreeTOC;
var
X: Integer;
begin
  for X := 0 to fTOCList.Count-1 do begin
    Dispose(PTplOCEntry(fTocList.Items[X]));
    fTocList.Items[X] := nil;
  end;
  fTOCList.Clear;
end;

function TpCustomUnCompress.GetCount: Integer;
begin
  Result := fTOCList.Count;
end;

function TpCustomUnCompress.GetTplOCEntry(AIndex: Integer): TTplOCEntry;
begin
  Result := PTplOCEntry(fTOCList.Items[AIndex])^;
end;

function TpCustomUnCompress.FindFilePath(const AFilePath: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to fTOCList.Count - 1 do begin
    if AFilePath = PTplOCEntry(fTOCList[i])^.FilePath+PTplOCEntry(fTOCList[i])^.FileName then
    begin
      Result:=i;
      Break;
    end;
  end;
end;

procedure TpCustomUnCompress.ExtractFileToStream(AIndex: Integer; Stream: TStream);
var
TmpStream: TMemoryStream;
Md5Sum: TMD5Digest;
begin
  if Stream = nil then Stream := TMemoryStream.Create;
  Stream.Position := 0;
  Stream.Size := 0;
  TmpStream := TMemoryStream.Create;
  try
  // Move to the position of the compressed file in the archive
  fStream.Position := FilesInArchive[AIndex].Position;
  // read the compressed file into a temp stream
  TmpStream.CopyFrom(fStream, FilesInArchive[AIndex].CompressedSize);
  // decompress the tmp stream into the output stream
  InternalExtractStream(TmpStream, Stream);
  //Check Md5 sum
  Md5Sum := StreamMD5(Stream);
  if not MD5Match(Md5Sum, FilesInArchive[AIndex].Md5sum) then begin
    DoError(ERR_CHECKSUM_FAILED, 'Saved=' + MD5Print(FilesInArchive[AIndex].Md5sum) +' Found='+ MD5Print(Md5sum));
  end;
  finally
  TmpStream.Free;
  end;

end;

procedure TpCustomUnCompress.ExtractFileToStream(const AFileName: String; Stream: TStream);
begin
  ExtractFileToStream(AFileName,'/',Stream);
end;

procedure TpCustomUnCompress.ExtractFileToStream(const AFileName, APath: String; Stream: TStream);
var
  i: Integer;
begin
  i:=FindFilePath(APath+AFileName);
  if i <> -1 then
    ExtractFileToStream(i,Stream)
  else
    DoError(ERR_FILENAME_NOT_FOUND,'Could not find '+APath+AFileName+' in '+fFileName);
end;

procedure TpCustomUnCompress.DoError(ErrorCode: Integer; ErrorString: String);
var fErrCode: Integer;
begin
  fErrCode := ErrorCode;
  if Assigned(fOnError) then fOnError(Self, fErrCode, ErrorString);
  if fErrCode <> 0 then Raise Exception.Create('UnCompressError('+IntToStr(fErrCode)+') '+ErrorString);
end;

procedure TpCustomUnCompress.VerifyFile;
begin
 //.....
end;

procedure TpCustomUnCompress.ReadTOC;
begin
  //.....
end;

function TpCustomUnCompress.InternalExtractStream(aInStream: TStream; var aOutStream: TStream): Integer;
begin
  //.....
end;


end.

