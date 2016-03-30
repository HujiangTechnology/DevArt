unit bzip2lib;

{****************************************************************************

  Stream classes for BZIP2 compression and decompression.
  Class TBzip2CompressStream is for compressing (packing) data to bzip2 format and
  class TBzip2DecompressStream is for decompressing (unpacking) data back to its original form.

  This unit is collected and edited from 2 unit files in Jcl package,
    JclCompression and bzip2.
  The purpose was to make it compile and work with FPC (Free Pascal Compiler).
  In the process many code blocks with conditional compilation directives were removed
  and also many unit dependencies were removed.

  The code uses external bzip2 libraries for its job:
    'bzip2.dll' in Windows
    'libbz2.so.1' in Linux

  This code is licensed under the same terms as Jcl library.

  Author: Juha Manninen <juha dot manninen (at) phnet dot fi>
         + the original authors from Jcl code.

****************************************************************************}

{$MODE Delphi}

interface

uses
  Classes, SysUtils;

//------------------------------------------------------------------------
// The beginning is copied and modified from bzip2 unit under Jcl package.
// Extra conditionals etc. are stripped out.
//------------------------------------------------------------------------

const
  BZ_RUN              = 0;
  BZ_FLUSH            = 1;
  BZ_FINISH           = 2;

  BZ_OK               = 0;
  BZ_RUN_OK           = 1;
  BZ_FLUSH_OK         = 2;
  BZ_FINISH_OK        = 3;
  BZ_STREAM_END       = 4;
  BZ_SEQUENCE_ERROR   = -1;
  BZ_PARAM_ERROR      = -2;
  BZ_MEM_ERROR        = -3;
  BZ_DATA_ERROR       = -4;
  BZ_DATA_ERROR_MAGIC = -5;
  BZ_IO_ERROR         = -6;
  BZ_UNEXPECTED_EOF   = -7;
  BZ_OUTBUFF_FULL     = -8;
  BZ_CONFIG_ERROR     = -9;

type
   bz_stream = record
      next_in: PByte;
      avail_in: Cardinal;
      total_in_lo32: Cardinal;
      total_in_hi32: Cardinal;

      next_out: PByte;
      avail_out: Cardinal;
      total_out_lo32: Cardinal;
      total_out_hi32: Cardinal;

      state: Pointer;

      bzalloc: function (opaque: Pointer; n, m: Integer): Pointer; cdecl; // returns n*m bytes
      bzfree: procedure (opaque, p: Pointer); cdecl; // free p
      opaque: Pointer;
   end;

type
  BZ2_bzCompressInit_func = function(var strm: bz_stream;
    blockSize100k, verbosity, workFactor: Integer): Integer; stdcall; cdecl;
  BZ2_bzCompress_func = function(var strm: bz_stream; action: Integer): Integer; stdcall; cdecl;
  BZ2_bzCompressEnd_func = function(var strm: bz_stream): Integer; stdcall; cdecl;
  BZ2_bzDecompressInit_func = function(var strm: bz_stream; verbosity, small: Integer): Integer; stdcall; cdecl;
  BZ2_bzDecompress_func = function(var strm: bz_stream): Integer; stdcall; cdecl;
  BZ2_bzDecompressEnd_func = function(var strm: bz_stream): Integer; stdcall; cdecl;
  BZ2_bzBuffToBuffCompress_func = function(dest: PByte; destLen: PCardinal;
    source: PByte; sourceLen: Cardinal; blockSize100k, verbosity, workFactor: Integer): Integer; stdcall; cdecl;
  BZ2_bzBuffToBuffDecompress_func = function(dest: PByte; destLen: PCardinal;
    source: PByte; sourceLen: Cardinal; small, verbosity: Integer): Integer; stdcall; cdecl;
  BZ2_bzlibVersion_func = function: PAnsiChar; stdcall; cdecl;

var
  BZ2_bzCompressInit: BZ2_bzCompressInit_func = nil;
  BZ2_bzCompress: BZ2_bzCompress_func = nil;
  BZ2_bzCompressEnd: BZ2_bzCompressEnd_func = nil;
  BZ2_bzDecompressInit: BZ2_bzDecompressInit_func = nil;
  BZ2_bzDecompress: BZ2_bzDecompress_func = nil;
  BZ2_bzDecompressEnd: BZ2_bzDecompressEnd_func = nil;
  BZ2_bzBuffToBuffCompress: BZ2_bzBuffToBuffCompress_func = nil;
  BZ2_bzBuffToBuffDecompress: BZ2_bzBuffToBuffDecompress_func = nil;
  BZ2_bzlibVersion: BZ2_bzlibVersion_func = nil;

var
  bz2_internal_error_event: procedure(errcode: Integer) of object = nil;

function LoadBZip2: Boolean;
function IsBZip2Loaded: Boolean;
procedure UnloadBZip2;


//----------------------------------------------------------------------
// The following types are copied and modified from JclCompression unit.
//----------------------------------------------------------------------

type
  // Base class for BZIP2 compression and decompression.
  TCompressStreamBase = class(TStream)
  private
    FOnProgress: TNotifyEvent;
    FBuffer: Pointer;
    FBufferSize: Cardinal;
    FStream: TStream;
  protected
    function SetBufferSize(Size: Cardinal): Cardinal; virtual;
    procedure Progress(Sender: TObject); dynamic;
    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  public
    class function StreamName: string; virtual;
    class function StreamExtensions: string; virtual;

    constructor Create(AStream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    procedure Reset; virtual;
  end;


  // BZIP2 compression
  TBzip2CompressStream = class(TCompressStreamBase)
  private
    FDeflateInitialized: Boolean;
    FCompressionLevel: Integer;
  protected
    BZLibRecord: bz_stream;
    procedure SetCompressionLevel(const Value: Integer);
  public
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

    constructor Create(Destination: TStream; ACompressionLevel: integer = 9);
    destructor Destroy; override;

    function Flush: Integer; //override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    property CompressionLevel: Integer read FCompressionLevel write SetCompressionLevel;
  end;


  // BZIP2 decompression
  TBzip2DecompressStream = class(TCompressStreamBase)
  private
    FInflateInitialized: Boolean;
  protected
    BZLibRecord: bz_stream;
  public
    // stream description
    class function StreamName: string; override;
    class function StreamExtensions: string; override;

    constructor Create(Source: TStream); overload;
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;


  CompressionError = class(Exception);

resourcestring
//  RsCompressionOperationNotSupported = 'Operation is not supported.';
  RsCompressionReadNotSupported      = 'read is not an supported operation.';
  RsCompressionWriteNotSupported     = 'write is not an supported operation.';
  RsCompressionResetNotSupported     = 'reset is not an supported operation.';
  RsCompressionSeekNotSupported      = 'seek is not an supported operation.';
  RsCompressionBZIP2SequenceError    = 'bzip2 returned: sequence error';
  RsCompressionBZIP2ParameterError   = 'bzip2 returned: parameter error';
  RsCompressionBZIP2MemoryError      = 'bzip2 returned: memory error';
  RsCompressionBZIP2DataError        = 'bzip2 returned: data error';
  RsCompressionBZIP2HeaderError      = 'bzip2 returned: header error';
  RsCompressionBZIP2IOError          = 'bzip2 returned: IO error';
  RsCompressionBZIP2EOFError         = 'bzip2 returned: unexpected end of file';
  RsCompressionBZIP2OutBuffError     = 'bzip2 returned: out buffer is too small';
  RsCompressionBZIP2ConfigError      = 'bzip2 returned: configuration error';
  RsCompressionBZIP2Error            = 'bzip2 returned: unknown error (%d)';
  RsCompressionBZip2Name             = 'BZip2 archive';
  RsCompressionBZip2Extensions       = '*.bz2;*.bzip2;*.tbz2;*.tbz';


implementation

//------------------------------------------------------------------------
// This is the implementation originally from bzip2 unit under Jcl package.
// Extra conditionals etc. are stripped out.
//------------------------------------------------------------------------

{$Hints off}

uses
  {$IFDEF MSWINDOWS}
  Windows;
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  Types,
  {$IFDEF HAS_UNIT_LIBC}
  Libc;
  {$ELSE ~HAS_UNIT_LIBC}
  dl;
  {$ENDIF ~HAS_UNIT_LIBC}
  {$ENDIF UNIX}

type
  {$IFDEF WINDOWS}
  TModuleHandle = HINST;
  {$ELSE}
  TModuleHandle = Pointer;
  {$ENDIF}

const
  {$IFDEF WINDOWS}
  szBZIP2 = 'bzip2.dll'; // from http://gnuwin32.sourceforge.net/
  {$ELSE}
  {$IFDEF DARWIN}
  szBZIP2 = 'libbz2.1.0.dylib';
  {$ELSE}
  szBZIP2 = 'libbz2.so.1';
  {$ENDIF}
  {$ENDIF}

  BZ2CompressInitExportName = 'BZ2_bzCompressInit';
  BZ2CompressExportName = 'BZ2_bzCompress';
  BZ2CompressEndExportName = 'BZ2_bzCompressEnd';
  BZ2DecompressInitExportName = 'BZ2_bzDecompressInit';
  BZ2DecompressExportName = 'BZ2_bzDecompress';
  BZ2DecompressEndExportName = 'BZ2_bzDecompressEnd';
  BZ2BuffToBuffCompressExportName = 'BZ2_bzBuffToBuffCompress';
  BZ2BuffToBuffDecompressExportName = 'BZ2_bzBuffToBuffDecompress';
  BZ2LibVersionExportName = 'BZ2_bzlibVersion';
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

var
  BZip2Lib: TModuleHandle = INVALID_MODULEHANDLE_VALUE;

function LoadBZip2: Boolean;
  function GetSymbol(SymbolName: PAnsiChar): Pointer;
  begin
    {$IFDEF WINDOWS}
    Result := GetProcAddress(BZip2Lib, SymbolName);
    {$ENDIF MSWINDOWS}
    {$IFDEF IX}
    Result := dlsym(BZip2Lib, SymbolName);
    {$ENDIF UNIX}
  end;
begin
  Result := BZip2Lib <> INVALID_MODULEHANDLE_VALUE;
  if Result then
    Exit;

  if BZip2Lib = INVALID_MODULEHANDLE_VALUE then
    {$IFDEF WINDOWS}
    BZip2Lib := SafeLoadLibrary(szBZIP2);
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
    BZip2Lib := dlopen(PAnsiChar(szBZIP2), RTLD_NOW);
    {$ENDIF UNIX}
  Result := BZip2Lib <> INVALID_MODULEHANDLE_VALUE;
  if Result then
  begin
    @BZ2_bzCompressInit := GetSymbol(BZ2CompressInitExportName);
    @BZ2_bzCompress := GetSymbol(BZ2CompressExportName);
    @BZ2_bzCompressEnd := GetSymbol(BZ2CompressEndExportName);
    @BZ2_bzDecompressInit := GetSymbol(BZ2DecompressInitExportName);
    @BZ2_bzDecompress := GetSymbol(BZ2DecompressExportName);
    @BZ2_bzDecompressEnd := GetSymbol(BZ2DecompressEndExportName);
    @BZ2_bzBuffToBuffCompress := GetSymbol(BZ2BuffToBuffCompressExportName);
    @BZ2_bzBuffToBuffDecompress := GetSymbol(BZ2BuffToBuffDecompressExportName);
    @BZ2_bzlibVersion := GetSymbol(BZ2LibVersionExportName);
  end;
end;

function IsBZip2Loaded: Boolean;
begin
  Result := BZip2Lib <> INVALID_MODULEHANDLE_VALUE;
end;

procedure UnloadBZip2;
begin
  if BZip2Lib <> INVALID_MODULEHANDLE_VALUE then
    {$IFDEF WINDOWS}
    FreeLibrary(BZip2Lib);
    {$ENDIF WINDOWS}
    {$IFDEF UNIX}
    dlclose(Pointer(BZip2Lib));
    {$ENDIF UNIX}
  BZip2Lib := INVALID_MODULEHANDLE_VALUE;
end;


//----------------------------------------------------------------------
// The following types are copied and modified from JclCompression unit.
//----------------------------------------------------------------------

const
  DefaultBufferSize = 131072; // 128k

//=== { TCompressStreamBase } ==============================================

constructor TCompressStreamBase.Create(AStream: TStream);
begin
  inherited Create;
  FBuffer := nil;
  SetBufferSize(DefaultBufferSize);
  FStream := AStream;
end;

destructor TCompressStreamBase.Destroy;
begin
  SetBufferSize(0);
  inherited Destroy;
end;

function TCompressStreamBase.Read(var Buffer; Count: Longint): Longint;
begin
  raise CompressionError.CreateRes(@RsCompressionReadNotSupported);
end;

function TCompressStreamBase.Write(const Buffer; Count: Longint): Longint;
begin
  raise CompressionError.CreateRes(@RsCompressionWriteNotSupported);
end;

function TCompressStreamBase.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  raise CompressionError.CreateRes(@RsCompressionSeekNotSupported);
end;

procedure TCompressStreamBase.Reset;
begin
  raise CompressionError.CreateRes(@RsCompressionResetNotSupported);
end;

function TCompressStreamBase.SetBufferSize(Size: Cardinal): Cardinal;
begin
  if FBuffer <> nil then
    FreeMem(FBuffer, FBufferSize);
  FBufferSize := Size;
  if FBufferSize > 0 then
    GetMem(FBuffer, FBufferSize)
  else
    FBuffer := nil;
  Result := FBufferSize;
end;

class function TCompressStreamBase.StreamExtensions: string;
begin
  Result := '';
end;

class function TCompressStreamBase.StreamName: string;
begin
  Result := '';
end;

procedure TCompressStreamBase.Progress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender);
end;


//=== { TBzip2CompressStream } ==============================================

{ Error checking helper }

function BZIP2LibCheck(const ErrCode: Integer): Integer;
begin
  case ErrCode of
    0..High(ErrCode):
      Result := ErrCode; // no error
    BZ_SEQUENCE_ERROR:
      raise CompressionError.CreateRes(@RsCompressionBZIP2SequenceError);
    BZ_PARAM_ERROR:
      raise CompressionError.CreateRes(@RsCompressionBZIP2ParameterError);
    BZ_MEM_ERROR:
      raise CompressionError.CreateRes(@RsCompressionBZIP2MemoryError);
    BZ_DATA_ERROR:
      raise CompressionError.CreateRes(@RsCompressionBZIP2DataError);
    BZ_DATA_ERROR_MAGIC:
      raise CompressionError.CreateRes(@RsCompressionBZIP2HeaderError);
    BZ_IO_ERROR:
      raise CompressionError.CreateRes(@RsCompressionBZIP2IOError);
    BZ_UNEXPECTED_EOF:
      raise CompressionError.CreateRes(@RsCompressionBZIP2EOFError);
    BZ_OUTBUFF_FULL:
      raise CompressionError.CreateRes(@RsCompressionBZIP2OutBuffError);
    BZ_CONFIG_ERROR:
      raise CompressionError.CreateRes(@RsCompressionBZIP2ConfigError);
  else
    raise CompressionError.CreateResFmt(@RsCompressionBZIP2Error, [ErrCode]);
  end;
end;

constructor TBzip2CompressStream.Create(Destination: TStream; ACompressionLevel: integer);
begin
  inherited Create(Destination);

  LoadBZip2;

  Assert(FBuffer <> nil);
  Assert(FBufferSize > 0);

  // Initialize ZLib StreamRecord
  BZLibRecord.bzalloc   := nil; // Use build-in memory allocation functionality
  BZLibRecord.bzfree    := nil;
  BZLibRecord.next_in   := nil;
  BZLibRecord.avail_in  := 0;
  BZLibRecord.next_out  := FBuffer;
  BZLibRecord.avail_out := FBufferSize;

  FDeflateInitialized := False;

  FCompressionLevel := ACompressionLevel;
end;

destructor TBzip2CompressStream.Destroy;
begin
  Flush;
  if FDeflateInitialized then
    BZIP2LibCheck(BZ2_bzCompressEnd(BZLibRecord));

  inherited Destroy;
end;

function TBzip2CompressStream.Flush: Integer;
begin
  Result := 0;

  if FDeflateInitialized then
  begin
    BZLibRecord.next_in := nil;
    BZLibRecord.avail_in := 0;

    while (BZIP2LibCheck(BZ2_bzCompress(BZLibRecord, BZ_FINISH)) <> BZ_STREAM_END) and (BZLibRecord.avail_out = 0) do
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);

      BZLibRecord.next_out := FBuffer;
      BZLibRecord.avail_out := FBufferSize;
      Inc(Result, FBufferSize);
    end;

    if BZLibRecord.avail_out < FBufferSize then
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize - BZLibRecord.avail_out);
      Progress(Self);
      Inc(Result, FBufferSize - BZLibRecord.avail_out);
      BZLibRecord.next_out := FBuffer;
      BZLibRecord.avail_out := FBufferSize;
    end;
  end;
end;

function TBzip2CompressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
   if (Offset = 0) and (Origin = soCurrent) then
    Result := (BZLibRecord.total_in_hi32 shl 32) or BZLibRecord.total_in_lo32
   else
   if (Offset = 0) and (Origin = soBeginning) and (BZLibRecord.total_in_lo32 = 0) then
       Result := 0
   else
     Result := inherited Seek(Offset, Origin);
end;

procedure TBzip2CompressStream.SetCompressionLevel(const Value: Integer);
begin
  if not FDeflateInitialized then
    FCompressionLevel := Value
  else
    raise CompressionError.CreateRes(@RsCompressionBZIP2SequenceError);
end;

class function TBzip2CompressStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionBZip2Extensions);
end;

class function TBzip2CompressStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionBZip2Name);
end;

function TBzip2CompressStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not FDeflateInitialized then
  begin
    BZIP2LibCheck(BZ2_bzCompressInit(BZLibRecord, FCompressionLevel, 0, 0));
    FDeflateInitialized := True;
  end;

  BZLibRecord.next_in := @Buffer;
  BZLibRecord.avail_in := Count;

  while BZLibRecord.avail_in > 0 do
  begin
    BZIP2LibCheck(BZ2_bzCompress(BZLibRecord, BZ_RUN));

    if BZLibRecord.avail_out = 0 then   // Output buffer empty. Write to stream and go on...
    begin
      FStream.WriteBuffer(FBuffer^, FBufferSize);
      Progress(Self);
      BZLibRecord.next_out := FBuffer;
      BZLibRecord.avail_out := FBufferSize;
    end;
  end;

  Result := Count;
end;


//=== { TBzip2DecompressStream } =======================================

constructor TBzip2DecompressStream.Create(Source: TStream);
begin
  inherited Create(Source);

  LoadBZip2;

  // Initialize ZLib StreamRecord
  BZLibRecord.bzalloc   := nil; // Use build-in memory allocation functionality
  BZLibRecord.bzfree    := nil;
  BZLibRecord.opaque    := nil;
  BZLibRecord.next_in   := nil;
  BZLibRecord.state     := nil;
  BZLibRecord.avail_in  := 0;
  BZLibRecord.next_out  := FBuffer;
  BZLibRecord.avail_out := FBufferSize;

  FInflateInitialized := False;
end;

destructor TBzip2DecompressStream.Destroy;
begin
  if FInflateInitialized then
  begin
    FStream.Seek(-BZLibRecord.avail_in, soFromCurrent);
    BZIP2LibCheck(BZ2_bzDecompressEnd(BZLibRecord));
  end;

  inherited Destroy;
end;

function TBzip2DecompressStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not FInflateInitialized then
  begin
    BZIP2LibCheck(BZ2_bzDecompressInit(BZLibRecord, 0, 0));
    FInflateInitialized := True;
  end;

  BZLibRecord.next_out := @Buffer;
  BZLibRecord.avail_out := Count;
  Result := 0;

  while Result < Count do     // as long as we need data
  begin
    if BZLibRecord.avail_in = 0 then // no more compressed data
    begin
      BZLibRecord.avail_in := FStream.Read(FBuffer^, FBufferSize);
      if BZLibRecord.avail_in = 0 then
        Exit;

      BZLibRecord.next_in := FBuffer;
    end;

    if BZLibRecord.avail_in > 0 then
    begin
      BZIP2LibCheck(BZ2_bzDecompress(BZLibRecord));
      Result := Count;
      Dec(Result, BZLibRecord.avail_out);
    end
  end;

  Result := Count;
end;

function TBzip2DecompressStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
   if (Offset = 0) and (Origin = soCurrent) then
    Result := (BZLibRecord.total_out_hi32 shl 32) or BZLibRecord.total_out_lo32
   else
     Result := inherited Seek(Offset, Origin);
end;

class function TBzip2DecompressStream.StreamExtensions: string;
begin
  Result := LoadResString(@RsCompressionBZip2Extensions);
end;

class function TBzip2DecompressStream.StreamName: string;
begin
  Result := LoadResString(@RsCompressionBZip2Name);
end;


end.

