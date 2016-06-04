library alg_ios;

{$MODE objfpc}{$H+}

uses
  cthreads,
  Classes,
  sysutils,
  lockbox,
  base64,
  math;

type
  TDsaBlockEvent = class
  private
    FDsaR: string;
    FDSaS: string;
  public
    constructor Create(dsaRS: string);
    procedure evtDsaGetR(Sender: TObject; var Block: TLbDSABlock);
    procedure evtDsaGetS(Sender: TObject; var Block: TLbDSABlock);
  end;

  TRSASSABlockEvent = class
  private
    FSig: string;
  public
    constructor Create(sig: string);
    procedure evtGetSignatre(Sender: TObject; var Sig: TRSASignatureBlock);
  end;

constructor TDsaBlockEvent.Create(dsaRS: string);
var
  p: Integer;
begin
  p := Pos('|', dsaRS);
  FDsaR:= LeftStr(dsaRS, p - 1);
  FDSaS:= Copy(dsaRS, p + 1, Length(dsaRS) - p);
end;

procedure TDsaBlockEvent.evtDsaGetR(Sender: TObject; var Block: TLbDSABlock);
begin
  HexToBuffer(FDsaR, Block, SizeOf(Block));
end;

procedure TDsaBlockEvent.evtDsaGetS(Sender: TObject; var Block: TLbDSABlock);
begin
  HexToBuffer(FDsaS, Block, SizeOf(Block));
end;

constructor TRSASSABlockEvent.Create(sig: string);
begin
  FSig:= sig;
end;

procedure TRSASSABlockEvent.evtGetSignatre(Sender: TObject;
  var Sig: TRSASignatureBlock);
begin
  HexToBuffer(FSig, Sig, SizeOf(Sig));
end;


function openFileStream(AFilePath: String): TStream;
begin
  try
    Result := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  except
    Result := nil;
  end;
end;

procedure closeFileStream(AStream: TStream);
begin
  try
    AStream.Free;
  except
  end;
end;

function StringToBytes(rida:string):TDataBytes;
var
  vaherida:string;
  arv:integer;
  pikkus:integer;
  vahe:integer;
  vahealgus:integer;
  Mitmes:integer;
begin
  vahe:=AnsiPos(';',rida) ;
  if vahe>0 then begin
    rida:=copy(rida,0,vahe-1);
  end;
  vahe:=AnsiPos('/',rida) ;
  if vahe>0 then begin
    rida:=copy(rida,0,vahe-1);
  end;
  vahe:=AnsiPos(':',rida) ;
  if vahe>0 then begin
    rida:=copy(rida,vahe+1,length(rida));
  end;
  vahe:=0 ;
  vahealgus:=0;
  pikkus:=Length(rida);
  mitmes:=0;
  setlength(result,pikkus);
  while vahe<=pikkus do begin
    if IsDelimiter(#32,rida,vahe)=true then begin
      vaherida:=trim(copy(rida,vahealgus,vahe-vahealgus));
      vahealgus:=vahe;
      if length(vaherida)>0 then begin
        if TryStrToInt(vaherida,arv)=false then
          raise exception.Create('Error convert TryStrToIntExt "'+vaherida+'"');
        result[mitmes]:=arv;
        inc(mitmes);
      end;
    end else begin
      if vahe=pikkus then begin
        vaherida:=trim(copy(rida,vahealgus,vahe-vahealgus+1));
        if length(vaherida)>0 then begin
          if TryStrToInt(vaherida,arv)=false then
            raise exception.Create('Error convert TryStrToIntExt "'+vaherida+'"');
          result[mitmes]:=lo(arv);
          inc(mitmes);
        end;
      end;
    end;
    inc(vahe);
  end;
  setlength(result,mitmes);
end;

procedure setDesKey(des: TLb3DES; key: PChar);
var
  aKey: TKey128;
  tmpArr: TDataBytes;
  i: integer;
begin
  tmpArr := StringToBytes(string(key));
  for I := 0 to Length(TmpArr) - 1 do begin
    aKey[I] := tmpArr[I];
  end;
  des.SetKey(aKey);
end;


/////////////////////////////////////////////////////////////////////////////////////////////////////

var
  Buffer: array[0..1023] of Byte;

function md5EncryptString(str: PChar): PChar; cdecl;
var
  d: TMD5Digest;
  ret: string;
begin
  StringHashMD5(d, string(str));
  ret := BufferToHex(d, SizeOf(d));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function md5EncryptFile(filePath: PChar): PChar; cdecl;
var
  s: TStream;
  c: TMD5Context;
  d: TMD5Digest;
  ret: string;
  bs: Int64;
begin
  ret := '';
  s := openFileStream(string(filePath));
  if Assigned(s) then begin
    InitMD5(c);
    bs := s.Read(Buffer, SizeOf(Buffer));
    while (bs > 0) do begin
      UpdateMD5(c, Buffer, bs);
      bs := s.Read(Buffer, SizeOf(Buffer));
    end;
    FinalizeMD5(c, d);
    closeFileStream(s);
    ret := BufferToHex(d, SizeOf(d));
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function sha1EncryptString(str: PChar): PChar; cdecl;
var
  d: TSHA1Digest;
  ret: string;
begin
  StringHashSHA1(d, string(str));
  ret := BufferToHex(d, SizeOf(d));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function sha1EncryptFile(filePath: PChar): PChar; cdecl;
var
  s: TStream;
  c: TSHA1Context;
  d: TSHA1Digest;
  ret: string;
  bs: Int64;
begin
  ret := '';
  s := openFileStream(string(filePath));
  if Assigned(s) then begin
    InitSHA1(c);
    bs := s.Read(Buffer, SizeOf(Buffer));
    while (bs > 0) do begin
      UpdateSHA1(c, Buffer, bs);
      bs := s.Read(Buffer, SizeOf(Buffer));
    end;
    FinalizeSHA1(c, d);
    closeFileStream(s);
    ret := BufferToHex(d, SizeOf(d));
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function lmdEncryptString(str: PChar): PChar; cdecl;
var
  d: LongInt;
  ret: string;
begin
  StringHashLMD(d, SizeOf(d), string(str));
  ret := BufferToHex(d, SizeOf(d));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function lmdEncryptFile(filePath: PChar): PChar; cdecl;
var
  s: TStream;
  c: TLMDContext;
  d: LongInt;
  ret: string;
  bs: Int64;
begin
  ret := '';
  s := openFileStream(string(filePath));
  if Assigned(s) then begin
    InitLMD(c);
    bs := s.Read(Buffer, SizeOf(Buffer));
    while (bs > 0) do begin
      UpdateLMD(c, Buffer, bs);
      bs := s.Read(Buffer, SizeOf(Buffer));
    end;
    FinalizeLMD(c, d, SizeOf(d));
    closeFileStream(s);
    ret := BufferToHex(d, SizeOf(d));
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function elfEncryptString(str: PChar): PChar; cdecl;
var
  d: LongInt;
  ret: string;
begin
  StringHashELF(d, string(str));
  ret := BufferToHex(d, SizeOf(d));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function base64EncryptString(str: PChar): PChar; cdecl;
var
  ret: string;
begin
  ret := EncodeStringBase64(string(str));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function base64DecryptString(str: Pchar): PChar; cdecl;
var
  ret: string;
begin
  ret := DecodeStringBase64(string(str));
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;



function des3EncryptString(str: PChar; key: PChar): PChar; cdecl;
var
  des: TLb3DES;
  inBuffer: TDataBytes;
  outBuffer: TDataBytes;
  I: Integer;
  dataLen: Integer;
  S: String = '';
begin
  des := TLb3DES.Create(nil);
  try
    setDesKey(des, key);
    inBuffer := StringToBytes(string(str));
    SetLength(outBuffer, Length(inBuffer) *4);
    dataLen := des.EncryptBuffer(inBuffer[0], Length(InBuffer), outBuffer[0]);
    SetLength(outBuffer, dataLen);
    for I := 0 to Length(outBuffer) - 1 do begin
      S := S + '0x' + IntToHex(outBuffer[I],2) + ' ';
    end;
  finally
    des.Free;
  end;
  Result := StrAlloc(Length(S));
  strcopy(Result, PChar(S));
end;

function des3DecryptString(str: PChar; key: PChar): PChar; cdecl;
var
  des: TLb3DES;
  inBuffer: TDataBytes;
  outBuffer: TDataBytes;
  I, dataLen: Integer;
  S: String = '';
begin
  des := TLb3DES.Create(nil);
  try
    setDesKey(des, key);
    inBuffer := StringToBytes(string(str));
    SetLength(outBuffer, Length(inBuffer) *2);
      dataLen := des.DecryptBuffer(inBuffer[0], Length(InBuffer), outBuffer[0]);
      SetLength(outBuffer, dataLen);
      for I := 0 to Length(outBuffer) - 1 do begin
        S := S + '0x' +IntToHex(outBuffer[I],2) + ' ';
      end;
  finally
    des.Free;
  end;
  Result := StrAlloc(Length(S));
  strcopy(Result, PChar(S));
end;

function rsaGenerateKeysEx(keySize: Integer; pubPass: PChar; privPass: PChar;
  pubSavePath: PChar; privSavePath: PChar): Integer; cdecl;
var
  rsa: TLbRSA;
begin
  Result := -1;
  rsa := TLbRSA.Create(nil);
  try
    rsa.PrimeTestIterations := 20;
    rsa.KeySize:= TLbAsymKeySize(keySize);
    rsa.GenerateKeyPair;
    rsa.PublicKey.Passphrase:= String(pubPass);
    rsa.PublicKey.StoreToFile(string(pubSavePath));
    rsa.PrivateKey.Passphrase:= String(privPass);
    rsa.PrivateKey.StoreToFile(string(privSavePath));
    Result := 0;
  finally
    rsa.Free;
  end;
end;

function rsaEncryptStringEx(keySize: Integer; pubPass: PChar; pubPath: PChar;
  str: PChar): PChar; cdecl;
var
  rsa: TLbRSA;
  ret: String = '';
begin
  rsa := TLbRSA.Create(nil);
  try
    rsa.PrimeTestIterations:= 20;
    rsa.KeySize:= TLbAsymKeySize(keySize);
    rsa.PublicKey.Passphrase:= string(pubPass);
    rsa.PublicKey.LoadFromFile(string(pubPath));
    ret := rsa.EncryptString(string(str));
  finally
    rsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function rsaEncryptFileEx(keySize: Integer; pubPass: PChar; pubPath: PChar;
  filePath: PChar; outFilePath: PChar): Integer; cdecl;
var
  rsa: TLbRSA;
begin
  Result := -1;
  rsa := TLbRSA.Create(nil);
  try
    rsa.PrimeTestIterations:= 20;
    rsa.KeySize:= TLbAsymKeySize(keySize);
    rsa.PublicKey.Passphrase:= string(pubPass);
    rsa.PublicKey.LoadFromFile(string(pubPath));
    rsa.EncryptFile(string(filePath), string(outFilePath));
    Result := 0;
  finally
    rsa.Free;
  end;
end;

function rsaDecryptStringEx(keySize: Integer; privPass: PChar; privPath: PChar;
  str: PChar): PChar; cdecl;
var
  rsa: TLbRSA;
  ret: String = '';
begin
  rsa := TLbRSA.Create(nil);
  try
    rsa.PrimeTestIterations:= 20;
    rsa.KeySize:= TLbAsymKeySize(keySize);
    rsa.PrivateKey.Passphrase:= string(privPass);
    rsa.PrivateKey.LoadFromFile(string(privPath));
    ret := rsa.DecryptString(string(str));
  finally
    rsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function rsaDecryptFileEx(keySize: Integer; privPass: PChar; privPath: PChar;
  filePath: PChar; outFilePath: PChar): Integer; cdecl;
var
  rsa: TLbRSA;
begin
  Result := -1;
  rsa := TLbRSA.Create(nil);
  try
    rsa.PrimeTestIterations:= 20;
    rsa.KeySize:= TLbAsymKeySize(keySize);
    rsa.PrivateKey.Passphrase:= string(privPass);
    rsa.PrivateKey.LoadFromFile(string(privPath));
    rsa.DecryptFile(string(filePath), string(outFilePath));
    Result := 0;
  finally
    rsa.Free;
  end;
end;

function rsaGetPubkeyModulesEx(keySize: Integer; pubPass: PChar; pubPath: PChar
  ): PChar; cdecl;
var
  rsa: TLbRSA;
  ret: string = '';
begin
  rsa := TLbRSA.Create(nil);
  try
    rsa.PrimeTestIterations:= 20;
    rsa.KeySize:= TLbAsymKeySize(keySize);
    rsa.PublicKey.Passphrase:= string(pubPass);
    rsa.PublicKey.LoadFromFile(string(pubPath));
    ret := rsa.PublicKey.ModulusAsString;
  finally
    rsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function rsaGetPrivkeyModulesEx(keySize: Integer; privPass: PChar;
  privPath: PChar): PChar; cdecl;
var
  rsa: TLbRSA;
  ret: string = '';
begin
  rsa := TLbRSA.Create(nil);
  try
    rsa.PrimeTestIterations:= 20;
    rsa.KeySize:= TLbAsymKeySize(keySize);
    rsa.PrivateKey.Passphrase:= string(privPass);
    rsa.PrivateKey.LoadFromFile(string(privPath));
    ret := rsa.PrivateKey.ModulusAsString;
  finally
    rsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function dsaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar;
  pubSavePath: PChar; privSavePath: PChar): Integer; cdecl;
var
  dsa: TLbDSA;
begin
  Result := -1;
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.GenerateKeyPair;
    dsa.PublicKey.Passphrase:= String(pubPass);
    dsa.PublicKey.StoreToFile(string(pubSavePath));
    dsa.PrivateKey.Passphrase:= String(privPass);
    dsa.PrivateKey.StoreToFile(string(privSavePath));
    Result := 0;
  finally
    dsa.Free;;
  end;
end;

function dsaSignString(keySize: Integer; privPass: PChar; privPath: PChar;
  str: PChar): PChar; cdecl;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PrivateKey.Passphrase:= string(privPass);
    dsa.PrivateKey.LoadFromFile(string(privPath));
    dsa.SignString(string(str));
    ret := Format('%s|%s', [dsa.SignatureR.IntStr, dsa.SignatureS.IntStr]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function dsaSignFile(keySize: Integer; privPass: PChar; privPath: PChar;
  filePath: PChar): PChar; cdecl;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PrivateKey.Passphrase:= string(privPass);
    dsa.PrivateKey.LoadFromFile(string(privPath));
    dsa.SignFile(string(filePath));
    ret := Format('%s|%s', [dsa.SignatureR.IntStr, dsa.SignatureS.IntStr]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function dsaVerifyString(keySize: Integer; pubPass: PChar; pubPath: PChar;
  rs: PChar; str: PChar): Integer; cdecl;
var
  dsa: TLbDSA;
  evt: TDsaBlockEvent;
  ret: Boolean;
begin
  Result := -1;
  dsa := TLbDSA.Create(nil);
  evt := TDsaBlockEvent.Create(string(rs));
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PublicKey.Passphrase:= string(pubPass);
    dsa.PublicKey.LoadFromFile(pubPath);
    dsa.OnGetR:= @evt.evtDsaGetR;
    dsa.OnGetS:= @evt.evtDsaGetS;
    ret := dsa.VerifyString(string(str));
    Result := ifthen(ret, 0, 1);
  finally
    dsa.Free;
    evt.Free;
  end;
end;

function dsaVerifyFile(keySize: Integer; pubPass: PChar; pubPath: PChar;
  rs: PChar; filePath: PChar): Integer; cdecl;
var
  dsa: TLbDSA;
  evt: TDsaBlockEvent;
  ret: Boolean;
begin
  Result := -1;
  dsa := TLbDSA.Create(nil);
  evt := TDsaBlockEvent.Create(string(rs));
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PublicKey.Passphrase:= string(pubPass);
    dsa.PublicKey.LoadFromFile(pubPath);
    dsa.OnGetR:= @evt.evtDsaGetR;
    dsa.OnGetS:= @evt.evtDsaGetS;
    ret := dsa.VerifyFile(string(filePath));
    Result := ifthen(ret, 0, 1);
  finally
    dsa.Free;
    evt.Free;
  end;
end;

function dsaGetPubkeyQPGY(keySize: Integer; pubPass: PChar; pubPath: PChar
  ): PChar; cdecl;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PublicKey.Passphrase:= string(pubPass);
    dsa.PublicKey.LoadFromFile(pubPath);
    ret := Format('%s|%s|%s|%s', [dsa.PublicKey.QAsString, dsa.PublicKey.PAsString, dsa.PublicKey.GAsString, dsa.PublicKey.YAsString]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function dsaGetPrivkeyQPGX(keySize: Integer; privPass: PChar; privPath: PChar
  ): PChar; cdecl;
var
  dsa: TLbDSA;
  ret: string = '';
begin
  dsa := TLbDSA.Create(nil);
  try
    dsa.PrimeTestIterations:= 20;
    dsa.KeySize:= TLbAsymKeySize(keySize);
    dsa.PrivateKey.Passphrase:= string(privPass);
    dsa.PrivateKey.LoadFromFile(privPath);
    ret := Format('%s|%s|%s|%s', [dsa.PrivateKey.QAsString, dsa.PrivateKey.PAsString, dsa.PrivateKey.GAsString, dsa.PrivateKey.XAsString]);
  finally
    dsa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;


function rdlEncryptStringEx2(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; cdecl;
var
  rdl: TLbRijndael;
  ret: string = '';
begin
  rdl := TLbRijndael.Create(nil);
  try
    rdl.KeySize:= TLbKeySizeRDL(keySize);
    rdl.CipherMode:= TLbCipherMode(cipherMode);
    rdl.GenerateKey(string(key));
    ret  := rdl.EncryptString(string(str));
  finally
    rdl.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function rdlEncryptFileEx2(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; cdecl;
var
  rdl: TLbRijndael;
begin
  Result := -1;
  rdl := TLbRijndael.Create(nil);
  try
    rdl.KeySize:= TLbKeySizeRDL(keySize);
    rdl.CipherMode:= TLbCipherMode(cipherMode);
    rdl.GenerateKey(string(key));
    rdl.EncryptFile(string(filePath), string(outFilePath));
    Result := 0;
  finally
    rdl.Free;
  end;
end;

function rdlDecryptStringEx2(keySize: Integer; cipherMode: Integer; key: PChar;
  str: PChar): PChar; cdecl;
var
  rdl: TLbRijndael;
  ret: string = '';
begin
  rdl := TLbRijndael.Create(nil);
  try
    rdl.KeySize:= TLbKeySizeRDL(keySize);
    rdl.CipherMode:= TLbCipherMode(cipherMode);
    rdl.GenerateKey(string(key));
    ret  := rdl.DecryptString(string(str));
  finally
    rdl.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function rdlDecryptFileEx2(keySize: Integer; cipherMode: Integer; key: PChar;
  filePath: PChar; outFilePath: PChar): Integer; cdecl;
var
  rdl: TLbRijndael;
begin
  Result := -1;
  rdl := TLbRijndael.Create(nil);
  try
    rdl.KeySize:= TLbKeySizeRDL(keySize);
    rdl.CipherMode:= TLbCipherMode(cipherMode);
    rdl.GenerateKey(string(key));
    rdl.DecryptFile(string(filePath), string(outFilePath));
    Result := 0;
  finally
    rdl.Free;
  end;
end;

function rsassaGenerateKeys(keySize: Integer; pubPass: PChar; privPass: PChar;
  pubSavePath: PChar; privSavePath: PChar): Integer; cdecl;
var
  rsassa: TLbRSASSA;
begin
  Result := -1;
  rsassa := TLbRSASSA.Create(nil);
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.GenerateKeyPair;
    rsassa.PublicKey.Passphrase:= string(pubPass);
    rsassa.PublicKey.StoreToFile(string(pubSavePath));
    rsassa.PrivateKey.Passphrase:= string(privPass);
    rsassa.PrivateKey.StoreToFile(string(privSavePath));
    Result := 0;
  finally
    rsassa.Free;
  end;
end;

function rsassaSignString(keySize: Integer; hashMethod: Integer;
  privPass: PChar; privPath: PChar; str: PChar): PChar; cdecl;
var
  rsassa: TLbRSASSA;
  ret: string = '';
begin
  rsassa := TLbRSASSA.Create(nil);
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.PrivateKey.Passphrase:= string(privPass);
    rsassa.PrivateKey.LoadFromFile(string(privPath));
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.SignString(string(str));
    ret := rsassa.Signature.IntStr;
  finally
    rsassa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function rsassaSignFile(keySize: Integer; hashMethod: Integer;
  privPass: PChar; privPath: PChar; filePath: PChar): PChar; cdecl;
var
  rsassa: TLbRSASSA;
  ret: string = '';
begin
  rsassa := TLbRSASSA.Create(nil);
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.PrivateKey.Passphrase:= string(privPass);
    rsassa.PrivateKey.LoadFromFile(string(privPath));
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.SignFile(string(filePath));
    ret := rsassa.Signature.IntStr
  finally
    rsassa.Free;
  end;
  Result := StrAlloc(Length(ret));
  strcopy(Result, PChar(ret));
end;

function rsassaVerifyString(keySize: Integer; hashMethod: Integer;
  pubPass: PChar; pubPath: PChar; sig: PChar; str: PChar): Integer; cdecl;
var
  rsassa: TLbRSASSA;
  evt: TRSASSABlockEvent;
  ret: Boolean;
begin
  Result := -1;
  rsassa := TLbRSASSA.Create(nil);
  evt := TRSASSABlockEvent.Create(string(sig));
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.PublicKey.Passphrase:= string(pubPass);
    rsassa.PublicKey.LoadFromFile(string(pubPath));
    rsassa.OnGetSignature:= @evt.evtGetSignatre;
    ret := rsassa.VerifyString(string(str));
    Result := ifthen(ret, 0, 1);
  finally
    rsassa.Free;
    evt.Free;
  end;
end;

function rsassaVerifyFile(keySize: Integer; hashMethod: Integer;
  pubPass: PChar; pubPath: PChar; sig: PChar; filePath: PChar): Integer;
  cdecl;
var
  rsassa: TLbRSASSA;
  evt: TRSASSABlockEvent;
  ret: Boolean;
begin
  Result := -1;
  rsassa := TLbRSASSA.Create(nil);
  evt := TRSASSABlockEvent.Create(string(sig));
  try
    rsassa.PrimeTestIterations:= 20;
    rsassa.KeySize:= TLbAsymKeySize(keySize);
    rsassa.HashMethod:= TRSAHashMethod(hashMethod);
    rsassa.PublicKey.Passphrase:= string(pubPass);
    rsassa.PublicKey.LoadFromFile(string(pubPath));
    rsassa.OnGetSignature:= @evt.evtGetSignatre;
    ret := rsassa.VerifyFile(string(filePath));
    Result := ifthen(ret, 0, 1);
  finally
    rsassa.Free;
    evt.Free;
  end;
end;

exports
  md5EncryptString,
  md5EncryptFile,
  sha1EncryptString,
  sha1EncryptFile,
  lmdEncryptString,
  lmdEncryptFile,
  elfEncryptString,
  base64EncryptString,
  base64DecryptString,
  des3EncryptString,
  des3DecryptString,
  rsaGenerateKeysEx,
  rsaEncryptStringEx,
  rsaEncryptFileEx,
  rsaDecryptStringEx,
  rsaDecryptFileEx,
  rsaGetPubkeyModulesEx,
  rsaGetPrivkeyModulesEx,
  dsaGenerateKeys,
  dsaSignString,
  dsaSignFile,
  dsaVerifyString,
  dsaVerifyFile,
  dsaGetPubkeyQPGY,
  dsaGetPrivkeyQPGX,
  rdlEncryptStringEx2,
  rdlEncryptFileEx2,
  rdlDecryptStringEx2,
  rdlDecryptFileEx2,
  rsassaGenerateKeys,
  rsassaSignString,
  rsassaSignFile,
  rsassaVerifyString,
  rsassaVerifyFile;

begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
end.
