unit adot.Hash;

{
  TCustomHash = class
    Abstract class for hashes.

  THashes = class
    Simple API for hashing functions (including CRC32/Adler32)

}

interface

uses
  System.ZLib,
  System.Hash,
  System.SysUtils,
  System.Classes,
  System.Math;

type
  THashData = TArray<byte>;

  { Abstract class for hashes }
  TCustomHash = class abstract
  protected

    { used by CRC/Adler to transform 32bit hash to TBytes }
    class function Hash32ToBytes(Hash: Cardinal): TBytes; static;

    { single call }
    class function DoEncode(const Buf; ByteBufSize: integer): TBytes; overload; virtual; abstract;
    class function DoEncode(S: TStream): TBytes; overload; virtual; abstract;

    { streaming }
    class procedure DoInit(out Hash: THashData); virtual; abstract;
    class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); virtual; abstract;
    class function DoDone(var Hash: THashData): TBytes; virtual; abstract;
  public

    { single call functions }
    class function Encode(const Buf; ByteBufSize: integer): TBytes; overload;
    class function Encode(S: TStream): TBytes; overload;
    class function Encode(const S: TBytes; StartIndex,Count: integer): TBytes; overload;
    class function Encode(const S: TBytes): TBytes; overload;
    class function Encode(const S: string): TBytes; overload;
    {$If Defined(MSWindows)}
      class function EncodeAnsiString(const S: AnsiString): TBytes;
    {$EndIf}
    class function Encode(const Lines: TArray<string>): TBytes; overload;
    class function EncodeFile(const AFileName: string): TBytes; overload;

    { streaming functions }
    class procedure Init(out Hash: THashData);

    { for values with fixed length, hash will be generated from data only }
    class procedure Update(const Value; ValueByteSize: integer; var Hash: THashData); overload;
    class procedure Update(const Value: integer;      var Hash: THashData); overload;
    class procedure Update(const Value: double;       var Hash: THashData); overload;
    class procedure Update(const Value: TGUID;        var Hash: THashData); overload;

    { for values with variable length, hash will be generated from length and data }
    class procedure Update(const Value: TArray<byte>; var Hash: THashData); overload;
    class procedure Update(const Value: string;       var Hash: THashData); overload;

    class function Done(var Hash: THashData): TBytes;

  end;

  THashClass = class of TCustomHash;

  { AH: Don't use THashMD5/THashSHA1 directly, implementation in XE8 has serious bugs:
    http://qc.embarcadero.com/wc/qcmain.aspx?d=132100
    AH (update from 05.04.2016): The issue is fixed, in Delphi 10 Seattle it works correctly.
    Why we still keep THashes class:
    - For now Delphi doesn't have CRC/Adler (usefull for files, but can be replaced by THashBobJenkins)
    - In object model it is much easier to introduce new functions (like hash file/stream etc)
    - If other issues will be discovered in Delphi lib, we can fix it without changes in the code

   Simple API for hashing functions (including CRC32/Adler32). Example:

    function GetHash(L: TList<TRec>): string;
    var
      H: THashClass;
      D: THashData;
      I: Integer;
    begin
      H := THashUtils.Strong;
      H.Init(D);
      for I := 0 to List.Count-1 do
        H.Update(L[I], SizeOf(L[I]), D);
      result := THashUtils.HashToString(H.Done(D));
    end;

    Same example without THashClass:

    function GetHash(L: TList<TRec>): string;
    var
      D: THashData;
      I: Integer;
    begin
      THashUtils.Strong.Init(D);
      for I := 0 to List.Count-1 do
        THashUtils.Strong.Update(L[I], SizeOf(L[I]), D);
      result := THashUtils.HashToString(THashUtils.Strong.Done(D));
    end; }
  THashUtils = class
  private
    type
      { Stream block reader (ReadNext to get next block of data from stream as array of byte) }
      TBlockReader = class
      private
        Stream: TStream;
        OwnsStream: boolean;
        BytesToRead: int64;

      public
        Bytes: TArray<Byte>;
        Count: integer;

        constructor Create(Src: TStream; AOwnsStream: Boolean; BufSize: integer; FromBeginning: boolean = True);
        destructor Destroy; override;

        function ReadNext: Boolean;
      end;

  public
    const
      StreamingBufSize = 64*1024;

    type
      MD5 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      SHA1 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      { we use default 256bit hash (use THashSHA2 directly for other options - 224bit,384bit,...) }
      SHA2 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      CRC32 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      Adler32 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      BobJenkins32 = class(TCustomHash)
      protected
        class function DoEncode(const Buf; ByteBufSize: integer): TBytes; override;
        class function DoEncode(S: TStream): TBytes; override;
        class procedure DoInit(out Hash: THashData); override;
        class procedure DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData); override;
        class function DoDone(var Hash: THashData): TBytes; override;
      end;

      { Strong hash for critical parts (password checksum etc).
        MD5 is outdated for use in cryptography, but for other tasks it's still good enough }
      Strong = MD5;

      { Fast hash with good avalanche effect (hash tables etc). }
      Fast = BobJenkins32;

      { Fastest hash for detection of modifications in massive data arrays (file checksum etc).
        We use Adler32, it's two times faster than Crc32 and still quite good. }
      Fastest = Adler32;

    { general }
    class function Mix(const HashA,HashB: integer): integer; overload; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function Mix(const HashA,HashB,HashC: integer): integer; overload; static;
    class function Mix(const HashA,HashB: TBytes): TBytes; overload; static;
    class function Mix(const Hashes: array of integer): integer; overload; static;
    class function Mix(const Hashes: array of TBytes): TBytes; overload; static;

    class function HashToString(const AHash: TBytes): string; static;

    class function GetHash32(const Hash: TBytes): integer; static;
    class function GetHash24(const Hash: TBytes): integer; static;
    class function GetHash16(const Hash: TBytes): integer; static;
  end;

  THashes = THashUtils;

implementation

uses
  adot.Tools;

{ THashUtils.TBlockReader }

constructor THashUtils.TBlockReader.Create(Src: TStream; AOwnsStream: Boolean; BufSize: integer; FromBeginning: boolean);
begin
  Stream := Src;
  OwnsStream := AOwnsStream;
  SetLength(Bytes, BufSize);
  if not FromBeginning then
    BytesToRead := Stream.Size - Stream.Position
  else
  begin
    Stream.Position := 0;
    BytesToRead := Stream.Size;
  end;
end;

destructor THashUtils.TBlockReader.Destroy;
begin
  if OwnsStream then
    Stream.Free;
  Stream := nil;
  inherited;
end;

function THashUtils.TBlockReader.ReadNext: Boolean;
begin
  Result := BytesToRead > 0;
  if Result then
  begin
    Count := Min(BytesToRead, Length(Bytes));
    Stream.ReadBuffer(Bytes,  Count);
    Dec(BytesToRead, Count);
  end
  else
  begin
    SetLength(Bytes, 0);
    Count := 0;
  end;
end;

{ TCustomHash }

class function TCustomHash.Encode(const Buf; ByteBufSize: integer): TBytes;
begin
  result := DoEncode(Buf, ByteBufSize);
end;

class function TCustomHash.Encode(S: TStream): TBytes;
begin
  result := DoEncode(S);
end;

class function TCustomHash.Encode(const S: TBytes; StartIndex,Count: integer): TBytes;
begin
  Assert((StartIndex >= 0) and (StartIndex + Count <= Length(S)));
  if Count <= 0 then
    result := DoEncode(nil^, 0)
  else
    result := DoEncode(S[StartIndex], Count);
end;

class function TCustomHash.Encode(const S: TBytes): TBytes;
begin
  result := Encode(S, 0, Length(S));
end;

class function TCustomHash.Encode(const S: string): TBytes;
begin
  if Length(S)=0 then
    result := DoEncode(nil^, 0)
  else
    result := DoEncode(S[Low(S)], length(S)*SizeOf(S[Low(S)]));
end;

class function TCustomHash.Encode(const Lines: TArray<string>): TBytes;
var
  H: THashData;
  S: string;
begin
  Init(H);
  for S in Lines do
    Update(S, H);
  result := Done(H);
end;

{$If Defined(MSWindows)}
class function TCustomHash.EncodeAnsiString(const S: AnsiString): TBytes;
begin
  if Length(S)=0 then
    result := DoEncode(nil^, 0)
  else
    result := DoEncode(S[Low(S)], length(S)*SizeOf(S[Low(S)]));
end;
{$EndIf}

class function TCustomHash.EncodeFile(const AFileName: string): TBytes;
var
  S: TFileStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Result := DoEncode(S);
  finally
    S.Free;
  end;
end;

class function TCustomHash.Hash32ToBytes(Hash: Cardinal): TBytes;
begin
  SetLength(Result, 4);
  PCardinal(@Result[0])^ := System.Hash.THash.ToBigEndian(Hash);
end;

class procedure TCustomHash.Init(out Hash: TArray<byte>);
begin
  DoInit(Hash);
end;

class procedure TCustomHash.Update(const Value; ValueByteSize: integer; var Hash: THashData);
begin
  DoUpdate(Value, ValueByteSize, Hash);
end;

class procedure TCustomHash.Update(const Value: integer; var Hash: THashData);
begin
  DoUpdate(Value, SizeOf(Value), Hash);
end;

class procedure TCustomHash.Update(const Value: double; var Hash: THashData);
begin
  DoUpdate(Value, SizeOf(Value), Hash);
end;

class procedure TCustomHash.Update(const Value: TGUID; var Hash: THashData);
begin
  DoUpdate(Value, SizeOf(Value), Hash);
end;

class procedure TCustomHash.Update(const Value: TArray<byte>; var Hash: THashData);
begin
  { Length must be included in hash, otherwise arrays
    [a, ab] and [aa, b] will produce same hash }
  Update(Length(Value), Hash);
  if Length(Value) > 0 then
    DoUpdate(Value[0], Length(Value), Hash);
end;

class procedure TCustomHash.Update(const Value: string; var Hash: THashData);
begin
  { Length must be included in hash, otherwise arrays
    ["a", "ab"] and ["aa", "b"] will produce same hash }
  Update(Length(Value), Hash);
  if Value <> '' then
    DoUpdate(Value[Low(Value)], Length(Value)*SizeOf(Char), Hash);
end;

class function TCustomHash.Done(var Hash: TArray<byte>): TBytes;
begin
  result := DoDone(Hash);
end;

{ TMD5 }

class function THashUtils.MD5.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Hash: THashMD5;
begin
  Hash := THashMD5.Create; { Create may have params and is not equal to Reset }
  Hash.Update(Buf, ByteBufSize);
  Result := Hash.HashAsBytes;
end;

class function THashUtils.MD5.DoEncode(S: TStream): TBytes;
var
  Reader: TBlockReader;
  Hash: THashMD5;
begin
  Reader := TBlockReader.Create(S, False, StreamingBufSize, True);
  try
    Hash := THashMD5.Create; { Create may have params and is not equal to Reset }
    while Reader.ReadNext do
      Hash.Update(Reader.Bytes, Reader.Count);
    Result := Hash.HashAsBytes;
  finally
    Reader.Free;
  end;
end;

class procedure THashUtils.MD5.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashMD5));
  THashMD5((@Hash[0])^) := THashMD5.Create;
end;

class procedure THashUtils.MD5.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashMD5));
  THashMD5((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.MD5.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashMD5));
  result := THashMD5((@Hash[0])^).HashAsBytes;
  THashMD5((@Hash[0])^) := Default(THashMD5);
  SetLength(Hash, 0);
end;

{ THashUtils.SHA1 }

class function THashUtils.SHA1.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Hash: THashSHA1;
begin
  Hash := THashSHA1.Create;
  Hash.Update(Buf, ByteBufSize);
  Result := Hash.HashAsBytes;
end;

class function THashUtils.SHA1.DoEncode(S: TStream): TBytes;
var
  Reader: TBlockReader;
  Hash: THashSHA1;
begin
  Reader := TBlockReader.Create(S, False, StreamingBufSize, True);
  try
    Hash := THashSHA1.Create;
    while Reader.ReadNext do
      Hash.Update(Reader.Bytes, Reader.Count);
    Result := Hash.HashAsBytes;
  finally
    Reader.Free;
  end;
end;

class procedure THashUtils.SHA1.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashSHA1));
  THashSHA1((@Hash[0])^) := THashSHA1.Create;
end;

class procedure THashUtils.SHA1.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashSHA1));
  THashSHA1((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.SHA1.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashSHA1));
  result := THashSHA1((@Hash[0])^).HashAsBytes;
  THashSHA1((@Hash[0])^) := Default(THashSHA1);
  SetLength(Hash, 0);
end;

{ THashUtils.SHA2 }

class function THashUtils.SHA2.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Hash: THashSHA2;
begin
  Hash := THashSHA2.Create;
  Hash.Update(Buf, ByteBufSize);
  Result := Hash.HashAsBytes;
end;

class function THashUtils.SHA2.DoEncode(S: TStream): TBytes;
var
  Reader: TBlockReader;
  Hash: THashSHA2;
begin
  Reader := TBlockReader.Create(S, False, StreamingBufSize, True);
  try
    Hash := THashSHA2.Create;
    while Reader.ReadNext do
      Hash.Update(Reader.Bytes, Reader.Count);
    Result := Hash.HashAsBytes;
  finally
    Reader.Free;
  end;
end;

class procedure THashUtils.SHA2.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashSHA2));
  THashSHA2((@Hash[0])^) := THashSHA2.Create;
end;

class procedure THashUtils.SHA2.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashSHA2));
  THashSHA2((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.SHA2.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashSHA2));
  result := THashSHA2((@Hash[0])^).HashAsBytes;
  THashSHA2((@Hash[0])^) := Default(THashSHA2);
  SetLength(Hash, 0);
end;

{ THashUtils.CRC32 }

class function THashUtils.CRC32.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Crc: Cardinal;
begin
  Crc := System.ZLib.crc32(0, nil, 0);
  Crc := System.ZLib.crc32(Crc, @Buf, ByteBufSize);
  Result := Hash32ToBytes(Crc);
end;

class function THashUtils.CRC32.DoEncode(S: TStream): TBytes;
var
  Reader: TBlockReader;
  Crc: Cardinal;
begin
  Reader := TBlockReader.Create(S, False, StreamingBufSize, True);
  try
    Crc := System.ZLib.crc32(0, nil, 0);
    while Reader.ReadNext do
      Crc := System.ZLib.crc32(Crc, @Reader.Bytes[0], Reader.Count);
    Result := Hash32ToBytes(Crc);
  finally
    Reader.Free;
  end;
end;

class procedure THashUtils.CRC32.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.crc32(0, nil, 0);
end;

class procedure THashUtils.CRC32.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.crc32(cardinal((@Hash[0])^), @Buf, ByteBufSize);
end;

class function THashUtils.CRC32.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  Result := Hash32ToBytes(cardinal((@Hash[0])^));
  cardinal((@Hash[0])^) := Default(cardinal);
  SetLength(Hash, 0);
end;

{ THashUtils.Adler32 }

class function THashUtils.Adler32.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  Crc: Cardinal;
begin
  Crc := System.ZLib.adler32(0, nil, 0);
  Crc := System.ZLib.adler32(Crc, @Buf, ByteBufSize);
  Result := Hash32ToBytes(Crc);
end;

class function THashUtils.Adler32.DoEncode(S: TStream): TBytes;
var
  Reader: TBlockReader;
  Crc: Cardinal;
begin
  Reader := TBlockReader.Create(S, False, StreamingBufSize, True);
  try
    Crc := System.ZLib.adler32(0, nil, 0);
    while Reader.ReadNext do
      Crc := System.ZLib.adler32(Crc, @Reader.Bytes[0], Reader.Count);
    Result := Hash32ToBytes(Crc);
  finally
    Reader.Free;
  end;
end;

class procedure THashUtils.Adler32.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.adler32(0, nil, 0);
end;

class procedure THashUtils.Adler32.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  cardinal((@Hash[0])^) := System.ZLib.adler32(cardinal((@Hash[0])^), @Buf, ByteBufSize);
end;

class function THashUtils.Adler32.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(cardinal));
  Result := Hash32ToBytes(cardinal((@Hash[0])^));
  cardinal((@Hash[0])^) := Default(cardinal);
  SetLength(Hash, 0);
end;

{ THashUtils.BobJenkins32 }

class function THashUtils.BobJenkins32.DoEncode(const Buf; ByteBufSize: integer): TBytes;
var
  h: THashBobJenkins;
begin
  h := THashBobJenkins.Create;
  h.Update(Buf, ByteBufSize);
  Result := h.HashAsBytes;
end;

class function THashUtils.BobJenkins32.DoEncode(S: TStream): TBytes;
var
  Reader: TBlockReader;
  Hash: THashBobJenkins;
begin
  Reader := TBlockReader.Create(S, False, StreamingBufSize, True);
  try
    Hash := THashBobJenkins.Create;
    while Reader.ReadNext do
      Hash.Update(Reader.Bytes, Reader.Count);
    Result := Hash.HashAsBytes;
  finally
    Reader.Free;
  end;
end;

class procedure THashUtils.BobJenkins32.DoInit(out Hash: THashData);
begin
  SetLength(Hash, SizeOf(THashBobJenkins));
  THashBobJenkins((@Hash[0])^) := THashBobJenkins.Create;
end;

class procedure THashUtils.BobJenkins32.DoUpdate(const Buf; ByteBufSize: integer; var Hash: THashData);
begin
  Assert(Length(Hash)=SizeOf(THashBobJenkins));
  THashBobJenkins((@Hash[0])^).Update(Buf, ByteBufSize);
end;

class function THashUtils.BobJenkins32.DoDone(var Hash: THashData): TBytes;
begin
  Assert(Length(Hash)=SizeOf(THashBobJenkins));
  Result := THashBobJenkins((@Hash[0])^).HashAsBytes;
  THashBobJenkins((@Hash[0])^) := Default(THashBobJenkins);
  SetLength(Hash, 0);
end;

{ THashUtils }

class function THashUtils.Mix(const HashA, HashB, HashC: integer): integer;
begin
  result := Mix(Mix(HashA, HashB), HashC);
end;

class function THashUtils.Mix(const HashA, HashB: integer): integer;
begin
  result := (HashA*1103515245 + 12345) xor HashB;
end;

class function THashUtils.Mix(const HashA, HashB: TBytes): TBytes;
var
  V,L1,L2,LR: Integer;
  Src1,Src2,Dst: pointer;
begin
  L1 := Length(HashA);
  L2 := Length(HashB);
  LR := Max(L1, L2);
  SetLength(result, LR);
  if LR = 0 then
    Exit;
  Dst := @result[0];
  if L1>0 then Src1 := @HashA[0] else Src1 := nil;
  if L2>0 then Src2 := @HashB[0] else Src2 := nil;
  V := 0;

  while LR >= SizeOf(integer) do
  begin

    { read from HashA }
    if L1 >= SizeOf(integer) then
    begin
      V := (V*1103515245 + 12345) xor integer(Src1^);
      inc(PByte(Src1), SizeOf(integer));
      dec(L1, SizeOf(integer));
    end
    else
      while L1 > 0 do
      begin
        V := (V*1103515245 + 12345) xor PByte(Src1)^;
        inc(PByte(Src1));
        dec(L1);
      end;

    { read from HashB }
    if L2 >= SizeOf(integer) then
    begin
      V := (V*1103515245 + 12345) xor integer(Src2^);
      inc(PByte(Src2), SizeOf(integer));
      dec(L2, SizeOf(integer));
    end
    else
      while L2 > 0 do
      begin
        V := (V*1103515245 + 12345) xor PByte(Src2)^;
        inc(PByte(Src2));
        dec(L2);
      end;

    integer(Dst^) := V;
    dec(LR, SizeOf(integer));
  end;

  while LR > 0 do
  begin
    if L1 > 0 then
    begin
      V := (V*1103515245 + 12345) xor byte(Src1^);
      inc(PByte(Src1));
      dec(L1);
    end;
    if L2 > 0 then
    begin
      V := (V*1103515245 + 12345) xor byte(Src2^);
      inc(PByte(Src2));
      dec(L2);
    end;
    Byte(Dst^) := V;
    dec(LR);
  end;
end;

class function THashUtils.HashToString(const AHash: TBytes): string;
begin
  Result := THex.Encode(AHash);
end;

class function THashUtils.GetHash32(const Hash: TBytes): integer;
begin
  if Length(Hash) = 0 then
    result := 0
  else
  if Length(Hash) = 4 then
    result :=
      (integer(Hash[0]) shl 24) or
      (integer(Hash[1]) shl 16) or
      (integer(Hash[2]) shl  8) or
      (integer(Hash[3]))
  else
    result := GetHash32(CRC32.Encode(Hash,0,Length(Hash)));
end;

class function THashUtils.GetHash24(const Hash: TBytes): integer;
begin
  if Length(Hash) = 0 then
    result := 0
  else
  if Length(Hash) = 4 then
    result :=
      (integer(Hash[0]) shl 16) or
      (integer(Hash[1]) shl  8) or
      (integer(Hash[2]) xor integer(Hash[3]))
  else
    result := GetHash24(CRC32.Encode(Hash,0,Length(Hash)));
end;

class function THashUtils.GetHash16(const Hash: TBytes): integer;
begin
  if Length(Hash) = 0 then
    result := 0
  else
  if Length(Hash) = 4 then
    result :=
      ((integer(Hash[0]) xor integer(Hash[1])) shl 8) or
      (integer(Hash[2]) xor integer(Hash[3]))
  else
    result := GetHash16(CRC32.Encode(Hash,0,Length(Hash)));
end;

class function THashUtils.Mix(const Hashes: array of integer): integer;
var
  I: Integer;
begin
  result := 0;
  for I := Low(Hashes) to High(Hashes) do
    result := Mix(result, Hashes[I]);
end;

class function THashUtils.Mix(const Hashes: array of TBytes): TBytes;
var
  I: Integer;
begin
  SetLength(result, 0);
  for I := Low(Hashes) to High(Hashes) do
    result := Mix(result, Hashes[I]);
end;

end.
