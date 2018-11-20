unit adot.Hash;

{
  TDigest = record
    Simple API for block / chain hashing
    Supported hashes:
      MD5, SHA1, SHA2, BobJenkins32 (System.Hash)
      CRC32, Adler32 (System.ZLib)

  TDigests = record
    Simple API for single data block hashing
    Hashing helpers (mix function etc)
}

interface

uses
  System.ZLib,
  System.Hash,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  System.Generics.Defaults;

type
  TCustomDigest = class abstract(TInterfacedObject, IInterface)
  protected
    type
      { Stream block reader (ReadNext to get next block of data from stream as array of byte) }
      TBlockReader = class
      private
        const
          StreamingBufSize = 64*1024;
        var
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

    { block/chain functions }
    procedure DoUpdate(const Data; SizeOFData: integer); virtual; abstract;
    function DoDone: TBytes; virtual; abstract;

    class procedure CreateDigest(out ADigest: TCustomDigest; out ADigestInt: IInterface);

    { Low level functions. They calculate digest from data only }
    procedure Update(const AData; ADataSize: integer); overload;
    procedure Update(const AData: TStream); overload;
    procedure Update(const AData: TBytes; AStartIndex,ACount: integer); overload;
    procedure Update(const AData: TBytes); overload;
    procedure Update(const AData: string); overload;
    procedure Update(const AData: string; AStartIndex,ACount: integer); overload;
    procedure Update(const AData: TArray<string>); overload;
    procedure Update(const AData: TEnumerable<string>); overload;
    procedure Update(const AData: TObject); overload;
    procedure Update<T: record>(const AData: T); overload;
    procedure UpdateFromFile(const AFileName: string); overload;

  public
    constructor Create; virtual; abstract;

    { High level functions. They calculate digest from size and data, not from data only }
    class function GetHash(const AData; ADataSize: integer): TBytes; overload;
    class function GetHash(const AData: TStream): TBytes; overload;
    class function GetHash(const AData: TBytes; AStartIndex,ACount: integer): TBytes; overload;
    class function GetHash(const AData: TBytes): TBytes; overload;
    class function GetHash(const AData: string): TBytes; overload;
    class function GetHash(const AData: string; AStartIndex,ACount: integer): TBytes; overload;
    class function GetHash(const AData: TObject): TBytes; overload;
    class function GetHash<T: record>(const AData: T): TBytes; overload;
    class function GetHashOfFile(const AFileName: string): TBytes; overload;

    { For containers we calculate digest from size and data, not from data only.
      It guarantees that
        GetHash(["A", "BC"]) <>
        GetHash(["AB", "C"]) }
    class function GetHash(const AData: TArray<string>): TBytes; overload;
    class function GetHash(const AData: TEnumerable<string>): TBytes; overload;
  end;
  TDigestClass = class of TCustomDigest;

  {
    function GetHash(L: TList<TRec>): string;
    var
      D: TDigest;
      I: Integer;
    begin
      D.InitStrong;
      for I := 0 to List.Count-1 do
        D.Update(L[I]);
      result := D.ToString;
    end;
  }
  TDigest = record
  private
    type
      TMD5 = class(TCustomDigest)
      protected
        FData: THashMD5;

        procedure DoUpdate(const Data; SizeOFData: integer); override;
        function DoDone: TBytes; override;
      public
        constructor Create; override;
      end;

      TSHA1 = class(TCustomDigest)
      protected
        FData: THashSHA1;

        procedure DoUpdate(const Data; SizeOFData: integer); override;
        function DoDone: TBytes; override;
      public
        constructor Create; override;
      end;

      TSHA2 = class(TCustomDigest)
      protected
        FData: THashSHA2;

        procedure DoUpdate(const Data; SizeOFData: integer); override;
        function DoDone: TBytes; override;
      public
        constructor Create; overload; override;
        constructor Create(const Ver: THashSHA2.TSHA2Version); reintroduce; overload;
      end;

      TCRC32 = class(TCustomDigest)
      protected
        FData: cardinal;

        procedure DoUpdate(const Data; SizeOFData: integer); override;
        function DoDone: TBytes; override;
      public
        constructor Create; override;
      end;

      TAdler32 = class(TCustomDigest)
      protected
        FData: cardinal;

        procedure DoUpdate(const Data; SizeOFData: integer); override;
        function DoDone: TBytes; override;
      public
        constructor Create; override;
      end;

      TBobJenkins32 = class(TCustomDigest)
      protected
        FData: THashBobJenkins;

        procedure DoUpdate(const Data; SizeOFData: integer); override;
        function DoDone: TBytes; override;
      public
        constructor Create; override;
      end;

    var
      FDigest: TCustomDigest;
      FDigestInt: IInterface;

    procedure SetDigest(const Value: TCustomDigest);

    property Digest: TCustomDigest read FDigest write SetDigest;

  public

    { Block / chain functions }
    procedure InitMD5;
    procedure InitSHA1;
    procedure InitSHA2; overload;
    procedure InitSHA2(const Ver: THashSHA2.TSHA2Version); overload;
    procedure InitCRC32;
    procedure InitAdler32;
    procedure InitBobJenkins32;

    procedure InitStrong;
    procedure InitFast;
    procedure InitFastest;

    procedure Update(const AData; ADataSize: integer); overload;
    procedure Update(const AData: TStream); overload;
    procedure Update(const AData: TBytes; AStartIndex,ACount: integer); overload;
    procedure Update(const AData: TBytes); overload;
    procedure Update(const AData: string); overload;
    procedure Update(const AData: string; AStartIndex,ACount: integer); overload;
    procedure Update(const AData: TArray<string>); overload;
    procedure Update(const AData: TEnumerable<string>); overload;
    procedure Update(const AData: TObject); overload;
    procedure Update<T: record>(const AData: T); overload;
    procedure UpdateFromFile(const AFileName: string); overload;

    function Done: TBytes;
    function ToString: string;
    function ToBytes: TBytes;
  end;

  {
    var
      A,B,R: TBytes;
      V: TArray<string>;
    begin
      V := ['1', '2', '3'];
      A := TDigests.MD5.GetHashOfFile('c:\1.xml');
      B := TDigests.SHA2.GetHash(V);
      R := TDigests.Mix(A,B);
    end;
  }
  TDigests = record
  private
    class function GetAdler32Class      : TDigestClass; static;
    class function GetBobJenkins32Class : TDigestClass; static;
    class function GetCRC32Class        : TDigestClass; static;
    class function GetMD5Class          : TDigestClass; static;
    class function GetSHA1Class         : TDigestClass; static;
    class function GetSHA2Class         : TDigestClass; static;

  public
    class property MD5          : TDigestClass read GetMD5Class;
    class property SHA1         : TDigestClass read GetSHA1Class;
    class property SHA2         : TDigestClass read GetSHA2Class;
    class property CRC32        : TDigestClass read GetCRC32Class;
    class property Adler32      : TDigestClass read GetAdler32Class;
    class property BobJenkins32 : TDigestClass read GetBobJenkins32Class;

    class function Mix(const HashA,HashB: integer): integer; overload; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function Mix(const HashA,HashB,HashC: integer): integer; overload; static;
    class function Mix(const HashA,HashB: TBytes): TBytes; overload; static;
    class function Mix(const Hashes: array of integer): integer; overload; static;
    class function Mix(const Hashes: array of TBytes): TBytes; overload; static;

    class function ToString(const AHash: TBytes): string; static;

    class function GetHash32(const Hash: TBytes): integer; static;
    class function GetHash24(const Hash: TBytes): integer; static;
    class function GetHash16(const Hash: TBytes): integer; static;
  end;

implementation

uses
  adot.Tools;

{ TDigest.TMD5 }

constructor TDigest.TMD5.Create;
begin
  FData := THashMD5.Create;
end;

procedure TDigest.TMD5.DoUpdate(const Data; SizeOFData: integer);
begin
  inherited;
  FData.Update(Data, SizeOFData);
end;

function TDigest.TMD5.DoDone: TBytes;
begin
  result := FData.HashAsBytes;
end;

{ TDigest }

function TDigest.Done: TBytes;
begin
  result := Digest.DoDone;
end;

procedure TDigest.InitAdler32;
begin
  Digest := TAdler32.Create;
end;

procedure TDigest.InitBobJenkins32;
begin
  Digest := TBobJenkins32.Create;
end;

procedure TDigest.InitCRC32;
begin
  Digest := TCRC32.Create;
end;

procedure TDigest.InitMD5;
begin
  Digest := TMD5.Create;
end;

procedure TDigest.InitSHA1;
begin
  Digest := TSHA1.Create;
end;

procedure TDigest.InitSHA2;
begin
  Digest := TSHA2.Create(THashSHA2.TSHA2Version.SHA256);
end;

procedure TDigest.InitSHA2(const Ver: THashSHA2.TSHA2Version);
begin
  Digest := TSHA2.Create(Ver);
end;

procedure TDigest.InitStrong;
begin
  InitMD5;
end;

procedure TDigest.InitFast;
begin
  InitBobJenkins32;
end;

procedure TDigest.InitFastest;
begin
  InitAdler32;
end;

procedure TDigest.SetDigest(const Value: TCustomDigest);
begin
  if FDigest = Value then
    Exit;
  FDigest := nil;
  FDigestInt := nil;
  FDigestInt := Value;
  FDigest := Value;
end;

function TDigest.ToBytes: TBytes;
begin
  result := Digest.DoDone;
end;

function TDigest.ToString: string;
begin
  result := TDigests.ToString(Digest.DoDone);
end;

procedure TDigest.Update(const AData: TBytes);
begin
  Digest.Update(AData);
end;

procedure TDigest.Update(const AData: TBytes; AStartIndex, ACount: integer);
begin
  Digest.Update(AData, AStartIndex, ACount);
end;

procedure TDigest.Update(const AData: TStream);
begin
  Digest.Update(AData);
end;

procedure TDigest.Update(const AData; ADataSize: integer);
begin
  Digest.Update(AData, ADataSize);
end;

procedure TDigest.Update(const AData: string);
begin
  Digest.Update(AData);
end;

procedure TDigest.Update(const AData: string; AStartIndex,ACount: integer);
begin
  Digest.Update(AData,AStartIndex,ACount);
end;

procedure TDigest.Update(const AData: TObject);
begin
  Digest.Update(AData);
end;

procedure TDigest.Update(const AData: TEnumerable<string>);
begin
  Digest.Update(AData);
end;

procedure TDigest.Update(const AData: TArray<string>);
begin
  Digest.Update(AData);
end;

procedure TDigest.Update<T>(const AData: T);
begin
  Digest.Update(AData);
end;

procedure TDigest.UpdateFromFile(const AFileName: string);
begin
  Digest.UpdateFromFile(AFileName);
end;

{ TDigest.TSHA1 }

constructor TDigest.TSHA1.Create;
begin
  FData := THashSHA1.Create;
end;

procedure TDigest.TSHA1.DoUpdate(const Data; SizeOFData: integer);
begin
  inherited;
  FData.Update(Data, SizeOFData);
end;

function TDigest.TSHA1.DoDone: TBytes;
begin
  result := FData.HashAsBytes;
end;

{ TDigest.TSHA2 }

constructor TDigest.TSHA2.Create(const Ver: THashSHA2.TSHA2Version);
begin
  FData := THashSHA2.Create(Ver);
end;

constructor TDigest.TSHA2.Create;
begin
  FData := THashSHA2.Create(THashSHA2.TSHA2Version.SHA256);
end;

procedure TDigest.TSHA2.DoUpdate(const Data; SizeOFData: integer);
begin
  inherited;
  FData.Update(Data, SizeOFData);
end;

function TDigest.TSHA2.DoDone: TBytes;
begin
  result := FData.HashAsBytes;
end;

{ TDigest.TCRC32 }

constructor TDigest.TCRC32.Create;
begin
  FData := System.ZLib.crc32(0, nil, 0);
end;

procedure TDigest.TCRC32.DoUpdate(const Data; SizeOFData: integer);
begin
  inherited;
  FData := System.ZLib.crc32(FData, @Data, SizeOFData);
end;

function TDigest.TCRC32.DoDone: TBytes;
begin
  SetLength(Result, 4);
  PCardinal(@Result[0])^ := System.Hash.THash.ToBigEndian(FData);
end;

{ TDigest.TAdler32 }

constructor TDigest.TAdler32.Create;
begin
  FData := System.ZLib.adler32(0, nil, 0);
end;

procedure TDigest.TAdler32.DoUpdate(const Data; SizeOFData: integer);
begin
  inherited;
  FData := System.ZLib.adler32(FData, @Data, SizeOFData);
end;

function TDigest.TAdler32.DoDone: TBytes;
begin
  SetLength(Result, 4);
  PCardinal(@Result[0])^ := System.Hash.THash.ToBigEndian(FData);
end;

{ TDigest.TBobJenkins32 }

constructor TDigest.TBobJenkins32.Create;
begin
  FData := THashBobJenkins.Create;
end;

procedure TDigest.TBobJenkins32.DoUpdate(const Data; SizeOFData: integer);
begin
  inherited;
  FData.Update(Data, SizeOFData);
end;

function TDigest.TBobJenkins32.DoDone: TBytes;
begin
  result := FData.HashAsBytes;
end;

{ TCustomDigest }

class procedure TCustomDigest.CreateDigest(out ADigest: TCustomDigest; out ADigestInt: IInterface);
begin
  ADigest := Self.Create;
  ADigestInt := ADigest;
end;

class function TCustomDigest.GetHash(const AData: TBytes): TBytes;
begin
  result := GetHash(AData, 0, Length(AData));
end;

class function TCustomDigest.GetHash(const AData: TBytes; AStartIndex,ACount: integer): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
begin
  CreateDigest(Digest, DigestInt);
  Digest.Update(AData, AStartIndex, ACount);
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHash(const AData: TStream): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
begin
  CreateDigest(Digest, DigestInt);
  Digest.Update(AData);
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHash(const AData; ADataSize: integer): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
begin
  CreateDigest(Digest, DigestInt);
  Digest.Update(AData, ADataSize);
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHash(const AData: string): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
begin
  CreateDigest(Digest, DigestInt);
  Digest.Update(AData);
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHash(const AData: string; AStartIndex,ACount: integer): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
begin
  CreateDigest(Digest, DigestInt);
  Digest.Update(AData,AStartIndex,ACount);
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHash(const AData: TEnumerable<string>): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
  S: string;
begin
  CreateDigest(Digest, DigestInt);
  for S in AData do
  begin
    Digest.Update(Length(S));
    Digest.Update(S);
  end;
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHash(const AData: TArray<string>): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
  S: string;
begin
  CreateDigest(Digest, DigestInt);
  for S in AData do
  begin
    Digest.Update(Length(S));
    Digest.Update(S);
  end;
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHashOfFile(const AFileName: string): TBytes;
var
  Hasher: TCustomDigest;
  HasherInt: IInterface;
begin
  CreateDigest(Hasher, HasherInt);
  Hasher.UpdateFromFile(AFileName);
  Result := Hasher.DoDone;
end;

procedure TCustomDigest.Update(const AData: string);
begin
  Update(TEncoding.UTF8.GetBytes(AData));
end;

procedure TCustomDigest.Update(const AData: string; AStartIndex,ACount: integer);
begin
  Update(TEncoding.UTF8.GetBytes(AData.Substring(AStartIndex, ACount)));
end;

procedure TCustomDigest.Update(const AData: TBytes);
begin
  if Length(AData) > 0
    then DoUpdate(AData[0], Length(AData))
    else DoUpdate(nil^, 0);
end;

procedure TCustomDigest.Update(const AData: TBytes; AStartIndex, ACount: integer);
begin
  if ACount > 0
    then DoUpdate(AData[AStartIndex], ACount)
    else DoUpdate(nil^, 0);
end;

procedure TCustomDigest.Update(const AData: TStream);
var
  Reader: TBlockReader;
begin
  Reader := TBlockReader.Create(AData, False, TBlockReader.StreamingBufSize, True);
  try
    while Reader.ReadNext do
      DoUpdate(Reader.Bytes[0], Reader.Count);
  finally
    Reader.Free;
  end;
end;

procedure TCustomDigest.Update(const AData: TObject);
var
  Code: Integer;
begin
  Code := AData.GetHashCode;
  DoUpdate(Code, SizeOf(Code));
end;

procedure TCustomDigest.Update(const AData; ADataSize: integer);
begin
  if ADataSize > 0
    then DoUpdate(AData, ADataSize)
    else DoUpdate(nil^, 0);
end;

procedure TCustomDigest.Update(const AData: TEnumerable<string>);
var
  S: string;
begin
  for S in AData do
    Update(S);
end;

procedure TCustomDigest.Update(const AData: TArray<string>);
var
  S: string;
begin
  for S in AData do
    Update(S);
end;

procedure TCustomDigest.Update<T>(const AData: T);
begin
  DoUpdate(AData, SizeOf(AData));
end;

procedure TCustomDigest.UpdateFromFile(const AFileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    Update(FileStream);
  finally
    FileStream.Free;
  end;
end;

class function TCustomDigest.GetHash(const AData: TObject): TBytes;
var
  Digest: TCustomDigest;
  DigestInt: IInterface;
begin
  CreateDigest(Digest, DigestInt);
  Digest.Update(AData);
  Result := Digest.DoDone;
end;

class function TCustomDigest.GetHash<T>(const AData: T): TBytes;
var
  Hasher: TCustomDigest;
  HasherInt: IInterface;
begin
  CreateDigest(Hasher, HasherInt);
  Hasher.Update(AData);
  Result := Hasher.DoDone;
end;

{ TDigests }

class function TDigests.GetAdler32Class: TDigestClass;
begin
  result := TDigest.TAdler32;
end;

class function TDigests.GetBobJenkins32Class: TDigestClass;
begin
  result := TDigest.TBobJenkins32;
end;

class function TDigests.GetCRC32Class: TDigestClass;
begin
  result := TDigest.TCRC32;
end;

class function TDigests.GetMD5Class: TDigestClass;
begin
  result := TDigest.TMD5;
end;

class function TDigests.GetSHA1Class: TDigestClass;
begin
  result := TDigest.TSHA1;
end;

class function TDigests.GetSHA2Class: TDigestClass;
begin
  result := TDigest.TSHA2;
end;

class function TDigests.GetHash16(const Hash: TBytes): integer;
begin
  if Length(Hash) = 0 then
    result := 0
  else
  if Length(Hash) = 4 then
    result :=
      ((integer(Hash[0]) xor integer(Hash[1])) shl 8) or
      (integer(Hash[2]) xor integer(Hash[3]))
  else
    result := GetHash16(CRC32.GetHash(Hash,0,Length(Hash)));
end;

class function TDigests.GetHash24(const Hash: TBytes): integer;
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
    result := GetHash24(CRC32.GetHash(Hash,0,Length(Hash)));
end;

class function TDigests.GetHash32(const Hash: TBytes): integer;
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
    result := GetHash32(CRC32.GetHash(Hash,0,Length(Hash)));
end;

class function TDigests.ToString(const AHash: TBytes): string;
begin
  Result := THex.Encode(AHash).ToLower;
end;

class function TDigests.Mix(const HashA, HashB: integer): integer;
begin
  result := (HashA*1103515245 + 12345) xor HashB;
end;

class function TDigests.Mix(const HashA, HashB, HashC: integer): integer;
begin
  result := Mix(Mix(HashA, HashB), HashC);
end;

class function TDigests.Mix(const HashA, HashB: TBytes): TBytes;
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

class function TDigests.Mix(const Hashes: array of integer): integer;
var
  I: Integer;
begin
  result := 0;
  for I := Low(Hashes) to High(Hashes) do
    result := Mix(result, Hashes[I]);
end;

class function TDigests.Mix(const Hashes: array of TBytes): TBytes;
var
  I: Integer;
begin
  SetLength(result, 0);
  for I := Low(Hashes) to High(Hashes) do
    result := Mix(result, Hashes[I]);
end;

{ TCustomDigest.TBlockReader }

constructor TCustomDigest.TBlockReader.Create(Src: TStream; AOwnsStream: Boolean; BufSize: integer; FromBeginning: boolean);
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

destructor TCustomDigest.TBlockReader.Destroy;
begin
  if OwnsStream then
    Stream.Free;
  Stream := nil;
  inherited;
  inherited;
end;

function TCustomDigest.TBlockReader.ReadNext: Boolean;
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

end.
