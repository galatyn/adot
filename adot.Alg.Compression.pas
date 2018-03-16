unit adot.Alg.Compression;

{
  TRLE = class
    PDF-compatible RLE codec.

}

interface

uses
  System.SysUtils;

type
  { PDF-compatible RLE codec }
  TRLE = class
  public
    class function MaxEncodedSize(ASrcSize: cardinal): cardinal;
    class function Encode(const ASrc; ASrcSize: cardinal; var ADest): cardinal;

    class function DecodedSize(const ASrc; APackedSize: cardinal): cardinal;
    class procedure Decode(const ASrc; APackedSize: cardinal; var ADest; AUnpackedSize: cardinal);
  end;

implementation

{ TRLE }

class function TRLE.MaxEncodedSize(ASrcSize: cardinal): cardinal;
begin
  result := ASrcSize + ASrcSize div 64 + 16;
end;

(*
  RunLengthEncoding according to PDF specification. Stream of bytes encoded
  as stream of packets. The packet contains ocntrol byte (length) and
  optional string of bytes. Allowed values for control byte:
    0..127: next N+1 bytes (1..128) should be writtens as is
       128: EOF (decoding finished)
  129..255: next byte should be repeated 257-N times (2..128)

  At every step we have current state described in following variables:
  S[1.2...N][1.2..M]
  N-number of uncompressable bytes
  M-number of items in sequance of equal (potentially compressable)
*)
class function TRLE.Encode(const ASrc; ASrcSize: cardinal; var ADest): cardinal;
var
  Src, Dst: PByte;
  M,N: cardinal;
  PrevChar: byte;

  { 0 1 [2 3 4] [5 6] (7)
           N=3    M=2 CurPos }
  procedure COPY_N;
  begin
    assert((N>=1) and (N<=128));
    inc(result,N+1);
    Dst^ := N-1; { copy N (1..128) bytes as is }
    inc(Dst);
    system.move((Src-M-N)^, Dst^, N);
    inc(Dst, N);
  end;

  { M=0! }
  procedure COPY_128;
  begin
    assert(N>=128);
    inc(result,129);
    Dst^ := 127; { copy N (1..128) bytes as is }
    inc(Dst);
    system.move((Src-N)^, Dst^, 128);
    inc(Dst, 128);
    dec(N, 128);
  end;

  { 257-x=M -> x=257-M }
  procedure PACK_M;
  begin
    assert((M>=2) and (M<=128));
    inc(result,2);
    dst^ := 257-M; { copy folowing byte 257-x times }
    inc(dst);
    dst^ := PrevChar;
    inc(dst);
  end;

  function Init:longword;
  begin
    //ADest := AllocMem(ASrcSize + ASrcSize div 64 + 16);
    Src := @ASrc;
    Dst := @ADest;
    result := 0;

    // too short chain
    if ASrcSize<3 then
    begin
      result := ASrcSize;
      if result>0 then
      begin
        inc(result);              // + 1 byte command
        dst^ := ASrcSize-1;   // command (copy ASourceCount bytes)
        inc(dst);
        system.move(Src^, Dst^, ASrcSize);
        inc(dst, ASrcSize);
      end;

      // end of data
      inc(result);
      dst^ := 128;
      inc(dst);
      exit;
    end;
  end;

  procedure Finish;
  begin

    // short repeated chain we join to "as is" chain (M must be greater than 1)
    if M<2 then
    begin
      inc(N, M);
      if N>128 then
        COPY_128;
      M := 0;
    end;

    // out "as is" chain
    if N>0 then
    begin
      COPY_N;
      N := 0;
    end;

    // out "repeated" chain
    if M>0 then
    begin
      PACK_M;
      M := 0;
    end;

    // out "end of data" marker
    inc(result);
    dst^ := 128;
    inc(dst);
  end;

begin

  // initialize
  result := Init;
  if result>0 then
    exit;

  // read src
  N := 0;
  M := 0;
  repeat
    if M=0 then
    begin
      PrevChar := Src^;
      inc(M);
    end
    else
    begin
      if Src^=PrevChar then
        inc(M);

      // two folowing conditions never met at same time
      if (Src^<>PrevChar) or (M=128) then
        if N>0 then
          if M>=4 then
            if M=128 then
            begin // N>0, M=128, Src^=PrevChar
              inc(Src);
              COPY_N;
              PACK_M;
              dec(Src);
              N := 0;
              M := 0;
            end
            else
            begin // N>0, M=[4..127], Src^<>PrevChar
              COPY_N;
              PACK_M;
              N := 0;
              M := 1;
              PrevChar := Src^;
            end
          else
          begin // N>0, M<4, Src^<>PrevChar
            inc(N,M);
            if N>128 then
              COPY_128;
            M := 1;
            PrevChar := Src^;
          end
        else
          if M>=3 then
          begin
            if M=128 then
            begin // N=0, M=128, Src^=PrevChar
              inc(Src);
              PACK_M;
              dec(Src);
              M := 0;
            end
            else
            begin // N=0, M=[3..127], Src^<>PrevChar
              PACK_M;
              M := 1;
              PrevChar := Src^;
            end
          end
          else
          begin // N=0, M=[1..2], Src^<>PrevChar
            N := M;
            M := 1;
            PrevChar := Src^;
          end
    end;
    dec(ASrcSize);
    inc(Src);
  until ASrcSize=0;

  // finish uncompleted chain
  finish;
//  assert(result<(ASrcSize + ASrcSize div 64 + 16));
end;

class function TRLE.DecodedSize(const ASrc; APackedSize: cardinal): cardinal;
var
  src: PByte;
  n: byte;
begin
  Src := @ASrc;
  result := 0;
  while APackedSize>0 do
  begin
    n := Src^;
    inc(Src);
    dec(APackedSize);

    // copy
    if n<128 then
    begin
      inc(n);
      if APackedSize<n then
        raise exception.create('RLDecode error');
      inc(result, n);
      inc(Src, n);
      dec(APackedSize, n);
      continue;
    end;

    // stop
    if n=128 then
      break;

    // repeat
    n := 257-n;
    if APackedSize<1 then
      raise exception.create('RLDecode error');
    inc(result, n);
    inc(Src);
    dec(APackedSize);
  end;
end;

class procedure TRLE.Decode(const ASrc; APackedSize: cardinal; var ADest; AUnpackedSize: cardinal);
var
  src,dst: PByte;
  n: byte;
begin
  Src := @ASrc;
  Dst := @ADest;
  while APackedSize>0 do
  begin
    n := Src^;
    inc(Src);
    dec(APackedSize);

    // copy
    if n<128 then
    begin
      inc(n);
      if (APackedSize<n) or (AUnpackedSize<n) then
        raise exception.create('RLDecode error');
      system.move(Src^, dst^, n);
      dec(AUnpackedSize, n);
      inc(Src, n);
      dec(APackedSize, n);
      inc(dst, n);
      continue;
    end;

    // stop
    if n=128 then
      break;

    // repeat
    n := 257-n;
    if (APackedSize<1) or (AUnpackedSize<1) then
      raise exception.create('RLDecode error');
    FillChar(dst^, n, Src^);
    dec(AUnpackedSize, n);
    inc(Src);
    dec(APackedSize);
    inc(dst, n);
  end;
  if (AUnpackedSize<>0) then
    raise exception.create('RLDecode error');
end;

end.
