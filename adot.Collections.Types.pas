unit adot.Collections.Types;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Classes,
  System.SysUtils,
  System.StrUtils;

type
  TEnumerableExt<T> = class(TEnumerable<T>)
  protected
    class procedure DoSaveToStream(Src: TEnumerable<T>; Dst: TStream; Encoding: TEncoding = nil); static;
    class procedure DoSaveToFile(Src: TEnumerable<T>; const FileName: string; Encoding: TEncoding = nil; MemStream: boolean = True); static;

  public
    procedure SaveToStream(Dst: TStream; Encoding: TEncoding = nil);
    procedure SaveToFile(const FileName: string; Encoding: TEncoding = nil; MemStream: boolean = True);
    function ToString: string; override;
  end;

implementation

uses
  adot.Tools,
  adot.Tools.RTTI;

{ TEnumerableExt<T> }

class procedure TEnumerableExt<T>.DoSaveToFile(Src: TEnumerable<T>; const FileName: string; Encoding: TEncoding; MemStream: boolean);
var
  S: TStream;
begin
  if MemStream
    then S := TMemoryStream.Create
    else S := TFileStream.Create(FileName, System.Classes.fmCreate);
  try
    DoSaveToStream(Src, S, Encoding);
    if MemStream then
      TMemoryStream(S).SaveToFile(FileName);
  finally
    Sys.FreeAndNil(S);
  end;
end;

class procedure TEnumerableExt<T>.DoSaveToStream(Src: TEnumerable<T>; Dst: TStream; Encoding: TEncoding);
var
  I: Integer;
  S: string;
  B: TArray<byte>;
  V: T;
  N: boolean;
begin
  if Encoding = nil then
    Encoding := TEncoding.UTF8;
  B := Encoding.GetPreamble;
  Dst.WriteBuffer(B, System.Length(B));
  N := False;
  for V in Src do
  begin
    S := IfThen(N,#13#10,'') + TRttiUtils.ValueAsString<T>(V);
    B := Encoding.GetBytes(S);
    Dst.WriteBuffer(B, System.Length(B));
    N := True;
  end;
end;

procedure TEnumerableExt<T>.SaveToFile(const FileName: string; Encoding: TEncoding; MemStream: boolean);
begin
  DoSaveToFile(Self, Filename, Encoding, MemStream);
end;

procedure TEnumerableExt<T>.SaveToStream(Dst: TStream; Encoding: TEncoding);
begin
  DoSaveTostream(Self, dst, Encoding);
end;

function TEnumerableExt<T>.ToString: string;
var
  Builder: TStringBuilder;
  V: T;
  N: Boolean;
begin
  Builder := TStringBuilder.Create;
  try
    N := False;
    for V in Self do
    begin
      if N then Builder.Append(#13#10) else N := True;
      Builder.Append(TRttiUtils.ValueAsString<T>(V));
    end;
    result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

end.
