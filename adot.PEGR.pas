unit adot.PEGR;

interface

uses
  {$IFDEF PEGLOG}
  adot.Log, adot.VCL.Log,
  {$ENDIF}
  adot.PEG, adot.Tools, System.Types, System.SysUtils,
  System.Classes, System.Math, System.StrUtils;

type

  PExpr = ^TExpr;
  // **************************************************************************
  // Record type with overloaded operations to provide simple and clear
  // way to construct and (what is even more important) debug PEG-based
  // parsers directly in Delphi IDE.
  // **************************************************************************
  TExprCommand = (ecString, ecBytes, ecCharSet, ecByteSet, ecLink, ecAND, ecOR, ecREP);
  TExpr = record
    Command: TExprCommand;
    ValueStr: string;
    ValueStrCI: boolean;
    ValueBytes: TByteDynArray;
    ValueAnsiCharSet: array of adot.PEG.TAnsiCharSet; // array to keep TExpr record small
    ValueByteSet: array of adot.PEG.TByteSet; // we use array to keep TExpr record small
    ValueOperands: array of TExpr;
    ValueLink: PExpr;
    ValueRepMin: integer;
    ValueRepMax: integer;
    Initialized: string;
    PEG: TAutoFree<TPEGCustom>;
    FName: string;

    procedure SetName(const AName: string);

    // terminals
    class operator Implicit(const a: string): TExpr;
    class operator Implicit(const a: array of byte): TExpr;
    class operator Implicit(const a: TAnsiCharSet): TExpr;
    class operator Implicit(const a: TByteSet): TExpr;
    class operator Implicit(a: PExpr): TExpr; // link to another expression

    // "AND" for sequence, "OR" for choice
    class operator LogicalAnd(const a,b: TExpr): TExpr;
    class operator LogicalOr(const a,b: TExpr): TExpr;

    // repeaters
    class function Rep(const a: TExpr; MinRep,MaxRep: integer): TExpr; overload; static;
    class function Rep0(const a: TExpr): TExpr; static;
    class function Rep1(const a: TExpr): TExpr; static;
    class function Optional(const a: TExpr): TExpr; overload; static;

    // Build object model with interface to run parser.
    // Should be called only when all subexpressions are defined.
    function BuildPEG(AParser: TPEGParser): TPEGCustom;

    property Name: string read FName write SetName;
  end;
  E = TExpr;
  L = record
    Value: PExpr;

    class operator Implicit(var a: TExpr): L; // link to another expression
    class operator Implicit(const ALink: L): TExpr; // link to another expression
  end;


function ParseAndLog(const AExpr: TExpr; const AText: string): Boolean;
function Verify(const AExpr: TExpr; const AText: string; ACaseInsensitive: Boolean = True): Boolean; overload;
function Verify(const AExpr: TPEGCustom; const AText: string; ACaseInsensitive: Boolean = True): Boolean; overload;

implementation

{ TExpr }

{
  We can't build TPEG* classes when building TExpr expressions, because some of rules
  can be defined later, for example:
  A := E('test') and E(B);    // B is not defined here yet!
  B := E('the best');         // Only when we finish definitions of all rules, we can build object model.
}
class operator TExpr.Implicit(const a: string): TExpr;
begin
  result.Command := ecString;
  result.ValueStr := a;
  result.ValueStrCI := True;
  result.Initialized := '1';
end;

class operator TExpr.Implicit(const a: array of byte): TExpr;
var
  i: Integer;
begin
  result.Command := ecBytes;
  setlength(result.ValueBytes, length(a));
  for i := Low(a) to High(a) do
    result.ValueBytes[i] := a[i];
  result.Initialized := '1';
end;

class operator TExpr.Implicit(const a: TAnsiCharSet): TExpr;
begin
  result.Command := ecCharSet;
  SetLength(result.ValueAnsiCharSet, 1);
  result.ValueAnsiCharSet[0] := a;
  result.Initialized := '1';
end;

class operator TExpr.Implicit(const a: TByteSet): TExpr;
begin
  result.Command := ecByteSet;
  SetLength(result.ValueByteSet, 1);
  result.ValueByteSet[0] := a;
  result.Initialized := '1';
end;

procedure TExpr.SetName(const AName: string);
begin
  FName := AName;
  if PEG.Value<>nil then
    PEG.Value.Name := AName;
end;

class operator TExpr.Implicit(a: PExpr): TExpr;
begin
  result.Command := ecLink;
  result.ValueLink := a;
  result.Initialized := '1';
  result.Name := a.Name;
end;

class operator TExpr.LogicalAnd(const a, b: TExpr): TExpr;
begin
  // 1. A is temporary value      : rule := E('a') and <...>
  // 2. A is link to another rule : rule := E(a) and <...>
  // 3. A is another rule         : IMPOSSIBLE!
  SetLength(result.ValueOperands, 2);
  result.ValueOperands[0] := A;
  result.ValueOperands[1] := B;
  result.Command := ecAND;
  result.Initialized := '1';
end;

class operator TExpr.LogicalOr(const a, b: TExpr): TExpr;
begin
  SetLength(result.ValueOperands, 2);
  result.ValueOperands[0] := A;
  result.ValueOperands[1] := B;
  result.Command := ecOR;
  result.Initialized := '1';
end;

class function TExpr.Rep(const a: TExpr; MinRep, MaxRep: integer): TExpr;
begin
  SetLength(result.ValueOperands, 1);
  result.ValueOperands[0] := A;
  result.ValueRepMin := MinRep;
  result.ValueRepMax := MaxRep;
  result.Command := ecREP;
  result.Initialized := '1';
end;

class function TExpr.Rep1(const a: TExpr): TExpr;
begin
  result := Rep(a, 1, High(integer));
end;

class function TExpr.Rep0(const a: TExpr): TExpr;
begin
  result := Rep(a, 0, High(integer));
end;

class function TExpr.Optional(const a: TExpr): TExpr;
begin
  result := Rep(a, 0, 1);
end;

function TExpr.BuildPEG(AParser: TPEGParser): TPEGCustom;
begin
  if PEG.Value<>nil then
  begin
    result := PEG.Value;
    Exit;
  end;

  if Initialized='' then
    raise Exception.Create('Expression is not defined!');

  if PEG.Value=nil then
  case Command of
    ecString:
      if AParser.DataSourceFormat=sfAnsiString then
        PEG.Value := TPEGAnsiString.Create(AnsiString(ValueStr), AParser.CaseInsensitive)
      else
        PEG.Value := TPEGString.Create(ValueStr, AParser.CaseInsensitive);
    ecBytes:
      PEG.Value := TPEGBytes.Create(ValueBytes);
    ecCharSet:
      if AParser.DataSourceFormat=sfAnsiString then
        PEG.Value := TPEGAnsiCharSet.Create(ValueAnsiCharSet[0], AParser.CaseInsensitive)
      else
        PEG.Value := TPEGCharSet.Create(ValueAnsiCharSet[0], AParser.CaseInsensitive);
    ecByteSet:
      PEG.Value := TPEGByteSet.Create(ValueByteSet[0]);
    ecLink:
      begin
        PEG.AsLink := ValueLink.BuildPEG(AParser);
//        PEG.Value := TPEGLink.Create(nil);
//        TPEGLink(PEG.Value).Value := ValueLink.BuildPEG(AParser);
      end;
    ecAND:
      begin
        PEG.Value := TPEGSequence.Create;
        TPEGSequence(PEG.Value).Add([
          ValueOperands[0].BuildPEG(AParser),
          ValueOperands[1].BuildPEG(AParser)]);
      end;
    ecOR:
      begin
        PEG.Value := TPEGChoice.Create;
        TPEGChoice(PEG.Value).Add([
          ValueOperands[0].BuildPEG(AParser),
          ValueOperands[1].BuildPEG(AParser)]);
      end;
    ecREP:
      begin
        PEG.Value := TPEGRepeat.Create;
        TPEGRepeat(PEG.Value).Add(ValueOperands[0].BuildPEG(AParser), ValueRepMin, ValueRepMax);
      end
    else
      raise Exception.Create('Error');
  end;

  result := PEG.Value;
  result.Name := Name;
end;

function ParseAndLog(const AExpr: TExpr; const AText: string): Boolean;
begin
  result := Verify(AExpr, AText);
end;

function Verify(const AExpr: TPEGCustom; const AText: string; ACaseInsensitive: Boolean = True): Boolean; overload;
var
  p: TPEGParser;
  s: TMemoryStream;
begin
  s := TMemoryStream.Create;
  p := TPEGParser.Create(s, True, sfString, ACaseInsensitive);
  try
    s.Write(AText[Low(AText)], Length(AText)*SizeOf(Char));
    AExpr.Parser := p;
    Result := p.Execute(AExpr) and (p.DataPosition=s.Size);
    {$IFDEF PEGLOG}
    AppLog.Log('');
    AppLog.Log('PEGR.Verify "%S" = %s', [AText, IfThen(Result, 'OK', 'NO MATCHING')]);
    if Result then
      p.LogResults;
    {$ENDIF}
  finally
    FreeAndNil(p);
  end;
end;

function Verify(const AExpr: TExpr; const AText: string; ACaseInsensitive: Boolean = True): Boolean;
var
  p: TPEGParser;
  s: TMemoryStream;
  e: TPEGCustom;
begin
  s := TMemoryStream.Create;
  p := TPEGParser.Create(s, True, sfString, ACaseInsensitive);
  try
    s.Write(AText[Low(AText)], Length(AText)*SizeOf(Char));
    e := AExpr.BuildPEG(p);
    e.Parser := p;
    Result := p.Execute(e) and (p.DataPosition=s.Size);
    {$IFDEF PEGLOG}
    AppLog.Log('');
    AppLog.Log('PEGR.Verify "%S" = %s', [AText, IfThen(Result, 'OK', 'NO MATCHING')]);
    if Result then
      p.LogResults;
    {$ENDIF}
  finally
    FreeAndNil(p);
  end;
end;

{ L }

class operator L.Implicit(var a: TExpr): L;
begin
  result.Value := @A;
end;

class operator L.Implicit(const ALink: L): TExpr;
begin
  result.Command := ecLink;
  result.ValueLink := ALink.Value;
  result.Initialized := '1';
end;

end.
