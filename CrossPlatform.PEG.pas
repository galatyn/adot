unit CrossPlatform.PEG;
// Parsing expression grammar (PEG) for Delphi.

interface

uses
  {$IFDEF PEGLOG}
  CrossPlatform.Log, VCL.Log,
  {$ENDIF}
  CrossPlatform.Tools, System.Generics.Collections, System.Generics.Defaults,
  System.SysUtils, System.Classes, System.Types, System.AnsiStrings,
  System.StrUtils;

type
  TByteSet = set of Byte;
  TAnsiCharSet = set of AnsiChar;
  TSourceFormat = (sfString, sfAnsiString);

  // Status of matching for expression/subexpression.
  TPEGOpRes = (
    oprFail,          // pattern matching failed
    oprOk,            // pattern matching succeded
    oprOkRestoreData, // matching succedded, but data should not be consumed
    oprMore);         // additional operation is required

  TPEGCustom = class;
  TInstanceId = record
    PEG: TPEGCustom;
    Pos: integer;
  end;

  // Item of parsing stack (keeps PEG, instance variables, rollback position etc).
  TPEGInstance = class
  protected
    function GetInstanceId: TInstanceId;
    function GetAsString: string;
    function GetResultatAsString: string;

  public
    Parent   : TPEGInstance; // to build tree of results
    PEG      : TPEGCustom;   // single TPEGxxx can be shared by many TPEGInstance at the stack
    DataPos  : integer;      // start position (used to rollback data stream, calculate len etc)
    Len      : integer;      // Len bytes at DataPos are matched
    Started  : Boolean;      // False=first call, True=subsequent call
    CurIndex : integer;      // repeat/choice/sequence/... keep here internal state (usually subexpression index)
    EndPos   : integer;      // repeat/... keep here internal state (end position for last op)

    constructor Create(AParent: TPEGInstance; APEG: TPEGCustom; ADataPos: integer);

    property Id: TInstanceId read GetInstanceId;
    property AsString: string read GetAsString;
    property ResAsString: string read GetResultatAsString;
  end;

  TRecursionAction = (lraSkip, lraError);
  TPEGParser = class
  protected
    FDataSource: TStream;
    FOwnsDataSource: boolean;
    FLastResult: TPEGOpRes;
    FStack: TObjectStack<TPEGInstance>;
    FCP: integer;
    FDataSourceFormat: TSourceFormat;
    FCaseInsensitive: Boolean;
    FRecursionSet: TSet<TInstanceId>;
    FRecursionAction: TRecursionAction;
    FIteration: integer;
    FChildInstance: TPEGInstance;

    procedure ExecSubExpression(AParent: TPEGInstance; ASubExpression: TPEGCustom); inline;
    function GetBytesLeft: integer; inline;
    function GetDataPosition: integer; inline;
    procedure RecursionError;
    procedure ResetCurrentExpression; inline;

    {$IFDEF PEGLOG}
    procedure LogStr(const s: string; const Args: array of const);
    {$ENDIF}
    function LogExprStack(APEGInstance: TPEGInstance): Boolean;
    function LogNextOp: Boolean;
    function LogRecursion: Boolean;
    function LogFirstOp: Boolean;
    function LogFirstOpWithSubexpr(APEGInstance: TPEGInstance): Boolean;
    function LogExprFailed: Boolean;
    function LogFinishRecursionTrack(APEGInstance: TPEGInstance): Boolean;
    function LogException(e: Exception): Boolean;

    property LastExprResult: TPEGOpRes read FLastResult;
  public
    constructor Create(ADataSource: TStream; AOwns: boolean;
      ADataSourceFormat: TSourceFormat; ACaseInsensitive: Boolean);
    destructor Destroy; override;
    function Execute(AStartPEG: TPEGCustom): Boolean;
    {$IFDEF PEGLOG}
    procedure LogResults;
    {$ENDIF}

    property DataSource: TStream read FDataSource;
    property DataSourceFormat: TSourceFormat read FDataSourceFormat;
    property DataBytesLeft: integer read GetBytesLeft;
    property DataPosition: integer read GetDataPosition;
    property CaseInsensitive: Boolean read FCaseInsensitive;
    property RecursionAction: TRecursionAction read FRecursionAction write FRecursionAction;
    property IterationsCounter: integer read FIteration;
  end;

  TExpressionType = (
    // strings
    etString, etAnsiString,
    // chars
    etCharSet, etAnsiCharSet,
    // bytes
    etBytes, etByteSet,
    // control
    etLink, etRepeat, etChoice, etSequence);

  TPEGCustom = class
  protected
    FKind: TExpressionType;
    FParser: TPEGParser;
    FSourceFormat: TSourceFormat;
    FName: string;

    // Complex expressions (OR/AND/...) may need to execute subexpressions.
    // In this case they put subexpressions one by one to Parser. It helps us
    // to avoid of deep recursion (we use parser expressions stack instead).
    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; virtual; abstract;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; virtual; abstract;
    procedure SetParser(AParser: TPEGParser); virtual;
  public
    property Kind: TExpressionType read FKind write FKind;
    property Parser: TPEGParser read FParser write SetParser;
    property SourceFormat: TSourceFormat read FSourceFormat;
    property Name: string read FName write FName;
  end;

  TPEGString = class(TPEGCustom)
  protected
    FValue: string;
    FCaseInsensitive: Boolean;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;

  public
    constructor Create(const AValue: string; ACaseInsensitive: boolean);

    property Value: String read FValue write FValue;
    property CaseInsensitive: boolean read FCaseInsensitive write FCaseInsensitive;
  end;

  TPEGAnsiString = class(TPEGCustom)
  private
  protected
    FValue: AnsiString;
    FCaseInsensitive: Boolean;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;

  public
    constructor Create(const AValue: AnsiString; ACaseInsensitive: boolean);

    property Value: AnsiString read FValue write FValue;
    property CaseInsensitive: boolean read FCaseInsensitive write FCaseInsensitive;
  end;

  TPEGCharSet = class(TPEGCustom)
  protected
    FValue: TDictionary<Char, Boolean>;
    FCIValue: TDictionary<Char, Boolean>; // lowercased+uppercased
    FCaseInsensitive: Boolean;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;

  public
    constructor Create(const AChars: TAnsiCharSet; ACaseInsensitive: Boolean);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const AChars: array of Char); overload;
    procedure Add(const AChars: string); overload;

    property CaseInsensitive: boolean read FCaseInsensitive write FCaseInsensitive;
  end;

  TPEGAnsiCharSet = class(TPEGCustom)
  private
    procedure SetValue(const Value: TAnsiCharSet);
  protected
    FValue: TAnsiCharSet;
    FCIValue: TAnsiCharSet; // lowercased + uppercased
    FCaseInsensitive: Boolean;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;

  public
    constructor Create(const AValue: TAnsiCharSet; ACaseInsensitive: Boolean); overload;

    property Value: TAnsiCharSet read FValue write SetValue;
    property CaseInsensitive: boolean read FCaseInsensitive write FCaseInsensitive;
  end;

  TPEGBytes= class(TPEGCustom)
  protected
    FValue: TByteDynArray;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;

  public
    constructor Create(const AValue: array of Byte); overload;
  end;

  TPEGByteSet = class(TPEGCustom)
  protected
    FValue: TByteSet;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;

  public
    constructor Create(const ABytes: TByteSet);
  end;

  TPEGLink = class(TPEGCustom)
  protected
    FValue: TPEGCustom;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;
    procedure SetParser(AParser: TPEGParser); override;

  public
    constructor Create(AValue: TPEGCustom);

    property Value: TPEGCustom read FValue write FValue;
  end;

  // Repeat operator is always greedy for PEG.
  TPEGRepeat = class(TPEGCustom)
  protected
    FPEG: TPEGCustom;
    FMinRep, FMaxRep: integer;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;
    procedure SetParser(AParser: TPEGParser); override;

  public
    constructor Create;
    procedure Add(APEG: TPEGCustom; AMinRep, AMaxRep: integer);
  end;

  TPEGChoice = class(TPEGCustom)
  protected
    FSubExpressions: TList<TPEGCustom>;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;
    procedure SetParser(AParser: TPEGParser); override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ASubExpressions: array of TPEGCustom);
  end;

  TPEGSequence = class(TPEGCustom)
  protected
    FSubExpressions: TList<TPEGCustom>;

    function FirstOp(AInstance: TPEGInstance): TPEGOpRes; override;
    function NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes; override;
    procedure SetParser(AParser: TPEGParser); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ASubExpressions: array of TPEGCustom);
  end;

function SetToAnsiString(const ASrc: TAnsiCharSet):AnsiString;
function PEGResToString(ASrc: TPEGOpRes): string;

implementation

function SetToAnsiString(const ASrc: TAnsiCharSet):AnsiString;
var
  C: AnsiChar;
begin
  Result := '';
  if ASrc<>[] then
    for C := Low(C) to High(C) do
      if C in ASrc then
        Result := Result + C;
end;

function PEGResToString(ASrc: TPEGOpRes): string;
begin
  case ASrc of
    oprFail          : result := 'oprFail';
    oprOk            : result := 'oprOk';
    oprOkRestoreData : result := 'oprOkRestoreData';
    oprMore          : result := 'oprMore';
    else raise Exception.Create('Error');
  end;
end;

{ TPEGParser.TPEGInstance }

constructor TPEGInstance.Create(AParent: TPEGInstance; APEG: TPEGCustom; ADataPos: integer);
begin
  Parent := AParent;
  PEG := APEG;
  DataPos := ADataPos;
end;

{ TPEGParser }

constructor TPEGParser.Create(ADataSource: TStream; AOwns: boolean;
  ADataSourceFormat: TSourceFormat; ACaseInsensitive: Boolean);
begin
  FDataSource := ADataSource;
  FOwnsDataSource := AOwns;
  FStack := TObjectStack<TPEGInstance>.Create(False);
  FDataSourceFormat := ADataSourceFormat;
  FCaseInsensitive := ACaseInsensitive;
  FRecursionSet := TSet<TInstanceId>.Create;
end;

destructor TPEGParser.Destroy;
begin
  if FOwnsDataSource then
    FreeAndNil(FDataSource)
  else
    FDataSource := nil;
  FreeAndNil(FStack);
  FreeAndNil(FRecursionSet);
  FreeAndNil(FChildInstance);
  inherited;
end;

function TPEGParser.GetBytesLeft: integer;
begin
  result := FDataSource.Size-FDataSource.Position;
end;

function TPEGParser.GetDataPosition: integer;
begin
  result := FDataSource.Position;
end;

procedure TPEGParser.RecursionError;
begin
  raise Exception.Create('Left recursion detected');
end;

procedure TPEGParser.ResetCurrentExpression;
begin
  DataSource.Position := FStack.Peek.DataPos;
end;

{$IFDEF PEGLOG}
procedure TPEGParser.LogStr(const s: string; const Args: array of const);
var p: string;
begin
  // log string with left margin
  if FStack.Count<=3 then
    p := StringOfChar('.',FStack.Count)
  else
    p := IntToStr(FStack.Count)+StringOfChar('.',FStack.Count-Length(IntToStr(FStack.Count)));
  AppLog.Log(p + '| ' + format(s,Args))
end;
{$ENDIF}

function TPEGParser.LogExprStack(APEGInstance: TPEGInstance): Boolean;
{$IFDEF PEGLOG}
var
  Enum: TObjectStack<TPEGInstance>.TEnumerator;
{$ENDIF}
begin
  result := True;
  {$IFDEF PEGLOG}
  AppLog.Log(''); LogStr('%d %s [CurStreamPos: %d][CurCmdCount: %d]', [FIteration, APEGInstance.AsString,
    DataSource.Position, FStack.Count]);
  Enum := FStack.GetEnumerator;
  try
    while Enum.MoveNext do
      LogStr('  %s : %.2d %s (%s)', [
        THex.PointerToHex(Enum.Current.PEG),
        Enum.Current.DataPos,
        Enum.Current.PEG.ClassName,
        Enum.Current.PEG.Name]);
  finally
    FreeAndNil(Enum);
  end;
  {$ENDIF}
end;

function TPEGParser.LogNextOp: Boolean;
begin
  result := True;
  {$IFDEF PEGLOG}
  LogStr('NextOp=%s [CurStreamPos: %d][CurCmdCount: %d]', [PEGResToString(FLastResult),
    DataSource.Position, FStack.Count]);
  {$ENDIF}
end;

function TPEGParser.LogFirstOp: Boolean;
begin
  result := True;
  {$IFDEF PEGLOG}
  LogStr('FirstOp=%s [CurStreamPos: %d][CurCmdCount: %d]', [PEGResToString(FLastResult),
    DataSource.Position, FStack.Count]);
  {$ENDIF}
end;

function TPEGParser.LogRecursion: Boolean;
begin
  result := True;
  {$IFDEF PEGLOG}
  LogStr('Left recursion detected, Result=oprFail (auto, no execution)', []);
  {$ENDIF}
end;

{$IFDEF PEGLOG}
procedure TPEGParser.LogResults;
begin

end;
{$ENDIF}

function TPEGParser.LogFirstOpWithSubexpr(APEGInstance: TPEGInstance): Boolean;
begin
  result := True;
  {$IFDEF PEGLOG}
  LogStr('register [PEG=%s; POS=%d] at recursion tracker]', [APEGInstance.PEG.ClassName, DataSource.Position]);
  {$ENDIF}
end;

function TPEGParser.LogExprFailed: Boolean;
begin
  result := True;
  {$IFDEF PEGLOG}
  LogStr('AFTER POP[POS&RES] [CurStreamPos: %d][CurCmdCount: %d]', [DataSource.Position,
    FStack.Count]);
  {$ENDIF}
end;

function TPEGParser.LogFinishRecursionTrack(APEGInstance: TPEGInstance): Boolean;
begin
  result := True;
  {$IFDEF PEGLOG}
  LogStr('remove [PEG=%s; POS=%d] from left recursion tracker]', [APEGInstance.PEG.ClassName, APEGInstance.DataPos]);
  {$ENDIF}
end;

function TPEGParser.LogException(e: Exception): Boolean;
begin
  result := True;
  {$IFDEF PEGLOG}
  LogStr('%s: %s', [e.ClassName, e.Message]);
  {$ENDIF}
end;

function TPEGParser.Execute(AStartPEG: TPEGCustom): Boolean;
var
  PEGInstance: TPEGInstance;
  Recursion: Boolean;
begin
  {$IFDEF PEGLOG} try {$ENDIF}
  FIteration := 0;
  DataSource.Position := 0;
  FStack.Clear;
  FRecursionSet.Clear;
  FLastResult := oprFail;
  ExecSubExpression(nil, AStartPEG);
  FCP := FStack.Count-1;
  FreeAndNil(FChildInstance);
  repeat
    Inc(FIteration);
    PEGInstance := FStack.Peek;
    Assert(LogExprStack(PEGInstance));

    if not PEGInstance.Started then
    // first step of PEG execution
    begin

      // Check for recursion.
      // Do we have the same PEG on the stack at same source position?
      Recursion := FRecursionSet.Contains(PEGInstance.Id);

      if Recursion then
      begin
        if RecursionAction=lraError then
          RecursionError;
        FLastResult := oprFail;
        Assert(LogRecursion);
      end
      else
      begin
        PEGInstance.Started := True;
        FLastResult := PEGInstance.PEG.FirstOp(PEGInstance);
        Assert(LogFirstOp);
        // We track command only if it has subcommands, otherwise it will be deleted from the stack.
        if FLastResult=oprMore then
        begin
          FRecursionSet.Add(PEGInstance.Id);
          Assert(LogFirstOpWithSubexpr(PEGInstance));
        end;
      end;
    end
    else
    // Next step of running PEG (skip recursion check)
    begin
      Recursion := False;
      FLastResult := PEGInstance.PEG.NextOp(PEGInstance, FChildInstance);
      // Some PEGs (sequence for example) may save child expressions internally to be able
      // to restore tree of results after successfull matching.
      if FChildInstance<>nil then
        FreeAndNil(FChildInstance);
      Assert(LogNextOp);
    end;

    // we got result from instance of PEG
    if FLastResult<>oprMore then
    begin
      // if command placed something to the stack, it must return oprMore
      Assert(PEGInstance = FStack.Peek);
      if FLastResult=oprOkRestoreData then
        DataSource.Position := PEGInstance.DataPos
      else
        if FLastResult=oprFail then
        begin
          DataSource.Position := PEGInstance.DataPos;
          Assert(LogExprFailed);
        end;
      if not Recursion then
      begin
        FRecursionSet.Remove(PEGInstance.Id);
        Assert(LogFinishRecursionTrack(PEGInstance));
      end;
      FChildInstance := FStack.Extract;
    end
    else
      FChildInstance := nil;

  until (FStack.Count=0);
  result := FLastResult in [oprOk, oprOkRestoreData];
  if not result then
    FreeAndNil(FChildInstance);

  {$IFDEF PEGLOG}
  except on e: Exception do begin
    LogException(e);
    Raise;
  end; end;
  {$ENDIF}
end;

procedure TPEGParser.ExecSubExpression(AParent: TPEGInstance; ASubExpression: TPEGCustom);
begin
  FStack.Push(TPEGInstance.Create(AParent,ASubExpression, DataSource.Position));
end;

{ TPEGString }

constructor TPEGString.Create(const AValue: string; ACaseInsensitive: boolean);
begin
  inherited Create;
  Kind := etString;
  Value := AValue;
  CaseInsensitive := ACaseInsensitive;
end;

function TPEGString.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
var
  s: String;
begin

  // empty string is always matched
  if FValue='' then
  begin
    AInstance.Len := 0;
    Result := oprOk;
    Exit;
  end;

  // not enough of data to match
  if FParser.DataBytesLeft<Length(FValue)*SizeOf(Char) then
  begin
    Result := oprFail;
    Exit;
  end;

  SetLength(s, Length(FValue));
  FParser.DataSource.Read(s[Low(s)], Length(s)*SizeOf(Char));

  if CaseInsensitive then
    if SameText(FValue, s) then
      Result := oprOk
    else
      Result := oprFail
  else
    if s=FValue then
      Result := oprOk
    else
      Result := oprFail;

  if Result=oprOk then
    AInstance.Len := Length(s)*SizeOf(Char);

end;

function TPEGString.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  result := oprFail;
end;

{ TPEGBytes }

constructor TPEGBytes.Create(const AValue: array of Byte);
begin
  inherited Create;
  Kind := etBytes;
  SetLength(FValue, Length(AValue));
  System.Move(AValue[Low(AValue)], FValue[0], Length(FValue));
end;

function TPEGBytes.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
var
  t: TByteDynArray;
begin

  // empty string is always matched
  if Length(FValue)=0 then
  begin
    AInstance.Len := 0;
    Result := oprOk;
    Exit;
  end;

  // not enough of data to match
  if FParser.DataBytesLeft<Length(FValue) then
  begin
    Result := oprFail;
    Exit;
  end;

  SetLength(t, Length(FValue));
  FParser.DataSource.Read(t[Low(t)], Length(t));
  if not CompareMem(@t[0], @FValue[0], Length(t)) then
    Result := oprFail
  else
  begin
    AInstance.Len := Length(t);
    Result := oprOk;
  end;

end;

function TPEGBytes.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  result := oprFail;
end;

{ TPEGByteSet }

constructor TPEGByteSet.Create(const ABytes: TByteSet);
begin
  inherited Create;
  Kind := etByteSet;
  FValue := ABytes;
end;

function TPEGByteSet.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
var
  b: byte;
begin
  if Parser.DataBytesLeft=0 then
    result := oprFail
  else
  begin
    Parser.DataSource.Read(b, 1);
    if not (b in FValue) then
      Result := oprFail
    else
    begin
      AInstance.Len := 1;
      Result := oprOk;
    end;
  end;
end;

function TPEGByteSet.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  result := oprFail;
end;

{ TPEGCharSet }

constructor TPEGCharSet.Create(const AChars: TAnsiCharSet; ACaseInsensitive: Boolean);
begin
  inherited Create;
  Kind := etCharSet;
  FValue := TDictionary<Char, Boolean>.Create;
  FCIValue := TDictionary<Char, Boolean>.Create;
  CaseInsensitive := ACaseInsensitive;
  Add(String(SetToAnsiString(AChars)));
end;

destructor TPEGCharSet.Destroy;
begin
  FreeAndNil(FValue);
  FreeAndNil(FCIValue);
  inherited;
end;

procedure TPEGCharSet.Clear;
begin
  FValue.Clear;
  FCIValue.Clear;
end;

procedure TPEGCharSet.Add(const AChars: string);
var
  I: Integer;
  S: String;
begin
  // FValue
  for I := Low(AChars) to High(AChars) do
    FValue.AddOrSetValue(AChars[I], True);

  // FCIValue
  S := AnsiLowerCase(AChars);
  for I := Low(S) to High(S) do
    FCIValue.AddOrSetValue(S[I], True);
  S := AnsiUpperCase(S);
  for I := Low(S) to High(S) do
    FCIValue.AddOrSetValue(S[I], True);
end;

procedure TPEGCharSet.Add(const AChars: array of Char);
var
  I: Integer;
  S: String;
begin
  S := '';
  for I := Low(AChars) to High(AChars) do
    S := S + AChars[I];
  Add(S);
end;

function TPEGCharSet.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
var
  C: Char;
begin

  if Parser.DataBytesLeft<SizeOf(C) then
  begin
    result := oprFail;
    exit;
  end;

  Parser.DataSource.Read(C, SizeOf(C));

  // check
  if CaseInsensitive then
    if FCIValue.ContainsKey(C) then
      Result := oprOk
    else
      Result := oprFail
  else
    if FValue.ContainsKey(C) then
      Result := oprOk
    else
      Result := oprFail;

  if result=oprOk then
    AInstance.Len := SizeOf(C);
end;

function TPEGCharSet.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  result := oprFail;
end;

{ TPEGAnsiCharSet }

constructor TPEGAnsiCharSet.Create(const AValue: TAnsiCharSet; ACaseInsensitive: Boolean);
begin
  inherited Create;
  Kind := etAnsiCharSet;
  Value := AValue;
  CaseInsensitive := ACaseInsensitive;
end;

procedure TPEGAnsiCharSet.SetValue(const Value: TAnsiCharSet);
var
  S: AnsiString;
  I: Integer;
begin
  if FValue = Value then
    Exit;
  FValue := Value;
  S := AnsiLowerCase(SetToAnsiString(Value));
  FCIValue := [];
  for I := Low(S) to High(S) do
    Include(FCIValue, S[I]);
  S := AnsiUpperCase(S);
  for I := Low(S) to High(S) do
    Include(FCIValue, S[I]);
end;

function TPEGAnsiCharSet.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
var
  C: AnsiChar;
begin

  if Parser.DataBytesLeft<SizeOf(C) then
  begin
    result := oprFail;
    exit;
  end;

  Parser.DataSource.Read(C, SizeOf(C));

  // check
  if CaseInsensitive then
    if C in FCIValue then
      Result := oprOk
    else
      Result := oprFail
  else
    if C in FValue then
      Result := oprOk
    else
      Result := oprFail;

  if result=oprOk then
    AInstance.Len := SizeOf(C);
end;

function TPEGAnsiCharSet.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  result := oprFail;
end;

{ TPEGLink }

constructor TPEGLink.Create(AValue: TPEGCustom);
begin
  inherited Create;
  Kind := etLink;
  FValue := AValue;
end;

function TPEGLink.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
begin
  result := FValue.FirstOp(AInstance);
end;

function TPEGLink.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  result := FValue.NextOp(AInstance, AFinishedChild);
end;

procedure TPEGLink.SetParser(AParser: TPEGParser);
begin
  if Parser=AParser then
    Exit;
  inherited;
  FValue.SetParser(AParser);
end;

{ TPEGRepeat }

procedure TPEGRepeat.Add(APEG: TPEGCustom; AMinRep, AMaxRep: integer);
begin
  Assert(AMaxRep>0);
  Kind    := etRepeat;
  FPEG    := APEG;
  FMinRep := AMinRep;
  FMaxRep := AMaxRep;
end;

constructor TPEGRepeat.Create;
begin
  inherited Create;
  Kind := etRepeat;
end;

function TPEGRepeat.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
begin
  Result := oprMore;
  AInstance.EndPos := AInstance.DataPos;
  Parser.ExecSubExpression(AInstance, FPEG);
end;

function TPEGRepeat.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin

  // We don't accept oprOkRestoreData here!
  // Expression under repeat operator must consume data, otherwise
  // greeby repeater never go forward and never stop.
  if Parser.LastExprResult=oprOk then
  begin
    inc(AInstance.CurIndex);
    AInstance.EndPos := FParser.DataPosition;

    // we should try to repeat
    if AInstance.CurIndex<FMaxRep then
    begin
      Result := oprMore;
      Parser.ExecSubExpression(AInstance, FPEG);
      Exit;
    end;

    // Count=FMaxRep
    Result := oprOk;
    AInstance.Len := AInstance.EndPos-AInstance.DataPos;
    Exit;
  end;

  Assert(Parser.LastExprResult=oprFail);

  // we don't have MinRep items, matching failed
  if AInstance.CurIndex<FMinRep then
  begin
    Result := oprFail; // parser will cancel our results automatically
    Exit;
  end;

  // we found MinRep matches at least, so we have result
  Result := oprOk;
  if AInstance.CurIndex>0 then
    AInstance.Len := AInstance.EndPos-AInstance.DataPos
  else
  begin
    // Repeaters like "x*" return oprOk even if there is no matching for "x" (0 matches found),
    // in such case EndPos is not set and we should use EndPos=FStart.
    AInstance.EndPos := AInstance.DataPos;
    AInstance.Len := 0;
  end;

end;

procedure TPEGRepeat.SetParser(AParser: TPEGParser);
begin
  if Parser=AParser then
    Exit;
  inherited;
  FPEG.Parser := AParser;
end;

{ TPEGChoice }

procedure TPEGChoice.Add(const ASubExpressions: array of TPEGCustom);
var
  I: Integer;
begin
  FSubExpressions := TList<TPEGCustom>.Create;
  Kind := etChoice;
  for I := Low(ASubExpressions) to High(ASubExpressions) do
    FSubExpressions.Add(ASubExpressions[I]);
end;

constructor TPEGChoice.Create;
begin
  inherited Create;
  Kind := etChoice;
end;

destructor TPEGChoice.Destroy;
begin
  FreeAndNil(FSubExpressions);
  inherited;
end;

function TPEGChoice.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
begin
  if FSubExpressions.Count=0 then
  begin
    result := oprFail;
    Exit;
  end;

  Result := oprMore;
  Parser.ExecSubExpression(AInstance, FSubExpressions[0]);
end;

function TPEGChoice.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin

  // match found
  if Parser.LastExprResult=oprOk then
    // last command must consume all source data, for exampe if we parse "(2)+3" it is not enough to find "(2)", we
    // should parse full expression
    if (Parser.FStack.Count=1) and (Parser.DataBytesLeft>0) then
      Parser.ResetCurrentExpression  // we will try another subexpression to consume all input
    else
    begin
      Result := oprOk;
      AInstance.Len := FParser.DataPosition-AInstance.DataPos;
      Exit;
    end;

  // we checked all expression, no matching
  inc(AInstance.CurIndex);
  if AInstance.CurIndex>=FSubExpressions.Count then
  begin
    Result := oprFail;
    Exit;
  end;

  // try next expression
  Result := oprMore;
  Parser.ExecSubExpression(AInstance, FSubExpressions[AInstance.CurIndex]);

end;

procedure TPEGChoice.SetParser(AParser: TPEGParser);
var
  I: Integer;
begin
  if Parser=AParser then
    Exit;
  inherited;
  for I := 0 to FSubExpressions.Count-1 do
    FSubExpressions[I].Parser := AParser;
end;

{ TPEGSequence }

procedure TPEGSequence.Add(const ASubExpressions: array of TPEGCustom);
var
  I: Integer;
begin
  FSubExpressions := TList<TPEGCustom>.Create;
  Kind := etSequence;
  for I := Low(ASubExpressions) to High(ASubExpressions) do
    FSubExpressions.Add(ASubExpressions[I]);
end;

constructor TPEGSequence.Create;
begin
  inherited Create;
  Kind := etSequence;
end;

destructor TPEGSequence.Destroy;
begin
  FreeAndNil(FSubExpressions);
  inherited;
end;

function TPEGSequence.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
begin
  if FSubExpressions.Count=0 then
  begin
    result := oprFail;
    Exit;
  end;

  Result := oprMore;
  Parser.ExecSubExpression(AInstance, FSubExpressions[0]);
end;

function TPEGSequence.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  if Parser.LastExprResult<>oprOk then
  begin
    Result := oprFail;
    Exit;
  end;

  inc(AInstance.CurIndex);
  if AInstance.CurIndex<FSubExpressions.Count then
  begin
    Parser.ExecSubExpression(AInstance, FSubExpressions[AInstance.CurIndex]);
    Result := oprMore;
    Exit;
  end;

  Result := oprOk;
  AInstance.Len := FParser.DataPosition-AInstance.DataPos;
end;

procedure TPEGSequence.SetParser(AParser: TPEGParser);
var
  I: Integer;
begin
  if Parser=AParser then
    Exit;
  inherited;
  for I := 0 to FSubExpressions.Count-1 do
    FSubExpressions[I].Parser := AParser;
end;

{ TPEGCustom }

procedure TPEGCustom.SetParser(AParser: TPEGParser);
begin
  FParser := AParser;
end;

{ TPEGAnsiString }

constructor TPEGAnsiString.Create(const AValue: AnsiString; ACaseInsensitive: boolean);
begin
  inherited Create;
  Kind := etAnsiString;
  Value := AValue;
  CaseInsensitive := ACaseInsensitive;
end;

function TPEGAnsiString.FirstOp(AInstance: TPEGInstance): TPEGOpRes;
var
  s: AnsiString;
begin

  // empty string is always matched
  if FValue='' then
  begin
    AInstance.Len := 0;
    Result := oprOk;
    Exit;
  end;

  // not enough of data to match
  if FParser.DataBytesLeft<Length(FValue) then
  begin
    Result := oprFail;
    Exit;
  end;

  SetLength(s, Length(FValue));
  FParser.DataSource.Read(s[Low(s)], Length(s));

  if CaseInsensitive then
    if SameText(FValue, s) then
      Result := oprOk
    else
      Result := oprFail
  else
    if s=FValue then
      Result := oprOk
    else
      Result := oprFail;

  if Result=oprOk then
    AInstance.Len := Length(s);

end;

function TPEGAnsiString.NextOp(AInstance: TPEGInstance; var AFinishedChild: TPEGInstance): TPEGOpRes;
begin
  result := oprFail;
end;

function TPEGInstance.GetAsString: string;
var PegName: string;
begin
  if PEG=nil then PegName := 'PEG=nil' else
  begin
    PegName := PEG.ClassName+'.'+PEG.Name;
    if PEG.Kind=etLink then
      PegName := PegName + '.' + TPEGLink(PEG).FValue.ClassName+'.'+TPEGLink(PEG).FValue.Name;
  end;
  result := Format('%s."%s"[%s][DataPos: %d]', [
    ClassName, PegName, IfThen(Started, 'RUNNING', 'NEW'), DataPos]);
end;

function TPEGInstance.GetInstanceId: TInstanceId;
begin
  result.PEG := PEG;
  result.Pos := DataPos;
end;

function TPEGInstance.GetResultatAsString: string;
begin
  result := Format('"%s": %s (Start: %d, Len: %d)', [PEG.Name, PEG.ClassName, DataPos, Len]);
end;

end.
