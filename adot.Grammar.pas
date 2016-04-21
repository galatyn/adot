unit adot.Grammar;

{ Uniform grammar presentation classes }

interface

uses
  adot.Types,
  adot.Collections,
  adot.Tools;

type
  TByteSet = set of byte;

  TGrammarClass = class abstract
  protected
  end;

  TGrammarLink = class(TGrammarClass)
  private
    { doesn't own subgrammar, it's just a link }
    FLink: TGrammarClass;
  public
    constructor Create(ALink: TGrammarClass);
  end;

  TGrammarString = class(TGrammarClass)
  private
    FValue: String;
    FCaseSensitive: boolean;
  public
    constructor Create(Value: String; CaseSensitive: boolean);
  end;

  TGrammarSequence = class(TGrammarClass)
  public

  end;

  TGrammar = record
  public
    type
      TRange = record
        MinCount, MaxCount: integer;
      end;

      TMedia = record
      private
        MediaGrm: IInterfacedObject<TGrammarClass>;

      public
        { Repeater: B*, A+, A[2;5] }
        class operator Multiply(A: TMedia; const R: TRange): TMedia;

        { Sequence: A "abc" B}
        class operator Add(A,B: TMedia): TMedia;

        { Selection:  A | "abc" }
        class operator LogicalOr(A,B: TMedia): TMedia;

        { Logical not: A + not "abc"}
        class operator LogicalNot(A: TMedia): TMedia;
      end;

    var
      FGrm: IInterfacedObject<TGrammarClass>;
  public
    class operator Implicit(A : TMedia) : TGrammar;
  end;

{ all possible expressions for TGrammar (used in right side of rule definition) }
function Expr(var AGrammar: TGrammar): TGrammar.TMedia; overload;
function Expr(Value: String; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function Expr(CharRangeFrom,CharRangeTo: Char; CaseSensitive: boolean = False): TGrammar.TMedia; overload;
function Expr(Value: TArray<Byte>): TGrammar.TMedia; overload;
function Expr(const ByteSet: TByteSet): TGrammar.TMedia; overload;
function Expr(CharSet: TSet<Char>): TGrammar.TMedia; overload;

{ all possible repeaters for TGrammar (used as multiplyer of expression in right side of rule definiton) }
function Rep(AMinCount,AMaxCount: integer): TGrammar.TRange; overload;
function Rep(AExactCount: integer): TGrammar.TRange; overload;
function Rep: TGrammar.TRange; overload;

implementation

function Expr(var AGrammar: TGrammar): TGrammar.TMedia;
begin
  result.MediaGrm := AGrammar.FGrm;
end;

function Expr(Value: String; CaseSensitive: boolean): TGrammar.TMedia;
begin
  result.MediaGrm := TInterfacedObject<TGrammarClass>.Create(TGrammarString.Create(Value, CaseSensitive));
end;

function Expr(CharRangeFrom,CharRangeTo: Char; CaseSensitive: boolean): TGrammar.TMedia;
begin
end;

function Expr(Value: TArray<Byte>): TGrammar.TMedia;
begin
end;

function Expr(const ByteSet: TByteSet): TGrammar.TMedia;
begin

end;

function Expr(CharSet: TSet<Char>): TGrammar.TMedia;
begin

end;

function Rep(AMinCount,AMaxCount: integer): TGrammar.TRange;
begin
  result.MinCount := AMinCount;
  result.MaxCount := AMaxCount;
end;

function Rep(AExactCount: integer): TGrammar.TRange;
begin
  result.MinCount := AExactCount;
  result.MaxCount := AExactCount;
end;

function Rep: TGrammar.TRange;
begin
  result.MinCount := 0;
  result.MaxCount := High(result.MaxCount);
end;

{ TGrammar }

class operator TGrammar.Implicit(A: TMedia): TGrammar;
begin

end;

{ TMedia }

class operator TGrammar.TMedia.Add(A, B: TMedia): TMedia;
begin

end;

class operator TGrammar.TMedia.LogicalNot(A: TMedia): TMedia;
begin

end;

class operator TGrammar.TMedia.LogicalOr(A, B: TMedia): TMedia;
begin

end;

class operator TGrammar.TMedia.Multiply(A: TMedia; const R: TRange): TMedia;
begin

end;

{ TGrammarLink }

constructor TGrammarLink.Create(ALink: TGrammarClass);
begin
  FLink := ALink;
end;

{ TGrammarString }

constructor TGrammarString.Create(Value: String; CaseSensitive: boolean);
begin
  FValue := Value;
  FCaseSensitive := CaseSensitive;
end;

end.
