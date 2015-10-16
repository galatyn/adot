unit adot.Strings;

interface

uses
  System.Classes, System.SysUtils, System.Character;

type
  TAnsiChars = set of AnsiChar;

  // Class to search words inside of text (to extract words/numbers etc).
  // Example 1:
  //   TTextWords.Get('list of 7 words and 2 numbers', Arr, TTextWords.IsPartOfNumber) -> Arr=['7', '2']
  //   TTextWords.Get('list of 7 words and 2 numbers', Arr) -> Arr=['list', 'of', '7', 'words', 'and', '2', 'numbers']
  // Example 2:
  //   W := TTextWords.Create(AText);
  //   while W.findNext(Str) do
  //     <do something with> Str
  // Example 3:
  //   W := TTextWords.Create(AText);
  //   W.Get(L); // L: TArray<TWordPosRec>
  //   for i := 0 to High(l) do
  //     <do something with> W.Words[l[i]]
  PTextWords = ^TTextWords;
  TTextWords = record
  public
    type
      TIsAlphaPredicate = reference to function(const C: Char):Boolean;
      TWordPosRec = record
        Start, Len: Integer;
      end;
    var
      Text: String;
      Position: integer;
      IsAlphaChar: TIsAlphaPredicate;

    // Set Text and IsAlpha predicate (default is IsLetterOrDigit).
    constructor Create(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate = nil);
    function Prepare(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate = nil): PTextWords;

    // Find next word in the text.
    function FindNext(var AStart,ALen: integer):Boolean; overload;
    function FindNext(var AWord: String):Boolean; overload;
    function FindNext: String; overload; inline;

    // Find all words in the text (from begin to end).
    procedure Get(ADst: TStrings); overload;
    procedure Get(var ADst: TArray<String>); overload;
    procedure Get(var ADst: TArray<TWordPosRec>); overload;

    // Helpers
    class procedure Get(const AText: string; ADst: TStrings; AIsAlphaPredicate: TIsAlphaPredicate = nil); overload; static;
    class procedure Get(const AText: string; var ADst: TArray<String>; AIsAlphaPredicate: TIsAlphaPredicate = nil); overload; static;
    class procedure Get(const AText: string; var ADst: TArray<TWordPosRec>; AIsAlphaPredicate: TIsAlphaPredicate = nil); overload; static;

    // Start search from begin of the text
    procedure Reset; inline;

    // Get number of words in the text (doesn't change current position)
    function Count:Integer;

    // Get substring from the assigned text (usually APos is result of FindNext or Get)
    function GetSubStr(const APos: TWordPosRec): String;

    // Find subsequence of strings in the list of words:
    //    r.Prepare('word0 word1 it is test xxx xxx xxx').Get( WordList );
    //    Assert(r.Find(['it', 'is', 'test'], WordList) = 2);
    function Find(const ASubSequence: array of string; const ASequence: TArray<TWordPosRec>): integer;

    // Most common TIsAlphaPredicate functions
    class function IsNonSpace(const C: Char): Boolean; static;
    class function IsLetter(const C: Char): Boolean; static;
    class function IsLetterOrDigit(const C: Char): Boolean; static;
    class function IsDigit(const C: Char): Boolean; static;
    class function IsPartOfNumber(const C: Char): Boolean; static; // digit or DecimalSeparator

    property Words[const APos: TWordPosRec]: string read GetSubStr; default;
  end;

function CharsToString(const AChars: TAnsiChars): string;
function StringToChars(const s: string): TAnsiChars;
function RandomString(ALen: integer; const AChars: TAnsiChars = ['a'..'z']): string; overload;
function RandomString(ALen: integer; const AChars: string): string; overload;

implementation

uses
  adot.Tools;

function CharsToString(const AChars: TAnsiChars): string;
var
  c: AnsiChar;
  n: Integer;
begin
  n := 0;
  for c := Low(c) to High(c) do
    if c in AChars then
      inc(n);
  setlength(result, n);
  n := 0;
  for c := Low(c) to High(c) do
    if c in AChars then
    begin
      result[low(result)+n] := Char(Word(c));
      inc(n);
    end;
end;

function StringToChars(const s: string): TAnsiChars;
var
  i: Integer;
begin
  result := [];
  for i := Low(s) to High(s) do
    if Word(s[i])>127 then
      raise EInvalidOperation.Create('Error')
    else
      include(result, AnsiChar(Byte(s[i])));
end;

function RandomString(ALen: integer; const AChars: TAnsiChars = ['a'..'z']): string;
begin
  result := RandomString(ALen, CharsToString(AChars));
end;

function RandomString(ALen: integer; const AChars: string): string;
var
  I,J: Integer;
begin
  J := Length(AChars);
  setlength(result, ALen);
  for I := Low(result) to High(result) do
    result[I] := AChars[Low(AChars)+Random(J)];
end;

{ TTextWords }

function TTextWords.Prepare(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate = nil): PTextWords;
begin
  result := @Self;
  Text := AText;
  if Assigned(AIsAlphaPredicate) then
    IsAlphaChar := AIsAlphaPredicate
  else
    IsAlphaChar := IsLetterOrDigit;
  Reset;
end;

procedure TTextWords.Reset;
begin
  Position := Low(Text);
end;

function TTextWords.Count: Integer;
var
  OldPosition, Start, Len: Integer;
begin
  OldPosition := Position;
  Reset;
  Result := 0;
  while FindNext(Start, Len) do
    Inc(Result);
  Position := OldPosition;
end;

function TTextWords.FindNext(var AStart, ALen: integer): Boolean;
var
  i, Finish: integer;
begin
  AStart := -1;
  for i := Position to Length(Text) do
    if IsAlphaChar(Text[i]) then
    begin
      AStart := i;
      Break;
    end;
  Result := AStart>=Low(Text);
  if not Result then
  begin
    ALen := 0;
    Position := High(Text)+1;
    Exit;
  end;
  Finish := High(Text);
  for i := AStart+1 to High(Text) do
    if not IsAlphaChar(Text[i]) then
    begin
      Finish := i-1;
      Break;
    end;
  Position := Finish+1;
  ALen := Finish-AStart+1;
end;

function TTextWords.FindNext(var AWord: String): Boolean;
var
  Start, Len: integer;
begin
  Result := FindNext(Start, Len);
  if Result then
    AWord := Copy(Text, Start, Len)
  else
    AWord := '';
end;

function TTextWords.FindNext: String;
begin
  FindNext(Result);
end;

constructor TTextWords.Create(const AText: string; AIsAlphaPredicate: TIsAlphaPredicate);
begin
  Prepare(AText, AIsAlphaPredicate);
end;

function TTextWords.Find(const ASubSequence: array of string;
  const ASequence: TArray<TWordPosRec>): integer;
var
  i,j: Integer;
  b: Boolean;
begin
  for i := 0 to High(ASequence)-Length(ASubSequence)+1 do
  begin
    b := True;
    for j := 0 to High(ASubSequence) do
      if not AnsiSameText(GetSubStr(ASequence[i+j]), ASubSequence[j]) then
      begin
        b := False;
        break;
      end;
    if b then
    begin
      result := i;
      Exit;
    end;
  end;
  result := -1;
end;

procedure TTextWords.Get(ADst: TStrings);
var
  w: string;
begin
  Reset;
  while FindNext(w) do
    ADst.Add(w);
end;

procedure TTextWords.Get(var ADst: TArray<String>);
var
  i: Integer;
begin
  SetLength(ADst, Count);
  Reset;
  for i := 0 to High(ADst) do
    FindNext(ADst[i]);
end;

procedure TTextWords.Get(var ADst: TArray<TWordPosRec>);
var
  i: Integer;
begin
  SetLength(ADst, Count);
  Reset;
  for i := 0 to High(ADst) do
    FindNext(ADst[i].Start, ADst[i].Len);
end;

class function TTextWords.IsNonSpace(const C: Char): Boolean;
begin
  result := C>' ';
end;

class function TTextWords.IsPartOfNumber(const C: Char): Boolean;
begin
  result := C.IsDigit or (C=FormatSettings.DecimalSeparator);
end;

class function TTextWords.IsLetter(const C: Char): Boolean;
begin
  Result := C.IsLetter;
end;

class function TTextWords.IsLetterOrDigit(const C: Char): Boolean;
begin
  Result := C.IsLetter or C.IsDigit;
end;

class function TTextWords.IsDigit(const C: Char): Boolean;
begin
  Result := C.IsDigit;
end;

function TTextWords.GetSubStr(const APos: TWordPosRec): String;
begin
  Result := Copy(Text, APos.Start, APos.Len);
end;

class procedure TTextWords.Get(const AText: string; ADst: TStrings; AIsAlphaPredicate: TIsAlphaPredicate);
var
  w: TTextWords;
begin
  w.Prepare(AText, AIsAlphaPredicate);
  w.Get(ADst);
end;

class procedure TTextWords.Get(const AText: string; var ADst: TArray<String>; AIsAlphaPredicate: TIsAlphaPredicate);
var
  w: TTextWords;
begin
  w.Prepare(AText, AIsAlphaPredicate);
  w.Get(ADst);
end;

class procedure TTextWords.Get(const AText: string; var ADst: TArray<TWordPosRec>; AIsAlphaPredicate: TIsAlphaPredicate);
var
  w: TTextWords;
begin
  w.Prepare(AText, AIsAlphaPredicate);
  w.Get(ADst);
end;

end.
