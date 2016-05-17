unit adot.Grammar.Peg;

{ Parser for grammar based on TGrammarClass:
  - recursive (similar to Recursive descent parser)
  - stops at first successfull choice (similar to "parsing expression grammars")
  - uses memoization of intermediate results (similar to "packrat parsing") }

interface

uses
  adot.Grammar.Types,
  adot.Grammar,
  adot.Tools,
  adot.Collections,
  adot.Strings,
  adot.Log,
  System.SysUtils,
  System.Classes,
  System.Character;

type

  TPegParser = class(TGrammarParser)
  protected
    type
      PCallStackItem = ^TCallStackItem;
      TCallStackItem = record
        Rule: TGrammarClass;
        Step: integer;
        Start: integer;
        Len: integer;
        ResIndex: integer;      { position of matching result in tree-like structure organized as array of TMatchingResult)}
        ResLastChild: integer;

        procedure SetUp(ARule: TGrammarClass); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
        procedure SetUp(ARule: TGrammarClass; AStart: integer); overload; {$IFNDEF DEBUG}inline;{$ENDIF}
      end;

    procedure LogTextInputParseTree(const ParseTree: TVector<TParseTreeItem>; ResIndex, Margin: integer);

  public
    function Accepted: Boolean; override;
    procedure LogParseTree; override;
  end;

implementation

{ TPegParser.TCallStackItem }

procedure TPegParser.TCallStackItem.SetUp(ARule: TGrammarClass);
begin
  Rule := ARule;
  Step := 0;
end;

procedure TPegParser.TCallStackItem.SetUp(ARule: TGrammarClass; AStart: integer);
begin
  Rule  := ARule;
  Step  := 0;
  Start := AStart;
end;

{ TPegParser }

procedure TPegParser.LogParseTree;
var
  Tree: TVector<TParseTreeItem>;
begin
  AppLog.Log('');
  AppLog.Log('Input string:');
  AppLog.Log(TStr.GetPrintable(Data.Text));
  AppLog.Log('Parse tree:');
  GetParseTree(Tree);
  LogTextInputParseTree(Tree, 0,0);
  AppLog.Log('Full parse tree:');
  LogTextInputParseTree(ParseTree.Tree, ParseTree.Root,0);
end;

procedure TPegParser.LogTextInputParseTree(const ParseTree: TVector<TParseTreeItem>; ResIndex, Margin: integer);

  procedure L(const S: string; const Args: array of const; Margin: integer);
  begin
    AppLog.Log(StringOfChar(' ', Margin) + Format(S, Args));
  end;

  function ShowWS(const S: string): String;
  const
    CWhiteSpace = #$2591  { shade char to show trailing spaces };
    { arrows are not available for most of mono fonts, we use russian char instead }  //#$2B10; { arrow char to show empty string position }
    CEmptyStrPos = 'Ã';
  var
    I: Integer;
  begin
    result := S;

    { we replace trailing whitespaces to make them visible }
    for I := Low(Result) to High(Result) do
      if Result[I].IsWhiteSpace then Result[I] := CWhiteSpace else Break;
    for I := High(Result) downto Low(Result) do
      if Result[I].IsWhiteSpace then Result[I] := CWhiteSpace else Break;

    { we replace empty string to indicate position }
    if Result='' then
      Result := CEmptyStrPos;
  end;

var
  R: TParseTreeItem;
  S,T: string;
begin
  while ResIndex>=0 do
  begin
    R := ParseTree.Items[ResIndex];

    Assert(R.Position.Len mod SizeOf(Char)=0);
    L('%s Pos: %d Len: %d)', [R.Rule.Info, R.Position.Start, R.Position.Len], Margin);
    Data.Position := R.Position.Start;
    Data.Read(S, R.Position.Len div SizeOf(Char));
    T := Data.Text;
    if T=S then
      L('       %s', [TStr.GetPrintable(S)], Margin)
    else
    begin
      L('       %s', [StringOfChar(' ', R.Position.Start div 2) + ShowWS(TStr.GetPrintable(S)) ], Margin);
      L('       %s', [ShowWS(TStr.GetPrintable(T))], Margin);
    end;

    LogTextInputParseTree(ParseTree, R.FirstChild, Margin + 2);
    ResIndex := ParseTree.Items[ResIndex].NextSibling;
  end;
end;

function TPegParser.Accepted: Boolean;
var

  { stack of calls (to avoid deep recursion) }
  Stack: TVector<TCallStackItem>;

  { result of last operation on stack:
    Accept   - input accepted
    Len      - length of accepted block if Accept=True, undefined if Accept=False
    ResIndex - index of parse tree element }
  Len: integer;
  Accept: boolean;
  ResIndex: integer;

  { pointer to current/new item on the stack (invalid after any modification of the stack) }
  Item: PCallStackItem;
begin
  Stack.Clear;
  ParseTree.Clear;
  Accept := False;
  Len := 0;
  ResIndex := -1;
  Stack.Items[Stack.Add].SetUp(Grammar);
  repeat
    Item := @Stack.Items[Stack.Count-1];
    case Item.Rule.GrammarType of

      gtUnknown:
        raise Exception.Create('rule is not initialized');

      gtLink:
        Item.SetUp(TGrammarLink(Item.Rule).Op.Data);

      gtString:
        begin
          Len    := TGrammarString(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtChar:
        begin
          Len    := TGrammarChar(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtCharClass:
        begin
          Len    := TGrammarCharClass(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtCharSet:
        begin
          Len    := TGrammarCharSetClass(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtBytes:
        begin
          Len    := TGrammarBytesClass(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept then
          begin
            ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[ResIndex].SetUp(Item.Rule, Data.Position, Len);
          end;
          Stack.DeleteLast;
        end;

      gtSequence:
        case Item.Step of
          0: begin
               Inc(Item.Step);
               Stack.Items[Stack.Add].SetUp(TGrammarSequence(Item.Rule).Op1.Data);
             end;
          1: if not Accept then
               Stack.DeleteLast
             else begin
               Inc(Item.Step);
               Item.Len := Len;
               Data.Position := Data.Position + Len;
               Item.ResIndex := ResIndex;
               Stack.Items[Stack.Add].SetUp(TGrammarSequence(Item.Rule).Op2.Data);
             end;
          2: begin
               Data.Position := Data.Position - Item.Len;
               if Accept then
               begin
                 Inc(Len, Item.Len);
                 ParseTree.Tree.Items[ParseTree.Tree.Add].SetUp(Item.Rule, Data.Position, Len, Item.ResIndex);
                 ParseTree.Tree.Items[Item.ResIndex].NextSibling := ResIndex;
                 ResIndex := ParseTree.Tree.Count-1;
               end;
               Stack.DeleteLast;
             end;
        end;

      gtSelection:
        case Item.Step of
          0: begin
               Inc(Item.Step);
               Stack.Items[Stack.Add].SetUp(TGrammarSelection(Item.Rule).Op1.Data);
             end;
          1: if Accept then begin
               ParseTree.Tree.Items[ParseTree.Tree.Add].SetUp(Item.Rule, Data.Position, Len, ResIndex);
               ResIndex := ParseTree.Tree.Count-1;
               Stack.DeleteLast;
             end else begin
               Inc(Item.Step);
               Stack.Items[Stack.Add].SetUp(TGrammarSelection(Item.Rule).Op2.Data);
             end;
          2: begin
               if Accept then
               begin
                 ParseTree.Tree.Items[ParseTree.Tree.Add].SetUp(Item.Rule, Data.Position, Len, ResIndex);
                 ResIndex := ParseTree.Tree.Count-1;
               end;
               Stack.DeleteLast;
             end;
        end;

      gtRepeat:
        if Item.Step=0 then
          if TGrammarGreedyRepeater(Item.Rule).MaxCount <= 0 then begin
            Accept := False;
            Stack.DeleteLast;
          end else begin
            Inc(Item.Step);
            Item.Start := Data.Position;
            Item.Len := 0;
            Item.ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[Item.ResIndex].SetUp(Item.Rule, Data.Position, 0);
            Stack.Items[Stack.Add].SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
          end
        else
        if Accept and (Item.Step < TGrammarGreedyRepeater(Item.Rule).MaxCount) then begin
          Inc(Item.Step);
          Inc(Item.Len, Len);
          if Item.Step=2 { we just incremented, 2 means that first accepted iteration } then
            ParseTree.Tree.Items[Item.ResIndex].FirstChild := ResIndex
          else
            ParseTree.Tree.Items[Item.ResLastChild].NextSibling := ResIndex;
          Item.ResLastChild := ResIndex;
          Data.Position := Data.Position + Len;
          Stack.Items[Stack.Add].SetUp(TGrammarGreedyRepeater(Item.Rule).Op.Data);
        end
        else begin
          { add last iteration to the result (if accepted) }
          if Accept then
          begin
            Inc(Item.Step);
            Inc(Item.Len, Len);
            if Item.Step=2 { we just incremented, 2 means that first accepted iteration } then
              ParseTree.Tree.Items[Item.ResIndex].FirstChild := ResIndex
            else
              ParseTree.Tree.Items[Item.ResLastChild].NextSibling := ResIndex;
          end;
          { ">" because Step=AcceptedCount+1. No need to check MaxCount here. }
          Accept := (Item.Step > TGrammarGreedyRepeater(Item.Rule).MinCount);
          { assign or reset result }
          if not Accept then
            ParseTree.Tree.Count := Item.ResIndex
          else
          begin
            ResIndex := Item.ResIndex;
            Len := Item.Len;
            ParseTree.Tree.Items[ResIndex].Position.Len := Len;
          end;
          Data.Position := Item.Start;
          Stack.DeleteLast;
        end;

      gtNot:
        if Item.Step=0 then
        begin
          Inc(Item.Step);
          Stack.Items[Stack.Add].SetUp(TGrammarNot(Item.Rule).Op.Data);
        end
        else
        begin
          Accept := not Accept;
          if Accept then
          begin
            Len := 0;
            ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[ResIndex].SetUp(Item.Rule, Data.Position,0);
          end;
          Stack.DeleteLast;
        end;

      gtEOF:
        begin
          Accept := Data.EOF;
          if Accept then
          begin
            Len := 0;
            ResIndex := ParseTree.Tree.Add;
            ParseTree.Tree.Items[ResIndex].SetUp(Item.Rule, Data.Position,0);
          end;
          Stack.DeleteLast;
        end;

    end; { case Item.Rule.GrammarType of }
  until Stack.Count=0;
  result := Accept;
  if result then
    ParseTree.Root := ResIndex
  else
    ParseTree.Clear;
end;

end.
