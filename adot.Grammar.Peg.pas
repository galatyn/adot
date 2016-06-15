unit adot.Grammar.Peg;

{$DEFINE adot}

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
  {$IF Defined(adot)}
  adot.Log,
  {$ELSE}
  msLog,
  {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Character,
  System.Generics.Collections,
  System.Generics.Defaults;

type

  TPegParser = class(TGrammarParser)
  protected
    type
      PCallStackItem = ^TCallStackItem;
      TCallStackItem = record
        Rule: TGrammarClass;
        Step: integer;
        Len: integer;

        class function Create(ARule: TGrammarClass): TCallStackItem; static; inline;
      end;

  public
    function Accepted: Boolean; override;
    procedure LogParseTree; override;
  end;

implementation

{ TPegParser.TCallStackItem }

class function TPegParser.TCallStackItem.Create(ARule: TGrammarClass): TCallStackItem;
begin
  with result do
  begin
    Rule := ARule;
    Step := 0;
  end;
end;

{ TPegParser }

procedure TPegParser.LogParseTree;
begin
  AppLog.Log('');
  AppLog.Log('Input string:');
  AppLog.Log(TStr.GetPrintable(Data.Text));
  AppLog.Log('Parse tree:');
  ParseTree.LogTextInputParseTree(Data);
end;

function TPegParser.Accepted: Boolean;
var

  { stack of calls (to avoid deep recursion) }
  Stack: TVector<TCallStackItem>;

  { result of last operation on stack:
    Accept   - input accepted
    Len      - length of accepted block (undefined if Accept=False) }
  Len: integer;
  Accept: boolean;

  { pointer to current/new item on the stack (invalid after any modification of the stack) }
  Item: PCallStackItem;
begin
  Stack.Clear;
  ParseTree.Clear;
  Accept := False;
  Len := 0;
  Stack.Add(TCallStackItem.Create(Grammar));
  repeat
    Item := @Stack.Items[Stack.Count-1];
    case Item.Rule.GrammarType of

      gtUnknown:
        raise Exception.Create('rule is not initialized');

      gtLink:
        Item.Rule := TGrammarLink(Item.Rule).Op.Data;

      gtString:
        begin
          Len    := TGrammarString(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept and Item.Rule.IncludeIntoParseTree then
            ParseTree.Add(Item.Rule, Data.Position, Len);
          Stack.DeleteLast;
        end;

      gtChar:
        begin
          Len    := TGrammarChar(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept and Item.Rule.IncludeIntoParseTree then
            ParseTree.Add(Item.Rule, Data.Position, Len);
          Stack.DeleteLast;
        end;

      gtCharClass:
        begin
          Len    := TGrammarCharClass(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept and Item.Rule.IncludeIntoParseTree then
            ParseTree.Add(Item.Rule, Data.Position, Len);
          Stack.DeleteLast;
        end;

      gtCharSet:
        begin
          Len    := TGrammarCharSetClass(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept and Item.Rule.IncludeIntoParseTree then
            ParseTree.Add(Item.Rule, Data.Position, Len);
          Stack.DeleteLast;
        end;

      gtBytes:
        begin
          Len    := TGrammarBytesClass(Item.Rule).GetAcceptedBlock(Data);
          Accept := Len >= 0;
          if Accept and Item.Rule.IncludeIntoParseTree then
            ParseTree.Add(Item.Rule, Data.Position, Len);
          Stack.DeleteLast;
        end;

      gtSequence:
        case Item.Step of
          0: begin
               if Item.Rule.IncludeIntoParseTree then
                 ParseTree.Append;
               Inc(Item.Step);
               Stack.Add(TCallStackItem.Create(TGrammarSequence(Item.Rule).Op1.Data));
             end;
          1: if not Accept then
             begin
               if Item.Rule.IncludeIntoParseTree then
                 ParseTree.Rollback;
               Stack.DeleteLast;
             end
             else begin
               Inc(Item.Step);
               Item.Len := Len;
               Data.Position := Data.Position + Len;
               Stack.Add(TCallStackItem.Create(TGrammarSequence(Item.Rule).Op2.Data));
             end;
          2: begin
               Data.Position := Data.Position - Item.Len;
               if Accept then
               begin
                 Inc(Len, Item.Len);
                 if Item.Rule.IncludeIntoParseTree then
                   ParseTree.Commit(Item.Rule, Data.Position, Len);
               end
               else
                 if Item.Rule.IncludeIntoParseTree then
                   ParseTree.Rollback;
               Stack.DeleteLast;
             end;
        end;

      gtSelection:
        case Item.Step of
          0: begin
               if Item.Rule.IncludeIntoParseTree then
                 ParseTree.Append;
               Inc(Item.Step);
               Stack.Add(TCallStackItem.Create(TGrammarSelection(Item.Rule).Op1.Data));
             end;
          1: if Accept then begin
               if Item.Rule.IncludeIntoParseTree then
                 ParseTree.Commit(Item.Rule, Data.Position, Len);
               Stack.DeleteLast;
             end else begin
               Inc(Item.Step);
               Stack.Add(TCallStackItem.Create(TGrammarSelection(Item.Rule).Op2.Data));
             end;
          2: begin
               if Item.Rule.IncludeIntoParseTree then
                 if Accept then
                   ParseTree.Commit(Item.Rule, Data.Position, Len)
                 else
                   ParseTree.Rollback;
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
            Item.Len := 0;
            if Item.Rule.IncludeIntoParseTree then
              ParseTree.Append;
            Stack.Add(TCallStackItem.Create(TGrammarGreedyRepeater(Item.Rule).Op.Data));
          end
        else
        if Accept and (Item.Step < TGrammarGreedyRepeater(Item.Rule).MaxCount) then begin
          Inc(Item.Step);
          Inc(Item.Len, Len);
          Data.Position := Data.Position + Len;
          Stack.Add(TCallStackItem.Create(TGrammarGreedyRepeater(Item.Rule).Op.Data));
        end
        else begin
          { add last iteration to the result (if accepted) }
          if Accept then
          begin
            Inc(Item.Step);
            Inc(Item.Len, Len);
            Data.Position := Data.Position + Len;
          end;
          { ">" because Step=AcceptedCount+1. No need to check MaxCount here. }
          Accept := (Item.Step > TGrammarGreedyRepeater(Item.Rule).MinCount);
          { assign or reset result }
          Len := Item.Len;
          if Item.Rule.IncludeIntoParseTree then
            if Accept then
              ParseTree.Commit(Item.Rule, Data.Position - Len, Len)
            else
              ParseTree.Rollback;
          Data.Position := Data.Position - Len;
          Stack.DeleteLast;
        end;

      gtNot:
        if Item.Step=0 then
        begin
          Inc(Item.Step);
          Stack.Add(TCallStackItem.Create(TGrammarNot(Item.Rule).Op.Data));
        end
        else
        begin
          Accept := not Accept;
          if Accept then
          begin
            Len := 0;
            if Item.Rule.IncludeIntoParseTree then
              ParseTree.Add(Item.Rule, Data.Position, 0);
          end;
          Stack.DeleteLast;
        end;

      gtEOF:
        begin
          Accept := Data.EOF;
          if Accept then
          begin
            Len := 0;
            if Item.Rule.IncludeIntoParseTree then
              ParseTree.Add(Item.Rule, Data.Position, 0);
          end;
          Stack.DeleteLast;
        end;

    end; { case Item.Rule.GrammarType of }

  until Stack.Count=0;
  result := Accept;
  if result then ParseTree.Root := 0
    else ParseTree.Clear;
  ParseTree.Tree.Nodes.TrimExcess;
end;

end.
