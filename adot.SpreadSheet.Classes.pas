unit adot.SpreadSheet.Classes;

interface

uses
  adot.SpreadSheet.Types,
  adot.Tools,
  adot.Strings,
  adot.Collections,
  adot.Variants,
  Vcl.Dialogs,
  System.Math,
  System.StrUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.UITypes,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Variants;

type
  TXLSSheet = class;

  TXLSCell = class
  private
  protected
    type
      TValue = record
        case TXLSValueType of
          xevtNull     : ();
          xevtBoolean  : (ValueBoolean  : boolean);
          xevtInteger  : (ValueInteger  : int64);
          xevtFloat    : (ValueDouble   : double);
          xevtCurrency : (ValueCurrency : currency);
          xevtDateTime : (ValueDateTime : TDateTime);
          xevtDate     : (ValueDate     : TDate);
          xevtString   : (); {FValueString}
          xevtFormula  : (); {FValueString}
      end;

    var
      FValueType: TXLSValueType;
      FValueString: string; { xevtString / xevtFormula}
      FValueOther: TValue;
      FPos: TXLSPos;
      FPreserveStringType: boolean;
      FConsiderWhenApplyBestFit: boolean;
      FDisplayFormat: TXLSDisplayFormat;
      FOwner: TXLSSheet;

    function GetAsCurrency: Currency;
    function GetAsDateTime: TDateTime;
    function GetAsDate: TDate;
    function GetAsFloat: double;
    function GetAsInteger: int64;
    function GetAsString: String;
    function GetAsFormula: String;
    procedure SetAsCurrency(const Value: Currency);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsDate(const Value: TDate);
    procedure SetAsFloat(const Value: double);
    procedure SetAsInteger(const Value: int64);
    procedure SetAsString(const Value: String);
    procedure SetAsFormula(const Value: String);
    function GetAsBoolean: boolean;
    procedure SetAsBoolean(const Value: boolean);
    function GetEmpty: Boolean;
    procedure CheckValueType(AValueType: TXLSValueType);
    function GetIsNull: Boolean;
    procedure SetIsNull(const Value: Boolean);
    procedure SetAsVarRec(const Value: TVarRec);
    function GetAsVariant: variant;
    procedure SetAsVariant(const Value: variant);

  public
    Style: TXLSCellStyle;

    constructor Create(AOwner: TXLSSheet; const APos: TXLSPos);

    property Sheet: TXLSSheet read FOwner;
    { Assigned automatically when value is written by AsInteger/AsFloat etc. }
    property ValueType: TXLSValueType read FValueType;
    { Some value types can be shown differently (datetime as date, float as currency etc). }
    property DisplayFormat: TXLSDisplayFormat read FDisplayFormat write FDisplayFormat;

    { Access to stored value of the cell. }
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: int64 read GetAsInteger write SetAsInteger;
    property AsFloat: double read GetAsFloat write SetAsFloat;
    property AsCurrency: Currency read GetAsCurrency write SetAsCurrency;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsDate: TDate read GetAsDate write SetAsDate;
    property AsString: String read GetAsString write SetAsString;
    property AsFormula: String read GetAsFormula write SetAsFormula;
    property AsVariant: variant read GetAsVariant write SetAsVariant;
    property AsVarRec: TVarRec write SetAsVarRec;

    { Returns True if there is no value assigned/no changes in style registered. }
    property Empty: boolean read GetEmpty;
    { True: all strings will be displayed as strings (disable automatic conversion to date/float and other formats). }
    property PreserveStringType: boolean read FPreserveStringType write FPreserveStringType;
    property Pos: TXLSPos read FPos;
    property ConsiderWhenApplyBestFit: Boolean read FConsiderWhenApplyBestFit write FConsiderWhenApplyBestFit;
    property IsNull: Boolean read GetIsNull write SetIsNull;
  end;

  TXLSCellEnumerator = TMapClass<TXLSPos, TXLSCell>.TPairEnumerator;

  TXLSBook = class;

  TXLSSheet = class
  public
    type
      TOptionsPrint = class
      protected
        FFitToPagesWide: integer;
        FFitToPagesTall: integer;
      public

        { limit the number of pages on which all the worksheet content is printed }
        property FitToPagesWide: integer read FFitToPagesWide write FFitToPagesWide;
        property FitToPagesTall: integer read FFitToPagesTall write FFitToPagesTall;
      end;

      TOptionsView = class
      protected
        FFrozenRows: integer;
        FFrozenColumns: integer;
        FVisible: boolean;
      public
        constructor Create;

        { number of not scrollable columns/rows }
        property FrozenRows: integer read FFrozenRows write FFrozenRows;
        property FrozenColumns: integer read FFrozenColumns write FFrozenColumns;
        property Visible: boolean read FVisible write FVisible;
      end;

  private
    function GetAsVariantArray: variant;
    procedure SetAsVariantArray(const Value: variant);
    function GetColumnsArray: TArray<integer>;
    function GetRowsArray: TArray<integer>;
    function GetColumnCount: integer;
    function GetRowCount: integer;
    function GetDataRect: TRect;

  protected
    FOwner: TXLSBook;
    FSheetName: string;
    FCells: TMap<TXLSPos, TXLSCell>;
    FColumnsSet, FRowsSet: TSet<integer>;
    FColumns, FRows: TArray<integer>;
    FMerge: TSet<TRect>;
    FApplyBestFit: TSet<integer>;
    FColWidth: TMap<integer, integer>;
    FRowHeight: TMap<integer, integer>;
    FSheetIndex: integer;
    FWriteCursor: TXLSPos; { used by WriteCell/WritelnCell functions }
    FHorOffset,FVertOffset: integer;
    FOptionsPrint: TOptionsPrint;
    FOptionsView: TOptionsView;

    procedure SetSheetName(const Value: string);
    function GetOrAddCell(Row,Col: integer): TXLSCell;
    function CreateCell(const P: TXLSPos): TXLSCell; virtual;
    function GetMergeRegionsCollection: TEnumerable<TRect>;
    function GetApplyBestFitColumns: TEnumerable<integer>;
    function GetApplyBestFit(Col: integer): Boolean;
    procedure SetApplyBestFit(Col: integer; const Value: Boolean);
    function GetCellAtCursorPos: TXLSCell; overload;
    function WriteVarArray(const Value: variant; FontStyle: TFontStyles = []; FontSize: integer = -1): TXLSCell;
    function GetColWidth(Col: integer): integer;
    function GetRowHeight(Row: integer): integer;
    procedure SetColWidth(Col: integer; const Value: integer);
    procedure SetRowHeight(Row: integer; const Value: integer);
    function GetColWidthCollection: TEnumerable<TPair<integer,integer>>;
    function GetRowHeightCollection: TEnumerable<TPair<integer,integer>>;
    function GetColCollection: TEnumerable<integer>;
    function GetRowCollection: TEnumerable<integer>;
  public
    constructor Create(AOwner: TXLSBook; AIndex: integer);
    destructor Destroy; override;

    { Enumerates all cells: For Cell in Sheet do <process Cell:TXLSCell> }
    function GetEnumerator: TXLSCellEnumerator;
    procedure DeleteCell(Row,Col: integer);
    function DeleteRow(Row: integer): integer;
    function DeleteCol(Col: integer): integer;

    { Create merged region. Use MergeRegions property to enumerate all merged regions. }
    procedure Merge(x1,y1,x2,y2: integer); overload;
    procedure Merge(R: TRect); overload;
    { Enumerates all regions created by .Merge : For R in Sheet.MergeRegions <do something with R> }
    property MergeRegionsCollection: TEnumerable<TRect> read GetMergeRegionsCollection;

    { Console-like methods, simplest way to output data. Write Null to skip cell.
      Check also .Defaults.WriteFormatFloat etc. }
    function Write(const Values: array of const; FontStyle: TFontStyles = []; FontSize: integer = -1): TXLSCell; overload;
    function Write: TXLSCell; overload;
    function Writeln(const Values: array of const; FontStyle: TFontStyles = []; FontSize: integer = -1): TXLSCell; overload;
    procedure Writeln; overload;

    property Book: TXLSBook read FOwner;

    { used by Write/Writeln functions }
    property WriteCursor: TXLSPos read FWriteCursor write FWriteCursor;

    { Access to values and style options of cells. Cells are created on demand, no need to create them directly. }
    property Cells[Row,Col: integer]: TXLSCell read GetOrAddCell; default;

    { Check also Owner.CellDefaults.ConsiderWhenApplyBestFit (default values for new cells). }
    property ApplyBestFit[Col: integer]: Boolean read GetApplyBestFit write SetApplyBestFit;
    { Enumerates all columns with BetFit=True : For Col in Sheet.ApplyBestFitColumns do <process column Col> }
    property ApplyBestFitCollection: TEnumerable<integer> read GetApplyBestFitColumns;
    procedure ApplyBestFitForAllColumns;

    { Default value: -1 (do not assign custom width). Ignored if ApplyBestFit[Col]=True. }
    property ColWidth[Col: integer]: integer read GetColWidth write SetColWidth;
    { Enumerates pairs [Col;Width] assigned by ColWidth property. }
    property ColWidthCollection: TEnumerable<TPair<integer,integer>> read GetColWidthCollection;
    property Columns: TArray<integer> read GetColumnsArray;
    property ColumnCount: integer read GetColumnCount;
    { Enumerates all columns. }
    property ColumnsCollection: TEnumerable<integer> read GetColCollection;
    { Default value: -1 (do not assign custom height). }
    property RowHeight[Row: integer]: integer read GetRowHeight write SetRowHeight;
    property RowHeightCollection: TEnumerable<TPair<integer,integer>> read GetRowHeightCollection;
    { Enumerates all rows. }
    property RowsCollection: TEnumerable<integer> read GetRowCollection;
    property Rows: TArray<integer> read GetRowsArray;
    property RowCount: integer read GetRowCount;

    { all output will be shifter by HorOffset/VertOffset }
    property HorOffset: integer read FHorOffset write FHorOffset;
    property VertOffset: integer read FVertOffset write FVertOffset;

    { Unique name of the sheet, can be shown as caption. }
    property SheetName: string read FSheetName write SetSheetName;
    { Index of the sheet in TXLSBook.Sheets }
    property SheetIndex: integer read FSheetIndex;

    property OptionsPrint: TOptionsPrint read FOptionsPrint;
    property OptionsView: TOptionsView read FOptionsView;

    { get rect (MinColumn,MinRow)-(MaxColumn,MaxRow) }
    property DataRect: TRect read GetDataRect;

    property AsVariantArray: variant read GetAsVariantArray write SetAsVariantArray;
  end;

  { Basic class to import/export (or both). Use CreateExporter from SpreadSheet.Export.pas to create instance. }
  TXLSBook = class abstract
  private
  protected
    FAutoFreeCollection: TAutoFreeCollection;
    FSheetMap: TMap<string, TXLSSheet>;
    FSheets: TObjectList<TXLSSheet>;
    FCellDefaults: TXLSCellDefaults;
    FActiveSheetIndex: integer;
    FPassword: string;

    { Must be implemented by descendents }
    class function DoIsPasswordProtected(const SpreadsheetFileName: string): boolean; virtual; abstract;
    class function DoIsValidPassword(const SpreadsheetFileName,Password: string): boolean; virtual; abstract;
    procedure DoSaveToStream(Dst: TStream; const FileType: string = '.xlsx'); virtual; abstract;
    procedure DoLoadFromStream(Src: TStream); virtual; abstract;

    procedure DoSaveToFile(const FileName: string; ShowSaveDialog: Boolean); virtual;
    procedure DoLoadFromFile(const FileName: string); virtual;
    function CreateSheet(AIndex: integer): TXLSSheet; virtual;
    function GetSaveDialog(const FileNameNoExt: string): TSaveDialog; virtual;
    procedure RenameSheet(Sheet: TXLSSheet; const OldName,NewName: string);
    procedure CheckSheetName(const NewName: string);
    function GetSheetByIndex(Index: integer): TXLSSheet;
    function GetSheetByName(const SheetName: string): TXLSSheet;
    function GetSheetCount: integer;
    procedure SetActiveSheetIndex(const Value: integer);
    function GetActiveWorksheet: TXLSSheet;
  public
    constructor Create; virtual;

    procedure Clear; virtual;
    function SheetExists(const SheetName: string): Boolean;
    function AddSheet(const SheetName: string): TXLSSheet;
    procedure RemoveSheet(const SheetName: string);

    procedure SaveToStream(Dst: TStream; const FileType: string = '.xlsx');
    procedure SaveToFile(const FileName: string; ShowSaveDialog: Boolean);
    procedure LoadFromStream(Src: TStream);
    procedure LoadFromFile(const FileName: string);

    class function IsPasswordProtected(const SpreadsheetFileName: string): boolean;
    class function IsValidPassword(const SpreadsheetFileName,Password: string): boolean;

    property SheetCount: integer read GetSheetCount;
    property Sheets[Index: integer]:TXLSSheet read GetSheetByIndex; default;
    property SheetByName[const Name: string]:TXLSSheet read GetSheetByName;
    property ActiveSheetIndex: integer read FActiveSheetIndex write SetActiveSheetIndex;
    property ActiveWorksheet: TXLSSheet read GetActiveWorksheet;
    property Password: string read FPassword write FPassword;

    property CellDefaults: TXLSCellDefaults read FCellDefaults;
  end;

  TXlsFactory = class
  protected
    function DoNewBook: TXLSBook; virtual; abstract;
  public
    function NewBook: TXLSBook;
  end;

implementation

{ TXLSCell }

constructor TXLSCell.Create(AOwner: TXLSSheet; const APos: TXLSPos);
var
  Def: TXLSCellDefaults;
begin
  inherited Create;
  FOwner                   := AOwner;
  FPos                     := APos;
  Def                      := FOwner.FOwner.CellDefaults;
  Style                    := Def.Style;
  PreserveStringType       := Def.PreserveStringType;
  ConsiderWhenApplyBestFit := Def.ConsiderWhenApplyBestFit;
end;

function TXLSCell.GetEmpty: Boolean;
begin
  result := (ValueType=xevtNull) and Style.Empty;
end;

function TXLSCell.GetIsNull: Boolean;
begin
  result := (ValueType=xevtNull);
end;

function TXLSCell.GetAsBoolean: boolean;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := False;
    xevtBoolean  : result := FValueOther.ValueBoolean;
    xevtInteger  : result := FValueOther.ValueInteger<>0;
    xevtFloat    : result := FValueOther.ValueDouble<>0;
    xevtCurrency : result := FValueOther.ValueCurrency<>0;
    xevtDateTime : result := FValueOther.ValueDateTime<>0;
    xevtDate     : result := FValueOther.ValueDate<>0;
    xevtString   : result := (FValueString<>'0') and not SameText(FValueString.Substring(0,1),'N');
    xevtFormula  : result := (FValueString<>'0') and not SameText(FValueString.Substring(0,1),'N');
    else raise Exception.Create('Error');
  end;
end;

function TXLSCell.GetAsCurrency: Currency;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := 0;
    xevtBoolean  : result := Byte(FValueOther.ValueBoolean);
    xevtInteger  : result := FValueOther.ValueInteger;
    xevtFloat    : result := FValueOther.ValueDouble;
    xevtCurrency : result := FValueOther.ValueCurrency;
    xevtDateTime : result := FValueOther.ValueDateTime;
    xevtDate     : result := FValueOther.ValueDate;
    xevtString   : result := StrToCurrDef(FValueString, 0);
    xevtFormula  : result := StrToCurrDef(FValueString, 0);
    else raise Exception.Create('Error');
  end;
end;

function TXLSCell.GetAsDateTime: TDateTime;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := 0;
    xevtBoolean  : result := Byte(FValueOther.ValueBoolean);
    xevtInteger  : result := FValueOther.ValueInteger;
    xevtFloat    : result := FValueOther.ValueDouble;
    xevtCurrency : result := FValueOther.ValueCurrency;
    xevtDateTime : result := FValueOther.ValueDateTime;
    xevtDate     : result := FValueOther.ValueDate;
    xevtString   : result := StrToDateTimeDef(FValueString, 0);
    xevtFormula  : result := StrToDateTimeDef(FValueString, 0);
    else raise Exception.Create('Error');
  end;
end;

function TXLSCell.GetAsDate: TDate;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := 0;
    xevtBoolean  : result := Byte(FValueOther.ValueBoolean);
    xevtInteger  : result := FValueOther.ValueInteger;
    xevtFloat    : result := FValueOther.ValueDouble;
    xevtCurrency : result := FValueOther.ValueCurrency;
    xevtDateTime : result := FValueOther.ValueDateTime;
    xevtDate     : result := FValueOther.ValueDate;
    xevtString   : result := StrToDateDef(FValueString, 0);
    xevtFormula  : result := StrToDateDef(FValueString, 0);
    else raise Exception.Create('Error');
  end;
end;

function TXLSCell.GetAsFloat: double;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := 0;
    xevtBoolean  : result := Byte(FValueOther.ValueBoolean);
    xevtInteger  : result := FValueOther.ValueInteger;
    xevtFloat    : result := FValueOther.ValueDouble;
    xevtCurrency : result := FValueOther.ValueCurrency;
    xevtDateTime : result := FValueOther.ValueDateTime;
    xevtDate     : result := FValueOther.ValueDate;
    xevtString   : result := StrToFloatDef(FValueString, 0);
    xevtFormula  : result := StrToFloatDef(FValueString, 0);
    else raise Exception.Create('Error');
  end;
end;

function TXLSCell.GetAsInteger: int64;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := 0;
    xevtBoolean  : result := Byte(FValueOther.ValueBoolean);
    xevtInteger  : result := FValueOther.ValueInteger;
    xevtFloat    : result := Round(FValueOther.ValueDouble);
    xevtCurrency : result := Round(FValueOther.ValueCurrency);
    xevtDateTime : result := Round(FValueOther.ValueDateTime);
    xevtDate     : result := Round(FValueOther.ValueDate);
    xevtString   : result := StrToInt64Def(FValueString, 0);
    xevtFormula  : result := StrToInt64Def(FValueString, 0);
    else raise Exception.Create('Error');
  end;
end;

function TXLSCell.GetAsString: String;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := '';
    xevtBoolean  : result := IfThen(FValueOther.ValueBoolean, 'True', 'False');
    xevtInteger  : result := FValueOther.ValueInteger.ToString;
    xevtFloat    : result := FValueOther.ValueDouble.ToString;
    xevtCurrency : result := CurrToStr(FValueOther.ValueCurrency);
    xevtDateTime : result := DateTimeToStr(FValueOther.ValueDateTime);
    xevtDate     : result := DateToStr(FValueOther.ValueDate);
    xevtString   : result := FValueString;
    xevtFormula  : result := FValueString;
    else raise Exception.Create('Error');
  end;
end;

function TXLSCell.GetAsFormula: String;
begin
  result := GetAsString;
end;

function TXLSCell.GetAsVariant: variant;
begin
  {$IF [Low(ValueType)..High(ValueType)]<>[xevtNull,xevtBoolean,xevtInteger,xevtFloat,xevtCurrency,xevtDateTime,xevtDate,xevtString,xevtFormula]}
    {$Message Error 'Support of new type must be implemented here'}
  {$ENDIF}
  case ValueType of
    xevtNull     : result := Null;
    xevtBoolean  : result := FValueOther.ValueBoolean;
    xevtInteger  : result := FValueOther.ValueInteger;
    xevtFloat    : result := FValueOther.ValueDouble;
    xevtCurrency : result := FValueOther.ValueCurrency;
    xevtDateTime : result := FValueOther.ValueDateTime;
    xevtDate     : result := FValueOther.ValueDate;
    xevtString   : result := FValueString;
    xevtFormula  : result := FValueString;
    else raise Exception.Create('Error');
  end;
end;

procedure TXLSCell.SetAsBoolean(const Value: boolean);
begin
  FValueOther.ValueBoolean := Value;
  FValueString := '';
  CheckValueType(xevtBoolean);
end;

procedure TXLSCell.SetAsCurrency(const Value: Currency);
begin
  FValueOther.ValueCurrency := Value;
  FValueString := '';
  CheckValueType(xevtCurrency);
end;

procedure TXLSCell.SetAsDateTime(const Value: TDateTime);
begin
  FValueOther.ValueDateTime := Value;
  FValueString := '';
  CheckValueType(xevtDateTime);
end;

procedure TXLSCell.SetAsDate(const Value: TDate);
begin
  FValueOther.ValueDate := Value;
  FValueString := '';
  CheckValueType(xevtDate);
end;

procedure TXLSCell.SetAsFloat(const Value: double);
begin
  FValueOther.ValueDouble := Value;
  FValueString := '';
  CheckValueType(xevtFloat);
end;

procedure TXLSCell.SetAsInteger(const Value: int64);
begin
  FValueOther.ValueInteger := Value;
  FValueString := '';
  CheckValueType(xevtInteger);
end;

procedure TXLSCell.SetAsString(const Value: String);
var
  ForcePreserveString: boolean;
begin
  ForcePreserveString := Value.StartsWith('''');
  if ForcePreserveString then
    FValueString := Value.Substring(1)
  else
    FValueString := Value;
  CheckValueType(xevtString);
  PreserveStringType := ForcePreserveString;
end;

procedure TXLSCell.SetAsFormula(const Value: String);
begin
  FValueString := Value;
  CheckValueType(xevtFormula);
end;

procedure TXLSCell.SetAsVariant(const Value: variant);
begin
  case VarType(Value) of
    varSmallInt : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varInteger  : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varSingle   : AsFloat    := TVar.ToFloatDef(Value, 0);
    varDouble   : AsFloat    := TVar.ToFloatDef(Value, 0);
    varCurrency : AsCurrency := TVar.ToCurrencyDef(Value, 0);
    varDate     : AsDateTime := TVar.ToDateTimeDef(Value, 0);
    varOleStr   : AsString   := TVar.ToStringDef(Value, '');
    varDispatch : IsNull     := True;
    varError    : IsNull     := True;
    varBoolean  : AsBoolean  := TVar.ToBooleanDef(Value, False);
    varUnknown  : IsNull     := True;
    varShortInt : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varByte     : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varWord     : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varLongWord : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varInt64    : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varUInt64   : AsInteger  := TVar.ToIntegerDef(Value, 0);
    varString   : AsString   := TVar.ToStringDef(Value, '');
    varAny      : IsNull     := True;
    varArray    : IsNull     := True;
    varByRef    : IsNull     := True;
    varUString  : AsString   := TVar.ToStringDef(Value, '');
    varRecord   : IsNull     := True;
  end;
end;

procedure TXLSCell.SetAsVarRec(const Value: TVarRec);
begin
  case Value.VType of
    vtInteger       : AsInteger := Value.VInteger;
    vtBoolean       : AsBoolean := Value.VBoolean;
    vtChar          : AsString  := Char(Value.VChar);
    vtExtended      : AsFloat   := Value.VExtended^;
    {$IFNDEF NEXTGEN}
    vtString        : AsString  := String(Value.VString^);
    {$ENDIF NEXTGEN}
    vtPointer       : IsNull    := True;
    vtPChar         : AsString  := Char(Value.VPChar^);
    vtObject        : IsNull    := True;
    vtClass         : IsNull    := True;
    vtWideChar      : AsString  := Value.VWideChar;
    vtPWideChar     : AsString  := Value.VPWideChar^;
    vtAnsiString    : AsString  := UnicodeString(AnsiString(Value.VAnsiString));
    vtCurrency      : AsFloat   := Value.VCurrency^;
    vtVariant       : AsVariant := Value.VVariant^;
    vtInterface     : IsNull    := True;
    vtWideString    : AsString  := WideString(Value.VWideString);
    vtInt64         : AsInteger := Value.VInt64^;
    vtUnicodeString : AsString  := String(Value.VUnicodeString); // !!!
  end;
end;

procedure TXLSCell.SetIsNull(const Value: Boolean);
begin
  FillChar(FValueOther, SizeOf(FValueOther), 0);
  FValueString := '';
  CheckValueType(xevtNull);
end;

procedure TXLSCell.CheckValueType(AValueType: TXLSValueType);
begin
  if AValueType<>ValueType then
  begin
    FValueType := AValueType;
    if (Sheet <> nil) and (Sheet.Book <> nil) then
      DisplayFormat := Sheet.Book.CellDefaults.Format[ValueType];
  end;
end;

{ TXLSSheet }

function TXLSSheet.GetApplyBestFit(Col: integer): Boolean;
begin
  result := Col in FApplyBestFit;
end;

procedure TXLSSheet.SetApplyBestFit(Col: integer; const Value: Boolean);
begin
  if Value then
    FApplyBestFit.Add(Col)
  else
    FApplyBestFit.Remove(Col);
end;

procedure TXLSSheet.ApplyBestFitForAllColumns;
var
  I: integer;
  ColArr: TArray<integer>;
begin
  ColArr := Columns;
  for I := 0 to High(ColArr) do
    ApplyBestFit[ColArr[I]] := True;
end;

function TXLSSheet.GetAsVariantArray: variant;
var
  RowArr,ColArr: TArray<integer>;
  Row,Col: Integer;
begin
  RowArr := Rows;
  ColArr := Columns;
  Result := VarArrayCreate([Low(RowArr),High(RowArr), Low(ColArr),High(ColArr)], varVariant);
  for Row := Low(RowArr) to High(RowArr) do
    for Col := Low(ColArr) to High(ColArr) do
      Result[Row,Col] := Cells[RowArr[Row], ColArr[Col]].AsVariant;
end;

procedure TXLSSheet.SetAsVariantArray(const Value: variant);
var
  Row,Col: Integer;
begin
  for Row := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
    for Col := VarArrayLowBound(Value, 2) to VarArrayHighBound(Value, 2) do
      Cells[Row,Col].AsVariant := Value[Row, Col];
end;

function TXLSSheet.GetColCollection: TEnumerable<integer>;
begin
  result := FColumnsSet.Collection;
end;

function TXLSSheet.GetColumnCount: integer;
begin
  result := FColumnsSet.Count;
end;

function TXLSSheet.GetColumnsArray: TArray<integer>;
begin
  { We only extend FColumnsSet, it is enough to check number of items to compare FColumns and FColumnsSet. }
  if FColumnsSet.Count <> Length(FColumns) then
  begin
    FColumns := FColumnsSet.Collection.ToArray;
    TArray.Sort<integer>(FColumns);
  end;
  result := FColumns;
end;

function TXLSSheet.GetRowsArray: TArray<integer>;
begin
  { We only extend FRowsSet, it is enough to check number of items to compare FRows and FRowsSet. }
  if FRowsSet.Count <> Length(FRows) then
  begin
    FRows := FRowsSet.Collection.ToArray;
    TArray.Sort<integer>(FRows);
  end;
  result := FRows;
end;

function TXLSSheet.GetRowCollection: TEnumerable<integer>;
begin
  result := FRowsSet.Collection;
end;

function TXLSSheet.GetRowCount: integer;
begin
  result := FRowsSet.Count;
end;

function TXLSSheet.GetColWidth(Col: integer): integer;
begin
  if not FColWidth.TryGetValue(Col, result) then
    result := -1;
end;

function TXLSSheet.GetRowHeight(Row: integer): integer;
begin
  if not FRowHeight.TryGetValue(Row, result) then
    result := -1;
end;

function TXLSSheet.GetColWidthCollection: TEnumerable<TPair<integer,integer>>;
begin
  result := FColWidth.Collection;
end;

function TXLSSheet.GetDataRect: TRect;
var
  I: integer;
begin
  result := TRect.Create(High(integer),High(integer),Low(integer),Low(integer));
  for I in ColumnsCollection do
  begin
    result.Left := Min(I, result.Left);
    result.Right := Max(I, result.Right);
  end;
  for I in RowsCollection do
  begin
    result.Top := Min(I, result.Top);
    result.Bottom := Max(I, result.Bottom);
  end;
  if (Result.Right < Result.Left) or (Result.Bottom < Result.Top) then
    Result := TRect.Create(0,0,-1,-1);
end;

function TXLSSheet.GetRowHeightCollection: TEnumerable<TPair<integer,integer>>;
begin
  result := FRowHeight.Collection;
end;

procedure TXLSSheet.SetColWidth(Col: integer; const Value: integer);
begin
  if Value < 0 then
    FColWidth.Remove(Col)
  else
    FColWidth.AddOrSetValue(Col, Value);
end;

procedure TXLSSheet.SetRowHeight(Row: integer; const Value: integer);
begin
  if Value < 0 then
    FRowHeight.Remove(Row)
  else
    FRowHeight.AddOrSetValue(Row, Value);
end;

function TXLSSheet.GetEnumerator: TXLSCellEnumerator;
begin
  result := FCells.GetEnumerator;
end;

function TXLSSheet.GetMergeRegionsCollection: TEnumerable<TRect>;
begin
  result := FMerge.Collection;
end;

function TXLSSheet.GetApplyBestFitColumns: TEnumerable<integer>;
begin
  result := FApplyBestFit.Collection;
end;

function TXLSSheet.GetCellAtCursorPos: TXLSCell;
begin
  result := Cells[WriteCursor.Row, WriteCursor.Col];
end;

function TXLSSheet.GetOrAddCell(Row,Col: integer): TXLSCell;
var
  P: TXLSPos;
begin
  P := TXLSPos.Create(Row, Col);
  if not FCells.TryGetValue(P, Result) then
  begin
    Result := CreateCell(P);
    FCells.Add(P, Result);
    FColumnsSet.Add(P.Col);
    FRowsSet.Add(P.Row);
  end;
end;

constructor TXLSSheet.Create(AOwner: TXLSBook; AIndex: integer);
begin
  inherited Create;
  FOptionsPrint := TOptionsPrint.Create;
  FOptionsView := TOptionsView.Create;
  FOwner := AOwner;
  FSheetIndex := AIndex;
  FCells.Clear;
  FCells.OwnsKeys := False;
  FCells.OwnsValues := True;
  FMerge.Clear;
  FApplyBestFit.Clear;
  FColWidth.Clear;
  FRowHeight.Clear;
  FColumnsSet.Clear;
  SetLength(FColumns, 0);
  FRowsSet.Clear;
  SetLength(FRows, 0);
end;

function TXLSSheet.CreateCell(const P: TXLSPos): TXLSCell;
begin
  result := TXLSCell.Create(Self, P);
end;

procedure TXLSSheet.DeleteCell(Row, Col: integer);
begin
  FCells.Remove(TXLSPos.Create(Row, Col));
end;

function TXLSSheet.DeleteCol(Col: integer): integer;
var
  P: TXLSPos;
  S: TVector<TXLSPos>;
begin
  S.Clear;
  for P in FCells.Keys do
    if P.Col = Col then
      S.Add(P);
  S.TrimExcess;
  FCells.Remove(S.Items);
  result := S.Count;
end;

function TXLSSheet.DeleteRow(Row: integer): integer;
var
  P: TXLSPos;
  S: TVector<TXLSPos>;
begin
  S.Clear;
  for P in FCells.Keys do
    if P.Row = Row then
      S.Add(P);
  S.TrimExcess;
  FCells.Remove(S.Items);
  result := S.Count;
end;

destructor TXLSSheet.Destroy;
begin
  FreeAndNil(FOptionsPrint);
  FreeAndNil(FOptionsView);
  inherited;
end;

procedure TXLSSheet.Merge(x1, y1, x2, y2: integer);
begin
  Merge(TRect.Create(x1,y1,x2,y2));
end;

procedure TXLSSheet.Merge(R: TRect);
begin
  FMerge.Add(R);
end;

procedure TXLSSheet.SetSheetName(const Value: string);
begin
  FOwner.RenameSheet(Self, FSheetName, Value);
  FSheetName := Value;
end;

function TXLSSheet.Write(const Values: array of const; FontStyle: TFontStyles = []; FontSize: integer = -1): TXLSCell;
var
  I: Integer;
begin
  Result := nil;
  for I := Low(Values) to High(Values) do
    { we have separate routine to output array }
    if (Values[I].VType=vtVariant) and VarIsArray(Values[I].VVariant^) then
      Result := WriteVarArray(Values[I].VVariant^, FontStyle, FontSize)
    else
    { create and setup cells }
    begin
      Result := Write;
      Result.AsVarRec := Values[I];
      if FontStyle<>[] then Result.Style.Font.Style := FontStyle;
      if FontSize > 0 then Result.Style.Font.Size := FontSize;
    end;
end;

function TXLSSheet.WriteVarArray(const Value: variant; FontStyle: TFontStyles = []; FontSize: integer = -1): TXLSCell;
var
  Col,Row,X,N: Integer;
begin
  Result := nil;
  if VarIsArray(Value) then
    case VarArrayDimCount(Value) of
      1:
        for Col := VarArrayLowBound(Value,1) to VarArrayHighBound(Value,1) do
          Result := Write([Value[Col]], FontStyle, FontSize);
      2:
        begin
          X := FWriteCursor.Col;
          N := VarArrayHighBound(Value,1);
          for Row := VarArrayLowBound(Value,1) to N do
          begin
            for Col := VarArrayLowBound(Value,2) to VarArrayHighBound(Value,2) do
              Result := Write([Value[Row, Col]]);
            if Row < N then
            begin
              inc(FWriteCursor.Row);
              FWriteCursor.Col := X;
            end;
          end;
        end;
    end;
end;

function TXLSSheet.Write: TXLSCell;
begin
  Result := GetCellAtCursorPos;
  inc(FWriteCursor.Col);
end;

function TXLSSheet.Writeln(const Values: array of const; FontStyle: TFontStyles = []; FontSize: integer = -1): TXLSCell;
begin
  Result := Write(Values, FontStyle, FontSize);
  inc(FWriteCursor.Row);
  FWriteCursor.Col := 0;
end;

procedure TXLSSheet.Writeln;
begin
  inc(FWriteCursor.Row);
  FWriteCursor.Col := 0;
end;

{ TXLSBook }

constructor TXLSBook.Create;
begin
  inherited Create;
  FSheets := FAutoFreeCollection.Add( TObjectList<TXLSSheet>.Create );
  FCellDefaults := FAutoFreeCollection.Add( TXLSCellDefaults.Create );
  Clear;
end;

procedure TXLSBook.Clear;
begin
  FSheetMap.Clear;
  FSheets.Clear;
  FCellDefaults.Clear;
  FActiveSheetIndex := -1;
end;

function TXLSBook.CreateSheet(AIndex: integer): TXLSSheet;
begin
  result := TXLSSheet.Create(Self, AIndex);
end;

function TXLSBook.AddSheet(const SheetName: string): TXLSSheet;
begin
  CheckSheetName(SheetName);
  Result := CreateSheet(FSheets.Count);
  FSheets.Add(Result);
  Result.SheetName := SheetName; { will add to FSheetMap }
end;

function TXLSBook.GetSheetByIndex(Index: integer): TXLSSheet;
begin
  result := FSheets[Index];
end;

function TXLSBook.GetSheetByName(const SheetName: string): TXLSSheet;
begin
  if not FSheetMap.TryGetValue(SheetName, Result) then
    raise Exception.Create(format('Sheet "%s" does not exist', [SheetName]));
end;

function TXLSBook.GetSheetCount: integer;
begin
  result := FSheets.Count;
end;

class function TXLSBook.IsPasswordProtected(const SpreadsheetFileName: string): boolean;
begin
  result := DoIsPasswordProtected(SpreadsheetFileName);
end;

class function TXLSBook.IsValidPassword(const SpreadsheetFileName, Password: string): boolean;
begin
  result := DoIsValidPassword(SpreadsheetFileName, Password);
end;

function TXLSBook.SheetExists(const SheetName: string): Boolean;
begin
  result := FSheetMap.ContainsKey(SheetName);
end;

procedure TXLSBook.RemoveSheet(const SheetName: string);
var
  Sheet: TXLSSheet;
begin
  Sheet := SheetByName[SheetName];
  FSheets.Remove(Sheet);
  FSheetMap.Remove(SheetName);
end;

procedure TXLSBook.RenameSheet(Sheet: TXLSSheet; const OldName, NewName: string);
begin
  if not TStr.SameText(OldName, NewName) then
    CheckSheetName(NewName);
  FSheetMap.ExtractPair(OldName);
  FSheetMap.Add(NewName, Sheet);
end;

function TXLSBook.GetSaveDialog(const FileNameNoExt: string): TSaveDialog;
begin
  Result := TSaveDialog.Create(nil);
  Result.DefaultExt := '.xlsx';
  Result.Filter := 'Excel 97-2003-arbeidsbok|*.xls|Excel-arbeidsbok|*.xlsx';
  Result.FilterIndex := 2;
  Result.Options := Result.Options + [ofOverwritePrompt, ofPathMustExist];
  Result.FileName := FileNameNoExt;
end;

procedure TXLSBook.DoSaveToFile(const FileName: string; ShowSaveDialog: Boolean);
var
  M: TMemoryStream;
  D: TSaveDialog;
  S: string;
begin
  M := TMemoryStream.Create;
  try
    if ShowSaveDialog then D := GetSaveDialog(ChangeFileExt(FileName, ''))
      else D := nil;
    try
      if not ShowSaveDialog then
        S := FileName
      else
        if D.Execute then S := D.FileName
          else Exit;
      SaveToStream(M, Trim(ExtractFileExt(S)));
      M.SaveToFile(S);
    finally
      FreeAndNil(D);
    end;
  finally
    M.Free;
  end;
end;

procedure TXLSBook.SaveToFile(const FileName: string; ShowSaveDialog: Boolean);
begin
  DoSaveToFile(FileName, ShowSaveDialog);
end;

procedure TXLSBook.SaveToStream(Dst: TStream; const FileType: string);
begin
  DoSaveToStream(Dst, FileType);
end;

procedure TXLSBook.SetActiveSheetIndex(const Value: integer);
begin
  Assert((FActiveSheetIndex >= -1) and (FActiveSheetIndex < SheetCount ));
  FActiveSheetIndex := Value;
end;

procedure TXLSBook.DoLoadFromFile(const FileName: string);
var
  M: TMemoryStream;
begin
  M := TMemoryStream.Create;
  try
    M.LoadFromFile(FileName);
    M.Position := 0;
    LoadFromStream(M);
  finally
    M.Free;
  end;
end;

procedure TXLSBook.LoadFromFile(const FileName: string);
begin
  DoLoadFromFile(FileName);
end;

procedure TXLSBook.LoadFromStream(Src: TStream);
begin
  DoLoadFromStream(Src);
end;

procedure TXLSBook.CheckSheetName(const NewName: string);
begin
  if SheetExists(NewName) then
    raise Exception.Create(format('Sab "%s" already exists', [NewName]));
end;

function TXLSBook.GetActiveWorksheet: TXLSSheet;
begin
  if ActiveSheetIndex >= 0 then
    result := Sheets[ActiveSheetIndex]
  else
    result := nil;
end;

{ TXLSSheet.TOptionsView }

constructor TXLSSheet.TOptionsView.Create;
begin
  FFrozenRows := -1;
  FFrozenColumns := -1;
  FVisible := True;
end;

{ TXlsFactory }

function TXlsFactory.NewBook: TXLSBook;
begin
  result := DoNewBook;
end;

end.
