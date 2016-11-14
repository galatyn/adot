unit adot.SpreadSheet.Types;
(*******************************************************************************
KortInfo    : Export/import framework for MS Excel and other.
Forfatter   : Andrei Halatsin
Laget dato  : 26.06.2016
Eier        :
Beskrivelse : Simple/basic data types.
Endringer   :
*******************************************************************************)

interface

uses
  adot.Tools,
  System.Types,
  System.UITypes,
  System.SysUtils;

type
  TXLSFont = record
    Name   : TBox<string>;
    Color  : TBox<TColor>;
    Style  : TBox<TFontStyles>;
    Height : TBox<Integer>;
    Size   : TBox<Integer>;

    procedure Clear;
    function Empty: Boolean;

    class operator Equal(ALeft, ARight: TXLSFont): Boolean; overload;
    class operator NotEqual(ALeft, ARight: TXLSFont): Boolean; overload;
  end;

  { All cells get ExportFormat matching the value type, but it can be changed manually. Some examples:
      - Data provided as array of doubles, but actually it is currency values.
      - Data provided as DateTime, but only Date should be displayed.
    In such and similar cases data format can be changed. }
  TXLSDisplayFormat = (dfDefault, dfCurrency, dfFloatNumber, dfIntNumber, dfDateTime, dfDate, dfBoolean, dfString);

  TXLSBorderStyle = (xebsDefault, xebsHair, xebsDotted, xebsDashDotDot, xebsDashDot, xebsDashed,
    xebsThin, xebsMediumDashDotDot, xebsSlantedDashDot, xebsMediumDashDot, xebsMediumDashed, xebsMedium,
    xebsThick, xebsDouble, xebsNone);

  TXLSBorderOptions = record
    Style: TBox<TXLSBorderStyle>;
    Color: TBox<TColor>;

    procedure Clear;
    function Empty: Boolean;

    class operator Equal(ALeft, ARight: TXLSBorderOptions): Boolean; overload;
    class operator NotEqual(ALeft, ARight: TXLSBorderOptions): Boolean; overload;
  end;

  TXLSAlignHorz = (xeahGeneral, xeahLeft, xeahCenter, xeahRight, xeahFill, xeahJustify, xeahDistributed);
  TXLSAlignVert = (xeavTop, xeavCenter, xeavBottom, xeavJustify, xeavDistributed);
  TXLSBorder = (xbLeft, xbTop, xbRight, xbBottom);

  TXLSCellStyle = record
    Font            : TXLSFont;
    Borders         : array[TXLSBorder] of TXLSBorderOptions;
    BackgroundColor : TBox<TColor>;
    AlignHorz       : TBox<TXLSAlignHorz>;
    AlignVert       : TBox<TXLSAlignVert>;

    procedure Clear;
    function Empty: Boolean;

    class operator Equal(ALeft, ARight: TXLSCellStyle): Boolean; overload;
    class operator NotEqual(ALeft, ARight: TXLSCellStyle): Boolean; overload;
  end;

  { simple envelop to keep TXLSCellStyle as object instance }
  TXLSCellStyleClass = class
    Style: TXLSCellStyle;
  end;

  TXLSValueType = (xevtNull, xevtBoolean, xevtInteger, xevtFloat, xevtCurrency, xevtDateTime, xevtDate, xevtString, xevtFormula);

  TXLSPos = record
    Row,Col: integer;

    constructor Create(ARow,ACol: integer);
  end;

  TXLSCellDefaults = class
  protected
    const
      MapDisplayFormat: array[TXLSValueType] of TXLSDisplayFormat = (
      { xevtNull,  xevtBoolean, xevtInteger, xevtFloat,     xevtCurrency, xevtDateTime, xevtDate, xevtString, xevtFormula }
        dfDefault, dfBoolean,   dfIntNumber, dfFloatNumber, dfCurrency,   dfDateTime,   dfDate,   dfString,   dfFloatNumber
      );

  protected
    FStyle: TXLSCellStyle;
    FFormat: array[TXLSValueType] of TXLSDisplayFormat;
    FPreserveStringType: boolean;
    FConsiderWhenApplyBestFit: boolean;

    function GetFormat(ValueType: TXLSValueType): TXLSDisplayFormat;
    procedure SetFormat(ValueType: TXLSValueType; const Value: TXLSDisplayFormat);

  public
    constructor Create;
    procedure Assign(Src: TXLSCellDefaults);
    procedure Clear;

    { Visual settings of the cell: color, font, border etc.
      Default value: clear.}
    property Style: TXLSCellStyle read FStyle write FStyle;

    { Mapping of ValueType (bool, int, float etc) to display format (money, float number etc).
      Default value: display format is matching value type. }
    property Format[ValueType: TXLSValueType]: TXLSDisplayFormat read GetFormat write SetFormat;

    { Some exporters may convert strings to "corresponding" type, for example string "12.11.13" can be converted to
      date "12 november 2013", string "2.22" can be converted to floating number etc. Set this option to disable it.
      Default value: True. }
    property PreserveStringType: boolean read FPreserveStringType write FPreserveStringType;

    { Some cells should not be taken into account when "best fit" is calculated. For example title in left-top cell should
      not increase width of whole first column with numbers.
      Default value: True. }
    property ConsiderWhenApplyBestFit: boolean read FConsiderWhenApplyBestFit write FConsiderWhenApplyBestFit;
  end;

  TXLSPrintOptions = record
  private
  public
    PrinterName: string;
    SettingsUI: boolean;

    constructor Create(APrinterName: string; ASettingsUI: boolean);
  end;

implementation

{ TXLSFont }

procedure TXLSFont.Clear;
begin
  Name.Clear;
  Color.Clear;
  Style.Clear;
  Height.Clear;
  Size.Clear;
end;

function TXLSFont.Empty: Boolean;
begin
  result := Name.Empty and Color.Empty and Style.Empty and Height.Empty and Size.Empty;
end;

class operator TXLSFont.Equal(ALeft, ARight: TXLSFont): Boolean;
begin
  result :=
    (ALeft.Name = ARight.Name) and
    (ALeft.Color = ARight.Color) and
    (ALeft.Style = ARight.Style) and
    (ALeft.Height = ARight.Height) and
    (ALeft.Size = ARight.Size);
end;

class operator TXLSFont.NotEqual(ALeft, ARight: TXLSFont): Boolean;
begin
  result := not (ALeft = ARight);
end;

{ TXLSCellStyle }

procedure TXLSCellStyle.Clear;
var I: TXLSBorder;
begin
  Font.Clear;
  for I := Low(I) to High(I) do
    Borders[I].Clear;
  BackgroundColor.Clear;
  AlignHorz.Clear;
  AlignVert.Clear;
end;

function TXLSCellStyle.Empty: Boolean;
var I: TXLSBorder;
begin
  for I := Low(I) to High(I) do
    if not Borders[I].Empty then
      Exit(False);
  result := Font.Empty and BackgroundColor.Empty and AlignHorz.Empty and AlignVert.Empty;
end;

class operator TXLSCellStyle.Equal(ALeft, ARight: TXLSCellStyle): Boolean;
var
  I: TXLSBorder;
begin
  for I := Low(TXLSBorder) to High(TXLSBorder) do
    if ALeft.Borders[I] <> ARight.Borders[I] then
      Exit(False);
  result :=
    (ALeft.Font = ARight.Font) and
    (ALeft.BackgroundColor = ARight.BackgroundColor) and
    (ALeft.AlignHorz = ARight.AlignHorz) and
    (ALeft.AlignVert = ARight.AlignVert);
end;

class operator TXLSCellStyle.NotEqual(ALeft, ARight: TXLSCellStyle): Boolean;
begin
  result := not (ALeft = ARight);
end;

{ TXLSSheet.TXLSPos }

constructor TXLSPos.Create(ARow,ACol: integer);
begin
  Row := ARow;
  Col := ACol;
end;

{ TXLSBorderOptions }

procedure TXLSBorderOptions.Clear;
begin
  Style.Clear;
  Color.Clear;
end;

function TXLSBorderOptions.Empty: Boolean;
begin
  result := Style.Empty and Color.Empty;
end;

class operator TXLSBorderOptions.Equal(ALeft, ARight: TXLSBorderOptions): Boolean;
begin
  result := (ALeft.Style = ARight.Style) and (ALeft.Color = ARight.Color);
end;

class operator TXLSBorderOptions.NotEqual(ALeft, ARight: TXLSBorderOptions): Boolean;
begin
  result := not (ALeft = ARight);
end;

{ TXLSCellDefaults }

constructor TXLSCellDefaults.Create;
begin
  inherited Create;
  Clear;
end;

procedure TXLSCellDefaults.Clear;
var
  I: TXLSValueType;
begin
  FStyle.Clear;
  for I := Low(TXLSValueType) to High(TXLSValueType) do
    FFormat[I] := MapDisplayFormat[I];
  FPreserveStringType := True;
  FConsiderWhenApplyBestFit := True;
end;

procedure TXLSCellDefaults.Assign(Src: TXLSCellDefaults);
begin
  FStyle := Src.FStyle;
  FFormat := Src.FFormat;
  FPreserveStringType := Src.FPreserveStringType;
  FConsiderWhenApplyBestFit := Src.FConsiderWhenApplyBestFit;
end;

function TXLSCellDefaults.GetFormat(ValueType: TXLSValueType): TXLSDisplayFormat;
begin
  result := FFormat[ValueType];
end;

procedure TXLSCellDefaults.SetFormat(ValueType: TXLSValueType; const Value: TXLSDisplayFormat);
begin
  FFormat[ValueType] := Value;
end;

{ TXLSPrintOptions }

constructor TXLSPrintOptions.Create(APrinterName: string; ASettingsUI: boolean);
begin
  Self := Default(TXLSPrintOptions);
  PrinterName := APrinterName;
  SettingsUI := ASettingsUI;
end;

end.
