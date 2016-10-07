unit adot.Graphics;
{$IFNDEF Debug}
  { $Define UseInline}
{$ENDIF}

{ Definition of classes/record types:

  TColorUtils = class
    Find similar standard color, change brightness etc.

}
interface

uses
  adot.Tools,
  System.UITypes,
  System.Math,
  System.SysUtils;

type
  { Find similar standard color, change brightness etc }
  TColorUtils = class
  public
    type
      TDistanceType = (dtMaxComponentDeviation, dtStandardDeviation);
      TColorClass = (
        ccBlack, ccGray,      ccSilver,  ccWhite,
        ccRed,   ccOrange,    ccYellow,  ccSpringGreen,
        ccGreen, ccTurquoise, ccCyan,    ccOcean,
        ccBlue,  ccViolet,    ccMagenta, ccRaspberry);

    const
      TDefDist = dtMaxComponentDeviation;
      ColorValues: array[TColorClass] of TColor = (
        TColorRec.Black, TColorRec.Gray,      TColorRec.Silver,  TColorRec.White,
        TColorRec.Red,   TColorRec.Orange,    TColorRec.Yellow,  TColorRec.Springgreen,
        TColorRec.Green, TColorRec.Turquoise, TColorRec.Cyan,    TColor($FF7D00) {ccOcean},
        TColorRec.Blue,  TColorRec.Violet,    TColorRec.Magenta, TColor($7D00FF) {ccRaspberry}
      );
      ColorNames: array[TColorClass] of String = (
        'Black', 'Gray',      'Silver',  'White',
        'Red',   'Orange',    'Yellow',  'SpringGreen',
        'Green', 'Turquoise', 'Cyan',    'Ocean',
        'Blue',  'Violet',    'Magenta', 'Raspberry'
      );

      { Subset of most important colors and mapping to corresponding TColorClass }
      BaseColorClasses = [
        ccRed,   ccGreen, ccBlue,   ccYellow,
        ccBlack, ccGray,  ccSilver, ccWhite
      ];
      BaseColors: array [0..8] of TColor = (
        TColorRec.Red,     TColorRec.Green,  TColorRec.Blue,
        TColorRec.SkyBlue, TColorRec.Yellow, TColorRec.Black,
        TColorRec.White,   TColorRec.Gray,   TColorRec.Silver
      );
      BaseColorToColorClass: array [0..8] of TColorClass = (
        ccRed,   ccGreen,  ccBlue,
        ccBlue,  ccYellow, ccBlack,
        ccWhite, ccGray,   ccSilver
      );
    const
      { luminance = (R*kRed + G*kGreen + B*kBlue) / 3000 }
      kRed   = 299;
      kGreen = 587;
      kBlue  = 114;
  private

  public
    { distance between colors }
    class function Distance(A,B: TColor; ADistType: TDistanceType = TDefDist):Integer; static;

    { recognition of colors according to distance  }
    class function RecognizeColor(ASample: TColor; const AColors: array of TColor; ADistType: TDistanceType = TDefDist): Integer; overload; static;
    class function RecognizeColor(ASample: TColor; ADistType: TDistanceType = TDefDist): TColorClass; overload; static;
    class function RecognizeBaseColor(ASample: TColor; ADistType: TDistanceType = TDefDist): TColorClass; static;
    class function GetName(C: TColor; ADistType: TDistanceType = TDefDist): String; static;
    class function GetBasicColorName(C: TColor; ADistType: TDistanceType = TDefDist): String; static;

    { find brightness of the color 0..255 }
    class function GetBrightness(c: TColor): byte; overload; static;
    class function GetBrightness(r,g,b: byte): byte; overload; static;
    class function GetBrightnessBGRA(c: cardinal): byte; static;
    { result has brightness = N% from brightness of C }
    class function AdjustComponentBrightness(c: byte; BrightnessPercent: cardinal): byte; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function AdjustBrightness(c: TColor; BrightnessPercent: cardinal): TColor; static;
    class function AdjustBrightnessBin(c: TColor; BrightnessPercent: cardinal): TColor; static; { no ColorToRGB transformation }
    { result has brightness = N% from max }
    class function SetBrightness(c: TColor; BrightnessPercent: byte): TColor; static;
    class function SetBrightnessAbs(c: TColor; Brightness: Byte): TColor; static;

    { broduces mix of several colors }
    class function MixColors(const Colors: array of TColor): TColor; static;

    { color <-> components }
    class function ColorToRGB(c: TColor): TColor; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function RGBToColor(R,G,B: byte): TColor; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function GetR(C: TColor): byte; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function GetG(C: TColor): byte; static; {$IFDEF UseInline}inline;{$ENDIF}
    class function GetB(C: TColor): byte; static; {$IFDEF UseInline}inline;{$ENDIF}
  end;
  TColorTools = TColorUtils;

  { Pixel format is automatically translated to standard TAlphaColor form System.UITypes.pas:
      ARGB (4 bytes) = (A shl 24) or (R shl 16) or (G shl 8) or B.
    Check TBitmapDirectAccess from adot.FMX.Graphics. }
  TBitmapDirectAccessCustom = class abstract
  protected
    function DoGetLine(y: integer): PAlphaColor; virtual; abstract;
    function DoGetWidth: integer; virtual; abstract;
    function DoGetHeight: integer; virtual; abstract;
    procedure DoLock; virtual; abstract;
    procedure DoUnlock; virtual; abstract;

    function GetPixel(x,y: integer): TAlphaColor;
    procedure SetPixel(x,y: integer; colors: TAlphaColor);
  public
    procedure Lock;
    procedure Unlock;

    property Lines[y: integer]: PAlphaColor read DoGetLine;
    property Pixels[x,y: integer]: TAlphaColor read GetPixel write SetPixel;
    property Width: integer read DoGetWidth;
    property Height: integer read DoGetHeight;
  end;

  { Class to perform gamma correction for raster image }
  TGammaCorrection = class
  protected
    FGamma: double;
    FMap: array[byte] of TAlphaColor;

    procedure SetGamma(const Value: double);
  public
    constructor Create(Gamma: double);

    procedure Correct(Dst: PAlphaColor; Count: integer); overload;
    procedure Correct(Bmp: TBitmapDirectAccessCustom); overload;

    property Gamma: double read FGamma write SetGamma;
  end;

  { Record type for efficient calculation of luminance for any area of raster image }
  TIntegralLuminance = record
  private
    FIntegralImage: TIntegralImageInt64; { record type, no need to free }

    function GetAvgLuminance(x1,y1,x2,y2: integer): byte;

  public
    procedure Build(Bmp: TBitmapDirectAccessCustom);

    { usually there is no need to call it, call it to free allocated memory imediately }
    procedure Clear;

    property AvgLuminance[x1,y1,x2,y2: integer]: byte read GetAvgLuminance; default;
  end;

implementation


{ TColorUtils }

class function TColorUtils.AdjustBrightness(c: TColor; BrightnessPercent: cardinal): TColor;
begin
  result := AdjustBrightnessBin(TColorRec.ColorToRGB(c), BrightnessPercent);
end;

class function TColorUtils.AdjustBrightnessBin(c: TColor; BrightnessPercent: cardinal): TColor;
begin
  result :=
    (c and $FF000000) or
    (AdjustComponentBrightness((c shr 16) and $FF, BrightnessPercent) shl 16) or
    (AdjustComponentBrightness((c shr  8) and $FF, BrightnessPercent) shl  8) or
    (AdjustComponentBrightness((c shr  0) and $FF, BrightnessPercent) shl  0);
end;

class function TColorUtils.AdjustComponentBrightness(c: byte; BrightnessPercent: cardinal): byte;
begin
  result := Min(255, c*BrightnessPercent div 100);
end;

class function TColorUtils.ColorToRGB(c: TColor): TColor;
begin
  result := TColorRec.ColorToRGB(c);
end;

class function TColorUtils.Distance(A, B: TColor; ADistType: TDistanceType = TDefDist): Integer;
var
  CA,CB: Longint;
begin
  CA := TColorRec.ColorToRGB(A);
  CB := TColorRec.ColorToRGB(B);
  case ADistType of
    dtMaxComponentDeviation:
      Result := Max(
        Max(
          Abs(((CA shr  8) and $FF)-((CB shr  8) and $FF)),
          Abs(((CA shr 16) and $FF)-((CB shr 16) and $FF))
        ),
        Abs((CA and $FF)-(CB and $FF))
      );
    dtStandardDeviation:
      Result := (
        Sqr((CA and $FF)-(CB and $FF)) +
        Sqr(((CA shr  8) and $FF)-((CB shr  8) and $FF)) +
        Sqr(((CA shr 16) and $FF)-((CB shr 16) and $FF))
      ) div 3;
    else
      result := High(Result);
  end;
end;

class function TColorUtils.RecognizeColor(ASample: TColor; const AColors: array of TColor;
  ADistType: TDistanceType = TDefDist): Integer;
var
  Dist: Integer;
  i,j: Integer;
begin
  Dist := High(Integer);
  Result := -1;
  for i := Low(AColors) to High(AColors) do
  begin
    j := Distance(ASample, AColors[i], ADistType);
    if j<Dist then
    begin
      Dist := j;
      Result := i;
    end;
  end;
end;

class function TColorUtils.RecognizeColor(ASample: TColor; ADistType: TDistanceType = TDefDist): TColorClass;
begin
  Result := TColorClass(RecognizeColor(ASample, ColorValues, ADistType));
end;

class function TColorUtils.GetName(C: TColor; ADistType: TDistanceType = TDefDist): String;
begin
  Result := ColorNames[RecognizeColor(C, ADistType)];
end;

class function TColorUtils.RGBToColor(R, G, B: byte): TColor;
begin
  Result := (TColor(B) shl 16) or (TColor(G) shl 8) or R;
end;

class function TColorUtils.GetBrightness(c: TColor): byte;
begin
  C := ColorToRGB(C);
  result := (cardinal(GetR(C))*kRed + cardinal(GetG(C))*kGreen + cardinal(GetB(C))*kBlue) div 1000;
end;

class function TColorUtils.GetBrightness(r, g, b: byte): byte;
begin
  result := (cardinal(r)*kRed + cardinal(g)*kGreen + cardinal(b)*kBlue) div 1000;
end;

class function TColorUtils.GetBrightnessBGRA(c: cardinal): byte;
begin
  result := (((c shr 24) and $FF)*kRed + ((c shr 16) and $FF)*kGreen + cardinal(c and $FF)*kBlue) div 1000;
end;

class function TColorUtils.SetBrightness(c: TColor; BrightnessPercent: byte): TColor;
var
  BrCur,BrNew: byte;
begin
  BrCur := GetBrightness(C);
  BrNew := 255*BrightnessPercent div 100;
  if BrCur=0 then
    result := RGBToColor(BrNew, BrNew, BrNew)
  else
    result := AdjustBrightness(C, BrNew*100 div BrCur);
end;

class function TColorUtils.SetBrightnessAbs(c: TColor; Brightness: Byte): TColor;
var
  R,G,B: byte;
  x: double;
begin
  {
    L = x*(R+G+B)/3
    x=L*3/(R+G+B)
  }
  C := ColorToRGB(C);
  R := GetR(C);
  G := GetG(C);
  B := GetB(C);
  x := Brightness*3/(R+G+B);
  result := RGBToColor(
    Min(Round(R*x), 255),
    Min(Round(G*x), 255),
    Min(Round(B*x), 255)
  );
end;

class function TColorUtils.GetR(C: TColor): byte;
begin
  result := Byte(C);
end;

class function TColorUtils.GetG(C: TColor): byte;
begin
  result := Byte(C shr 8);
end;

class function TColorUtils.GetB(C: TColor): byte;
begin
  result := Byte(C shr 16);
end;

class function TColorUtils.GetBasicColorName(C: TColor; ADistType: TDistanceType): String;
begin
  case RecognizeColor(C, [
    TColorRec.Red,    TColorRec.Green, TColorRec.Blue,  TColorRec.SkyBlue,
    TColorRec.Yellow, TColorRec.Black, TColorRec.White, TColorRec.Gray,
    TColorRec.Silver
  ]) of
    0: Result := ColorNames[ccRed];
    1: Result := ColorNames[ccGreen];
    2: Result := ColorNames[ccBlue];
    3: Result := ColorNames[ccBlue];
    4: Result := ColorNames[ccYellow];
    5: Result := ColorNames[ccBlack];
    6: Result := ColorNames[ccWhite];
    7: Result := ColorNames[ccGray];
    8: Result := ColorNames[ccSilver];
    else result := '';
  end;
end;

class function TColorUtils.MixColors(const Colors: array of TColor): TColor;
var
  R, G, B: Integer;
  i: Integer;
  L: Integer;
begin
  R := 0;
  G := 0;
  B := 0;
  for i := Low(Colors) to High(Colors) do
  begin
    Result := TColorRec.ColorToRGB(Colors[i]);
    R := R + GetR(Result);
    G := G + GetG(Result);
    B := B + GetB(Result);
  end;
  L := Length(Colors);
  Result := RGBToColor(R div L, G div L, B div L);
end;

class function TColorUtils.RecognizeBaseColor(ASample: TColor; ADistType: TDistanceType): TColorClass;
begin
  result := BaseColorToColorClass[RecognizeColor(ASample, BaseColors, ADistType)];
end;

{ TBitmapDirectAccessCustom }

function TBitmapDirectAccessCustom.GetPixel(x, y: integer): TAlphaColor;
var p: PAlphaColor;
begin
  p := Lines[y];
  inc(p, x);
  result := p^;;
end;

procedure TBitmapDirectAccessCustom.SetPixel(x, y: integer; colors: TAlphaColor);
var p: PAlphaColor;
begin
  p := Lines[y];
  inc(p, x);
  p^ := colors;
end;

procedure TBitmapDirectAccessCustom.Lock;
begin
  DoLock;
end;

procedure TBitmapDirectAccessCustom.Unlock;
begin
  DoUnlock;
end;

{ TGammaCorrection }

constructor TGammaCorrection.Create(Gamma: double);
begin
  SetGamma(Gamma);
end;

procedure TGammaCorrection.SetGamma(const Value: double);
var
  I: Integer;
begin
  FGamma := Value;
  for I := 0 to 255 do
    FMap[I] := Trunc(255*Power(I/255, 1/Value));
end;

procedure TGammaCorrection.Correct(Dst: PAlphaColor; Count: integer);
var
  C: TAlphaColor;
begin
  while Count > 0 do
  begin
    dec(Count);
    C := Dst^;
    C := (C and $FF000000) or
      (FMap[(C shr 16) and $FF] shl 16) or
      (FMap[(C shr  8) and $FF] shl  8) or
      (FMap[ C         and $FF]       );
    Dst^ := C;
  end;
end;

procedure TGammaCorrection.Correct(Bmp: TBitmapDirectAccessCustom);
var
  y: Integer;
begin
  Bmp.Lock;
  try
    for y := 0 to Bmp.Height-1 do
      Correct(Bmp.Lines[y], Bmp.Width);
  finally
    Bmp.Unlock;
  end;
end;

{ TIntegralLuminance }

procedure TIntegralLuminance.Build(Bmp: TBitmapDirectAccessCustom);
var
  X,Y: Integer;
  Src: PAlphaColor;
  Dst: PInt64Array;
begin
  FIntegralImage.SetSize(Bmp.Width, Bmp.Height);
  for Y := 0 to Bmp.Height-1 do
  begin
    Src := Bmp.Lines[Y];
    Dst := FIntegralImage.Lines[Y];
    for X := 0 to Bmp.Width-1 do
    begin
      Dst[X] := TColorUtils.GetBrightnessBGRA(Src^);
      inc(Src);
    end;
  end;
  FIntegralImage.Build;
end;

procedure TIntegralLuminance.Clear;
begin
  FIntegralImage.Clear;
end;

function TIntegralLuminance.GetAvgLuminance(x1, y1, x2, y2: integer): byte;
begin
  result := FIntegralImage.Avg[x1,y1,x2,y2];
end;

end.
