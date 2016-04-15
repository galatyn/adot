unit adot.Graphics;

{ Definition of classes/record types:

  TColorUtils = class
    Find similar standard color, change brightness etc.

}
interface

uses
  System.UITypes,
  System.Math;

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
  private

  public
    class function Distance(A,B: TColor; ADistType: TDistanceType = TDefDist):Integer; static;
    class function RecognizeColor(ASample: TColor; const AColors: array of TColor; ADistType: TDistanceType = TDefDist): Integer; overload; static;
    class function RecognizeColor(ASample: TColor; ADistType: TDistanceType = TDefDist): TColorClass; overload; static;
    class function RecognizeBaseColor(ASample: TColor; ADistType: TDistanceType = TDefDist): TColorClass; static;
    class function GetName(C: TColor; ADistType: TDistanceType = TDefDist): String; static;
    class function GetBasicColorName(C: TColor; ADistType: TDistanceType = TDefDist): String; static;
    class function AdjustComponentBrightness(c: byte; BrightnessPercent: byte): byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function AdjustBrightness(c: TColor; BrightnessPercent: byte): TColor; static;
    class function AdjustBrightnessBin(c: TColor; BrightnessPercent: byte): TColor; static; { no ColorToRGB transformation }
    class function MixColors(const Colors: array of TColor): TColor; static;
    class function ColorToRGB(c: TColor): TColor; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function RGBToColor(R,G,B: byte): TColor; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function GetR(C: TColor): byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function GetG(C: TColor): byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
    class function GetB(C: TColor): byte; static; {$IFNDEF DEBUG}inline;{$ENDIF}
  end;
  TColorTools = TColorUtils;

implementation


{ TColorUtils }

class function TColorUtils.AdjustBrightness(c: TColor; BrightnessPercent: byte): TColor;
begin
  result := AdjustBrightnessBin(TColorRec.ColorToRGB(c), BrightnessPercent);
end;

class function TColorUtils.AdjustBrightnessBin(c: TColor; BrightnessPercent: byte): TColor;
begin
  result :=
    (c and $FF000000) or
    (AdjustComponentBrightness((c shr 16) and $FF, BrightnessPercent) shl 16) or
    (AdjustComponentBrightness((c shr  8) and $FF, BrightnessPercent) shl  8) or
    (AdjustComponentBrightness((c shr  0) and $FF, BrightnessPercent) shl  0);
end;

class function TColorUtils.AdjustComponentBrightness(c, BrightnessPercent: byte): byte;
begin
  result := Min(255, longword(c)*BrightnessPercent div 100);
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

end.
