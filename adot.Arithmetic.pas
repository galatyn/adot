unit adot.Arithmetic;

interface

{
  Implementation of IArithmetic for all basic types in Delphi:
    class function TArithmeticUtils<T>.DefaultArithmetic: IArithmetic<T>;
  - Used by TBox<T> to make possible arithmetic operations without type conversion.
  - Can be used for calculations on generic types, for example Avg/Sum etc implemented in TList<T>
}

uses
  adot.Types,
  System.SysUtils,
  System.Character,
  System.AnsiStrings,
  System.WideStrings;

type
  TArithmeticUtils<T> = class
  private
    class var
      FArithmetic: IArithmetic<T>;

    type

      { Platform-dependent integer types }

      TArithmeticNativeInt = class(TCustomArithmetic<NativeInt>)
      protected
        class var
          FOrdinal: TArithmeticNativeInt;
      public
        class function Ordinal: TArithmeticNativeInt;
        function Add(Left: NativeInt; Right: NativeInt): NativeInt; override;
        function Subtract(Left: NativeInt; Right: NativeInt): NativeInt; override;
        function Multiply(Left: NativeInt; Right: NativeInt): NativeInt; override;
        function Divide(Left: NativeInt; Right: NativeInt): NativeInt; override;
        function Negative(Value: NativeInt): NativeInt; override;
      end;

      TArithmeticNativeUInt = class(TCustomArithmetic<NativeUInt>)
      protected
        class var
          FOrdinal: TArithmeticNativeUInt;
      public
        class function Ordinal: TArithmeticNativeUInt;
        function Add(Left: NativeUInt; Right: NativeUInt): NativeUInt; override;
        function Subtract(Left: NativeUInt; Right: NativeUInt): NativeUInt; override;
        function Multiply(Left: NativeUInt; Right: NativeUInt): NativeUInt; override;
        function Divide(Left: NativeUInt; Right: NativeUInt): NativeUInt; override;
        function Negative(Value: NativeUInt): NativeUInt; override;
      end;

      { 32-bit platforms and 64-bit Windows platforms
        64-bit iOS platforms }
      TArithmeticLongInt = class(TCustomArithmetic<LongInt>)
      protected
        class var
          FOrdinal: TArithmeticLongInt;
      public
        class function Ordinal: TArithmeticLongInt;
        function Add(Left: LongInt; Right: LongInt): LongInt; override;
        function Subtract(Left: LongInt; Right: LongInt): LongInt; override;
        function Multiply(Left: LongInt; Right: LongInt): LongInt; override;
        function Divide(Left: LongInt; Right: LongInt): LongInt; override;
        function Negative(Value: LongInt): LongInt; override;
      end;

      { 32-bit platforms and 64-bit Windows platforms
        64-bit iOS platforms }
      TArithmeticLongWord = class(TCustomArithmetic<LongWord>)
      protected
        class var
          FOrdinal: TArithmeticLongWord;
      public
        class function Ordinal: TArithmeticLongWord;
        function Add(Left: LongWord; Right: LongWord): LongWord; override;
        function Subtract(Left: LongWord; Right: LongWord): LongWord; override;
        function Multiply(Left: LongWord; Right: LongWord): LongWord; override;
        function Divide(Left: LongWord; Right: LongWord): LongWord; override;
        function Negative(Value: LongWord): LongWord; override;
      end;

      { Platform-independent integer types }

      TArithmeticShortInt = class(TCustomArithmetic<ShortInt>)
      protected
        class var
          FOrdinal: TArithmeticShortInt;
      public
        class function Ordinal: TArithmeticShortInt;
        function Add(Left: ShortInt; Right: ShortInt): ShortInt; override;
        function Subtract(Left: ShortInt; Right: ShortInt): ShortInt; override;
        function Multiply(Left: ShortInt; Right: ShortInt): ShortInt; override;
        function Divide(Left: ShortInt; Right: ShortInt): ShortInt; override;
        function Negative(Value: ShortInt): ShortInt; override;
      end;

      TArithmeticSmallInt = class(TCustomArithmetic<SmallInt>)
      protected
        class var
          FOrdinal: TArithmeticSmallInt;
      public
        class function Ordinal: TArithmeticSmallInt;
        function Add(Left: SmallInt; Right: SmallInt): SmallInt; override;
        function Subtract(Left: SmallInt; Right: SmallInt): SmallInt; override;
        function Multiply(Left: SmallInt; Right: SmallInt): SmallInt; override;
        function Divide(Left: SmallInt; Right: SmallInt): SmallInt; override;
        function Negative(Value: SmallInt): SmallInt; override;
      end;

      TArithmeticFixedInt = class(TCustomArithmetic<FixedInt>)
      protected
        class var
          FOrdinal: TArithmeticFixedInt;
      public
        class function Ordinal: TArithmeticFixedInt;
        function Add(Left: FixedInt; Right: FixedInt): FixedInt; override;
        function Subtract(Left: FixedInt; Right: FixedInt): FixedInt; override;
        function Multiply(Left: FixedInt; Right: FixedInt): FixedInt; override;
        function Divide(Left: FixedInt; Right: FixedInt): FixedInt; override;
        function Negative(Value: FixedInt): FixedInt; override;
      end;

      TArithmeticInteger = class(TArithmeticFixedInt);

      TArithmeticInt64 = class(TCustomArithmetic<Int64>)
      protected
        class var
          FOrdinal: TArithmeticInt64;
      public
        class function Ordinal: TArithmeticInt64;
        function Add(Left: Int64; Right: Int64): Int64; override;
        function Subtract(Left: Int64; Right: Int64): Int64; override;
        function Multiply(Left: Int64; Right: Int64): Int64; override;
        function Divide(Left: Int64; Right: Int64): Int64; override;
        function Negative(Value: Int64): Int64; override;
      end;

      TArithmeticByte = class(TCustomArithmetic<Byte>)
      protected
        class var
          FOrdinal: TArithmeticByte;
      public
        class function Ordinal: TArithmeticByte;
        function Add(Left: Byte; Right: Byte): Byte; override;
        function Subtract(Left: Byte; Right: Byte): Byte; override;
        function Multiply(Left: Byte; Right: Byte): Byte; override;
        function Divide(Left: Byte; Right: Byte): Byte; override;
        function Negative(Value: Byte): Byte; override;
      end;

      TArithmeticWord = class(TCustomArithmetic<Word>)
      protected
        class var
          FOrdinal: TArithmeticWord;
      public
        class function Ordinal: TArithmeticWord;
        function Add(Left: Word; Right: Word): Word; override;
        function Subtract(Left: Word; Right: Word): Word; override;
        function Multiply(Left: Word; Right: Word): Word; override;
        function Divide(Left: Word; Right: Word): Word; override;
        function Negative(Value: Word): Word; override;
      end;

      TArithmeticFixedUInt = class(TCustomArithmetic<FixedUInt>)
      protected
        class var
          FOrdinal: TArithmeticFixedUInt;
      public
        class function Ordinal: TArithmeticFixedUInt;
        function Add(Left: FixedUInt; Right: FixedUInt): FixedUInt; override;
        function Subtract(Left: FixedUInt; Right: FixedUInt): FixedUInt; override;
        function Multiply(Left: FixedUInt; Right: FixedUInt): FixedUInt; override;
        function Divide(Left: FixedUInt; Right: FixedUInt): FixedUInt; override;
        function Negative(Value: FixedUInt): FixedUInt; override;
      end;

      TArithmeticCardinal = class(TArithmeticFixedUInt);

      TArithmeticUInt64 = class(TCustomArithmetic<UInt64>)
      protected
        class var
          FOrdinal: TArithmeticUInt64;
      public
        class function Ordinal: TArithmeticUInt64;
        function Add(Left: UInt64; Right: UInt64): UInt64; override;
        function Subtract(Left: UInt64; Right: UInt64): UInt64; override;
        function Multiply(Left: UInt64; Right: UInt64): UInt64; override;
        function Divide(Left: UInt64; Right: UInt64): UInt64; override;
        function Negative(Value: UInt64): UInt64; override;
      end;

      { Real types }

      TArithmeticSingle = class(TCustomArithmetic<Single>)
      protected
        class var
          FOrdinal: TArithmeticSingle;
      public
        class function Ordinal: TArithmeticSingle;
        function Add(Left: Single; Right: Single): Single; override;
        function Subtract(Left: Single; Right: Single): Single; override;
        function Multiply(Left: Single; Right: Single): Single; override;
        function Divide(Left: Single; Right: Single): Single; override;
        function Negative(Value: Single): Single; override;
      end;

      TArithmeticDouble = class(TCustomArithmetic<double>)
      protected
        class var
          FOrdinal: TArithmeticDouble;
      public
        class function Ordinal: TArithmeticDouble;
        function Add(Left: double; Right: double): double; override;
        function Subtract(Left: double; Right: double): double; override;
        function Multiply(Left: double; Right: double): double; override;
        function Divide(Left: double; Right: double): double; override;
        function Negative(Value: double): double; override;
      end;

      TArithmeticReal = class(TCustomArithmetic<Real>)
      protected
        class var
          FOrdinal: TArithmeticReal;
      public
        class function Ordinal: TArithmeticReal;
        function Add(Left: Real; Right: Real): Real; override;
        function Subtract(Left: Real; Right: Real): Real; override;
        function Multiply(Left: Real; Right: Real): Real; override;
        function Divide(Left: Real; Right: Real): Real; override;
        function Negative(Value: Real): Real; override;
      end;

      TArithmeticExtended = class(TCustomArithmetic<Extended>)
      protected
        class var
          FOrdinal: TArithmeticExtended;
      public
        class function Ordinal: TArithmeticExtended;
        function Add(Left: Extended; Right: Extended): Extended; override;
        function Subtract(Left: Extended; Right: Extended): Extended; override;
        function Multiply(Left: Extended; Right: Extended): Extended; override;
        function Divide(Left: Extended; Right: Extended): Extended; override;
        function Negative(Value: Extended): Extended; override;
      end;

      TArithmeticComp = class(TCustomArithmetic<Comp>)
      protected
        class var
          FOrdinal: TArithmeticComp;
      public
        class function Ordinal: TArithmeticComp;
        function Add(Left: Comp; Right: Comp): Comp; override;
        function Subtract(Left: Comp; Right: Comp): Comp; override;
        function Multiply(Left: Comp; Right: Comp): Comp; override;
        function Divide(Left: Comp; Right: Comp): Comp; override;
        function Negative(Value: Comp): Comp; override;
      end;

      TArithmeticCurrency = class(TCustomArithmetic<Currency>)
      protected
        class var
          FOrdinal: TArithmeticCurrency;
      public
        class function Ordinal: TArithmeticCurrency;
        function Add(Left: Currency; Right: Currency): Currency; override;
        function Subtract(Left: Currency; Right: Currency): Currency; override;
        function Multiply(Left: Currency; Right: Currency): Currency; override;
        function Divide(Left: Currency; Right: Currency): Currency; override;
        function Negative(Value: Currency): Currency; override;
      end;

      { string types }

      TArithmeticAnsiString = class(TCustomArithmetic<AnsiString>)
      protected
        class var
          FOrdinal: TArithmeticAnsiString;
      public
        class function Ordinal: TArithmeticAnsiString;
        function Add(Left: AnsiString; Right: AnsiString): AnsiString; override;
        function Subtract(Left: AnsiString; Right: AnsiString): AnsiString; override;
        function Multiply(Left: AnsiString; Right: AnsiString): AnsiString; override;
        function Divide(Left: AnsiString; Right: AnsiString): AnsiString; override;
        function Negative(Value: AnsiString): AnsiString; override;
      end;

      TArithmeticString = class(TCustomArithmetic<String>)
      protected
        class var
          FOrdinal: TArithmeticString;
      public
        class function Ordinal: TArithmeticString;
        function Add(Left: String; Right: String): String; override;
        function Subtract(Left: String; Right: String): String; override;
        function Multiply(Left: String; Right: String): String; override;
        function Divide(Left: String; Right: String): String; override;
        function Negative(Value: String): String; override;
      end;

      {$IF defined(MSWINDOWS)}
      TArithmeticWideString = class(TCustomArithmetic<WideString>)
      protected
        class var
          FOrdinal: TArithmeticWideString;
      public
        class function Ordinal: TArithmeticWideString;
        function Add(Left: WideString; Right: WideString): WideString; override;
        function Subtract(Left: WideString; Right: WideString): WideString; override;
        function Multiply(Left: WideString; Right: WideString): WideString; override;
        function Divide(Left: WideString; Right: WideString): WideString; override;
        function Negative(Value: WideString): WideString; override;
      end;
      {$ENDIF}

  public
    class function DefaultArithmetic: IArithmetic<T>; static;
  end;


implementation

{ TArithmeticUtils<T> }

class function TArithmeticUtils<T>.DefaultArithmetic: IArithmetic<T>;
begin
  if FArithmetic = nil then
  begin
    { Platform-dependent integer types }
    if TypeInfo(T) = TypeInfo(NativeInt)  then FArithmetic := IArithmetic<T>( IArithmetic<NativeInt> (TArithmeticNativeInt.Ordinal) )  else
    if TypeInfo(T) = TypeInfo(NativeUInt) then FArithmetic := IArithmetic<T>( IArithmetic<NativeUInt>(TArithmeticNativeUInt.Ordinal) ) else
    if TypeInfo(T) = TypeInfo(LongInt)    then FArithmetic := IArithmetic<T>( IArithmetic<LongInt>   (TArithmeticLongInt.Ordinal) )    else
    if TypeInfo(T) = TypeInfo(LongWord)   then FArithmetic := IArithmetic<T>( IArithmetic<LongWord>  (TArithmeticLongWord.Ordinal) )   else

    { Platform-independent integer types }
    if TypeInfo(T) = TypeInfo(ShortInt)   then FArithmetic := IArithmetic<T>( IArithmetic<ShortInt>  (TArithmeticShortInt.Ordinal) )   else
    if TypeInfo(T) = TypeInfo(SmallInt)   then FArithmetic := IArithmetic<T>( IArithmetic<SmallInt>  (TArithmeticSmallInt.Ordinal) )   else
    if TypeInfo(T) = TypeInfo(FixedInt)   then FArithmetic := IArithmetic<T>( IArithmetic<FixedInt>  (TArithmeticFixedInt.Ordinal) )   else
    if TypeInfo(T) = TypeInfo(integer)    then FArithmetic := IArithmetic<T>( IArithmetic<integer>   (TArithmeticInteger.Ordinal) )    else
    if TypeInfo(T) = TypeInfo(Int64)      then FArithmetic := IArithmetic<T>( IArithmetic<Int64>     (TArithmeticInt64.Ordinal) )      else
    if TypeInfo(T) = TypeInfo(Byte)       then FArithmetic := IArithmetic<T>( IArithmetic<Byte>      (TArithmeticByte.Ordinal) )       else
    if TypeInfo(T) = TypeInfo(Word)       then FArithmetic := IArithmetic<T>( IArithmetic<Word>      (TArithmeticWord.Ordinal) )       else
    if TypeInfo(T) = TypeInfo(FixedUInt)  then FArithmetic := IArithmetic<T>( IArithmetic<FixedUInt> (TArithmeticFixedUInt.Ordinal) )  else
    if TypeInfo(T) = TypeInfo(Cardinal)   then FArithmetic := IArithmetic<T>( IArithmetic<Cardinal>  (TArithmeticCardinal.Ordinal) )   else
    if TypeInfo(T) = TypeInfo(UInt64)     then FArithmetic := IArithmetic<T>( IArithmetic<UInt64>    (TArithmeticUInt64.Ordinal) )     else

    { Real types }
    if TypeInfo(T) = TypeInfo(Single)     then FArithmetic := IArithmetic<T>( IArithmetic<Single>    (TArithmeticSingle.Ordinal) )     else
    if TypeInfo(T) = TypeInfo(Double)     then FArithmetic := IArithmetic<T>( IArithmetic<Double>    (TArithmeticDouble.Ordinal) )     else
    if TypeInfo(T) = TypeInfo(Real)       then FArithmetic := IArithmetic<T>( IArithmetic<Real>      (TArithmeticReal.Ordinal) )       else
    if TypeInfo(T) = TypeInfo(Extended)   then FArithmetic := IArithmetic<T>( IArithmetic<Extended>  (TArithmeticExtended.Ordinal) )   else
    if TypeInfo(T) = TypeInfo(Comp)       then FArithmetic := IArithmetic<T>( IArithmetic<Comp>      (TArithmeticComp.Ordinal) )       else
    if TypeInfo(T) = TypeInfo(Currency)   then FArithmetic := IArithmetic<T>( IArithmetic<Currency>  (TArithmeticCurrency.Ordinal) )   else

    { String types }
    if TypeInfo(T) = TypeInfo(AnsiString) then FArithmetic := IArithmetic<T>( IArithmetic<AnsiString>(TArithmeticAnsiString.Ordinal) ) else
    if TypeInfo(T) = TypeInfo(String)     then FArithmetic := IArithmetic<T>( IArithmetic<String>    (TArithmeticString.Ordinal) )     else
    {$IFDEF MSWINDOWS}
    if TypeInfo(T) = TypeInfo(WideString) then FArithmetic := IArithmetic<T>( IArithmetic<WideString>(TArithmeticWideString.Ordinal) ) else
    {$ENDIF}

    { unsupported type}
    raise Exception.Create('not implemented');
  end;
  Result := FArithmetic;
end;

{ TArithmeticUtils<T>.TArithmeticNativeInt }

class function TArithmeticUtils<T>.TArithmeticNativeInt.Ordinal: TArithmeticNativeInt;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticNativeInt.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticNativeInt.Add(Left, Right: NativeInt): NativeInt;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticNativeInt.Divide(Left, Right: NativeInt): NativeInt;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticNativeInt.Multiply(Left, Right: NativeInt): NativeInt;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticNativeInt.Negative(Value: NativeInt): NativeInt;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticNativeInt.Subtract(Left, Right: NativeInt): NativeInt;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticNativeUInt }

class function TArithmeticUtils<T>.TArithmeticNativeUInt.Ordinal: TArithmeticNativeUInt;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticNativeUInt.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticNativeUInt.Add(Left, Right: NativeUInt): NativeUInt;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticNativeUInt.Divide(Left, Right: NativeUInt): NativeUInt;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticNativeUInt.Multiply(Left, Right: NativeUInt): NativeUInt;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticNativeUInt.Negative(Value: NativeUInt): NativeUInt;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticNativeUInt.Subtract(Left, Right: NativeUInt): NativeUInt;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticLongInt }

class function TArithmeticUtils<T>.TArithmeticLongInt.Ordinal: TArithmeticLongInt;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticLongInt.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticLongInt.Add(Left, Right: LongInt): LongInt;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticLongInt.Divide(Left, Right: LongInt): LongInt;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticLongInt.Multiply(Left, Right: LongInt): LongInt;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticLongInt.Negative(Value: LongInt): LongInt;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticLongInt.Subtract(Left, Right: LongInt): LongInt;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticLongWord }

class function TArithmeticUtils<T>.TArithmeticLongWord.Ordinal: TArithmeticLongWord;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticLongWord.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticLongWord.Add(Left, Right: LongWord): LongWord;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticLongWord.Divide(Left, Right: LongWord): LongWord;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticLongWord.Multiply(Left, Right: LongWord): LongWord;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticLongWord.Negative(Value: LongWord): LongWord;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticLongWord.Subtract(Left, Right: LongWord): LongWord;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticShortInt }

class function TArithmeticUtils<T>.TArithmeticShortInt.Ordinal: TArithmeticShortInt;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticShortInt.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticShortInt.Add(Left, Right: ShortInt): ShortInt;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticShortInt.Divide(Left, Right: ShortInt): ShortInt;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticShortInt.Multiply(Left, Right: ShortInt): ShortInt;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticShortInt.Negative(Value: ShortInt): ShortInt;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticShortInt.Subtract(Left, Right: ShortInt): ShortInt;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticSmallInt }

class function TArithmeticUtils<T>.TArithmeticSmallInt.Ordinal: TArithmeticSmallInt;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticSmallInt.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticSmallInt.Add(Left, Right: SmallInt): SmallInt;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticSmallInt.Divide(Left, Right: SmallInt): SmallInt;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticSmallInt.Multiply(Left, Right: SmallInt): SmallInt;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticSmallInt.Negative(Value: SmallInt): SmallInt;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticSmallInt.Subtract(Left, Right: SmallInt): SmallInt;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticFixedInt }

class function TArithmeticUtils<T>.TArithmeticFixedInt.Ordinal: TArithmeticFixedInt;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticFixedInt.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticFixedInt.Add(Left, Right: FixedInt): FixedInt;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticFixedInt.Divide(Left, Right: FixedInt): FixedInt;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticFixedInt.Multiply(Left, Right: FixedInt): FixedInt;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticFixedInt.Negative(Value: FixedInt): FixedInt;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticFixedInt.Subtract(Left, Right: FixedInt): FixedInt;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticInt64 }

class function TArithmeticUtils<T>.TArithmeticInt64.Ordinal: TArithmeticInt64;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticInt64.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticInt64.Add(Left, Right: Int64): Int64;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticInt64.Divide(Left, Right: Int64): Int64;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticInt64.Multiply(Left, Right: Int64): Int64;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticInt64.Negative(Value: Int64): Int64;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticInt64.Subtract(Left, Right: Int64): Int64;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticByte }

class function TArithmeticUtils<T>.TArithmeticByte.Ordinal: TArithmeticByte;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticByte.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticByte.Add(Left, Right: Byte): Byte;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticByte.Divide(Left, Right: Byte): Byte;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticByte.Multiply(Left, Right: Byte): Byte;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticByte.Negative(Value: Byte): Byte;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticByte.Subtract(Left, Right: Byte): Byte;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticWord }

class function TArithmeticUtils<T>.TArithmeticWord.Ordinal: TArithmeticWord;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticWord.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticWord.Add(Left, Right: Word): Word;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticWord.Divide(Left, Right: Word): Word;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticWord.Multiply(Left, Right: Word): Word;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticWord.Negative(Value: Word): Word;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticWord.Subtract(Left, Right: Word): Word;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticFixedUInt }

class function TArithmeticUtils<T>.TArithmeticFixedUInt.Ordinal: TArithmeticFixedUInt;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticFixedUInt.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticFixedUInt.Add(Left, Right: FixedUInt): FixedUInt;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticFixedUInt.Divide(Left, Right: FixedUInt): FixedUInt;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticFixedUInt.Multiply(Left, Right: FixedUInt): FixedUInt;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticFixedUInt.Negative(Value: FixedUInt): FixedUInt;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticFixedUInt.Subtract(Left, Right: FixedUInt): FixedUInt;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticUInt64 }

class function TArithmeticUtils<T>.TArithmeticUInt64.Ordinal: TArithmeticUInt64;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticUInt64.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticUInt64.Add(Left, Right: UInt64): UInt64;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticUInt64.Divide(Left, Right: UInt64): UInt64;
begin
  Result := Left div Right;
end;

function TArithmeticUtils<T>.TArithmeticUInt64.Multiply(Left, Right: UInt64): UInt64;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticUInt64.Negative(Value: UInt64): UInt64;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticUInt64.Subtract(Left, Right: UInt64): UInt64;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticSingle }

class function TArithmeticUtils<T>.TArithmeticSingle.Ordinal: TArithmeticSingle;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticSingle.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticSingle.Add(Left, Right: Single): Single;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticSingle.Divide(Left, Right: Single): Single;
begin
  Result := Left / Right;
end;

function TArithmeticUtils<T>.TArithmeticSingle.Multiply(Left, Right: Single): Single;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticSingle.Negative(Value: Single): Single;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticSingle.Subtract(Left, Right: Single): Single;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticDouble }

class function TArithmeticUtils<T>.TArithmeticDouble.Ordinal: TArithmeticDouble;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticDouble.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticDouble.Add(Left, Right: double): double;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticDouble.Divide(Left, Right: double): double;
begin
  Result := Left / Right;
end;

function TArithmeticUtils<T>.TArithmeticDouble.Multiply(Left, Right: double): double;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticDouble.Negative(Value: double): double;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticDouble.Subtract(Left, Right: double): double;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticReal }

class function TArithmeticUtils<T>.TArithmeticReal.Ordinal: TArithmeticReal;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticReal.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticReal.Add(Left, Right: Real): Real;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticReal.Divide(Left, Right: Real): Real;
begin
  Result := Left / Right;
end;

function TArithmeticUtils<T>.TArithmeticReal.Multiply(Left, Right: Real): Real;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticReal.Negative(Value: Real): Real;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticReal.Subtract(Left, Right: Real): Real;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticExtended }

class function TArithmeticUtils<T>.TArithmeticExtended.Ordinal: TArithmeticExtended;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticExtended.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticExtended.Add(Left, Right: Extended): Extended;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticExtended.Divide(Left, Right: Extended): Extended;
begin
  Result := Left / Right;
end;

function TArithmeticUtils<T>.TArithmeticExtended.Multiply(Left, Right: Extended): Extended;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticExtended.Negative(Value: Extended): Extended;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticExtended.Subtract(Left, Right: Extended): Extended;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticComp }

class function TArithmeticUtils<T>.TArithmeticComp.Ordinal: TArithmeticComp;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticComp.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticComp.Add(Left, Right: Comp): Comp;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticComp.Divide(Left, Right: Comp): Comp;
begin
  Result := Left / Right;
end;

function TArithmeticUtils<T>.TArithmeticComp.Multiply(Left, Right: Comp): Comp;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticComp.Negative(Value: Comp): Comp;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticComp.Subtract(Left, Right: Comp): Comp;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticCurrency }

class function TArithmeticUtils<T>.TArithmeticCurrency.Ordinal: TArithmeticCurrency;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticCurrency.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticCurrency.Add(Left, Right: Currency): Currency;
begin
  Result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticCurrency.Divide(Left, Right: Currency): Currency;
begin
  Result := Left / Right;
end;

function TArithmeticUtils<T>.TArithmeticCurrency.Multiply(Left, Right: Currency): Currency;
begin
  Result := Left * Right;
end;

function TArithmeticUtils<T>.TArithmeticCurrency.Negative(Value: Currency): Currency;
begin
  Result := -Value;
end;

function TArithmeticUtils<T>.TArithmeticCurrency.Subtract(Left, Right: Currency): Currency;
begin
  Result := Left - Right;
end;

{ TArithmeticUtils<T>.TArithmeticAnsiString }

class function TArithmeticUtils<T>.TArithmeticAnsiString.Ordinal: TArithmeticAnsiString;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticAnsiString.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticAnsiString.Add(Left, Right: AnsiString): AnsiString;
begin
  result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticAnsiString.Subtract(Left, Right: AnsiString): AnsiString;
begin
  result := Left;
  if AnsiEndsText(Right, Left) then
    SetLength(Result, Length(Result)-Length(Right));
end;

function TArithmeticUtils<T>.TArithmeticAnsiString.Negative(Value: AnsiString): AnsiString;
var
  I: Integer;
  L,U: AnsiString;
begin
  L := AnsiUpperCase(Value);
  U := AnsiLowerCase(Value);
  result := Value;
  for I := Low(Result) to High(Result) do
    if Result[I] = L[I] then
      Result[I] := U[I]
    else
      Result[I] := L[I];
end;

function TArithmeticUtils<T>.TArithmeticAnsiString.Multiply(Left, Right: AnsiString): AnsiString;
begin
  raise Exception.Create('Bad operation');
end;

function TArithmeticUtils<T>.TArithmeticAnsiString.Divide(Left, Right: AnsiString): AnsiString;
begin
  raise Exception.Create('Bad operation');
end;

{ TArithmeticUtils<T>.TArithmeticString }

class function TArithmeticUtils<T>.TArithmeticString.Ordinal: TArithmeticString;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticString.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticString.Add(Left, Right: String): String;
begin
  result := Left + Right;
end;

function TArithmeticUtils<T>.TArithmeticString.Subtract(Left, Right: String): String;
begin
  result := Left;
  if result.EndsWith(Right, True) then
    SetLength(Result, Length(Result)-Length(Right));
end;

function TArithmeticUtils<T>.TArithmeticString.Negative(Value: String): String;
var
  I: Integer;
begin
  result := Value;
  for I := Low(Result) to High(Result) do
    if Result[I].IsUpper then
      Result[I] := Result[I].ToLower
    else
      Result[I] := Result[I].ToUpper;
end;

function TArithmeticUtils<T>.TArithmeticString.Multiply(Left, Right: String): String;
begin
  raise Exception.Create('Bad operation');
end;

function TArithmeticUtils<T>.TArithmeticString.Divide(Left, Right: String): String;
begin
  raise Exception.Create('Bad operation');
end;

{ TArithmeticUtils<T>.TArithmeticWideString }

{$IF defined(MSWINDOWS)}
class function TArithmeticUtils<T>.TArithmeticWideString.Ordinal: TArithmeticWideString;
begin
  if FOrdinal = nil then
    FOrdinal := TArithmeticWideString.Create;
  Result := FOrdinal;
end;

function TArithmeticUtils<T>.TArithmeticWideString.Add(Left, Right: WideString): WideString;
begin
  result := TArithmeticString.Ordinal.Add(Left, Right);
end;

function TArithmeticUtils<T>.TArithmeticWideString.Subtract(Left, Right: WideString): WideString;
begin
  result := TArithmeticString.Ordinal.Subtract(Left, Right);
end;

function TArithmeticUtils<T>.TArithmeticWideString.Negative(Value: WideString): WideString;
begin
  result := TArithmeticString.Ordinal.Negative(Value);
end;

function TArithmeticUtils<T>.TArithmeticWideString.Multiply(Left, Right: WideString): WideString;
begin
  result := TArithmeticString.Ordinal.Multiply(Left, Right);
end;

function TArithmeticUtils<T>.TArithmeticWideString.Divide(Left, Right: WideString): WideString;
begin
  result := TArithmeticString.Ordinal.Divide(Left, Right);
end;
{$ENDIF}

end.
