unit adot.Collections.Iterators;

interface

{$If not defined(debug)}
  {$define doinline}
{$EndIf}

type
  TArrayIterator<T> = record
  public
    type
      PVal = ^T;
  private
    Ref: PVal;

    function GetValue: T;               {$If defined(doinline)} inline; {$EndIf}
    procedure SetValue(const Value: T); {$If defined(doinline)} inline; {$EndIf}

  public
    procedure Init(Ref: PVal);

    class operator Implicit          (const a : TArrayIterator<T>) : T;                                  {$If defined(doinline)} inline; {$EndIf}
    class operator Explicit          (const a : TArrayIterator<T>) : T;                                  {$If defined(doinline)} inline; {$EndIf}
    class operator Add               (const a: TArrayIterator<T>; const b: integer): TArrayIterator<T>;  {$If defined(doinline)} inline; {$EndIf}
    class operator Subtract          (const a: TArrayIterator<T>; const b: integer): TArrayIterator<T>;  {$If defined(doinline)} inline; {$EndIf}
    class operator LessThan          (const a,b : TArrayIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator LessThanOrEqual   (const a,b : TArrayIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator Equal             (const a,b : TArrayIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator NotEqual          (const a,b : TArrayIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator GreaterThanOrEqual(const a,b : TArrayIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator GreaterThan       (const a,b : TArrayIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}

    property Value: T read GetValue write SetValue;
  end;

  TArrayRIterator<T> = record
  public
    type
      PVal = ^T;
  private
    Ref: PVal;

    function GetValue: T;               {$If defined(doinline)} inline; {$EndIf}
    procedure SetValue(const Value: T); {$If defined(doinline)} inline; {$EndIf}

  public
    procedure Init(Ref: PVal); {$If defined(doinline)} inline; {$EndIf}

    class operator Implicit          (const a : TArrayRIterator<T>) : T;                                  {$If defined(doinline)} inline; {$EndIf}
    class operator Explicit          (const a : TArrayRIterator<T>) : T;                                  {$If defined(doinline)} inline; {$EndIf}
    class operator Add               (const a: TArrayRIterator<T>; const b: integer): TArrayRIterator<T>; {$If defined(doinline)} inline; {$EndIf}
    class operator Subtract          (const a: TArrayRIterator<T>; const b: integer): TArrayRIterator<T>; {$If defined(doinline)} inline; {$EndIf}
    class operator LessThan          (const a,b : TArrayRIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator LessThanOrEqual   (const a,b : TArrayRIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator Equal             (const a,b : TArrayRIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator NotEqual          (const a,b : TArrayRIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator GreaterThanOrEqual(const a,b : TArrayRIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}
    class operator GreaterThan       (const a,b : TArrayRIterator<T>) : Boolean;                          {$If defined(doinline)} inline; {$EndIf}

    property Value: T read GetValue write SetValue;
  end;

implementation

{ TArrayIterator<T> }

class operator TArrayIterator<T>.Add(const a: TArrayIterator<T>; const b: integer): TArrayIterator<T>;
begin
  result.Ref := a.Ref;
  inc(result.Ref, b);
end;

class operator TArrayIterator<T>.Equal(const a, b: TArrayIterator<T>): Boolean;
begin
  result := a.Ref = b.Ref;
end;

class operator TArrayIterator<T>.Explicit(const a: TArrayIterator<T>): T;
begin
  result := a.Ref^;
end;

function TArrayIterator<T>.GetValue: T;
begin
  result := Ref^;
end;

class operator TArrayIterator<T>.GreaterThan(const a, b: TArrayIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) > NativeUInt(b.Ref);
end;

class operator TArrayIterator<T>.GreaterThanOrEqual(const a, b: TArrayIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) >= NativeUInt(b.Ref);
end;

class operator TArrayIterator<T>.Implicit(const a: TArrayIterator<T>): T;
begin
  result := a.Ref^;
end;

procedure TArrayIterator<T>.Init(Ref: PVal);
begin
  Self.Ref := Ref;
end;

class operator TArrayIterator<T>.LessThan(const a, b: TArrayIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) < NativeUInt(b.Ref);
end;

class operator TArrayIterator<T>.LessThanOrEqual(const a, b: TArrayIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) <= NativeUInt(b.Ref);
end;

class operator TArrayIterator<T>.NotEqual(const a, b: TArrayIterator<T>): Boolean;
begin
  result := a.Ref <> b.Ref;
end;

procedure TArrayIterator<T>.SetValue(const Value: T);
begin
  Ref^ := Value;
end;

class operator TArrayIterator<T>.Subtract(const a: TArrayIterator<T>; const b: integer): TArrayIterator<T>;
begin
  result.Ref := a.Ref;
  dec(result.Ref, b);
end;

{ TArrayRIterator<T> }

class operator TArrayRIterator<T>.Add(const a: TArrayRIterator<T>; const b: integer): TArrayRIterator<T>;
begin
  result.Ref := a.Ref;
  dec(result.Ref, b);
end;

class operator TArrayRIterator<T>.Equal(const a, b: TArrayRIterator<T>): Boolean;
begin
  result := a.Ref = b.Ref;
end;

class operator TArrayRIterator<T>.Explicit(const a: TArrayRIterator<T>): T;
begin
  result := a.Ref^;
end;

function TArrayRIterator<T>.GetValue: T;
begin
  result := Ref^;
end;

class operator TArrayRIterator<T>.GreaterThan(const a, b: TArrayRIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) < NativeUInt(b.Ref);
end;

class operator TArrayRIterator<T>.GreaterThanOrEqual(const a, b: TArrayRIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) <= NativeUInt(b.Ref);
end;

class operator TArrayRIterator<T>.Implicit(const a: TArrayRIterator<T>): T;
begin
  result := a.Ref^;
end;

procedure TArrayRIterator<T>.Init(Ref: PVal);
begin
  Self.Ref := Ref;
end;

class operator TArrayRIterator<T>.LessThan(const a, b: TArrayRIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) > NativeUInt(b.Ref);
end;

class operator TArrayRIterator<T>.LessThanOrEqual(const a, b: TArrayRIterator<T>): Boolean;
begin
  result := NativeUInt(a.Ref) >= NativeUInt(b.Ref);
end;

class operator TArrayRIterator<T>.NotEqual(const a, b: TArrayRIterator<T>): Boolean;
begin
  result := a.Ref <> b.Ref;
end;

procedure TArrayRIterator<T>.SetValue(const Value: T);
begin
  Ref^ := Value;
end;

class operator TArrayRIterator<T>.Subtract(const a: TArrayRIterator<T>; const b: integer): TArrayRIterator<T>;
begin
  result.Ref := a.Ref;
  inc(result.Ref, b);
end;

end.
