unit adot.Types;

{ Definition of classes/record types:

  TCustomCachable = class
    Basic class with support ICachable interface.

  TEmptyRec = record
    types.

}
interface

uses
  System.Generics.Defaults;

const
  NullGuid: TGUID = '{00000000-0000-0000-0000-000000000000}';

type

  { types }

  TEmptyRec = record end;
  TSetOfByte = set of Byte;
  TSetOfAnsiChar = set of AnsiChar;

  TProcConst<T> = reference to procedure (const Arg1: T);
  TProcConst<T1,T2> = reference to procedure (const Arg1: T1; const Arg2: T2);
  TProcConst<T1,T2,T3> = reference to procedure (const Arg1: T1; const Arg2: T2; const Arg3: T3);

  TProcVar<T> = reference to procedure (var Arg1: T);
  TProcVar<T1,T2> = reference to procedure (var Arg1: T1; var Arg2: T2);
  TProcVar<T1,T2,T3> = reference to procedure (var Arg1: T1; var Arg2: T2; var Arg3: T3);

  TProcVar1<T> = reference to procedure (var Arg1: T);
  TProcVar1<T1,T2> = reference to procedure (Arg1: T1; var Arg2: T2);
  TProcVar1<T1,T2,T3> = reference to procedure (Arg1: T1; Arg2: T2; var Arg3: T3);

  { interfaces }

  IInterfacedObject<T> = interface(IInterface)
    function GetData: T;
    procedure SetData(AData: T);
    function GetRefCount: integer;

    property Data: T read GetData write SetData;
    property ReferenceCount: integer read GetRefCount;
  end;

  IArithmetic<T> = interface(IInterface)
    function Add(Left: T; Right: T): T;
    function Subtract(Left: T; Right: T): T;
    function Multiply(Left: T; Right: T): T;
    function Divide(Left: T; Right: T): T;
    function Negative(Value: T): T;
  end;

  ICachable = interface
    ['{C61BCB34-157D-4302-A8F9-96BCCF48483A}']
    procedure BegynnKalkulasjon;
    procedure AvsluttKalkulasjon;
    procedure OmstartKalkulasjon;
    function GetKalkulasjonErAktiv: Boolean;
    function GetKalkulasjonBalanse: Integer;
  end;

  { classes }

  TCustomArithmetic<T> = class(TSingletonImplementation, IArithmetic<T>)
  public
    function Add(Left: T; Right: T): T; virtual; abstract;
    function Subtract(Left: T; Right: T): T; virtual; abstract;
    function Multiply(Left: T; Right: T): T; virtual; abstract;
    function Divide(Left: T; Right: T): T; virtual; abstract;
    function Negative(Value: T): T; virtual; abstract;
  end;

  { A non-reference-counted IInterface implementation. }
  TNRCInterfacedObject = TSingletonImplementation;

  { Basic class with support ICachable interface }
  TCustomCachable = class(TNRCInterfacedObject, ICachable)
  protected
    function GetKalkulasjonErAktiv: Boolean; virtual; abstract;
    function GetKalkulasjonBalanse: Integer; virtual; abstract;
  public
    procedure BegynnKalkulasjon; virtual; abstract;
    procedure AvsluttKalkulasjon; virtual; abstract;
    procedure OmstartKalkulasjon; virtual; abstract;

    property KalkulasjonErAktiv: Boolean read GetKalkulasjonErAktiv;
    property KalkulasjonBalanse: Integer read GetKalkulasjonBalanse;
  end;

  TCopyFileInfo = record
    FileSize    : int64;
    Copied      : int64;
    SrcFileName : string;
    DstFileName : string;
  end;
  TCopyFileProgressProc = reference to procedure(const Info: TCopyFileInfo; var Cancel: boolean);
  TCopyStreamProgressProc = reference to procedure(const Transferred: int64; var Cancel: boolean);

implementation

end.
