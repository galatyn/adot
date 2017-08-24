unit adot.Types;

{ Definition of classes/record types:

  TCustomCachable = class
    Basic class with support ICachable interface.

  TEmptyRec = record
    types.

}
interface

uses
  System.Generics.Defaults,
  System.SysUtils;

const
  NullGuid: TGUID = '{00000000-0000-0000-0000-000000000000}';
  RecordTypes = [tkInteger, tkChar, tkEnumeration, tkFloat, tkSet, tkMethod, tkWChar, tkRecord, tkInt64, tkPointer, tkProcedure];

type

  { types }

  TBoolean = (BoolTrue, BoolFalse, BoolAny);
  TCycleAction = (caContinue, caBreak);
  TIEMode = (iemIE7, iemIE8, iemIE9, iemIE10, iemIE11, iemIEInstalled);

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

  TFuncCompareValues<T> = reference to function(const Left,Right: T): integer;
  TFuncFilterValueIndex<T> = reference to function(const Value: T; ValueIndex: integer): boolean;

  EForbiddenOperation = class(Exception);

  { interfaces }

  IInterfacedObject<T> = interface(IInterface)
    ['{A6E4E6FF-DE6E-41E8-9B30-69365C0C397C}']

    function GetData: T;
    procedure SetData(const AData: T);
    function GetRefCount: integer;
    function Extract: T;

    property Data: T read GetData write SetData;
    property ReferenceCount: integer read GetRefCount;
  end;

  IArithmetic<T> = interface(IInterface)
    ['{EA1E0AFC-90E4-4497-9194-ACCDF3012E08}']

    function Add(Left: T; Right: T): T;
    function Subtract(Left: T; Right: T): T;
    function Multiply(Left: T; Right: T): T;
    function Divide(Left: T; Right: T): T;
    function Negative(Value: T): T;
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
