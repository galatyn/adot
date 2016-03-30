unit adot.Types;

interface

uses
  System.Generics.Defaults;

const
  NullGuid: TGUID = '{00000000-0000-0000-0000-000000000000}';
  TeamViewerExeName = 'TeamViewerQS_no-idc9gek29q.exe';
  scShiftF1 = $2070;

type

  { types }

  TEmptyRec = record end;
  TSetOfByte = set of Byte;
  TSetOfAnsiChar = set of AnsiChar;

  { interfaces }

  IInterfacedObject<T> = interface(IInterface)
    function GetData: T;
    procedure SetData(AData: T);
    function GetRefCount: integer;

    property Data: T read GetData write SetData;
    property ReferenceCount: integer read GetRefCount;
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

  { A non-reference-counted IInterface implementation. }
  TNRCInterfacedObject = TSingletonImplementation;

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

implementation

end.
