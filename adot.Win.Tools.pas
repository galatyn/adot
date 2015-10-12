unit adot.Win.Tools;
{$ALIGN ON}
{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.TlHelp32, Winapi.Windows, Winapi.PsAPI, System.Generics.Collections,
  System.Generics.Defaults, System.Masks, System.SysUtils, System.SyncObjs,
  System.Math, System.Threading, adot.Generics.Collections, System.Classes;

const
  MAX_PATH_LEN                         = MAX_PATH*8;
  STATUS_SUCCESS                       = $00000000;
  STATUS_INVALID_INFO_CLASS            = $C0000003;
  STATUS_INFO_LENGTH_MISMATCH          = $C0000004;
  STATUS_INVALID_DEVICE_REQUEST        = $C0000010;
  STATUS_FUNCTION_IS_NOT_AVAILABLE     = $CFFFFFFF;
  ObjectNameInformation                = 1;
  FileDirectoryInformation             = 1;
  FileNameInformation                  = 9;
  SystemProcessesAndThreadsInformation = 5;
  SystemHandleInformation              = 16;

type
  NT_STATUS = Cardinal;

  PSYSTEM_THREADS = ^SYSTEM_THREADS;
  SYSTEM_THREADS  = record
    KernelTime         : LARGE_INTEGER;
    UserTime           : LARGE_INTEGER;
    CreateTime         : LARGE_INTEGER;
    WaitTime           : ULONG;
    StartAddress       : Pointer;
    UniqueProcess      : DWORD;
    UniqueThread       : DWORD;
    Priority           : Integer;
    BasePriority       : Integer;
    ContextSwitchCount : ULONG;
    State              : Longint;
    WaitReason         : Longint;
  end;

  PSYSTEM_PROCESS_INFORMATION = ^SYSTEM_PROCESS_INFORMATION;
  SYSTEM_PROCESS_INFORMATION = record
    NextOffset                   : ULONG;
    ThreadCount                  : ULONG;
    Reserved1                    : array [0..5] of ULONG; // Что такое, пока не понятно...
    CreateTime                   : FILETIME;
    UserTime                     : FILETIME;
    KernelTime                   : FILETIME;
    ModuleNameLength             : WORD;
    ModuleNameMaxLength          : WORD;
    ModuleName                   : PWideChar;
    BasePriority                 : ULONG;
    ProcessID                    : ULONG;
    InheritedFromUniqueProcessID : ULONG;
    HandleCount                  : ULONG;
    Reserved2                    : array[0..1] of ULONG; // Что такое, пока не понятно...
    PeakVirtualSize              : ULONG;
    VirtualSize                  : ULONG;
    PageFaultCount               : ULONG;
    PeakWorkingSetSize           : ULONG;
    WorkingSetSize               : ULONG;
    QuotaPeakPagedPoolUsage      : ULONG;
    QuotaPagedPoolUsage          : ULONG;
    QuotaPeakNonPagedPoolUsage   : ULONG;
    QuotaNonPagedPoolUsage       : ULONG;
    PageFileUsage                : ULONG;
    PeakPageFileUsage            : ULONG;
    PrivatePageCount             : ULONG;
    ReadOperationCount           : LARGE_INTEGER;
    WriteOperationCount          : LARGE_INTEGER;
    OtherOperationCount          : LARGE_INTEGER;
    ReadTransferCount            : LARGE_INTEGER;
    WriteTransferCount           : LARGE_INTEGER;
    OtherTransferCount           : LARGE_INTEGER;
    ThreadInfo                   : array [0..0] of SYSTEM_THREADS;
  end;

  PSYSTEM_HANDLE_INFORMATION = ^SYSTEM_HANDLE_INFORMATION;
  SYSTEM_HANDLE_INFORMATION = record
    ProcessId        : DWORD;
    ObjectTypeNumber : Byte;
    Flags            : Byte;
    Handle           : Word;
    pObject          : Pointer;
    GrantedAccess    : DWORD;
  end;

  PSYSTEM_HANDLE_INFORMATION_EX = ^SYSTEM_HANDLE_INFORMATION_EX;
  SYSTEM_HANDLE_INFORMATION_EX = record
    NumberOfHandles : dword;
    Information     : array [0..0] of SYSTEM_HANDLE_INFORMATION;
  end;

  PFILE_NAME_INFORMATION = ^FILE_NAME_INFORMATION;
  FILE_NAME_INFORMATION = record
    FileNameLength : ULONG;
    FileName       : array [0..MAX_PATH_LEN - 1] of WideChar;
  end;

  PUNICODE_STRING = ^TUNICODE_STRING;
  TUNICODE_STRING = record
    Length        : WORD;
    MaximumLength : WORD;
    Pad           : DWORD;
    Buffer        : array [0..MAX_PATH_LEN - 1] of WideChar;
  end;

  POBJECT_NAME_INFORMATION = ^TOBJECT_NAME_INFORMATION;
  TOBJECT_NAME_INFORMATION = record
    Name : TUNICODE_STRING;
  end;

  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  IO_STATUS_BLOCK = record
    Status      : NT_STATUS;
    Information : DWORD;
  end;

  TProcess = class
  public
    type
      TInfo = record
        pe32: PROCESSENTRY32;
      end;
      TOnProcess = reference to procedure(const AInfo: TInfo);
      TThreadInfo = record
        t32: TThreadEntry32;
      end;
      TOnThread = reference to procedure(const AInfo: TThreadInfo);
      TWindowInfo = record
        Wnd: hwnd;
      end;
      TOnWindow = reference to procedure(const AInfo: TWindowInfo);
      TIntegrityLevel = (ilUntrusted, ilLow, ilMedium, ilHigh, ilSystem, ilProtectedProcess, ilUnknown);
    const
      Integrities: array[TIntegrityLevel] of String = (
        'Untrusted', 'Low', 'Medium', 'High', 'System', 'ProtectedProcess', 'Unknown'
      );

  protected
    type
      PEnumWndRec = ^TEnumWndRec;
      TEnumWndRec = record
        Callback: TOnWindow;
      end;

    class function EnumerateProcessThreads(pid: DWORD; ASkipCurrProcess: boolean;
      AOnThread: TOnThread): Boolean; static;

  public

    // Usually you need to call TSecurity.AddDbgPrivileges to read info about
    // system processes, otherwise QueryImagePath may return "" for example.
    class function EnumerateProcesses(ASkipCurrProcess: boolean;
      AOnProcess: TOnProcess):Boolean; static;
    class function EnumerateThreads(pid: DWORD; AOnThread: TOnThread):Boolean; overload; static;
    class function EnumerateThreads(ASkipCurrProcess: boolean; AOnThread: TOnThread):Boolean; overload; static;
    class function QueryImagePath(AProcessId: DWORD; AMaxLen: integer = 4096):String; static;
    class function ListByName(ASkipCurrProcess: boolean;
      const AProcessNameMask: string): TList<TInfo>; static;
    class function EnumerateThreadWindows(AThreadId: DWORD; AOnWindow: TOnWindow):Boolean; static;
    class function GetIntegrityLevel(AProcessId: DWORD; var Integrity: DWORD):Boolean; overload; static;
    class function GetIntegrityLevel(AProcessId: DWORD; var Integrity: TIntegrityLevel):Boolean; overload; static;
    class function GetIntegrityLevel(var Integrity: TIntegrityLevel):Boolean; overload; static;
    class function GetIntegrityLevel: String; overload; static;
  end;

  TSecurity = class
  public
    type
      TPrivilege = (
        SE_CREATE_TOKEN_NAME, SE_ASSIGNPRIMARYTOKEN_NAME, SE_LOCK_MEMORY_NAME,
        SE_INCREASE_QUOTA_NAME, SE_UNSOLICITED_INPUT_NAME, SE_MACHINE_ACCOUNT_NAME,
        SE_TCB_NAME, SE_SECURITY_NAME, SE_TAKE_OWNERSHIP_NAME,
        SE_LOAD_DRIVER_NAME, SE_SYSTEM_PROFILE_NAME, SE_SYSTEMTIME_NAME,
        SE_PROF_SINGLE_PROCESS_NAME, SE_INC_BASE_PRIORITY_NAME, SE_CREATE_PAGEFILE_NAME,
        SE_CREATE_PERMANENT_NAME, SE_BACKUP_NAME, SE_RESTORE_NAME,
        SE_SHUTDOWN_NAME, SE_DEBUG_NAME, SE_AUDIT_NAME,
        SE_SYSTEM_ENVIRONMENT_NAME, SE_CHANGE_NOTIFY_NAME, SE_REMOTE_SHUTDOWN_NAME,
        SE_UNDOCK_NAME, SE_SYNC_AGENT_NAME, SE_ENABLE_DELEGATION_NAME,
        SE_MANAGE_VOLUME_NAME, SE_INTERACTIVE_LOGON_NAME, SE_NETWORK_LOGON_NAME,
        SE_BATCH_LOGON_NAME, SE_SERVICE_LOGON_NAME, SE_DENY_INTERACTIVE_LOGON_NAME,
        SE_DENY_NETWORK_LOGON_NAME, SE_DENY_BATCH_LOGON_NAME, SE_DENY_SERVICE_LOGON_NAME,
        SE_REMOTE_INTERACTIVE_LOGON_NAME, SE_DENY_REMOTE_INTERACTIVE_LOGON_NAME
      );
    const
      Privileges: array[TPrivilege] of string =
      (
        'SeCreateTokenPrivilege', 'SeAssignPrimaryTokenPrivilege', 'SeLockMemoryPrivilege',
        'SeIncreaseQuotaPrivilege', 'SeUnsolicitedInputPrivilege', 'SeMachineAccountPrivilege',
        'SeTcbPrivilege', 'SeSecurityPrivilege', 'SeTakeOwnershipPrivilege',
        'SeLoadDriverPrivilege', 'SeSystemProfilePrivilege', 'SeSystemtimePrivilege',
        'SeProfileSingleProcessPrivilege', 'SeIncreaseBasePriorityPrivilege', 'SeCreatePagefilePrivilege',
        'SeCreatePermanentPrivilege', 'SeBackupPrivilege', 'SeRestorePrivilege',
        'SeShutdownPrivilege', 'SeDebugPrivilege', 'SeAuditPrivilege',
        'SeSystemEnvironmentPrivilege', 'SeChangeNotifyPrivilege', 'SeRemoteShutdownPrivilege',
        'SeUndockPrivilege', 'SeSyncAgentPrivilege', 'SeEnableDelegationPrivilege',
        'SeManageVolumePrivilege', 'SeInteractiveLogonRight', 'SeNetworkLogonRight',
        'SeBatchLogonRight', 'SeServiceLogonRight', 'SeDenyInteractiveLogonRight',
        'SeDenyNetworkLogonRight', 'SeDenyBatchLogonRight', 'SeDenyServiceLogonRight',
        'SeRemoteInteractiveLogonRight', 'SeDenyRemoteInteractiveLogonRight'
      );
    type
      TDescrAttr = record
        Descriptor: TSecurityDescriptor;
        Attrs: TSecurityAttributes;
      end;
  public
    // Example: addPrivilege('SeDebugPrivilege'); addPrivilege('SeImpersonatePrivilege');
    class function AddDbgPrivileges: Boolean; static;
    class function AddPrivilege(tok: THandle; const AName: string): boolean; overload; static;
    class function AddPrivilege(pid: DWORD; const AName: string): boolean; overload; static;
    class function AddPrivilege(const AName: string): boolean; overload; static;
    class function ImpersonateThreadAsLoggedUser: Boolean; static;
    // https://msdn.microsoft.com/en-us/library/windows/desktop/aa379286(v=vs.85).aspx
    // NullDACL  - grant access to any user
    // EmptyDACL - grnt no access
    class function NullDACL(var s: TDescrAttr): Boolean; static;
  end;

  TSharedMem = class(THandleObject)
  protected
    FName: string;
    FSize: Cardinal;
    FCreated: Boolean;
    FFileView: Pointer;
  public
    constructor Create(const Name: string; Size: Cardinal; resetDACL : boolean = false);
    destructor Destroy; override;

    property Name: string read FName;
    property Size: Cardinal read FSize;
    property Memory: Pointer read FFileView;
    property Created: Boolean read FCreated;
  end;

  TWinApiExt = class
  protected
    type
      NT_STATUS = Cardinal;
      TZwQuerySystemInformation = function(ASystemInformationClass: DWORD;
        ASystemInformation: Pointer; ASystemInformationLength: DWORD;
        AReturnLength: PDWORD): NT_STATUS; stdcall;
      TNtQueryInformationFile = function(FileHandle: THandle;
        IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: Pointer;
        Length: DWORD; FileInformationClass: DWORD): NT_STATUS; stdcall;
      TNtQueryObject = function(ObjectHandle: THandle;
        ObjectInformationClass: DWORD; ObjectInformation: Pointer;
        ObjectInformationLength: ULONG;ReturnLength: PDWORD): NT_STATUS; stdcall;
      TGetFileInformationByHandleEx = function(hFile: THandle;
        FileInformationClass: DWord; lpFileInformation: pointer;
        dwBufferSize: DWord): BOOL; stdcall;
    var
      FNtDllLib, FKernel32Lib: THandle;
      FZwQuerySystemInformation: TZwQuerySystemInformation;
      FNtQueryInformationFile: TNtQueryInformationFile;
      FNtQueryObject: TNtQueryObject;
      FGetFileInformationByHandleEx: TGetFileInformationByHandleEx;
  public
    constructor Create;
    destructor Destroy; override;

    function ZwQuerySystemInformation(ASystemInformationClass: DWORD;
      ASystemInformation: Pointer; ASystemInformationLength: DWORD;
      AReturnLength: PDWORD): NT_STATUS;

    function NtQueryInformationFile(FileHandle: THandle;
      IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: Pointer;
      Length: DWORD; FileInformationClass: DWORD
    ): NT_STATUS;

    function NtQueryObject(ObjectHandle: THandle;
      ObjectInformationClass: DWORD; ObjectInformation: Pointer;
      ObjectInformationLength: ULONG; ReturnLength: PDWORD): NT_STATUS;

    function GetFileInformationByHandleEx(hFile: THandle;
      FileInformationClass: DWord; lpFileInformation: pointer;
      dwBufferSize: DWord): BOOL;
  end;

  TDiskLetters = class
  private
    FLetters: TDictionary<string, char>;
    class function NormalizePath(const APath: string): String; static;
    class procedure GetDiskLetters(var ALetters: TDictionary<string, char>); static;

  public
    constructor Create;
    destructor Destroy; override;

    function DiskPathToLetter(const APath: string): String;
    function ResolvePath(const APath: string): String;
  end;

  TLockedFiles = class
  private
    function RunTask(ParamIdx: integer): ITask;
    function EmptyTask: ITask;
  protected
    type
      TTaskParams = record
        FileHandle: THandle;
        FileName: string;
      end;

    var
      FApi: TWinApiExt;
      FTaskParams: array of TTaskParams;

    function DoZwQuerySystemInformation(ASysInfoClass: DWORD; var AData: Pointer): Boolean;
    function GetFileHandles(var AHandleInfo: PSYSTEM_HANDLE_INFORMATION_EX; var AFileType: Byte): Boolean;

    // GetFileInformationByHandleEx
    procedure GetFileNameFromHandleV3(hFile: THandle; Idx: integer);

    // NtQueryObject
    procedure GetFileNameFromHandleV2(hFile: THandle; Idx: integer);

    // NtQueryInformationFile
    procedure GetFileNameFromHandleV1(hFile: THandle; Idx: integer);

    //GetFinalPathNameByHandle
    procedure GetFileNameFromHandleV0(hFile: THandle; Idx: integer);

    procedure GetFileNameFromHandle(hFile: THandle; Idx: integer);
  public
    type
      TFileInfo = record
        PID: THandle;
        FilePath: string;
      end;

    constructor Create;
    destructor Destroy; override;

    function GetFilesOpenedByProcesses(AAddPrivileges: Boolean = True): TList<TFileInfo>;
  end;

// AH: We should remove it when Embarcadero include it into Winapi.Windows
{$EXTERNALSYM QueryFullProcessImageName}
function QueryFullProcessImageName(hProcess: THandle; dwFlags: DWORD;
  lpFilename: LPCWSTR; var nSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM QueryFullProcessImageNameA}
function QueryFullProcessImageNameA(hProcess: THandle; dwFlags: DWORD;
  lpFilename: LPCSTR; var nSize: DWORD): BOOL; stdcall;
{$EXTERNALSYM QueryFullProcessImageNameW}
function QueryFullProcessImageNameW(hProcess: THandle; dwFlags: DWORD;
  lpFilename: LPCWSTR; var nSize: DWORD): BOOL; stdcall;

implementation

function QueryFullProcessImageName; external kernel32 name 'QueryFullProcessImageNameW';
function QueryFullProcessImageNameA; external kernel32 name 'QueryFullProcessImageNameA';
function QueryFullProcessImageNameW; external kernel32 name 'QueryFullProcessImageNameW';

{ TProcess }

class function TProcess.EnumerateProcesses(ASkipCurrProcess: boolean; AOnProcess: TOnProcess):Boolean;
var
  h: THandle;
  info: TInfo;
  cpid: DWORD;
begin
  h := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  Result := h <> INVALID_HANDLE_VALUE;
  if Result then
    try
      if ASkipCurrProcess then
        cpid := GetCurrentProcessId
      else
        cpid := 0;
      info.pe32.dwSize := sizeof(info.pe32);
      if Process32First(h, info.pe32) then
        repeat
          if not (ASkipCurrProcess and (cpid=info.pe32.th32ProcessID)) then
            if (Info.pe32.th32ProcessID<>0) then // pid=0 for pseudo process "idle"
              AOnProcess(info);
        until not Process32Next(h, info.pe32);
    finally
      CloseHandle(h);
    end;
end;

// PID=high(dword) -> all processes
class function TProcess.EnumerateProcessThreads(pid: DWORD; ASkipCurrProcess: boolean;
  AOnThread: TOnThread): Boolean;
var
  Info: TThreadInfo;
  h: THandle;
  CurPID: dword;
begin
  Info.t32.dwSize := sizeof(Info.t32);
  h := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  Result := h<>INVALID_HANDLE_VALUE;
  if Result then
    try
      if ASkipCurrProcess then
        CurPID := getCurrentProcessID
      else
        CurPID := 0;
      repeat
        if ASkipCurrProcess and (CurPID=Info.t32.th32OwnerProcessID) or
          (pid<>high(dword)) and (pid<>Info.t32.th32OwnerProcessID)
        then
          Continue;
        if (Info.t32.th32ThreadID<>0) and (Info.t32.th32OwnerProcessID<>0) then
          AOnThread(Info);
      until not Thread32Next(h, Info.t32);
    finally
      CloseHandle(h);
    end;
end;

class function TProcess.EnumerateThreads(pid: DWORD; AOnThread: TOnThread): Boolean;
begin
  Result := EnumerateProcessThreads(pid, False, AOnThread);
end;

class function TProcess.EnumerateThreads(ASkipCurrProcess: boolean;
  AOnThread: TOnThread): Boolean;
begin
  Result := EnumerateProcessThreads(High(DWORD), ASkipCurrProcess, AOnThread);
end;

function EnumThreadWndProc(W: hwnd; L: TProcess.PEnumWndRec): Bool; stdcall;
var
  Info: TProcess.TWindowInfo;
begin
  Info.Wnd := W;
  L.Callback(Info);
  EnumChildWindows(W, @EnumThreadWndProc, NativeInt(l));
  Result := True;
end;

class function TProcess.EnumerateThreadWindows(AThreadId: DWORD;
  AOnWindow: TOnWindow): Boolean;
var
  r: TEnumWndRec;
begin
  r.Callback := AOnWindow;
  EnumThreadWindows(AThreadId, @EnumThreadWndProc, NativeInt(@r));
  Result := True;
end;

class function TProcess.GetIntegrityLevel(AProcessId: DWORD;
  var Integrity: DWORD): Boolean;
type
  PTokenMandatoryLabel = ^TTokenMandatoryLabel;
  TTokenMandatoryLabel = packed record
    SidAttrs: TSidAndAttributes;
  end;
var
  hProcess: THandle;
  hToken: THandle;
  dwSize: DWORD;
  pbCount: PByte;
  pdwProcIL: PDWORD;
  pTIL: PTokenMandatoryLabel;
begin
  Result := False;
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION, False, AProcessId);
  if hProcess=0 then
    Exit;
  if OpenProcessToken(hProcess, TOKEN_QUERY, hToken) then
  begin
    if not GetTokenInformation(hToken, TokenIntegrityLevel, nil, 0, dwSize) and
      (GetLastError=ERROR_INSUFFICIENT_BUFFER)
    then
    begin
      pTIL := AllocMem(dwSize);
      if pTIL<>nil then
      begin
        if GetTokenInformation(hToken, TokenIntegrityLevel, pTIL, dwSize, dwSize) then
        begin
          pbCount := PByte(GetSidSubAuthorityCount(pTIL.SidAttrs.Sid));
          if pbCount<>nil then
          begin
            pdwProcIL := GetSidSubAuthority(pTIL^.SidAttrs.Sid, pbCount^ - 1);
            Result := pdwProcIL<>nil;
            if Result then
              Integrity := pdwProcIL^;
          end;
        end;
        ReallocMem(pTIL, 0);
      end;
    end;
    CloseHandle(hToken);
  end;
  CloseHandle(hProcess);
end;

class function TProcess.GetIntegrityLevel(AProcessId: DWORD;
  var Integrity: TIntegrityLevel): Boolean;
const
  UNTRUSTED_RID         = $00000000;
  LOW_RID               = $00001000;
  MEDIUM_RID            = $00002000;
  HIGH_RID              = $00003000;
  SYSTEM_RID            = $00004000;
  PROTECTED_PROCESS_RID = $00005000;
var
  IL: DWORD;
begin
  Integrity := ilUnknown;
  Result := GetIntegrityLevel(AProcessId, IL);
  if Result then
    case IL of
      UNTRUSTED_RID         : Integrity := ilUntrusted;
      LOW_RID               : Integrity := ilLow;
      MEDIUM_RID            : Integrity := ilMedium;
      HIGH_RID              : Integrity := ilHigh;
      SYSTEM_RID            : Integrity := ilSystem;
      PROTECTED_PROCESS_RID : Integrity := ilProtectedProcess;
    end;
end;

class function TProcess.GetIntegrityLevel(
  var Integrity: TIntegrityLevel): Boolean;
begin
  result := GetIntegrityLevel(GetCurrentProcessId, Integrity);
end;

class function TProcess.ListByName(ASkipCurrProcess: boolean;
  const AProcessNameMask: string): TList<TInfo>;
var
  Dst: TList<TInfo>;
begin
  result := TList<TInfo>.Create;
  try
    Dst := result;
    EnumerateProcesses(ASkipCurrProcess,
      procedure(const AInfo: TInfo)
      var
        s: String;
      begin
        s := QueryImagePath(AInfo.pe32.th32ProcessID);
        if MatchesMask(s, AProcessNameMask) then
          Dst.Add(AInfo);
      end);
  except
    result.Free;
    raise;
  end;
end;

class function TProcess.QueryImagePath(AProcessId: DWORD; AMaxLen: integer = 4096): String;
var
  p: THandle;
  l: DWORD;
begin
  result := '';
  p := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if p<>0 then
    try
      setlength(Result, AMaxLen);
      l := length(Result)-1;
      // AH: QueryFullProcessImageName is more reliable, than GetModuleFileNameEx.
      if QueryFullProcessImageName(p, 0, PChar(Result), l) then
        setlength(Result, l)
      else
        setlength(Result, GetModuleFileNameEx(p, 0, PChar(Result), length(Result) - 1));
    finally
      CloseHandle(p);
    end;
end;

class function TProcess.GetIntegrityLevel: String;
var
  il: TIntegrityLevel;
begin
  GetIntegrityLevel(il);
  Result := Integrities[il];
end;

{ TSecurity }

class function TSecurity.AddPrivilege(tok: THandle; const AName: string): boolean;
var
  t: PTokenPrivileges;
  l, n: cardinal;
begin
  Result := False;
  n := SizeOf(TTokenPrivileges) + SizeOf(TLUIDAndAttributes);
  t := AllocMem(n);
  try
    t.PrivilegeCount := 1;
    t.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    if not LookupPrivilegeValue('', PChar(AName), t.Privileges[0].Luid) then
      exit;
    Result := AdjustTokenPrivileges(tok, False, t^, 0, nil, l);
  finally
    ReallocMem(t, 0);
  end;
end;

class function TSecurity.AddPrivilege(pid: DWORD; const AName: string): boolean;
var
  h, tok: THandle;
begin
  h := OpenProcess(process_all_access, False, pid);
  Result := h<>0;
  if Result then
    try
      Result := OpenProcessToken(h, TOKEN_ADJUST_PRIVILEGES, tok);
      if Result then
        try
          Result := AddPrivilege(tok, AName);
        finally
          CloseHandle(tok);
        end;
    finally
      CloseHandle(h);
    end;
end;

class function TSecurity.AddDbgPrivileges: Boolean;
var
  b1,b2: Boolean;
begin
  b1 := addPrivilege('SeDebugPrivilege');
  b2 := addPrivilege('SeImpersonatePrivilege');
  result := b1 and b2;
end;

class function TSecurity.AddPrivilege(const AName: string): boolean;
begin
  result := AddPrivilege(GetCurrentProcessId, AName);
end;

class function TSecurity.NullDACL(var s: TDescrAttr): Boolean;
begin
  Result :=
    InitializeSecurityDescriptor(@s.Descriptor, SECURITY_DESCRIPTOR_REVISION) and
    SetSecurityDescriptorDacl(@s.Descriptor, True, nil, False);
  if not Result then
    Exit;
  s.Attrs.nLength := SizeOf(s.Attrs);
  s.Attrs.lpSecurityDescriptor := @s.Descriptor;
  s.Attrs.bInheritHandle := False;
end;

class function TSecurity.ImpersonateThreadAsLoggedUser: Boolean;
var
  d: TList<TProcess.TInfo>;
  h, tok: THandle;
begin
  // we should be on the input desktop here!
  result := False;

  // We may need SeDebugPrivilege to open any (almost) process.
  // We may need SeImpersonatePrivilege to impersonate current thread in
  // security context of logged on user.
  AddDbgPrivileges;

  d := TProcess.ListByName(True, '*\explorer.exe');
  try
    if d.Count = 0 then
      Exit;
    h := OpenProcess(PROCESS_QUERY_INFORMATION, False, d[0].pe32.th32ProcessID);
    if h<>0 then
      try
        if OpenProcessToken(h, TOKEN_QUERY or TOKEN_DUPLICATE or TOKEN_IMPERSONATE, tok) then
          try
            Result := ImpersonateLoggedOnUser(tok);
          finally
            CloseHandle(tok);
          end;
      finally
        CloseHandle(h);
      end;
  finally
    FreeAndNil(d);
  end;
end;

{ TSharedMem }

constructor TSharedMem.Create(const Name: string; Size: Cardinal; resetDACL : boolean = False);
var
  Sec: TSecurity.TDescrAttr;
begin
  FName := Name;
  FSize := Size;
  if not resetDACL then
    FHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, Size, PChar(FName))
  else
  begin
    TSecurity.NullDACL(Sec);
    FHandle := CreateFileMapping(INVALID_HANDLE_VALUE, @Sec.Attrs, PAGE_READWRITE, 0, Size, PChar(FName))
  end;
  FCreated := (FHandle<>0) and (GetLastError <> ERROR_ALREADY_EXISTS);
  if FHandle<>0 then
    FFileView := MapViewOfFile(FHandle, FILE_MAP_WRITE, 0, 0, Size);
end;

destructor TSharedMem.Destroy;
begin
  if FFileView <> nil then
    UnmapViewOfFile(FFileView);
  inherited Destroy;
end;

// http://stackoverflow.com/questions/8726906/delphi-finding-the-process-that-is-accessing-a-file-from-my-program

{ TWinApiExt }

constructor TWinApiExt.Create;
begin
  FNtDllLib := LoadLibrary('ntdll.dll');
  FKernel32Lib := LoadLibrary(kernel32);
  FZwQuerySystemInformation := GetProcAddress(FNtDllLib, 'ZwQuerySystemInformation');
  FNtQueryInformationFile := GetProcAddress(FNtDllLib, 'NtQueryInformationFile');
  FNtQueryObject := GetProcAddress(FNtDllLib, 'NtQueryObject');
  FGetFileInformationByHandleEx := GetProcAddress(FKernel32Lib, 'GetFileInformationByHandleEx');
end;

destructor TWinApiExt.Destroy;
begin
  FZwQuerySystemInformation := nil;
  FNtQueryInformationFile := nil;
  FNtQueryObject := nil;
  FGetFileInformationByHandleEx := nil;
  if FNtDllLib<>0 then
    FreeLibrary(FNtDllLib);
  if FKernel32Lib<>0 then
    FreeLibrary(FKernel32Lib);
  inherited;
end;

function TWinApiExt.GetFileInformationByHandleEx(hFile: THandle;
  FileInformationClass: DWord; lpFileInformation: pointer;
  dwBufferSize: DWord): BOOL;
begin
  result := Assigned(FGetFileInformationByHandleEx) and
    FGetFileInformationByHandleEx(hFile, FileInformationClass, lpFileInformation, dwBufferSize);
end;

function TWinApiExt.NtQueryInformationFile(FileHandle: THandle;
  IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: Pointer;
  Length,FileInformationClass: DWORD): NT_STATUS;
begin
  if not Assigned(FNtQueryInformationFile) then
    result := STATUS_FUNCTION_IS_NOT_AVAILABLE
  else
    result := FNtQueryInformationFile(FileHandle, IoStatusBlock, FileInformation, Length, FileInformationClass);
end;

function TWinApiExt.NtQueryObject(ObjectHandle: THandle;
  ObjectInformationClass: DWORD; ObjectInformation: Pointer;
  ObjectInformationLength: ULONG; ReturnLength: PDWORD): NT_STATUS;
begin
  if not Assigned(FNtQueryObject) then
    result := STATUS_FUNCTION_IS_NOT_AVAILABLE
  else
    result := FNtQueryObject(ObjectHandle, ObjectInformationClass, ObjectInformation, ObjectInformationLength, ReturnLength);
end;

function TWinApiExt.ZwQuerySystemInformation(ASystemInformationClass: DWORD;
  ASystemInformation: Pointer; ASystemInformationLength: DWORD;
  AReturnLength: PDWORD): NT_STATUS;
begin
  if not Assigned(FZwQuerySystemInformation) then
    result := STATUS_FUNCTION_IS_NOT_AVAILABLE
  else
    result := FZwQuerySystemInformation(ASystemInformationClass, ASystemInformation, ASystemInformationLength, AReturnLength);
end;

{ TDiskLetters }

constructor TDiskLetters.Create;
begin
  FLetters := TDictionary<string, char>.Create;
  GetDiskLetters(FLetters);
end;

destructor TDiskLetters.Destroy;
begin
  FreeAndNil(FLetters);
  inherited;
end;

// \device\mup\ma-osl-f01\felles\andrei\09.01\delphifeatures.docx
// '\Device\LanmanRedirector\;V:000000000007d3bd\MA-OSL-F01\Felles
class procedure TDiskLetters.GetDiskLetters(var ALetters: TDictionary<string, char>);
const
  Lan = '\device\lanmanredirector\;';
var
  Letter: Char;
  Src,Dst,s: string;
  i: Integer;
begin
  ALetters.Clear;
  for Letter := 'a' to 'z' do
  begin
    Src := Letter + ':';
    SetLength(Dst, MAX_PATH_LEN);
    SetLength(Dst, QueryDosDevice(PChar(Src), PChar(Dst), Length(Dst)));
    if Dst='' then
      Continue;
    Dst := NormalizePath(Dst);
    ALetters.Add(Dst, Letter);
    if Dst.StartsWith(Lan) and (Dst.Substring(Length(Lan)+1, 1)=':') then
    begin
      i := Dst.IndexOf('\', Length(Lan) + 1);
      if i>0 then
      begin
        s := '\device\mup\' + Dst.Substring(i+1);
        ALetters.Add(s, Letter);
        s := '\' + Dst.Substring(i+1);
        ALetters.Add(s, Letter);
      end;
    end;
  end;
end;

class function TDiskLetters.NormalizePath(const APath: string): String;
begin
  result := AnsiLowerCase(Trim(ExcludeTrailingPathDelimiter(APath)));
end;

function TDiskLetters.DiskPathToLetter(const APath: string): String;
var
  Letter: Char;
begin
  if FLetters.TryGetValue(NormalizePath(APath), Letter) then
    result := Letter
  else
    result := '';
end;

function TDiskLetters.ResolvePath(const APath: string): String;
var
  s,l: string;
  i,j: Integer;
begin
  result := NormalizePath(APath);
  s := result;
  while s<>'' do
  begin
    l := DiskPathToLetter(s);
    if l<>'' then
      Exit(l + ':' + Copy(result, length(s)+low(result), high(integer)));
    j := -1;
    for i := High(s) downto Low(s) do
      if (s[i]='\') or (s[i]='/') then
      begin
        j := i;
        Break;
      end;
    SetLength(s, Max(0,j-Low(s)));
  end;
end;

{ TLockedFiles }

constructor TLockedFiles.Create;
begin
  FApi := TWinApiExt.Create;
end;

destructor TLockedFiles.Destroy;
begin
  FreeAndNil(FApi);
  inherited;
end;

function TLockedFiles.DoZwQuerySystemInformation(ASysInfoClass: DWORD;
  var AData: Pointer): Boolean;
var
  dwSize: DWORD;
  ntStatus: NT_STATUS;
begin
  AData := nil;
  dwSize := $40000;
  repeat
    dwSize := dwSize * 2;
    ReallocMem(AData, dwSize);
    ntStatus := FAPI.ZwQuerySystemInformation(ASysInfoClass, AData, dwSize, nil);
  until ntStatus <> STATUS_INFO_LENGTH_MISMATCH;
  result := ntStatus = STATUS_SUCCESS;
end;

// NtQueryInformationFile to get filename from handle
// This function provides better (readable) names for files from network, for example
// NtQueryInformationFile : '\MA-OSL-F01\Felles\Andrei\09.01\DelphiFeatures.docx'
// NtQueryObject          : '\Device\Mup\MA-OSL-F01\Felles\Andrei\09.01\DelphiFeatures.docx'
procedure TLockedFiles.GetFileNameFromHandleV1(hFile: THandle; Idx: integer);
var
  FileNameInfo: FILE_NAME_INFORMATION;
  IoStatusBlock: IO_STATUS_BLOCK;
  Status: NT_STATUS;
begin
  FTaskParams[Idx].FileName := '';
  ZeroMemory(@FileNameInfo, SizeOf(FILE_NAME_INFORMATION));
  Status := FAPI.NtQueryInformationFile(hFile, @IoStatusBlock, @FileNameInfo, SizeOf(FileNameInfo.FileName), FileNameInformation);
  if Status = STATUS_SUCCESS then
    FTaskParams[Idx].FileName := FileNameInfo.FileName;
end;

// NtQueryObject to get filename from handle
procedure TLockedFiles.GetFileNameFromHandleV2(hFile: THandle; Idx: integer);
var
  ObjectNameInfo: TOBJECT_NAME_INFORMATION;
  dwReturn: DWORD;
  Status: NT_STATUS;
begin
  FTaskParams[Idx].FileName := '';
  ZeroMemory(@ObjectNameInfo, SizeOf(ObjectNameInfo));
  Status := FAPI.NtQueryObject(hFile, ObjectNameInformation, @ObjectNameInfo, SizeOf(ObjectNameInfo.Name.Buffer), @dwReturn);
  if Status = STATUS_SUCCESS then
    FTaskParams[Idx].FileName := ObjectNameInfo.Name.Buffer;
end;

// This function GetFileInformationByHandleEx to get filename from handle
// it also hangs and result files in format:
// \Windows\winsxs\amd64_microsoft.windows.gdiplus_6595b64144ccf1df_1.1.7601.18946_none_2b27281071eac12c
procedure TLockedFiles.GetFileNameFromHandleV3(hFile: THandle; Idx: integer);
const
  FileNameInfo = 2;
var
  FNInfo: FILE_NAME_INFORMATION;
begin
  FTaskParams[Idx].FileName := '';
  if FAPI.GetFileInformationByHandleEx(hFile, FileNameInfo, @FNInfo, SizeOF(FILE_NAME_INFORMATION)) then
    FTaskParams[Idx].FileName := FNInfo.FileName;
end;

// GetFinalPathNameByHandle to get filename from handle
// this function is very good option, when available
procedure TLockedFiles.GetFileNameFromHandleV0(hFile: THandle; Idx: integer);
begin

  // \Device\HarddiskVolume2\Windows\winsxs\...
  // TDiskLetters class can be used to resolve to regular filename
//  setlength(result, MAX_PATH*8);
//  setlength(result, GetFinalPathNameByHandle(hFile, PChar(result), Length(result), FILE_NAME_NORMALIZED or VOLUME_NAME_NT));

  // \\?\C:\Windows\winsxs\... -> C:\Windows\winsxs\...
  setlength(FTaskParams[Idx].FileName, MAX_PATH*8);
  setlength(FTaskParams[Idx].FileName, GetFinalPathNameByHandle(hFile, PChar(FTaskParams[Idx].FileName), Length(FTaskParams[Idx].FileName), FILE_NAME_NORMALIZED or VOLUME_NAME_DOS));
  if FTaskParams[Idx].FileName.StartsWith('\\?\') then
    FTaskParams[Idx].FileName := '\' + FTaskParams[Idx].FileName.Substring(3);
  if FTaskParams[Idx].FileName.StartsWith('\\') and (FTaskParams[Idx].FileName.SubString(3,1)=':') then
    FTaskParams[Idx].FileName := FTaskParams[Idx].FileName.SubString(2);
end;

function TLockedFiles.GetFileHandles(var AHandleInfo: PSYSTEM_HANDLE_INFORMATION_EX; var AFileType: Byte): Boolean;
var
  hFile: THandle;
  I: Integer;
  p: SYSTEM_HANDLE_INFORMATION;
  CurProcessId: DWord;
begin
  result := False;
  AHandleInfo := nil;
  AFileType := 255;
  hFile := CreateFile('NUL', GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
  if hFile <> INVALID_HANDLE_VALUE then
  try
    CurProcessId := GetCurrentProcessId;
    if DoZwQuerySystemInformation(SystemHandleInformation, pointer(AHandleInfo)) then
      for I := 0 to AHandleInfo.NumberOfHandles - 1 do
      begin
        p := AHandleInfo.Information[I];
        if (p.Handle = hFile) and (p.ProcessId = CurProcessId) then
        begin
          AFileType := p.ObjectTypeNumber;
          result := True;
          Break;
        end;
      end;
  finally
    CloseHandle(hFile);
  end;
end;

procedure TLockedFiles.GetFileNameFromHandle(hFile: THandle; Idx: integer);
begin
  FTaskParams[Idx].FileName := '';
  GetFileNameFromHandleV0(hFile, Idx);
  if FTaskParams[Idx].FileName<>'' then Exit;
  GetFileNameFromHandleV1(hFile, Idx);
  if FTaskParams[Idx].FileName<>'' then Exit;
  GetFileNameFromHandleV2(hFile, Idx);
  if FTaskParams[Idx].FileName<>'' then Exit;
  GetFileNameFromHandleV3(hFile, Idx);
end;

function TLockedFiles.RunTask(ParamIdx: integer): ITask;
begin
  result := TTask.Create(
    procedure
    begin
      GetFileNameFromHandle(FTaskParams[ParamIdx].FileHandle, ParamIdx);
    end);
  result.Start;
end;

function TLockedFiles.EmptyTask: ITask;
begin
  result := TTask.Create(
    procedure
    begin
    end);
  result.Start;
end;

function TLockedFiles.GetFilesOpenedByProcesses(AAddPrivileges: Boolean = True): TList<TFileInfo>;
const
  PoolSize = 64;
var
  hDupFile, hProcess: THandle;
  HandleInfo: PSYSTEM_HANDLE_INFORMATION_EX;
  I,J: Integer;
  ObjectTypeOfFile: Byte;
  Letters: TDiskLetters;
  Rec: TFileInfo;
  Tasks: array of ITask;
  Files: TMultimap<DWord, THandle>;
  Keys: TMultimap<DWord, THandle>.TKeyEnumerator; // ProcessId -> Remote handle of file
  Values: TMultimap<DWord, THandle>.TValueEnumerator;
begin
  result := TList<TFileInfo>.Create(TDelegatedComparer<TFileInfo>.Create(
    function(const A,B: TFileInfo): integer
    begin
      result := Sign(A.PID-B.PID);
      if result=0 then
        result := AnsiCompareText(A.FilePath, B.FilePath);
    end));
  try
    SetLength(FTaskParams, PoolSize);
    SetLength(Tasks, PoolSize);
    HandleInfo := nil;
    Letters := nil;
    Files := nil;
    try
      Files := TMultimap<DWord, THandle>.Create;
      Letters := TDiskLetters.Create;

      // https://msdn.microsoft.com/en-us/library/windows/hardware/ff567052%28v=vs.85%29.aspx?f=255&MSPPError=-2147217396
      // if the user has SeChangeNotifyPrivilege (described in the Microsoft Windows SDK documentation),
      // ZwQueryInformationFile returns the full path in all cases.
      if AAddPrivileges then
      begin
        TSecurity.addPrivilege('SeDebugPrivilege');
        TSecurity.addPrivilege('SeChangeNotifyPrivilege');
      end;

      if not GetFileHandles(HandleInfo, ObjectTypeOfFile) then
        Exit;
      for I := 0 to Min(PoolSize, HandleInfo.NumberOfHandles) - 1 do
        if (HandleInfo.Information[I].ProcessId=3788) and
          (HandleInfo.Information[I].ObjectTypeNumber = ObjectTypeOfFile)
        then
          Files.Add(HandleInfo.Information[I].ProcessId, HandleInfo.Information[I].Handle);
      ReallocMem(HandleInfo, 0);

      for I := 0 to PoolSize-1 do
        Tasks[i] := EmptyTask;
      Keys := Files.Keys.GetEnumerator;
      while Keys.MoveNext do
      begin
        hProcess := OpenProcess(PROCESS_DUP_HANDLE, True, Keys.Current);
        if hProcess > 0 then
        try
          Values := Files.Values[Keys.Current];
          while Values.MoveNext do
            if DuplicateHandle(hProcess, Values.Current, GetCurrentProcess, @hDupFile, 0, False, DUPLICATE_SAME_ACCESS) then
            begin

              J := TTask.WaitForAny(Tasks, 500);
              if J<0 then
              begin
                for J := 0 to PoolSize-1 do
                  Tasks[J].Cancel;
                J := 0;
              end;
              if (Tasks[J].Status=TTaskStatus.Completed) and (FTaskParams[J].FileName<>'') then
              begin
                Rec.FilePath := Letters.ResolvePath(FTaskParams[J].FileName);
                Rec.PID := Keys.Current;
                result.Add(Rec);
              end;
              if FTaskParams[J].FileHandle<>0 then
                CloseHandle(FTaskParams[J].FileHandle);
              FTaskParams[J].FileHandle := hDupFile;
              FTaskParams[J].FileName := '';
              Tasks[J] := RunTask(I);

            end;
        finally
          CloseHandle(hProcess);
        end;
      end;
      TTask.WaitForAll(Tasks, 500);

(*      for I := 0 to HandleInfo.NumberOfHandles - 1 do
        if HandleInfo.Information[I].ProcessId=3788 then
        if HandleInfo.Information[I].ObjectTypeNumber = ObjectTypeOfFile then
        begin
          hProcess := OpenProcess(PROCESS_DUP_HANDLE, True, HandleInfo.Information[I].ProcessId);
          if hProcess > 0 then
          try
            if DuplicateHandle(hProcess, HandleInfo.Information[I].Handle, GetCurrentProcess, @hDupFile, 0, False, DUPLICATE_SAME_ACCESS) then
            try
              Rec.FilePath := GetFileNameFromHandle(hDupFile);
              if Rec.FilePath <> '' then
              begin
                Rec.FilePath := Letters.ResolvePath(Rec.FilePath);
                Rec.PID := HandleInfo.Information[I].ProcessId;
                result.Add(Rec);
              end;
            finally
              CloseHandle(hDupFile);
            end;
          finally
            CloseHandle(hProcess);
          end;
        end;  *)

    finally
      ReallocMem(HandleInfo, 0);
      FreeAndNil(Letters);
      FreeAndNil(Files);
      for i := 0 to High(FTaskParams) do
        if FTaskParams[i].FileHandle<>0 then
          CloseHandle(FTaskParams[i].FileHandle);
    end;

    result.Sort;
  except
    result.Free;
    raise;
  end;
end;

end.
