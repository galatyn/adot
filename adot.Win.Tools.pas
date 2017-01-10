unit adot.Win.Tools;
{$ALIGN ON}
{$MINENUMSIZE 4}
{$WARN SYMBOL_PLATFORM OFF}

{ Definition of classes/record types:

  TDiskLetters = class
    DiskPathToLetter etc.

  TMessenger = class
    Allows to send / receive messages by any component (even if component is not inherited from TWinControl)

  TProcess = class
    EnumerateProcesses, GetIntegrityLevel etc.

  TSecurity = class
    AddDbgPrivileges / AddPrivilege / NullDACL etc.

  TSharedMem = class
    Shared memory class.

  TWinFileUtils = class
    CopyFile using WinAPI with callback etc.

  TWebBrowserUtils = class
    SetWebBrowserMode

}
interface

uses
  adot.Types,
  adot.Collections,
  Winapi.TlHelp32,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.PsAPI,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Masks,
  System.SysUtils,
  System.SyncObjs,
  System.Math,
  System.Threading,
  System.Classes,
  Registry;

type

  { EnumerateProcesses, GetIntegrityLevel etc }
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

    { Usually you need to call TSecurity.AddDbgPrivileges to read info about
      system processes, otherwise QueryImagePath may return "" for example. }
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

    class function GetProcessesLockingFile(const FileName: string; var ProcessNames: TArray<string>): boolean; static;
  end;

  { AddDbgPrivileges / AddPrivilege / NullDACL etc }
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

  { Shared memory class }
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

  { DiskPathToLetter etc }
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

  { CopyFile using WinAPI with callback etc }
  TWinFileUtils = class
  public

    { Copy file functions (from simple to feature-rich):
      1. TFileUtils.CopyFile:
           uses standard Delphi streams, UI will freeze until operation is complete.
      2. TWinFileUtils.CopyFile:
           uses Windows function CopyFileEx, UI will freeze until operation is complete.
      3. TVCLFileUtils.Copyfile:
           uses standard Delphi streams, UI will not freeze.
      4. TCopyFileProgressDlg.CopyFile:
           uses either Delphi streams or CopyFileEx, UI will not freeze,
           progress bar with cancel command will be available for long operations). }
    class function CopyFile(const SrcFileName,DstFileName: string; out ErrorMessage: string; ProgressProc: TCopyFileProgressProc): boolean; overload;
    class function CopyFile(const SrcFileName,DstFileName: string; out ErrorMessage: string): boolean; overload;
  end;

  { To receive/process messages in component when it is not inherited from TWinControl.
    For every message (call of WndProc) will call handler twice - before and after standard processing. }
  { Allows to send / receive messages by any component (even if component is not inherited from TWinControl) }	
  TMessenger = class
  public
    type
      TOnMessage = procedure(var AMessage: TMessage) of object;
      TOnMessageRef = reference to procedure(var AMessage: TMessage);

  protected
    FOnMessage: TOnMessage;
    FOnMessageRef: TOnMessageRef;
    FWnd: HWND;

    procedure WndProc(var Message: TMessage);

  public
    constructor Create; overload;
    constructor Create(AMessageHandler: TOnMessageRef); overload;
    constructor Create(AMessageHandler: TOnMessage); overload;
    destructor Destroy; override;

    procedure Post(Msg: cardinal; wParam: NativeUInt = 0; lParam: NativeInt = 0);

    property OnMessage: TOnMessage read FOnMessage write FOnMessage;
    property OnMessageRef: TOnMessageRef read FOnMessageRef write FOnMessageRef;
    property Handle: HWND read FWnd;
  end;

  TRestartManager = class
  protected
    class destructor Destroy;

  public
    class var
      RestartManagerLibrary: THandle;
      InitCount: int64;

    type
      RM_APP_TYPE      = DWORD;
      RM_SHUTDOWN_TYPE = DWORD;
      RM_REBOOT_REASON = DWORD;

      RM_UNIQUE_PROCESS = record
        dwProcessId: DWORD;
        ProcessStartTime: TFileTime;
      end;
      PRM_UNIQUE_PROCESS = ^RM_UNIQUE_PROCESS;

    const
      CCH_RM_MAX_APP_NAME   = 255;
      CCH_RM_MAX_SVC_NAME   = 63;

    type
      RM_PROCESS_INFO = record
        Process             : RM_UNIQUE_PROCESS;
        strAppName          : array[0..CCH_RM_MAX_APP_NAME] of WideChar;
        strServiceShortName : array[0..CCH_RM_MAX_SVC_NAME] of WideChar;
        ApplicationType     : RM_APP_TYPE;
        AppStatus           : ULONG;
        TSSessionId         : DWORD;
        bRestartable        : BOOL;
      end;
      PRM_PROCESS_INFO = ^RM_PROCESS_INFO;

    const
      restartmanagerlib = 'Rstrtmgr.dll';

      RM_SESSION_KEY_LEN    = SizeOf(TGUID);
      CCH_RM_SESSION_KEY    = RM_SESSION_KEY_LEN*2;
      RM_INVALID_TS_SESSION = -1;
      RM_INVALID_PROCESS    = -1;

      RmUnknownApp  = 0;
      RmMainWindow  = 1;
      RmOtherWindow = 2;
      RmService     = 3;
      RmExplorer    = 4;
      RmConsole     = 5;
      RmCritical    = 1000;

      RmForceShutdown          = $01;
      RmShutdownOnlyRegistered = $10;

      RmRebootReasonNone             = $0;
      RmRebootReasonPermissionDenied = $1;
      RmRebootReasonSessionMismatch  = $2;
      RmRebootReasonCriticalProcess  = $4;
      RmRebootReasonCriticalService  = $8;
      RmRebootReasonDetectedSelf     = $10;

    class var
      RmStartSession:
        function(
          var pSessionHandle : DWORD;
              dwSessionFlags : DWORD;
              strSessionKey  : LPWSTR): DWORD; stdcall;

      RmRegisterResources:
        function(
          dwSessionHandle : DWORD;
          nFiles          : UINT;
          rgsFilenames    : PPWideChar;
          nApplications   : UINT;
          rgApplications  : PRM_UNIQUE_PROCESS;
          nServices       : UINT;
          rgsServiceNames : PPWideChar): DWORD; stdcall;

      RmGetList:
        function(
          dwSessionHandle   : DWORD;
          pnProcInfoNeeded  : PUINT;
          pnProcInfo        : PUINT;
          rgAffectedApps    : PRM_PROCESS_INFO;
          lpdwRebootReasons : LPDWORD): DWORD; stdcall;

      RmShutdown:
        function(
          dwSessionHandle : DWORD;
          lActionFlags    : ULONG;
          fnStatus        : Pointer): DWORD; stdcall;

      RmRestart:
        function(
          dwSessionHandle : DWORD;
          dwRestartFlags  : DWORD;
          fnStatus        : Pointer): DWORD; stdcall;

      RmEndSession:
        function(dwSessionHandle: DWORD): DWORD; stdcall;

    class function Initialize: Boolean; static;
    class procedure Uninitialize; static;

    class function GetProcessesLockingFile(FileName: string; var ProcessNames: TArray<string>): boolean; static;
  end;

  TWinUtils = class
  public

    { Analog of AppActivate in VB:
      https://msdn.microsoft.com/en-us/library/dyz95fhy%28v=vs.90%29.aspx?f=255&MSPPError=-2147217396 }
    class function AppActivate(const AppName: string): Boolean; static;
  end;

  TWebBrowserUtils = class
  public
    class function SetWebBrowserMode(Mode: TIEMode; AppName: string): boolean; static;
  end;

implementation

type
  TApiExt = class
  private
    class var
      FOrdinal: TApiExt;
    var
      Kernel32Module: HModule;
      varQueryFullProcessImageName: function (hProcess: THandle; dwFlags: DWORD;
        lpFilename: LPCWSTR; var nSize: DWORD): BOOL; stdcall;
      varQueryFullProcessImageNameA: function (hProcess: THandle; dwFlags: DWORD;
        lpFilename: LPCSTR; var nSize: DWORD): BOOL; stdcall;
      varQueryFullProcessImageNameW: function (hProcess: THandle; dwFlags: DWORD;
        lpFilename: LPCWSTR; var nSize: DWORD): BOOL; stdcall;

    class function GetOrdinal: TApiExt; static;
    class destructor ClassDestroy; static;

  public
    constructor Create;
    destructor Destroy; override;

    function QueryFullProcessImageName(hProcess: THandle; dwFlags: DWORD; lpFilename: LPCWSTR; var nSize: DWORD): Boolean;

    class property Ordinal: TApiExt read GetOrdinal;
  end;

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
const
  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
var
  p: THandle;
  l: DWORD;
  DesiredAccess: cardinal;
begin
  result := '';

  { QueryFullProcessImageName (more reliable, than GetModuleFileNameEx) }
  if System.SysUtils.Win32MajorVersion >= 6 then
    DesiredAccess := PROCESS_QUERY_LIMITED_INFORMATION { Windows Vista or higher }
  else
    DesiredAccess := PROCESS_QUERY_INFORMATION;
  p := OpenProcess(DesiredAccess, False, AProcessId);
  if p<>0 then
    try
      SetLength(Result, AMaxLen);
      l := length(Result)-1;
      if TApiExt.Ordinal.QueryFullProcessImageName(p, 0, PChar(Result), l) then
      begin
        SetLength(Result, l);
        Exit;
      end;
    finally
      CloseHandle(p);
    end;

  { GetModuleFileNameEx }
  p := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, AProcessId);
  if p<>0 then
    try
      SetLength(Result, AMaxLen);
      l := length(Result)-1;
      SetLength(Result, GetModuleFileNameEx(p, 0, PChar(Result), length(Result) - 1));
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

class function TProcess.GetProcessesLockingFile(const FileName: string; var ProcessNames: TArray<string>): boolean;
begin
  try
    result := TRestartManager.GetProcessesLockingFile(FileName, ProcessNames);
  except
    result := False;
    SetLength(ProcessNames, 0);
  end;
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

// \device\mup\my-srv\soft\09.01\delphifeatures.docx
// '\Device\LanmanRedirector\;V:000000000007d3bd\my-srv\soft
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
    SetLength(Dst, MAX_PATH*8);
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

{ TWinFileUtils }

type
  TWinCopyFileData = record
    Progress    : TCopyFileProgressProc;
    Cancel      : Bool;
    SrcFileName : string;
    DstFileName : string;
  end;
  PWinCopyFileData = ^TWinCopyFileData;

function CopyFile_CallBackProc(
  TotalFileSize          : LARGE_INTEGER; // total file size, in bytes
  TotalBytesTransferred  : LARGE_INTEGER; // total number of bytes transferred
  StreamSize             : LARGE_INTEGER; // total number of bytes for this stream
  StreamBytesTransferred : LARGE_INTEGER; // total number of bytes transferred for this stream
  dwStreamNumber         : DWORD;         // the current stream
  dwCallbackReason       : DWORD;         // reason for callback
  hSourceFile            : THANDLE;       // handle to the source file
  hDestinationFile       : THandle;       // handle to the destination file
  lpData                 : PWinCopyFileData
): DWord; stdcall;
var
  CopyfileInfo: TCopyFileInfo;
  Cancel: Boolean;
begin
  if (lpData=nil) or not Assigned(lpData.Progress) then
    Exit(PROGRESS_CONTINUE);
  CopyfileInfo.FileSize    := TotalFileSize.QuadPart;
  CopyfileInfo.Copied      := TotalBytesTransferred.QuadPart;
  CopyfileInfo.SrcFileName := lpData.SrcFileName;
  CopyfileInfo.DstFileName := lpData.DstFileName;
  Cancel                   := False;
  lpData.Progress(CopyfileInfo, Cancel);
  result := IfThen(Cancel, PROGRESS_CANCEL, PROGRESS_CONTINUE);
end;

class function TWinFileUtils.CopyFile(const SrcFileName, DstFileName: string; out ErrorMessage: string; ProgressProc: TCopyFileProgressProc): boolean;
var
  Data: TWinCopyFileData;
  Error: DWORD;
begin
  Data.Progress    := ProgressProc;
  Data.Cancel      := False;
  Data.SrcFileName := SrcFileName;
  Data.DstFileName := DstFileName;
  result := Winapi.Windows.CopyFileEx(PChar(SrcFileName), PChar(DstFileName), @CopyFile_CallBackProc, @Data, @Data.Cancel, 0);
  if not result then
  begin
    Error := GetLastError;
    case Error of
      ERROR_ACCESS_DENIED:
        ErrorMessage := 'Ingen tilgang';
      ERROR_REQUEST_ABORTED:
        ErrorMessage := 'Kopiering ble avbrutt';
      else
        ErrorMessage := 'Feilkode ' + IntToStr(Error)+'-'+SysErrorMessage(Error);
    end;
  end;
end;

class function TWinFileUtils.CopyFile(const SrcFileName, DstFileName: string; out ErrorMessage: string): boolean;
begin
  result := CopyFile(SrcFileName, DstFileName, ErrorMessage, nil);
end;

{ TMessenger }

constructor TMessenger.Create;
begin
  inherited;
  FWnd := AllocateHWnd(WndProc);
end;

destructor TMessenger.Destroy;
begin
  inherited;
  DeallocateHWnd(FWnd);
end;

procedure TMessenger.Post(Msg: cardinal; wParam: NativeUInt = 0; lParam: NativeInt = 0);
begin
  PostMessage(Handle, Msg, wParam, lPAram);
end;

constructor TMessenger.Create(AMessageHandler: TOnMessage);
begin
  Create;
  FOnMessage := AMessageHandler;
end;

constructor TMessenger.Create(AMessageHandler: TOnMessageRef);
begin
  Create;
  FOnMessageRef := AMessageHandler;
end;

procedure TMessenger.WndProc(var Message: TMessage);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Message);
  if Assigned(FOnMessageRef) then
    FOnMessageRef(Message);
end;

{ TRestartManager }

function GetSystemDir: String;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetSystemDirectory(PChar(result), Length(Result)));
  Result := IncludeTrailingPathDelimiter(Result);
end;

class function TRestartManager.Initialize: Boolean;
begin
  Inc(InitCount);
  if InitCount <> 1 then
    Result := RestartManagerLibrary <> 0
  else
  begin
    Result := System.SysUtils.Win32MajorVersion >= 6; { Windows Vista or higher }
    if not Result then
      Exit;
    RestartManagerLibrary := LoadLibrary(PChar(GetSystemDir + restartmanagerlib));
    Result := RestartManagerLibrary <> 0;
    if not Result then
      Exit;
    RmStartSession      := GetProcAddress(RestartManagerLibrary, 'RmStartSession');
    RmRegisterResources := GetProcAddress(RestartManagerLibrary, 'RmRegisterResources');
    RmGetList           := GetProcAddress(RestartManagerLibrary, 'RmGetList');
    RmShutdown          := GetProcAddress(RestartManagerLibrary, 'RmShutdown');
    RmRestart           := GetProcAddress(RestartManagerLibrary, 'RmRestart');
    RmEndSession        := GetProcAddress(RestartManagerLibrary, 'RmEndSession');
  end;
end;

class procedure TRestartManager.Uninitialize;
begin
  if InitCount <= 0 then
    Exit;
  Dec(InitCount);
  if (InitCount = 0) and (RestartManagerLibrary <> 0) then
  begin
    FreeLibrary(RestartManagerLibrary);
    RestartManagerLibrary := 0;

    RmStartSession      := nil;
    RmRegisterResources := nil;
    RmGetList           := nil;
    RmShutdown          := nil;
    RmRestart           := nil;
    RmEndSession        := nil;
  end;
end;

class destructor TRestartManager.Destroy;
begin
  if RestartManagerLibrary <> 0 then
  begin
    InitCount := 1;
    Uninitialize;
  end;
end;

class function TRestartManager.GetProcessesLockingFile(FileName: string; var ProcessNames: TArray<string>): boolean;
var
  ErrorCode: DWord;
  SessionHandle: DWORD;
  SessionKey: string;
  PFileName: PWideChar;
  ProcInfoNeededCount: UINT;
  ProcInfoCount: UINT;
  ProcessInfoArr: array of RM_PROCESS_INFO;
  RebootReason: DWORD;
  i: Integer;
begin

  { RmStartSession }
  SetLength(ProcessNames, 0);
  result := False;
  if not TRestartManager.Initialize then
    Exit;
  SessionKey := StringOfChar(#0, CCH_RM_SESSION_KEY);
  ErrorCode := RmStartSession(SessionHandle, 0, PChar(SessionKey));
  if ErrorCode <> ERROR_SUCCESS then
    Exit;

  try

    { RmRegisterResources }
    FileName := FileName + #0#0;
    PFileName := PChar(FileName);
    ErrorCode := RmRegisterResources(SessionHandle, 1, @PFileName, 0, nil, 0, nil);
    if ErrorCode <> ERROR_SUCCESS then
      Exit;

    { RmGetList }
    ProcInfoNeededCount := 0;
    ProcInfoCount := 0;
    SetLength(ProcessInfoArr, ProcInfoCount);
    ErrorCode := RmGetList(SessionHandle, @ProcInfoNeededCount, @ProcInfoCount, nil, @RebootReason);
    case ErrorCode of
      ERROR_SUCCESS:
        begin
          Result := True;
          Exit;
        end;
      ERROR_MORE_DATA:
        ;
      else
        Exit;
    end;
    ProcInfoCount := ProcInfoNeededCount + 10;
    SetLength(ProcessInfoArr, ProcInfoCount);
    ProcInfoNeededCount := 0;
    ErrorCode := RmGetList(SessionHandle, @ProcInfoNeededCount, @ProcInfoCount, @ProcessInfoArr[0], @RebootReason);
    if ErrorCode <> ERROR_SUCCESS then
      Exit;

    { fill ProcessNames }
    SetLength(ProcessNames, ProcInfoCount);
    for i := 0 to ProcInfoCount-1 do
    begin
      ProcessNames[i] := TProcess.QueryImagePath(ProcessInfoArr[i].Process.dwProcessId);
      if ProcessNames[i] = '' then
        ProcessNames[i] := ProcessInfoArr[i].strAppName;
      ProcessNames[i] := ProcessNames[i] + format('[%d]', [ProcessInfoArr[i].Process.dwProcessId]);
    end;

    Result := True;

  finally
    RmEndSession(SessionHandle);
  end;
end;

{ TWinUtils }

class function TWinUtils.AppActivate(const AppName: string): Boolean;
var
  W: HWND;
begin
  W := FindWindow(nil, PChar(AppName));
  result := (W <> 0) and SetForegroundWindow(W);
end;

{ TWebBrowserUtils }

class function TWebBrowserUtils.SetWebBrowserMode(Mode: TIEMode; AppName: string): boolean;
const
  REG_KEY = 'Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION';

var
  Reg: TRegistry;
  ValSet, ValRead, PPos : integer;
  VersionStr : string;

begin
  Result := False;
  ValRead := 0;
  ValSet := 10000;

  if AppName = '' then
    Exit;

   case Mode of
     iemIE7 : ValSet := 7000;
     iemIE8 : ValSet := 8000;
     iemIE9 : ValSet := 9000;
     iemIE10 : ValSet := 10000;
     iemIE11 : ValSet := 11000;
   end;


  if Mode = iemIEInstalled then
  begin
    Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
    try
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      if Reg.OpenKey('SOFTWARE\Microsoft\Internet Explorer', false) then
      begin
        if Reg.ValueExists('svcVersion') then
        begin
          VersionStr := Reg.ReadString('svcVersion');
          PPos := Pos('.', VersionStr);
          if PPos > 0 then
          begin
            VersionStr := Copy(VersionStr, 1, PPos - 1);
            if TryStrToInt(VersionStr, ValSet) then
            begin
              ValSet := 1000*ValSet
            end
            else
            begin
              ValSet := 10000;
            end;
          end;
        end;
      end;

    finally
      Reg.Free;
    end;
  end;

  try
    Reg := TRegistry.Create(KEY_READ or KEY_WOW64_64KEY);
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey(REG_KEY, false) then
      begin
        if Reg.ValueExists(AppName) then
        begin
          ValRead := Reg.ReadInteger(AppName);
        end;

        Reg.CloseKey;
      end;
    finally
      Reg.Free;
    end;

    if  ValSet > ValRead then
    begin
      Reg := TRegistry.Create(KEY_WRITE or KEY_WOW64_64KEY);
      try
        if Reg.OpenKey(REG_KEY, True) then
        begin
          Reg.WriteInteger(AppName, ValSet);
          Reg.CloseKey;
          Result:=True;
        end;

      finally
        Reg.Free;
      end;
    end
    else
    begin
      Result:=True;
    end;

  except;
    Result:=false;
  end;

end;

{ TApiExt }

constructor TApiExt.Create;
begin
  Kernel32Module := LoadLibrary(kernel32);
  if (Kernel32Module <> 0) then
  begin
    varQueryFullProcessImageNameA := GetProcAddress(Kernel32Module, 'QueryFullProcessImageNameA');
    varQueryFullProcessImageNameW := GetProcAddress(Kernel32Module, 'QueryFullProcessImageNameW');
    varQueryFullProcessImageName := varQueryFullProcessImageNameW;
  end;
end;

destructor TApiExt.Destroy;
begin
  if Kernel32Module <> 0 then
  begin
    FreeLibrary(Kernel32Module);
    Kernel32Module := 0;
  end;
  varQueryFullProcessImageName := nil;
  varQueryFullProcessImageNameA := nil;
  varQueryFullProcessImageNameW := nil;
  inherited;
end;

class destructor TApiExt.ClassDestroy;
begin
  FreeAndNil(FOrdinal);
end;

class function TApiExt.GetOrdinal: TApiExt;
begin
  if FOrdinal = nil then
    FOrdinal := TApiExt.Create;
  result := FOrdinal;
end;

function TApiExt.QueryFullProcessImageName(hProcess: THandle; dwFlags: DWORD; lpFilename: LPCWSTR; var nSize: DWORD): Boolean;
begin
  if Assigned(varQueryFullProcessImageName) then
    result := varQueryFullProcessImageName(hProcess, dwFlags, lpFilename, nSize)
  else
    result := False;
end;

end.
