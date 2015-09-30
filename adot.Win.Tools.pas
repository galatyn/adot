unit adot.Win.Tools;

interface

uses
  Winapi.TlHelp32, Winapi.Windows, Winapi.PsAPI, System.Generics.Collections,
  System.Generics.Defaults, System.Masks, System.SysUtils, System.SyncObjs;

type
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

end.
