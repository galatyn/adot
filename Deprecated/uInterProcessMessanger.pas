unit uInterProcessMessanger;

interface

uses Windows, Classes, adot, uIPC, SysUtils, systools, Math, uThreading, uLogUnit;
 // при запуске системы:
 // - проверить наличие запущенной копии
 // - если есть - передать ей параметры и выход
 // при работе системы:
 // - ожидать команд
 // - при поступлении команды - обработать ее и активизировать приложение

// возвращает false если копия программы уже работает, в этом случае сама передает параметры
function CanToWork: boolean;

implementation

uses uFormMainWRPC;

type
  // через разделяемую память передается handle на участок памяти с параметрами
  TInterProcessMessage = record
    sharedMemSize: integer;
  end;
  PInterProcessMessage = ^TInterProcessMessage;

  // поток, который ожидает команд от др. программ и передает их главной
  TParametersReader = class(TAdvThread)
  public
    ssem, wsem, mtx: cardinal;
    sm: TSharedMem;
    constructor Create(mtx: cardinal);
    procedure Execute; override;
  end;

const
  SEMAPHORE_ALL_ACCESS = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3;
  cnSharedMemoryMain = '9C6982D7-A0D3-4B4F-863A-4B004C1A1EAA'; // общая память - указатель на данные
  cnSharedMemoryData = '7C7710C7-00E0-407C-9755-62B10B629509'; // общая память - данные для передачи
  cnParameterSemaphoreName = '01B22598-06A3-4218-B735-65927CDDCFD6'; // спускается при передаче параметров
  cnAcceptedSemaphoreName = '0C173818-6CB5-48F9-8504-E0EE7A2E9800'; // спускается после приема параметров
  cnSendMutex  = '2D746AFB-A490-4DDF-AB74-9062169148BC'; // захватывается для передачи параметров
  cnOwnerMutex = '0E732765-F525-48C8-8364-46E44D346037'; // захватывается первым запущенным
  cnSendTimeout = 5 * 1000;                                  // сколько ждать запущеннй процес для передачи
  list: TAdvThreadList = nil;                                   // поток, ожидающий команд от др. процессов

// готовит сообщение для передачи
function getMessage(dest: PInterProcessMessage): TSharedMem;
var
  i: integer;
  t: string;
begin
  t := '';
  for i := 1 to paramcount do
    t := t + ParamStr(i) + #0;
  t := t + #0;
  dest.sharedMemSize := length(t);
  if dest.sharedMemSize > 0 then
  begin
    Result := TSharedMem.Create(cnSharedMemoryData, dest.sharedMemSize);
    move(t[1], Result.buffer^, dest.sharedMemSize);
  end
  else
  begin
    fillchar(dest^, sizeof(dest^), 0);
    Result := nil;
  end;
end;

procedure sendParameters(msg: PInterProcessMessage);
var
  g: IGuard;
  ssem: cardinal;
  wsem: cardinal;
begin
  g := CreateGuard;
  g.Add(getMessage(msg));
  ssem := g.Add(createSemaphore(nil, 0, high(integer), cnParameterSemaphoreName));
  wsem := g.Add(createSemaphore(nil, 0, high(integer), cnAcceptedSemaphoreName));
  if (ssem <> 0) and (wsem <> 0) then
    if releaseSemaphore(ssem, 1, nil) then            // отправляем блок в общей памяти
      waitForSingleObject(wsem, infinite)// ожидаем обработки
  ;
end;

function CanDup: boolean;
var
  i: integer;
begin
  for i := 1 to paramcount do
    if CompareText(ParamStr(i), '/Dup') = 0 then
    begin
      Result := True;
      exit;
    end;
  Result := False;
end;

function CanToWork: boolean;
var
  sm:  TSharedMem;
  mtx: cardinal;
begin
  Result := CanDup;
  if Result then
    exit;
  mtx := createMutex(nil, True, cnOwnerMutex);
  if waitforsingleobject(mtx, 0) <> WAIT_OBJECT_0 then // mutex abandoned already
  begin
    closeHandle(mtx);
    mtx := createMutex(nil, False, cnSendMutex);
    if mtx = 0 then
      exit;
    try
      if waitForSingleObject(mtx, cnSendTimeout) = WAIT_OBJECT_0 then
        try
          sm := TSharedMem.Create(cnSharedMemoryMain, sizeof(TInterProcessMessage));
          try
            sendParameters(sm.Buffer);
          finally
            sm.Free;
          end;
        finally
          releaseMutex(mtx);
        end;
    finally
      closeHandle(mtx);
    end;
    exit;
  end;
  // new process started
  Result := True;
  list := TAdvThreadList.Create;
  list.add(TParametersReader.Create(mtx), True);
end;

// ------------------------------------------ TParametersReader --------------------------------------------------------

constructor TParametersReader.Create(mtx: cardinal);
begin
  inherited Create;
  self.mtx := g.Add(mtx);
  ssem := g.Add(createSemaphore(nil, 0, high(integer), cnParameterSemaphoreName));
  wsem := g.Add(createSemaphore(nil, 0, high(integer), cnAcceptedSemaphoreName));
  sm := g.add(TSharedMem.Create(cnSharedMemoryMain, sizeof(TInterProcessMessage)));
end;

procedure TParametersReader.Execute;
var
  p: PInterProcessMessage;
  buf: pointer;
  n: integer;
  Data: TSharedMem;
begin
  repeat
    waitForObjects([ssem, ThreadStopSemaphore], infinite);
    if terminated then
      break;
    p := sm.Buffer;
    n := p.sharedMemSize;
    if n <= 0 then
      postMessage(FormMainWRPC.handle, wm_parameters, 0, 0)
    else
    begin
      Data := TSharedMem.Create(cnSharedMemoryData, n);
      buf  := allocmem(n);
      move(Data.buffer^, buf^, n);
      FreeAndNil(Data);
      releaseSemaphore(wsem, 1, nil);
      if not postMessage(FormMainWRPC.handle, wm_parameters, integer(buf), n) then
        reallocmem(buf, 0);
    end;
  until False;
end;

initialization

finalization
  log('finalization uInterProcessMessanger');
  try
    FreeAndNil(list);
  finally
    log('finalization uInterProcessMessanger exit');
  end;

end.
