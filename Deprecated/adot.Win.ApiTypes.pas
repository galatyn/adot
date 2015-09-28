unit Win.ApiTypes;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

interface

//uses
  //{Winapi.MMSystem, Winapi.DirectSound,} Winapi.Windows;

const
  AUDCLNT_STREAMFLAGS_CROSSPROCESS             = $00010000;
  AUDCLNT_STREAMFLAGS_LOOPBACK                 = $00020000;
  AUDCLNT_STREAMFLAGS_EVENTCALLBACK            = $00040000;
  AUDCLNT_STREAMFLAGS_NOPERSIST                = $00080000;
  AUDCLNT_STREAMFLAGS_RATEADJUST               = $00100000;
  AUDCLNT_SESSIONFLAGS_EXPIREWHENUNOWNED       = $10000000;
  AUDCLNT_SESSIONFLAGS_DISPLAY_HIDE            = $20000000;
  AUDCLNT_SESSIONFLAGS_DISPLAY_HIDEWHENEXPIRED = $40000000;
  GUID_NUL: TGUID = '{00000000-0000-0000-0000-000000000000}';
  IID_IAudioEndpointVolume: TGUID = '{5CDF2C82-841E-4546-9722-0CF74078229A}';
	IID_IAudioMeterInformation: TGUID = '{C02216F6-8C67-4B5B-9D00-D008E73E0064}';
	IID_IDeviceTopology: TGUID = '{2A07407E-6497-4A18-9787-32F79BD0D98F}';

type
  GUID    = System.TGUID;
  LPCGUID = ^System.TGUID;
  REFIID  = LPCGUID;
  REFGUID = LPCGUID;
  HANDLE  = System.THandle;
  FLOAT   = System.Single;
  PFLOAT  = ^FLOAT;
  //WaveFormatEx = TWaveFormatEx;

  _AUDCLNT_SHAREMODE = (
    AUDCLNT_SHAREMODE_SHARED,
    AUDCLNT_SHAREMODE_EXCLUSIVE
  );
  AUDCLNT_SHAREMODE = _AUDCLNT_SHAREMODE;

  _AudioSessionState          = (
    AudioSessionStateInactive = 0,
    AudioSessionStateActive   = 1,
    AudioSessionStateExpired  = 2
  );
  AudioSessionState = _AudioSessionState;

 (* IDirectSound = interface(IUnknown)
    ['{279AFA83-4981-11CE-A521-0020AF0BE560}']
    // IDirectSound methods
    function CreateSoundBuffer(const pcDSBufferDesc: TDSBufferDesc; out ppDSBuffer: IDirectSoundBuffer; const pUnkOuter: IUnknown): HResult; stdcall;
    function GetCaps(out pDSCaps: TDSCaps): HResult; stdcall;
    function DuplicateSoundBuffer(const pDSBufferOriginal: IDirectSoundBuffer; out ppDSBufferDuplicate: IDirectSoundBuffer): HResult; stdcall;
    function SetCooperativeLevel(hwnd: HWND; dwLevel: DWORD): HResult; stdcall;
    function Compact: HResult; stdcall;
    function GetSpeakerConfig(out pdwSpeakerConfig: DWORD): HResult; stdcall;
    function SetSpeakerConfig(dwSpeakerConfig: DWORD): HResult; stdcall;
    function Initialize(const pcGuidDevice: GUID): HResult; stdcall;
  end;

  IDirectSoundBuffer = interface(IUnknown)
    ['{279AFA85-4981-11CE-A521-0020AF0BE560}']
    // IDirectSoundBuffer methods
    function GetCaps(var pDSBufferCaps: TDSBcaps): HResult; stdcall;
    function GetCurrentPosition(pdwCurrentPlayCursor, pdwCurrentWriteCursor: PDWORD): HResult; stdcall;
    function GetFormat(pwfxFormat: PWaveFormatEx; dwSizeAllocated: DWORD; pdwSizeWritten: PDWORD): HResult; stdcall;
    function GetVolume(out plVolume: Longint): HResult; stdcall;
    function GetPan(out plPan: Longint): HResult; stdcall;
    function GetFrequency(out pdwFrequency: DWORD): HResult; stdcall;
    function GetStatus(out pdwStatus: DWORD): HResult; stdcall;
    function Initialize(const pDirectSound: IDirectSound; const pcDSBufferDesc: TDSBufferDesc): HResult; stdcall;
    function Lock(dwOffset, dwBytes: DWORD; ppvAudioPtr1: PPointer; pdwAudioBytes1: PDWORD;
      ppvAudioPtr2: PPointer; pdwAudioBytes2: PDWORD; dwFlags: DWORD): HResult; stdcall;
    function Play(dwReserved1, dwPriority, dwFlags: DWORD): HResult; stdcall;
    function SetCurrentPosition(dwNewPosition: DWORD): HResult; stdcall;
    function SetFormat(pcfxFormat: PWaveFormatEx): HResult; stdcall;
    function SetVolume(lVolume: Longint): HResult; stdcall;
    function SetPan(lPan: Longint): HResult; stdcall;
    function SetFrequency(dwFrequency: DWORD): HResult; stdcall;
    function Stop: HResult; stdcall;
    function Unlock(pvAudioPtr1: Pointer; dwAudioBytes1: DWORD; pvAudioPtr2: Pointer; dwAudioBytes2: DWORD): HResult; stdcall;
    function Restore: HResult; stdcall;
  end;  *)

implementation

end.
