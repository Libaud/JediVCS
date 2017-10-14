{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: EnumServices.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is: Thomas Hensle (freevcs@thensle.de)
Code move to JEDIVCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI-VCS
2003/03/02  THuber   - D5/D6 conditional compiler to avoid compiler warning
                       (RaiseLastWin32Error / RaiseLastOSError)

-----------------------------------------------------------------------------*)

{******************************************************************************

  Name     : EnumServices

  Project  : FreeVCS2

  Created  : 23.03.2000 / Holger Dors (dors@kittelberger.de)

  Content  : Use the GetServices function to get a list of active,
             inactive or alle installed services.

  Example  :

  var
    MyServices: TServiceStatusArray;
    i: Integer;
  begin
    Memo1.Lines.Clear;
    MyServices := GetServices(ssActive);
    for i := 0 to Length(MyServices) - 1 do
      Memo1.Lines.Add(MyServices[i].DisplayName)
  end;

******************************************************************************}

unit EnumServices;

{$I COND_DEF.INC}

interface

uses Classes, Windows, WinSvc;

type

  TEnumServiceStatus2 = record
    ServiceName: string;
    DisplayName: string;
    ServiceStatus: TServiceStatus;
  end;

  TServiceStatusArray = array of TEnumServiceStatus2;

  TServiceState = (ssActive, ssInactive, ssAll);

function KeyToServiceState(const Key: string): TServiceState;
procedure FillServiceStateStrings(Strings: TStrings);
function GetServices(ServiceState: TServiceState): TServiceStatusArray;

implementation

uses SysUtils;

const
  ServiceState_Key: array[TServiceState] of string[3] = ('A', 'I', 'ALL');
  ServiceState_Name: array[TServiceState] of string = ('Active', 'Inactive', 'All');

function KeyToServiceState(const Key: string): TServiceState;
begin
  for Result := Low(Result) to High(Result) do
    if CompareText(Key, ServiceState_Key[Result]) = 0 then exit;
  Result := ssActive;
end;

procedure FillServiceStateStrings(Strings: TStrings);
var
  i: TServiceState;
begin
  Strings.Clear;
  for i := Low(i) to High(i) do
    Strings.AddObject(ServiceState_Name[i], TObject(i));
end;

function GetServices(ServiceState: TServiceState): TServiceStatusArray;
type
  TEnumServiceStatusArray = array[0..0] of TEnumServiceStatus;
  PEnumServiceStatusArray = ^TEnumServiceStatusArray;
var
  lbRetVal: Boolean;
  SvcMgr, i, liLength, liServiceState: Integer;
  lwBytesNeeded, lwResumeHandle, lwServicesReturned: DWord;
  MyServiceStatus: PEnumServiceStatusArray;
begin
  SetLength(Result, 0);

  case ServiceState of
    ssActive: liServiceState := SERVICE_ACTIVE;
    ssInactive: liServiceState := SERVICE_INACTIVE;
    ssAll: liServiceState := SERVICE_ACTIVE or SERVICE_INACTIVE;
    else
      liServiceState := 0;
  end;

  //-- Get a handle to the Service Control Manager
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_ENUMERATE_SERVICE);
  {$IFDEF D6_UP}
  if SvcMgr = 0 then RaiseLastOSError;
  {$ELSE}
  if SvcMgr = 0 then RaiseLastWin32Error;
  {$ENDIF}

  try
    //-- Initialize variables
    lwBytesNeeded := 0;
    lwResumeHandle := 0;
    lwServicesReturned := 0;

    //-- Get some memory for the Enum-Results
    GetMem(MyServiceStatus, 256);

    try
      repeat
        FillChar(PChar(MyServiceStatus)^, 256, 0);

        //-- enummerate all active services
        lbRetVal := EnumServicesStatus(SvcMgr, SERVICE_WIN32, liServiceState,
          PEnumServiceStatus(MyServiceStatus)^, 256, lwBytesNeeded,
          lwServicesReturned, lwResumeHandle);

        //-- if call fails and last error isn't ERROR_MORE_DATE,
        //-- we have a "real" error!
        if not lbRetVal then
        begin
          {$IFDEF D6_UP}
          if GetLastError <> ERROR_MORE_DATA then RaiseLastOSError;
          {$ELSE}
          if GetLastError <> ERROR_MORE_DATA then RaiseLastWin32Error;
          {$ENDIF}
        end;

        //-- build result Copy
        liLength := Length(Result);
        SetLength(Result, liLength + Integer(lwServicesReturned));
        for i := 0 to lwServicesReturned - 1 do
        begin
          Result[liLength + i].ServiceStatus := MyServiceStatus^[i].ServiceStatus;
          Result[liLength + i].DisplayName := MyServiceStatus^[i].lpDisplayName;
          Result[liLength + i].ServiceName := MyServiceStatus^[i].lpServiceName;
        end;

        //-- if we don't have a ResumeHandle, we're done
      until lwResumeHandle = 0;
    finally
      //-- free memory for Enum-Results
      FreeMem(MyServiceStatus);
    end;

  finally
    //-- release Service Control Manager handle
    CloseServiceHandle(SvcMgr);
  end;
end;


end.

