{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvServiceUnit.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
  - use common version strings for server ports
  - only use oracle ncoci port, remove bde port
  - use same configuration consts for ControlPanel applet and server ports

-----------------------------------------------------------------------------

Unit history:

  Mar '00 by Marco Gosselink - mgosselink@yahoo.com &
             Thomas Hensle - freevcs@thensle.de - http://www.freevcs.de

2003/02/04  HDors   - 1st Migrationstep from FreeVCS code to JEDI VCS
2003/02/09  HDors   - Add MySQL Server Port by Ludo Brands
2003/02/22  THuber  - Add  F I B P L U S S R V  port which uses  F I B p l u s  components
                      therefor dangerous casts changed:
                       * TDateTime(...) => .AsDateTime
                       * Integer(...) => .AsInteger
                       * FQuery.Fields[Fld] => FQuery.Fields[Fld].AsString
                     - removed  I n t e r b a s e 5  support
                     - D4_UP directive removed
2003/03/01  THuber   - fixed typo's (reties => retries, Maximumreties => FMaximumretries
                      in some ports (MSSQL, I N F O R M I X)
2003/03/02  THuber   - removed compiler warnings
                     - changed handling for server port version detecting
2003/12/26  THuber   - DlgCaption => c_DlgCaption
2003/12/27  THuber   - removed Src Version const (no use)
2004/01/04  THuber   - added automatic archive upgrade
2004/04/23  THuber   - #1569 raise exception if dbconnect not possible on start
2004/11/14  THuber   - #2286 support for firebird embedded in  F I B  port

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster- style cleaning
                     - changes for Firebird port over JVCL UIB components
                       by Pierre Y. (partly adapted)
2004/12/11  USchuster- added register functions for new server objects
2004/12/18  USchuster- changes for Zeos DBO 6.x
2005/01/15  THuber   #2502 added mssql option for trusted nt connections
2005/02/13  THuber   #2570 use charset parameter on connection for firebird
2005/03/17  Diwic    - added possibility to log what message comes from DB when
                     it fails to open. Also saves logfile (if enabled) when DB
                     does not open.
2005/03/18  USchuster- changes to support user defined server banner in
                       service version as well (mantis #2296)
                     - save logfile if archive version check fails
2005/04/14  CSchuette- #2873 fixed resource/memory leak when service is stopped
2005/10/05  USchuster- added retry support for Oracle, MySQL and  F l a s h F i l e r  (mantis #3247)
2006/07/05  USchuster- changes for eventlog Event ID description by AK(mantis #3789)
2006/10/11  USchuster- changes for Oracle performance tweak "UseRuleOptimizerMode" (mantis #3950)
2007/10/06  USchuster- changes for (MySQL) connection timeout solution (Mantis #4260)
2009/01/17  USchuster- added register functions for property server objects
2009/05/19  USchuster- fixed Firebird embedded with UIB port (missing credentials)
vvvvvvvvvvvvvvvvvvvvvvvvv 2.45 STABLE ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
2009/11/22  THuber   - #5030 we've spend the service a description now
2009/11/30  THuber   - #5032 change server log location
2009/12/13  USchuster- changes for opt-out of using table latestrevision for performance increase
^^^^^^^^^^^^^^^^^^^^^^^^^ 2.50 Beta2 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
2009/12/22  THuber   #5062 add support for firebird 2.x databases, fixed broken embedded support
                     #4085 add support for connection of unicode databases over UIB
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     Removed unnecessary L Z H Define as it's always used
                     #5066 support for  F l a s h F i l e r  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SrvServiceUnit - main unit for the NT service version of the JVCS
                   application server .
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

unit SrvServiceUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I jedi.inc}
{$I COMPOPT.INC}

{$IFDEF DELPHI6_UP}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
{$IFnDEF FPC}
  Windows,
{$ELSE}
  LCLIntf, LCLType, LMessages,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls,
  // Modified by F. Libaud
  {$ifdef LCL}
  DaemonApp,
  {$else}
  SvcMgr,
  {$endif}
  Dialogs,
  ExtCtrls,
  {RBroker, ApServer,}
  SrvUDRec, SrvDBDef, WinSock, RFormat, WSocket,
  IniFiles, ApSrvCli,
  {$IFDEF MSSQLSRV}
  ActiveX,
  {$ENDIF MSSQLSRV}
  {$IFDEF ADOSRV}
  ActiveX,
  {$ENDIF ADOSRV}
  CipherLz, ComCtrls;

type
  TFreeVCSService = class(TService)
    AppServer1: TAppServer;
    RequestBroker1: TRequestBroker;
    UpTimer: TTimer;
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure AppServer1AfterSendReply(Sender: TObject;
      CliWSocket: TClientWSocket);
    procedure AppServer1BeforeProcessRequest(Sender: TObject;
      CliWSocket: TClientWSocket; var CmdBuf: Pointer; var CmdLen: Integer);
    procedure AppServer1BeforeSendReply(Sender: TObject;
      CliWSocket: TClientWSocket);
    procedure AppServer1ClientConnected(Sender: TObject;
      CliWSocket: TClientWSocket);
    procedure AppServer1Display(Sender: TObject; Msg: string);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure UpTimerTimer(Sender: TObject);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
  private
    { Private declarations }
    FServerQuery: TSrvQuery;
    FServerDatabase: TSrvDatabase;
    FUserData: PUserDataRecord;
    FInitialized,
    FDBMSConnected: Boolean;
    FIniFileName,
    FPort: string;
    FStartTime,
    FUpTime: Double;
    FLog: TStringList;
    UPTimeStr: string;
      //-- hdo
    FDBUser,
    FDBPassword,
    FDBServer: string;
    FMaximumRetries: Integer;
    {$IFDEF MSSQLSRV}
    FDBUseNTTrustedConnection: Boolean;
    {$ENDIF MSSQLSRV}
    {$IFDEF UIBSRV} //-- PrY
    FDBDatabase: string;
    FDBUseEmbeded: Boolean;
    FDBCharSet: string;
    {$ENDIF UIBSRV}
    function  LocalDT2GMTDT(const LocalDT: Double): Double;
    procedure GetLocalGMTDifference;
    function  CheckArchiveVersion: Boolean;
    procedure AddLogMessage(Value: string);
    procedure SaveSettings;
    procedure SaveLogfile;
    procedure CleanUp;
    function  GetLocalIP: string;
  public
    { Public declarations }
    function GetServiceController: TServiceController; override;
  end;

var
  FreeVCSService: TFreeVCSService;

implementation

uses
{$IFnDEF FPC}
  ShellAPI,
{$ELSE}
{$ENDIF}
  FileCtrl, Registry, JVCSServerFunctions,
  {$IFDEF ORACLESRV}
    NCOciWrapper,
  {$ENDIF ORACLESRV}
  {$IFDEF UIBSRV}
    uiblib,
  {$ENDIF}
  VCSCommon, ServerCryptInt, SrvConst,
  {$IFDEF NEWOBJECTS}
  JVCSSrvOBJNewObjects,
  {$ENDIF NEWOBJECTS}
  {$IFDEF BRANCHOBJECTS}
  JVCSSrvOBJBranches,
  {$ENDIF BRANCHOBJECTS}
  {$IFDEF PROPERTYOBJECTS}
  JVCSSrvOBJProperties,
  {$ENDIF PROPERTYOBJECTS}
  SrvOBJMain, SrvOBJReport, SrvOBJFeatures,
  SrvOBJLog_ToDo, SrvOBJModules, SrvOBJProjects, SrvOBJBugTrack, DB;

{$R *.dfm}

{$IFDEF UIBSRV}
  function GetFirebirdClientLibrary(const bEmbeded : Boolean) : string;
  var
    Key: HKEY;
    Size: Cardinal;
    HR: Integer;
    sClientLibName : String;
  begin
    // There are several posiblities where the client dll is located:
    // 1. Application directory - problem: this could be the embeded version!
    // 2. Windows\system32 - fbclient or gds32.dll
    // 3. Firebird directory - fbclient or gds32.dll
    //
    // Question: what is right?
    // ==> decision to use either application directory or firebird bin dir of default instance
    //
    if bEmbeded then
      sClientLibName := 'fbembed.dll'
    else
      sClientLibName := 'fbclient.dll';

    if FileExists(ExtractFilePath(ParamStr(0)) + sClientLibName) then
      Result := sClientLibName
    else
    begin
      HR := RegOpenKeyEx(HKEY_LOCAL_MACHINE, FBINSTANCES, 0, KEY_READ, Key);
      if (HR = ERROR_SUCCESS) then
      begin
        HR := RegQueryValueEx(Key, 'DefaultInstance', nil, nil, nil, @Size);
        if (HR = ERROR_SUCCESS) then
        begin
          SetLength(Result, Size div sizeof(Char));
          HR := RegQueryValueEx(Key, 'DefaultInstance', nil, nil, Pointer(Result), @Size);
          if (HR = ERROR_SUCCESS) then
            Result := Trim(Result)+ 'bin\' + sClientLibName;
        end;
        RegCloseKey(Key);
      end;
      if (HR <> ERROR_SUCCESS) then
        Result := sClientLibName;
    end;
  end;
{$ENDIF UIBSRV}


procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  FreeVCSService.Controller(CtrlCode);
end;

//==============================================================================
// Return the service controller.
// Implementation of this procedure has changed from Pointer to Type in D5.

{$IFDEF DELPHI5_UP} //--MG
function TFreeVCSService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;
{$ELSE}
function TFreeVCSService.GetServiceController: PServiceController;
begin
  Result := @ServiceController;
end;
{$ENDIF DELPHI5_UP}

//==============================================================================
// Wait for a Terminated signal which is automatically generated by the
// operating system when a net stop svcname is executed or stop service is
// chosen in the Services control panel applet

procedure TFreeVCSService.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    WaitMessage;
    Sender.ServiceThread.ProcessRequests(False);
  end;
end;

//=== Convert server's local timestamp to GMT ==================================

function TFreeVCSService.LocalDT2GMTDT(const LocalDT: Double): Double;

  // from ESBDates (c) 1999 by ESB Consultancy
  function GetLocalTZBias: Longint;
  var
    TZ: TTimeZoneInformation;
  begin
    case GetTimeZoneInformation (TZ) of
      TIME_ZONE_ID_STANDARD: Result := TZ.Bias + TZ.StandardBias;
      TIME_ZONE_ID_DAYLIGHT: Result := TZ.Bias + TZ.DaylightBias;
    else
      Result := TZ.Bias;
    end;
  end;

begin
  Result := LocalDT + GetLocalTZBias / (24 * 60);
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.GetLocalGMTDifference;
var
  TZ: TTimeZoneInformation;
  TZBias: Longint;
begin
  // calculate difference between local time & GMT
  // from ESBDates (c) 1999 by ESB Consultancy
  case GetTimeZoneInformation (TZ) of
    TIME_ZONE_ID_STANDARD: TZBias := TZ.Bias + TZ.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: TZBias := TZ.Bias + TZ.DaylightBias;
  else
    TZBias := TZ.Bias;
  end;
  if TZBias = 0 then
    FUserData.GMTDifference := 0
  else
  if TZBias < 0 then
  begin
    if TZBias mod 60 = 0 then
      FUserData.GMTDifference := (Abs (TZBias) div 60)
    else
      FUserData.GMTDifference := (Abs (TZBias) / 60)
  end // if TZ.Bias < 0 then
  else
  begin
    if TZBias mod 60 = 0 then
      FUserData.GMTDifference := -1 * (TZBias div 60)
    else
      FUserData.GMTDifference := -1 * (TZBias / 60)
  end; // else if TZ.Bias < 0 then
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.ServiceCreate(Sender: TObject);
var
  SvcDependency: string;
  NewDisplayName: string;
begin
  FLog := nil;
  FUserData := nil;
  FDBMSConnected := False;
  // Name the service and change the displayname and
  // check for dependend services - this function replaces the port-specific
  // routines
  FIniFileName := ChangeFileExt(ParamStr(0), '.ini');
  with TIniFile.Create(FIniFileName) do
  begin
    try
      // Change the service name
      Self.Name := ReadString(SectionData, ServiceName, 'JediVCSService');
      NewDisplayName := ReadString(SectionData, ServiceDisplayName,
        'JediVCS %port_version% Svc %server_version%');

      NewDisplayName := StringReplace(NewDisplayName, '%port_version%', GetSrvPortFromId(cServerId), []);
      NewDisplayName := StringReplace(NewDisplayName, '%server_version%', GetSrvVersionString(cServerId, True), []);

      // Change the service displayname
      Self.DisplayName := NewDisplayName;

      SvcDependency := ReadString(SectionData, Dependency, '');
      if SvcDependency <> '' then
        with Dependencies.Add as TDependency do
          Name := SvcDependency;
    finally
      Free;
    end;
  end; // with TIniFile.Create(...
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.ServiceStart(Sender: TService; var Started: Boolean);
var
  SrvCryptVersion: Integer;
  IniFile: TIniFile;
  ThreadPriority,
  DBMSMsg,
  ServerMsg: string;
  DBOpenErrMsg: string; // -- Diwic
  Retries: Integer;
  FBanner, FDefaultBanner: string;
begin
  {$IFDEF MSSQLSRV}
  CoInitialize(nil);
  {$ENDIF MSSQLSRV}
  {$IFDEF ADOSRV}
  CoInitialize(nil);
  {$ENDIF ADOSRV}
  Started := True;
  FServerDatabase := TSrvDatabase.Create(Self);
  FServerQuery := TSrvQuery.Create(Self);

  Retries := 0;
  // compiler hint off
  if Retries = 0 then;

  { Build Ini file name }
//  FIniFileName := 'FreeVCSNTService.ini'; <-- won't be of any use

  // we changed the folder location for ther server log with 2.45 and need
  // to move the existing to the new location.
  MoveServerLog2CommonAppFolder;

  { Initialize RequestBroker object }
  with RequestBroker1 do
  begin
    IniFileName := FIniFileName;
    AddServerObject(TServerObjectLOGIN);
    AddServerObject(TServerObjectLOGOUT);
    AddServerObject(TServerObjectWHOAMI);
    AddServerObject(TServerObjectGET_SERVER_OPTIONS);
    AddServerObject(TServerObjectSET_SERVER_OPTIONS);
    AddServerObject(TServerObjectGET_PROJECT_RIGHTS);
    AddServerObject(TServerObjectSET_PROJECT_RIGHTS);
    AddServerObject(TServerObjectGET_USERLIST);
    AddServerObject(TServerObjectGET_USERS);
    AddServerObject(TServerObjectADD_UPDATE_USER);
    AddServerObject(TServerObjectCHANGE_PASSWORD);
    AddServerObject(TServerObjectREMOVE_USER);
    AddServerObject(TServerObjectGET_SERVER_TIME);
    AddServerObject(TServerObjectGET_SERVER_LOG);
    AddServerObject(TServerObjectCLEAR_SERVER_LOG);
    AddServerObject(TServerObjectGET_ARCHIVE_TSTAMP);
    AddServerObject(TServerObjectLIVE_BACKUP);
    AddServerObject(TServerObjectGET_PROJECT_MODULE_LIST);
    AddServerObject(TServerObjectGET_BLANK_MODULE_LIST);
    AddServerObject(TServerObjectGET_VERSION_LIST);
    AddServerObject(TServerObjectGET_REVISION_LIST);
    AddServerObject(TServerObjectGET_LATEST_REVISIONS);
    AddServerObject(TServerObjectGET_VERSION_REVISION);
    AddServerObject(TServerObjectGET_ROLLBACK_REVISIONS);
    AddServerObject(TServerObjectGET_REVISION_LIST_BY_NAME);
    AddServerObject(TServerObjectGET_REVISION_LIST_BY_ID);
    AddServerObject(TServerObjectGET_REVISION_LIST_BY_VERSION);
    AddServerObject(TServerObjectGET_REVISION_STATUS);
    AddServerObject(TServerObjectGET_REVISION_COMMENT);
    AddServerObject(TServerObjectCHANGE_REVISION_COMMENT);
    AddServerObject(TServerObjectGET_MODULE_HISTORY);
    AddServerObject(TServerObjectGET_BLOB_STATUS);
    AddServerObject(TServerObjectGET_SINGLE_BLOB);
    AddServerObject(TServerObjectIS_MEMBER_OF_PROJECT);
    AddServerObject(TServerObjectGET_LABELS);
    AddServerObject(TServerObjectADD_UPDATE_LABEL);
    AddServerObject(TServerObjectREMOVE_LABEL);
    AddServerObject(TServerObjectADD_REMOVE_REVISION_LABEL);
    AddServerObject(TServerObjectLABEL_USED_BY);
    AddServerObject(TServerObjectGET_LABELS_BY_PROJECT);
    AddServerObject(TServerObjectGET_LABELS_BY_REVISION);
    AddServerObject(TServerObjectGET_UPDATE_DESCRIPTION);
    AddServerObject(TServerObjectGET_DESERTED_MODULES);
    AddServerObject(TServerObjectGET_FILE_FAMILIES);
    AddServerObject(TServerObjectGET_FAMILY_EXTENSIONS);
    AddServerObject(TServerObjectFAMILY_USED_BY);
    AddServerObject(TServerObjectADD_UPDATE_FILE_FAMILIES);
    AddServerObject(TServerObjectREMOVE_FILE_FAMILIES);
    AddServerObject(TServerObjectGET_MILESTONES);
    AddServerObject(TServerObjectADD_UPDATE_MILESTONES);
    AddServerObject(TServerObjectREMOVE_MILESTONE);
    AddServerObject(TServerObjectGET_PROJECT_MILESTONES);
    AddServerObject(TServerObjectASSIGN_PROJECT_MILESTONE);
    AddServerObject(TServerObjectREMOVE_PROJECT_MILESTONE);
    AddServerObject(TServerObjectGET_TODO_FILTER);
    AddServerObject(TServerObjectADD_UPDATE_TODO_ENTRIES);
    AddServerObject(TServerObjectREMOVE_TODO_ENTRIES);
    AddServerObject(TServerObjectGET_TODO_ENTRIES);
    AddServerObject(TServerObjectGET_LOG_FILTER);
    AddServerObject(TServerObjectGET_LOG_ENTRIES);
    AddServerObject(TServerObjectGET_LOG_COMMENT);
    AddServerObject(TServerObjectADD_LOG_ENTRIES);
    AddServerObject(TServerObjectREMOVE_LOG_ENTRIES);
    AddServerObject(TServerObjectGET_SHARED_BY);
    AddServerObject(TServerObjectGET_SHARED_MODULES);
    AddServerObject(TServerObjectCOPY_REVISION);
    AddServerObject(TServerObjectMERGE_VER_REV_NR);
    AddServerObject(TServerObjectGET_PROJECT_ID);
    AddServerObject(TServerObjectGET_PROJECT_RIGHT);
    AddServerObject(TServerObjectGET_MODULE_ID);
    AddServerObject(TServerObjectGET_MODULE_NAME);
    AddServerObject(TServerObjectRENAME_MODULE);
    AddServerObject(TServerObjectGET_MODULES_LIKE);
    AddServerObject(TServerObjectSEARCH_MODULES);
    AddServerObject(TServerObjectREPORT_PROJECT_STATE);
    AddServerObject(TServerObjectREPORT_ALL_PROJECTS);
    AddServerObject(TServerObjectREPORT_MILESTONES);
    AddServerObject(TServerObjectADD_NEW_PROJECT);
    AddServerObject(TServerObjectREMOVE_PROJECT);
    AddServerObject(TServerObjectRESTORE_PROJECT);
    AddServerObject(TServerObjectADD_NEW_MODULE);
    AddServerObject(TServerObjectREMOVE_MODULE);
    AddServerObject(TServerObjectREMOVE_REVISION);
    AddServerObject(TServerObjectREMOVE_VERSION);
    AddServerObject(TServerObjectMOVE_MODULE);
    AddServerObject(TServerObjectGET_CHECKOUT_MODULE);
    AddServerObject(TServerObjectCHECKOUT_ONLY_MODULE);
    AddServerObject(TServerObjectUNDO_CHECKOUT_MODULE);
    AddServerObject(TServerObjectCHECKIN_MODULE);
    AddServerObject(TServerObjectHIDE_UNHIDE_MODULE);
    AddServerObject(TServerObjectGET_PROJECT_REFERENCES);
    AddServerObject(TServerObjectADD_UPDATE_PROJECT_REFERENCES);
    AddServerObject(TServerObjectADD_UPDATE_PROJECT_GROUP);
    AddServerObject(TServerObjectREMOVE_PROJECT_GROUP);
    AddServerObject(TServerObjectGET_PROJECT_GROUP_INFORMATION);
    AddServerObject(TServerObjectADD_REMOVE_PROJECT_TO_GROUP);
    AddServerObject(TServerObjectREMOVE_PROJECT_REFERENCES);
    AddServerObject(TServerObjectGET_PROJECT_INFORMATION);
    AddServerObject(TServerObjectGET_PROJECT_LIST);
    AddServerObject(TServerObjectGET_LOCKED_MODULES);
    AddServerObject(TServerObjectRENAME_PROJECT);
    AddServerObject(TServerObjectGET_SPACE_BY_PROJECTS);
    AddServerObject(TServerObjectGET_SPACE_BY_MODULES);
    // Bug Tracking
    AddServerObject(TServerObjectGET_BUGS);
    AddServerObject(TServerObjectGET_BUGS_BY_PROJECT);
    AddServerObject(TServerObjectGET_BUGS_BY_MODULE);
    AddServerObject(TServerObjectGET_MOST_SEVERE_BUGS);
    AddServerObject(TServerObjectADD_UPDATE_BUG);
    AddServerObject(TServerObjectREMOVE_BUG);
    AddServerObject(TServerObjectASSIGN_REMOVE_BUG);
    AddServerObject(TServerObjectENABLE_MODULE_PROJECT_BUG);
    AddServerObject(TServerObjectBUGS_USED_BY);
    AddServerObject(TServerObjectREPORT_PROJECT_BUGS);
    // Purge
    AddServerObject(TServerObjectPURGE_PROJECT);
    // Direkt SQL
    AddServerObject(TServerObjectEXECUTE_SQL);
    // Client configuration
    AddServerObject(TServerObjectREAD_CONFIG_DATA);
    AddServerObject(TServerObjectWRITE_CONFIG_DATA);
  end; // with RequestBroker1 do begin
  {$IFDEF NEWOBJECTS}
  AddNewServerObjects(RequestBroker1);
  {$ENDIF NEWOBJECTS}
  {$IFDEF BRANCHOBJECTS}
  AddBranchServerObjects(RequestBroker1);
  AppServer1.ClientClass := TBranchClientWSocket;
  {$ENDIF BRANCHOBJECTS}
  {$IFDEF PROPERTYOBJECTS}
  AddPropertyServerObjects(RequestBroker1);
  {$ENDIF PROPERTYOBJECTS}


  { It's a good place to initialize TRequestBroker.UserData with for example }
  { a pointer to a dynamically allocated record or to an object.             }
  { UserData is passed to ServerObjects instanciated by the broker. Using    }
  { it, they can gain access to some global data or procedure, such as data  }
  { base session.                                                            }

  { In this sample we use a TUserDataRecord record }
  New(FUserData);
  RequestBroker1.UserData := Longint(FUserData);

  { Initialize user data }
  FUserData.RequestCount := 0;
  FUserData.UpTimeStr := '0 d, 0 h, 0 m';
  FUserData.UserCount := 0;
  FUserData.CheckinCount := 0;
  FUserData.CheckoutCount := 0;
  FUserData.GetCount := 0;
  FUserData.NewFilesCount := 0;
  FUserData.TransmittedBytes := 0;
  FUserData.ReceivedBytes := 0;

  if not FInitialized then
  begin
    FInitialized := True;
    // Prepare Log Info
    if not Assigned(FLog) then
      FLog := TStringList.Create;

    IniFile := TIniFile.Create(FIniFileName);
    try
      ThreadPriority := IniFile.ReadString(SectionData, Priority, 'tpNormal');
      FPort := IniFile.ReadString(SectionData, KeyPort, '2106');
      // FUserData
      FUserData.WriteServerLog := IniFile.ReadBool(SectionData, WriteSrvLog,
        False);
      FUserData.ConnectTimeOut := IniFile.ReadInteger(SectionData,
        ConnectTimeOut, 60);
      FUserData.LoginTimeOut := IniFile.ReadFloat(SectionData, LoginTimeOut,
        0.041666667);
      FUserData.LoginExpires := IniFile.ReadBool(SectionData, LoginExpires,
        False);
      FUserData.CasuallyCheckin := IniFile.ReadBool(SectionData,
        CasuallyCheckin, True);
      FUserData.WriteVCSLog := IniFile.ReadBool(SectionData, WriteVCSLog, True);
      FUserData.CheckIPAddr := IniFile.ReadBool(SectionData, CheckIPAddr,
        False);
      {$IFNDEF ADOSRV} // -- PL
        {$IFNDEF MSSQLSRV}
              {$IFNDEF MYSQLSRV} //-- LB
                  {$IFNDEF UIBSRV} //-- PrY
                    FUserData.DBPath := IniFile.ReadString(SectionData, DBPath, '');
                  {$ELSE}
                    FUserData.DBPath := FServerDatabase;
                  {$ENDIF ~UIBSRV}
              {$ELSE}
                FUserData.DBPath := FServerDatabase;
              {$ENDIF ~MYSQLSRV}
        {$ELSE}
          FUserData.DBPath := FServerDatabase;
        {$ENDIF ~MSSQLSRV}
      {$ELSE}
        FUserData.DBPath := FServerDatabase;
      {$ENDIF ~ADOSRV}
      {$IFDEF ORACLESRV}
      FUserData.DBPath := 'LocalSQLDB';
      {$ENDIF ORACLESRV}
      FUserData.BackupPath := IniFile.ReadString(SectionData, BackupPath, '');
      FUserData.LastLiveBackup := IniFile.ReadFloat(SectionData,
        LastLiveBackup, 0);
      FUserData.LocalLogin := IniFile.ReadBool(SectionData, LocalLogin, False);
      FUserData.ArchiveTimeStamp := IniFile.ReadFloat(SectionData,
        ArchiveTStamp, LocalDT2GMTDT(Now));
      FUserData.LogAccessFault := IniFile.ReadBool(SectionData,
        LogAccessFault, False);
      FDBUser := IniFile.ReadString(SectionData, DBUser, '');
      FDBPassword := fvcsSrvDeCrypt(IniFile.ReadString(SectionData,
        DBPassword, ''));
      FDBServer := IniFile.ReadString(SectionData, DBServer, '');
      {$IFDEF ORACLESRV}
      FServerDatabase.UseRuleOptimizerMode := IniFile.ReadBool(SectionData, UseRuleOptimizerMode, False);
      {$ENDIF ORACLESRV}
      {$IFDEF MSSQLSRV} //-- MG
      FUserData.DBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FUserData.ServerName := FDBServer; // MG 23-10-2000
      FDBUseNTTrustedConnection := IniFile.ReadBool(SectionData, FmssqlUseNTTrustedConnection, False);
      {$ENDIF MSSQLSRV}
      {$IFDEF MYSQLSRV} //-- LB
      FUserData.DBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FUserData.ServerName := FDBServer;
      {$ENDIF MYSQLSRV}
      {$IFDEF UIBSRV} //-- PrY
      FDBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FDBUseEmbeded := IniFile.ReadBool(SectionData, FibUseEmbeded, True);
      FDBCharSet := IniFile.ReadString(SectionData, FibCharSet, 'NONE');
      {$ENDIF UIBSRV}
      {$IFDEF ADOSRV} //-- PL
      FUserData.DBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FUserData.ServerName := FDBServer;
      {$ENDIF ADOSRV}
      FMaximumRetries := IniFile.ReadInteger(SectionData, MaximumRetries, 10);
      {$IFDEF USELATESTREVISION}
      FUserData.UseLatestRevisions := IniFile.ReadBool(SectionData, 'UseLatestRevisions', True);
      {$ENDIF USELATESTREVISION}      
      FDefaultBanner := Format('Welcome to %s', [MainTitle]);
      FBanner := IniFile.ReadString(SectionData, ServerBanner, FDefaultBanner);
      if FBanner = '' then
        FBanner := FDefaultBanner;
      if IniFile.ReadBool(SectionData, DisableServerBanner, False) then
        AppServer1.Banner := ''
      else
        AppServer1.Banner := FBanner;

      FUserData.CheckCliTimeStamp := IniFile.ReadBool(SectionData,
                                                  VerifyClientTimestamp, False);
      FUserData.DisableSQLDirekt := IniFile.ReadBool(SectionData,
                                                        DisableSQLDirekt, True);
    finally
      IniFile.Free;
    end;

    // Set service priority
    if ThreadPriority = 'tpHigher' then
      ServiceThread.Priority := tpHigher
    else
      ServiceThread.Priority := tpNormal;

    // Try to get service path
    with TRegistry.Create do
    begin
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKey('System\CurrentControlSet\Services\' + Self.Name, False) then
        begin
          if ValueExists('ImagePath') then
            FUserData.ServerPath := ExtractFilePath(ReadString('ImagePath'))
          else
            FUserData.ServerPath := '';
          CloseKey;
        end // if OpenKey...
        else FUserData.ServerPath := '';
      finally
        Free;
      end;
    end; // with TRegistry.Create do begin
    if FUserData.ServerPath = '' then
      LogMessage('Server path undefined!', EVENTLOG_WARNING_TYPE, 0, 0);

    FUserData.ShowServerMsg := True;

    // calculate difference between local time & GMT
    GetLocalGMTDifference;

    // clear 'settings need to be saved' flag
    FUserData.SettingsChanged := False;
  end; // if not FInitialized then

  // get the machines real IP number
  FUserData.ServerIPNo := GetLocalIP;

  SrvCryptVersion := fvcsCryptGetVersion;
  if SrvCryptVersion <> ExpectedSrvCryptVersion then
  begin
    Windows.Beep(500, 100);
    LogMessage(Format(
      'ServerCrypt.dll (%d.%2.2d) is not from the expected version (%d.%2.2d). '
      + 'Clients may not be able to connect the server!',
      [SrvCryptVersion div 100, SrvCryptVersion mod 100,
      ExpectedSrvCryptVersion div 100, ExpectedSrvCryptVersion mod 100]),
      EVENTLOG_ERROR_TYPE, 0, 0);
    AddLogMessage('!! Fatal Error: Invalid ServerCrypt.dll version.');
  end;
  // Uptime Timer, intervall 1sec / 30sec
  FStartTime := Now;
  {$IFDEF UPTIME1S}
  UpTimer.Interval := 1000;
  {$ELSE}
  UpTimer.Interval := 60000;
  {$ENDIF UPTIME1S}
  UpTimer.Enabled := True;

  { Initialize UserData }
  FUserData.ServerLabel := c_DlgCaption + ' V ' + GetSrvVersionString(cServerId, True);
  FUserData.ArchiveTimeStamp := Now;

  // connect to database
  if not FDBMSConnected then
  begin
    Assert((FServerQuery <> nil), 'FServerQuery not initialized.');
    Assert((FServerDatabase <> nil), 'FServerDatabase not initialized.');
    try
      DBOpenErrMsg := ''; // -- Diwic
      {$IFDEF ORACLESRV}
        //--hdo
        // Oracle engine verification not finished yet
        AddLogMessage('Oracle engine version 8.x');
        FServerDatabase.UserName := FDBUser;
        FServerDatabase.Password := FDBPassword;
        FServerDatabase.ServerName := FDBServer;
        // wait for dbserver ready, should be better done over service dependency!
        Retries := 0;
        while not FServerDatabase.Connected and (Retries < FMaximumretries) do
        try
          FServerDatabase.Open;
        except
          on E:Exception do begin
            DBOpenErrMsg := E.Message; // -- Diwic
            AddLogMessage(DBOpenErrMsg); // -- Diwic
            Inc(Retries);
            Sleep(5000);
          end;
        end;
        // raise exception which was catched before
        if not FServerDataBase.Connected then
        begin
          raise EDatabaseError.Create('DB not open!');
        end;
        FServerQuery.DatabaseName := 'LocalSQLDB';
        AddLogMessage(FServerDatabase.ServerVersion);
        AddLogMessage('Oracle client version ' + IntToStr(FServerDatabase.ClientVersionNo));
      {$ENDIF ORACLESRV}
      {$IFDEF MSSQLSRV} //--MG
      // MSSQL engine verification not finished yet
      AddLogMessage('MSSQL engine version 7');
      FServerDatabase.Params.Clear;
        FServerDatabase.Params.Add('Server name=' + FDBServer);
        FServerDatabase.Params.Add('Database name=' + FUserData.DBDatabase);
        if FDBUseNTTrustedConnection then
        begin
          FServerDatabase.Params.Add('Integrated Security=SSPI');
        end
        else
        begin
          FServerDatabase.Params.Add('User Name=' + FDBUser);
          FServerDatabase.Params.Add('Password=' + FDBPassword);
        end;
      // wait for dbserver ready, should be better done over service dependency!
      Retries := 0;
      while not FServerDatabase.Connected and (Retries < FMaximumRetries) do
      try
        FServerDatabase.Open;
      except
        on E: Exception do begin
          DBOpenErrMsg := E.Message; // -- Diwic
          AddLogMessage(DBOpenErrMsg); // -- Diwic
          Inc(Retries);
          Sleep(5000);
        end;
      end;
      // raise exception which was catched before
      if not FServerDatabase.Connected then
      begin
        raise EDatabaseError.Create('DB not open!');
      end;
      FServerQuery.DatabaseName := FServerDatabase;
      {$ENDIF MSSQLSRV}
      {$IFDEF MYSQLSRV} //--LB
      FServerDatabase.HostName:=FDBServer;
      FServerDatabase.Database:=FUserData.DBDatabase;
      FServerDatabase.User:=FDBUser;
      FServerDatabase.Password:=FDBPassword;
      // wait for dbserver ready, should be better done over service dependency!
      Retries := 0;
      while not FServerDatabase.Connected and (Retries < FMaximumretries) do
      try
        FServerDatabase.Connected:=True;
      except
        on E:Exception do begin
          DBOpenErrMsg := E.Message; // -- Diwic
          AddLogMessage(DBOpenErrMsg); // -- Diwic
          Inc(Retries);
          Sleep(5000);
        end;
      end;
      // raise exception which was catched before
      if not FServerDataBase.Connected then
      begin
        raise EDatabaseError.Create('DB not open!');
      end;
      FServerQuery.DatabaseName := FUserData.DBPath;
      with TSrvQuery.Create(nil) do
      begin
        DatabaseName := FUserData.DBPath;
        SQL.Text:='show variables like "version"';
        Open;
        AddLogMessage('MySQL engine version '+Fields[1].AsString);
        Close;
        Free;
      end;
      {$ENDIF MYSQLSRV} // {$IFDEF MYSQLSRV} //--LB
      {$IFDEF UIBSRV} //-- PrY
      AddLogMessage('Firebird (UIB) engine');
      // prepare Firebird Embedded
      if FDBUseEmbeded then
      begin
        AddLogMessage('Uses embedded Firebird version!');
        FDBUser := 'SYSDBA';
        FDBPassword := 'MASTERKEY';
        FDBServer := 'LOCALHOST';
      end;
      FServerDatabase.UserName := FDBUser;
      FServerDatabase.PassWord := FDBPassword;
      if (UpperCase(FDBServer) = 'LOCAL') or
         (UpperCase(FDBServer) = 'LOCALHOST') or (Trim(FDBServer) = '') then
        FServerDatabase.DatabaseName := FDBDatabase
      else
        FServerDatabase.DatabaseName := FDBServer + ':' + FDBDatabase;
      FServerDatabase.CharacterSet := StrToCharacterSet(FDBCharSet);
      FServerDatabase.LibraryName := GetFirebirdClientLibrary(FDBUseEmbeded);
      // wait for dbserver ready, should be better done over service dependency!
      Retries := 0;
      while not FServerDatabase.Connected and (Retries < FMaximumRetries) do
      try
        FServerDatabase.Connected := True;
      except
        on E: Exception do begin
          DBOpenErrMsg := E.Message; // -- Diwic
          AddLogMessage(DBOpenErrMsg); // -- Diwic
          Inc(Retries);
          Sleep(5000);
        end;
      end;
      // raise exception which was catched before
      if not FServerDatabase.Connected then
      begin
        raise EDatabaseError.Create('DB not open!');
      end;
      FServerQuery.DatabaseName := FServerDatabase;
      {$ENDIF UIBSRV}
      {$IFDEF ADOSRV} //--PL
      FServerDatabase.Params.Clear;
      FServerDatabase.Params.Add('User ID=' + FDBUser);
      FServerDatabase.Params.Add('Password=' + FDBPassword);
      FServerDatabase.Params.Add('Provider=' + FDBServer);
      FServerDatabase.Params.Add('Data Source=' + FUserData.DBDatabase);
      Retries := 0;
      while not FServerDatabase.Connected and (Retries < FMaximumRetries) do
      try
        FServerDatabase.Open;
      except
        on E: Exception do begin
          DBOpenErrMsg := E.Message; // -- Diwic
          AddLogMessage(DBOpenErrMsg); // -- Diwic
          Inc(Retries);
          Sleep(5000);
        end;
      end;
      // raise exception which was catched before
      if not FServerDatabase.Connected then
      begin
        raise EDatabaseError.Create('DB not open!');
      end;
      FServerQuery.DatabaseName := FServerDatabase;
      {$ENDIF ADOSRV}
    except
      on E: Exception do
      begin
        Windows.Beep(500, 100);
        LogMessage('Exception: ' + E.Message + ' ' + DBOpenErrMsg +
          ' - Unable to connect DBMS.', EVENTLOG_ERROR_TYPE, 0, 0); // -- Diwic
        AddLogMessage('DBMS Login failed');
        FDBMSConnected := False;
        // no service start if connect to db not established
        Started := False;
        if FUserData.WriteServerLog then SaveLogFile; // -- Diwic
        Exit;
      end;
    end; // try except
    DBMSMsg := 'DBMS connected [';
    AddLogMessage('Version archive server: ' + FDBServer);
    DBMSMsg := DBMSMsg + FDBServer + ']';
    OutputDebugString('Checking Archive version now...');
    if not CheckArchiveVersion then
    begin
      OutputDebugString('Could not upgrade Archive...');
      Started := False;
      FDBMSConnected := False;
      if FUserData.WriteServerLog then SaveLogfile;
      Exit;
    end;
    FDBMSConnected := True;
  end; // if not FDBMSConnected then begin
  OutputDebugString('Upgrade check done...');

  // (Try to) start the application server component
  AppServer1.ClientTimeOut := FUserData.ConnectTimeOut;
  AppServer1.Port := FPort;
  ServerMsg := '';
  try
    AppServer1.Start;
  except
    on E: ESocketException do
    begin
      if (AppServer1.SrvWSocket.LastError = WSAEADDRINUSE) then
        ServerMsg := Format('Port %s is already used by another process.',
          [FPort])
      else
        ServerMsg := Format('Winsock error %s occurred starting server.',
          [E.Message]);
    end; // on E: ESocketException do begin
    on E: Exception do
      ServerMsg := Format('Error %s occurred starting server.', [E.Message]);
  end; // try except
  if ServerMsg = '' then
  begin
    {$IFDEF BETAVER}
    ServerMsg := 'Server successfully started (Beta).';
    {$ELSE}
    ServerMsg := 'Server successfully started.';
    {$ENDIF BETAVER}
    LogMessage(ServerMsg + DBMSMsg, EVENTLOG_INFORMATION_TYPE, 0, 0);
  end
  else LogMessage(ServerMsg, EVENTLOG_ERROR_TYPE, 0, 0);
  AddLogMessage(ServerMsg + DBMSMsg);
end;

//=== Check & Upgrade archive to latest version ================================

function TFreeVCSService.CheckArchiveVersion: Boolean;
var
  nActArchiveVersion: Integer;
begin
  Result := True;
  if Assigned(FServerDatabase) and (FServerDatabase.Connected) then
  begin
    nActArchiveVersion := FServerDatabase.ArchiveVersion;
    if nActArchiveVersion < ExpectedArchiveVersion then
    begin
      AddLogMessage('Archive needs upgrade');
      Result := FServerDatabase.UpgradeArchive;
      if Result then
      begin
        AddLogMessage('Archive upgrade successfull.');
      end
      else
      begin
        AddLogMessage('ERROR on archive upgrade!!!');
      end;
    end;
  end;
end;

//==============================================================================
// A new client has connected
// // --vr
// (evidently here lies the reason for some strange "hang" problems in W2K.
//  It seems as if there is timing problem in the underlying code...)

procedure TFreeVCSService.AppServer1ClientConnected(
  Sender: TObject; CliWSocket: TClientWSocket);
var
  SysVersionInfo: _OSVersionInfoA;
begin
  SysVersionInfo.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
  GetVersionEx(SysVersionInfo);
  // if OS is Windows 2000 then "sleep" for 100 ms
  if SysVersionInfo.dwMajorVersion = 5 then
    Sleep(100);
end;

//==============================================================================
// Add a message from the server or from one of the server objects to the servers
// log file.

procedure TFreeVCSService.AddLogMessage(Value: string);
begin
  if Assigned(FUserData) then
  begin
    if FUserData.WriteServerLog then
      FLog.Add(DateTimeToStr(Now) + ' - ' + Value);
  end;
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FIniFileName);
  try
    IniFile.WriteString(SectionData, KeyPort, FPort);
    // FUserData
    IniFile.WriteBool(SectionData, WriteSrvLog, FUserData.WriteServerLog);
    IniFile.WriteBool(SectionData, CheckIPAddr, FUserData.CheckIPAddr);
    IniFile.WriteInteger(SectionData, ConnectTimeOut, FUserData.ConnectTimeOut);
    IniFile.WriteBool(SectionData, LoginExpires, FUserData.LoginExpires);
    IniFile.WriteFloat(SectionData, LoginTimeOut, FUserData.LoginTimeOut);
    IniFile.WriteBool(SectionData, CasuallyCheckin, FUserData.CasuallyCheckin);
    IniFile.WriteBool(SectionData, WriteVCSLog, FUserData.WriteVCSLog);
    {$IFNDEF ADOSRV} //-- PL
      {$IFNDEF MYSQLSRV}
        {$IFNDEF MSSQLSRV}
          {$IFNDEF ORACLESRV}
                  {$IFNDEF UIBSRV} //-- PrY
                  IniFile.WriteString(SectionData, DBPath, FUserData.DBPath);
                  {$ENDIF ~UIBSRV}
          {$ENDIF ~ORACLESRV}
        {$ENDIF ~MSSQLSRV}
      {$ENDIF ~MYSQLSRV}
    {$ENDIF ~ADOSRV} //-- PL
    IniFile.WriteString(SectionData, BackupPath, FUserData.BackupPath);
    IniFile.WriteFloat(SectionData, LastLiveBackup, FUserData.LastLiveBackup);
    IniFile.WriteBool(SectionData, LocalLogin, FUserData.LocalLogin);
    IniFile.WriteFloat(SectionData, ArchiveTStamp, FUserData.ArchiveTimeStamp);
    IniFile.WriteBool(SectionData, LogAccessFault, FUserData.LogAccessFault);
    //-- hdo
    IniFile.WriteString(SectionData, DBUser, FDBUser);
    IniFile.WriteString(SectionData, DBServer, FDBServer);
    IniFile.WriteString(SectionData, DBPassword, fvcsSrvEnCrypt(FDBPassword));
    {$IFDEF MSSQLSRV}
    IniFile.WriteString(SectionData, DBDatabase, FUserData.DBDatabase);
    {$ENDIF MSSQLSRV}
    {$IFDEF ADOSRV}
    IniFile.WriteString(SectionData, DBDatabase, FUserData.DBDatabase);
    {$ENDIF ADOSRV}
    {$IFDEF MYSQLSRV}
    IniFile.WriteString(SectionData, DBDatabase, FUserData.DBDatabase);
    {$ENDIF MYSQLSRV}
    {$IFDEF UIBSRV} //-- PrY
    IniFile.WriteString(SectionData, DBDatabase, FDBDatabase);
    {$ENDIF UIBSRV}
    IniFile.WriteBool(SectionData, DisableSQLDirekt, FUserData.DisableSQLDirekt);
  finally
    IniFile.Free;
  end;
  { Update the application server component }
  AppServer1.ClientTimeOut := FUserData.ConnectTimeOut;

  // clear 'settings need to be saved' flag
  FUserData.SettingsChanged := False;
end;

//=== Save the server's log file ===============================================

procedure TFreeVCSService.SaveLogfile;
begin
  try
    SaveServerLogFile(FLog);
    FLog.Clear;
  except
    on E:Exception do
    begin
      LogMessage('Log file access error: ' + GetJvcsServerLogFile + ' - ' + E.Message,
        EVENTLOG_WARNING_TYPE, 0, 0);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.AppServer1AfterSendReply(Sender: TObject;
  CliWSocket: TClientWSocket);
begin
  { Did we received an encrypted request ? }
  if CliWSocket.UserData = 0 then
      Exit; { No, nothing to do here ! }

  { Memory was allocated in the OnBeforeSendReply event handler. We have }
  { to free it to avoid memory leaks.                                    }
  if CliWSocket.ReplyHeaderLen > 0 then
      FreeMem(CliWSocket.ReplyHeader, CliWSocket.ReplyHeaderLen);
  if CliWSocket.ReplyBodyLen > 0 then
      FreeMem(CliWSocket.ReplyBody,   CliWSocket.ReplyBodyLen);
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.AppServer1BeforeProcessRequest(Sender: TObject;
  CliWSocket: TClientWSocket; var CmdBuf: Pointer; var CmdLen: Integer);
var
  Cnt: Integer;
  {$IFDEF ORACLESRV}
  I: Integer; //-- hdo
  EventMsg: string;
  {$ENDIF ORACLESRV}
begin
  // save statistic data
  FUserData.ReceivedBytes := FUserData.ReceivedBytes + CmdLen;

  { Check if we have some encrypted data }
  if (CmdLen < 3) or (PChar(CmdBuf)[0] <> #3) then
    CliWSocket.UserData := 0
  else
  begin
    { We've got encrypted data. Decrypt on same place. }
    Decrypt(CmdBuf, CmdLen, PChar(CmdBuf), Cnt);
    { Remember we received encrypted data.     }
    { Later we will then send encrypted reply. }
    CliWSocket.UserData := 1;
  end;
  {$IFDEF ORACLESRV}
  //-- hdo
  //-- make sure we're connected to DB server
  with FServerQuery do
  begin
    SQL.Clear;
    SQL.Add('Select 1 from dual');
    try
      Open;
      Close;
    except
      on E: EOCINativeError do
      with E as EOCINativeError do
      begin
        EventMsg := DateTimeToStr(Now) + ' - ' + E.Message + #10#13 +
                    'Errors reported: ';
        for I := 0 To E.ErrorCount - 1 do
          EventMsg := EventMsg + IntToStr(I + 1) + '. NativeError:' +
            IntToStr(Errors[I].ErrorCode) + #10#13;

        LogMessage(EventMsg, EVENTLOG_WARNING_TYPE, 0, 0);

        //-- not connected -> try to reconnect!
        with FServerDatabase do
        begin
          Close; // Zuerst die Verbindung schliessen!
          Open;
          LogMessage('Re-Connecting with DB...Connected.',
            EVENTLOG_WARNING_TYPE, 0, 0);
        end;

      end; // of with E as EDBEngineError
    end; // of try open
  end; // of with FQuery
  {$ELSE}
  FServerDatabase.AcquireConnection;
  {$ENDIF ORACLESRV}
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.AppServer1BeforeSendReply(
 Sender: TObject; CliWSocket: TClientWSocket);
var
  Dst: PChar;
  DstLen: Longint;
begin
  // save statistic data
  FUserData.TransmittedBytes := FUserData.TransmittedBytes +
                               CliWSocket.ReplyHeaderLen +
                               CliWSocket.ReplyBodyLen;

  { Did we received an encrypted request ? }
  if CliWSocket.UserData = 0 then
      Exit; { No, nothing to do here ! }

  Encrypt(CliWSocket.ReplyHeader, CliWSocket.ReplyHeaderLen,
          CliWSocket.ReplyBody,   CliWSocket.ReplyBodyLen,
          Dst, DstLen, 1);
  Dst[0] := #$FF;
  CliWSocket.ReplyHeader    := Dst;
  CliWSocket.ReplyHeaderLen := DstLen;
  CliWSocket.ReplyBody      := nil;
  CliWSocket.ReplyBodyLen   := 0;
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.AppServer1Display(Sender: TObject; Msg: string);
begin
  AddLogMessage(DateTimeToStr(Now) + ' - ' + Msg);
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.CleanUp;
begin
  // settings need to be saved?
  if FUserData.SettingsChanged then SaveSettings;
  if AppServer1.ClientCount > 0 then
  begin
    Windows.Beep(500, 100);
    LogMessage(IntToStr(AppServer1.ClientCount) +
      ' clients already connected! Terminating server!!!',
      EVENTLOG_WARNING_TYPE, 0, 0);
  end;
  AppServer1.Stop;
  AppServer1.DisconnectAll;
  {$IFNDEF DEBUG}
  // invalidate all login acounts?
  // we use a try..except statement here to be sure that the service can be
  // terminated if the DB server hangs or is absent
  try
    if FDBMSConnected then
      with FServerQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT * FROM transact');
        Open;
        if not Eof then
        begin
          Close;
          SQL.Clear;
          SQL.Add('DELETE FROM transact');
          ExecSQL;
        end // if not EoF then
        else Close;
      end; // if FDBMSConnected then begin
  except
    on E: Exception do
    begin
      LogMessage('DBMS error while terminating server: ' + E.Message,
        EVENTLOG_WARNING_TYPE, 0, 0);
    end;
  end;
  {$ENDIF ~DEBUG}
  // complete server log
  if FUserData.WriteServerLog then
  begin
    FLog.Add(DateTimeToStr(Now) + ' - Server closed.');
    FLog.Add('Uptime: ' + FUserData.UpTimeStr);
    FLog.Add('Requests: ' + IntToStr(FUserData.RequestCount));
    FLog.Add(Format('Received bytes: %n MB',
                                        [(FUserData.ReceivedBytes / 1048576)]));
    FLog.Add(Format('Transmitted bytes: %n MB',
                                     [(FUserData.TransmittedBytes / 1048576)]));
    FLog.Add('UserCount: ' + IntToStr(FUserData.UserCount));
    FLog.Add('New files: ' + IntToStr(FUserData.NewFilesCount));
    FLog.Add('Get modules: ' + IntToStr(FUserData.GetCount));
    FLog.Add('Checked In modules: ' + IntToStr(FUserData.CheckinCount));
    FLog.Add('Checked Out modules: ' + IntToStr(FUserData.CheckoutCount));
    FLog.Add('***************************************************************');
    SaveLogfile;
  end;

  // Destroying objects
  FreeAndNil(FServerQuery);
  FreeAndNil(FServerDatabase);
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.ServiceShutdown(Sender: TService);
begin
  CleanUp;
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  CleanUp;
  Stopped := True;
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.UpTimerTimer(Sender: TObject);
var
  Value: Double;
  M10Counter: Integer;
begin
  // calculate server uptime
  FUpTime := Now - FStartTime;
  Value := Int(FUpTime);
  UPTimeStr := FloatToStr(Value) + ' d, ';
  FUpTime := FUpTime - Value;
  FUpTime := FUpTime * 24;
  Value := Int(FUpTime);
  UPTimeStr := UPTimeStr + FloatToStr(Value) + ' h, ';
  FUpTime := FUpTime - Value;
  FUpTime := FUpTime * 60;
  Value := Int(FUpTime);
  M10Counter := Round(Value);
  if (M10Counter mod 10) = 0 then
  begin
    // every 10 minutes
    // calculate difference between local time & GMT
    GetLocalGMTDifference;
    // log file need to be saved?
    if FUserData.WriteServerLog then SaveLogfile;
    // settings need to be saved?
    if FUserData.SettingsChanged then SaveSettings;
  end;
  {$IFDEF UPTIME1S}
  UPTimeStr := UPTimeStr + FloatToStr(Value) + ' m, ';
  FUpTime := FUpTime - Value;
  FUpTime := FUpTime * 60;
  Value := Int(FUpTime);
  UPTimeStr := UPTimeStr + FloatToStr(Value) + ' s';
  {$ELSE}
  UPTimeStr := UPTimeStr + FloatToStr(Value) + ' m';
  {$ENDIF UPTIME1S}
  FUserData.RequestCount := AppServer1.RequestCount;
  FUserData.UpTimeStr := UPTimeStr;
end;

//=== Get the servers real IP number ===========================================

function TFreeVCSService.GetLocalIP: string;
type
  TaPInAddr = array [0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array [0..63] of Char;
  I: Integer;
  GInitData: TWSAData;
begin
  try
    WSAStartup($101, GInitData);
    try
      Result := '';
      GetHostName(Buffer, SizeOf(Buffer));
      phe := GetHostByName(Buffer);
      if phe = nil then Exit;
      pptr := PaPInAddr(phe^.h_addr_list);
      I := 0;
      while pptr^[I] <> nil do
        begin
          Result := inet_ntoa(pptr^[I]^);
          Inc(I);
        end;
    finally
      WSACleanup;
    end;
  except
    Result := '';
  end;
end;


//------------------------------------------------------------------------------

procedure TFreeVCSService.ServiceAfterInstall(Sender: TService);
begin
  with TRegistry.Create do
  try
    Rootkey := HKEY_LOCAL_MACHINE;
    if OpenKey('SYSTEM\CurrentControlSet\Services\EventLog\Application', False) then
    begin
      if OpenKey(Self.Name, True) then
      begin
        WriteString('EventMessageFile', ParamStr(0));
        WriteInteger('TypesSupported', 7);
        CloseKey;
      end;
      CloseKey;
    end;
// Create description for this Service in Windows ServiceManager
    if OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, false) then
    begin
      WriteString ( 'Description'
                  , Format( 'Handles JEDI Version Control Client requests based on %s. See also %s'
                          , [GetSrvVersionString(cServerId), GetJVCSInternetLink(ilHomepage)]
                          )
                  );
      CloseKey;
    end;
  finally;
    Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TFreeVCSService.ServiceAfterUninstall(Sender: TService);
begin
  with TRegistry.Create do
  try
    Rootkey := HKEY_LOCAL_MACHINE;
    if OpenKey('SYSTEM\CurrentControlSet\Services\EventLog\Application', False) then
    begin
      if KeyExists(Self.Name) then
        DeleteKey(Self.Name);
      CloseKey;
    end;
  finally
    Free;
  end;
end;

end.

