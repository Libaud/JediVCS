{ $NoKeywords } // Prevent JVCS from expanding keywords in this file
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SvrMain.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

  The server source code contains or uses code parts from:
  - Holger Dors - dors@kittelberger.de (NTServiceEnum function)
  - Marco Gosselink - mgosselink@zonnet.nl (MSSQL-Server/ NT service).
    MSSQL server port based on ADO.
    For MSSQL specific questions please contact Marco directly.
  - Ludo Brands <freevcsmysql@free.fr> (MySQL Server Port)
  - ZeosDBO (http://zeoslib.sourceforge.net)
  - ESB Consultancy (ESBDates).
  - Dmitry Arefiev (NCOCI8).
  - Chami (Start/Stop NT services).

  and François Piette, of course...

  Note: The JVCS server source supports currently following Database systems
        (Oracle 9+, MSSQL 7/2000, Firebird, ADO) by conditional compiler directives.
        The Windows service can also be compiled from this source.
        See cond_def.inc, SrvCustomSrvOBJ.pas & SrvDBDef.pas for details.

  Min. compiler version required to re-compile the source:

  Database       |Server   | Service
  ---------------+---------+---------
  Firebird       |D567 pro |D567 pro
  ---------------+---------+---------
  Oracle native  |D567 pro |D567 pro
  ---------------+---------+---------
  MSSQL          |D567 C/S |D567 C/S

  Third-party components required to re-compile the source:

  Component                          |required by     |Status
  -----------------------------------+----------------+----------
  Francois Piette's ICS & Midware    |all servers     |Freeware
  -----------------------------------+----------------+----------
  Dmitry Arefiev's NCOCI8 components |Oracle native   |Freeware
  -----------------------------------+----------------+----------
  ZeosDBO                            |MySQL server    |Freeware

Last Modified: see History

Known Issues:
- code cleanup necessary
Nov/04:
- USc: possible translation bugs:
    - Pos('Errcode', ... in RemoteDirectoryExists
      (could be a problem if we use translated VCL messages)
- JVCSRES_Server_banner58_34Welcome_to_37s34 and JVCSRES_Welcome_to_37s
  - JVCSRES_Server_banner58_34Welcome_to_37s34 should use the Banner and not
    build the banner string itself because could lead to a difference
  - should the banner stay in english?
ToDo
- change UpTimerTimer -> see VCSProcBase.pas GetUpTimeStr
-----------------------------------------------------------------------------

Unit history:

  Aug '99 by Thomas Hensle - freevcs@thensle.de - http://www.freevcs.de

2003/02/04  HDors     - 1st Migrationstep from FreeVCS code to JEDI VCS
2003/02/09  HDors     - Add MySQL Server Port by Ludo Brands
2003/02/22  THuber    - Add  F I B P L U S S R V  port which uses  F I B p l u s  components
                        therefor dangerous casts changed:
                         * TDateTime(...) => .AsDateTime
                         * Integer(...) => .AsInteger
                         * FQuery.Fields[Fld] => FQuery.Fields[Fld].AsString
                      - removed  I n t e r b a s e 5  support
                      - D4_UP directive removed
2003/02/28  THuber    - now use FieldByName everywhere
2003/03/01  THuber    - about: added  F I B +  credits, changes for JEDI VCS
                      - bugfix: databasename was not saved to inifile for  F I B  +
2003/03/02  THuber    - removed compiler warnings
2003/03/03  THuber    - changed handling for server port version detecting
2003/03/07  THuber    - removed A CCESSCTRL IFDEF
2003/03/08  THuber    - make source compile with d b i s a m  3.x
2003/03/09  THuber    - removed d b isam200 ifdef's, now only  d b i s a m s r v  used
2003/04/16  MGosselink- Changing the target directory for live backup produced
                        an error in  I N F O R M I X S R V  and MSSQLSRV -> solved
2003/09/21  PLauret   - Implementation ADO Connection : Under construction
2003/09/28  PLauret   - ADO port : in construction
2003/11/16  USchuster - new About dialog (mantis #658)
2003/11/16  THuber    - Ini file now for all server ports in appserver dir
                        (was only for   d b i s a m)
                      - fixed missing logon dialog for firebird port
                      - added Firebird embededed checkbox option
                        (needs Firebird embeded dll's in application folder)
2003/11/21  THuber    - additional message and log entry if  d b i s a m  3.00
                        migration is been canceled by user.
2003/12/21  THuber    - Bugfix in logon procedure for Standalone  F i b  Appserver
                        embeded option was never saved to config file.
2003/12/23  CSchuette - Added some settings to F l a s  F i l e r  local server
2003/12/26  THuber    - DlgCaption => c_DlgCaption
2003/12/27  THuber    - removed Src Version const (no use)
2004/01/04  THuber    - Connect SrvDatabase for  D B I S A M  and set databasename
                      - started on automatic archive upgrading
2004/06/25  THuber    - Changed messages for upgrading archive
                      - Appserver will not be closed automatically if an
                        error occured during upgrade.
                      - Menuitem to run database archive upgrade manually.
2004/10/31  USchuster - added space between MainTitle and GetSrvVersionString
2004/11/03  CSchuette - added "Hide Window on Close" setting, if set server
                        will not terminate when window is closed and only go
                        to the system tray.
                        added "Terminate server?" question on FormCloseQuery.
2004/11/07  CSchuette - fixed shutdown problems with new "Hide Window on Close
                        setting. Server will handle WM_QUERYENDSESSION and
                        WM_ENDSESSION correctly now (Mantis #2025)
2004/11/17  USchuster - style cleaning
2004/11/20  USchuster - added dxGetText support for localization with over IFDEF LANGUAGE
                      - res strings and displayed strings changed to resourcestrings
                      - added language menu item below "Display" and set the
                        default textdomain to "jvcssrv"
2004/11/29  CSchuette - added option to change server banner (Mantis #2296)
2004/11/30  CSchuette - added Application.ProcessMessages before the window is hidden
                        in WMAppStartup.

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster - changes for Firebird port over JVCL UIB components 
                        by Pierre Y. (partly adapted)
2004/12/11  USchuster - added register functions for new server objects
2004/12/18  USchuster - changes for Zeos DBO 6.x
2005/01/14  THuber   #2502 added mssql option for trusted nt connections
2005/02/13  THuber   #2570 added option characterset (lc_ctype) for firebird
2005/03/18  USchuster - changes to use default banner if user defined banner is
                        empty (mantis #2296)
2006/10/11  USchuster - changes for Oracle performance tweak "UseRuleOptimizerMode" (mantis #3950)
2007/07/01  USchuster - changes for large fonts (contraints size depend now on PixelsPerInch; analog to Mantis #3710)
2007/10/06  USchuster - changes for (MySQL) connection timeout solution (Mantis #4260)
2009/01/17  USchuster - added register functions for property server objects
2009/07/11  USchuster - changes for latest UIB from SVN (JvUIB -> UIB)
2009/11/30  THuber    #5032 change server log location
2009/12/13  USchuster - changes for opt-out of using table latestrevision for performance increase
--- 2.50 Beta 2 was released...
2009/12/22  THuber   #5062 add support for firebird 2.x databases, fixed broken embedded support
                     #4085 add support for connection of unicode databases over UIB
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     Removed unnecessary L Z H Define as it's always used
                     #5066 support for  F l a s h F i l e r  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
2011/01/15  USchuster - changed font to Tahoma                     

-----------------------------------------------------------------------------*)

unit SrvMain;

{$I jedi.inc}
{$I compopt.inc}

{$IFDEF DELPHI6_UP}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  {$ifdef LCL}LCLIntf, LCLType, LMessages,{$endif}
  {$ifndef LCL}WSocket, ApSrvCli, RFormat, RBroker, ApServer,{$endif}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IniFiles,
  Menus, StdCtrls, ExtCtrls, WinSock, SrvUDRec, SrvDBDef, DB, ComCtrls,
  CipherLz,
  SrvAbout;

const
  WM_APPSTARTUP = WM_APP + 100;   // startup message
  WM_ICONCALLBACK = WM_APP + 101; // taskbar icon (double-click) callback

type
  TServerForm = class(TForm)
    Panel1: TPanel;
    RequestBroker1: TRequestBroker;
    AppServer1: TAppServer;
    MainMenu1: TMainMenu;
    Server1: TMenuItem;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    Disconnectall1: TMenuItem;
    Functionlist1: TMenuItem;
    N3: TMenuItem;
    Exit1: TMenuItem;
    Hide1: TMenuItem;
    N4: TMenuItem;
    About1: TMenuItem;
    Port1: TMenuItem;
    N6: TMenuItem;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    ClientCountLabel: TLabel;
    Label7: TLabel;
    UpTimeLabel: TLabel;
    UpTimer: TTimer;
    Label5: TLabel;
    SrvLogLabel: TLabel;
    Log1: TMenuItem;
    Logfileactive1: TMenuItem;
    Showlogfile1: TMenuItem;
    TATimer: TTimer;
    Versionarchive1: TMenuItem;
    Label6: TLabel;
    RequestCountLabel: TLabel;
    DisplayRE: TRichEdit;
    Display1: TMenuItem;
    Clear1: TMenuItem;
    Backup1: TMenuItem;
    Targetfolder1: TMenuItem;
    Execute1: TMenuItem;
    Directory1: TMenuItem;
    Copytoclipboard1: TMenuItem;
    Savetofile1: TMenuItem;
    N1: TMenuItem;
    SaveDialog1: TSaveDialog;
    Clienttimeout1: TMenuItem;
    N5: TMenuItem;
    Clearlogfile1: TMenuItem;
    N7: TMenuItem;
    Autobackup1: TMenuItem;
    N8: TMenuItem;
    Label8: TLabel;
    AutoBkpLabel: TLabel;
    AutostartwithWindows1: TMenuItem;
    N9: TMenuItem;
    Help1: TMenuItem;
    Cliemts1: TMenuItem;
    LogoutAll1: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N2: TMenuItem;
    ShowDebugMessages1: TMenuItem;
    LogClientAccessFaults1: TMenuItem;
    DisableServerBanner1: TMenuItem;
    N14: TMenuItem;
    AllClients1: TMenuItem;
    N15: TMenuItem;
    ClientLists1: TMenuItem;
    LoggedIn1: TMenuItem;
    StartHidden1: TMenuItem;
    Security1: TMenuItem;
    VerifyClientTimestamp1: TMenuItem;
    N13: TMenuItem;
    Label9: TLabel;
    Label10: TLabel;
    RcvBytesLabel: TLabel;
    TrmBytesLabel: TLabel;
    Label3: TLabel;
    PortLabel: TLabel;
    TimeOutLabel: TLabel;
    Label1: TLabel;
    DisableSQLObject1: TMenuItem;
    N12: TMenuItem;
    ForceGlobalSettings1: TMenuItem;
    Upgradearchive1: TMenuItem;
    HideOnClose1: TMenuItem;
    N17: TMenuItem;
    ChangeServerBanner1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AppServer1ClientConnected(Sender: TObject; CliWSocket: TClientWSocket);
    procedure AppServer1ClientClosed(Sender: TObject; CliWSocket: TClientWSocket);
    procedure AppServer1Display(Sender: TObject; Msg: string);
    procedure FormDestroy(Sender: TObject);
    procedure AppServer1BeforeSendReply(Sender: TObject;
      CliWSocket: TClientWSocket);
    procedure AppServer1AfterSendReply(Sender: TObject;
      CliWSocket: TClientWSocket);
    procedure AppServer1BeforeProcessRequest(Sender: TObject;
      CliWSocket: TClientWSocket; var CmdBuf: Pointer; var CmdLen: Integer);
    procedure AppServer1AfterProcessRequest(Sender: TObject;
      CliWSocket: TClientWSocket; var CmdBuf: Pointer; var CmdLen: Integer);
    procedure Disconnectall1Click(Sender: TObject);
    procedure Functionlist1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Port1Click(Sender: TObject);
    procedure Clienttimeout1Click(Sender: TObject);
    procedure UpTimerTimer(Sender: TObject);
    procedure Start1Click(Sender: TObject);
    procedure Stop1Click(Sender: TObject);
    procedure Hide1Click(Sender: TObject);
    procedure Logfileactive1Click(Sender: TObject);
    procedure Showlogfile1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure About1Click(Sender: TObject);
    procedure TATimerTimer(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure Targetfolder1Click(Sender: TObject);
    procedure Execute1Click(Sender: TObject);
    procedure Directory1Click(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
    procedure Savetofile1Click(Sender: TObject);
    procedure Clearlogfile1Click(Sender: TObject);
    procedure Autobackup1Click(Sender: TObject);
    procedure AutostartwithWindows1Click(Sender: TObject);
    procedure LogoutAll1Click(Sender: TObject);
    procedure ShowDebugMessages1Click(Sender: TObject);
    procedure LogClientAccessFaults1Click(Sender: TObject);
    procedure DisableServerBanner1Click(Sender: TObject);
    procedure AllClients1Click(Sender: TObject);
    procedure LoggedIn1Click(Sender: TObject);
    procedure StartHidden1Click(Sender: TObject);
    procedure VerifyClientTimestamp1Click(Sender: TObject);
    procedure DisableSQLObject1Click(Sender: TObject);
    procedure ForceGlobalSettings1Click(Sender: TObject);
    procedure Upgradearchive1Click(Sender: TObject);
    procedure HideOnClose1Click(Sender: TObject);
    procedure ChangeServerBanner1Click(Sender: TObject);
  private
    { Déclarations privées }
    FServerQuery: TSrvQuery;
    FServerDatabase: TSrvDatabase;
    FUserData: PUserDataRecord;
    FInitialized,
    FAutoBackupActive,
    FHideWindow,
    FDBMSConnected,
    FAppServerStarted,
    FInExit,
    FInEndSession: Boolean;
    FIniFileName,
    FPort: string;
    FBanner: string;
    FDefaultBanner: string;
    FStartTime,
    FUpTime,
    FAutoBackupTime,
    FLastBackup: Double;
    FLog: TStringList;
    // Taskbar icon
    FSrvIcon: TIcon;
    //-- hdo
    FDBUser,
    FDBPassword,
    FDBServer: string;
    {$IFDEF UIBSRV} //-- PrY
    FDBDatabase: string;
    FDBUseEmbeded: Boolean;
    FDBCharSet: string;
    BackupSize: Integer;
    {$ENDIF UIBSRV}
    {$IFDEF MSSQLSRV}
    FDBUseNTTrustedConnection: Boolean;
    {$ENDIF MSSQLSRV}
    {$IFDEF MYSQLSRV}
    function RemoteDirectoryExists(sDir: string): Boolean;
    {$ENDIF MYSQLSRV}
    function  LocalDT2GMTDT(const LocalDT: Double): Double;
    procedure GetLocalGMTDifference;
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;
    function  StartApplicationServer(var ErrMsg: string): Boolean;
    function  EnumServerFunctions(Sender: TObject;
                                  FunctionCode: string): Boolean;
    function CheckArchiveVersion: Boolean;
    procedure AddMessage(Value: string; const LogMsg: Boolean);
    procedure SaveSettings;
    procedure SaveLogfile;
    procedure GetLogfileSize;
    procedure SetupUI;
    // Taskbar icon
    procedure ShowIcon(const Hint: string);
    procedure HideIcon;
    procedure WMIconCallback(var Msg: TMessage); message WM_ICONCALLBACK;
    {$IFDEF DBBACKUPSUPPORT}
    procedure LiveBackup(Location: string);
    {$ENDIF DBBACKUPSUPPORT}
    function  GetLocalIP: string;
    {$IFDEF UIBSRV}
    procedure OnBackupVerbose(Sender: TObject; Line: string);
    {$ENDIF UIBSRV}
    procedure WMQueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;
    procedure WMEndSession(var Msg: TMessage); message WM_ENDSESSION;
  public
  end;

var
  ServerForm: TServerForm;

implementation

uses
  ShellAPI,
  {$IFNDEF MYSQLSRV}
  FileCtrl,
  {$ENDIF ~MYSQLSRV}
  Registry, ServerCryptInt, SrvConst,
  {$IFDEF DBBACKUPSUPPORT}
    SrvAutoBkp,
    {$IFDEF UIBSRV} //-- PrY
    UIB,
    UIBLIB,
    {$ENDIF UIBSRV}
  {$ENDIF DBBACKUPSUPPORT}
  SrvLogon,
  {$IFDEF ORACLESRV}
  NCOciWrapper,
  {$ENDIF ORACLESRV}
  {$IFDEF LANGUAGE}
  JvGnugettext, JVCSLanguage,
  {$ENDIF LANGUAGE}
  VCSCommon,JVCSServerFunctions,
  {$IFDEF NEWOBJECTS}
  JVCSSrvOBJNewObjects,
  {$ENDIF NEWOBJECTS}
  {$IFDEF BRANCHOBJECTS}
  JVCSSrvOBJBranches,
  {$ENDIF BRANCHOBJECTS}
  {$IFDEF PROPERTYOBJECTS}
  JVCSSrvOBJProperties,
  {$ENDIF PROPERTYOBJECTS}
  SrvOBJMain, SrvOBJReport, SrvOBJFeatures, SrvOBJLog_ToDo,
  SrvOBJModules, SrvOBJProjects, SrvOBJBugTrack, JVCSSrvResources;

{$R *.dfm}
{$R icon.res} // taskbar icon

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


//=== Convert server's (actual) local timestamp to GMT =========================

function TServerForm.LocalDT2GMTDT(const LocalDT: Double): Double;

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

procedure TServerForm.FormCreate(Sender: TObject);
begin
  try
    Constraints.MinHeight := MulDiv(230, PixelsPerInch, 96);
    Constraints.MinWidth := MulDiv(340, PixelsPerInch, 96);
    { Initialize some variables }
    FLog := nil;
    FSrvIcon := nil;
    FUserData := nil;
    FDBMSConnected := False;
    FAppServerStarted := False;
    FInExit := False;
    FInEndSession := False;
    SetupUI;

    FServerDatabase := TSrvDatabase.Create(Self);
    FServerQuery := TSrvQuery.Create(Self);
    {$IFDEF SQL_DEBUG}
    FServerQuery.OnDisplay := AppServer1Display;
    {$ENDIF SQL_DEBUG}

    { Build Ini file name }
    // Location in Appserver folder (allows multiple instances)
    FIniFileName := LowerCase(ChangeFileExt(Application.ExeName, '.ini'));

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
      // Direct SQL
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

    { Initialize user interface }
    UpTimeLabel.Caption := '0 d, 0 h, 0 m';
    ClientCountLabel.Caption := '0';
    RequestCountLabel.Caption := '0';
    RcvBytesLabel.Caption := '0';
    TrmBytesLabel.Caption := '0';
    SrvLogLabel.Caption := '';
    AutoBkpLabel.Caption := '';
    DisplayRE.SelAttributes.Color := clWindowText;
    DisplayRE.Clear;
    DisplayRE.MaxLength := High(Integer);

    // TaskBar icon
    FSrvIcon := TIcon.Create;
    FSrvIcon.Handle := LoadIcon(hInstance, 'SRV_ICON');
    ShowIcon(MainTitle);

    Start1.Enabled := True;
    Stop1.Enabled := not Start1.Enabled;
    {$IFDEF DBBACKUPSUPPORT}
    Backup1.Enabled := True;
    {$ELSE}
    Backup1.Enabled := False;
    {$ENDIF DBBACKUPSUPPORT}
    {$IFDEF MYSQLSRV}
    Targetfolder1.Enabled:=False;
    {$ENDIF MYSQLSRV}
    Directory1.Caption := JVCSRES_Logon_to_38database464646;
    {$IFDEF DEBUG}
    ShowDebugMessages1.Enabled := True;
    {$ELSE}
    ShowDebugMessages1.Enabled := False;
    {$ENDIF DEBUG}
  finally
    {$IFDEF LANGUAGE}
    TextDomain('jvcssrv');
    AddDomainForResourceString('jvcssrv');
    //Translate form
    TranslateComponent(Self);
    AddLanguageMenuItem(Display1, 'jvcssrv');
    {$ENDIF LANGUAGE}
  end;
end;

//------------------------------------------------------------------------------

procedure TServerForm.GetLocalGMTDifference;
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

procedure TServerForm.FormShow(Sender: TObject);
var
  IniFile: TIniFile;
begin
  if not FInitialized then
  begin
    FInitialized := True;
    Caption := Format(JVCSRES_Starting_37s, [MainTitle]);

    // Prepare Log Info
    if not Assigned(FLog) then
      FLog := TStringList.Create;

    // Servers Ini
    IniFile := TIniFile.Create(FIniFileName);
    try
      Width := IniFile.ReadInteger(SectionWindow, KeyWidth, Width);
      Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
      Top := IniFile.ReadInteger(SectionWindow, KeyTop,
                                                (Screen.Height - Height) div 2);
      Left := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                                  (Screen.Width - Width) div 2);
      FHideWindow := IniFile.ReadBool(SectionWindow, HideWindow, False);
      FPort := IniFile.ReadString(SectionData, KeyPort, '2106');
      FAutoBackupActive := IniFile.ReadBool(SectionData, AutoBackupActive,
                                                                         False);
      // Auto backup default = 01:00
      FAutoBackupTime := IniFile.ReadFloat(SectionData, AutoBackupTime, (1/24));
      FLastBackup := IniFile.ReadFloat(SectionData, LastBackup, 0);
      // FUserData
      FUserData.WriteServerLog := IniFile.ReadBool(SectionData, WriteSrvLog,
                                                                         False);
      // Connect timeout default = 60sec
      FUserData.ConnectTimeOut := IniFile.ReadInteger(SectionData,
                                                            ConnectTimeOut, 60);
      // Login timeout default = 1h
      FUserData.LoginTimeOut := IniFile.ReadFloat(SectionData, LoginTimeOut,
                                                                          1/24);
      FUserData.LoginExpires := IniFile.ReadBool(SectionData, LoginExpires,
                                                                         False);
      FUserData.CasuallyCheckin := IniFile.ReadBool(SectionData,
                                                         CasuallyCheckin, True);
      FUserData.WriteVCSLog := IniFile.ReadBool(SectionData, WriteVCSLog, True);
      FUserData.CheckIPAddr := IniFile.ReadBool(SectionData, CheckIPAddr,
                                                                         False);
      FUserData.BackupPath := IniFile.ReadString(SectionData, BackupPath, '');
      FUserData.LastLiveBackup := IniFile.ReadFloat(SectionData,
                                                             LastLiveBackup, 0);
      FUserData.LocalLogin := IniFile.ReadBool(SectionData, LocalLogin, False);
      FUserData.ArchiveTimeStamp := IniFile.ReadFloat(SectionData,
                                             ArchiveTStamp, LocalDT2GMTDT(Now));
      FUserData.LogAccessFault := IniFile.ReadBool(SectionData,
                                                         LogAccessFault, False);
      //-- hdo
      FDBUser := IniFile.ReadString(SectionData, DBUser, '');
      FDBPassword := fvcsSrvDeCrypt(IniFile.ReadString(SectionData,
                                                               DBPassword, ''));
      FDBServer := IniFile.ReadString(SectionData, DBServer, '');
      FUserData.DBAutoLogon := IniFile.ReadBool(SectionData, DBAutoLogon, False);
      {$IFDEF ORACLESRV}
      FServerDatabase.UseRuleOptimizerMode := IniFile.ReadBool(SectionData, UseRuleOptimizerMode, False);
      {$ENDIF ORACLESRV}
      {$IFDEF MSSQLSRV}
      //-- MG
      FUserData.DBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FUserData.ServerName := FDBServer; // MG 23-10-2000
      FDBUseNTTrustedConnection := IniFile.ReadBool(SectionData, FmssqlUseNTTrustedConnection, False);
      {$ENDIF MSSQLSRV}
      {$IFDEF ADOSRV}
      //-- PL
      FUserData.DBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FUserData.ServerName := FDBServer;
      {$ENDIF ADOSRV}
      {$IFDEF MYSQLSRV}
      //-- LB
      FUserData.DBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FUserData.ServerName := FDBServer;
      {$ENDIF MYSQLSRV}
      {$IFDEF UIBSRV} //-- PrY
      FDBDatabase := IniFile.ReadString(SectionData, DBDatabase, '');
      FUserData.ServerName := FDBServer;
      FDBUseEmbeded := IniFile.ReadBool(SectionData, FibUseEmbeded, True);
      {$ENDIF UIBSRV}
      {$IFDEF USELATESTREVISION}
      FUserData.UseLatestRevisions := IniFile.ReadBool(SectionData, 'UseLatestRevisions', True);
      {$ENDIF USELATESTREVISION}      
      {$IFDEF DEBUG}
      ShowDebugMessages1.Checked := IniFile.ReadBool(SectionData,
                                                            ShowDebugMsg, True);
      {$ENDIF DEBUG}
      DisableServerBanner1.Checked := IniFile.ReadBool(SectionData,
                                                    DisableServerBanner, False);
      StartHidden1.Checked := IniFile.ReadBool(SectionData,
                                                            StartHidden, False);
      VerifyClientTimestamp1.Checked := IniFile.ReadBool(SectionData,
                                                  VerifyClientTimestamp, False);
      FUserData.DisableSQLDirekt := IniFile.ReadBool(SectionData,
                                                        DisableSQLDirekt, True);
      ForceGlobalSettings1.Checked := IniFile.ReadBool(GlobalUserData,
                                                    ForceGlobalSettings, False);
      HideOnClose1.Checked := IniFile.ReadBool(SectionData,
                                                            HideOnClose, False);
      FDefaultBanner := Format(JVCSRES_Welcome_to_37s, [MainTitle]);
      FBanner := IniFile.ReadString(SectionData, ServerBanner, FDefaultBanner);
    finally
      IniFile.Free;
    end;

    if FBanner = '' then
      FBanner := FDefaultBanner;
    if DisableServerBanner1.Checked then
      AppServer1.Banner := ''
    else
      AppServer1.Banner := FBanner;

    TimeOutLabel.Caption := Format(JVCSRES_37d_sec, [FUserData.ConnectTimeOut]);
    PortLabel.Caption := FPort;
    FUserData.ShowServerMsg := True;
    Logfileactive1.Checked := FUserData.WriteServerLog;
    DisableSQLObject1.Checked := FUserData.DisableSQLDirekt;

    // calculate difference between local time & GMT
    GetLocalGMTDifference;

    // get the machines real IP number
    FUserData.ServerIPNo := GetLocalIP;

    // Server log state/ size
    FUserData.ServerPath := LowerCase(ExtractFilePath(Application.ExeName));
    // we changed the folder location for ther server log with 2.45 and need
    // to move the existing to the new location.
    MoveServerLog2CommonAppFolder;
    
    if FUserData.WriteServerLog then
      GetLogfileSize
    else
      SrvLogLabel.Caption := JVCSRES_not_active;

    LogClientAccessFaults1.Checked := FUserData.LogAccessFault;

    // Autostart option
    with TRegistry.Create do
    begin
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', False) then
        begin
          if ValueExists('FreeVCSServer') then
            AutostartwithWindows1.Checked :=
              (LowerCase(ReadString('FreeVCSServer')) =
                                                 LowerCase(Application.ExeName))
          else
            AutostartwithWindows1.Checked := False;
          CloseKey;
        end // if OpenKey...
        else AutostartwithWindows1.Checked := False;
      finally
        Free;
      end;
    end; // with TRegistry.Create do begin

    // clear 'settings need to be saved' flag
    FUserData.SettingsChanged := False;

    { We use a custom message to initialize things once the form }
    { is visible                                                 }
    PostMessage(Handle, WM_APPSTARTUP, 0, 0);
  end; // if not FInitialized then begin
end;

//==============================================================================
// This message handler is triggered by the FormShow event. We comes here
// only when the form is visible on screen.

procedure TServerForm.WMAppStartup(var Msg: TMessage);
var
  Data: TWSAData;
  SrvCryptVersion: Integer;
  ErrMsg: string;
begin
  Caption := MainTitle + ' ' + GetSrvVersionString(cServerId, True);
  UpTimeLabel.Caption := '';
  Update;                { It's nice to have the form completely displayed }

  { Display all pertinent info versions }
  {$IFDEF DEBUG}
  AddMessage(JVCSRES_3333_Debug_build, False);
  {$ENDIF DEBUG}
  {$IFDEF BETAVER}
  AddMessage('', False);
  AddMessage(JVCSRES_3333_Remember_that_this_is_beta_stuff46, False);
  AddMessage(JVCSRES_3333_Not_guaranteed_to_do_anything_useful_at_all33, False);
  AddMessage('', False);
  Caption := Caption + ' ' + JVCSRES_Beta;
  {$ENDIF BETAVER}
  AddMessage(Format(JVCSRES_JEDI_VCS_37s_version_37s,
    [c_DlgCaption, GetSrvVersionString(cServerId, True)]), True);
  SrvCryptVersion := fvcsCryptGetVersion;
  AddMessage(Format(JVCSRES_JEDI_VCS_Encryption_Algorithm_version_37d46372462d,
                    [SrvCryptVersion div 100, SrvCryptVersion mod 100]), False);
  if SrvCryptVersion <> ExpectedSrvCryptVersion then
  begin
    Windows.Beep(500, 100);
    MessageBox(WindowHandle, PChar(Format(
      JVCSRES_ServerCrypt46dll_4037d46372462d41_is_not_from_the_expected_version_4037d46372462d4146
      + #10#13 + JVCSRES_Clients_may_not_be_able_to_connect_the_server33,
      [SrvCryptVersion div 100, SrvCryptVersion mod 100,
       ExpectedSrvCryptVersion div 100, ExpectedSrvCryptVersion mod 100])),
                                       PChar(c_DlgCaption), MB_OK or MB_ICONSTOP);
    AddMessage(JVCSRES_3333_Fatal_Error58_Invalid_ServerCrypt46dll_version46, True);
  end;
  AddMessage(Format(JVCSRES_JEDI_VCS_Client_version_required_6261_37d46372462d,
              [ExpectedCliVersion div 100, ExpectedCliVersion mod 100]), False);
  AddMessage(Format(JVCSRES_MidWare_TAppServer_version_37d46372462d,
                    [ApServerVersion div 100, ApServerVersion mod 100]), False);
  AddMessage(Format(JVCSRES_MidWare_TRequestBroker_version_37d46372462d,
                      [RBrokerVersion div 100, RBrokerVersion mod 100]), False);
  AddMessage(Format(JVCSRES_MidWare_TServerObject_version_37d46372462d,
            [ServerObjectVersion div 100, ServerObjectVersion mod 100]), False);
  AddMessage(Format(JVCSRES_MidWare_TMWBuffer_version_37d46372462d,
                    [MWBufferVersion div 100, MWBufferVersion mod 100]), False);
  AddMessage(Format(JVCSRES_ICS_TWSocket_version_37d46372462d,
                      [WSocketVersion div 100, WSocketVersion mod 100]), False);
  Data := WinsockInfo;
  AddMessage(Format(JVCSRES_Windows_TCP47IP_40Winsock41_version_37d4637d,
    [LOBYTE(Data.wVersion), HIBYTE(Data.wVersion)]), False);
  AddMessage(StrPas(Data.szDescription), False);
  AddMessage(StrPas(Data.szSystemStatus), False);
  AddMessage(JVCSRES_Settings58, False);
  AddMessage(Format(JVCSRES_45__Application_server_port58_37s, [FPort]), False);
  AddMessage(Format(JVCSRES_45__Application_server_location58_37s, [FUserData.ServerIPNo]),
                                                                         False);
  if FUserData.DisableSQLDirekt then
    AddMessage('-  ' + JVCSRES_Direct_SQL_object58_disabled, False)
  else
    AddMessage('-  ' + JVCSRES_Direct_SQL_object58_enabled, False);
  AddMessage(Format(JVCSRES_45__Client_time_out58_37d_sec, [FUserData.ConnectTimeOut]), False);
  if DisableServerBanner1.Checked then
    AddMessage('-  ' + JVCSRES_Server_banner58_disabled, False)
  else
    AddMessage('-  ' + Format(JVCSRES_Server_banner58_3437s34, [FBanner]), False);
  if VerifyClientTimestamp1.Checked then
    AddMessage('-  ' + JVCSRES_Client_timestamp_required58_434745_2h_Servertime, False)
  else
    AddMessage('-  ' + JVCSRES_Client_timestamp_required58_not_active, False);

  // Auto backup
  {$IFDEF DBBACKUPSUPPORT}
  FLastBackup := Time;
  if FAutoBackupActive then
    AutoBkpLabel.Caption :=
                          FormatDateTime('"' + JVCSRES_active + ', ("hh:mm")"', FAutoBackupTime)
  else
    AutoBkpLabel.Caption := JVCSRES_not_active;
  {$ELSE}
  AutoBkpLabel.Caption := JVCSRES_not_supported;
  {$ENDIF DBBACKUPSUPPORT}
  // Uptime Timer, intervall 1sec / 30sec
  FStartTime := Now;
  {$IFDEF UPTIME1S}
  UpTimer.Interval := 1000;
  {$ELSE}
  UpTimer.Interval := 60000;
  {$ENDIF UPTIME1S}
  UpTimer.Enabled := True;
  // TA Timer, intervall 1min
  TATimer.Interval := 60000;
  TATimer.Enabled := True;

  { Initialize UserData }
  FUserData.ServerLabel       := MainTitle + ' V ' + GetSrvVersionString(cServerId, True);
  FUserData.ArchiveTimeStamp  := LocalDT2GMTDT(Now);
  FUserData.CheckCliTimeStamp := VerifyClientTimestamp1.Checked;

  // connect to database
  if not FDBMSConnected then
  begin
    Assert((FServerQuery <> nil), 'FServerQuery not initialized.');
    Assert((FServerDatabase <> nil), 'FServerDatabase not initialized.');
    AddMessage(JVCSRES_DBMS_Login464646, False);
    try
      {$IFDEF UIBSRV} //-- PrY
      AddMessage(JVCSRES_Firebird_engine_40UIB41, False);
      if not FUserData.DBAutoLogon then
        with TFormLogon.Create(Self) do
          try
            cbUseFirebirdEmbeded.Checked := FDBUseEmbeded;
            EditUser.Text := FDBUser;
            EditPassword.Text := FDBPassword;
            EditServer.Text := FDBServer;
            EditDatabase.Text := FDBDatabase;
            CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
            if ShowModal = mrOk then
            begin
              FDBUseEmbeded := cbUseFirebirdEmbeded.Checked;
              FDBDatabase := EditDatabase.Text;
              FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
              // set 'settings need to be saved' flag
              FUserData.SettingsChanged := True;
              if FDBUseEmbeded then
              begin
                FDBUser := 'SYSDBA';
                FDBPassword := 'MANAGER'; //todo uib - war leer
                FDBServer := 'LOCALHOST'; //todo uib - war leer
              end
              else
              begin
                FDBUser := EditUser.Text;
                FDBPassword := EditPassword.Text;
                FDBServer := EditServer.Text;
              end;
            end;
          finally
            Free;
          end;
      FUserData.DBPath := FServerDatabase;
      FServerDatabase.UserName := FDBUser;
      FServerDatabase.PassWord := FDBPassword;
      if (UpperCase(FDBServer) = 'LOCAL') or
         (UpperCase(FDBServer) = 'LOCALHOST') or (Trim(FDBServer) = '') then
        FServerDatabase.DatabaseName := FDBDatabase
      else
        FServerDatabase.DatabaseName := FDBServer + ':' + FDBDatabase;
      // force login dialog if working on a SQL DBMS
      FServerDatabase.Connected := True;
      FServerQuery.DatabaseName := FUserData.DBPath;
      {$ENDIF UIBSRV} //-- PrY
      {$IFDEF ORACLESRV}
      //--hdo
      // Oracle engine verification not finished yet
      AddMessage(JVCSRES_Oracle_engine_version_846x, False);
      if not FUserData.DBAutoLogon then
        with TFormLogon.Create(Application) do
        try
          EditUser.Text := FDBUser;
          EditPassword.Text := FDBPassword;
          EditServer.Text := FDBServer;
          EditDatabase.Text := ''; //-- MG
          CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
          if ShowModal = mrCancel then Exit;
          FDBUser := EditUser.Text;
          FDBPassword := EditPassword.Text;
          FDBServer := EditServer.Text;
          FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
          // set 'settings need to be saved' flag
          FUserData.SettingsChanged := True;
        finally
          Free;
        end;
      FUserData.DBPath := 'LocalSQLDB';
      FServerDatabase.UserName := FDBUser;
      FServerDatabase.Password := FDBPassword;
      FServerDatabase.ServerName := FDBServer;
      // force login dialog if working on a SQL DBMS
      FServerDatabase.Open;
      FServerQuery.DatabaseName := 'LocalSQLDB';
      AddMessage(FServerDatabase.ServerVersion, False);
      AddMessage(Format(JVCSRES_Oracle_client_version_37d, [FServerDatabase.ClientVersionNo]), False);
      {$ENDIF ORACLESRV}
      {$IFDEF MSSQLSRV} //--MG
      // MSSQL engine verification not finished yet
      AddMessage(JVCSRES_MSSQL_engine_version, False);
      if not FUserData.DBAutoLogon then
        with TFormLogon.Create(Application) do
        try
          cbUseTrustedNTConnection.Checked := FDBUseNTTrustedConnection;
          EditUser.Text := FDBUser;
          EditPassword.Text := FDBPassword;
          EditServer.Text := FDBServer;
          EditDatabase.Text := FUserData.DBDatabase;
          CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
          if ShowModal = mrCancel then Exit;
          FDBUser := EditUser.Text;
          FDBPassword := EditPassword.Text;
          FDBServer := EditServer.Text;
          FUserData.DBDatabase := EditDatabase.Text;
          FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
          FDBUseNTTrustedConnection := cbUseTrustedNTConnection.Checked;
          // set 'settings need to be saved' flag
          FUserData.SettingsChanged := True;
        finally
          Free;
        end;
      FUserData.DBPath := FServerDatabase;
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
      // force login dialog if working on a SQL DBMS
      FServerDatabase.Open;
      FServerQuery.DatabaseName := FUserData.DBPath;

      with TSrvQuery.Create(nil) do
      begin
        DatabaseName := FUserData.DBPath;
        with SQL do
        begin
          Clear;
          Add('select @@version');
        end;
        Open;
        AddMessage(Fields[0].AsString, False);
        Close;
        Free;
      end;
      {$ENDIF MSSQLSRV} //--MG
      {$IFDEF MYSQLSRV} //--LB
      if not FUserData.DBAutoLogon then
        with TFormLogon.Create(Application) do
        try
          EditUser.Text := FDBUser;
          EditPassword.Text := FDBPassword;
          EditServer.Text := FDBServer;
          EditDatabase.Text := FUserData.DBDatabase;
          CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
          if ShowModal = mrCancel then Exit;
          FDBUser := EditUser.Text;
          FDBPassword := EditPassword.Text;
          FDBServer := EditServer.Text;
          FUserData.DBDatabase := EditDatabase.Text;
          FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
          // set 'settings need to be saved' flag
          FUserData.SettingsChanged := True;
        finally
          Free;
        end;
      FUserData.DBPath := FServerDatabase;
      FServerDatabase.HostName := FDBServer;
      FServerDatabase.Database := FUserData.DBDatabase;
      FServerDatabase.User := FDBUser;
      FServerDatabase.Password := FDBPassword;
      FServerDatabase.Connected := True;
      FServerQuery.DatabaseName := FUserData.DBPath;
      with TSrvQuery.Create(nil) do
      begin
        DatabaseName := FUserData.DBPath;
        SQL.Text := 'show variables like "version"';
        Open;
        AddMessage(Format(JVCSRES_MySQL_engine_version_37s, [Fields[1].AsString]), False);
        Close;
        Free;
      end;
      {$ENDIF MYSQLSRV} //--LB
      {$IFDEF ADOSRV} //--PL
      // ADO connection verification not finished yet
      AddMessage(JVCSRES_ADO_engine_version, False);
      if not FUserData.DBAutoLogon then
        with TFormLogon.Create(Application) do
        try
          EditUser.Text := FDBUser;
          EditPassword.Text := FDBPassword;
          EditServer.Text := FDBServer;
          EditDatabase.Text := FUserData.DBDatabase;
          CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
          if ShowModal = mrCancel then Exit;
          FDBUser := EditUser.Text;
          FDBPassword := EditPassword.Text;
          FDBServer := EditServer.Text;
          FUserData.DBDatabase := EditDatabase.Text;
          FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
          // set 'settings need to be saved' flag
          FUserData.SettingsChanged := True;
        finally
          Free;
        end;
      FUserData.DBPath := FServerDatabase;
      FServerDatabase.Params.Clear;
      FServerDatabase.Params.Add('User ID=' + FDBUser);
      FServerDatabase.Params.Add('Password=' + FDBPassword);
      FServerDatabase.Params.Add('Provider=' + FDBServer);
      FServerDatabase.Params.Add('Data Source=' + FUserData.DBDatabase);
      {$ENDIF ADOSRV} //--PL
    except
      on E:Exception do
      begin
        Windows.Beep(500, 100);
        MessageBox(WindowHandle, PChar(Format(JVCSRES_Exception58_37s, [E.Message]) + #10#13 +
        JVCSRES_Unable_to_connect_DBMS46), PChar(c_DlgCaption), MB_OK or MB_ICONWARNING);
        AddMessage(JVCSRES_3333_DBMS_Login_failed, False);
        FDBMSConnected := False;
        SetupUI;
        Exit;
      end;
    end; // try except
    AddMessage(Format(JVCSRES_37s_45_DBMS_connected, [DateTimeToStr(Now)]), False);
    AddMessage(Format(JVCSRES_45__Version_archive_server58_37s, [FDBServer]), True);
    // check archive version and upgrade if necessary.
    CheckArchiveVersion;
    FDBMSConnected := True;
  end; // if not FDBMSConnected then begin

  SetupUI;
  { Start the application server component }
  AppServer1.ClientTimeOut := FUserData.ConnectTimeOut;
  AppServer1.Port := FPort;
  if not StartApplicationServer(ErrMsg) then
  begin
    Windows.Beep(500, 100);
    MessageBox(WindowHandle, PChar(ErrMsg), PChar(c_DlgCaption), MB_OK or
      MB_ICONSTOP);
    AddMessage(DateTimeToStr(Now) + '!!' + ErrMsg, True);
    Exit;
  end;
  Start1.Enabled := False;
  Stop1.Enabled := not Start1.Enabled;
  {$IFDEF BETAVER}
  Caption := Format(JVCSRES_37s_37s_Beta_45_Running, [MainTitle, GetSrvVersionString(cServerId, True)]);
  {$ELSE}
  Caption := Format(JVCSRES_37s_37s_45_Running, [MainTitle, GetSrvVersionString(cServerId, True)]);
  {$ENDIF BETAVER}
  AddMessage(Format(JVCSRES_37s_45_Server_started46, [DateTimeToStr(Now)]), True);

  // iconize?
  if FHideWindow or StartHidden1.Checked then
  begin
    Application.ProcessMessages;
    ServerForm.Hide;
  end;
end;

//------------------------------------------------------------------------------

procedure TServerForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := False;
  // "Hide Window on Close"?
  if HideOnClose1.Checked and not FInExit then
  begin
    ServerForm.Hide;
    FHideWindow := True;
  end
  else
  begin
    // If WM_ENDSESSION or WM_QUERYENDSESSION then terminate because windows is shut down
    if FInEndSession then
      CanClose := True
    else
    begin
      // Do not terminate while clients are connected
      if AppServer1.ClientCount > 0 then
      begin
        Windows.Beep(500, 100);
        if MessageBox(WindowHandle, PChar(Format(JVCSRES_37d_clients_already_connected33,
          [AppServer1.ClientCount]) + #10#13 +
             JVCSRES_The_requested_action_will_disconnect_all_clients46 + #10#13 +
             JVCSRES_Terminate_server63), PChar(c_DlgCaption),
             MB_YESNO or MB_DEFBUTTON2 or MB_ICONWARNING) <> id_Yes then Exit;
      end // if AppServer1.ClientCount > 0 then begin
      else
      begin
        Windows.Beep(500, 100);
        if MessageBox(WindowHandle,
             PChar(JVCSRES_Terminate_server63),
             PChar(c_DlgCaption),
             MB_YESNO or MB_DEFBUTTON2 or MB_ICONWARNING) <> id_Yes then Exit;
      end;
    end;
    {$IFNDEF DEBUG}
    // invalidate all login acounts?
    {  we use a try..except statement here to be sure that the sever can be
       terminated if the DB server hangs or is absent  }
    try
      if FDBMSConnected then with FServerQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT * FROM transact');
        Open;
        if not Eof then
        begin
          Close;
          case MessageBox(WindowHandle, PChar(JVCSRES_Invalidate_all_login_acounts63),
               PChar(c_DlgCaption), MB_YESNOCANCEL or MB_ICONQUESTION) of
            id_Yes: begin
                       SQL.Clear;
                       SQL.Add('DELETE FROM transact');
                       ExecSQL;
                     end;
            id_No: ;
            else Exit;
          end; // case MessageBox(....
        end // if not EoF then
        else Close;
      end; // if FDBMSConnected then begin
      AppServer1.Stop;
      FAppServerStarted := False;
      AppServer1.DisconnectAll;
    except
      on E:Exception do
      begin
        MessageBox(WindowHandle, PChar(JVCSRES_DBMS_error58 + #10#13 + E.Message),
                                         PChar(c_DlgCaption), MB_OK or MB_ICONSTOP);
      end;
    end;
    {$ELSE}
    AppServer1.Stop;
    FAppServerStarted := False;
    AppServer1.DisconnectAll;
    {$ENDIF ~DEBUG}
    // complete server log
    if FUserData.WriteServerLog then
    begin
      FLog.Add(Format(JVCSRES_37s_45_Server_closed46, [DateTimeToStr(Now)]));
      if RequestCountLabel.Caption = '' then
        RequestCountLabel.Caption := '0';
      FLog.Add(Format(JVCSRES_Uptime58_37s, [UpTimeLabel.Caption]));
      FLog.Add(Format(JVCSRES_Requests58_37s, [RequestCountLabel.Caption]));
      FLog.Add(Format(JVCSRES_Received_bytes58_37n_MB,
                                          [(FUserData.ReceivedBytes / 1048576)]));
      FLog.Add(Format(JVCSRES_Transmitted_bytes58_37n_MB,
                                       [(FUserData.TransmittedBytes / 1048576)]));
      FLog.Add(Format(JVCSRES_Users58_37d, [FUserData.UserCount]));
      FLog.Add(Format(JVCSRES_New_files58_37d, [FUserData.NewFilesCount]));
      FLog.Add(Format(JVCSRES_Get_modules58_37d, [FUserData.GetCount]));
      FLog.Add(Format(JVCSRES_Checked_In_modules58_37d, [FUserData.CheckinCount]));
      FLog.Add(Format(JVCSRES_Checked_Out_modules58_37d, [FUserData.CheckoutCount]));
      FLog.Add('***************************************************************');
      SaveLogfile;
    end;
    CanClose := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TServerForm.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FIniFileName);
  try
    IniFile.WriteInteger(SectionWindow, KeyTop, Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft, Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth, Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.WriteBool(SectionWindow, HideWindow, FHideWindow);
    IniFile.WriteString(SectionData, KeyPort, FPort);
    IniFile.WriteBool(SectionData, AutoBackupActive, FAutoBackupActive);
    IniFile.WriteFloat(SectionData, AutoBackupTime, FAutoBackupTime);
    IniFile.WriteFloat(SectionData, LastBackup, FLastBackup);
    // FUserData
    IniFile.WriteBool(SectionData, WriteSrvLog, FUserData.WriteServerLog);
    IniFile.WriteBool(SectionData, CheckIPAddr, FUserData.CheckIPAddr);
    IniFile.WriteInteger(SectionData, ConnectTimeOut, FUserData.ConnectTimeOut);
    IniFile.WriteBool(SectionData, LoginExpires, FUserData.LoginExpires);
    IniFile.WriteFloat(SectionData, LoginTimeOut, FUserData.LoginTimeOut);
    IniFile.WriteBool(SectionData, CasuallyCheckin, FUserData.CasuallyCheckin);
    IniFile.WriteBool(SectionData, WriteVCSLog, FUserData.WriteVCSLog);
    IniFile.WriteString(SectionData, BackupPath, FUserData.BackupPath);
    IniFile.WriteFloat(SectionData, LastLiveBackup, FUserData.LastLiveBackup);
    IniFile.WriteBool(SectionData, LocalLogin, FUserData.LocalLogin);
    IniFile.WriteFloat(SectionData, ArchiveTStamp, FUserData.ArchiveTimeStamp);
    IniFile.WriteBool(SectionData, LogAccessFault, FUserData.LogAccessFault);
    //-- hdo
    IniFile.WriteBool(SectionData, DBAutoLogon, FUserData.DBAutoLogon);
    IniFile.WriteString(SectionData, DBUser, FDBUser);
    IniFile.WriteString(SectionData, DBServer, FDBServer);
    if FUserData.DBAutoLogon then
      IniFile.WriteString(SectionData, DBPassword, fvcsSrvEnCrypt(FDBPassword))
    else
      IniFile.WriteString(SectionData, DBPassword, '');
    {$IFDEF MSSQLSRV}
    IniFile.WriteString(SectionData, DBDatabase, FUserData.DBDatabase);
    IniFile.WriteBool(SectionData, FmssqlUseNTTrustedConnection, FDBUseNTTrustedConnection);
    {$ENDIF MSSQLSRV}
    {$IFDEF ADOSRV}
    IniFile.WriteString(SectionData, DBDatabase, FUserData.DBDatabase);
    {$ENDIF ADOSRV}
    {$IFDEF MYSQLSRV}
    IniFile.WriteString(SectionData, DBDatabase, FUserData.DBDatabase);
    {$ENDIF MYSQLSRV}
    {$IFDEF UIBSRV} //-- PrY
    IniFile.WriteString(SectionData, DBDatabase, FDBDatabase);
    IniFile.WriteBool(SectionData, FibUseEmbeded, FDBUseEmbeded);
    IniFile.WriteString(SectionData, FibCharSet, FDBCharSet);
    {$ENDIF UIBSRV}

    {$IFDEF DEBUG}
    IniFile.WriteBool(SectionData, ShowDebugMsg, ShowDebugMessages1.Checked);
    {$ENDIF DEBUG}
    IniFile.WriteBool(SectionData, DisableServerBanner,
                                                  DisableServerBanner1.Checked);
    IniFile.WriteBool(SectionData, StartHidden, StartHidden1.Checked);
    IniFile.WriteBool(SectionData, VerifyClientTimestamp,
                                                VerifyClientTimestamp1.Checked);
    IniFile.WriteBool(SectionData, DisableSQLDirekt, FUserData.DisableSQLDirekt);
    IniFile.WriteBool(GlobalUserData, ForceGlobalSettings,
                                                  ForceGlobalSettings1.Checked);
    IniFile.WriteBool(SectionData, HideOnClose, HideOnClose1.Checked);
    IniFile.WriteString(SectionData, ServerBanner, FBanner);
  finally
    IniFile.Free;
  end;

  { Update the application server component }
  AppServer1.ClientTimeOut := FUserData.ConnectTimeOut;

  // clear 'settings need to be saved' flag
  FUserData.SettingsChanged := False;
  AddMessage(Format(JVCSRES_37s_45_Configuration_file_37s_stored46,
    [DateTimeToStr(Now), FIniFileName]), False);
  // update menu
  LogClientAccessFaults1.Checked := FUserData.LogAccessFault;
end;

//------------------------------------------------------------------------------

procedure TServerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveSettings;
  HideIcon;
end;

//=== Clean up =================================================================

procedure TServerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FLog) then FLog.Free;
  if Assigned(FServerQuery) then FServerQuery.Free;
  if Assigned(FServerDatabase) then FServerDatabase.Free;
  if Assigned(FSrvIcon) then FSrvIcon.Free;
  if Assigned(FUserData) then
  begin
    Dispose(FUserData);
    FUserData := nil;
  end;
end;

//=== (Try to) start the application server ====================================

function TServerForm.StartApplicationServer(var ErrMsg: string): Boolean;
begin
  Result := False;
  ErrMsg := '';
  if not FAppServerStarted then
  begin
    try
      AppServer1.Start;
    except
      on E:ESocketException do
      begin
        if (AppServer1.SrvWSocket.LastError = WSAEADDRINUSE) then
          ErrMsg := Format(JVCSRES_Port_37s_is_already_used_by_another_process46, [FPort])
        else
          ErrMsg := Format(JVCSRES_Winsock_error_3437s34_occurred_starting_server46,
                                                                   [E.Message]);
        Exit;
      end; // on E:ESocketException do begin
      on E:Exception do
      begin
        ErrMsg := Format(JVCSRES_Error_37s_occurred_starting_server46, [E.Message]);
        Exit;
      end; // on E:Exception do begin
    end; // try except
    FAppServerStarted := True;
  end; // if not FAppServerStarted then
  AddMessage(Format(JVCSRES_37s_45_AppServer_Component58_started, [DateTimeToStr(Now)]), True);
  Result := True;
end;

//=== Set up user interface ====================================================

procedure TServerForm.SetupUI;
begin
  AllClients1.Enabled := FDBMSConnected;
  LoggedIn1.Enabled := FDBMSConnected;
  Disconnectall1.Enabled := FDBMSConnected;
  LogoutAll1.Enabled := FDBMSConnected;
  Execute1.Enabled := FDBMSConnected;
end;

//==============================================================================
// Check structure version of the current version archive and upgrade to newer
// Archivversion if necessary!

function TServerForm.CheckArchiveVersion: Boolean;
var
  nActArchiveVersion: Integer;
  Msg: string;
begin
  Result := True;
  nActArchiveVersion := FServerDatabase.ArchiveVersion;
  if nActArchiveVersion < ExpectedArchiveVersion then
  begin
    Result := False;
    Msg := Format ( JVCSRES_This_server_expects_archive_version_37d46372462d46
                  , [ExpectedArchiveVersion div 100, ExpectedArchiveVersion mod 100]
                  ) + #10#13 +
           Format ( JVCSRES_You_have_connected_to_an_archive_of_version_37d46372462d
                  , [nActArchiveVersion div 100, nActArchiveVersion mod 100]
                  ) + #13 +
           JVCSRES_You_may_now_upgrade_your_archive_to_the_most_recent_archive_or_close_JVCS_Appserver_and_upgrade_later46 + #13 +
           JVCSRES_Attention58_You_need_to_be_logged_on_to_the_database_as_database_administrator46 + #13 +
           JVCSRES_If_you_are_logged_on_without_permission_to_change_database_structure44_upgrade_will_fail33 + #13#13 +
           JVCSRES_Continue_with_archive_upgrade_now63;
    Windows.Beep(500, 100);
    if MessageBox ( WindowHandle
                  , PChar(Msg)
                  , PChar(c_DlgCaption)
                  , MB_OKCANCEL or MB_ICONINFORMATION) = mrOk then
    begin
      Result := FServerDatabase.UpgradeArchive;
      if Result then
      begin
        MessageBox( WindowHandle
                  , PChar(JVCSRES_Database_archive_successfully_upgraded46 + #13 +
                    JVCSRES_It_is_recommended_to_make_now_a_database_backup46)
                  , PChar(c_DlgCaption)
                  , MB_OK or MB_ICONINFORMATION);
      end
      else
      begin
        MessageBox( WindowHandle
                  , PChar(JVCSRES_There_was_an_error_on_upgrading_your_archive46 + #13 +
                    JVCSRES_Please_read_the_release_notes_about_database_upgrading_procedure + #13#13 +
                    JVCSRES_It_is_strongly_recommended_not_to_continue_without_having_archive_upgrading_successfully_completed33)
                  , PChar(c_DlgCaption)
                  , MB_OK or MB_ICONSTOP);
        AddMessage(JVCSRES_Error_on_upgrading_archive_3333, True);
      end;
    end;
  end;

  { 
  // This server supports version archives up to ArchMaxVersion
  if (VersionInfo >= ArchMaxVersion) then
  begin
    Msg := Format('This server supports version archives up to %d.%1.1d.',
           [ArchMaxVersion div 10, ArchMaxVersion mod 10]) + #10#13 +
           'The version archive you have selected is from version ' +
           Format('%d.%1.1d' , [VersionInfo div 10, VersionInfo mod 10]);
    Windows.Beep(500, 100);
    MessageBox(WindowHandle, PChar(Msg), PChar(c_DlgCaption),
                                                       mb_OK or mb_IconWarning);
  end;
   }
  nActArchiveVersion := FServerDatabase.ArchiveVersion;
  AddMessage(Format(JVCSRES_45__Archive_structure_version58_37d46372462d,
    [nActArchiveVersion div 100, nActArchiveVersion mod 100]), True);
end;

//==============================================================================
// A new client has connected, update our user interface
// // --vr
// (evidently here lies the reason for some strange "hang" problems in W2K.
// It seems as if there is timing problem in the underlying code...)

procedure TServerForm.AppServer1ClientConnected(
    Sender: TObject;
    CliWSocket: TClientWSocket);
var
  SysVersionInfo: _OSVersionInfoA;
begin
  ClientCountLabel.Caption := IntToStr(AppServer1.ClientCount);
  SysVersionInfo.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
  GetVersionEx(SysVersionInfo);
  // if OS is Windows 2000 then "sleep" for 100 ms
  if SysVersionInfo.dwMajorVersion = 5 then
    Sleep(100);
end;

//=== A client has disconnected, update our user interface =====================

procedure TServerForm.AppServer1ClientClosed(
    Sender: TObject;
    CliWSocket: TClientWSocket);
begin
  ClientCountLabel.Caption := IntToStr(AppServer1.ClientCount - 1);
end;

//==============================================================================
// This event handler is called when the AppServer component has some info
// to display. This info can comes from one of the server components also.

procedure TServerForm.AppServer1Display(Sender: TObject; Msg: string);
begin
  if Msg <> '' then
    AddMessage(DateTimeToStr(Now) + ' - ' + Msg, True)
  else
    AddMessage('', False);
end;

//==============================================================================
// This function is called back by the request broker for each function code
// he knows about when we ask to enumerate functions.

function TServerForm.EnumServerFunctions(
    Sender: TObject; FunctionCode: string): Boolean;
begin
  AddMessage(FunctionCode, False);
  Result := True;   { Continue to enumerate }
end;

//------------------------------------------------------------------------------
procedure TServerForm.AppServer1BeforeSendReply(
    Sender: TObject;
    CliWSocket: TClientWSocket);
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

procedure TServerForm.AppServer1AfterSendReply(Sender: TObject;
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

procedure TServerForm.AppServer1BeforeProcessRequest(Sender: TObject;
  CliWSocket: TClientWSocket; var CmdBuf: Pointer; var CmdLen: Integer);
var
  Dst: PChar;
  DstLen: Longint;
  {$IFDEF ORACLESRV}
  I: Integer; //-- hdo
  {$ENDIF ORACLESRV}
begin
  // save statistic data
  FUserData.ReceivedBytes := FUserData.ReceivedBytes + CmdLen;

  { Check if we have some encrypted data }
  if (CmdLen < 3) or (PChar(CmdBuf)[0] <> #$FF) then
    CliWSocket.UserData := 0
  else
  begin
    { We've got encrypted data. Decrypt. First byte is a flag, ignore. }
    Decrypt(PChar(CmdBuf) + 1, CmdLen - 1, Dst, DstLen);
    CmdBuf := Dst;
    CmdLen := DstLen;
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
        AddMessage(DateTimeToStr(Now) + ' - !!' + E.Message, True);
        AddMessage(Format(JVCSRES_37s_45_3333_Errors_reported58, [DateTimeToStr(Now)]), True);
        for I := 0 To E.ErrorCount - 1 do
          AddMessage(Format(JVCSRES_37d46_NativeError58, [I + 1]) +
        IntToStr(Errors[I].ErrorCode), True);
        //-- not connected -> try to reconnect!
        with FServerDatabase do
        begin
          AddMessage(JVCSRES_Re45Connecting_with_DB464646, False);
          Close; // Zuerst die Verbindung schliessen!
          Open;
          AddMessage(JVCSRES_Connected46, False);
        end;

      end; // of with E as EDBEngineError
    end; // of try open
  end; // of with FQuery
  {$ELSE}
  //-- lb
  //-- make sure we're connected to DB server
  FServerDatabase.AcquireConnection;
  {$ENDIF MYSQLSRV}
end;

//------------------------------------------------------------------------------

procedure TServerForm.AppServer1AfterProcessRequest(Sender: TObject;
  CliWSocket: TClientWSocket; var CmdBuf: Pointer; var CmdLen: Integer);
begin
  if CliWSocket.UserData <> 0 then FreeMem(CmdBuf, CmdLen);
end;

//=== Start the server =========================================================

procedure TServerForm.Start1Click(Sender: TObject);
var
  ErrMsg: string;
begin
  if not FDBMSConnected then
  begin
    Windows.Beep(500, 100);
    MessageBox(WindowHandle, PChar(JVCSRES_Version_archive_not_connected46 + #10#13 +
                                    JVCSRES_Unable_to_start_the_server_component46),
                                    PChar(c_DlgCaption), MB_OK or MB_ICONWARNING);
    Exit;
  end;
  if not StartApplicationServer(ErrMsg) then
  begin
    Windows.Beep(500, 100);
    MessageBox(WindowHandle, PChar(ErrMsg), PChar(c_DlgCaption), MB_OK or
      MB_ICONSTOP);
    AddMessage(DateTimeToStr(Now) + '!!' + ErrMsg, True);
    Exit;
  end;
  Start1.Enabled := False;
  Stop1.Enabled := not Start1.Enabled;
  {$IFDEF BETAVER}
  Caption := Format(JVCSRES_37s_37s_Beta_45_Running, [MainTitle, GetSrvVersionString(cServerId, True)]);
  {$ELSE}
  Caption := Format(JVCSRES_37s_37s_45_Running, [MainTitle, GetSrvVersionString(cServerId, True)]);
  {$ENDIF BETAVER}
  AddMessage(Format(JVCSRES_37s_45_Server_started46, [DateTimeToStr(Now)]), True);
end;

//=== Stop the server ==========================================================

procedure TServerForm.Stop1Click(Sender: TObject);
begin
  AppServer1.Stop;
  FAppServerStarted := False;
  AddMessage(Format(JVCSRES_37s_45_AppServer_Component58_stopped, [DateTimeToStr(Now)]), True);
  Start1.Enabled := True;
  Stop1.Enabled := not Start1.Enabled;
  {$IFDEF BETAVER}
  Caption := Format(JVCSRES_37s_37s_Beta, [MainTitle, GetSrvVersionString(cServerId, True)]);
  {$ELSE}
  Caption := MainTitle + ' ' + GetSrvVersionString(cServerId, True);
  {$ENDIF BETAVER}
  AddMessage(Format(JVCSRES_37s_45_Server_stopped46, [DateTimeToStr(Now)]), True);
end;

//=== Show a list of all clients ===============================================

procedure TServerForm.AllClients1Click(Sender: TObject);
var
  ClientInfo: string;
begin
  if FDBMSConnected then
  begin
    AddMessage('', False);
    AddMessage(Format(JVCSRES_Client_list_45_All_clients58_4037s41, [DateTimeToStr(Now)]), False);
    with FServerQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT users.userid, users.login,');
      SQL.Add('users.rights, users.deleted,');
      SQL.Add('users.description, users.ipaddr');
      SQL.Add('FROM users');
      SQL.Add('ORDER BY users.login');
      Open;
      if Eof then
        AddMessage(JVCSRES_No_clients_defined46, False);
      while not Eof do
      begin
        ClientInfo := IntToStr(FServerQuery.FieldByName('userid').AsInteger); 
        while Length(ClientInfo) < 3 do
          ClientInfo := '0' + ClientInfo;
        ClientInfo := Format(JVCSRES_37s_45_37s_45_Level58_37d_45_IP58_37s_45_37s,
          [ClientInfo, FieldByName('login').AsString, FServerQuery.FieldByName('rights').AsInteger,
           FieldByName('ipaddr').AsString, FieldByName('description').AsString]);
        AddMessage(ClientInfo, False);
        Next;
      end;
      Close;
    end; // with FServerQuery do begin
    AddMessage('', False);
  end; // if FDBMSConnected then begin
end;

//=== Show a list of all clients currently logged in ===========================

procedure TServerForm.LoggedIn1Click(Sender: TObject);
var
  ClientInfo: string;
begin
  if FDBMSConnected then
  begin
    AddMessage('', False);
    AddMessage(Format(JVCSRES_Client_list_45_logged_in58_4037s41, [DateTimeToStr(Now)]), False);
    with FServerQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT users.userid, users.login,');
      SQL.Add('users.rights, transact.accessid, users.deleted,');
      SQL.Add('users.description, users.ipaddr');
      SQL.Add('FROM users, transact');
      SQL.Add('WHERE users.userid = transact.accessid');
      SQL.Add('ORDER BY users.login');
      Open;
      if Eof then
        AddMessage(JVCSRES_No_clients_logged_in46, False);
      while not Eof do
      begin
        ClientInfo := IntToStr(FServerQuery.FieldByName('userid').AsInteger);
        while Length(ClientInfo) < 3 do
          ClientInfo := '0' + ClientInfo;
        ClientInfo := Format(JVCSRES_37s_45_37s_45_Level58_37d_45_IP58_37s_45_37s,
          [ClientInfo, FieldByName('login').AsString, FServerQuery.FieldByName('rights').AsInteger,
           FieldByName('ipaddr').AsString,  FieldByName('description').AsString]);
        AddMessage(ClientInfo, False);
        Next;
      end;
      Close;
    end; // with FServerQuery do begin
    AddMessage('', False);
  end; // if FDBMSConnected then begin
end;

//=== Log out all clients currently connected to the application server ========

procedure TServerForm.LogoutAll1Click(Sender: TObject);
begin
  if FDBMSConnected then
  begin
    if MessageBox(WindowHandle, PChar(JVCSRES_Are_you_sure_to_log_out_all_clients63),
           PChar(c_DlgCaption), MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_ICONWARNING)
      <> id_Yes then Exit;
    with FServerQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add('DELETE FROM transact');
      ExecSQL;
    end; // with FServerQuery do begin
  end; // if FDBMSConnected then begin
end;

//=== Disconnect all clients currently connected to the application server =====

procedure TServerForm.Disconnectall1Click(Sender: TObject);
begin
  AppServer1.DisconnectAll;
  ClientCountLabel.Caption := IntToStr(AppServer1.ClientCount);
end;

//------------------------------------------------------------------------------

procedure TServerForm.Functionlist1Click(Sender: TObject);
begin
  RequestBroker1.EnumServerFunctions(EnumServerFunctions);
end;

//==============================================================================
// Add a message from the server or from one of the server objects to the servers
// display. We use a TRichEdit instead of a TMemo to prevent memory overun in
// Win95/98, therefore line scrolling must be done by the application.

procedure TServerForm.AddMessage(Value: string; const LogMsg: Boolean);
begin
  try
    // is this a blank line?
    if Value = '' then begin
      DisplayRE.Lines.Add('');
      Exit;
    end;
    // default color = clWindowText
    DisplayRE.SelAttributes.Color := clWindowText;
    // is this an error message?
    if Pos('!!', Value) > 0 then
      DisplayRE.SelAttributes.Color := clRed;
    // is this a login/logout message?
    if (Pos('[LOGIN]', Value) > 0) or
       (Pos('[LOGOUT]', Value) > 0) then
      DisplayRE.SelAttributes.Color := clMaroon;
    {$IFDEF DEBUG}
    // is this a debug message?
    if (Pos('>>', Value) > 0) then
    begin
      if not ShowDebugMessages1.Checked then Exit;
      if (DisplayRE.SelAttributes.Color <> clRed) then
        DisplayRE.SelAttributes.Color := clNavy;
    end; // if (Pos('>>', Value) > 0) then begin
    {$ENDIF DEBUG}
    DisplayRE.Lines.Add(Value);
    DisplayRE.Perform(EM_LINESCROLL, 0, 1);
    if FUserData.WriteServerLog and LogMsg then FLog.Add(Value);
    {  Prevent the RichEdit from consuming too much resources
       - Debug = on -> line count will grow very fast  }
    if DisplayRE.Lines.Count > 1000 then
    begin
      DisplayRE.Clear;
      DisplayRE.Lines.Add(Format(JVCSRES_37s_45_Display_line_count_62_100044_cleared_by_server,
        [DateTimeToStr(Now)]));
    end;
  except
    on E:Exception do
    begin
      FLog.Add(Format(JVCSRES_37s_45_3333_Exception_3437s34_in_ServerForm46AddMessage46_Display_cleared,
        [DateTimeToStr(Now), E.Message]));
      DisplayRE.Clear;
    end;
  end;
end;

//=== this menu item is only availble if {$DEBUG} is enabled (cond_def.inc) ====

procedure TServerForm.ShowDebugMessages1Click(Sender: TObject);
begin
  ShowDebugMessages1.Checked := not ShowDebugMessages1.Checked;
end;

//==============================================================================
// Disable the server's banner response. Usually the server responds to any
// connection (i.e. via Telnet) with a standard banner message like
// "Welcome to FreeVCS xxx server". This gives a potential hacker a lot of
// information about what it is that has "answered the call".
// For safety reasons this can be disabled and the server returns only a blank
// string.

procedure TServerForm.DisableServerBanner1Click(Sender: TObject);
begin
  DisableServerBanner1.Checked := not DisableServerBanner1.Checked;
  if DisableServerBanner1.Checked then
  begin
    AppServer1.Banner := '';
    AddMessage(JVCSRES_Server_banner58_disabled, False);
  end
  else
  begin
    AppServer1.Banner := FBanner;
    AddMessage(Format(JVCSRES_Server_banner58_3437s34, [FBanner]), False);
  end;
end;

//==============================================================================
// Enable/Disable the EXECUTE_SQL server object (provides direct SQL access to
// the database ).
// For safety reasons this can be disabled/enabled only from server side.

procedure TServerForm.DisableSQLObject1Click(Sender: TObject);
begin
  if DisableSQLObject1.Checked then
    if MessageBox(WindowHandle, PChar(
      JVCSRES_Are_you_sure_you_want_to_enable_the_direct_SQL_server_object63 + #10#13 +
      JVCSRES_This_is_a_high45risk_function_and_therefore_usually_not_recommend33),
      PChar(c_DlgCaption), MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_ICONWARNING)
    <> id_Yes then Exit;

  DisableSQLObject1.Checked := not DisableSQLObject1.Checked;
  FUserData.DisableSQLDirekt := DisableSQLObject1.Checked;
  if FUserData.DisableSQLDirekt then
    AddMessage(JVCSRES_Direct_SQL_object58_disabled, False)
  else
    AddMessage(JVCSRES_Direct_SQL_object58_enabled, False);
end;

//==============================================================================
// Compare the clients local time to the servers timestamp. Client timestamps
// outside of Servertime +/- 2h will be rejected.
// For safety reasons this can be disabled/enabled only from server side.

procedure TServerForm.VerifyClientTimestamp1Click(Sender: TObject);
begin
  VerifyClientTimestamp1.Checked := not VerifyClientTimestamp1.Checked;
  FUserData.CheckCliTimeStamp := VerifyClientTimestamp1.Checked;
  if VerifyClientTimestamp1.Checked then
    AddMessage(JVCSRES_Client_timestamp_required58_434745_2h_Servertime, False)
  else
    AddMessage(JVCSRES_Client_timestamp_required58_not_active, False);
end;

//------------------------------------------------------------------------------

procedure TServerForm.ForceGlobalSettings1Click(Sender: TObject);
var
  IniFile: TIniFile;
begin
  ForceGlobalSettings1.Checked := not ForceGlobalSettings1.Checked;
  // Clients Ini
  IniFile := TIniFile.Create(FIniFileName);
  try
    IniFile.WriteBool(GlobalUserData, ForceGlobalSettings,
                                                  ForceGlobalSettings1.Checked);
  finally
    IniFile.Free;
  end;
end;

//=== Copy server display to clipboard =========================================

procedure TServerForm.Copytoclipboard1Click(Sender: TObject);
begin
  DisplayRE.SelStart := 0;
  DisplayRE.SelLength := Length(DisplayRE.Text);
  try
    DisplayRE.CopyToClipBoard;
  except
    on E:Exception do
    begin
      Windows.Beep(500, 100);
      MessageBox(WindowHandle, PChar(E.Message + #10#13 +
                        JVCSRES_Error_while_writing_to_clipboard33), PChar(c_DlgCaption),
                         MB_OK or MB_ICONWARNING);
    end;
  end;
  DisplayRE.SelLength := 0;
end;

//=== Save server display to file ==============================================

procedure TServerForm.Savetofile1Click(Sender: TObject);
begin
  with SaveDialog1 do
  begin
    InitialDir := FUserData.ServerPath;
    FileName := 'SrvDisplay.txt';
    Filter := JVCSRES_Text_files_404246txt411244246txt124All_files_4042464241124424642;
    if not Execute then Exit;
    try
      DisplayRE.Lines.SaveToFile(FileName);
    except
      on E:Exception do
      begin
        Windows.Beep(500, 100);
        MessageBox(WindowHandle, PChar(E.Message + #10#13 +
                             JVCSRES_Error_while_saving_the_file33), PChar(c_DlgCaption),
                             MB_OK or MB_ICONWARNING);
      end;
    end;
  end; // with SaveDialog1 do begin
end;

//=== Clear server display =====================================================

procedure TServerForm.Clear1Click(Sender: TObject);
begin
  DisplayRE.Clear;
end;

//=== Terminate server request =================================================

procedure TServerForm.Exit1Click(Sender: TObject);
begin
  FInExit := True;
  Close;
end;

//=== Set/ change server port/service name =====================================

procedure TServerForm.Port1Click(Sender: TObject);
var
  Value: string;
begin
  // Do not change this value while clients are connected
  if AppServer1.ClientCount > 0 then
  begin
    Windows.Beep(500, 100);
    if MessageBox(WindowHandle, PChar(Format(JVCSRES_37d_clients_already_connected33,
         [AppServer1.ClientCount]) + #10#13 +
         JVCSRES_The_requested_action_will_disconnect_all_clients46 + #10#13 +
         JVCSRES_Continue63), PChar(c_DlgCaption),
         MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_ICONWARNING) <> id_Yes then Exit;
  end;
  Disconnectall1Click(Self);
  Value := FPort;
  if InputQuery(c_DlgCaption, JVCSRES_Enter_TCP_port47service_name58, Value) then
  begin
    if Value = '' then Exit;
    FPort := Value;
    AddMessage(Format(JVCSRES_37s_45_Server_port_changed_to_37s,
      [DateTimeToStr(Now), FPort]), True);
    PortLabel.Caption := FPort;
    AppServer1.Port := FPort;
    // set 'settings need to be saved' flag
    FUserData.SettingsChanged := True;
  end;
end;

//=== Set/ change client time out ==============================================

procedure TServerForm.Clienttimeout1Click(Sender: TObject);
var
  Value: string;
  ValErr: Integer;
begin
  Value := IntToStr(FUserData.ConnectTimeOut);
  if InputQuery(c_DlgCaption, JVCSRES_Enter_client_timeout58_40sec41, Value) then
  begin
    Val(Value, FUserData.ConnectTimeOut, ValErr);
    if ValErr <> 0 then
    begin
      Windows.Beep(500, 100);
      MessageBox(WindowHandle, PChar(JVCSRES_Integer_expected33), PChar(c_DlgCaption),
                                                       MB_OK or MB_ICONWARNING);
      Exit;
    end;
    AppServer1.ClientTimeOut := FUserData.ConnectTimeOut;
    TimeOutLabel.Caption := Format(JVCSRES_37d_sec, [FUserData.ConnectTimeOut]);
    AddMessage(Format(JVCSRES_37s_45_Client_time_out_changed_to_37d_sec,
      [DateTimeToStr(Now), FUserData.ConnectTimeOut]), False);
    // set 'settings need to be saved' flag
    FUserData.SettingsChanged := True;
  end;
end;

//==============================================================================
// Set/ change version archive directory/ alias and try to reconnect the server
// to the new database/location.
// Check for valid DB server version (only MSSQL).

procedure TServerForm.Directory1Click(Sender: TObject);
var
  ErrMsg: string;
begin
  // Do not change this value while clients are connected
  if AppServer1.ClientCount > 0 then
  begin
    Windows.Beep(500, 100);
    if MessageBox(WindowHandle, PChar(Format(JVCSRES_37d_clients_already_connected33,
         [AppServer1.ClientCount]) + #10#13 +
         JVCSRES_The_requested_action_will_disconnect_all_clients46 + #10#13 +
         JVCSRES_Continue63), PChar(c_DlgCaption),
         MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_ICONWARNING) <> id_Yes then Exit;
  end;
  Disconnectall1Click(Self);
  Assert((FServerQuery <> nil), 'FServerQuery is nil.');
  Assert((FServerDatabase <> nil), 'FServerDatabase is nil.');
  {$IFDEF ORACLESRV}
  //-- hdo
  with TFormLogon.Create(Application) do
  try
    EditUser.Text := FDBUser;
    EditPassword.Text := FDBPassword;
    EditServer.Text := FDBServer;
    EditDatabase.Text := '';  //-- MG
    CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
    if ShowModal = mrCancel then Exit;
    FDBUser := EditUser.Text;
    FDBPassword := EditPassword.Text;
    FDBServer := EditServer.Text;
    FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
    FUserData.DBPath := FServerDatabase.DatabaseName;
    // set 'settings need to be saved' flag
    FUserData.SettingsChanged := True;
  finally
    Free;
  end;
  FServerDatabase.Close;
  FServerDatabase.UserName := FDBUser;
  FServerDatabase.Password := FDBPassword;
  FServerDatabase.ServerName := FDBServer;
  // force login dialog if working on a SQL DBMS
  FServerDatabase.Open;
  FServerQuery.DatabaseName := FUserData.DBPath;
  {$ENDIF ORACLESRV}
  {$IFDEF MSSQLSRV}
  //-- MG
  with TFormLogon.Create(Application) do
  try
    cbUseTrustedNTConnection.Checked := FDBUseNTTrustedConnection;
    EditUser.Text := FDBUser;
    EditPassword.Text := FDBPassword;
    EditServer.Text := FDBServer;
    EditDatabase.Text := FUserData.DBDatabase;
    CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
    if ShowModal = mrCancel then Exit;
    FDBUser := EditUser.Text;
    FDBPassword := EditPassword.Text;
    FDBServer := EditServer.Text;
    FUserData.DBDatabase := EditDatabase.Text;
    FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
    FUserData.DBPath := FServerDatabase;
    FDBUseNTTrustedConnection := cbUseTrustedNTConnection.Checked;
    // set 'settings need to be saved' flag
    FUserData.SettingsChanged := True;
  finally
    Free;
  end;
  FServerDatabase.Close;
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
  // force login dialog if working on a SQL DBMS
  FServerDatabase.Open;
  FServerQuery.DatabaseName := FUserData.DBPath;
  {$ENDIF MSSQLSRV}
  {$IFDEF MYSQLSRV} //--LB
  with TFormLogon.Create(Application) do
  try
    EditUser.Text := FDBUser;
    EditPassword.Text := FDBPassword;
    EditServer.Text := FDBServer;
    EditDatabase.Text := FUserData.DBDatabase;
    CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
    if ShowModal = mrCancel then Exit;
    FDBUser := EditUser.Text;
    FDBPassword := EditPassword.Text;
    FDBServer := EditServer.Text;
    FUserData.DBDatabase := EditDatabase.Text;
    FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
    FUserData.DBPath := FServerDatabase;
    // set 'settings need to be saved' flag
    FUserData.SettingsChanged := True;
  finally
    Free;
  end;
  FServerDatabase.Disconnect;
  FServerDatabase.HostName:=FDBServer;
  FServerDatabase.Database:=FUserData.DBDatabase;
  FServerDatabase.User:=FDBUser;
  FServerDatabase.Password:=FDBPassword;
  FServerDatabase.Connected:=True;
  FServerQuery.DatabaseName := FUserData.DBPath;
  {$ENDIF MYSQLSRV} //--LB
  {$IFDEF UIBSRV} //-- PrY
  with TFormLogon.Create(Application) do
  begin
    try
      cbUseFirebirdEmbeded.Checked := FDBUseEmbeded;
      EditUser.Text := FDBUser;
      EditPassword.Text := FDBPassword;
      EditServer.Text := FDBServer;
      EditDatabase.Text := FDBDatabase;
      cbFIBCharSet.ItemIndex := cbFIBCharSet.Items.IndexOf(FDBCharSet);
      CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
      if ShowModal = mrOk then
      begin
        FDBUser := EditUser.Text;
        FDBPassword := EditPassword.Text;
        FDBServer := EditServer.Text;
        FDBDatabase := EditDatabase.Text;
        FDBCharSet := cbFIBCharSet.text;
        FDBUseEmbeded := cbUseFirebirdEmbeded.checked;
        FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
        // set 'settings need to be saved' flag
        FUserData.SettingsChanged := True;
      end;
    finally
      Free;
    end;
  end;
  FServerDatabase.Connected := False;
  // Embedded or Service?
  FServerDatabase.LibraryName := GetFirebirdClientLibrary(FDBUseEmbeded);
  FServerDatabase.CharacterSet := StrToCharacterSet(FDBCharSet);
  FUserData.DBPath := FServerDatabase;
  FServerDatabase.UserName := FDBUser;
  FServerDatabase.PassWord := FDBPassword;
  if (UpperCase(FDBServer) = 'LOCAL') or
     (UpperCase(FDBServer) = 'LOCALHOST') or (Trim(FDBServer) = '') then
    FServerDatabase.DatabaseName := FDBDatabase
  else
    FServerDatabase.DatabaseName := FDBServer + ':' + FDBDatabase;
  // force login dialog if working on a SQL DBMS
  FServerDatabase.Connected := True;
  FServerQuery.DatabaseName := FUserData.DBPath;
  {$ENDIF UIBSRV}
  {$IFDEF ADOSRV} //-- PL
  with TFormLogon.Create(Self) do
  try
    EditUser.Text := FDBUser;
    EditPassword.Text := FDBPassword;
    EditServer.Text := FDBServer;
    CheckBoxAutoLogon.Checked := FUserData.DBAutoLogon;
    if ShowModal = mrCancel then Exit;
    FDBUser := EditUser.Text;
    FDBPassword := EditPassword.Text;
    FDBServer := EditServer.Text;
    FUserData.DBAutoLogon := CheckBoxAutoLogon.Checked;
    FUserData.DBPath := FServerDatabase;
    // set 'settings need to be saved' flag
    FUserData.SettingsChanged := True;
  finally
    Free;
  end;
  FServerDatabase.Close;
  FServerDatabase.Params.Clear;
  FServerDatabase.Params.Add('User ID=' + FDBUser);
  FServerDatabase.Params.Add('Password=' + FDBPassword);
  FServerDatabase.Params.Add('Provider=' + FDBServer);
  FServerDatabase.Params.Add('Data Source=' + FUserData.DBDatabase);
  // force login dialog if working on a SQL DBMS
  FServerDatabase.Open;
  FServerQuery.DatabaseName := FUserData.DBPath;
  {$ENDIF ADOSRV} //-- PL
  {$IFNDEF ADOSRV}
   {$IFNDEF MSSQLSRV}
    {$IFNDEF MYSQLSRV} //-- LB
        {$IFNDEF UIBSRV}
        Assert((FUserData.DBPath <> ''), 'FUserData.DBPath not initialized.');
        {$ELSE}
        Assert((FUserData.DBPath <> nil), 'FUserData.DBPath not initialized.');
        {$ENDIF ~UIBSRV}
    {$ELSE}
    Assert((FUserData.DBPath <> nil), 'FUserData.DBPath not initialized.');
    {$ENDIF ~MYSQLSRV}
  {$ENDIF ~MSSQLSRV}
  {$ELSE}
  Assert((FUserData.DBPath <> nil), 'FUserData.DBPath not initialized.');
  {$ENDIF ~ADOSRV}
  FDBMSConnected := True;
  SetupUI;
  {$IFDEF BETAVER}
  Caption := Format(JVCSRES_37s_37s_Beta_45_Running, [MainTitle, GetSrvVersionString(cServerId, True)]);
  {$ELSE}
  Caption := Format(JVCSRES_37s_37s_45_Running, [MainTitle, GetSrvVersionString(cServerId, True)]);
  {$ENDIF BETAVER}
  AddMessage(Format(JVCSRES_37s_45_DBMS_connected46, [DateTimeToStr(Now)]), False);
  { Start the application server component }
  AppServer1.ClientTimeOut := FUserData.ConnectTimeOut;
  AppServer1.Port := FPort;
  if not StartApplicationServer(ErrMsg) then
  begin
    Windows.Beep(500, 100);
    MessageBox(WindowHandle, PChar(ErrMsg), PChar(c_DlgCaption), MB_OK or
      MB_ICONSTOP);
    AddMessage(DateTimeToStr(Now) + '!!' + ErrMsg, True);
    Exit;
  end;
  Start1.Enabled := False;
  Stop1.Enabled := not Start1.Enabled;
end;

//==============================================================================
// Calculate/ update server up time - called every second for the application
// server/ every 60 secs for the Windows service.
// Calculate some statistic data.

procedure TServerForm.UpTimerTimer(Sender: TObject);
var
  Value: Double;
  UPTimeStr: string;
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
  {$IFDEF UPTIME1S}
  UPTimeStr := UPTimeStr + FloatToStr(Value) + ' m, ';
  FUpTime := FUpTime - Value;
  FUpTime := FUpTime * 60;
  Value := Int(FUpTime);
  UPTimeStr := UPTimeStr + FloatToStr(Value) + ' s';
  {$ELSE}
  UPTimeStr := UPTimeStr + FloatToStr(Value) + ' m';
  {$ENDIF UPTIME1S}
  // update user interface
  UpTimeLabel.Caption := UPTimeStr;
  if AppServer1.RequestCount = 0 then
    RequestCountLabel.Caption := '0'
  else
    RequestCountLabel.Caption := Format('%d  (Avg. %.0f/h)',
                  [AppServer1.RequestCount,
                   AppServer1.RequestCount / ((Now - FStartTime) * 24)]);
  RcvBytesLabel.Caption :=  Format('%n MB',
                                         [(FUserData.ReceivedBytes / 1048576)]);
  TrmBytesLabel.Caption :=  Format('%n MB',
                                      [(FUserData.TransmittedBytes / 1048576)]);
  FUserData.RequestCount := AppServer1.RequestCount;
  FUserData.UpTimeStr := UPTimeStr;
end;

{$IFDEF MYSQLSRV}
{-----------------------------------------------------------------------------
  Procedure: TServerForm.RemoteDirectoryExists
  Author:    LB
  Date:      20-déc.-2002
  Arguments: sDir: string
  Result:    boolean

  Test if directory exists in Database server.
-----------------------------------------------------------------------------}
function TServerForm.RemoteDirectoryExists(sDir: string): Boolean;

  function DoubleSlash(S: string): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(S) do
      if S[I] <> '\' then
        Result := Result + S[I]
      else
        Result := Result + S[I] + S[I];
  end;

begin
  //compiler hint, not necessary -> result:=false;
  with FServerQuery do
  begin
    SQL.Text := DoubleSlash('SELECT 1 INTO OUTFILE "' + sDir + 'testdir" from users');
    try
      Open;
// three answers possible:
//   OK File created, no return : directory exists
//   Error File exists : directory exists
//   Error can't create: directory does not exist
      Result:=True;
    except
      on E: EDataBaseError do
        Result := (Pos('Errcode', E.Message) <= 0);
    end;
  end;
end;
{$ENDIF MYSQLSRV}


//==============================================================================
// Check for expired log in, save server log file & settings if necessary & check
// for auto backup - called every 30 sec

procedure TServerForm.TATimerTimer(Sender: TObject);
var
  IdleTime: Double;
  ExpClients: Boolean;
begin
  {$IFDEF DBBACKUPSUPPORT}
  // Autobackup
  if FAutoBackupActive then
  begin
    // is this a new day?
    if Time < FLastBackup then FLastBackup := 0;
    // time for Backup?
    if (Time >= FAutoBackupTime) and
       (FAutoBackupTime > FLastBackup) then
    begin
      // yes
      // prevent auto backup from running twice a day
      FLastBackup := FAutoBackupTime;
      AddMessage(Format(JVCSRES_37s_45_Auto_backup_start46, [DateTimeToStr(Now)]), True);
      {$IFDEF MYSQLSRV}
      if (FUserData.BackupPath <> '') and
         (RemoteDirectoryExists(FUserData.BackupPath)) then
      {$ELSE}
      if (FUserData.BackupPath <> '') and
         (DirectoryExists(FUserData.BackupPath)) then
      {$ENDIF MYSQLSRV}
      begin
        LiveBackup(FUserData.BackupPath);
        AddMessage(Format(JVCSRES_37s_45_Auto_backup_ready46, [DateTimeToStr(Now)]), True);
      end else AddMessage(Format(JVCSRES_37s_45_3333_Backup_folder_not_found46, [DateTimeToStr(Now)]), True);
    end; // if (Time >= FAutoBackupTime) and...
  end; // if FAutoBackup then begin
  {$ENDIF DBBACKUPSUPPORT}
  // calculate difference between local time & GMT
  GetLocalGMTDifference;
  // log file need to be saved?
  if FUserData.WriteServerLog and (FLog.Count > 20) then SaveLogfile;
  // settings need to be saved?
  if FUserData.SettingsChanged then SaveSettings;
  // remove expired transaction numbers
  ExpClients := False;
  if FDBMSConnected and FUserData.LoginExpires then
  begin
    IdleTime := Now - FUserData.LoginTimeOut;
    with FServerQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT users.login');
      SQL.Add('FROM transact, users');
      SQL.Add('WHERE expires < :expires');
      SQL.Add('AND users.userid = transact.accessid');
      ParamByName('expires').AsDateTime := IdleTime;
      Open;
      while not Eof do
      begin
        ExpClients := True;
        AddMessage('', True);
        AddMessage(Format(JVCSRES_37s_45_Login_idle_timer_overflow46_37s_logged_out_by_server46_,
          [DateTimeToStr(Now), FieldByName('login').AsString]), True);
        AddMessage('', True);
        Next;
      end;
      Close;
      if ExpClients then
      begin
        SQL.Clear;
        SQL.Add('DELETE FROM transact');
        SQL.Add('WHERE expires < :expires');
        ParamByName('expires').AsDateTime := IdleTime;
        ExecSQL;
      end; // if ExpClients then begin
    end; // with FServerQuery do begin
  end; // if FDBMSConnected and FUserData.LoginExpires then begin
end;

//=== Save the server's log file ===============================================

procedure TServerForm.SaveLogfile;
begin
  try
    SaveServerLogFile(FLog);
    FLog.Clear;
    GetLogfileSize;
  except
    on E:Exception do
    begin
      AddMessage(Format(JVCSRES_37s_45_3333_Log_file_access_error58_37s,
        [DateTimeToStr(Now), GetJvcsServerLogFile]) + #10#13 + E.Message, True);
      Exit;
    end;
  end;
  AddMessage(Format(JVCSRES_37s_45_Log_file_37s_stored46, [DateTimeToStr(Now), GetJvcsServerLogFile]), False);
end;

//=== Get (&Show) the server's log file size ===================================

procedure TServerForm.GetLogfileSize;
begin
  if GetServerLogFileSizeKByte <> -1 then
    SrvLogLabel.Caption := Format(JVCSRES_active44_37d_KB, [GetServerLogFileSizeKByte])
    else
      SrvLogLabel.Caption := JVCSRES_active44_63_KB;
end;

//=== Enable/ disable server log file ==========================================

procedure TServerForm.Logfileactive1Click(Sender: TObject);
begin
  Logfileactive1.Checked := not Logfileactive1.Checked;
  FUserData.WriteServerLog := Logfileactive1.Checked;
  if not FUserData.WriteServerLog then
    SaveLogfile;
  if FUserData.WriteServerLog then
    GetLogfileSize
  else
    SrvLogLabel.Caption := JVCSRES_inactive;
  // set 'settings need to be saved' flag
  FUserData.SettingsChanged := True;
end;

//=== Show server log file =====================================================

procedure TServerForm.Showlogfile1Click(Sender: TObject);
begin
  SaveLogfile;
  if FileExists(GetJvcsServerLogFile) then
    ShowServerLogFile(application.handle)
  else
    begin
      Windows.Beep(500, 100);
      MessageBox(WindowHandle, PChar(Format(JVCSRES_File_37s_not_found46, [GetJvcsServerLogFile])),
                                       PChar(c_DlgCaption), MB_OK or MB_ICONSTOP);
  end;
end;

//=== Show & log client access faults (Access denied) ==========================

procedure TServerForm.LogClientAccessFaults1Click(Sender: TObject);
begin
  LogClientAccessFaults1.Checked := not LogClientAccessFaults1.Checked;
  FUserData.LogAccessFault := LogClientAccessFaults1.Checked;
end;

//=== Clear server log file ====================================================

procedure TServerForm.Clearlogfile1Click(Sender: TObject);
begin
  if FileExists(GetJvcsServerLogFile) then
  begin
    if MessageBox ( WindowHandle
                  , PChar(JVCSRES_Are_you_sure_to_clear_the_logfile63)
                  , PChar(c_DlgCaption)
                  , MB_YESNOCANCEL or MB_DEFBUTTON2 or MB_ICONWARNING) = id_Yes then
    begin
      DeleteFile(GetJvcsServerLogFile);
      AddMessage(Format(JVCSRES_37s_45_Log_file_37s_deleted46, [DateTimeToStr(Now), GetJvcsServerLogFile]), True);
      GetLogfileSize;
    end;
  end
  else
  begin
    Windows.Beep(500, 100);
    MessageBox( WindowHandle
              , PChar(Format(JVCSRES_File_37s_not_found46, [GetJvcsServerLogFile]))
              , PChar(c_DlgCaption)
              , MB_OK or MB_ICONSTOP
              );
  end;
end;

//=== Windows (Registry) based Autostart option ================================

procedure TServerForm.AutostartwithWindows1Click(Sender: TObject);
begin
  AutostartwithWindows1.Checked := not AutostartwithWindows1.Checked;
  with TRegistry.Create do
  begin
    try
      try
        RootKey := HKEY_CURRENT_USER;
        OpenKey('Software\Microsoft\Windows\CurrentVersion\Run', True);
        if AutostartwithWindows1.Checked then
          WriteString('FreeVCSServer', Application.ExeName + #0)
        else
        if ValueExists('FreeVCSServer')
          then DeleteValue('FreeVCSServer');
        CloseKey;
      finally
        Free;
      end;
    except
      on E:Exception do
      begin
        Windows.Beep(500, 100);
        MessageBox(WindowHandle, PChar(JVCSRES_Registry_write_access_error58_ +
                  #10#13 + E.Message), PChar(c_DlgCaption), MB_OK or MB_ICONSTOP);
      end;
    end;
  end; // with TRegistry.Create do begin
end;

//=== Change version archive backup folder =====================================

procedure TServerForm.Targetfolder1Click(Sender: TObject);
{$IFNDEF MYSQLSRV}
var
  Value: string;
{$ENDIF ~MYSQLSRV}  
begin
  {$IFNDEF MYSQLSRV}
  Value := '';
  if not SelectDirectory(JVCSRES_Select_backup_target_folder, '', Value) then
    Value := '';
  if Value <> '' then
  begin
    // Be sure that value is followed by an ending backslash
    if Value[Length(Value)] <> '\' then Value := Value + '\';
    {$IFNDEF ADOSRV} // PL
     {$IFNDEF MSSQLSRV}
        {$IFNDEF MYSQLSRV}
              {$IFNDEF UIBSRV}
              if LowerCase(FUserData.BackupPath) = LowerCase(FUserData.DBPath) then
              {$ELSE}
              if LowerCase(FUserData.BackupPath) =
                                     LowerCase(FUserData.DBPath.DatabaseName) then
              {$ENDIF ~UIBSRV}
        {$ENDIF ~MYSQLSRV}
        begin
          Windows.Beep(500, 100);
          MessageBox(WindowHandle,
                               PChar(JVCSRES_You_cannot_use_the_archive_folder_for_that33),
                               PChar(c_DlgCaption), MB_OK or MB_ICONWARNING);
          Exit;
        end;
     {$ENDIF ~MSSQLSRV}
    {$ENDIF ~ADOSRV}
    FUserData.BackupPath := Value;
    AddMessage(Format(JVCSRES_37s_45_Backup_target_folder_changed_to_37s,
      [DateTimeToStr(Now), FUserData.BackupPath]), True);
    // set 'settings need to be saved' flag
    FUserData.SettingsChanged := True;
  end;
  {$ENDIF ~MYSQLSRV}
end;

//=== Time controlled auto backup of the version archive - get settings ========

procedure TServerForm.Autobackup1Click(Sender: TObject);
{$IFDEF DBBACKUPSUPPORT}
var
  DlgResult: Integer;
{$ENDIF DBBACKUPSUPPORT}
begin
  {$IFDEF DBBACKUPSUPPORT}
  SrvAutoBackup := TSrvAutoBackup.Create(Application);
  try
    SrvAutoBackup.Left := Left + 60;
    SrvAutoBackup.Top := Top + 60;
    with SrvAutoBackup do
    begin
      BackupActive := FAutoBackupActive;
      BackupTime := FAutoBackupTime;
      {$IFDEF MYSQLSRV}
      BackupPath:=FUserData.BackupPath;
      {$ENDIF MYSQLSRV}
      DlgResult := ShowModal;
      if DlgResult = mrOk then
      begin
        {$IFDEF MYSQLSRV}
        FUserData.BackupPath:=BackupPath;
        {$ENDIF MYSQLSRV}
        FAutoBackupActive := BackupActive;
        FAutoBackupTime := BackupTime;
      end;
    end; // with SrvAutoBackup do begin
  finally
    SrvAutoBackup.Free;
  end;
  if DlgResult <> mrOk then Exit;
  // Target folder already defined?
  {$IFDEF MYSQLSRV}
  if FAutoBackupActive and
     ((FUserData.BackupPath = '') or
      (not RemoteDirectoryExists(FUserData.BackupPath))) then
  {$ELSE}
  if FAutoBackupActive and
     ((FUserData.BackupPath = '') or
      (not DirectoryExists(FUserData.BackupPath))) then
  {$ENDIF MYSQLSRV}
  begin
    Windows.Beep(500, 100);
    MessageBox(WindowHandle,
                           PChar(JVCSRES_Warning33_Backup_target_folder_is_undefined33),
                                    PChar(c_DlgCaption), MB_OK or MB_ICONWARNING);
    FAutoBackupActive :=  False;
  end;
  // update user interface
  if FAutoBackupActive
    then AutoBkpLabel.Caption :=
                          FormatDateTime('"' + JVCSRES_active + ', ("hh:mm")"', FAutoBackupTime)
      else AutoBkpLabel.Caption := JVCSRES_not_active;
  // set 'settings need to be saved' flag
  FUserData.SettingsChanged := True;
  {$ENDIF DBBACKUPSUPPORT}
end;

//==============================================================================
// Live backup of the version archive
//
// Available for UIB, MySQL, MSSQL servers

procedure TServerForm.Execute1Click(Sender: TObject);
begin
  {$IFDEF DBBACKUPSUPPORT}
  if MessageBox(WindowHandle, PChar(JVCSRES_Start_version_archive_backup_now63),
      PChar(c_DlgCaption), MB_YESNOCANCEL or MB_ICONQUESTION) <> id_Yes then Exit;
  if FUserData.BackupPath = '' then Targetfolder1Click(Self);
  if FUserData.BackupPath = '' then Exit;
  LiveBackup(FUserData.BackupPath);
  {$ELSE}
  MessageBox(WindowHandle, PChar(JVCSRES_Not_supported_by_this_type_of_server46),
                                       PChar(c_DlgCaption), MB_OK or MB_ICONSTOP);
  {$ENDIF DBBACKUPSUPPORT}
end;

{===============================================================================
 Live backup of the version archive - Backs up all database tables to the
 specified directory (FUserData.BackupPath).

 For MSSQL servers: //-- MG
 --------------------------------
 The database is archived to the backup folder and given the extension '.dat'.
 Already existing backup databases in the target folder are renamed with the
 '.~dat' extension and deleted AFTER a successful backup operation.
 Both the database and the transation log are backed-up.
 Please note that in order to restore a backed-up database you will need to use
 the Enterprise Manager.

 The same procedure is available as a server object (LIVE_BACKUP).

 Currently available for UIB, MySQL, MSSQL servers
===============================================================================}
{$IFDEF DBBACKUPSUPPORT} //-- VR
procedure TServerForm.LiveBackup(Location: string);
var
  {$IFNDEF UIBSRV}
  BackupSize: Int64;
  {$ENDIF ~UIBSRV}
  {$IFDEF UIBSRV}
  FIBBackup: TUIBBackup;
  BackupFileName: string;
  {$ENDIF UIBSRV}
  {$IFDEF MSSQLSRV}
  F: TSearchRec;
  BackupFileName: string;
  {$ENDIF MSSQLSRV}
  {$IFDEF MYSQLSRV}
  BackupFileName: string;

  function DoubleSlash(S: string): string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 1 to Length(S) do
      if S[I] <> '\' then
        Result := Result + S[I]
      else
        Result := Result + S[I] + S[I];
  end;
 {$ENDIF MYSQLSRV}

begin
  // Be sure that location is followed by an ending backslash
  if Location[Length(Location)] <> '\' then Location := Location + '\';

  BackupSize := 0;
  // compiler hint off
  if BackupSize = 0 then;

  {$IFDEF UIBSRV}
  try
    // create the Backup Service component, the rest is simple
    FIBBackup := TUIBBackup.Create(Self);
    with FIBBackup do
    begin
      try
        if (UpperCase(FDBServer) = 'LOCAL') or (FDBServer = '') then
          Host := 'localhost'
        else
          Host := FDBServer;
        Protocol := proTCPIP;
        UserName := FDBUser;
        Password := FDBPassword;
        Options := [boIgnoreLimbo];
        OnVerbose := OnBackupVerbose;
        Database := FDBDatabase;
        BackupFileName := Location +
                          ChangeFileExt(ExtractFileName(FDBDatabase), '.fbk');
        BackupFiles.Add(BackupFileName);
        RenameFile(BackupFileName, ChangeFileExt(BackupFileName, '.~fbk'));

        AddMessage(Format(JVCSRES_37s_45_Database_backup_started,
          [DateTimeToStr(Now)]), True);
        Run;
        AddMessage(Format(JVCSRES_37s_45_Database_backup_completed,
          [DateTimeToStr(Now)]), True);
        DeleteFile(ChangeFileExt(BackupFileName, '.~fbk'));
      finally
        Free;
      end;
    end;
  {$ENDIF UIBSRV}
  {$IFDEF MSSQLSRV}
  try
    BackupFileName := FUserData.BackupPath + FUserData.DBDatabase + '_backup.dat';
    RenameFile(BackupFileName, ChangeFileExt(BackupFileName, '.~dat'));
    try
      with TSrvQuery.Create(nil) do
      begin
        DatabaseName := FUserData.DBPath;
        AddMessage(Format(JVCSRES_37s_45_Database_backup_started_to_37s,
          [DateTimeToStr(Now), BackupFileName]), True);
        with SQL do
        begin
          Clear;
          Add('EXEC sp_addumpdevice ''disk'', ''FreeVCSBackup'', ''' +
                                                         BackupFileName + '''');
        end;
        ExecSQL;
        with SQL do
        begin
          Clear;
          Add('BACKUP DATABASE ' + FUserData.DBDatabase +
                                                 ' TO FreeVCSBackup WITH INIT');
        end;
        try
          ExecSQL;
        finally
          with SQL do
          begin
            Clear;
            Add('EXEC sp_dropdevice ''FreeVCSBackup''');
          end;
          ExecSQL;
        end;
        Free;
      end;
      DeleteFile(ChangeFileExt(BackupFileName, '.~dat'));

    except
    end;
    // I don't know how to collect the data returned from the BACKUP SQL-command
    if FindFirst(BackupFileName, faAnyFile, F) = 0 then
      BackupSize := F.Size;
    FindClose(F);
  {$ENDIF MSSQLSRV}
  {$IFDEF MYSQLSRV}
  try
    BackupFileName := FUserData.BackupPath;
    //remove ending slash or backslash
    if (BackupFileName[Length(BackupFileName)]='/') or (BackupFileName[Length(BackupFileName)]='\') then
      delete(BackupFileName,Length(BackupFileName),1);
    try
      with TSrvQuery.Create(nil) do
      begin
        DatabaseName := FUserData.DBPath;
        AddMessage(Format(JVCSRES_37s_45_Database_backup_started_to_37s,
          [DateTimeToStr(Now), BackupFileName]), True);
        SQL.Text:='BACKUP TABLE blobs,bugs,ffamily,groups,labels,logcomm,';
        SQL.Add('mdbugs,modules,mstones,pjbugs,pjgroups,pjmodule,pjmstone,');
        SQL.Add('pjref,pjusers,projects,revision,rvlabels,todo,transact,');
        SQL.Add('users,vcslog TO "' + DoubleSlash(BackupFileName) + '"');
        Open;
        //Dump Reply;
        while not Eof do
        begin
          AddMessage(DateTimeToStr(Now) + ' >> '+ Fields[0].AsString +' '+
                     Fields[1].AsString + ' '+ Fields[2].AsString +' '+
                     Fields[3].AsString , True);
          Next;
        end;
        Free;
      end;
    except
    end;
    BackupSize := 0;
  {$ENDIF MYSQLSRV}
  except
    on E:Exception do
    begin
      AddMessage(Format(JVCSRES_37s_45_3333_Exception_in_Backup58, [DateTimeToStr(Now)])
        + #10#13 + E.Message, True);
      AddMessage(Format(JVCSRES_37s_45_3333_Backup_failed46, [DateTimeToStr(Now)]), True);
      Exit;
    end;
  end;
  {$IFNDEF UIBSRV}
  AddMessage(Format(JVCSRES_37s_45_Backup_ready46_Success46_37d_KB,
    [DateTimeToStr(Now), BackupSize div 1024]), True);
  {$ENDIF ~UIBSRV}
  // store backup time stamp
  FUserData.LastLiveBackup := LocalDT2GMTDT(Now);
  // set 'settings need to be saved' flag
  FUserData.SettingsChanged := True;
end;
{$ENDIF DBBACKUPSUPPORT}

//=== Hide the application server window =======================================

procedure TServerForm.Hide1Click(Sender: TObject);
begin
  ServerForm.Hide;
  FHideWindow := True;
end;

//=== Always start hidden ======================================================

procedure TServerForm.StartHidden1Click(Sender: TObject);
begin
  StartHidden1.Checked := not StartHidden1.Checked;
end;

//==============================================================================
// Taskbar icon - show the icon
// Maximum hint length = 63 characters!

procedure TServerForm.ShowIcon(const Hint: string);
var
  NIData: TNotifyIconData;
begin
  NIData.cbSize := SizeOf(NIData);
  NIData.Wnd := Handle;
  NIData.uID := 0;
  NIData.uCallbackMessage := WM_ICONCALLBACK;
  NIData.hIcon := FSrvIcon.Handle;
  StrCopy(NIData.szTip, PChar(Hint));
  NIData.uFlags := NIF_MESSAGE or NIF_ICON or NIF_TIP;
  Shell_NotifyIcon(NIM_ADD, @NIData);
end;

//=== Taskbar icon - hide the icon =============================================

procedure TServerForm.HideIcon;
var
  NIData: TNotifyIconData;
begin
  NIData.cbSize := SizeOf(NIData);
  NIData.Wnd := Handle;
  NIData.uID := 0;
  Shell_NotifyIcon(NIM_DELETE, @NIData);
end;

//=== Taskbar icon - icon (double click) callback procedure ====================

procedure TServerForm.WMIconCallback(var Msg: TMessage);
begin
  if Msg.lParam = WM_LBUTTONDBLCLK then
  begin
    ServerForm.Show;
    ServerForm.BringToFront;
    FHideWindow := False;
  end;
end;

//=== Get the servers real IP number ===========================================

function TServerForm.GetLocalIP: string;
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

procedure TServerForm.About1Click(Sender: TObject);
begin
  ShowSrvAboutForm;
end;

procedure TServerForm.Upgradearchive1Click(Sender: TObject);
begin
  if MessageBox ( WindowHandle
                , PChar(JVCSRES_Upgrading_your_database_archive_is_normally_done_automatically_on_Appserver_startup46 + #13 +
                  JVCSRES_If_this_failed_for_any_reason_you_might_upgrade_now_manually46 + #13 +
                  JVCSRES_Please_make_sure_that_there_are_no_JVCS_clients_connected_during_upgrade33 + #13#13 +
                  JVCSRES_Continue_with_archive_upgrade_now63)
                , PChar(c_DlgCaption)
                , MB_OKCANCEL or MB_ICONINFORMATION) = mrOk then
  begin
    CheckArchiveVersion;
  end;
end;

procedure TServerForm.HideOnClose1Click(Sender: TObject);
begin
  HideOnClose1.Checked := not HideOnClose1.Checked;
end;

procedure TServerForm.WMQueryEndSession(var Msg: TMessage);
begin
  FInExit := True;
  FInEndSession := True;
  inherited;
  // Always allow shutdown when Windows is shutting down...
  Msg.Result := 1;
end;

procedure TServerForm.WMEndSession(var Msg: TMessage);
begin
  FInExit := True;
  FInEndSession := True;
  inherited;
  // Close if Windows is shutting down...
  Close;
end;

procedure TServerForm.ChangeServerBanner1Click(Sender: TObject);
begin
  if InputQuery(JVCSRES_Server_banner, JVCSRES_Server_banner, FBanner) then
  begin
    if FBanner = '' then
      FBanner := FDefaultBanner;
    if DisableServerBanner1.Checked then
      AddMessage(JVCSRES_Server_banner58_disabled, False)
    else
    begin
      AddMessage(Format(JVCSRES_Server_banner58_3437s34, [FBanner]), False);
      AppServer1.Banner := FBanner;
    end;
  end;
end;

{$IFDEF UIBSRV}
procedure TServerForm.OnBackupVerbose(Sender: TObject; Line: string);
begin
  AddMessage(DateTimeToStr(Now) + ' >> ' + Line, True);
  if (Pos('closing file', Line) > 0) then
    BackupSize := StrToIntDef(Copy(Line,
                              Pos('.', Line)+2,
                              Pos('bytes written', Line)- Pos('.', Line)-3),
                              0);
end;
{$ENDIF UIBSRV}

end.

