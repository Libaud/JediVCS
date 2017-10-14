{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SvrOBJMain.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
ToDo
- we need a GetDiskFreeSpaceEx equivalent function in GET_SERVER_OPTIONS for Linux
-----------------------------------------------------------------------------

Unit history:

  Aug '99 by Thomas Hensle - http://www.freevcs.de
  Based on François PIETTE's SvrTest demo application.

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
                      - removed IFDEFed SqlDateToDateTime in mySQL port
2003/03/02  THuber    - removed compiler warnings
                      - added some missed IFDEF's for F L A S H F I L E R
                      - changed handling for server port version detecting
2003/03/07  THuber    - removed A CCESSCTRL IFDEF
2003/03/08  THuber    - make source compile with  d b i s a m  3.x
2003/12/21  CSchuette - login problems with F l a s h F i l e r  2.13 fixed.
2003/12/23  CSchuette - corrected  F L A S H F I L E R  2.13 login fixes to not affect
                         D B I S A M  port
2003/12/26  THuber    - DlgCaption => c_DlgCaption
2004/07/10  THuber    - #1948 - speed up GET_DESERTED_MODULES
2004/07/12  CSchuette - #1961 - bug in PURGE_PROJECT with  F l a s h F i l e r  fixed
                      - changed "RecordCount>0" to "not IsEmpty" check in
                        PURGE PROJECT (speed issue)
2004/07/13  THuber    - replaced IsEmpty due to broken build for Firebird
2004/08/03  CSchuette - #2024 - GET_DESERTED_MODULES did not fetch module path
                        from database

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster - style cleaning
                      - changes for Firebird port over JVCL UIB components
                        by Pierre Y. (partly adapted)
2004/12/18  USchuster - changes for Zeos DBO 6.x
2005/01/11  CSchuette - #2494 - SEARCH_MODULES will handle full file names and
                        file masks correctly now
2005/03/13  USchuster - changed table and field names in GET_DESERTED_MODULES to
                        lowercase to avoid problems with casesensitive OS (mantis #2699)
2005/05/08  USchuster - removed unused units
                      - made some changes for Linux
2005/09/22  THuber    #3212 adjust AutoExpand Buffer for Blob transfer
                            GET_SERVER_LOG
                            added min buffer size to size calculation
2005/10/24  USchuster - workaround for wrong MySQL datetime field type (mantis #3285)
2009/07/11  USchuster - changes for latest UIB from SVN (JvUIB -> UIB)
2009/11/30  THuber    #5032 change server log location
--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
                     special Oracle J O I N  - Syntax was removed we now only
                     support the A N S I - J o i n Syntax and therefore removed
                     this compiler setting.

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	SvrOBJMain - ServerObjects for FreeVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvOBJMain;

{$I jedi.inc}
{$I compopt.inc}

{$IFDEF DELPHI6_UP}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF DELPHI6_UP}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  {$IFNDEF MYSQLSRV}
  FileCtrl, //necessary for DirectoryExists in LIVE_BACKUP
  {$ENDIF ~MYSQLSRV}
  {$IFDEF MYSQLSRV}
  {$IFDEF ZEOS54}
  ZExtra,
  {$ELSE}
  ZSysUtils,
  {$ENDIF ZEOS54}
  {$ENDIF MYSQLSRV}
  ApSrvCli, RFormat, SrvCustomSrvOBJ, SrvConst, SrvUDRec, SrvDBDef, SrvAccessDef, DB;

type
  TServerObjectLOGIN = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectLOGOUT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectWHOAMI = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SERVER_OPTIONS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectSET_SERVER_OPTIONS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_RIGHTS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectSET_PROJECT_RIGHTS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_USERLIST = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_USERS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_USER = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectCHANGE_PASSWORD = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_USER = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SERVER_TIME = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SERVER_LOG = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectCLEAR_SERVER_LOG = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_ARCHIVE_TSTAMP = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_ID = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_RIGHT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectSEARCH_MODULES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_DESERTED_MODULES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SPACE_BY_PROJECTS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SPACE_BY_MODULES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectLIVE_BACKUP = class(TFVCSServerObject)
  {$IFDEF UIBSRV}
  private
    BackupSize: Integer;
    procedure OnBackupVerbose(Sender: TObject; Line: string);
  {$ENDIF UIBSRV}
  public
    procedure Execute; override;
  end;

  TServerObjectPURGE_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectEXECUTE_SQL = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREAD_CONFIG_DATA = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectWRITE_CONFIG_DATA = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

implementation

uses
  ServerCryptInt, {  interface unit for ServerCrypt.dll -
                        compare if two encrypted passwords are equal  }
{$IFDEF UIBSRV} //-- PrY
 UIB, //necessary for LIVE_BACKUP
 {$ENDIF UIBSRV}
 VCSCommon,
 JVCSServerFunctions,
 IniFiles,
 JclStrings;

{==============================================================================
  Server Login
  Login with user name & password.
  User's password is encrypted using the servers local timestamp as (a part
  of) the encryption key.
  Due to this a password submitted by a client is only valid within one minute.
  Server login may be also restricted to the stored client IP address. All users
  with a stored IP of '0' can login from any client.
  Server return accepted or not accepted as boolean value, the user's ID, a
  transaction number for client requests and the server's login message (any
  text message like 'user accepted...')
  For each request client sends user ID and transaction number back to the
  server objects. The combination of user ID and transaction number is the
  access key for all server objects.
  Transaction numbers are valid until the user logs out, changes the project
  (ask for a new projectID) or the predifined expiration time was reached.
  The server removes such expired numbers automatically.

        Record  Field
        ======  =====
  Request:  0: [0]User name              - String(50)
               [1]User password          - String(200)
               [2]Client  version        - Integer
               [3]Clint timestamp        - Double

  Response: 0: [0]accepted?              - Boolean
               [1]User ID                - Integer
                  (return -1 if not accepted)
               [2]Transaction number     - Integer
                  (access key for client request - client will include this
                   number in any server requests, return -1 if not accepted)
               [3]Login message           - String(50)
                  (optional, may be blank)
               [4]Server version          - Integer
               [5]Server type             - String

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectLOGIN.Execute;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  User,
  Password,
  CliVersion,
  IPAddr,
  StoredPassword,
  StoredIPAddr,
  ErrorMsg,
  LevelMsg: string;
  CliTimeStamp: Double;
  Rights,
  UserID,
  TANr: Integer;
  ForceGlobalIni,
  Accepted: Boolean;
  IniFile: TIniFile;
  {  compare the clients IP address to the stored IP address
     - this function allows the use of IP masks like 192.168.0.* or
       192.168.0.2?  }
  function CheckIPMask(ClientIP, StoredIP: string): Boolean;
  var
    I, J: Integer;
  begin
    Result := True;
    try
      ClientIP := AnsiLowerCase(ClientIP);
      StoredIP := AnsiLowerCase(StoredIP);
      I := 0;
      J := 0;
      if (Pos('*', StoredIP) = 0) and (Length(StoredIP) <> Length(ClientIP))
        then Result := False
      else
      begin
        repeat // until (not Result) or...
          Inc(I);
          Inc(J);
          if StoredIP[I] = '*' then
          begin
            // '*' = last position of StoredIP?
            if (I < Length(StoredIP)) then
            begin // no
              Inc(I);
              while (J < Length(ClientIP)) and
                    (StoredIP[I] <> ClientIP[J]) do Inc(J);
              Result := (StoredIP[I] = ClientIP[J]);
            end;
          end // if StoredIP[i] = '*' then begin
          else
          begin
            if (StoredIP[I] <> '?') then
              Result := (StoredIP[I] = ClientIP[J]);
          end;
        until (not Result) or
              (J >= Length(ClientIP)) or (I >= Length(StoredIP));
      end; // else if (Pos('*', StoredIP) = 0) and...
      if Result then
        if (StoredIP[I] <> '*') then
          Result := (J = Length(ClientIP)) and (I = Length(StoredIP));
    except
      Result := False;
    end;
  end; // function CheckIPMask(...

begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    UserID := 0;
    TANr := 0;
    Rights := 0;
    LevelMsg := '';
    ErrorMsg := '';
    UData := PUserDataRecord(FUserData);
    User := FRequestBuffer.Fields[0];
    Password := FRequestBuffer.Fields[1];
    IPAddr := TClientWSocket(FORBDataPtr.Tag).PeerAddr;
    if UData.ShowServerMsg then
    begin
      TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr + ' [LOGIN] ' +
                                                                          User);
    end;
    {  check for additional client version information, to be sure that we
       don't allow connects from outdatet clients. Only clients as from 2.2.0
       provides such information, older clients will provide a blank string  }
    CliVersion := FRequestBuffer.Fields[2];
    if UData.ShowServerMsg then
    begin
      if CliVersion <> '' then
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                                       ' [LOGIN] Client version: ' + CliVersion)
      else
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
         ' [LOGIN] Client version: unknown or older than 2.2.0. Access denied');
    end; // if UData.ShowServerMsg then
    // check if the client's version number is ok
    if (CliVersion = '') or
       (StrToInt(CliVersion) < ExpectedCliVersion) then
    begin
      // no, client version is wrong, reject the client
      FResultStatus := 403;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
            [Format(InvClient, [FFunctionCode, (ExpectedCliVersion div 100),
                                               (ExpectedCliVersion mod 100)])]);
      Finish;
      Exit;
    end; // if (CliVersion = '') or...
    // check if the client's timestamp is ok
    CliTimeStamp := _StrToFloat(FRequestBuffer.Fields[3]);
    if UData.CheckCliTimeStamp and (CliTimeStamp <> 0) then
    begin
      if UData.ShowServerMsg then
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
          ' [LOGIN] Client timestamp: ' + DateTimeToStr(CliTimeStamp) + ' GMT');
      {  max. 2 hours difference (GMT) between server & client allowed,
         otherwise the clients date is invalid  }
      if (CliTimeStamp < (LocalDT2GMTDT(Now) - 1/12)) or
         (CliTimeStamp > (LocalDT2GMTDT(Now) + 1/12)) then
      begin
        // no, client's timestamp is invalid, reject the client
        if UData.ShowServerMsg then
          TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                         ' [LOGIN] Client timestamp is invalid. Access denied');
        FResultStatus := 403;
        FResponseBuffer.Rewrite;
        FResponseBuffer.WriteFields(False,
                                      [Format(InvClientTime, [FFunctionCode])]);
        Finish;
        Exit;
      end;
    end; // if UData.CheckCliTimeStamp and (CliTimeStamp <> 0) then
    // user access management
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT userid, pw, rights, ipaddr FROM users');
        SQL.Add('WHERE login = :login');
        ParamByName('login').AsString := User;
        Open;
        if not Eof then
        begin
          UserID := FieldByName('userid').AsInteger;
          StoredPassword := FieldByName('pw').AsString;
          Rights := FieldByName('rights').AsInteger;
          StoredIPAddr := FieldByName('ipaddr').AsString;
        end else ErrorMsg := ' Unknown user.';
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // local login w/o password enabled?
    if UData.LocalLogin and
      ((IPAddr = '127.0.0.1') or (IPAddr = UData.ServerIPNo)) then // yes
      Accepted := (UserID <> 0)
    else
    begin
      // login restricted to stored IP address?
      if UData.CheckIPAddr then
      begin // yes
        Accepted := (UserID <> 0) and
                    (fvcsCryptCompare(Password, StoredPassword));
        if (not Accepted) and (ErrorMsg = '') then
          ErrorMsg := ' Invalid Password.';
        // check IP only if StoredIPAddr <> '0'
        if Accepted then
          Accepted := CheckIPMask(IPAddr, StoredIPAddr) or
                      (StoredIPAddr = '0');
      end // if UData.CheckIPAddr then begin
      else
      begin
        Accepted := (UserID <> 0) and
                       (fvcsCryptCompare(Password, StoredPassword));
        if (not Accepted) and (ErrorMsg = '') then
          ErrorMsg := ' Invalid Password.';
      end; // else if UData.CheckIPAddr then begin
    end; // else if UData.LocalLogin and (IPAddr = '127.0.0.0') then
    if Accepted then
    begin
      // create random transaction number
      Randomize;
      TANr := Round(Random * High(TANr));
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          // remove all old entries
          Close;
          SQL.Clear;
          SQL.Add('DELETE FROM transact');
          SQL.Add('WHERE accessid = :accessid');
          ParamByName('accessid').AsInteger := UserID;
          ExecSQL;
          // store user ID, transaction number & default rights
          SQL.Clear;
          SQL.Add('INSERT INTO transact');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('transact')]));
          SQL.Add(':accessid, null, :rights, :tanr,');
          SQL.Add(':faults, :expires)');
          // projectid = null
          ParamByName('accessid').AsInteger := UserID;
          ParamByName('rights').AsInteger := Rights;
          ParamByName('tanr').AsInteger := TANr;
          ParamByName('faults').AsInteger := 0;
          ParamByName('expires').AsDateTime := Now;
          ExecSQL;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
      // save statistic data
      Inc(UData.UserCount);
    end; // if Accepted then begin
    FResponseBuffer.Rewrite;
    // Clients Ini
    IniFile := TIniFile.Create(FIniFileName);
    try
      ForceGlobalIni := IniFile.ReadBool(GlobalUserData, ForceGlobalSettings,
                                                                         False);
    finally
      IniFile.Free;
    end;
    // User accepted ?
    FResponseBuffer.WriteFields(True, [Accepted]);
    if Accepted then
    begin
      LevelMsg := ' Access level: ' + IntToStr(Rights);
      case Rights of
        0: LevelMsg := LevelMsg + ' [Guest]';
        1: LevelMsg := LevelMsg + ' [read-only]';
        2: LevelMsg := LevelMsg + ' [read-write]';
        3: LevelMsg := LevelMsg + ' [Project Admin]';
        4: LevelMsg := LevelMsg + ' [Archive Admin]';
      end; // case Rights of
      FResponseBuffer.WriteFields(False, [UserID]);
      FResponseBuffer.WriteFields(False, [TANr]);
      FResponseBuffer.WriteFields(False, ['User accepted.' + LevelMsg]);
      FResponseBuffer.WriteFields(False, [GetSrvVersion(cServerId)]);
      FResponseBuffer.WriteFields(False, [GetSrvPortFromId(cServerId)]);
      FResponseBuffer.WriteFields(False, [ForceGlobalIni]);
      if UData.ShowServerMsg then
      begin
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                             ' [LOGIN] User accepted,' + LevelMsg + ' ' + User);
      end;
    end // if Accepted then begin
    else
    begin
      if ErrorMsg = '' then ErrorMsg := ' Invalid IPAddr.';
      FResponseBuffer.WriteFields(False, [-1]);
      FResponseBuffer.WriteFields(False, [-1]);
      FResponseBuffer.WriteFields(False, ['Access denied.' + ErrorMsg]);
      FResponseBuffer.WriteFields(False, [GetSrvVersion(cServerId)]);
      FResponseBuffer.WriteFields(False, [GetSrvPortFromId(cServerId)]);
      FResponseBuffer.WriteFields(False, ['0']);
      if UData.ShowServerMsg then
      begin
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                             ' [LOGIN] Access denied,' + ErrorMsg + ' ' + User);
      end; // if UData.ShowServerMsg then begin
    end; // else if Accepted then begin
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Server Logout
  Remove current transaction number assigned to this user
  (Administrators may use this object to log out any user)

        Record  Field
        ======  =====

  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectLOGOUT.Execute;
const
  RequestedRightsSelf = LevelNone;
  RequestedRightsAdmin = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  RequestedRights,
  UserID: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    if not FRequestBuffer.Eof then FRequestBuffer.Next;
    UserID := StrToInt(FRequestBuffer.Fields[0]);
    if UserID = AccessID then RequestedRights := RequestedRightsSelf
      else RequestedRights := RequestedRightsAdmin;
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;

    // remove transaction number
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('DELETE FROM transact');
        SQL.Add('WHERE accessid = :accessid');
        ParamByName('accessid').AsInteger := UserID;
        ExecSQL;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // server display
    if UData.ShowServerMsg then
    begin
      TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr + ' [LOGOUT]');
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Whoami (User information)

        Record  Field
        ======  =====

  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]User ID                - Integer
               [1]User name              - String(50)
               [2]Access level           - Integer
               [3]Access label           - String
               [4]Login expires?         - Boolean
               [5]Login time             - Double (TDateTime)
               [6]Login time out         - Double (TDateTime)
               [7]Stored User IPAddr     - String(50)
               [8]Current User IPAddr    - String
               [9]Server label           - String(255)
              [10]DB type                - String
              [11]Project access level   - Integer
              [12]Project access label   - String
              [13]Server's GMT difference- String

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectWHOAMI.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  ProjectRights,
  AccessID: Integer;
  DT: Double;
  ServerGMTStr,
  DBVersionInfo: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // global or not restricted function -> get access rights from Users
        Close;
        SQL.Clear;
        SQL.Add('SELECT users.rights, users.login,');
        SQL.Add('transact.tanr, transact.expires, users.ipaddr');
        SQL.Add('FROM transact, users');
        SQL.Add('WHERE transact.accessid = :accessid');
        SQL.Add('AND users.userid = transact.accessid');
        SQL.Add('AND users.deleted <> ''1''');
        ParamByName('accessid').AsInteger := AccessID;
        Open;
        if not Eof then
        begin
          if FieldByName('tanr').AsInteger = TANr
            then GrantedRights := FieldByName('rights').AsInteger;
          FResponseBuffer.Rewrite;
          FResponseBuffer.WriteFields(True, [AccessID]);
          FResponseBuffer.WriteFields(False, [FieldByName('login').AsString]);
          FResponseBuffer.WriteFields(False, [GrantedRights]);
          case GrantedRights of
            0: FResponseBuffer.WriteFields(False, ['Guest']);
            1: FResponseBuffer.WriteFields(False, ['read-only']);
            2: FResponseBuffer.WriteFields(False, ['read-write']);
            3: FResponseBuffer.WriteFields(False, ['Project administrator']);
            4: FResponseBuffer.WriteFields(False, ['Archive administrator']);
          end; // case GrantedRights of
          FResponseBuffer.WriteFields(False, [UData.LoginExpires]);
          DT := LocalDT2GMTDT(FieldByName('expires').AsDateTime);
          FResponseBuffer.WriteFields(False, [DT]);
          FResponseBuffer.WriteFields(False, [UData.LoginTimeOut]);
          FResponseBuffer.WriteFields(False, [FieldByName('ipaddr').AsString]);
          FResponseBuffer.WriteFields(False,
                                    [TClientWSocket(FORBDataPtr.Tag).PeerAddr]);
          FResponseBuffer.WriteFields(False, [UData.Serverlabel]);
        end // if not EoF then begin
        else
        begin
          FResultStatus := 403; // forbidden
          FResponseBuffer.Rewrite;
          FResponseBuffer.WriteFields(False, [Unknown]);
          Finish;
          Exit;
        end; // else if not EoF then begin
        Close;
        {$IFDEF UIBSRV} //-- PrY
        DBVersionInfo := 'Firebird (UIB) 1.2';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF UIBSRV}
        {$IFDEF ORACLESRV}
        //-- hdo
        DBVersionInfo := 'ORACLE Ver 1.1';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF ORACLESRV}
        {$IFDEF MSSQLSRV} //-- MG
        DBVersionInfo := 'MSSQL Ver 1.1';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF MSSQLSRV}
        {$IFDEF MYSQLSRV} //-- LB
        DBVersionInfo := 'MySQL Ver 1.1';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF MYSQLSRV}
        {$IFDEF ADOSRV} //-- PL
        DBVersionInfo := 'ADO Ver 1.0';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF ADOSRV}
        SQL.Clear;
        SQL.Add('SELECT rights, tanr');
        SQL.Add('FROM transact');
        SQL.Add('WHERE accessid = :accessid');
        ParamByName('accessid').AsInteger := AccessID;
        Open;
        if not Eof then
          ProjectRights := FieldByName('rights').AsInteger
        else
          ProjectRights := -1;
        Close;
        FResponseBuffer.WriteFields(False, [ProjectRights]);
        case ProjectRights of
         -1: FResponseBuffer.WriteFields(False, ['Default rights']);
          0: FResponseBuffer.WriteFields(False, ['Guest']);
          1: FResponseBuffer.WriteFields(False, ['read-only']);
          2: FResponseBuffer.WriteFields(False, ['read-write']);
          3: FResponseBuffer.WriteFields(False, ['Project administrator']);
          4: FResponseBuffer.WriteFields(False, ['Archive administrator']);
        end; // case ProjectRights of
      end; // with FQuery do begin
      ServerGMTStr := FloatToStr(UData.GMTDifference);
      if UData.GMTDifference >= 0 then
        ServerGMTStr := '(GMT+' + ServerGMTStr + ')'
      else
        ServerGMTStr := '(GMT' + ServerGMTStr + ')';
      FResponseBuffer.WriteFields(False, [ServerGMTStr]);
    finally
      FQuery.Free;
    end;
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
      if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get application server options & statistic information

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Connection Timeout     - Integer
               [1]Login Timeout          - Double (TDateTime)
               [2]Casually checkin       - Boolean
               [3]Write VCSLog           - Boolean
               [4]Check Client IP        - Boolean
               [5]Login expires          - Boolean
               [6]Write server log       - Boolean
               [7]Server log size (kB)   - Integer
               [8]Live backup path       - String
               [9]Last live backup       - Double (TDateTime)
              [10]Loc. login w/o passwrd - Boolean
              [11]ORB Object Count       - Integer
              [12]Auto login available   - Boolean
              [13]Auto login             - Boolean
              [14]Log access faults      - Boolean
              [15]Uptime                 - String(255)
              [16]RequestCount           - Integer
              [17]Archive drive          - String(255)
              [18]Avail. space/archive drive - String(255)
              [19]Total space/archive drive - String(255)
              [20]CheckInCount           - Integer
              [21]CheckOutCount          - Integer
              [22]NewFilesCount          - Integer
              [23]UserCount              - Integer
              [24]Transmitted Bytes      - String(255)
              [25]Received Bytes         - String(255)
              [26]GetFilesCount          - Integer

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_SERVER_OPTIONS.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  TANr,
  GrantedRights,
  AccessID: Integer;
  FreeAvailable,
  TotalSpace,
  TotalFree: Int64;
  DBPath: string;
  {$IFDEF MSSQLSRV}
  Size: DWORD;
  ComputerName: array [0..MAX_COMPUTERNAME_LENGTH +1] of Char;
  {$ENDIF MSSQLSRV}
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    FResponseBuffer.WriteFields(True, [UData.ConnectTimeOut]);
    FResponseBuffer.WriteFields(False, [UData.LoginTimeOut]);
    FResponseBuffer.WriteFields(False, [UData.CasuallyCheckin]);
    FResponseBuffer.WriteFields(False, [UData.WriteVCSLog]);
    FResponseBuffer.WriteFields(False, [UData.CheckIPAddr]);
    FResponseBuffer.WriteFields(False, [UData.LoginExpires]);
    FResponseBuffer.WriteFields(False, [UData.WriteServerLog]);
    FResponseBuffer.WriteFields(false, [GetServerLogFileSizeKByte]);
    FResponseBuffer.WriteFields(False, [UData.BackupPath]);
    FResponseBuffer.WriteFields(False, [UData.LastLiveBackup]);
    FResponseBuffer.WriteFields(False, [UData.LocalLogin]);
    FResponseBuffer.WriteFields(False, [FORBDataPtr.ORB.ObjectCount]);
    {$IFNDEF NTSERVICESRV}
    FResponseBuffer.WriteFields(False, [True]);
    FResponseBuffer.WriteFields(False, [UData.DBAutoLogon]);
    {$ELSE} // {$IFNDEF NTSERVICESRV}
    FResponseBuffer.WriteFields(False, [False]);
    FResponseBuffer.WriteFields(False, [True]);
    {$ENDIF ~NTSERVICESRV} // else {$IFNDEF NTSERVICESRV}
    FResponseBuffer.WriteFields(False, [UData.LogAccessFault]);
    // Statistic
    if GrantedRights = LevelAAdmin then
    begin
      // Information is only available to Admins
      FResponseBuffer.WriteFields(False, [UData.UpTimeStr]);
      FResponseBuffer.WriteFields(False, [UData.RequestCount]);
      // archive drive/path
      {$IFDEF ORACLESRV}
      DBPath := UData.DBPath;
      {$ENDIF ORACLESRV}
      {$IFDEF MYSQLSRV}
      DBPath := UData.DBPath.Database;
      {$ENDIF MYSQLSRV}
      {$IFDEF MSSQLSRV} //-- MG
      with TSrvQuery.Create(nil) do
      begin
        DatabaseName := UData.DBPath;
        with SQL do
        begin
          Clear;
          Add('select filename from sysfiles');
        end;
        try
          Open;
          if not Eof then
          begin
            DBPath := ExtractFilePath(FieldByName('filename').AsString);
            Size := SizeOf(ComputerName);
            GetComputerName(ComputerName, Size);
            if UpperCase(ComputerName) <> UpperCase(UData.ServerName) then
            begin
              DBPath[Pos(':', DBPath)] := '$';
              DBPath := '\\' + UData.ServerName + '\' + DBPath;
            end;
          end
          else
            DBPath := '';
        except
          DBPath := '';
        end;
        Close;
        Free;
      end;
      {$ENDIF MSSQLSRV}
      {$IFDEF ADOSRV} //-- PL
      DBPath := ''; // no inforomation in ADO connection for backup database, Searching ...
      {$ENDIF ADOSRV}
      FResponseBuffer.WriteFields(False, [LowerCase(ExtractFilePath(DBPath))]);
      // get free space for the archive drive
      try
        {  in some rare cases the function raises an exception on remote
           database servers?? - trap all exceptions for now  }
        {$IFDEF MSWINDOWS}
        if GetDiskFreeSpaceEx(PChar(DBPath), FreeAvailable, TotalSpace,
          @TotalFree) then
        begin
          FResponseBuffer.WriteFields(False, [Format('%n MB',
                                                 [(FreeAvailable / 1048576)])]);
          FResponseBuffer.WriteFields(False, [Format('%n MB',
                                                    [(TotalSpace / 1048576)])]);
        end else
        {$ENDIF MSWINDOWS}
        begin
          FResponseBuffer.WriteFields(False, ['?']);
          FResponseBuffer.WriteFields(False, ['?']);
        end;
      except
        FResponseBuffer.WriteFields(False, ['?']);
        FResponseBuffer.WriteFields(False, ['?']);
      end;
      FResponseBuffer.WriteFields(False, [UData.CheckinCount]);
      FResponseBuffer.WriteFields(False, [UData.CheckoutCount]);
      FResponseBuffer.WriteFields(False, [UData.NewFilesCount]);
      FResponseBuffer.WriteFields(False, [UData.UserCount]);
      FResponseBuffer.WriteFields(False, [Format('%n MB',
                                        [(UData.TransmittedBytes / 1048576)])]);
      FResponseBuffer.WriteFields(False, [Format('%n MB',
                                           [(UData.ReceivedBytes / 1048576)])]);
      FResponseBuffer.WriteFields(False, [UData.GetCount]);
    end // if GrantedRights = LevelAAdmin then
    else
    begin
      // Request by normal user
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
      FResponseBuffer.WriteFields(False, ['N/A']);
    end; // else if GrantedRights = LevelAAdmin then
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Set application server options

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Connection Timeout     - Integer
               [1]Login Timeout          - Double (TDateTime)
               [2]Casually checkin       - Boolean
               [3]Write VCSLog           - Boolean
               [4]Check Client IP        - Boolean
               [5]Login expires          - Boolean
               [6]Write server log       - Boolean
               [7]Live backup path       - String
               [8]Loc. login w/o passwrd - Boolean
               [9]Auto login             - Boolean
              [10]Log access faults      - Boolean

   Response: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectSET_SERVER_OPTIONS.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID: Integer;
  LogMsg: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    UData.ConnectTimeOut := StrToInt(FRequestBuffer.Fields[0]);
    LogMsg := 'ConnectTimeOut: ' + FRequestBuffer.Fields[0] + '/ ';
    UData.LoginTimeOut := _StrToFloat(FRequestBuffer.Fields[1]);
    LogMsg := LogMsg + 'LoginTimeOut: ' + FRequestBuffer.Fields[1] + '/ ';
    UData.CasuallyCheckin := (FRequestBuffer.Fields[2] = '1');
    LogMsg := LogMsg + 'CasuallyCheckin: ' + FRequestBuffer.Fields[2] + '/ ';
    UData.WriteVCSLog := (FRequestBuffer.Fields[3] = '1');
    LogMsg := LogMsg + 'WriteVCSLog: ' + FRequestBuffer.Fields[3] + '/ ';
    UData.CheckIPAddr := (FRequestBuffer.Fields[4] = '1');
    LogMsg := LogMsg + 'CheckIPAddr: ' + FRequestBuffer.Fields[4] + '/ ';
    UData.LoginExpires := (FRequestBuffer.Fields[5] = '1');
    LogMsg := LogMsg + 'LoginExpires: ' + FRequestBuffer.Fields[5] + '/ ';
    UData.WriteServerLog := (FRequestBuffer.Fields[6] = '1');
    LogMsg := LogMsg + 'WriteServerLog: ' + FRequestBuffer.Fields[6] + '/ ';
    UData.BackupPath := FRequestBuffer.Fields[7];
    LogMsg := LogMsg + 'BackupPath: ' + FRequestBuffer.Fields[7] + '/ ';
    UData.LocalLogin := (FRequestBuffer.Fields[8] = '1');
    LogMsg := LogMsg + 'LocalLogin: ' + FRequestBuffer.Fields[8] + '/ ';
    {$IFNDEF NTSERVICESRV}
    UData.DBAutoLogon := (FRequestBuffer.Fields[9] = '1');
    LogMsg := LogMsg + 'DBAutoLogon: ' + FRequestBuffer.Fields[9] + '/ ';
    {$ENDIF ~NTSERVICESRV}
    UData.LogAccessFault := (FRequestBuffer.Fields[10] = '1');
    LogMsg := LogMsg + 'LogAccessFault: ' + FRequestBuffer.Fields[10];
    // Server settings changed -> set 'settings need to be saved' flag
    UData.SettingsChanged := True;

    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // prepare server log file information
        Close;
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := AccessID;
        Open;
        if not Eof then
          LogMsg := 'Server administration: Changed by Admin <' +
            FieldByName('login').AsString + '>: ' + LogMsg;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    TriggerDisplay(LogMsg);
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get access rights related to single projects
  Default access rights (defined as a basic value in Users) may be overwritten
  by access rights related to a single project.
  So a user may be defined as read-only, but get write access to a single
  project (or vise versa).

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]User ID                - Integer
               [1]Project ID             - Integer
               [2]Access level           - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROJECT_RIGHTS.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  Fld,
  AccessID: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT userid, projectid, rights');
        SQL.Add('FROM pjusers');
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
          Next;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Set access rights related to single projects
  Default access rights (defined as a basic value in Users) may be overwritten
  by access rights related to a single project.
  So a user may be defined as read-only, but get write access to a single
  project (or vise versa).
  If Access level = 0 remove the entry related to this user & project
    -> default rights (as defined in Users) are valid.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer
               [1]Project ID             - Integer
               [2]Access level           - Integer

  Response: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectSET_PROJECT_RIGHTS.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID,
  UserID,
  ProjectID,
  AccessLevel: Integer;
  LogMsg: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    UserID := StrToInt(FRequestBuffer.Fields[0]);
    ProjectID := StrToInt(FRequestBuffer.Fields[1]);
    AccessLevel := StrToInt(FRequestBuffer.Fields[2]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // remove old entry -> default rights in this project
        Close;
        SQL.Clear;
        SQL.Add('DELETE FROM pjusers');
        SQL.Add('WHERE userid = :userid');
        SQL.Add('AND projectid = :projectid');
        ParamByName('userid').AsInteger := UserID;
        ParamByName('projectid').AsInteger := ProjectID;
        ExecSQL;
        // store new project right
        if AccessLevel > 0 then
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO pjusers');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjusers')]));
          SQL.Add(':userid, :projectid, :rights)');
          ParamByName('userid').AsInteger := UserID;
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('rights').AsInteger := AccessLevel;
          ExecSQL;
        end; // if AccessLevel > 0 then begin
        // Write information to the servers log file
        LogMsg := 'User administration: Project right [';
        case AccessLevel of
          0: LogMsg := LogMsg + 'default';
          1: LogMsg := LogMsg + 'read-only';
          2: LogMsg := LogMsg + 'read-write';
          3: LogMsg := LogMsg + 'Project administrator';
        end;
        LogMsg := LogMsg + '] assigned to user <';
        Close;
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := UserID;
        Open;
        if not Eof then
          LogMsg := LogMsg + FieldByName('login').AsString + '> in project <';
        Close;
        SQL.Clear;
        SQL.Add('SELECT name FROM projects');
        SQL.Add('WHERE projectid = :projectid');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        if not Eof then
          LogMsg := LogMsg + FieldByName('name').AsString + '> by Admin <';
        Close;
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := AccessID;
        Open;
        if not Eof then
          LogMsg := LogMsg + FieldByName('login').AsString + '>';
        Close;
        TriggerDisplay(LogMsg);
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get a list from all users / all logged in users / Admin use

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]User ID                - Integer
               [1]User name              - String(50)
               [2]User access level      - Integer
               [3]User access ID         - Integer
                  (logged in -> User access ID = User ID,
                   not logged in -> User access ID = '')
               [4]User deleted?          - String(1)
                  (0 = User not deleted, 1 = User deleted)
               [5]User description       - String
               [6]User IPAddr            - String(50)

            1: [next user...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_USERLIST.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  Fld,
  AccessID: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT users.userid, users.login,');
        SQL.Add('users.rights, transact.accessid, users.deleted,');
        SQL.Add('users.description, users.ipaddr');
        SQL.Add('FROM users');
        SQL.Add('LEFT OUTER JOIN transact ON users.userid = transact.accessid');
        SQL.Add('ORDER BY users.login');
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
          Next;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get a list from all users / common use

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]User ID                - Integer
               [1]User name              - String(50)
               [2]User access level      - Integer
               [3]User deleted           - String(1)
                  ('1' = deleted, '0' = not deleted)

            1: [next user...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_USERS.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  Fld,
  AccessID: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT userid, login, rights, deleted');
        SQL.Add('FROM users');
        SQL.Add('ORDER BY users.login');
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
          Next;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Add a new user / change settings for an existing user

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer
                  (0 = add a new user,
                   x = change user ID x)
               [1]User name              - String(50)
               [2]User password          - String(50)
                  (do not touch the password if this string is blank)
               [3]User access level      - Integer
               [4]User description       - String
               [5]User IPAddr            - String(50)

  Response: 0: [0]Request accepted?      - Boolean
               [1]Error message          - String
                  (if not accepted)
               [2]new User ID            - Integer
                  (if this is a new user, otherwise return old ID)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_USER.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID,
  UserID,
  NewUserID,
  UserLevel: Integer;
  UserName,
  UserPW,
  UserIPAddr,
  UserDescr,
  LogMsg: string;
  IsNewUser,
  IsNewName: Boolean;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    UserID := StrToInt(FRequestBuffer.Fields[0]);
    UserName := FRequestBuffer.Fields[1];
    UserPW := FRequestBuffer.Fields[2];
    UserLevel := StrToInt(FRequestBuffer.Fields[3]);
    UserDescr := FRequestBuffer.Fields[4];
    UserIPAddr := FRequestBuffer.Fields[5];
    if UserIPAddr = '' then UserIPAddr := '0';

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // prepare server log file information
        LogMsg := 'user <' + UserName + '>, IP <' + UserIPAddr + '> as [';
        case UserLevel of
          0: LogMsg := LogMsg + 'Guest';
          1: LogMsg := LogMsg + 'read-only';
          2: LogMsg := LogMsg + 'read-write';
          3: LogMsg := LogMsg + 'Project administrator';
          4: LogMsg := LogMsg + 'Archive administrator';
        end;
        LogMsg := LogMsg + '] by Admin <';
        Close;
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := AccessID;
        Open;
        if not Eof then
          LogMsg := LogMsg + FieldByName('login').AsString + '>';
        Close;
        // add new or change existing user?
        SQL.Clear;
        SQL.Add('SELECT userid FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := UserID;
        Open;
        IsNewUser := Eof;
        Close;
        if IsNewUser then
          LogMsg := 'User administration: New ' + LogMsg
        else
          LogMsg := 'User administration: Changed ' + LogMsg;
      end; // with FQuery do begin
      if IsNewUser then
      begin
        // check if this name is already in use
        with FQuery do
        begin
          Close;
          SQL.Clear;
          SQL.Add('SELECT userid FROM users');
          SQL.Add('WHERE login = :login');
          ParamByName('login').AsString := UserName;
          Open;
          IsNewName := Eof;
          Close;
        end; // with FQuery do begin
        if IsNewName then
        begin
          // ok, add the new user
          with FQuery do
          begin
            Close;
            SQL.Clear;
            SQL.Add('INSERT INTO users');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('users')]));            
            SQL.Add(':login, :pw, :ipaddr, :rights, :deleted, :description)');
            ParamByName('login').AsString := UserName;
            ParamByName('pw').AsString := UserPW;
            ParamByName('ipaddr').AsString := UserIPAddr;
            ParamByName('rights').AsInteger := UserLevel;
            ParamByName('deleted').AsString := '0';
            ParamByName('description').AsString := UserDescr;
            ExecSQL;
            // get the new user ID
            SQL.Clear;
            SQL.Add('SELECT userid FROM users');
            SQL.Add('WHERE login = :login');
            ParamByName('login').AsString := UserName;
            Open;
            NewUserID := FieldByName('userid').AsInteger;
            Close;
            // return true
            FResponseBuffer.WriteFields(True, [True]);
            // return blank error message
            FResponseBuffer.WriteFields(False, [True]);
            // return the new user ID
            FResponseBuffer.WriteFields(False, [NewUserID]);
          end; // with FQuery do begin
        end // if IsNewName then begin
        else
        begin
          // return false
          FResponseBuffer.WriteFields(True, [False]);
          // return error message
          FResponseBuffer.WriteFields(False, ['<' + UserName +
                                             '> This name is already in use.']);
          // return zero
          FResponseBuffer.WriteFields(False, [0]);
        end;
      end // if IsNewUser then begin
      else
      begin
        // change existing user
        with FQuery do
        begin
          Close;
          SQL.Clear;
          SQL.Add('UPDATE users');
          if UserPW <> '' then
            SQL.Add('SET login = :login, pw = :pw, ipaddr = :ipaddr,')
          else
            SQL.Add('SET login = :login, ipaddr = :ipaddr,');
          SQL.Add('rights = :rights, deleted = :deleted,');
          SQL.Add('description = :description');
          SQL.Add('WHERE userid = :userid');
          ParamByName('login').AsString := UserName;
          if UserPW <> '' then
            ParamByName('pw').AsString := UserPW;
          ParamByName('rights').AsInteger := UserLevel;
          ParamByName('ipaddr').AsString := UserIPAddr;
          ParamByName('description').AsString := UserDescr;
          ParamByName('userid').AsInteger := UserID;
          ParamByName('deleted').AsString := '0'; //-- hdo
          ExecSQL;
        end; // with FQuery do begin
        // return false
        FResponseBuffer.WriteFields(True, [True]);
        // return blank error message
        FResponseBuffer.WriteFields(False, [True]);
        // return the old user ID
        FResponseBuffer.WriteFields(False, [UserID]);
      end; // else if IsNewUser then begin
    finally
      FQuery.Free;
    end;
    TriggerDisplay(LogMsg);
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Change password
  Every user may change his own password, Archive Admins may change all
  passwords (if they know the old password).

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer
               [1]Old password           - String(50)
               [2]New password           - String(50)

  Response: 0: [0]new password accepted? - Boolean
               [1]Error message          - String
                  (if not accepted)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectCHANGE_PASSWORD.Execute;
const
  RequestedRightsSelf = LevelNone;
  RequestedRightsAll = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  RequestedRights,
  AccessID,
  UserID: Integer;
  OldPassword,
  NewPassword: string;
  Accepted: Boolean;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    if not FRequestBuffer.Eof then FRequestBuffer.Next;
    UserID := StrToInt(FRequestBuffer.Fields[0]);
    UData := PUserDataRecord(FUserData);
    if (UserID = AccessID) then RequestedRights := RequestedRightsSelf
      else RequestedRights := RequestedRightsAll;
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;

    OldPassword := FRequestBuffer.Fields[1];
    NewPassword := FRequestBuffer.Fields[2];
    Accepted := False;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check the old password
        Close;
        SQL.Clear;
        SQL.Add('SELECT login, pw FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := UserID;
        Open;
        if not Eof then
          Accepted := fvcsCryptCompare(OldPassword, FieldByName('pw').AsString);
        Close;
        if Accepted then
        begin
          // store new password
          SQL.Clear;
          SQL.Add('UPDATE users');
          SQL.Add('SET pw = :pw');
          SQL.Add('WHERE userid = :userid');
          ParamByName('userid').AsInteger := UserID;
          ParamByName('pw').AsString := NewPassword;
          ExecSQL;
          // old password accepted, return true
          FResponseBuffer.WriteFields(True, [True]);
          // return blank error message
          FResponseBuffer.WriteFields(False, ['']);
        end // if Accepted then begin
        else
        begin
          // old password not accepted, return false
          FResponseBuffer.WriteFields(True, [False]);
          // return error message
          FResponseBuffer.WriteFields(False,
                                         ['Invalid password or unknown user.']);
        end; // else if Accepted then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Remove a user (set deleted flag)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_USER.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID,
  UserID: Integer;
  UserName,
  LogMsg: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    UserID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // get user login name
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := UserID;
        Open;
        if not Eof then UserName := FieldByName('login').AsString;
        Close;
        // mark user as 'deleted'
        SQL.Clear;
        SQL.Add('UPDATE users');
        SQL.Add('SET login = :username, deleted = ''1''');
        SQL.Add('WHERE userid = :userid');
        ParamByName('username').AsString := UserName + ' (Deleted)';
        ParamByName('userid').AsInteger := UserID;
        ExecSQL;
        // prepare server log file information
        LogMsg := 'User administration: <';
        Close;
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := UserID;
        Open;
        if not Eof then
          LogMsg := LogMsg + FieldByName('login').AsString +
            '> deleted by Admin <';
        Close;
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := AccessID;
        Open;
        if not Eof then
          LogMsg := LogMsg + FieldByName('login').AsString + '>';
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    TriggerDisplay(LogMsg);
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get server time & server time zone information
  Clients may use this object to synchronize their local time/date.
  (equal time/date on clients is necessary because FreeVCS uses file
   timestamps to get latest versions)
  Server time is also (a part of) the key to encrypt the passwords sent by
  the clients in the login procedure.
  Due to this the object MUST return the local date from the machine holding the
  application server, whether the archive is located at the same machine or not!

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Server time            - Double (TDateTime format expected)
               [1]local GMT difference   - Double (TDateTime format expected)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_SERVER_TIME.Execute;
var
  UData: PUserDataRecord;
begin
  {  this is the first object called in the login procedure to get the time
     related part of the encryption key.
     For this reason requested access level = none.  }
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    UData := PUserDataRecord(FUserData);
    FResponseBuffer.Rewrite;
    {  since the timestamp is used for password encryption we must use
       local dates at this place  }
    FResponseBuffer.WriteFields(True, [Now]);
    FResponseBuffer.WriteFields(False, [UData.GMTDifference]);
    FResponseBuffer.WriteFields(False, [LocalDT2GMTDT(Now)]);
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get server's log file

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]File found             - Boolean
               [0]Server log file        - Stream

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_SERVER_LOG.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  TANr,
  AccessID,
  GrantedRights: Integer;
  FS: TFileStream;
  nAutoExpand: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;

    if FileExists(GetJvcsServerLogFile) then
    begin
      // file found, return true
      FResponseBuffer.WriteFields(True, [True]);
      nAutoExpand := FResponseBuffer.AutoExpand;
      // copy server log file
      FS := TFileStream.Create( GetJvcsServerLogFile
                              , fmOpenRead + fmShareDenyNone
                              );
      try
        FS.Seek(0, 0);
        // Adjust Response memory (min 256)
        FResponseBuffer.AutoExpand := FS.Size + (FS.Size div 4) + 256;
        FResponseBuffer.WriteStreamField(False, mwBlob, FS);
      finally
        FS.Free;
        FResponseBuffer.AutoExpand := nAutoExpand;
      end;
    end // if FileExists(GetJvcsServerLogFile) then begin
    else
      FResponseBuffer.WriteFields(True, [False]);

    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Clear server's log file

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectCLEAR_SERVER_LOG.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights: Integer;
  LogMsg: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    if FileExists(GetJvcsServerLogFile) then
      DeleteFile(GetJvcsServerLogFile);

    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // prepare server log file information
        LogMsg := 'Server administration: Server log cleared by Admin <';
        Close;
        SQL.Clear;
        SQL.Add('SELECT login FROM users');
        SQL.Add('WHERE userid = :userid');
        ParamByName('userid').AsInteger := AccessID;
        Open;
        if not Eof then
          LogMsg := LogMsg + FieldByName('login').AsString + '>';
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    TriggerDisplay(LogMsg);
    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get archive time stamp (last write access to the archive)
  Archive time stamp is set by the server behind every write access.
  Client may use this function to decide if he must update the local view.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Archive time stamp     - Double (TDateTime)
                 (return 0 if not supported)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_ARCHIVE_TSTAMP.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  TANr,
  AccessID,
  GrantedRights: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    FResponseBuffer.WriteFields(True, [UData.ArchiveTimeStamp]);
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Get project ID: user enters a new project.

  Server object has two functions:
    - Get project's ID by name
    - Check if there is any project related access level defined for this user.
      In this case:
      - The default access level (given by login) will be overwritten by the
        project related access level and the user will get a new transaction
        number.
      - Old transaction number will be cleared.
      - Login time out will be resetted.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project name           - String

  Response: 0: [0]Project ID             - Integer
                  (return 0 if the project was not found)
               [1]Project deleted?        - String(1)
                  ('1' = deleted, '0' = not deleted)
               [2]Transaction number      - Integer
               [3]Access level            - Integer

              1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROJECT_ID.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  ProjectAccessLevel: Integer;
  Project: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        // get default rights from Users
        SQL.Clear;
        SQL.Add('SELECT users.rights, transact.tanr');
        SQL.Add('FROM transact, users');
        SQL.Add('WHERE transact.accessid = :accessid');
        SQL.Add('AND users.userid = transact.accessid');
        SQL.Add('AND users.deleted <> ''1''');
        ParamByName('accessid').AsInteger := AccessID;
        Open;
        if not Eof then
          if FieldByName('tanr').AsInteger = TANr
            then GrantedRights := FieldByName('rights').AsInteger;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    Project := AnsiLowerCase(FRequestBuffer.Fields[0]);
    ProjectID := 0;
    ProjectAccessLevel := 0;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT projects.projectid, projects.deleted');
        SQL.Add('FROM projects');
        SQL.Add('WHERE projects.name = :project');
        ParamByName('project').AsString := Project;
        Open;
        if not Eof then
        begin
          // return project ID & deleted flag
          ProjectID := FieldByName('projectid').AsInteger;
          FResponseBuffer.WriteFields(True, [ProjectID]);
          FResponseBuffer.WriteFields(False, [FieldByName('deleted').AsString]);
        end else
        begin
          // unknown project -> return zero
          FResponseBuffer.WriteFields(True, [0]);
          FResponseBuffer.WriteFields(False, ['0']);
        end;
        Close;
        if ProjectID <> 0 then
        begin
          // known project
          // check for project related access level
          Close;
          SQL.Clear;
          SQL.Add('SELECT rights FROM pjusers');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND userid = :userid');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('userid').AsInteger := AccessID;
          Open;
          if not Eof then // project related access level is defined
            ProjectAccessLevel := FieldByName('rights').AsInteger;
          Close;
          // new transact record
          if ProjectAccessLevel = 0 then // assign default rights
            ProjectAccessLevel := GrantedRights;
          // create random transaction number
          Randomize;
          TANr := Round(Random * High(TANr));
          // remove all old entries
          SQL.Clear;
          SQL.Add('DELETE FROM transact');
          SQL.Add('WHERE accessid = :accessid');
          ParamByName('accessid').AsInteger := AccessID;
          ExecSQL;
          // store user ID, transaction number & new rights
          SQL.Clear;
          SQL.Add('INSERT INTO transact');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('transact')]));
          SQL.Add(':accessid, :projectid, :rights, :tanr,');
          SQL.Add(':faults, :expires)');
          ParamByName('accessid').AsInteger := AccessID;
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('rights').AsInteger := ProjectAccessLevel;
          ParamByName('tanr').AsInteger := TANr;
          ParamByName('faults').AsInteger := 0;
          ParamByName('expires').AsDateTime := Now;
          ExecSQL;
          if UData.ShowServerMsg then
            TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                                             ' Connect to project: ' + Project);
        end; // if ProjectID <> 0 then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // return (new) transaction number & rights
    FResponseBuffer.WriteFields(False, [TANr]);
    FResponseBuffer.WriteFields(False, [ProjectAccessLevel]);
    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Get current right: returns the access level for a given project ID and a
                     given user ID.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer

  Response: 0: [0]Project Right          - Integer

              1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROJECT_RIGHT.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    ProjectID := StrToInt(FRequestBuffer.Fields[0]);
    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        // get default rights from Users
        SQL.Clear;
        SQL.Add('SELECT rights FROM pjusers');
        SQL.Add('WHERE projectid = :projectid');
        SQL.Add('AND userid = :userid');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('userid').AsInteger := AccessID;
        Open;
        if not Eof then
          FResponseBuffer.WriteFields(True, [FieldByName('rights').AsInteger])
        else
          FResponseBuffer.WriteFields(True, [GrantedRights]);
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Search for modules in the version archive - wildcard (*, ?) support

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Mask string            - String(255)

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Path                   - String(255)
               [3]Assigned to project    - String(50)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectSEARCH_MODULES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery,
  FQuery2: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
  MaskStr,
  MaskPath,
  MaskName,
  MaskExt,
  ModulePath,
  ModuleName,
  ModuleExt,
  SQLParam: string;
  Match: Boolean;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    MaskStr := AnsiLowerCase(FRequestBuffer.Fields[0]);
    MaskPath := ExtractFilePath(MaskStr);
    MaskName := ChangeFileExt(ExtractFileName(MaskStr), '');
    if MaskName = '*' then
      MaskName := '';
    MaskExt := ExtractFileExt(MaskStr);
    if (MaskExt <> '') and (MaskExt[1] = '.') then
      MaskExt := Copy(MaskExt, 2, Length(MaskExt)-1);
    if MaskExt = '*' then
      MaskExt := '';

    SQLParam := 'WHERE (modules.moduleid IN (';

    FResponseBuffer.Rewrite;
    // get all module ID's and names in the archive
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT moduleid, name, path');
        SQL.Add('FROM modules');
        Open;
        while not Eof do
        begin
          // compare the module name to our mask string
          ModuleName := AnsiLowerCase(FieldByName('name').AsString);
          ModulePath := AnsiLowerCase(FieldByName('path').AsString);

          // CS, 11.01.2005: Use StrMatches function from JCL
          // maybe there is a better way to solve this, but it works
          if MaskPath<>'' then
            Match := StrMatches(MaskPath, ModulePath)
          else
            Match := True;
          if Match and (MaskName <> '') then
            Match := StrMatches(MaskName, ChangeFileExt(ExtractFileName(ModuleName), ''));
          if Match and (MaskExt <> '') then
          begin
            ModuleExt := ExtractFileExt(ModuleName);
            if (ModuleExt <> '') and (ModuleExt[1] = '.') then
              ModuleExt:= Copy(ModuleExt, 2, Length(ModuleExt)-1);
            Match := StrMatches(MaskExt, ModuleExt);
          end;
          
          // module name matches mask string?
          if Match then
          begin
            FQuery2 := TSrvQuery.Create(Self);
            try
              FQuery2.DatabaseName := UData.DBPath;
              {(* at least one module matches our search string *)
              ModulesFound := true;
              (* insert module ID into SQL string *)
              SQLParam := SQLParam + ' ' +
                                IntToStr(Integer(FieldByName('moduleid'))) + ',';}
              // yes, get information for this module
              FQuery2.Close;
              FQuery2.SQL.Clear;
              FQuery2.SQL.Add('SELECT modules.path, projects.name');
              FQuery2.SQL.Add('FROM modules, projects, pjmodule');
              FQuery2.SQL.Add('WHERE modules.moduleid = :moduleid');
              FQuery2.SQL.Add('AND pjmodule.moduleid = modules.moduleid');
              FQuery2.SQL.Add('AND projects.projectid = pjmodule.projectid');
              FQuery2.ParamByName('moduleid').AsInteger := FieldByName('moduleid').AsInteger;
              FQuery2.Open;
              while not FQuery2.Eof do
              begin
                // return module ID & name
                FResponseBuffer.WriteFields(True, [FieldByName('moduleid').AsInteger]);
                FResponseBuffer.WriteFields(False, [ModuleName]);
                // Copy the record to the response
                for Fld := 0 to FQuery2.FieldCount - 1 do
                  FResponseBuffer.WriteFields(False, [FQuery2.Fields[Fld].AsString]);
                FQuery2.Next;
              end; // while not FQuery2.EoF do begin
              FQuery2.Close;
            finally
              FQuery2.Free;
            end;
          end; // if Match then begin
          Next; // FQuery
        end; // while not EoF do begin
        Close; // FQuery
        // any modules matches our search string?
        {if ModulesFound then
        begin
          (* remove the last ',' *)
          if SQLParam[Length(SQLParam)] = ',' then
            System.Delete(SQLParam, Length(SQLParam), 1);
          (* complete SQL string *)
          SQLParam := SQLParam + '))';
          (* get more information for all these modules *)
          Close;
          SQL.Clear;
          SQL.Add('SELECT Modules.moduleid, Modules.name,');
          SQL.Add('Modules.path, Projects.name');
          SQL.Add('FROM Modules, Projects, PJModule');
          SQL.Add(SQLParam);
          SQL.Add('AND PJModule.moduleid = Modules.moduleid');
          SQL.Add('AND Projects.projectid = PJModule.projectid');}
          {Open;
          while not EoF do
          begin
            (* Copy the record to the response *)
            for Fld := 0 to FQuery2.FieldCount - 1 do
              FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            Next;
          end; // while not FQuery2.EoF do begin
          Close;
        end; // if ModulesFound then begin}
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Get a list of all 'deserted' modules (modules without a parent project)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Module path            - String(255)

            1: [next record...]
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_DESERTED_MODULES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
  slSqlIn, slSqlJoin: TStringList;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;

    slSqlIn := TStringList.Create;
    slSqlJoin := TStringList.Create;
    try
      // SQL, if DBMS supports subselect
      slSqlIn.Add('SELECT modules.moduleid,modules.name,modules.path FROM modules');
      slSqlIn.Add('WHERE modules.moduleid NOT IN (SELECT pjmodule.moduleid FROM pjmodule)');
      // SQL, if DBMS needs Join workaround
      slSqlJoin.Add('SELECT modules.moduleid,modules.name,modules.path FROM modules');
      slSqlJoin.Add('LEFT OUTER JOIN pjmodule ON modules.moduleid=pjmodule.moduleid');
      slSqlJoin.Add('WHERE pjmodule.moduleid IS NULL');

      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          Close;
          {$IFNDEF MYSQLSRV}
              SQL.Assign(slSqlIn);
          {$ELSE}
              SQL.Assign(slSqlJoin);
          {$ENDIF ~MYSQLSRV}
          Open;
          // Step 3: return only modules w/o project link
          while not Eof do
          begin
            for Fld := 0 to FieldCount - 1 do
              FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            Next;
          end; // while not EoF do begin
          Close;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
      FResultStatus := 200; // OK
      {$IFDEF DEBUG}
      ExitDebugMessage;
      {$ENDIF DEBUG}
    finally
      slSqlIn.Free;
      slSqlJoin.Free;
    end;
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Get revision count & space for all projects (listed by project)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Project name           - String(50)
               [1]No. of revisions       - Integer
               [2]Sum of module size     - Integer
               [3]Sum of comp. module size - Integer
               [4]Last access time       - Double (TDateTime)

            1: [Next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_SPACE_BY_PROJECTS.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights: Integer;
  Fld: Integer;
  DT: Double;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT projects.name, COUNT(*), SUM(blobs.origsize),');
        SQL.Add('SUM(blobs.compsize), MAX(modules.tstamp) AS maxtstamp');
        SQL.Add('FROM pjmodule, modules, projects, blobs, revision');
        SQL.Add('WHERE pjmodule.projectid = projects.projectid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        SQL.Add('GROUP BY projects.name');
        Open;
        while not Eof do
        begin
          { Copy all fields from the record to the response }
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   {$IFDEF MYSQLSRV}
                   if FieldByName('maxtstamp') is TStringField then
                     {$IFDEF ZEOS54}
                     DT := SqlDateToDateTime(FieldByName('maxtstamp').AsString)
                     {$ELSE}
                     DT := AnsiSQLDateToDateTime(FieldByName('maxtstamp').AsString)
                     {$ENDIF ZEOS54}
                   else
                   {$ENDIF MYSQLSRV}
                   DT := FieldByName('maxtstamp').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FieldCount - 1 do begin
          Next;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Get revision count & space for all modules from one project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer

  Response: 0: [0]Module name            - String(50)
               [1]No. of revisions       - Integer
               [2]Sum of module size     - Integer
               [3]Sum of comp. module size - Integer
               [4]Last access time       - Double (TDateTime)

            1: [Next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_SPACE_BY_MODULES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  ProjectID: Integer;
  DT: Double;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetProjectRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    ProjectID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.name, COUNT(*),');
        SQL.Add('SUM(blobs.origsize), SUM(blobs.compsize),');
        SQL.Add('MAX(modules.tstamp) AS maxtstamp');
        SQL.Add('FROM projects, pjmodule, modules, blobs, revision');
        SQL.Add('WHERE projects.projectid = :projectid');
        SQL.Add('AND projects.deleted = ''0''');
        SQL.Add('AND pjmodule.projectid = projects.projectid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        SQL.Add('GROUP BY modules.name');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        while not Eof do
        begin
          { Copy all fields from the record to the response }
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   {$IFDEF MYSQLSRV}
                   if FieldByName('maxtstamp') is TStringField then
                     {$IFDEF ZEOS54}
                     DT := SqlDateToDateTime(FieldByName('maxtstamp').AsString)
                     {$ELSE}
                     DT := AnsiSQLDateToDateTime(FieldByName('maxtstamp').AsString)
                     {$ENDIF ZEOS54}
                   else
                   {$ENDIF MYSQLSRV}
                   DT := FieldByName('maxtstamp').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FieldCount - 1 do begin
          Next;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Live backup (MSSQL) - backup the version archive database //-- MG
    - the database is archived to the backup folder and given the extension
      'dat'
    - already existing backup database in the target folder is renamed with the
      extension '~dat'
    - after a successful backup operation the server will remove the '~dat'
      backup database

  Live backup supported by UIB, MySQL, MSSQL servers - for now

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Target folder          - String

  Response: 0: [0]Backup success         - Boolean
               [1]Error message          - String
                  (if success = false)

            1: [0]Table name             - String
               [1]Table size             - Integer
                  (includes all files assigned to this table)

            2: [Next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectLIVE_BACKUP.Execute;
const
  RequestedRights = LevelAAdmin;
{$IFDEF DBBACKUPSUPPORT} //-- VR
var
  UData: PUserDataRecord;

  {$IFDEF UIBSRV} //-- PrY
  UIBBackup: TUIBBackup;
  BackupFileName: string;
  TANr,
  AccessID,
  GrantedRights: Integer;
  {$ENDIF UIBSRV}

  {$IFDEF MSSQLSRV}
  TANr,
  AccessID,
  GrantedRights,
  BackupSize: Integer;
  F: TSearchRec;
  BackupFileName: string;
  {$ENDIF MSSQLSRV}

  {$IFDEF MYSQLSRV}
  TANr,
  AccessID,
  GrantedRights: Integer;
  BackupFileName: string;
  {$ENDIF MYSQLSRV}

{$ENDIF DBBACKUPSUPPORT}

{$IFDEF MYSQLSRV}
function DoubleSlash(S:string):string;
var
  I: Integer;
begin
  Result:='';
  for I := 1 to Length(S) do
    if S[I] <> '\' then
      Result := Result + S[I]
    else
      Result := Result + S[I] + S[I];
end;

function RemoteDirectoryExists(sDir:string):Boolean;
begin
  with TSrvQuery.Create(nil) do
  begin
  DatabaseName := UData.DBPath;
  try
    SQL.Text:=DoubleSlash('SELECT 1 INTO OUTFILE "' + sDir + 'testdir" from users');
// three answers possible:
//   OK File created , Error File exists : directory exists
//   Error can't create: directory does not exist
    try
      Open;
  // three answers possible:
  //   OK File created, no return : directory exists
  //   Error File exists : directory exists
  //   Error can't create: directory does not exist
      Result := True;
    except
      on E:EDataBaseError do
        Result:=(Pos('Errcode',E.Message)<=0);
    end;
  finally
    Free;
  end;
  end;
end;
{$ENDIF MYSQLSRV}

begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  {$IFNDEF DBBACKUPSUPPORT} //-- VR
  {  Live backup only supported by UIB, MySQL, MSSQL servers
     - for now  }
  FResultStatus := 200;
  FResponseBuffer.Rewrite;
  // return false
  FResponseBuffer.WriteFields(True, [False]);
  // return error message
  FResponseBuffer.WriteFields(False,
          ['You are connected to an ' + c_DlgCaption + '.' + #10#13 +
           'This server does not provide client controlled backup functions.']);
  {$IFDEF DEBUG}
  ExitDebugMessage;
  {$ENDIF DEBUG}
  {$ELSE} // {$IFNDEF DBBACKUPSUPPORT}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    UData.BackupPath := FRequestBuffer.Fields[0];

    FResponseBuffer.Rewrite;
    // backup folder defined?
    {$IFDEF MYSQLSRV}
    if (UData.BackupPath <> '') and
       RemoteDirectoryExists(UData.BackupPath) then
    {$ELSE}
    if (UData.BackupPath <> '') and
       DirectoryExists(UData.BackupPath) then
    {$ENDIF MYSQLSRV}
    begin
      // yes, return success message
      FResponseBuffer.WriteFields(True, [True]);
      FResponseBuffer.WriteFields(False, ['']);

      // first back up the servers ini file
      {??? RequestBroker = nil
      CopyFile(PChar(RequestBroker.IniFileName), PChar(UData.BackupPath +
        ChangeFileExt(ExtractFileName(RequestBroker.IniFileName), '.~ini')),
        false);}

      {$IFDEF UIBSRV} //-- PrY
      // create the Firebird Backup Service component, the rest is simple
      BackupSize := 0;
      UIBBackup := TUIBBackup.Create(Self);
      with UIBBackup do
      begin
        try
          if (UpperCase(UData.ServerName) = 'LOCAL') or
             (UData.ServerName = '') then
            Host := 'localhost'
          else
            Host := UData.ServerName;
          Protocol := proTCPIP;
          UserName := UData.DBPath.Params.Values['user_name'];
          PassWord := UData.DBPath.Params.Values['password'];
          Options := [boIgnoreLimbo];
          Database := StringReplace(UData.DBPath.DatabaseName, Host + ':', '', [rfReplaceAll]);
          BackupFileName := UData.BackupPath +
                            ChangeFileExt(ExtractFileName(
                                          UData.DBPath.DatabaseName), '.gbk');
          BackupFiles.Add(BackupFileName);
          RenameFile(BackupFileName, ChangeFileExt(BackupFileName, '.~gbk'));
          OnVerbose := OnBackupVerbose;
          Run;
          FResponseBuffer.WriteFields(True, [BackupFileName]);
          FResponseBuffer.WriteFields(False, [BackupSize]);
          DeleteFile(ChangeFileExt(BackupFileName, '.~gbk'));
        finally
          Free;
        end;
      end;
      {$ENDIF UIBSRV}
      {$IFDEF MSSQLSRV}
      BackupFileName := UData.BackupPath + UData.DBDatabase + '_backup.dat';
      //LogBackupFileName := UData.BackupPath + UData.DBDatabase + '_log.dat';
      RenameFile(BackupFileName, ChangeFileExt(BackupFileName, '.~dat'));
      //RenameFile(LogBackupFileName, ChangeFileExt(LogBackupFileName, '.~dat'));
      try
        with TSrvQuery.Create(nil) do
        begin
          DatabaseName := UData.DBPath;
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
            Add('BACKUP DATABASE ' + UData.DBDatabase +
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
      {  I don't know how to collect the data returned from the BACKUP
         SQL-command  }
      BackupSize := 0;
      if FindFirst(BackupFileName, faAnyFile, F) = 0 then
        BackupSize := F.Size;
      FindClose(F);
      FResponseBuffer.WriteFields(True, [BackupFileName]);
      FResponseBuffer.WriteFields(False, [BackupSize]);
      {$ENDIF MSSQLSRV}
      {$IFDEF MYSQLSRV}
      try
        BackupFileName := UData.BackupPath;
        //remove ending slash or backslash
        if (BackupFileName[Length(BackupFileName)]='/') or (BackupFileName[Length(BackupFileName)]='\') then
          delete(BackupFileName,Length(BackupFileName),1);
        try
          with TSrvQuery.Create(nil) do
          begin
            DatabaseName := UData.DBPath;
            SQL.Text:='BACKUP TABLE blobs,bugs,ffamily,groups,labels,logcomm,';
            SQL.Add('mdbugs,modules,mstones,pjbugs,pjgroups,pjmodule,pjmstone,');
            SQL.Add('pjref,pjusers,projects,revision,rvlabels,todo,transact,');
            SQL.Add('users,vcslog TO "'+DoubleSlash(BackupFileName)+'"');
            Open;
            //Dump Reply;
            while not Eof do
              begin
              FResponseBuffer.WriteFields(True, [Fields[3].AsString+':'+Fields[0].AsString]);
              FResponseBuffer.WriteFields(False, [0]);
              Next;
              end;
            Free;
          end;
        except
        end;
      except
      end;
      {$ENDIF MYSQLSRV}
      // store backup time stamp
      UData.LastLiveBackup := LocalDT2GMTDT(Now);
      // Server settings changed -> set 'settings need to be saved' flag
      UData.SettingsChanged := True;
      // update server log file
      if UData.ShowServerMsg then
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                                  ' [LIVE_BACKUP] Target: ' + UData.BackupPath);
    end // if UData.BackupPath <> '' and...
    else
    begin
      // no, return error message
      FResponseBuffer.WriteFields(True, [False]);
      FResponseBuffer.WriteFields(False,
                              ['Backup target folder undefined or not found!']);
      if UData.ShowServerMsg then
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                             '!! Backup target folder undefined or not found!');
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  {$ENDIF ~DBBACKUPSUPPORT} // else {$IFNDEF DBBACKUPSUPPORT}
  Finish;
end;

{$IFDEF UIBSRV}
procedure TServerObjectLIVE_BACKUP.OnBackupVerbose(Sender: TObject;
  Line: string);
begin
  // check backup message for information on backup file size
  {  there may be a faster way of doing this by interrogating the
     actual physical file size, but his will do for now ???  }
  if (Pos('closing file', Line) > 0) then
    BackupSize := StrToIntDef(Copy(Line,
                               Pos('.', Line)+2,
                               Pos('bytes written',Line)-Pos('.', Line)-3),
                               0);
end;
{$ENDIF UIBSRV}

{==============================================================================
  Purge - free space by removing unnecessary (older) modules from the DB.
  Purge all revisions from the specified project, keep max. "Number of Rev. to
  keep" in the archive.
  When called with "Execute = false" the object does not really delete anything.
  Clients call this object twice - one time to get the number of affected files,
  second time (if the user has confirmed that he/she really wants to delete) to
  remove the revisions.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Number of Rev. to keep - Integer
               [2]Execute                - Boolean

  Response: 0: [0]Number of affected files - Integer
               [1]Number of deleted Rev.   - Integer

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectPURGE_PROJECT.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQueryModules,
  FQueryDelete,
  FQueryExecute: TSrvQuery;
  I,
  liDel,
  liRevID,
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  KeepRevisions,
  DeletedRevisions: Integer;
  ExecutePurge: Boolean;
  nRecCount: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    ProjectID := StrToInt(FRequestBuffer.Fields[0]);
    KeepRevisions := StrToInt(FRequestBuffer.Fields[1]);
    ExecutePurge := (FRequestBuffer.Fields[2] = '1');

    FResponseBuffer.Rewrite;
    FQueryModules := TSrvQuery.Create(Self);
    try
      FQueryModules.DatabaseName := UData.DBPath;
      with FQueryModules do
      begin
        {  Step 1: get all modules containing more revisions than
           'KeepRevisions' }
        SQL.Clear;
        SQL.Add('select count(*) as revcount, modules.moduleid, modules.name');
        SQL.Add('from modules, revision, pjmodule');
        SQL.Add('where pjmodule.projectid = :projectid');
        SQL.Add('and pjmodule.moduleid = modules.moduleid');
        SQL.Add('and revision.moduleid = modules.moduleid');
        SQL.Add('group by modules.moduleid, modules.name');
        SQL.Add('having count(*) > ' + IntToStr(KeepRevisions));
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        // are there any modules affected?
        nRecCount := RecordCount; // get recordcount one time 
        FResponseBuffer.WriteFields(True, [nRecCount]);
        // really Purge now?
        if ExecutePurge and (nRecCount>0) then
        begin
          // yes
          // Step 2: get the revisions for all affected modules
          FQueryDelete := TSrvQuery.Create(Self);
          FQueryExecute := TSrvQuery.Create(Self);
          DeletedRevisions := 0;
          try
            FQueryDelete.DatabaseName := UData.DBPath;
            FQueryExecute.DatabaseName := UData.DBPath;
            while not Eof do
            begin
              // caculate the number of revisions to delete
              liDel := FieldByName('revcount').AsInteger - KeepRevisions;
              FQueryDelete.SQL.Clear;
              FQueryDelete.SQL.Add('select revisionid, version, revision'); //MG
              FQueryDelete.SQL.Add('from revision');
              FQueryDelete.SQL.Add('where moduleid = ' +
                                              FieldByName('moduleid').AsString);
              FQueryDelete.SQL.Add('order by version asc, revision asc');  //MG
              FQueryDelete.Open;
              {  Step 3: delete all revisions and all records in other tables
                 dependend on the revision  }
              for I := 1 to liDel do
              begin
                Inc(DeletedRevisions);
                liRevID := FQueryDelete.FieldByName('revisionid').AsInteger;
                {  to avoid foreign key violations start at the bottom of the
                   dependencies  }
                FQueryExecute.SQL.Text :=
                             'delete from blobs where revisionid = :revisionid';
                FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                FQueryExecute.ExecSQL;
                FQueryExecute.SQL.Text :=
                          'delete from rvlabels where revisionid = :revisionid';
                FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                FQueryExecute.ExecSQL;
                FQueryExecute.SQL.Text :=
                          'delete from logcomm where revisionid = :revisionid';
                FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                FQueryExecute.ExecSQL;
                FQueryExecute.SQL.Text :=
                          'delete from revision where revisionid = :revisionid';
                FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                FQueryExecute.ExecSQL;
                FQueryDelete.Next; // -> next revision
                if FQueryDelete.Eof then Break;
              end;
              FQueryDelete.Close;
              Next; // -> next module
            end; // while not (FQueryModules) EOF do
          finally
            FQueryDelete.Free;
            FQueryExecute.Free;
          end;
          FResponseBuffer.WriteFields(False, [DeletedRevisions]);
          // update archive time stamp
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
        end; // if ExecutePurge and (nRecCount>0) then ...
        Close; // FQueryModules
      end; // with FQueryModules do
    finally
      FQueryModules.Free;
    end;
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  EXECUTE_SQL: this server objects allows Archive Admins direct access to the
  underlying database engine. It may be used for special tasks not provided by
  common FreeVCS functions.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]SQL statement          - String
                  (Lines separated with LF [#10])
               [1]Exec SQL               - Boolean
                  ('1' = Exec SQL / '0' = Open SQL)
               [2]Max. returned rows     - Integer

  Response: depends on the SQL statement

  ExecSQL:  0: [0]Affected Rows          - Integer

  OpenSQL:  0: [0]Field Name 0           - String
               [1]Field Name 1           - String
               [..]
            1: [0]Row 0, Field 0         - String
               [1]Row 0, Field 1         - String
               [..]
            2: [0]Row 1, Field 0         - String
               [1]Row 1, Field 1         - String
               [..]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectEXECUTE_SQL.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  I,
  Fld,
  TANr,
  AccessID,
  GrantedRights,
  MaxReturnedRows: Integer;
  ExecCommand: Boolean;
  SQLText,
  InvCommand: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    // object allowed on the current server?
    if UData.DisableSQLDirekt then
    begin
      // No
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False, [Format(SrvException,
                   [FFunctionCode, 'Remote SQL not allowed on this server.'])]);
      Finish;
      Exit;
    end // if not UData.EnableSQLDirekt then
    else
    begin
      // Yes
      FRequestBuffer.Next;
      SQLText := FRequestBuffer.Fields[0];
      ExecCommand := (FRequestBuffer.Fields[1] = '1');
      MaxReturnedRows := StrToInt(FRequestBuffer.Fields[2]);
      // check for invalid commands
      InvCommand := '';
      if Pos('drop', LowerCase(SQLText)) > 0 then
        InvCommand := InvCommand + 'drop/';
        // ??? probably there must be more...

      // have we found invalid words?
      if InvCommand <> '' then
      begin
        // Yes
        FResultStatus := 403; // forbidden
        FResponseBuffer.Rewrite;
        FResponseBuffer.WriteFields(False, [Format(SrvException,
                       [FFunctionCode, 'Command not allowed: ' + InvCommand])]);
        Finish;
        Exit;
      end // if InvCommand <> '' then
      else
      begin
        // No, execute the command
        FResponseBuffer.Rewrite;
        FQuery := TSrvQuery.Create(Self);
        try
          FQuery.DatabaseName := UData.DBPath;
          with FQuery do
          begin
            Close;
            SQL.Clear;
            SQL.Text := SQLText;
            if ExecCommand then
            begin
              ExecSQL;
              FResponseBuffer.WriteFields(False, [RowsAffected]);
            end
            else
            begin
              Open;
              if not Eof then
              // Copy all field names to the response
                for Fld := 0 to FQuery.FieldCount - 1 do
                  FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].FieldName]);
              I := 0;
              // Copy all rows/fields from the record to the response
              while (not Eof) and (I <= MaxReturnedRows) do
              begin
                Inc(I);
                for Fld := 0 to FQuery.FieldCount - 1 do
                  FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
                Next;
              end;
              Close;
            end; // else if ExecCommand then
          end; // with FQuery do begin
        finally
          FQuery.Free;
        end;
      end; // else if InvCommand <> '' then
    end; // else if not UData.EnableSQLDirekt then
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Read general client configuration data from the servers config file

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Value                  - String

            2: [0]Value                  - String
               [..]

  Response: 0: [0]Value                  - String
               [1]Data for value         - String
               ('???' = Key/Value not found)

            1: [0]Value                  - String
               [1]Data for value         - String
               [..]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREAD_CONFIG_DATA.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  TANr,
  GrantedRights,
  AccessID: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    with TIniFile.Create(FIniFileName) do
    begin
      try
        while not FRequestBuffer.Eof do
        begin
          FResponseBuffer.WriteFields(True, [FRequestBuffer.Fields[0]]);
          FResponseBuffer.WriteFields(False, [ReadString(GlobalUserData,
                                             FRequestBuffer.Fields[0], '???')]);
          FRequestBuffer.Next;
        end; // while not FRequestBuffer.EoF do
      finally
        Free;
      end;
    end; // with TIniFile.Create(...
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{==============================================================================
  Write general client configuration data to the servers client config file
  (not the servers config file!)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Value                  - String
               [1]Data                   - String

            2: [0]Value                  - String
               [1]Data                   - String
               [..]

  Response:    none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectWRITE_CONFIG_DATA.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  TANr,
  GrantedRights,
  AccessID: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);
    if GrantedRights < RequestedRights then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
     if GrantedRights > -1 then
        FResponseBuffer.WriteFields(False,
         [Format(Levels, [AccLevel[RequestedRights], AccLevel[GrantedRights]])])
      else
        FResponseBuffer.WriteFields(False, [Unknown]);
      Finish;
      Exit;
    end;
    FRequestBuffer.Next;

    FResponseBuffer.Rewrite;
    with TIniFile.Create(FIniFileName) do
    begin
      try
        while not FRequestBuffer.Eof do
        begin
          WriteString(GlobalUserData, FRequestBuffer.Fields[0],
                                                      FRequestBuffer.Fields[1]);
          FRequestBuffer.Next;
        end; // while not FRequestBuffer.EoF do
      finally
        Free;
      end;
    end; // with TIniFile.Create(...
    FResultStatus := 200;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;

end.

