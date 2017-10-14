{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvOBJModules.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

  Aug '99 by Thomas Hensle - http://www.freevcs.de
  Based on François PIETTE's SvrTest demo application.

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI VCS
2003/02/09  HDors    - Add MySQL Server Port by Ludo Brands
2003/02/22  THuber   - Add  F I B P L U S S R V  port which uses  F I B p l u s  components
                       therefor dangerous casts changed:
                        * TDateTime(...) => .AsDateTime
                        * Integer(...) => .AsInteger
                        * FQuery.Fields[Fld] => FQuery.Fields[Fld].AsString
                     - removed Interbase5 support
                     - D4_UP directive removed
2003/02/28  THuber   - now use FieldByName everywhere
2003/03/07  THuber   - removed A CCESSCTRL IFDEF
2003/04/04  THuber   - SQL92 SQL for D B I S A M, FF, IB, FB, FF, MYSQL
                       for better reading now every DBMS has it's own sql
                       in GET_REVISION_LIST_BY_ID
                     - HDo: mantis  #685, #686
                       #685: GET_SHARED_MODULES: suggested optimization having count
                       #686: GET_REVISION_LIST_BY_ID: Tuneup for Oracle
2003/04/14 MGosselink- MG: mantis
                       #779: a revision can checked in more than one time
2003/07/01 THuber    -  F i b  + Bugfix in GET_MODULE_HISTORY
2003/09/10 MGosselink- IB6 Bugfix in GET_REVISION_LIST_BY_ID:
                       "ORDER BY" added. I think this should be added in
                       every DBMS
2003/09/10 THuber    - #702, #1126: applied marcos fix also to other ports and use an
                       A N S I _ O U T E R J O I N  define for simplification of sql92 change
2003/10/21 LBrands   - #1195: fixed REMOVE_MODULES for use with MySQL 3.x
                       (no use of subselects)
2004/01/17 USchuster - better error message for MySQL max_allowed_packet problem
                       in functioncodes CHECKIN_MODULE and COPY_REVISION
                     - fixed GET_REVISION_LIST_BY_NAME (was broken at least since
                       FreeVCS server source 2.00 [2001/04/27] but fortunately this
                       function is not used in the client)
2003/09/10 THuber    - #2015 - conflict detection for MOVE_MODULES:
                       same module must not exist in target folder

--- branchpoint for 2.5 dev server ---

2004/12/08 USchuster - style cleaning
                     - simplified creation of insert statements
2004/12/25 USchuster - checkout comment will now be cleared in UNDO_CHECKOUT_MODULE
                       (mantis #800)
2004/12/27 USchuster - COPY_REVISION and MERGE_VER_REV_NR now will check if the
                       new version.revision of the module still exists (mantis #1284)
2005/01/10 USchuster - now we do lowercase the new module name in RENAME_MODULE (mantis #2454)
2005/02/13 USchuster - in CHECKIN_MODULE modules.locktstamp will now be updated
                       and not longer cleared in put mode (mantis #2616)
2005/05/08 USchuster - removed unused units
2005/05/09 USchuster - RENAME_MODULE does now check if there is already a module
                       with the new name in the same directory (mantis #2941)
2005/07/04 USchuster - TFVCSServerObject.VCSLog is used now to create log entries(mantis #2631)
2005/09/22 THuber     #3212 adjust AutoExpand Buffer for Blob transfer
                            GET_CHECKOUT_MODULE, GET_SINGLE_BLOB
                            added min buffer size to size calculation
2005/10/01 USchuster - improved blob access(reading) (mantis #3239)
2007/06/16 USchuster - adjusted REMOVE_MODULE to write ProjectID and ModuleID into log if possible
                       and as of now if a module is removed completely the log entries "survive" (mantis #3639)
                     - adjusted order in GET_MODULE_HISTORY (mantis #4151)
2007/07/05 USchuster - D B I S A M  and  F l a s h F i l e r  fix for last change (Mantis #4151) in GET_MODULE_HISTORY
2009/06/20 USchuster - fixed duplicate revision problem for checked out modules (Mantis #779)
                       (old solution was unfortunately added one "end" to early)
--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5066 support for  F l a s h F i l e r  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
                     special Oracle J O I N  - Syntax was removed we now only
                     support the A N S I - J o i n Syntax and therefore removed
                     this compiler setting.
-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SvrOBJModules - ServerObjects for JVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvOBJModules;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Classes, RFormat, SrvConst, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef,
  SrvAccessDef, DB;

type
  TJVCSModuleServerObject = class(TFVCSServerObject)
  public
    function ModuleVersionRevisionExists(AModuleID, AVersion, ARevision: Integer): Boolean;
  end;

  TServerObjectGET_MODULE_ID = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_MODULE_NAME = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectRENAME_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_MODULES_LIKE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_REVISION_LIST_BY_NAME = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_REVISION_LIST_BY_ID = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_REVISION_LIST_BY_VERSION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_REVISION_STATUS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_REVISION_COMMENT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectCHANGE_REVISION_COMMENT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_BLOB_STATUS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_MODULE_HISTORY = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectIS_MEMBER_OF_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SINGLE_BLOB = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SHARED_MODULES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_NEW_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_REVISION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_VERSION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectMOVE_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_CHECKOUT_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectCHECKOUT_ONLY_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectUNDO_CHECKOUT_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectCHECKIN_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectHIDE_UNHIDE_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectCOPY_REVISION = class(TJVCSModuleServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectMERGE_VER_REV_NR = class(TJVCSModuleServerObject)
  public
    procedure Execute; override;
  end;

implementation

{$IFDEF DELPHI6_UP}
uses
  Variants;
{$ENDIF DELPHI6_UP}

procedure FileDataBlobToMWBuffer(AQuery: TSrvQuery; ABuffer: TMWBuffer);
var
  tempStream: TStream;
  nAutoExpand: Integer;
begin
  nAutoExpand := ABuffer.AutoExpand;
  try
    tempStream := AQuery.CreateBlobStream(AQuery.FieldByName('filedata'), bmRead);
    try
      tempStream.Seek(0, 0);
      // Adjust Response memory (min 256)
      ABuffer.AutoExpand := tempStream.Size + (tempStream.Size div 4) + 256;
      ABuffer.WriteStreamField(False, mwBlob, tempStream);
    finally
      tempStream.Free;
    end;
  finally
    ABuffer.AutoExpand := nAutoExpand;
  end;
end;

procedure FileDataBlobToStream(ADataSet: TObject; AStream: TStream);
var
  tempStream: TStream;
begin
  tempStream := TDataSet(ADataSet).CreateBlobStream(TDataSet(ADataSet).FieldByName('filedata'), bmRead);
  try
    AStream.Size := tempStream.Size;
    AStream.Position := 0;
    AStream.CopyFrom(tempStream, AStream.Size);
  finally
    tempStream.Free;
  end;
end;

function TJVCSModuleServerObject.ModuleVersionRevisionExists(AModuleID, AVersion,
  ARevision: Integer): Boolean;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
begin
  UData := PUserDataRecord(FUserData);
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      SQL.Clear;
      SQL.Add('SELECT revisionid');
      SQL.Add('FROM revision');
      SQL.Add('WHERE moduleid = :moduleid');
      SQL.Add('AND version = :version');
      SQL.Add('AND revision = :revision');
      ParamByName('moduleid').AsInteger := AModuleID;
      ParamByName('version').AsInteger := AVersion;
      ParamByName('revision').AsInteger := ARevision;
      Open;
      Result := not Eof;
      Close;
    end;
  finally
    FQuery.Free;
  end;
end;


{==============================================================================
  Add a new module & create a link to the current project
  - if the module is used in other projects create only a shared link
  - if the module is already used in the current project do nothing
  - update project's history & vcslog

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module (name & path)   - String
               [3]Flags                  - Integer
                 (internal bit-coded field, mainly for debug purposes,
                  no effects on version controlling)
               [4]Description            - String
                  (optional)

  Response: 0: [0]new module?            - Boolean
               [1]Module ID              - Integer
                  (if this is a real new module return the new module ID,
                   otherwise return the old)

            1: [0]Project ID             - Integer
                  (if this module is already in the archive return the
                   assigned project ID's)

            2: [0]Project ID             - Integer
                  (return the next project ID's - maybe a shared module?)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_NEW_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  UserID,
  ModuleID,
  ProjectID,
  Flags: Integer;
  Module,
  ModuleDescr: string;
  IsNewModule,
  NeedAssign,
  IsAlreadyAssigned: Boolean;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    Module := AnsiLowerCase(FRequestBuffer.Fields[2]);
    Flags := StrToInt(FRequestBuffer.Fields[3]);
    ModuleDescr := FRequestBuffer.Fields[4];

    FResponseBuffer.Rewrite;
    IsAlreadyAssigned := False;
    ModuleID := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT moduleid FROM modules');
        SQL.Add('WHERE name = :module');
        SQL.Add('AND path = :path');
        ParamByName('module').AsString := ExtractFileName(Module);
        ParamByName('path').AsString := ExtractFilePath(Module);
        Open;
        IsNewModule := Eof;
        if not Eof then ModuleID := FQuery.FieldByName('moduleid').AsInteger;
        Close;
        if IsNewModule then
        begin
          // return true
          FResponseBuffer.WriteFields(True, [True]);
          SQL.Clear;
          SQL.Add('INSERT INTO modules');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('modules')]));          
          SQL.Add(':name, :path, :tstamp, ''0'', :userid,');
          SQL.Add(':locktstamp, :description, null, null, :flags)');
          // lastuser, familyid = null
          ParamByName('name').AsString := ExtractFileName(Module);
          ParamByName('path').AsString := ExtractFilePath(Module);
          ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('userid').AsInteger := UserID;
          ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('description').AsString :=
                     ModuleDescr + ' - Added to archive: ' + DateTimeToStr(LocalDT2GMTDT(Now));
          ParamByName('flags').AsInteger := Flags;
          ExecSQL;
          // get the new created module ID
          SQL.Clear;
          SQL.Add('SELECT moduleid FROM modules');
          SQL.Add('WHERE name = :name AND path = :path');
          ParamByName('name').AsString := ExtractFileName(Module);
          ParamByName('path').AsString := ExtractFilePath(Module);
          Open;
          ModuleID := FQuery.FieldByName('moduleid').AsInteger;
          Close;
          // return the new module ID
          FResponseBuffer.WriteFields(False, [ModuleID]);
          // Assign the module to the project
          NeedAssign := True;
          // save statistic data
          Inc(UData.NewFilesCount);
        end // if IsNewModule then begin
        else
        begin
          // return false
          FResponseBuffer.WriteFields(True, [False]);
          // return the old module ID
          FResponseBuffer.WriteFields(False, [ModuleID]);
          // get the projects already assigned with this module
          Close;
          SQL.Clear;
          SQL.Add('SELECT pjmodule.projectid, projects.name');
          SQL.Add('FROM pjmodule, projects');
          SQL.Add('WHERE pjmodule.moduleid = :moduleid');
          SQL.Add('AND projects.projectid = pjmodule.projectid');
          ParamByName('moduleid').AsInteger := ModuleID;
          Open;
          NeedAssign := True;
          while not Eof do
          begin
            // check if we have already a link to this module
            if (FQuery.FieldByName('projectid').AsInteger = ProjectID)
              then NeedAssign := False;
            // Copy all fields from the record to the response
            for Fld := 0 to FQuery.FieldCount - 1 do
              FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
            Next;
          end; // while not EoF do begin
          Close;
        end; // else if IsNewModule then begin
        if NeedAssign then
        begin
          // Assign the module to the project
          // Be sure to not add double links to one project
          Close;
          SQL.Clear;
          SQL.Add('SELECT * FROM pjmodule');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND moduleid = :moduleid');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('moduleid').AsInteger := ModuleID;
          Open;
          IsAlreadyAssigned := not Eof;
          Close;
          if not IsAlreadyAssigned then
          begin
            SQL.Clear;
            SQL.Add('INSERT INTO pjmodule');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjmodule')]));
            SQL.Add(':projectid, :moduleid, :hidden)');
            ParamByName('projectid').AsInteger := ProjectID;
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('hidden').AsString := '0';
            ExecSQL;
          end; // if not IsAlreadyAssigned then begin
        end; // if NeedAssign then begin
        // update project information
        if IsNewModule or (NeedAssign and not IsAlreadyAssigned) then
        begin
          Close;
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end; // if IsNewModule or NeedAssign then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and
       (IsNewModule or (NeedAssign and not IsAlreadyAssigned)) then
      VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, 0, 'a', 'Added as new module');
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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
  Remove a module and all revisions & blobs from the version archive or
  remove a shared link to a project.
  - if the module is used only in the current project removed it
  - if the module is used in other projects remove the shared link
  - if the module is not used in the current project do nothing
  - update project's history & vcslog

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]remove from archive    - Boolean
                 (remove physically)
               [4]Module name            - String(255
                 (only to complete the log file, significant
                  is the module's ID !)

  Response: 0: [0]module removed?        - Boolean
                  (if this is shared modul return false otherwise
                   return true)
            1: [0]Project ID             - Integer
                  (if this is shared modul (Field 0 = false) return  the
                   assigned project ID's, otherwise return the removed
                   revisions)

            2: [0]next Project ID        - Integer


  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  UserID,
  ModuleID,
  ProjectID: Integer;
  IsSharedModule,
  RemoveVersions: Boolean;
  ModuleName,
  LogMessage: string;
  {$IFDEF MYSQLSRV}
  sSubSelect: string;
  {$ENDIF MYSQLSRV}
  ProjectIDForLog, ModuleIDForLog: Integer;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    // object access management **********************************************
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    RemoveVersions := (FRequestBuffer.Fields[3] = '1');
    ModuleName := FRequestBuffer.Fields[4];

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // is this a shared module?
        Close;
        SQL.Clear;
        SQL.Add('SELECT pjmodule.projectid, projects.name');
        SQL.Add('FROM pjmodule, projects');
        SQL.Add('WHERE pjmodule.moduleid = :moduleid');
        SQL.Add('AND pjmodule.projectid <> :projectid');
        SQL.Add('AND pjmodule.projectid = projects.projectid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        IsSharedModule := not Eof;
        if IsSharedModule then
          // this is a shared module, return false
          FResponseBuffer.WriteFields(True, [False]);
        while not Eof do
        begin
          // return the assigned project ID's and names
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        if not IsSharedModule then
        begin
          // this is not a shared module, we can delete it, return true
          FResponseBuffer.WriteFields(True, [True]);
          // remove the module physically from the archive?
          if RemoveVersions then
          begin
            {  delete all bug links for this moduleid to prevent foreign
               key violation  }
            SQL.Clear;
            SQL.Add('DELETE FROM mdbugs');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            {  delete all log comment links assigned with this module to
               prevent foreign key violation }
            {$IFDEF MYSQLSRV} // no subselects in MySQL 3.x
            SQL.Text := 'SELECT revisionid FROM revision WHERE moduleid = :moduleid';
            Open;
            sSubSelect := '(';
            while not Eof do
            begin
              sSubSelect := sSubSelect + Fields[0].AsString + ',';
              Next;
            end; // while not EoF do begin
            sSubSelect[Length(sSubSelect)] := ')';
            {$ENDIF MYSQLSRV}
            SQL.Clear;
            SQL.Add('DELETE FROM logcomm');
            SQL.Add('WHERE revisionid IN ');
            {$IFDEF MYSQLSRV}
            if Length(sSubSelect) > 2 then // only execute when SELECT not empty
            begin
              SQL.Add(sSubSelect);
              ExecSQL;
            end;
            {$ELSE MYSQLSRV}
            SQL.Add('(SELECT revisionid FROM revision');
            SQL.Add('WHERE moduleid = :moduleid)');
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            {$ENDIF MYSQLSRV}
            {  old version: delete all log entries for this moduleid to prevent foreign
               key violation
               new version: just clear the moduleid in the log entries to prevent foreign
               key violation}
            SQL.Clear;
            {//old version (before Mantis #3639)
            SQL.Add('DELETE FROM vcslog');
            SQL.Add('WHERE moduleid = :moduleid');
            }
            SQL.Add('UPDATE vcslog');
            SQL.Add('SET moduleid = NULL');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            // delete all blobs assigned with this module
            SQL.Clear;
            SQL.Add('DELETE FROM blobs');
            SQL.Add('WHERE revisionid IN ');
            {$IFDEF MYSQLSRV}
            if Length(sSubSelect) > 2 then // only execute when SELECT not empty
            begin
              SQL.Add(sSubSelect);
              ExecSQL;
            end;
            {$ELSE MYSQLSRV}
            SQL.Add('(SELECT revisionid FROM revision');
            SQL.Add('WHERE moduleid = :moduleid)');
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            {$ENDIF MYSQLSRV}
            // delete all label links assigned with this module
            SQL.Clear;
            SQL.Add('DELETE FROM rvlabels');
            SQL.Add('WHERE revisionid IN ');
            {$IFDEF MYSQLSRV}
            if Length(sSubSelect) > 2 then // only execute when SELECT not empty
            begin
              SQL.Add(sSubSelect);
              ExecSQL;
            end;
            {$ELSE MYSQLSRV}
            SQL.Add('(SELECT revisionid FROM revision');
            SQL.Add('WHERE moduleid = :moduleid)');
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            {$ENDIF MYSQLSRV}
            // delete all revisions assigned with this module
            SQL.Clear;
            SQL.Add('DELETE FROM revision');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            // return the removed revisions
            FResponseBuffer.WriteFields(True, [RowsAffected]);
          end; // if RemoveVersions then begin
          // delete the module link
          SQL.Clear;
          SQL.Add('DELETE FROM pjmodule');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND moduleid = :moduleid');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          if RemoveVersions then
          begin
            // finally delete the module
            SQL.Clear;
            SQL.Add('DELETE FROM modules');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
          end; // if RemoveVersions then begin
        end // if not IsSharedModule then begin
        else
        begin
          // shared module: delete only the module link
          SQL.Clear;
          SQL.Add('DELETE FROM pjmodule');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND moduleid = :moduleid');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end; // else if not IsSharedModule then begin
        // update project information
        SQL.Clear;
        SQL.Add('UPDATE projects');
        SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
        SQL.Add('WHERE projectid = :projectid');
        ParamByName('lastuser').AsInteger := UserID;
        ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
        ParamByName('projectid').AsInteger := ProjectID;
        ExecSQL;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
    begin
      LogMessage := 'Removed module ' + ModuleName;
      ModuleIDForLog := ModuleID;
      // build log string
      if IsSharedModule then
        LogMessage := LogMessage + ' - project link removed'
      else
      if RemoveVersions then
      begin
        LogMessage := LogMessage + ' & all revisions';
        ModuleIDForLog := 0;
      end;
      ProjectIDForLog := 0;
      //check if project exist to prevent foreign key violation
      if ProjectID > 0 then
      begin
        FQuery := TSrvQuery.Create(Self);
        try
          FQuery.DatabaseName := UData.DBPath;
          with FQuery do
          begin
            SQL.Clear;
            SQL.Add('SELECT projectid FROM projects');
            SQL.Add('WHERE projectid = :projectid');
            ParamByName('projectid').AsInteger := ProjectID;
            Open;
            if not (Bof and Eof) then
              ProjectIDForLog := ProjectID;
            Close;
          end;
        finally
          FQuery.Free;
        end;
      end;
      VCSLog(UData.DBPath, ProjectIDForLog, UserID, ModuleIDForLog, 0, 'r', LogMessage);
    end; // if UData.WriteVCSLog....
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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
  Remove a revision from one module and all blobs from the version archive.
  - if the module is used only in the current project removed it
  - if the module is used in other projects do nothing
  - update project's history & vcslog

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Revision ID            - Integer
               [4]Module name & Ver/Rev  - String(255)
                 (only to complete the log file, significant
                  is the module's ID !)

  Response: 0: [0]Revision removed?      - Boolean
                  (if this is shared modul return false otherwise
                   return true)
            1: [0]Project ID             - Integer
                  (if this is shared modul (Field 0 = false) return  the
                   assigned project ID's, otherwise return the removed
                   revisions)

            2: [0]next Project ID        - Integer


  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_REVISION.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  UserID,
  ModuleID,
  RevisionID,
  ProjectID: Integer;
  IsSharedModule: Boolean;
  ModuleName: string;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    RevisionID := StrToInt(FRequestBuffer.Fields[3]);
    ModuleName := FRequestBuffer.Fields[4];

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // is this a shared module?
        Close;
        SQL.Clear;
        SQL.Add('SELECT pjmodule.projectid, projects.name');
        SQL.Add('FROM pjmodule, projects');
        SQL.Add('WHERE pjmodule.moduleid = :moduleid');
        SQL.Add('AND pjmodule.projectid <> :projectid');
        SQL.Add('AND pjmodule.projectid = projects.projectid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        IsSharedModule := not Eof;
        if IsSharedModule then
          // this is a shared module, return false
          FResponseBuffer.WriteFields(True, [False]);
        while not Eof do
        begin
          // return the assigned project ID's and names
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        if not IsSharedModule then
        begin
          // this is not a shared module, we can delete it, return true
          FResponseBuffer.WriteFields(True, [True]);
          // delete all label links assigned with this module
          SQL.Clear;
          SQL.Add('DELETE FROM rvlabels');
          SQL.Add('WHERE revisionid = :revisionid');
          ParamByName('revisionid').AsInteger := RevisionID;
          ExecSQL;
          // delete all blobs assigned with this module
          SQL.Clear;
          SQL.Add('DELETE FROM blobs');
          SQL.Add('WHERE revisionid = :revisionid');
          ParamByName('revisionid').AsInteger := RevisionID;
          ExecSQL;
          // return the removed revisions
          FResponseBuffer.WriteFields(True, [RowsAffected]);
          // delete all log comment links assigned with this module
          SQL.Clear;
          SQL.Add('DELETE FROM logcomm');
          SQL.Add('WHERE revisionid = :revisionid');
          ParamByName('revisionid').AsInteger := RevisionID;
          ExecSQL;
          // delete the revision assigned with this module
          SQL.Clear;
          SQL.Add('DELETE FROM revision');
          SQL.Add('WHERE revisionid = :revisionid');
          ParamByName('revisionid').AsInteger := RevisionID;
          ExecSQL;
        end; // if not IsSharedModule then begin
        // update project information
        SQL.Clear;
        SQL.Add('UPDATE projects');
        SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
        SQL.Add('WHERE projectid = :projectid');
        ParamByName('lastuser').AsInteger := UserID;
        ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
        ParamByName('projectid').AsInteger := ProjectID;
        ExecSQL;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
      VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, 0, 'r',
        'Removed revision ' + ModuleName);
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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
  Remove a version from one module and all blobs from the version archive.
  - if the module is used only in the current project removed it
  - if the module is used in other projects do nothing
  - update project's history & vcslog

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Version                - Integer
               [4]Module name            - String(255)
                 (only to complete the log file, significant
                  is the module's ID !)

  Response: 0: [0]Revision removed?      - Boolean
                  (if this is shared modul return false otherwise
                   return true)
            1: [0]Project ID             - Integer
                  (if this is shared modul (Field 0 = false) return  the
                   assigned project ID's, otherwise return the removed
                   revisions)

            2: [0]next Project ID        - Integer


  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_VERSION.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery,
  FQuery2: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  UserID,
  ModuleID,
  ProjectID,
  Version_ : Integer; // clash with TFFQuery.Version
{$IFDEF UIBSRV}
  RemovedFiles: Cardinal;
{$ELSE}
  RemovedFiles: Integer;
{$ENDIF UIBSRV}
  IsSharedModule: Boolean;
  ModuleName: string;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    Version_ := StrToInt(FRequestBuffer.Fields[3]);
    ModuleName := FRequestBuffer.Fields[4];

    FResponseBuffer.Rewrite;
    RemovedFiles := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // is this a shared module?
        Close;
        SQL.Clear;
        SQL.Add('SELECT pjmodule.projectid, projects.name');
        SQL.Add('FROM pjmodule, projects');
        SQL.Add('WHERE pjmodule.moduleid = :moduleid');
        SQL.Add('AND pjmodule.projectid <> :projectid');
        SQL.Add('AND pjmodule.projectid = projects.projectid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        IsSharedModule := not Eof;
        if IsSharedModule then
          // this is a shared module, return false
          FResponseBuffer.WriteFields(True, [False]);
        while not Eof do
        begin
          // return the assigned project ID's and names
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        if not IsSharedModule then
        begin
          // this is not a shared module, we can delete it, return true
          FResponseBuffer.WriteFields(True, [True]);
          FQuery2 := TSrvQuery.Create(Self);
          try
            FQuery2.DatabaseName := UData.DBPath;
            // get all revisions assigned to this version
            FQuery2.Close;
            FQuery2.SQL.Clear;
            FQuery2.SQL.Add('SELECT revisionid');
            FQuery2.SQL.Add('FROM revision');
            FQuery2.SQL.Add('WHERE moduleid = :moduleid');
            FQuery2.SQL.Add('AND version = :version');
            FQuery2.ParamByName('moduleid').AsInteger := ModuleID;
            FQuery2.ParamByName('version').AsInteger := Version_;
            FQuery2.Open;
            while not FQuery2.Eof do
            begin
              // delete all log comment links assigned with this module
              SQL.Clear;
              SQL.Add('DELETE FROM logcomm');
              SQL.Add('WHERE revisionid = :revisionid');
              ParamByName('revisionid').AsInteger := FQuery2.FieldByName('revisionid').AsInteger;
              ExecSQL;
              // delete all label links assigned with this module
              SQL.Clear;
              SQL.Add('DELETE FROM rvlabels');
              SQL.Add('WHERE revisionid = :revisionid');
              ParamByName('revisionid').AsInteger := FQuery2.FieldByName('revisionid').AsInteger;
              ExecSQL;
              // delete all blobs
              SQL.Clear;
              SQL.Add('DELETE FROM blobs');
              SQL.Add('WHERE revisionid = :revisionid');
              ParamByName('revisionid').AsInteger := FQuery2.FieldByName('revisionid').AsInteger;
              ExecSQL;
              RemovedFiles := RemovedFiles + RowsAffected;
              FQuery2.Next;
            end; // while not FQuery2.EoF do begin
            FQuery2.Close;
          finally
            FQuery2.Free;
          end;
          // return the removed revisions
          FResponseBuffer.WriteFields(True, [RemovedFiles]);
          // delete the version
          SQL.Clear;
          SQL.Add('DELETE FROM revision');
          SQL.Add('WHERE moduleid = :moduleid');
          SQL.Add('AND version = :version');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('version').AsInteger := Version_;
          ExecSQL;
        end; // if not IsSharedModule then begin
        // update project information
        SQL.Clear;
        SQL.Add('UPDATE projects');
        SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
        SQL.Add('WHERE projectid = :projectid');
        ParamByName('lastuser').AsInteger := UserID;
        ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
        ParamByName('projectid').AsInteger := ProjectID;
        ExecSQL;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
      VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, 0, 'r',
        'Removed version ' + ModuleName);
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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
  Move a module (change the module's path)
  - if the module is checked out do nothing and return false
  - if the module is used only in the current project move it

    (This has been changed since there is no real cause to diallow moving for
    changed modules)
  - if the module is used in other projects do nothing and return false

  - update project's history & vcslog

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]new module path        - String(255)
               [4]Module name            - String(255
                 (only to complete the log file, significant
                  is the module's ID !)

  Response: 0: [0]module moved?          - Boolean
                  (if this is a checked out modul return
                   false otherwise return true)
               [1]Error message          - String

            1: [0]Project ID             - Integer
                  (if this is checked out modul (Field 0 = false) return  the
                   assigned project ID's)

            2: [0]next Project ID        - Integer


  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectMOVE_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  //Fld,
  UserID,
  ModuleID,
  ProjectID: Integer;
  //IsSharedModule,
  IsExistingModule,
  IsReadOnly: Boolean;
  ModuleName,
  NewPath: string;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    NewPath := AnsiLowerCase(FRequestBuffer.Fields[3]);
    ModuleName := FRequestBuffer.Fields[4];

    FResponseBuffer.Rewrite;
    IsReadOnly := False;
    //SharedModule := false;
    // a path MUST end with a backslash
    if NewPath[Length(NewPath)] <> '\' then
      NewPath := NewPath + '\';
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        {  #2015 - same module must not exist in target folder:
           Check: would the move lead to the existance of a module with same
                  path/name but with a different moduleid?
           => Raise 400 exception
           Remark: shared modules are not stored in modules table so I can check
                   the modules table
         }
        Close;
        SQL.Clear;
        SQL.Add('SELECT m.moduleid FROM modules m');
        SQL.Add('WHERE m.path = :path');
        SQL.Add('AND   m.name = :name');
        ParamByName('path').AsString := NewPath;
        ParamByName('name').AsString := ExtractFileName(ModuleName);
        Open;
        try
          IsExistingModule := (not Eof) and (FieldByName('moduleid').AsInteger <> ModuleID);
        finally
          Close;
        end;

        if not IsExistingModule then
        begin
          // check if the module is checked out
          Close;
          SQL.Clear;
          SQL.Add('SELECT readonly FROM modules');
          SQL.Add('WHERE moduleid = :moduleid');
          ParamByName('moduleid').AsInteger := ModuleID;
          Open;
          try
            if not Eof then IsReadOnly := (FieldByName('readonly').AsString = '1');
          finally
            Close;
          end;

          if not IsReadOnly then
          begin
            // is this a shared module?
            {Close;
            SQL.Clear;
            SQL.Add('SELECT pjmodule.projectid, projects.name');
            SQL.Add('FROM pjmodule, projects');
            SQL.Add('WHERE pjmodule.moduleid = :moduleid');
            SQL.Add('AND pjmodule.projectid <> :projectid');
            SQL.Add('AND pjmodule.projectid = projects.projectid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('projectid').AsInteger := ProjectID;
            Open;
            IsSharedModule := not EoF;
            if IsSharedModule then
            begin
              (* this is a shared module, return false *)
              FResponseBuffer.WriteFields(true, [false]);
              (* return error message *)
              FResponseBuffer.WriteFields(false, ['shared module']);
            end; // if IsSharedModule then begin
            while not EoF do
            begin
              (* return the assigned project ID's and names *)
              for Fld := 0 to FQuery.FieldCount - 1 do
                FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
              Next;
            end; // while not EoF do begin
            Close;}
          end // if not IsReadOnly then begin
          else
          begin
            // this is a checked out module, return false
            FResponseBuffer.WriteFields(True, [False]);
            // return error message
            FResponseBuffer.WriteFields(False, ['checked out module']);
          end; // else if not IsReadOnly then begin

          if (not IsReadOnly) {and (not IsSharedModule)} then
          begin
            // this is not a read-only module, we can move it, return true
            FResponseBuffer.WriteFields(True, [True]);
            // change the path
            SQL.Clear;
            SQL.Add('UPDATE modules');
            SQL.Add('SET path = :path');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('path').AsString := NewPath;
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            // update project information
            SQL.Clear;
            SQL.Add('UPDATE projects');
            SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
            SQL.Add('WHERE projectid = :projectid');
            ParamByName('lastuser').AsInteger := UserID;
            ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
            ParamByName('projectid').AsInteger := ProjectID;
            ExecSQL;
          end; // if (not IsReadOnly) and (not IsSharedModule) then begin
        end;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and (not IsReadOnly) and (not IsExistingModule) {and (not IsSharedModule)} then
      VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, 0, 'v',
        'Module moved from ' + ExtractFilePath(ModuleName) + ' to ' + NewPath);
    // Raise Exception if conflict detected...
    if IsExistingModule then
    begin
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields ( False
                                  , [Format ( SrvException
                                            , [ FFunctionCode
                                              , 'Conflict for module ' + ExtractFileName(ModuleName) + ' detected:'+#13+#13+
                                                'There is already a module with the same name in the target folder!'+#13+#13+
                                                'Conflict has to be resolved by human brainpower!'
                                              ]
                                            )
                                    ]
                                  );
    end
    else
    begin
      // ... everything was ok, update archive time stamp
      UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
      FResultStatus := 200; // OK
    end;
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
  Get or Checkout a version/revision of a module
  (& all revision members assigned with this version/revision)
  Checkout:
  - check the read-only flag first, set to '1' after Checkout
  - update project's history & vcslog

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Revision ID            - Integer
               [4]Check Out              - Boolean
               [5]Module name            - String(255)
                 (only to complete the log file)

  Response: 0: [0]Check Out allowed?      - Boolean
                  (only if Check Out is requested, return true on Get)
               [1]Owner                   - String(50)
                  (only if Check Out is requested, return a blank on Get)

            1: [0]Module orig. time       - Double (TDateTime)
               [1]Module orig. size       - Integer
               [2]Module orig. CRC        - Integer
               [3]Module compr. size      - Integer
               [4]Module Extension        - String(20)
               [5]Module Binary           - Stream

            2: [0]next Module
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_CHECKOUT_MODULE.Execute;
const
  RequestedRightsGet = LevelRO;
  RequestedRightsCheckout = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  RequestedRights,
  Fld,
  UserID,
  ModuleID,
  RevisionID,
  ProjectID: Integer;
  CheckOut,
  CanCheckOut,
  IsMostRecent: Boolean;
  ModuleName,
  AffectedFiles: string;
  DT: Double;
  LogType: Char;
  LogMessage: string;
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
    CheckOut := (FRequestBuffer.Fields[4] = '1');
    if CheckOut then RequestedRights := RequestedRightsCheckout
      else RequestedRights := RequestedRightsGet;
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

    ProjectID := StrToInt(FRequestBuffer.Fields[0]);
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    RevisionID := StrToInt(FRequestBuffer.Fields[3]);
    ModuleName := FRequestBuffer.Fields[5];

    FResponseBuffer.Rewrite;
    CanCheckOut := False;
    AffectedFiles := '';
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        {  be sure that the requested revision ID is already the most recent
           one. Probably things have changed in the meantime...  }
        if CheckOut then
        begin
          // however, this is only meaningful for Check Out operations...
          IsMostRecent := False;
          Close;
          SQL.Clear;
          {  RevisionID is always increasing, therefore the most recent revision
             has the highest revisionid  }
          SQL.Add('SELECT MAX(revisionid) AS maxrevid');
          SQL.Add('FROM revision');
          SQL.Add('WHERE moduleid = :moduleid');
          ParamByName('moduleid').AsInteger := ModuleID;
          Open;
          // compare requested revision to most recent
          if not Eof then
            IsMostRecent := (RevisionID = FieldByName('maxrevid').AsInteger);
          Close;
        end // if CheckOut then
        else IsMostRecent := True;
        if not IsMostRecent then
        begin
          {  the requested revision is not the most recent. Someone has been
             faster... Break and return an error message  }
          FResponseBuffer.WriteFields(True, [False]);
          // return the module's owner & timestamp
          FResponseBuffer.WriteFields(False,
                                           ['busy archive - please try again']);
          FResponseBuffer.WriteFields(False, [Now]);
        end // if not IsMostRecent then
        else
        begin
          {  ok, this is the most recent revision - or Get is requested -,
             go right ahead  }
          if CheckOut then
          begin
            // check if the module is available for check out
            Close;
            SQL.Clear;
            SQL.Add('SELECT modules.readonly, modules.locktstamp, users.login');
            SQL.Add('FROM modules, users');
            SQL.Add('WHERE modules.moduleid = :moduleid');
            SQL.Add('AND users.userid = modules.userid');
            ParamByName('moduleid').AsInteger := ModuleID;
            Open;
            if not Eof then
              CanCheckOut := (FieldByName('readonly').AsString = '0');
            if not CanCheckOut then
            begin
              if not Eof then
              begin
                // Module is already checked out, return false
                FResponseBuffer.WriteFields(True, [False]);
                // return the module's owner & timestamp
                FResponseBuffer.WriteFields(False,
                                               [FieldByName('login').AsString]);
                // MWBuffer workaround for DateTime fields
                DT := FQuery.FieldByName('locktstamp').AsDateTime;
                FResponseBuffer.WriteFields(False, [DT]);
              end; // if not EoF then begin
              Close;
            end // if not CanCheckOut then begin
            else
            begin
              // Module is available, return true
              FResponseBuffer.WriteFields(True, [True]);
              Close;
              // Lock the module first
              SQL.Clear;
              SQL.Add('UPDATE modules');
              SQL.Add('SET readonly = ''1'',');
              SQL.Add('userid = :userid, lastuser = :lastuser,');
              SQL.Add('tstamp = :tstamp,');
              SQL.Add('locktstamp = :locktstamp');
              SQL.Add('WHERE moduleid = :moduleid');
              ParamByName('userid').AsInteger := UserID;
              ParamByName('lastuser').AsInteger := UserID;
              ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now);
              ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
              ParamByName('moduleid').AsInteger := ModuleID;
              ExecSQL;
              // Module is available for check out, get values & binaries
              SQL.Clear;
              {$IFNDEF ORACLESRV}
              SQL.Add('SELECT origtime, origsize, origcrc, compsize,');
              SQL.Add('extension, filedata');
              SQL.Add('FROM blobs');
              SQL.Add('WHERE revisionid = :revisionid');
              {$ELSE}
              // Oracle needs SQL in uppercase if RequestLive = true
              SQL.Add('SELECT ORIGTIME, ORIGSIZE, ORIGCRC, COMPSIZE,');
              SQL.Add('EXTENSION, FILEDATA');
              SQL.Add('FROM BLOBS');
              SQL.Add('WHERE REVISIONID = :revisionid');
              {$ENDIF ~ORACLESRV}
              ParamByName('revisionid').AsInteger := RevisionID;
              Open;
              while not Eof do
              begin
                // MWBuffer workaround for DateTime fields
                DT := FQuery.FieldByName('origtime').AsDateTime;
                FResponseBuffer.WriteFields(True, [DT]);
                // Copy common fields
                for Fld := 1 to FQuery.FieldCount - 2 do
                  FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
                // Copy stream field
                FileDataBlobToMWBuffer(FQuery, FResponseBuffer);
                // save affected files
                AffectedFiles := AffectedFiles + ' / ' +
                                      ChangeFileExt(ExtractFileName(ModuleName),
                                             FieldByName('extension').AsString);
                Next; // revision member
              end; // while not EoF do begin
              Close;
              // save statistic data
              Inc(UData.CheckOutCount);
            end; // else if not CanCheckOut then begin
          end // if CheckOut then begin
          else
          begin
            // Get requested, return true
            FResponseBuffer.WriteFields(True, [True]);
            // return blank user name
            FResponseBuffer.WriteFields(False, ['']);
            // get values & binaries
            SQL.Clear;
            {$IFNDEF ORACLESRV}
            SQL.Add('SELECT origtime, origsize, origcrc, compsize,');
            SQL.Add('extension, filedata');
            SQL.Add('FROM blobs');
            SQL.Add('WHERE revisionid = :revisionid');
            {$ELSE}
            // Oracle needs SQL in uppercase if RequestLive = true
            SQL.Add('SELECT ORIGTIME, ORIGSIZE, ORIGCRC, COMPSIZE,');
            SQL.Add('EXTENSION, FILEDATA');
            SQL.Add('FROM BLOBS');
            SQL.Add('WHERE REVISIONID = :revisionid');
            {$ENDIF ~ORACLESRV}
            ParamByName('revisionid').AsInteger := RevisionID;
            Open;
            while not Eof do
            begin
              // MWBuffer workaround for DateTime fields
              DT := FQuery.FieldByName('origtime').AsDateTime;
              FResponseBuffer.WriteFields(True, [DT]);
              // Copy common fields
              for Fld := 1 to FQuery.FieldCount - 2 do
                FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
              // Copy stream field
              FileDataBlobToMWBuffer(FQuery, FResponseBuffer);
              Next; // revision member
            end; // while not EoF do begin
            Close;
            // save statistic data
            Inc(UData.GetCount);
          end; // else if CheckOut then begin
          // update project information
          if CheckOut and CanCheckOut then
          begin
            // Get is not a write access, leave project information unchanged
            SQL.Clear;
            SQL.Add('UPDATE projects');
            SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
            SQL.Add('WHERE projectid = :projectid');
            ParamByName('lastuser').AsInteger := UserID;
            ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
            ParamByName('projectid').AsInteger := ProjectID;
            ExecSQL;
          end; // if CheckOut then begin
        end; // if IsMostRecent then
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
    begin
      if IsMostRecent and ((not CheckOut) or (CheckOut and CanCheckOut)) then
      begin
        if not CheckOut then
        begin
          LogType := 'g';
          LogMessage := 'Get module: ' + ModuleName + ' - Affected files: ' + AffectedFiles;
        end else
        begin
          LogType := 'o';
          LogMessage := 'Check out module: ' + ModuleName + ' - Affected files: ' + AffectedFiles;
        end;
        VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, 0, LogType, LogMessage);
      end; // if (not CheckOut) or (CheckOut and CanCheckOut) then begin
    end; // if UData.WriteVCSLog....
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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

{$IFDEF MYSQLSRV}
resourcestring
{
  USc - for localized client it would be better to send only an "encrypted" errorstring for
  such extended error messages ?
  something like that
  "SRVERROR: 00001;12345;678"
  and the FreeVCS Client would show this as it is and the JEDI VCS Client would decode this
  to "The size ... (12345 Byte) is greater ... "max_allowed_packet" (678 Byte)..." 

}
  rsMySQL_BlobstreamSize_greater_max_allowed_packet =
    'The size of the blobstream (%d Byte) is greater than the MySQL variable "max_allowed_packet" (%d Byte).';
  rsMySQL_BlobstreamSize_very_close_to_max_allowed_packet =
    'The size of the blobstream (%d Byte) is very close to the MySQL variable "max_allowed_packet" (%d Byte).';
  rsMySQL_increase_max_allowed_packet =
    'Increase the MySQL variable "max_allowed_packet" to avoid this problem.';
  //USc - GERMAN: so nach dem Motto "Wenn Sie von alledem noch nichts gehört haben..."
  rsMySQL_DAU_contact_admin =
    'If you have no idea what that means contact your JEDI VCS/MySQL administrator.';
    
procedure CheckAndAddMaxAllowedPacketErrorMsg(ABlobStreamSize: Integer;
  UserDataPtr: PUserDataRecord; var AErrorMsg: string);
var
  Qry: TSrvQuery;
  MaxAllowedPacket: Integer;
  AdditionalMsg: string;
begin
  //Usc with try/except to avoid an exception in exception handler
  try
    if (ABlobStreamSize <> -1) then
    begin
      Qry := TSrvQuery.Create(nil);
      try
        with Qry do
        begin
          DatabaseName := UserDataPtr^.DBPath;
          SQL.Text := 'show variables like "max_allowed_packet"';
          Open;
          if not (Qry.Bof and Qry.Eof) then
            try
              MaxAllowedPacket := Fields[1].AsInteger;
            except
              MaxAllowedPacket := -1;
            end
          else
            MaxAllowedPacket := -1;
          Close;
        end;
      finally
        Qry.Free;
      end;
      if MaxAllowedPacket <> -1 then
      begin
        if ABlobStreamSize > MaxAllowedPacket then
          AdditionalMsg := Format(rsMySQL_BlobstreamSize_greater_max_allowed_packet,
            [ABlobStreamSize, MaxAllowedPacket])
        else
        if ABlobStreamSize + 10000 > MaxAllowedPacket then
          AdditionalMsg := Format(rsMySQL_BlobstreamSize_very_close_to_max_allowed_packet,
            [ABlobStreamSize, MaxAllowedPacket]);
        if AdditionalMsg <> '' then
          AErrorMsg := AErrorMsg + #13 + AdditionalMsg + #13 +
            rsMySQL_increase_max_allowed_packet + #13 +
            rsMySQL_DAU_contact_admin;
      end;
    end;
  except
  end;
end;
{$ENDIF MYSQLSRV}

{==============================================================================
  This object is tested and optimized for use with DBISAM.

  Checkin a new version/revision of a module
  (& all family members assigned with this version/revision)
  - check the read-only flag of the module first, set to '0' after Checkin
  - if this is not a module w/o other revisions (check in for the first time),
    the module must be checked out before by the same user if
    CasuallyCheckin = false.
  - assing the submitted label ID
  - update project's history & vcslog.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer
               [2]Put                    - Boolean
                  (If PutOnly = true leave the module locked, just update the
                   module/project information. )

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Label ID               - Integer
               [4]Old Revision ID        - Integer
               [5]Module Version         - Integer
               [6]Module Revision        - Integer
               [7]Module orig. time      - Double (TDateTime)
               [8]Module orig. size      - Integer
               [9]Module orig. CRC       - Integer
              [10]Module compr. size     - Integer
              [11]Module Extension       - String(20)
              [12]Module Binary          - Stream
              [13]Check in comment       - String
              [14]IDE Version            - Integer
              [15]Module name            - String(255)
                 (only to complete the log file)
              [16]Family ID              - Integer

            2: [0]next Module
               [...]

  Response: 0: [0]Check in allowed?      - Boolean
               [1]Error message          - String
                  (only if Check in is not allowed)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectCHECKIN_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  UserID,
  ModuleID,
  LabelID,
  NewRevisionID,
  ProjectID,
  FamilyID: Integer;
  PutOnly,
  IsNewModule,
  IsCheckedOut,
  CanCheckIn: Boolean;
  ModuleName,
  AffectedFiles,
  ErrMsg: string;
  MS: TMemoryStream;
  FldType: TMWFieldType;
  {$IFDEF MYSQLSRV}
  BlobStreamSize: Integer; //for MySQL max_allowed_packet check
  {$ENDIF MYSQLSRV}
  ExceptionMessage: string;
  LogMessage: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  {$IFDEF MYSQLSRV}
  BlobStreamSize := -1;
  {$ENDIF MYSQLSRV}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    // prevent access faults from older clients
    if FRequestBuffer.FieldCount > 2 then
      PutOnly := (FRequestBuffer.Fields[2] = '1')
    else
      PutOnly := False;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    LabelID := StrToInt(FRequestBuffer.Fields[3]);
    ModuleName := FRequestBuffer.Fields[15];
    FamilyID := StrToInt(FRequestBuffer.Fields[16]);

    FResponseBuffer.Rewrite;
    AffectedFiles := '';
    NewRevisionID := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if the module is new to the archive
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.readonly, modules.userid, users.login');
        SQL.Add('FROM revision, modules, users');
        SQL.Add('WHERE revision.moduleid = :moduleid');
        SQL.Add('AND modules.moduleid = revision.moduleid');
        SQL.Add('AND users.userid = modules.userid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        IsNewModule := Eof;
        if not IsNewModule then
        begin
          // module is not new, check if the module is checked out
          IsCheckedOut := (FieldByName('readonly').AsString = '1');
          if IsCheckedOut then
          begin
            // module is checked out
            // checkin requested by the owner of the module?
            CanCheckIn := (FQuery.FieldByName('userid').AsInteger = UserID);
            if not CanCheckIn then
              ErrMsg := 'Module is locked by <' +
                                            FieldByName('login').AsString + '>';
          end // if IsCheckedOut then begin
          else
          begin
            // module is not checked out
            if UData.CasuallyCheckin then
            begin
              {  casually check in handling: module must not be checked out
                 before it can be checked in  }
              CanCheckIn := True;
            end else
            begin
              {  restrictive check in handling: module must be checked out
                 before it can be checked in  }
              CanCheckIn := False;
              ErrMsg := 'Module is not checked out';
            end; // else if UData.CasuallyCheckin then begin
          end; // else if IsCheckedOut then begin
          if CanCheckIn then
          begin
            // check if the revision already exists, mantis #779
            Close;
            SQL.Clear;
            SQL.Add('SELECT revisionid');
            SQL.Add('FROM revision');
            SQL.Add('WHERE moduleid = :moduleid');
            SQL.Add('AND version = :version');
            SQL.Add('AND revision = :revision');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('version').AsInteger := StrToInt(FRequestBuffer.Fields[5]);
            ParamByName('revision').AsInteger := StrToInt(FRequestBuffer.Fields[6]);
            Open;
            if not Eof then
            begin
              CanCheckIn := False;
              ErrMsg := 'Revision already checked in';
            end;
          end; // else if CanCheckIn then begin
        end // if not IsNewModule then begin
        else CanCheckIn := True; // module is new, no restrictions
        Close;
      end; // with FQuery do begin
      if CanCheckIn then
      begin
        with FQuery do
        begin
          // create the new revision first to get a new revision ID
          SQL.Clear;
          SQL.Add('INSERT INTO revision');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('revision')]));
          SQL.Add(':moduleid, :userid, :version, :verstate, :revision,');
          SQL.Add(':ideversion, :comment_i, :comment_o)');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('userid').AsInteger := UserID;
          ParamByName('version').AsInteger := StrToInt(FRequestBuffer.Fields[5]);
          ParamByName('verstate').AsInteger := 0;
          ParamByName('revision').AsInteger := StrToInt(FRequestBuffer.Fields[6]);
          ParamByName('ideversion').AsInteger :=
                                              StrToInt(FRequestBuffer.Fields[14]);
          ParamByName('comment_i').AsString := FRequestBuffer.Fields[13];
          ParamByName('comment_o').AsString := '';
          ExecSQL;
          // get the new revision ID
          SQL.Clear;
          SQL.Add('SELECT revisionid FROM revision');
          SQL.Add('WHERE moduleid = :moduleid AND version = :version');
          SQL.Add('AND revision = :revision');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('version').AsInteger := StrToInt(FRequestBuffer.Fields[5]);
          ParamByName('revision').AsInteger := StrToInt(FRequestBuffer.Fields[6]);
          Open;
          NewRevisionID := FQuery.FieldByName('revisionid').AsInteger;
          Close;
          // create all blobs assigned with this revision
          while not FRequestBuffer.Eof do
          begin
            // save affected files
            AffectedFiles := AffectedFiles + ' / ' +
                                      ChangeFileExt(ExtractFileName(ModuleName),
                                      FRequestBuffer.Fields[11]);
            // store file data
            SQL.Clear;
            SQL.Add('INSERT INTO blobs');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('blobs')]));            
            SQL.Add(':revisionid, :extension, :origtime, :origsize,');
            SQL.Add(':origcrc, :compsize, null, :filedata)');
            // compcrc = null
            ParamByName('revisionid').AsInteger := NewRevisionID;
            ParamByName('origtime').AsDateTime :=
                                          _StrToFloat(FRequestBuffer.Fields[7]);
            ParamByName('origsize').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[8]);
            ParamByName('origcrc').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[9]);
            ParamByName('compsize').AsInteger :=
                                            StrToInt(FRequestBuffer.Fields[10]);
            ParamByName('extension').AsString := FRequestBuffer.Fields[11];
            // store binary (compressed file content)
            MS := TMemoryStream.Create;
            try
              FRequestBuffer.GetStreamField(12, MS, FldType);
              {$IFDEF MYSQLSRV}
              BlobStreamSize := MS.Size;
              {$ENDIF MYSQLSRV}
              ParamByName('filedata').LoadFromStream(MS, ftBlob);
            finally
              MS.Free;
            end;
            ExecSQL;
            FRequestBuffer.Next;
          end; // while not FRequestBuffer.EoF do begin
        end; // with FQuery do begin
        with FQuery do
        begin
          // assign a label?
          if LabelID > 0 then
          begin
            // add label link
            SQL.Clear;
            SQL.Add('INSERT INTO rvlabels');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('rvlabels')]));
            SQL.Add(':revisionid, :labelid)');
            ParamByName('revisionid').AsInteger := NewRevisionID;
            ParamByName('labelid').AsInteger := LabelID;
            ExecSQL;
          end; // if LabelID > 0 then begin
          {  last step: unlock the module depending on the state PutOnly.
             If PutOnly = true leave the module locked, just update the
             module/project information  }
          SQL.Clear;
          SQL.Add('UPDATE modules');
          if PutOnly then
            SQL.Add('SET readonly = ''1'',')
          else
            SQL.Add('SET readonly = ''0'',');
          SQL.Add('userid = :userid, lastuser = :lastuser,');
          SQL.Add('tstamp = :tstamp,');
          SQL.Add('locktstamp = :locktstamp,');
          if FamilyID > 0 then
            SQL.Add('familyid = :familyid')
          else
            SQL.Add('familyid = null');
          SQL.Add('WHERE moduleid = :moduleid');
          ParamByName('userid').AsInteger := UserID;
          ParamByName('lastuser').AsInteger := UserID;
          if PutOnly then
            ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now)
          else
            ParamByName('locktstamp').AsDateTime := 0;
          ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('moduleid').AsInteger := ModuleID;
          if FamilyID > 0 then
            ParamByName('familyid').AsInteger := FamilyID;
          ExecSQL;
          // update project information
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end; // with FQuery do begin
        // Process ready, return true
        FResponseBuffer.WriteFields(True, [True]);
        // leave error message blank
        FResponseBuffer.WriteFields(False, ['']);
        // save statistic data
        Inc(UData.CheckinCount);
      end // if CanCheckIn then begin
      else
      begin
        // Check in denied, return false
        FResponseBuffer.WriteFields(True, [False]);
        // return error message
        FResponseBuffer.WriteFields(False, [ErrMsg]);
      end; // else if CanCheckIn then begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and CanCheckIn then
    begin
      if PutOnly then
        LogMessage := 'Update (Put) module: ' + ModuleName + ' - Affected files: ' + AffectedFiles
      else
        LogMessage := 'Check in module: ' + ModuleName + ' - Affected files: ' + AffectedFiles;
      VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, NewRevisionID, 'i', LogMessage);
    end; // if UData.WriteVCSLog and CanCheckIn then begin
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E: Exception do
    begin
      ExceptionMessage := E.Message;
      {$IFDEF MYSQLSRV}
      CheckAndAddMaxAllowedPacketErrorMsg(BlobStreamSize, PUserDataRecord(FUserData),
        ExceptionMessage);
      {$ENDIF MYSQLSRV}
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, ExceptionMessage]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, ExceptionMessage])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Create (Merge) a new version/revision of a module
  (& all family members assigned with this version/revision)
  - Client checks the read-only flag of the module first
  - update project's history & vcslog.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Source Revision ID     - Integer
               [4]Target Version         - Integer
               [5]Target Revision        - Integer
               [6]Module name            - String(255)
                 (only to complete the log file)
               [7]IDE Vesion             - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectCOPY_REVISION.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  FQuery2: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  UserID,
  ModuleID,
  SourceRevisionID,
  NewRevisionID,
  TargetVersion,
  TargetRevision,
  IDEVersion: Integer;
  Module: string;
  MS: TMemoryStream;
  {$IFDEF MYSQLSRV}
  BlobStreamSize: Integer; //for MySQL max_allowed_packet check
  {$ENDIF MYSQLSRV}
  ExceptionMessage: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  {$IFDEF MYSQLSRV}
  BlobStreamSize := -1;
  {$ENDIF MYSQLSRV}
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    SourceRevisionID := StrToInt(FRequestBuffer.Fields[3]);
    TargetVersion := StrToInt(FRequestBuffer.Fields[4]);
    TargetRevision := StrToInt(FRequestBuffer.Fields[5]);
    Module := FRequestBuffer.Fields[6];
    IDEVersion := StrToInt(FRequestBuffer.Fields[7]);

    if not ModuleVersionRevisionExists(ModuleID, TargetVersion, TargetRevision) then
    begin
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          // Create the new revision first to get the new revision ID
          SQL.Clear;
          SQL.Add('INSERT INTO revision');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('revision')]));        
          SQL.Add(':moduleid, :userid, :version, :verstate, :revision,');
          SQL.Add(':ideversion, :comment_i, :comment_o)');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('userid').AsInteger := UserID;
          ParamByName('version').AsInteger := TargetVersion;
          ParamByName('revision').AsInteger := TargetRevision;
          ParamByName('verstate').AsInteger := 3;
          ParamByName('ideversion').AsInteger := IDEVersion;
          ParamByName('comment_i').AsString := 'Merge - Ancestor: ' +
                                                       IntToStr(SourceRevisionID);
          ParamByName('comment_o').AsString := '';
          ExecSQL;
          // get the new revision ID
          SQL.Clear;
          SQL.Add('SELECT revisionid FROM revision');
          SQL.Add('WHERE moduleid = :moduleid AND version = :version');
          SQL.Add('AND revision = :revision');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('version').AsInteger := TargetVersion;
          ParamByName('revision').AsInteger := TargetRevision;
          Open;
          NewRevisionID := FQuery.FieldByName('revisionid').AsInteger;
          Close;
          FQuery2 := TSrvQuery.Create(Self);
          try
            FQuery2.DatabaseName := UData.DBPath;
            // select all blob records assigned to the source revision
            FQuery2.Close;
            FQuery2.SQL.Clear;
            {$IFNDEF ORACLESRV}
            FQuery2.SQL.Add('SELECT extension, origtime, origsize,');
            FQuery2.SQL.Add('origcrc, compsize, compcrc, filedata');
            FQuery2.SQL.Add('FROM blobs');
            FQuery2.SQL.Add('WHERE revisionid = :revisionid');
            {$ELSE}
            // Oracle needs SQL in uppercase if RequestLive = true
            FQuery2.SQL.Add('SELECT EXTENSION, ORIGTIME, ORIGSIZE,');
            FQuery2.SQL.Add('ORIGCRC, COMPSIZE, COMPCRC, FILEDATA');
            FQuery2.SQL.Add('FROM BLOBS');
            FQuery2.SQL.Add('WHERE REVISIONID = :revisionid');
            {$ENDIF ~ORACLESRV}
            FQuery2.ParamByName('revisionid').AsInteger := SourceRevisionID;
            FQuery2.Open;
            while not FQuery2.Eof do
            begin
              SQL.Clear;
              SQL.Add('INSERT INTO blobs');
              SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('blobs')]));            
              SQL.Add(':revisionid, :extension, :origtime, :origsize,');
              SQL.Add(':origcrc, :compsize, null, :filedata)');
              ParamByName('revisionid').AsInteger := NewRevisionID;
              ParamByName('extension').AsString := FQuery2.FieldByName('extension').AsString;
              ParamByName('origtime').AsDateTime := FQuery2.FieldByName('origtime').AsDateTime;
              ParamByName('origsize').AsInteger := FQuery2.FieldByName('origsize').AsInteger;
              ParamByName('origcrc').AsInteger := FQuery2.FieldByName('origcrc').AsInteger;
              ParamByName('compsize').AsInteger := FQuery2.FieldByName('compsize').AsInteger;
              // Copy stream field
              MS := TMemoryStream.Create;
              try
                FileDataBlobToStream(FQuery2, MS);
                {$IFDEF MYSQLSRV}
                BlobStreamSize := MS.Size;
                {$ENDIF MYSQLSRV}
                MS.Seek(0, 0);
                ParamByName('filedata').LoadFromStream(MS, ftBlob);
              finally
                MS.Free;
              end;
              ExecSQL;
              FQuery2.Next;
            end; // while not FQuery2.EoF do begin
            FQuery2.Close;
          finally
            FQuery2.Free;
          end;
          // update project information
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
      // update archive time stamp
      UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
      FResultStatus := 200;
    end
    else
    begin
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False, [Format(SrvException, [FFunctionCode,
        Format('Version %d.%d of %s[ModuleID %d] still exists', [TargetVersion, TargetRevision, Module, ModuleID])])]);
    end;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E:Exception do
    begin
      ExceptionMessage := E.Message;
      {$IFDEF MYSQLSRV}
      CheckAndAddMaxAllowedPacketErrorMsg(BlobStreamSize, PUserDataRecord(FUserData),
        ExceptionMessage);
      {$ENDIF MYSQLSRV}
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, ExceptionMessage]));
      {$ENDIF LOGEXCEPTION}
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
                            [Format(SrvException, [FFunctionCode, ExceptionMessage])]);
    end;
  end;

  Finish;
end;
{==============================================================================
  Create (Merge) a new version/revision of a module
  (just change version/revision number w/o creating a new record)
  - Client checks the read-only flag of the module first
  - update project's history & vcslog.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Source Revision ID     - Integer
               [4]Target Version         - Integer
               [5]Target Revision        - Integer
               [6]Module name            - String(255)
                 (only to complete the log file)
               [7]IDE Vesion             - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectMERGE_VER_REV_NR.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  UserID,
  ModuleID,
  SourceRevisionID,
  TargetVersion,
  TargetRevision: Integer;
  Module: string;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    SourceRevisionID := StrToInt(FRequestBuffer.Fields[3]);
    TargetVersion := StrToInt(FRequestBuffer.Fields[4]);
    TargetRevision := StrToInt(FRequestBuffer.Fields[5]);
    Module := FRequestBuffer.Fields[6];

    if not ModuleVersionRevisionExists(ModuleID, TargetVersion, TargetRevision) then
    begin
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          // change version/revision number
          SQL.Clear;
          SQL.Add('UPDATE revision');
          SQL.Add('SET version = :version, revision = :revision,');
          SQL.Add('userid = :userid, verstate = :verstate');
          SQL.Add('WHERE revisionid = :revisionid');
          ParamByName('userid').AsInteger := UserID;
          ParamByName('version').AsInteger := TargetVersion;
          ParamByName('revision').AsInteger := TargetRevision;
          ParamByName('verstate').AsInteger := 3;
          ParamByName('revisionid').AsInteger := SourceRevisionID;
          ExecSQL;
          // update project information
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
      // update archive time stamp
      UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
      FResultStatus := 200;
    end
    else
    begin
      FResultStatus := 400;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False, [Format(SrvException, [FFunctionCode,
        Format('Version %d.%d of %s[ModuleID %d] still exists', [TargetVersion, TargetRevision, Module, ModuleID])])]);
    end;
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
  Hide a module or make it visible

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
                  (zero = hide the module in all projects)
               [1]Module ID              - Integer
               [2]User ID                - Integer
               [3]Hide                   - Boolean

  Response: 0: [0]success?               - Boolean
                  (if the client asks for hide and the module is checked out
                   do nothing and return false)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectHIDE_UNHIDE_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  UserID,
  ModuleID,
  ProjectID: Integer;
  IsReadOnly,
  Hide: Boolean;
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
    ModuleID := StrToInt(FRequestBuffer.Fields[1]);
    UserID := StrToInt(FRequestBuffer.Fields[2]);
    Hide := (FRequestBuffer.Fields[3] = '1');;

    FResponseBuffer.Rewrite;
    IsReadOnly := False;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if the module is checked out
        Close;
        SQL.Clear;
        SQL.Add('SELECT readonly FROM modules');
        SQL.Add('WHERE moduleid = :moduleid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        if not Eof then IsReadOnly := (FieldByName('readonly').AsString = '1');
        Close;
        if not IsReadOnly then
        begin
          // module is available, return true
          FResponseBuffer.WriteFields(True, [True]);
          // set or clear the hidden flag
          Close;
          SQL.Clear;
          SQL.Add('UPDATE pjmodule');
          SQL.Add('SET hidden = :hidden');
          SQL.Add('WHERE moduleid = :moduleid');
          if ProjectID > 0 then
            SQL.Add('AND projectid = :projectid');
          ParamByName('moduleid').AsInteger := ModuleID;
          if ProjectID > 0 then
            ParamByName('projectid').AsInteger := ProjectID;
          if Hide
            then ParamByName('hidden').AsString := '1'
              else ParamByName('hidden').AsString := '0';
          ExecSQL;
          // update project information
          Close;
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end // if not IsReadOnly then begin
        else
        begin
          // return false
          FResponseBuffer.WriteFields(True, [False]);
        end; // else not IsReadOnly then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;      
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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
  Change comment fields for a single revision of a module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Revision ID            - Integer
               [1]Comment type           - String(1)
                  ('i' = Check in comment,
                   'o' = Check out comment)
               [2]Comment value          - String

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectCHANGE_REVISION_COMMENT.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  RevisionID: Integer;
  Comment,
  CommentType: string;
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

    RevisionID := StrToInt(FRequestBuffer.Fields[0]);
    CommentType := AnsiLowerCase(FRequestBuffer.Fields[1]);
    Comment := FRequestBuffer.Fields[2];

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('UPDATE revision');
        case CommentType[1] of
          'i': SQL.Add('SET comment_i = :new_comment');
          'o': SQL.Add('SET comment_o = :new_comment');
        end;
        SQL.Add('WHERE revisionid = :revisionid');
        ParamByName('revisionid').AsInteger := RevisionID;
        ParamByName('new_comment').AsString := Comment;
        ExecSQL;
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
  Get history for a single module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer

  Response: 0: [0]Module description     - String

            1: [0]Module checked out     - String(1)
                  ('1' = checked out, '0' = not checked out)
               [1]Version                - Integer
               [2]Revision               - Integer
               [3]User name              - String(50)
                 (only the first member of a revision needs a value)
               [4]Timestamp              - Double (TDateTime)
               [5]Size                   - Integer
               [6]Extension              - String(20)
               [7]Check In comment       - String
                 (only the first member of a revision needs a value)
               [8]Check Out comment      - String
                 (only the first member of a revision needs a value)
               [9]Revision ID            - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_MODULE_HISTORY.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  ModuleID,
  CurrentVersion,
  CurrentRevision: Integer;
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

    ModuleID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // Step 1: Module description
        SQL.Clear;
        SQL.Add('SELECT description FROM modules');
        SQL.Add('WHERE moduleid = :moduleid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        if not Eof then FResponseBuffer.WriteFields(True, [FQuery.Fields[0].AsString])
          else FResponseBuffer.WriteFields(True, ['']);
        Close;
        // Step 2: Module's versions
        SQL.Clear;
        SQL.Add('SELECT modules.readonly, revision.version,');
        SQL.Add('revision.revision, users.login, blobs.origtime,');
        SQL.Add('blobs.origsize, blobs.extension, revision.comment_i,');
        SQL.Add('revision.comment_o, revision.revisionid');
        SQL.Add('FROM modules, revision, blobs, users');
        SQL.Add('WHERE modules.moduleid = :moduleid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND revision.revisionid = blobs.revisionid');
        SQL.Add('AND revision.userid = users.userid');
        SQL.Add('ORDER BY revision.version, revision.revision, blobs.blobid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        while not Eof do
        begin
          CurrentVersion := FQuery.FieldByName('version').AsInteger;
          CurrentRevision := FQuery.FieldByName('revision').AsInteger;
          // copy the first record - include all informations
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   DT := FQuery.FieldByName('origtime').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0,
                                                    [FQuery.Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FQuery.FieldCount - 1 do begin
          if not Eof then Next;
          // copy the rest of this revision
          while (not Eof) and
                (CurrentVersion = FQuery.FieldByName('version').AsInteger) and
                (CurrentRevision = FQuery.FieldByName('revision').AsInteger) do
          begin
            {  return user name & comments as blank strings to prevent the
               transfer of unneccessary data  }
            for Fld := 0 to FQuery.FieldCount - 1 do
            begin
              case Fld of
                3, 7, 8: FResponseBuffer.WriteFields(False, ['']);
                // MWBuffer workaround for DateTime fields
                4: begin
                     DT := FQuery.FieldByName('origtime').AsDateTime;
                     FResponseBuffer.WriteFields(False, [DT]);
                   end;
                else FResponseBuffer.WriteFields(Fld = 0,
                                                    [FQuery.Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 0 to FQuery.FieldCount - 1 do begin
            Next; // of this revision
          end; // while (not EoF) and...
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
  Get module's ID by name

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module name            - String (incl. path);

  Response: 0: [0]Module ID             - Integer
                  (return 0 if the module was not found)
               [1]Error message          - String
                  (optional, only if the module was not found)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_MODULE_ID.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ModuleID: Integer;
  Module: string;
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

    Module := AnsiLowerCase(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    ModuleID := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT moduleid FROM modules');
        SQL.Add('WHERE name = :name');
        SQL.Add('AND path = :path');
        ParamByName('name').AsString := ExtractFileName(Module);
        ParamByName('path').AsString := ExtractFilePath(Module);
        Open;
        if not Eof then ModuleID := FQuery.FieldByName('moduleid').AsInteger;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;      
    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
    FResponseBuffer.WriteFields(True, [ModuleID]);
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
  Get module's name by ID

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer

  Response: 0: [0]Module name            - String(255) + String(255)
                  (return a blank if the module was not found)
               [1]Error message          - String
                  (optional, only if the module was not found)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_MODULE_NAME.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ModuleID: Integer;
  Module: string;
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

    ModuleID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    Module := '';
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT path, name FROM modules');
        SQL.Add('WHERE moduleid = :moduleid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        if not Eof then
          Module := FieldByName('path').AsString + FieldByName('name').AsString;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;      
    FResultStatus := 200; // OK
    FResponseBuffer.WriteFields(True, [Module]);
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
  Change a module's name (w/o changing the path)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer
               [1]New module name        - Sring
               [2]Project ID             - Integer

  Response: 0: [0]Success                - Boolean
               [1]Error message          - String
                  (optional, only if Success = false)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectRENAME_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  ModuleID: Integer;
  NewName,
  OldName, ModulePath: string;
  CanRename, IsExistingModule: Boolean;
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

    ModuleID := StrToInt(FRequestBuffer.Fields[0]);
    NewName := AnsiLowerCase(FRequestBuffer.Fields[1]);
    ProjectID := StrToInt(FRequestBuffer.Fields[2]);

    FResponseBuffer.Rewrite;
    CanRename := False;
    IsExistingModule := False;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if the module is locked
        Close;
        SQL.Clear;
        SQL.Add('SELECT name, readonly, path FROM modules');
        SQL.Add('WHERE moduleid = :moduleid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        if not Eof then
        begin
          OldName := FieldByName('name').AsString;
          CanRename := (FieldByName('readonly').AsString = '0');
          ModulePath := FieldByName('path').AsString;
        end;
        Close;
        if CanRename then
        begin
          Close;
          SQL.Clear;
          SQL.Add('SELECT m.moduleid FROM modules m');
          SQL.Add('WHERE m.path = :path');
          SQL.Add('AND   m.name = :name');
          ParamByName('path').AsString := ModulePath;
          ParamByName('name').AsString := NewName;
          Open;
          IsExistingModule := (not Eof) and (FieldByName('moduleid').AsInteger <> ModuleID);
          CanRename := not IsExistingModule;
          Close;
          if not IsExistingModule then
          begin
            // return true
            FResponseBuffer.WriteFields(True, [True]);
            // return blank error message
            FResponseBuffer.WriteFields(False, ['']);
            // rename the module
            SQL.Clear;
            SQL.Add('UPDATE modules SET name = :newname');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('newname').AsString := NewName;
            ExecSQL;
          end;
        end
        else
        begin
          // return false
          FResponseBuffer.WriteFields(True, [False]);
          // return error message
          FResponseBuffer.WriteFields(False, ['Module is locked.']);
        end;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
    begin
      if CanRename then
        VCSLog(UData.DBPath, ProjectID, AccessID, ModuleID, 0, 'c',
          'Module renamed from ' + OldName + ' to ' + NewName);
    end; // if UData.WriteVCSLog....
    if IsExistingModule then
    begin
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields ( False
                                  , [Format ( SrvException
                                            , [ FFunctionCode
                                              , Format('Conflict for module <%s> detected:', [ExtractFileName(OldName)]) + #13#13 +
                                                Format('There is already a module with the name <%s> in folder <%s>!', [NewName, ModulePath])
                                              ]
                                            )
                                    ]
                                  );
    end
    else
    begin
      // update archive time stamp
      UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
      FResultStatus := 200; // OK
    end;
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
  Get module IDs/ paths where the name matches a given name

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module name            - String (without path)

  Response: 0: [0]Module ID             - Integer
               [1]Module path           - String(250)

            1: [next module...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_MODULES_LIKE.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  Fld,
  TANr,
  AccessID,
  GrantedRights: Integer;
  Module: string;
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

    Module := AnsiLowerCase(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT moduleid, path FROM modules');
        SQL.Add('WHERE name = :name');
        ParamByName('name').AsString := Module;
        Open;
        while not Eof do
        begin
          // return the project ID's and paths
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
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
  Checkout a version/revision of a module
  (& all revision members assigned with this version/revision)
  Checkout:
  - check the read-only flag first, set to '1' after Checkout
  - update project's history & vcslog

  This is in principle the same function as in GET_CHECKOUT_MODULE, but does
  only lock the module and do not return the binaries to the client (much more
  faster).

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Revision ID            - Integer
               [4]Module name            - String(255)
                 (only to complete the log file)

  Response: 0: [0]Check Out allowed?      - Boolean
                  (only if Check Out is requested, return true on Get)
               [1]Owner                   - String(50)
                  (only if Check Out is requested, return a blank on Get)

            1: [0]Module Extension        - String(20)

            2: [0]next Module
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectCHECKOUT_ONLY_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  UserID,
  ModuleID,
  RevisionID,
  ProjectID: Integer;
  CanCheckOut: Boolean;
  ModuleName,
  AffectedFiles: string;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    RevisionID := StrToInt(FRequestBuffer.Fields[3]);
    ModuleName := FRequestBuffer.Fields[4];

    FResponseBuffer.Rewrite;
    CanCheckOut := False;
    AffectedFiles := '';
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if the module is available for check out
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.readonly, modules.locktstamp, users.login');
        SQL.Add('FROM modules, users');
        SQL.Add('WHERE modules.moduleid = :moduleid');
        SQL.Add('AND users.userid = modules.userid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        if not Eof then
          CanCheckOut := (FieldByName('readonly').AsString = '0');
        if not CanCheckOut then
        begin
          if not Eof then
          begin
            // Module is already checked out, return false
            FResponseBuffer.WriteFields(True, [False]);
            // return the module's owner & timestamp
            FResponseBuffer.WriteFields(False, [FieldByName('login').AsString]);
            // MWBuffer workaround for DateTime fields
            DT := FQuery.FieldByName('locktstamp').AsDateTime;
            FResponseBuffer.WriteFields(False, [DT]);
            // return affected modules (none)
            FResponseBuffer.WriteFields(True, ['']);
          end; // if not EoF then begin
          Close;
        end // if not CanCheckOut then begin
        else
        begin
          // Module is available, return true
          FResponseBuffer.WriteFields(True, [True]);
          // return the module's owner (blank) & timestamp (now)
          FResponseBuffer.WriteFields(False, ['']);
          FResponseBuffer.WriteFields(False, [LocalDT2GMTDT(Now)]);
          Close;
          // Lock the module first
          SQL.Clear;
          SQL.Add('UPDATE modules');
          SQL.Add('SET readonly = ''1'',');
          SQL.Add('userid = :userid, lastuser = :lastuser,');
          SQL.Add('tstamp = :tstamp,');
          SQL.Add('locktstamp = :locktstamp');
          SQL.Add('WHERE moduleid = :moduleid');
          ParamByName('userid').AsInteger := UserID;
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('moduleid').AsInteger := ModuleID;
          ExecSQL;
          // Module is available for check out, get affected files
          SQL.Clear;
          SQL.Add('SELECT extension');
          SQL.Add('FROM blobs');
          SQL.Add('WHERE revisionid = :revisionid');
          ParamByName('revisionid').AsInteger := RevisionID;
          Open;
          while not Eof do
          begin
            // save affected files
            AffectedFiles := AffectedFiles + ' / ' +
                                      ChangeFileExt(ExtractFileName(ModuleName),
                                             FieldByName('extension').AsString);
            // return affected file (extensions)
            FResponseBuffer.WriteFields(True,
                                           [FieldByName('extension').AsString]);
            Next; // revision member
          end; // while not EoF do begin
          Close;
          // save statistic data
          Inc(UData.CheckOutCount);
        end; // else if not CanCheckOut then begin
        // update project information
        if CanCheckOut then
        begin
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end; // if CanCheckOut then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
    begin
      if CanCheckOut then
        VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, 0, 'o',
          'Check out module: ' + ModuleName + ' - Affected files: ' + AffectedFiles);
    end; // if UData.WriteVCSLog....
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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
  Undo Checkout a version/revision of a module
  (& all revision members assigned with this version/revision)
  - check read-only flag & userid first, set to '0' after Undo Checkout
  - update project's history & vcslog

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer  

            1: [0]Project ID             - Integer
               [1]User ID                - Integer
               [2]Module ID              - Integer
               [3]Module name            - String(255)
                 (only to complete the log file)

  Response: 0: [0]Undo Check Out allowed? - Boolean
               [1]Owner                   - String(50)
               [2]Error message           - String

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectUNDO_CHECKOUT_MODULE.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  UserID,
  ModuleID,
  ProjectID,
  RevisionID: Integer;
  CanUndo,
  IsCheckedOut: Boolean;
  ModuleName: string;
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
    UserID := StrToInt(FRequestBuffer.Fields[1]);
    ModuleID := StrToInt(FRequestBuffer.Fields[2]);
    ModuleName := FRequestBuffer.Fields[3];

    FResponseBuffer.Rewrite;
    IsCheckedOut := False;
    CanUndo := False;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if the module is available for undo check out
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.readonly, modules.userid,');
        SQL.Add('users.login');
        SQL.Add('FROM modules, users');
        SQL.Add('WHERE modules.moduleid = :moduleid');
        SQL.Add('AND users.userid = modules.userid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        if not Eof then
          IsCheckedOut := (FieldByName('readonly').AsString = '1');
        if IsCheckedOut then
        begin
          // checked out by current user or requested by Project Admin?
          if (GrantedRights > 2) then CanUndo := True
            else CanUndo := (FQuery.FieldByName('userid').AsInteger = UserID);
          if CanUndo then
          begin
            {  User is owner or at least Project Admin
               -> undo checkout, return true  }
            FResponseBuffer.WriteFields(True, [True]);
            // Unlock the module
            Close;
            SQL.Clear;
            SQL.Add('UPDATE modules');
            SQL.Add('SET readonly = ''0'',');
            SQL.Add('userid = :userid, lastuser = :lastuser,');
            SQL.Add('tstamp = :tstamp,');
            SQL.Add('locktstamp = :locktstamp');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('userid').AsInteger := UserID;
            ParamByName('lastuser').AsInteger := UserID;
            ParamByName('locktstamp').AsDateTime := 0;
            ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;

            //get revisionid of latest revision
            RevisionID := -1;
            SQL.Clear;
            //USc 25.12.2004 normally the fields version and revision are obsolete
            // in the SELECT clause but DBISAM requires them
            // in order to get the ORDER BY working (otherwise exception)
            SQL.Add('SELECT revisionid, version, revision FROM revision');
            SQL.Add('WHERE moduleid = :moduleid');
            SQL.Add('ORDER BY revision.version DESC, revision.revision DESC');
            ParamByName('moduleid').AsInteger := ModuleID;
            Open;
            if not Eof then
              RevisionID := FieldByName('revisionid').AsInteger;
            Close;
            //remove the checkout comment in the latest revision
            if RevisionID <> -1 then
            begin
              SQL.Clear;
              SQL.Add('UPDATE revision');
              SQL.Add('SET comment_o = NULL');
              SQL.Add('WHERE revisionid = :revisionid');
              ParamByName('revisionid').AsInteger := RevisionID;
              ExecSQL;
            end;

          end // if CanUndo then begin
          else
          begin
            // User is not owner, access denied
            FResponseBuffer.WriteFields(True, [False]);
            // return the module's owner
            FResponseBuffer.WriteFields(False, [FieldByName('login').AsString]);
            // return error message
            FResponseBuffer.WriteFields(False, ['Access denied.']);
            Close;
          end; // else if CanUndo then begin
        end // if IsCheckedOut then begin
        else
        begin
          // Module is not checked out, nothing to undo, return false
          FResponseBuffer.WriteFields(True, [False]);
          // return a blank as the module's owner
          FResponseBuffer.WriteFields(False, ['']);
          // return error message
          FResponseBuffer.WriteFields(False, ['Module not checked out.']);
          Close;
        end; // else if IsCheckedOut then begin
        // update project information
        if CanUndo then
        begin
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
        end; // if CanUndo then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and CanUndo then
      VCSLog(UData.DBPath, ProjectID, UserID, ModuleID, 0, 'u',
        'Undo check out module');
    // update archive time stamp
    UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
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
  Get comment fields for a single revision of a module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Revision ID            - Integer

  Response: 0: [0]Check In comment       - String
               [1]Check Out comment      - String
               [2]Version State          - Integer

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_REVISION_COMMENT.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  RevisionID: Integer;
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

    RevisionID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        SQL.Add('SELECT revision.comment_i, revision.comment_o,');
        SQL.Add('revision.verstate');
        SQL.Add('FROM revision');
        SQL.Add('WHERE revision.revisionid = :revisionid');
        ParamByName('revisionid').AsInteger := RevisionID;
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
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
  This object is tested and optimized for use with DBISAM.

  Get a list of revisions for a single file (by module ID)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer
               [1]Project ID             - Integer
                  (if this value is 0 (not NULL!) server should return
                   the revisions unfiltered -> shared modules
                   otherwise filtered for this project only)

  Response: 0: [0]Module ID              - Integer
                  (Server should return 0 if no module matches the query)
               [1]Module name            - String(255)
                  (only the first record needs a value)
               [2]Path                   - String(255)
                  (only the first record needs a value)
               [3]Checked Out            - Boolean
               [4]TimeStamp              - Double (TDateTime)
                  (only the first record needs a value)
               [5]UserID                 - Integer
               [6]Revision ID            - Integer
               [7]Version                - Integer
               [8]Revision               - Integer
               [9]Owner                  - String(50)
                  (Server may return a blank if not Checked Out)
              [10]Project ID              - Integer
              [11]Hidden                  - Boolean

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_REVISION_LIST_BY_ID.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  ProjectID,
  ModuleID: Integer;
  CheckedOut: Boolean;
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

    ModuleID := StrToInt(FRequestBuffer.Fields[0]);
    ProjectID := StrToInt(FRequestBuffer.Fields[1]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name, modules.path,');
        SQL.Add('modules.readonly, modules.tstamp, modules.userid,');
        SQL.Add('revision.revisionid, revision.version, revision.revision,');
        SQL.Add('users.login, projects.projectid, pjmodule.hidden');
        SQL.Add('FROM pjmodule');
        SQL.Add('INNER JOIN projects    ON projects.projectid = pjmodule.projectid');
        SQL.Add('INNER JOIN modules     ON modules.moduleid = pjmodule.moduleid');
        SQL.Add('INNER JOIN revision    ON revision.moduleid = modules.moduleid');
        SQL.Add('LEFT OUTER JOIN users  ON modules.userid = users.userid');
        SQL.Add('WHERE pjmodule.moduleid = :moduleid');
        if ProjectID <> 0 then
          SQL.Add('AND pjmodule.projectid = :projectid');
        SQL.Add('ORDER BY revision.version, revision.revision, projects.projectid');
        ParamByName('moduleid').AsInteger := ModuleID;
        if ProjectID <> 0 then
        begin
          ParamByName('projectid').AsInteger := ProjectID;
        end;
        Open;
        if not Eof then
        begin // any records match our Query?
          // yes, first record - insert name & path
          CheckedOut := (FieldByName('readonly').AsString = '1');
          FResponseBuffer.WriteFields(True, [FQuery.FieldByName('moduleid').AsInteger]);
          FResponseBuffer.WriteFields(False, [FieldByName('name').AsString]);
          FResponseBuffer.WriteFields(False, [FieldByName('path').AsString]);
          // Copy the rest from the record to the response
          for Fld := 3 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   DT := FQuery.FieldByName('tstamp').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              // insert user only if checked out
              9: if CheckedOut
                    then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                      else FResponseBuffer.WriteFields(False, ['']);
                      // left user empty
              else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 3 to FieldCount - 1 do begin
          if not Eof then Next;
          // further records - left name & path blank
          while not Eof do
          begin
            CheckedOut := (FieldByName('readonly').AsString = '1');
            FResponseBuffer.WriteFields(True, [FQuery.FieldByName('moduleid').AsInteger]);
            FResponseBuffer.WriteFields(False, ['']); // name
            FResponseBuffer.WriteFields(False, ['']); // path
            // Copy the rest from the record to the response
            for Fld := 3 to FieldCount - 1 do
            begin
              case Fld of
              // MWBuffer workaround for DateTime fields
                4: begin
                     DT := FQuery.FieldByName('tstamp').AsDateTime;
                     FResponseBuffer.WriteFields(False, [DT]);
                   end;
                // insert user only if checked out
                9: if CheckedOut
                      then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                        else FResponseBuffer.WriteFields(False, ['']);
                        // left user empty
                else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 3 to FieldCount - 1 do begin
            Next;
          end; // while not EoF do begin
        end; // if not EoF then begin
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
  Get a list of revisions for a single project assigned to one version/revision
  or the latest revisions from one version

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Version                - Integer
               [2]Revision               - Integer
                 (if this value is -1 client requests the newest
                  revisions from [1]Version, otherwise a specific revision)
               [3]excl. hidden modules   - Boolean

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Path                   - String(255)
               [3]Checked Out            - String(1)
                  ('1' = checked out, '0' = not checked out)
               [4]TimeStamp              - Double (TDateTime)
               [5]Revision ID            - Integer
               [6]Version                - Integer
               [7]Revision               - Integer
               [8]Owner                  - String(50)
                  (Server may return a blank if not Checked Out)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_REVISION_LIST_BY_VERSION.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  ProjectID,
  Version_, // name clash with TFFQuery.Version (-- EB)
  Revision,
  CurrentModuleID: Integer;
  ExclHidden: Boolean;
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
    Version_ := StrToInt(FRequestBuffer.Fields[1]);
    Revision := StrToInt(FRequestBuffer.Fields[2]);
    ExclHidden := (FRequestBuffer.Fields[3] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name, modules.path,');
        SQL.Add('modules.readonly, modules.locktstamp, revision.revisionid,');
        SQL.Add('revision.version, revision.revision, users.login');
        SQL.Add('FROM pjmodule, revision, modules, users');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND revision.version = :version');
        if Revision > -1 then // all revisions?
          SQL.Add('AND revision.revision = :revision');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND users.userid = modules.userid');
        SQL.Add('ORDER BY modules.moduleid DESC, revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('version').AsInteger := Version_;
        if Revision > -1 then
          ParamByName('revision').AsInteger := Revision;
        Open;
        while not Eof do
        begin
          // Filter out the latest revision
          CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
          // Copy the latest revision to the response
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   DT := FQuery.FieldByName('locktstamp').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              // insert user only if checked out
              8: if (FieldByName('readonly').AsString = '1')
                    then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                      else FResponseBuffer.WriteFields(False, ['']);
                      // left user empty
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FieldCount - 1 do begin
          // skip the rest of this module
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger) do Next;
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
  Get status for a single revision of a module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Revision ID            - Integer

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Module path            - String(255)
               [3]Checked Out            - String(1) '1' = true, '0' = false
               [4]Version                - Integer
               [5]Revision               - Integer
               [6]Revision orig. time    - Double (TDateTime)
               [7]Revision orig. size    - Integer
               [8]Revision orig. CRC     - Integer
               [9]Revision compr. size   - Integer
              [10]Revision Extension     - String(20)

            2: [0]next Revision member
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_REVISION_STATUS.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  RevisionID: Integer;
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

    RevisionID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name,');
        SQL.Add('modules.path, modules.readonly,');
        SQL.Add('revision.version, revision.revision,');
        SQL.Add('blobs.origtime, blobs.origsize, blobs.origcrc,');
        SQL.Add('blobs.compsize, blobs.extension');
        SQL.Add('FROM revision, modules, blobs');
        SQL.Add('WHERE revision.revisionid = :revisionid');
        SQL.Add('AND modules.moduleid = revision.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        ParamByName('revisionid').AsInteger := RevisionID;
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              6: begin
                   DT := FQuery.FieldByName('origtime').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FQuery.FieldCount - 1 do begin
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
  Get blob status for all revisions of a single module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID               - Integer

  Response: 0: [0]Revision ID            - Integer
               [1]Version                - Integer
               [2]Revision               - Integer
               [3]Revision orig. time    - Double (TDateTime)
               [4]Revision orig. size    - Integer
               [5]Revision orig. CRC     - Integer
               [6]Revision compr. size   - Integer
               [7]Revision Extension     - String(20)

            2: [0]next Revision member
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_BLOB_STATUS.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  ModuleID: Integer;
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

    ModuleID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        SQL.Add('SELECT revision.revisionid, revision.version,');
        SQL.Add('revision.revision,');
        SQL.Add('blobs.origtime, blobs.origsize, blobs.origcrc,');
        SQL.Add('blobs.compsize, blobs.extension');
        SQL.Add('FROM revision, blobs');
        SQL.Add('WHERE revision.moduleid = :moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        SQL.Add('ORDER BY revision.version, revision.revision');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              3: begin
                   DT := FQuery.FieldByName('origtime').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FQuery.FieldCount - 1 do begin
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
  Get list of revisions for a single file (by name/path)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module name            - String (incl. path)
               [1]Project ID             - Integer
                  (if this value is 0 (not NULL!) server should return
                   the revisions unfiltered -> shared modules
                   otherwise filtered for this project only)

  Response: 0: [0]Module ID              - Integer
                  (Server should return 0 if no module matches the query)
               [1]Module name            - String(255)
                  (Server may return a blank because the client already knows
                   this value)
               [2]Path                   - String(255)
                  (Server may return a blank...)
               [3]Checked Out            - Boolean
               [4]TimeStamp              - Double (TDateTime)
                  (only the first record needs a value)
               [5]UserID                 - Integer
               [6]Revision ID            - Integer
               [7]Version                - Integer
               [6]Revision               - Integer
               [9]Owner                  - String(50)
                  (Server may return a blank if not Checked Out)
              [10]Project ID              - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_REVISION_LIST_BY_NAME.Execute;
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
  Module: string;
  CheckedOut: Boolean;
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

    Module := AnsiLowerCase(FRequestBuffer.Fields[0]);
    ProjectID := StrToInt(FRequestBuffer.Fields[1]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid,');
        SQL.Add('modules.readonly, modules.tstamp, modules.userid,');
        SQL.Add('revision.revisionid, revision.version, revision.revision,');
        SQL.Add('users.login, projects.projectid');
        SQL.Add('FROM modules, projects, pjmodule, revision');
        SQL.Add('LEFT OUTER JOIN users ON modules.userid = users.userid');
        SQL.Add('WHERE modules.name = :module');
        SQL.Add('AND modules.path = :path');
        if ProjectID <> 0 then
          SQL.Add('AND projects.projectid = :projectid');
        SQL.Add('AND projects.projectid = pjmodule.projectid');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        SQL.Add('AND modules.moduleid = revision.moduleid');
        SQL.Add('ORDER BY revision.version, revision.revision,');
        SQL.Add('projects.projectid');
        ParamByName('module').AsString := ExtractFileName(Module);
        ParamByName('path').AsString := ExtractFilePath(Module);
        if ProjectID <> 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        Open;
        if not Eof then
        begin // any records match our Query?
          // yes, first record - insert timestamp
          CheckedOut := (FieldByName('readonly').AsString = '1');
          FResponseBuffer.WriteFields(True,
                                           [FQuery.FieldByName('moduleid').AsInteger]);
          {  return blank name & path because the client already knows
             these values  }
          FResponseBuffer.WriteFields(False, ['']);
          FResponseBuffer.WriteFields(False, ['']);
          // Copy the rest from the record to the response
          for Fld := 1 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              2: begin
                   DT := FQuery.FieldByName('tstamp').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              // insert user only if checked out
              7: if CheckedOut
                    then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                      else FResponseBuffer.WriteFields(False, ['']);
                      // left user empty
              else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 3 to FieldCount - 1 do begin
          if not Eof then Next;
          // further records - left timestamp blank
          while not Eof do
          begin
            CheckedOut := (FieldByName('readonly').AsString = '1');
            FResponseBuffer.WriteFields(True,
                                           [FQuery.FieldByName('moduleid').AsInteger]);
            {  return blank name & path because the client already knows
               these values  }
            FResponseBuffer.WriteFields(False, ['']);
            FResponseBuffer.WriteFields(False, ['']);
            // Copy the rest from the record to the response
            for Fld := 1 to FieldCount - 1 do
            begin
              case Fld of
                // Timestamp: Insert zero
                2: FResponseBuffer.WriteFields(False, [0]);
                // insert user only if checked out
                7: if CheckedOut
                      then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                        else FResponseBuffer.WriteFields(False, ['']);
                        // left user empty
                else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 3 to FieldCount - 1 do begin
            Next;
          end; // while not EoF do begin
        end; // if not EoF then begin
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
  This object is tested and optimized for use with DBISAM.

  Get all shared modules (modules assigned to more than one project ID)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Module ID              - Integer

            1: [next Module ID...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_SHARED_MODULES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
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

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT COUNT(*) AS modcount, moduleid');
        SQL.Add('FROM pjmodule');
        SQL.Add('GROUP BY moduleid');
        //mantis #685 - performance suggestion from HDo
        //actually tested with Ora, Ib, Fb, DBIsam:
        //Todo: Other DBMS port maintainers please check if working!
        SQL.Add('HAVING COUNT(moduleid) > 1');
        Open;
        while not Eof do
        begin
          // ModuleCount > 1 = shared module
          if (FQuery.FieldByName('modcount').AsInteger > 1) then
            FResponseBuffer.WriteFields(True, [FQuery.FieldByName('moduleid').AsInteger]);
          Next;
        end;
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
  Get a single blob field (a single family member) from one revision

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Revision ID            - Integer
               [1]Extension              - String(20)

  Response: 0: [0]Module orig. time       - Double (TDateTime)
               [1]Module orig. size       - Integer
               [2]Module orig. CRC        - Integer
               [3]Module compr. size      - Integer
               [4]Module Extension        - String(20)
               [5]Module Binary           - Stream

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_SINGLE_BLOB.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  RevisionID: Integer;
  Extension: string;
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

    RevisionID := StrToInt(FRequestBuffer.Fields[0]);
    Extension := AnsiLowerCase(FRequestBuffer.Fields[1]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        {$IFNDEF ORACLESRV}
        SQL.Add('SELECT origtime, origsize, origcrc, compsize,');
        SQL.Add('extension, filedata');
        SQL.Add('FROM blobs');
        SQL.Add('WHERE revisionid = :revisionid');
        SQL.Add('AND extension = :extension');
        {$ELSE}
        // Oracle needs SQL in uppercase if RequestLive = true
        SQL.Add('SELECT ORIGTIME, ORIGSIZE, ORIGCRC, COMPSIZE,');
        SQL.Add('EXTENSION, FILEDATA');
        SQL.Add('FROM BLOBS');
        SQL.Add('WHERE REVISIONID = :revisionid');
        SQL.Add('AND EXTENSION = :extension');
        {$ENDIF ~ORACLESRV}
        ParamByName('revisionid').AsInteger := RevisionID;
        ParamByName('extension').AsString := Extension;
        Open;
        if not Eof then
        begin
          // MWBuffer workaround for DateTime fields
          DT := FQuery.FieldByName('origtime').AsDateTime;
          FResponseBuffer.WriteFields(True, [DT]);
          // Copy common fields
          for Fld := 1 to FQuery.FieldCount - 2 do
            FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
          // Copy stream field
          FileDataBlobToMWBuffer(FQuery, FResponseBuffer);
        end; // if not EoF then begin
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
  Check if a module is member of a project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Module name & path     - String(255) + String(255)

  Response: 0: [0]Is member of           - Boolean
               [1]Module ID              - Integer
                  (only if 'Is member of = true', otherwise return 0)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectIS_MEMBER_OF_PROJECT.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  ModuleID: Integer;
  Module: string;
  IsMemberOf: Boolean;
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
    Module := AnsiLowerCase(FRequestBuffer.Fields[1]);

    ModuleID := 0;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid');
        SQL.Add('FROM pjmodule, modules');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND modules.name = :module');
        SQL.Add('AND modules.path = :path');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('module').AsString := ExtractFileName(Module);
        ParamByName('path').AsString := ExtractFilePath(Module);
        Open;
        IsMemberOf := not Eof;
        if not Eof then ModuleID := FQuery.FieldByName('moduleid').AsInteger;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;      
    // result
    FResponseBuffer.WriteFields(True, [IsMemberOf]);
    FResponseBuffer.WriteFields(False, [ModuleID]);
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

end.

