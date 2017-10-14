{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SvrOBJProjects.pas

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
2003/03/07  THuber   - #702: SQL92 fix for Firebird port in GET_REVISION_LIST
                       (Test: project history|project tree => (400) no record ...)
2003/10/21  THuber   - #1181 - project groups entries were not deleted and caused
                       'not found' entries in project hierarchy
2004/01/17  USchuster- fixed GET_REVISION_LIST for MySQL
                       (MySQL don't like different casings of tablenames in a statement)
2004/01/17  USchuster- fixed GET_REVISION_LIST for MySQL
2004/07/18  THuber   - #1965 - added ArchiveTimestamp where necessary

--- branchpoint for 2.5 dev server ---

2004/12/08  USchuster- style cleaning
                     - simplified creation of insert statements
2005/05/06  USchuster- added jedi.inc
2005/05/08  USchuster- removed unused units
2005/07/04  USchuster- TFVCSServerObject.VCSLog is used now to create log entries(mantis #2631)

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
                     special Oracle J O I N  - Syntax was removed we now only
                     support the A N S I - J o i n Syntax and therefore removed
                     this compiler setting.
2010/01/11  USchuster- increased performance of GET_BLANK_MODULE_LIST (Mantis #5086)

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SvrOBJProjects - ServerObjects for JVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvOBJProjects;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Classes, SrvConst, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef, SrvAccessDef;

type
  TServerObjectGET_PROJECT_MODULE_LIST = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_BLANK_MODULE_LIST = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_SHARED_BY = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_NEW_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectRESTORE_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_REFERENCES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_PROJECT_REFERENCES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_PROJECT_REFERENCES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_INFORMATION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_LIST = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_VERSION_LIST = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_REVISION_LIST = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LATEST_REVISIONS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_VERSION_REVISION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_ROLLBACK_REVISIONS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LOCKED_MODULES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectRENAME_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_PROJECT_GROUP = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_PROJECT_GROUP = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_GROUP_INFORMATION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_REMOVE_PROJECT_TO_GROUP = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

implementation

{==============================================================================
  Add a new project
  - if the project already exists do nothing

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer
               [1]Project (only the name)- String(50)
               [2]Description            - String
                  (Description is optional - client does not need this value)

  Response: 0: [0]new project?           - Boolean
               [1]Project ID             - Integer
                  (if this is a real new project return the new project ID,
                   otherwise return the old)

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_NEW_PROJECT.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  UserID,
  ProjectID: Integer;
  Project,
  ProjectDescr: string;
  IsNewProject: Boolean;
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
    Project := AnsiLowerCase(FRequestBuffer.Fields[1]);
    ProjectDescr := FRequestBuffer.Fields[2];
    ProjectID := 0;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if we have already this project in the archive
        Close;
        SQL.Clear;
        SQL.Add('SELECT projectid FROM projects');
        SQL.Add('WHERE name = :project');
        ParamByName('project').AsString := Project;
        Open;
        IsNewProject := Eof;
        if not Eof then ProjectID := FQuery.FieldByName('projectid').AsInteger;
        Close;
      end; // with FQuery do begin
      if IsNewProject then
      begin
        // return true
        FResponseBuffer.WriteFields(True, [True]);
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO projects');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('projects')]));
          SQL.Add(':name, :created, :createdby, :lastuser,');
          SQL.Add(':lastaccess, :history, :deleted, :description)');
          ParamByName('name').AsString := Project;
          ParamByName('created').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('createdby').AsInteger := UserID;
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('deleted').AsString := '0';
          ParamByName('history').AsString := 'Project added at ' +
                                              DateTimeToStr(LocalDT2GMTDT(Now));
          ParamByName('description').AsString := ProjectDescr;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // get the new created project ID
          SQL.Clear;
          SQL.Add('SELECT projectid FROM projects');
          SQL.Add('WHERE name = :name');
          ParamByName('name').AsString := Project;
          Open;
          ProjectID := FQuery.FieldByName('projectid').AsInteger;
          Close;
        end; // with FQuery do begin
        // return the new project ID
        FResponseBuffer.WriteFields(False, [ProjectID]);
      end // if IsNewProject then begin
      else
      begin
        // return false
        FResponseBuffer.WriteFields(True, [False]);
        // return the old project ID
        FResponseBuffer.WriteFields(False, [ProjectID]);
      end; // else if IsNewProject then begin
      // update log file
      if UData.WriteVCSLog and IsNewProject then
        VCSLog(UData.DBPath, ProjectID, UserID, 0, 0, 'a', 'Added as new project');
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
  Remove a project from the archive
  This function does not really remove the project. For safety reasons (and
  because I'm a coward ;-) this function will only remove all links between
  modules and the project, between project related rights and the project, all
  project reference links and mark the project as 'deleted'.
  Modules left in the archive from this project may be removed by 'Deserted
  modules'.
  Projects cannot be removed if they contain checked out modules.


        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [1]Project ID             - Integer

  Response: 0: [0]Removed                - Boolean
               [1]Error message          - String
                  (only if Removed = false)

               (only if Removed = false)
            1: [0]Module ID              - Integer
               [1]Module name            - String(50)

            2: [next checked out module]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_PROJECT.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  Fld,
  AccessID,
  GrantedRights,
  ProjectID: Integer;
  OldHistory: string;
  CheckedOutModules: Boolean;
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
        // check if we have any checked out modules
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name');
        SQL.Add('FROM pjmodule, modules');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        SQL.Add('AND modules.readonly = ''1''');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        CheckedOutModules := not Eof;
        if CheckedOutModules then
        begin
          // checked out modules, return false
          FResponseBuffer.WriteFields(True, [False]);
          // return error message
          FResponseBuffer.WriteFields(False,
                                     ['Project contains checked out modules.']);
          // return the checked out modules
          while not Eof do
          begin
            for Fld := 0 to FieldCount - 1 do
              FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            Next;
          end; // while not EoF do begin
          Close;
        end // if CheckedOutModules then begin
        else
        begin
          Close;        
          // no checked out modules, return true
          FResponseBuffer.WriteFields(True, [True]);
          // return blank error message
          FResponseBuffer.WriteFields(False, ['']);
          {  delete all bug links for this projectid to prevent foreign
             key violation  }
          SQL.Clear;
          SQL.Add('DELETE FROM pjbugs');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // delete all project related rights
          SQL.Clear;
          SQL.Add('DELETE FROM pjusers');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // delete all project references
          SQL.Clear;
          SQL.Add('DELETE FROM pjref');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('OR reference = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // delete all module links
          SQL.Clear;
          SQL.Add('DELETE FROM pjmodule');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          //thu mantis #1181 - avoid not found entries in hierarchy
          // delete project links in project groups
          SQL.Clear;
          SQL.Add('DELETE FROM pjgroups');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // set the project's deleted flag
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET deleted = ''1''');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        end; // else if CheckedOutModules then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    if not CheckedOutModules then
    begin
      // update project information
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          // save old history
          SQL.Clear;
          SQL.Add('SELECT history FROM projects');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          Open;
          OldHistory := FieldByName('history').AsString;
          Close;
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess,');
          SQL.Add('history = :history, deleted =:deleted');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('lastuser').AsInteger := AccessID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('deleted').AsString := '1';
          ParamByName('history').AsString := OldHistory +
                           ' / Deleted at ' + DateTimeToStr(LocalDT2GMTDT(Now));
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // update log file
          if UData.WriteVCSLog then
            VCSLog(UData.DBPath, ProjectID, AccessID, 0, 0, 'r', 'Project removed');
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
    end; // if not CheckedOutModules then begin
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
  Restore a deleted project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [1]Project ID             - Integer

  Response: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectRESTORE_PROJECT.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID: Integer;
  OldHistory: string;
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
    // update project information
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // save old history
        SQL.Clear;
        SQL.Add('SELECT history FROM projects');
        SQL.Add('WHERE projectid = :projectid');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        OldHistory := FieldByName('history').AsString;
        Close;
        SQL.Clear;
        SQL.Add('UPDATE projects');
        SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess,');
        SQL.Add('history = :history, deleted =:deleted');
        SQL.Add('WHERE projectid = :projectid');
        ParamByName('lastuser').AsInteger := AccessID;
        ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
        ParamByName('deleted').AsString := '0';
        ParamByName('history').AsString := OldHistory +
                          ' / Restored at ' + DateTimeToStr(LocalDT2GMTDT(Now));
        ParamByName('projectid').AsInteger := ProjectID;
        ExecSQL;
        UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        // update log file
        if UData.WriteVCSLog then
          VCSLog(UData.DBPath, ProjectID, AccessID, 0, 0, 'a', 'Project restored');
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
  Add/ Update project references

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Update                 - Boolean
                 (Update = remove all old references before adding the new,
                  otherwise add only the new)

            2: [0]Reference ID           - Integer

            3: [0]next Reference ID      - Integer

  Response: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_PROJECT_REFERENCES.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID: Integer;
  Update: Boolean;
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
    Update := (FRequestBuffer.Fields[1] = '1');
    if not FRequestBuffer.Eof then FRequestBuffer.Next;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        // Update?
        if Update then
        begin
          // yes, remove all old entries
          SQL.Clear;
          SQL.Add('DELETE FROM pjref');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        end; // if UpDate then begin
        // add new entries
        while not FRequestBuffer.Eof do
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO pjref');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjref')]));
          SQL.Add(':projectid, :reference)');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('reference').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[0]);
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          FRequestBuffer.Next;
        end; // while not FRequestBuffer.EoF do begin
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
  Get list of all locked (checked out) modules from one or from all projects

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
                 (zero = return locked modules from all projects)
               [1]User ID                - Integer
                 (zero = return locked modules from all users)

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Path                   - String(255)
               [3]Version                - Integer
               [4]Revision               - Integer
               [5]Locked TimeStamp       - Double (TDateTime)
               [6]Owner                  - String(50)
               [7]Owner user ID          - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LOCKED_MODULES.Execute;
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
  UserID,
  CurrentModuleID: Integer;
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

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // First step: get ALL locked versions/revisions
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name,');
        SQL.Add('modules.path, revision.version, revision.revision,');
        SQL.Add('modules.locktstamp, users.login, users.userid');
        SQL.Add('FROM pjmodule, modules, revision, users');
        if ProjectID > 0 then
        begin
          SQL.Add('WHERE pjmodule.projectid = :projectid');
          SQL.Add('AND modules.readonly = ''1''');
        end else SQL.Add('WHERE modules.readonly = ''1''');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        if UserID > 0 then
          SQL.Add('AND users.userid = :userid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND users.userid = modules.userid');
        SQL.Add('ORDER BY modules.name DESC, modules.moduleid DESC,');
        SQL.Add('revision.version DESC, revision.revision DESC');
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        Open;
        // Second step: Filter out only the latest version/revision
        while not Eof do
        begin
          CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
          // get the latest version
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              5: begin
                   DT := FQuery.FieldByName('locktstamp').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FieldCount - 1 do begin
          if not Eof then Next;
          // skip the rest - all older revisions - of this module
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger)
            do Next;
          // -> next module
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
  Get information about a single or about all projects

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
                  (zero = all projects)

  Response: 0: [0]Project ID             - Integer
                  (return 0 if the project was not found)
               [1]Project name           - String(50)
               [2]Created at             - Double (TDateTime)
               [3]Created by             - String(50)
               [4]Last write access      - Double (TDateTime)
               [5]Last user              - String(50)
               [6]Description            - String
               [7]Project deleted?       - String(1)
                  ('1' = deleted, '0' = not deleted)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_PROJECT_INFORMATION.Execute;
const
  RequestedRights = LevelNone;
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
        SQL.Add('SELECT P.projectid, P.name, P.created,');
        SQL.Add('U1.login createdby, P.lastaccess,');
        SQL.Add('U2.login lastuser, P.description, P.deleted');
        SQL.Add('FROM projects P, users U1, users U2');
        SQL.Add('WHERE P.createdby = U1.userid');
        SQL.Add('AND P.lastuser = U2.userid');
        if ProjectID > 0 then
        begin
          SQL.Add('AND P.projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
        end;
        Open;
        ProjectID := 0;
        while not Eof do
        begin
          ProjectID := FQuery.FieldByName('projectid').AsInteger;
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              2: begin
                   DT := FQuery.FieldByName('created').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              4: begin
                   DT := FQuery.FieldByName('lastaccess').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0,
                                                    [FQuery.Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FQuery.FieldCount - 1 do begin
          Next;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResponseBuffer.WriteFields(False, [ProjectID]);
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
  Get a list of all projects in the version archive

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer
               [2]Purpose                - String(1)
                  (Purpose of the request:
                   blank - undefined,
                   'x' - remove project,
                   'r' - rename project,
                   'p' - purge project)

            1: [0]Include details        - Boolean

  Response: 0: [0]Project ID             - Integer
               [1]Project name           - String(50)
               [2]Project deleted?       - String(1)
                  ('1' = deleted, '0' = not deleted)
               [3]History                - String
                  (if InclDetails = true)
               [4]Description            - String
                  (if InclDetails = true)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_PROJECT_LIST.Execute;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  RequestedRights,
  GrantedRights,
  Fld: Integer;
  InclDetails: Boolean;
  Purpose: string;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    Purpose := AnsiLowerCase(FRequestBuffer.Fields[2]);
    if Length(Purpose) > 0 then
    begin
      case Purpose[1] of
        'x', 'r', 'p': RequestedRights := LevelAAdmin;
        else RequestedRights := LevelNone;
      end; // case Purpose of
    end else RequestedRights := LevelNone;
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

    InclDetails := (FRequestBuffer.Fields[0] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        if InclDetails
          then SQL.Add('SELECT projectid, name, deleted, history, description')
            else SQL.Add('SELECT projectid, name, deleted');
        SQL.Add('FROM projects');
        SQL.Add('ORDER BY name');
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
  Get list of all modules from one project - with or w/o revisions in
  the archive

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]excl. hidden modules   - Boolean

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Path                   - String(255)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_PROJECT_MODULE_LIST.Execute;
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
  ExclHidden: Boolean;
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
    ExclHidden := (FRequestBuffer.Fields[1] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name, modules.path');
        SQL.Add('FROM pjmodule, modules');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        if ExclHidden then // skip hiddden modules
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY modules.moduleid');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        while not Eof do
        begin
          // Copy the record to the response
          for Fld := 0 to FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
      end;
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
  Get list of all modules without revisions from one project
  - these are modules assigned with the current project, but never checked in
    (implement as '*.prl-files' in earlier versions)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]excl. hidden modules   - Boolean

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Path                   - String(255)
               [3]Hidden                 - String(1)
                  ('1' = hidden, '0' = visible)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_BLANK_MODULE_LIST.Execute;
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
  ExclHidden: Boolean;
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
    ExclHidden := (FRequestBuffer.Fields[1] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name,');
        SQL.Add('modules.path, pjmodule.hidden');
        SQL.Add('FROM pjmodule, modules');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        if ExclHidden then // skip hiddden modules
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND NOT EXISTS');
        SQL.Add('(SELECT 1 FROM revision');
        SQL.Add('WHERE revision.moduleid = pjmodule.moduleid)');
        SQL.Add('ORDER BY modules.moduleid');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
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

  Get list of the latest versions from one project
  - w/o blob information & file family members

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]excl. hidden modules   - Boolean

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Path                   - String(255)
               [3]Checked Out            - Boolean
               [4]TimeStamp              - Double (TDateTime)
               [5]UserID                 - Integer
               [6]Revision ID            - Integer
               [7]Version                - Integer
               [8]Revision               - Integer
               [9]Hidden                 - String(1)
                  ('1' = hidden, '0' = visible)
              [10]Owner                  - String(50)
                  (Server may return a blank if not Checked Out)
              [11]Revision count          - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_VERSION_LIST.Execute;
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
  CurrentModuleID,
  RevisionCount: Integer;
  CheckedOut,
  ExclHidden: Boolean;
  DT: Double;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    UData := PUserDataRecord(FUserData);
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
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
    ExclHidden := (FRequestBuffer.Fields[1] = '1');

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
        SQL.Add('pjmodule.hidden, users.login');
        SQL.Add('FROM pjmodule, modules, revision, users');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND users.userid = modules.userid');
        SQL.Add('ORDER BY modules.moduleid DESC, revision.version DESC,');
        SQL.Add('revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        while not Eof do
        begin
          // store actual module ID
          CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
          CheckedOut := (FieldByName('readonly').AsString = '1');
          RevisionCount := 0;
          {  Copy the first query record (because we are sorting
             version/revision descending this is also the latest) to the
             response  }
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   DT := FQuery.FieldByName('tstamp').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              // insert user only if module checked out
              10: if CheckedOut
                    then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                      else FResponseBuffer.WriteFields(False, ['']);
                      // left user empty
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 3 to FieldCount - 1 do begin
          {  skip all older versions from this module ID - we need only the
             number of different versions here  }
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger) do
          begin
            Next;
            Inc(RevisionCount);
          end;
          // store the number of versions
          FResponseBuffer.WriteFields(False, [RevisionCount]);
         // -> new module
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
  Get list of all revisions from one project (by version/revision)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]excl. hidden modules   - Boolean

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
                  (only the first record of a module ID needs a value)
               [2]Path                   - String(255)
                  (only the first record of a module ID needs a value)
               [3]Checked Out            - Boolean
               [4]TimeStamp              - Double (TDateTime)
                  (only the first record of a module ID needs a value)
               [5]UserID                 - Integer
               [6]Revision ID            - Integer
                  (same as version)
               [7]Version                - Integer
                  (only valid if Revision count > 0)
               [8]Revision               - Integer
                  (same as version)
               [9]Hidden                 - String(1)
                  ('1' = hidden, '0' = visible)
              [10]Owner                  - String(50)
                  (Server may return a blank if not Checked Out)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_REVISION_LIST.Execute;
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
  CurrentModuleID: Integer;
  CheckedOut,
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
    ExclHidden := (FRequestBuffer.Fields[1] = '1');

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
        SQL.Add('pjmodule.hidden, users.login');
        SQL.Add('FROM pjmodule');
        SQL.Add('INNER JOIN modules  ON pjmodule.moduleid = modules.moduleid');
        SQL.Add('INNER JOIN revision ON pjmodule.moduleid = revision.moduleid');
        SQL.Add('LEFT OUTER JOIN users ON modules.userid = users.userid');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        if ExclHidden then // skip hiddden modules
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY modules.name, modules.moduleid,');
        SQL.Add('revision.version, revision.revision');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        if not Eof then
        begin // any records match our Query?
          while not Eof do
          begin
            CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
            CheckedOut := (FieldByName('readonly').AsString = '1');
            {  insert name, path & timestamp only for the first record of a
               module to prevent the transfer of needless information  }
            FResponseBuffer.WriteFields(True, [CurrentModuleID]);
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
                10: if CheckedOut
                      then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                        else FResponseBuffer.WriteFields(False, ['']);
                        // left user empty
                else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 3 to FieldCount - 1 do begin
            // next record
            if not Eof then Next;
            // same Module ?
            while (not Eof) and
                  (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger) do
            begin
              CheckedOut := (FieldByName('readonly').AsString = '1');
              FResponseBuffer.WriteFields(True, [CurrentModuleID]);
              FResponseBuffer.WriteFields(False, ['']); // left name empty
              FResponseBuffer.WriteFields(False, ['']); // left path empty
              // Copy the rest from the record to the response
              for Fld := 3 to FieldCount - 1 do
              begin
                case Fld of
                  // further records don't need the timestamp
                  4: FResponseBuffer.WriteFields(False, [0]);
                  // insert user only if checked out
                  10: if CheckedOut
                        then FResponseBuffer.WriteFields(False, [Fields[Fld].AsString])
                          else FResponseBuffer.WriteFields(False, ['']);
                          // left user empty
                  else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
                end; // case Fld of
              end; // for Fld := 0 to FieldCount - 1 do begin
              Next; // -> same module
            end; // while (not EoF) and....
           // -> new module
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
  Get list of the latest revisions from one project
  (by version/revision and/or by label)
  - include ALL blob information - also file family members

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]excl. hidden modules   - Boolean
               [2]Label ID               - Integer
                 (filter by LabelID
                  - LabelID = 0 -> return all modules/revisions)

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
                  (only the first record of a module ID needs a value)
               [2]Path                   - String(255)
                  (only the first record of a module ID needs a value)
               [3]Revision ID            - Integer
               [4]Version                - Integer
               [5]Revision               - Integer
               [6]Revision TimeStamp     - Double (TDateTime)
               [7]Revision Extension     - String(20)
               [8]Revision CRC32         - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LATEST_REVISIONS.Execute;
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
  LabelID,
  CurrentModuleID,
  CurrentRevisionID: Integer;
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
    ExclHidden := (FRequestBuffer.Fields[1] = '1');
    LabelID := StrToInt(FRequestBuffer.Fields[2]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // First step: get ALL revisions & revision members
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name, modules.path,');
        SQL.Add('revision.revisionid, revision.version,');
        SQL.Add('revision.revision, blobs.origtime,');
        SQL.Add('blobs.extension, blobs.origcrc');
        if LabelID > 0 // filter by label?
          then SQL.Add('FROM pjmodule, modules, revision, blobs, rvlabels')
            else SQL.Add('FROM pjmodule, modules, revision, blobs');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        if LabelID > 0 then
        begin // filter by label?
          SQL.Add('AND rvlabels.labelid = :labelid');
          SQL.Add('AND rvlabels.revisionid = revision.revisionid');
        end;
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');

        //-- added by hdo
        {$IFDEF ORACLESRV}
        //-- faster version with these lines added, at least
        //-- if the project has a reasonable amount of revisions,
        //-- because the sorting slows down things a lot, and less
        //-- records have to be transmitted
        if LabelID <= 0 then //-- don't do this when filtering  for labels!
        begin
          SQL.Add('AND revision.version = (SELECT max(version) FROM revision WHERE moduleid = modules.moduleid)');
          SQL.Add('AND revision.revision = (SELECT max(revision) FROM revision WHERE moduleid = modules.moduleid');
          SQL.Add(' AND version = (SELECT max(version) FROM revision WHERE moduleid = modules.moduleid))');
        end;
        {$ENDIF ORACLESRV}

        SQL.Add('ORDER BY modules.name DESC, modules.moduleid DESC,');
        SQL.Add('revision.version DESC, revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        if LabelID > 0 then // filter by label?
          ParamByName('labelid').AsInteger := LabelID;
        Open;
        {  Second step: Filter out only the latest members from every revision
           - to prevent the transfer of unneccessary data return name & path
             only with the first member of a revision  }
        while not Eof do
        begin
          CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
          CurrentRevisionID := FQuery.FieldByName('revisionid').AsInteger;
          // get the first revision member
          // only the first record of a revision needs complete information
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              6: begin
                   DT := FQuery.FieldByName('origtime').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FieldCount - 1 do begin
          if not Eof then Next;
          // get all revision members
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger) and
                (CurrentRevisionID = FQuery.FieldByName('revisionid').AsInteger) do
          begin
            // further records will get a blank name & path
            FResponseBuffer.WriteFields(True, [FQuery.FieldByName('moduleid').AsInteger]);
            FResponseBuffer.WriteFields(False, ['']);
            FResponseBuffer.WriteFields(False, ['']);
            // Copy the rest of the record to the response
            for Fld := 3 to FieldCount - 1 do
            begin
              case Fld of
                // MWBuffer workaround for DateTime fields
                6: begin
                     DT := FQuery.FieldByName('origtime').AsDateTime;
                     FResponseBuffer.WriteFields(False, [DT]);
                   end;
                else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 3 to FieldCount - 1 do begin
            // next family member
            Next;
          end; // while (not EoF) and...
          // skip the rest - all older revisions - of this module
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger)
            do Next;
          // -> next module
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
  Get list of the revisions from one project asigned to a specific
  version/revision
  - include ALL blob information - also file family members

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]excl. hidden modules   - Boolean
               [2]Version                - Integer
               [3]Revision               - Integer
                 (filter by Revision
                  - Revision = -1 -> return all modules/revisions)

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
                  (only the first record of a module ID needs a value)
               [2]Path                   - String(255)
                  (only the first record of a module ID needs a value)
               [3]Revision ID            - Integer
               [4]Version                - Integer
               [5]Revision               - Integer
               [6]Revision TimeStamp     - Double (TDateTime)
               [7]Revision Extension     - String(20)
               [8]Revision CRC32         - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_VERSION_REVISION.Execute;
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
  CurrentModuleID,
  CurrentRevisionID: Integer;
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
    ExclHidden := (FRequestBuffer.Fields[1] = '1');
    Version_ := StrToInt(FRequestBuffer.Fields[2]);
    Revision := StrToInt(FRequestBuffer.Fields[3]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // First step: get ALL revisions & revision members
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name, modules.path,');
        SQL.Add('revision.revisionid, revision.version,');
        SQL.Add('revision.revision, blobs.origtime,');
        SQL.Add('blobs.extension, blobs.origcrc');
        SQL.Add('FROM pjmodule, modules, revision, blobs');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        SQL.Add('AND revision.version = :version');
        if Revision > -1 then // filter by revision?
          SQL.Add('AND revision.revision = :revision');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY modules.name DESC, revision.version DESC,');
        SQL.Add('revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('version').AsInteger := Version_;
        if Revision > -1 then // filter by revision?
          ParamByName('revision').AsInteger := Revision;
        Open;
        {  Second step: Filter out only the latest members from every revision
           - to prevent the transfer of unneccessary data return name & path
             only with the first member of a revision  }
        while not Eof do
        begin
          CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
          CurrentRevisionID := FQuery.FieldByName('revisionid').AsInteger;
          // get the first revision member
          // only the first record of a revision needs complete information
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              6: begin
                   DT := FQuery.FieldByName('origtime').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FieldCount - 1 do begin
          if not Eof then Next;
          // get all revision members
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger) and
                (CurrentRevisionID = FQuery.FieldByName('revisionid').AsInteger) do
          begin
            // further records will get a blank name & path
            FResponseBuffer.WriteFields(True, [FQuery.FieldByName('moduleid').AsInteger]);
            FResponseBuffer.WriteFields(False, ['']);
            FResponseBuffer.WriteFields(False, ['']);
            // Copy the rest of the record to the response
            for Fld := 3 to FieldCount - 1 do
            begin
              case Fld of
                // MWBuffer workaround for DateTime fields
                6: begin
                     DT := FQuery.FieldByName('origtime').AsDateTime;
                     FResponseBuffer.WriteFields(False, [DT]);
                   end;
                else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 3 to FieldCount - 1 do begin
            // next family member
            Next;
          end; // while (not EoF) and...
          // skip the rest - all older revisions - of this module
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger)
            do Next;
          // -> next module
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
  Get list of the latest revisions from one project related to a user defined
  timestamp. Used for Rollback.
  (by version/revision)
  - include ALL blob information - also file family members

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]excl. hidden modules   - Boolean
               [2]Rollback date          - Double (TDateTime)

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
                  (only the first record of a module ID needs a value)
               [2]Path                   - String(255)
                  (only the first record of a module ID needs a value)
               [3]Revision ID            - Integer
               [4]Version                - Integer
               [5]Revision               - Integer
               [6]Revision TimeStamp     - Double (TDateTime)
               [7]Revision Extension     - String(20)
               [8]Revision CRC32         - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_ROLLBACK_REVISIONS.Execute;
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
  CurrentModuleID,
  CurrentRevisionID: Integer;
  RevisionIDs: TStringList;
  ExclHidden,
  IsNewerRevision: Boolean;
  RollBackDate,
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
    ExclHidden := (FRequestBuffer.Fields[1] = '1');
    RollBackDate := _StrToFloat(FRequestBuffer.Fields[2]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // First step: get ALL revisions & revision members
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name, modules.path,');
        SQL.Add('revision.revisionid, revision.version,');
        SQL.Add('revision.revision, blobs.origtime,');
        SQL.Add('blobs.extension, blobs.origcrc');
        SQL.Add('FROM pjmodule, modules, revision, blobs');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY modules.name DESC, modules.moduleid DESC,');
        SQL.Add('revision.revisionid DESC, revision.version DESC,');
        SQL.Add('revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        {  Second step: Filter out only the latest members from every revision
           related to the given timestamp  }
        RevisionIDs := TStringList.Create;
        try
          while not Eof do
          begin
            CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
            CurrentRevisionID := FQuery.FieldByName('revisionid').AsInteger;
            IsNewerRevision := False;
            while (not Eof) and
                  (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger) and
                  (CurrentRevisionID = FQuery.FieldByName('revisionid').AsInteger) do
            begin
              if not IsNewerRevision then
                IsNewerRevision := (FQuery.FieldByName('origtime').AsDateTime > RollBackDate);
              Next;
            end; // while (not EoF) and...
            if IsNewerRevision then
              RevisionIDs.Add(IntToStr(CurrentRevisionID) + '=1')
            else
              RevisionIDs.Add(IntToStr(CurrentRevisionID) + '=0');
          end; // while not EoF do
          {  Third step: Sample the module's date
             - to prevent the transfer of unneccessary data return name & path
               only with the first member of a revision  }
          First;
          while not Eof do
          begin
            CurrentModuleID := FQuery.FieldByName('moduleid').AsInteger;
            CurrentRevisionID := FQuery.FieldByName('revisionid').AsInteger;
            // skip all revisions with a newer date then the given timestamp
            if RevisionIDs.Values[IntToStr(FQuery.FieldByName('revisionid').AsInteger)]
              = '0' then
            begin
              // This must be our first revision member - get it
              // only the first record of a revision needs complete information
              for Fld := 0 to FieldCount - 1 do
              begin
                case Fld of
                  // MWBuffer workaround for DateTime fields
                  6: begin
                       DT := FQuery.FieldByName('origtime').AsDateTime;
                       FResponseBuffer.WriteFields(False, [DT]);
                     end;
                  else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
                end; // case Fld of
              end; // for Fld := 0 to FieldCount - 1 do begin
              if not Eof then Next;
              // get all revision members
              while (not Eof) and
                    (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger) and
                    (CurrentRevisionID = FQuery.FieldByName('revisionid').AsInteger) do
              begin
                // further records will get a blank name & path
                FResponseBuffer.WriteFields(True, [FQuery.FieldByName('moduleid').AsInteger]);
                FResponseBuffer.WriteFields(False, ['']);
                FResponseBuffer.WriteFields(False, ['']);
                // Copy the rest of the record to the response
                for Fld := 3 to FieldCount - 1 do
                begin
                  case Fld of
                    // MWBuffer workaround for DateTime fields
                    6: begin
                         DT := FQuery.FieldByName('origtime').AsDateTime;
                         FResponseBuffer.WriteFields(False, [DT]);
                       end;
                    else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
                  end; // case Fld of
                end; // for Fld := 3 to FieldCount - 1 do begin
                // next family member
                Next;
              end; // while (not EoF) and...
              // skip the rest - all older revisions - of this module
              while (not Eof) and
                    (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger)
                do Next;
              // -> next module
            end // if RevisionIDs.Values(IntToStr(FQuery['revisionid'].AsInteger))...
            else Next;
          end; // while not EoF do begin
          Close;
        finally
          RevisionIDs.Free;
        end;
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
  Get project references

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer

  Response: 0: [0]Project ID             - Integer
               [1]Project name           - String(50)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROJECT_REFERENCES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  Fld,
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
        SQL.Add('SELECT DISTINCT projects.projectid, projects.name');
        SQL.Add('FROM projects, pjref');
        SQL.Add('WHERE pjref.projectid = :projectid');
        SQL.Add('AND pjref.reference = projects.projectid');
        SQL.Add('ORDER BY projects.name');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        while not Eof do
        begin
          // copy record to response
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
  Get all projects sharing one module
  - client may use this object also to find the project for a specific
    module ID
    Server returns only one record:
    -> client assumes that this is not a shared module
    Server returns nothing:
    -> client assumes that this module is deserted

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer

  Response: 0: [0]Project ID             - Integer
               [1]Project name           - String(50)

            1: [next record...]


  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_SHARED_BY.Execute;
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
        Close;
        SQL.Clear;
        SQL.Add('SELECT projects.projectid, projects.name');
        SQL.Add('FROM pjmodule, projects');
        SQL.Add('WHERE pjmodule.moduleid = :moduleid');
        SQL.Add('AND projects.projectid = pjmodule.projectid');
        SQL.Add('AND projects.deleted = ''0''');
        SQL.Add('ORDER BY projects.name');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        while not Eof do
        begin
          // Copy the record to the response
          for Fld := 0 to FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
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
  Remove project reference

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Reference ID           - Integer

  Response: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_PROJECT_REFERENCES.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  ReferenceID: Integer;
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
    ReferenceID := StrToInt(FRequestBuffer.Fields[1]);
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('DELETE FROM pjref');
        SQL.Add('WHERE pjref.projectid = :projectid');
        SQL.Add('AND pjref.reference = :reference');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('reference').AsInteger := ReferenceID;
        ExecSQL;
        UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
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
  Rename an existing project
  - rename the project's name
  - search for *.dpr, *.res, *.dof... files assigned to this project and
    change the name of these modules (if they are not checked out or shared)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer
               [1]Project ID             - Integer
               [2]New project name       - String(50)

            2: [0]Module ID              - Integer
               [2]New module name        - String(255)

            3: [next module...]

  Response: 0: [0]renamed?               - Boolean

            1: [0]Checked out modules (if renamed = false) - String(255)
                  or affected modules (if renamed = true) - String(255)

            2: [next module...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectRENAME_PROJECT.Execute;
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
  ProjectID: Integer;
  OldProjectName,
  NewProjectName,
  AffectedModules: string;
  CheckedOutModules,
  IsSharedModule: Boolean;
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

    UserID := StrToInt(FRequestBuffer.Fields[0]);
    ProjectID := StrToInt(FRequestBuffer.Fields[1]);
    NewProjectName := AnsiLowerCase(FRequestBuffer.Fields[2]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // Any modules of this project checked out?
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name');
        SQL.Add('FROM pjmodule, modules');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND modules.readonly = ''1''');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        SQL.Add('ORDER BY modules.name');
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        CheckedOutModules := not Eof;
        if CheckedOutModules then
        begin
          {  project has checked out modules, rename not possible,
             return false  }
          FResponseBuffer.WriteFields(True, [False]);
          // return the checked out modules
          while not Eof do
          begin
            // Copy all fields from the record to the response
            for Fld := 0 to FQuery.FieldCount - 1 do
              FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
            Next;
          end;
          Close;
        end // if CheckedOutModules then begin
        else
        begin
          Close;
          // no modules were checked out, return true
          FResponseBuffer.WriteFields(True, [True]);
          // get old projects name
          SQL.Clear;
          SQL.Add('SELECT name FROM projects');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('projectid').AsInteger := ProjectID;
          Open;
          OldProjectName := FieldByName('name').AsString;
          Close;
          // rename the project & update access history
          SQL.Clear;
          SQL.Add('UPDATE projects');
          SQL.Add('SET name = :name,');
          SQL.Add('lastuser = :lastuser, lastaccess = :lastaccess');
          SQL.Add('WHERE projectid = :projectid');
          ParamByName('name').AsString := NewProjectName;
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('projectid').AsInteger := ProjectID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // rename *.dpr, *.dpk, *.res, *.dof... files
          AffectedModules := '';
          if not FRequestBuffer.Eof then FRequestBuffer.Next;
          FQuery2 := TSrvQuery.Create(Self);
          try
            FQuery2.DatabaseName := UData.DBPath;
            while not FRequestBuffer.Eof do
            begin
              // shared module?
              FQuery2.Close;
              FQuery2.SQL.Clear;
              FQuery2.SQL.Add('SELECT * FROM pjmodule');
              FQuery2.SQL.Add('WHERE moduleid = :moduleid');
              FQuery2.SQL.Add('AND projectid <> :projectid');
              FQuery2.ParamByName('moduleid').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[0]);
              FQuery2.ParamByName('projectid').AsInteger := ProjectID;
              FQuery2.Open;
              IsSharedModule := not FQuery2.Eof;
              FQuery2.Close;
              if not IsSharedModule then
              begin
                // build log string
                AffectedModules := AffectedModules +
                                               FRequestBuffer.Fields[1] + ' / ';
                // return renamed modules
                FResponseBuffer.WriteFields(True, [FRequestBuffer.Fields[1]]);
                // update the modules name
                SQL.Clear;
                SQL.Add('UPDATE modules');
                SQL.Add('SET name = :name');
                SQL.Add('WHERE moduleid = :moduleid');
                ParamByName('name').AsString := FRequestBuffer.Fields[1];
                ParamByName('moduleid').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[0]);
                ExecSQL;
                UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
              end; // if not IsSharedModule then begin
              FRequestBuffer.Next;
            end; // while not FRequestBuffer.EoF do begin
          finally
            FQuery2.Free;
          end;
        end; // else if CheckedOutModules then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and (not CheckedOutModules) then
      VCSLog(UData.DBPath, ProjectID, UserID, 0, 0, 'n', 'Project ' + OldProjectName +
        ' renamed to ' + NewProjectName + ' - Affected modules: ' + AffectedModules);
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
  Add/ Update project group

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Group ID               - Integer
                  (0 = add new group)
               [1]Parent ID              - Integer
                  (0 = root entry)
               [2]Group Level            - Integer
               [3]Flags                  - Integer
               [4]Group name             - String(50)
               [5]Group description      - String(2000)

  Response: 0: [0]Group ID               - Integer
                  (only if "add new group")

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_PROJECT_GROUP.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  GroupID,
  GroupLevel,
  GroupFlags,
  ParentID: Integer;
  GroupName,
  GroupDescription: string;
  Update: Boolean;
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

    GroupID := StrToInt(FRequestBuffer.Fields[0]);
    Update := (GroupID <> 0);
    GroupLevel := StrToInt(FRequestBuffer.Fields[2]);
    GroupFlags := StrToInt(FRequestBuffer.Fields[3]);
    ParentID := StrToInt(FRequestBuffer.Fields[1]);
    GroupName := FRequestBuffer.Fields[4];
    GroupDescription := FRequestBuffer.Fields[5];

    FResponseBuffer.Rewrite;
    if Update then
    begin
      // change an existing group record
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('UPDATE groups');
          SQL.Add('SET name = :name, description = :description,');
          SQL.Add('flags = :flags');
          SQL.Add('WHERE groupid = :groupid');
          ParamByName('name').AsString := GroupName;
          ParamByName('description').AsString := GroupDescription;
          ParamByName('flags').AsInteger := GroupFlags;
          ParamByName('groupid').AsInteger := GroupID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
    end // if Update then
    else
    begin
      // add a new group record
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO groups');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('groups')]));
          SQL.Add(':parentid, :treelevel, :flags, :name, :description)');
          ParamByName('parentid').AsInteger := ParentID;
          ParamByName('treelevel').AsInteger := GroupLevel;
          ParamByName('flags').AsInteger := GroupFlags;
          ParamByName('name').AsString := GroupName;
          ParamByName('description').AsString := GroupDescription;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // get the new created group ID
          SQL.Clear;
          SQL.Add('SELECT groupid FROM groups');
          SQL.Add('WHERE name = :name');
          ParamByName('name').AsString := GroupName;
          Open;
          GroupID := FQuery.FieldByName('groupid').AsInteger;
          Close;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
      FResponseBuffer.WriteFields(True, [GroupID]);
    end; // else if Update then
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
  Remove project group

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Group ID               - Integer

  Response: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_PROJECT_GROUP.Execute;
const
  RequestedRights = LevelPAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  GroupID: Integer;
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

    GroupID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        {  remove all related entries from PJGroups to prevent foreign
           key violations  }
        SQL.Clear;
        SQL.Add('DELETE FROM pjgroups');
        SQL.Add('WHERE groupid = :groupid');
        ParamByName('groupid').AsInteger := GroupID;
        ExecSQL;
        // remove the project group
        SQL.Clear;
        SQL.Add('DELETE FROM groups');
        SQL.Add('WHERE groupid = :groupid');
        ParamByName('groupid').AsInteger := GroupID;
        ExecSQL;
        UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
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
  Get project group information

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Record type            - String(1)
                  ('g' = group info, 'p' = project info)
               [1]Group ID               - Integer
               [2]Parent/Project ID      - Integer
               [3]Group Level            - Integer
                  (blank if type = 'p')
               [4]Flags                  - Integer
                  (blank if type = 'p')
               [5]Group name             - String(50)
                  (blank if type = 'p')
               [6]Group description      - String(2000)
                  (blank if type = 'p')

            1: [next group/project]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROJECT_GROUP_INFORMATION.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
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
        // Step 1: group info
        SQL.Clear;
        SQL.Add('SELECT groupid, parentid, treelevel, flags, name, description');
        SQL.Add('FROM groups');
        SQL.Add('ORDER BY parentid, treelevel, name');
        Open;
        while not Eof do
        begin
          // Return "g" (group info)
          FResponseBuffer.WriteFields(True, ['g']);
          for Fld := 0 to FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        // Step 2: project info
        SQL.Clear;
        SQL.Add('SELECT pjgroups.groupid, pjgroups.projectid, projects.name');
        SQL.Add('FROM pjgroups, projects');
        SQL.Add('WHERE projects.projectid = pjgroups.projectid');
        SQL.Add('ORDER BY pjgroups.groupid, projects.name');
        Open;
        while not Eof do
        begin
          // Return "p" (project info)
          FResponseBuffer.WriteFields(True, ['p']);
          for Fld := 0 to FieldCount - 2 do
            FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
          // fill the record with blanks
          FResponseBuffer.WriteFields(False, ['']);
          FResponseBuffer.WriteFields(False, ['']);
          FResponseBuffer.WriteFields(False, ['']);
          FResponseBuffer.WriteFields(False, ['']);
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
  Add or remove a project link to/from a project group

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Group ID               - Integer
               [2]Add                    - Boolean
                 (true = add, false = remove)

  Response: 0: [0]Added?                 - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_REMOVE_PROJECT_TO_GROUP.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  GroupID: Integer;
  AddEntry,
  IsNewEntry: Boolean;
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
    GroupID := StrToInt(FRequestBuffer.Fields[1]);
    AddEntry := (FRequestBuffer.Fields[2] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        if AddEntry then
        begin
          // check if the entry is new
          SQL.Clear;
          SQL.Add('SELECT * FROM pjgroups');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND groupid = :groupid');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('groupid').AsInteger := GroupID;
          Open;
          IsNewEntry := Eof;
          Close;
          if IsNewEntry then
          begin
            // new, add it
            SQL.Clear;
            SQL.Add('INSERT INTO pjgroups');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjgroups')]));
            SQL.Add(':projectid, :groupid)');
            ParamByName('projectid').AsInteger := ProjectID;
            ParamByName('groupid').AsInteger := GroupID;
            ExecSQL;
            UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
            // We've added a new record, return true
            FResponseBuffer.WriteFields(True, [True]);
          end // if IsNewEntry then
          else
            // We've added nothing, return false
            FResponseBuffer.WriteFields(True, [False]);
        end // if AddEntry then
        else
        begin
          SQL.Clear;
          SQL.Add('DELETE FROM pjgroups');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND groupid = :groupid');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('groupid').AsInteger := GroupID;
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        end; // else if AddEntry then
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

end.

