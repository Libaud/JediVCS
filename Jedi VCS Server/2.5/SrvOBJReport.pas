{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvOBJReport.pas

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
2003/02/22  THuber   - Add  F I B P L U S S R V  port which uses  F I B p l u s  components
                       therefor dangerous casts changed:
                        * TDateTime(...) => .AsDateTime
                        * Integer(...) => .AsInteger
                        * FQuery.Fields[Fld] => FQuery.Fields[Fld].AsString
                     - removed Interbase5 support
                     - D4_UP directive removed
2003/03/07  THuber   - removed A CCESSCTRL IFDEF

--- branchpoint for 2.5 dev server ---

2005/05/06  USchuster- style cleaning
2005/05/08  USchuster- removed unused units

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
                     special Oracle J O I N  - Syntax was removed we now only
                     support the A N S I - J o i n Syntax and therefore removed
                     this compiler setting.
-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SvrOBJReport - ServerObjects for JVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvOBJReport;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Classes, SrvConst, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef, SrvAccessDef;

type
  TServerObjectREPORT_PROJECT_STATE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREPORT_ALL_PROJECTS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREPORT_PROJECT_BUGS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREPORT_MILESTONES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

implementation

{===============================================================================
  Report the current developement state from one project
  - include ALL blob information - also file family members

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Checked out            - Boolean
                  (false = all modules)
               [2]Hidden                 - Boolean
                  (false = all modules)
               [3]User ID                - Integer
                  (false = all users)
               [4]Label ID               - Integer
                  (false = all labels)

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(250)
                  (only the first record of a revision needs a value)
               [2]Path                   - String(250)
                  (only the first record of a revision needs a value)
               [3]Revision ID            - Integer
               [4]Version                - Integer
               [5]Revision               - Integer
               [6]Revision TimeStamp     - Double (TDateTime)
               [7]Revision Extension     - String(20)
               [8]Label ID               - Integer
               [9]Read only              - String(1)
                  (read-only = '1', available = '0')
              [10]Owner                  - String(50)
                  (only the first record of a revision needs a value)
              [11]Revision file size     - Integer
              [12]Revision comp. size    - Integer
              [13]Revision CRC           - Integer
              [14]Hidden                 - String(1)
                  (hidden = '1', visible = '0')

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
===============================================================================}
procedure TServerObjectREPORT_PROJECT_STATE.Execute;
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
  LabelID,
  CurrentModuleID,
  CurrentRevisionID: Integer;
  CheckedOut,
  Hidden: Boolean;
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
    CheckedOut := (FRequestBuffer.Fields[1] = '1');
    Hidden := (FRequestBuffer.Fields[2] = '1');
    UserID := StrToInt(FRequestBuffer.Fields[3]);
    LabelID := StrToInt(FRequestBuffer.Fields[4]);

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
        SQL.Add('blobs.extension, rvlabels.labelid, modules.readonly,');
        SQL.Add('users.login, blobs.origsize, blobs.compsize,');
        SQL.Add('blobs.origcrc, pjmodule.hidden');
        {$IFNDEF MSSQLSRV}
        SQL.Add('FROM pjmodule, modules, revision, blobs, users');
        {$ELSE}
        SQL.Add('FROM pjmodule, modules, blobs, users, revision');
        {$ENDIF ~MSSQLSRV}
        SQL.Add('LEFT OUTER JOIN rvlabels ON rvlabels.revisionid = ' +
                                                         'revision.revisionid');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        if CheckedOut then
          SQL.Add('AND modules.readonly = ''1''');
        if Hidden then
          SQL.Add('AND pjmodule.hidden = ''1''');
        if UserID > 0 then
          SQL.Add('AND modules.userid = :userid');
        if LabelID > 0 then
          SQL.Add('AND rvlabels.labelid = :labelid');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        SQL.Add('AND users.userid = modules.userid');
        SQL.Add('ORDER BY modules.name ASC, revision.version DESC,');
        SQL.Add('revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        if LabelID > 0 then
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
            FResponseBuffer.WriteFields(True,  [FQuery.FieldByName('moduleid').AsInteger]);
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
                // further records will get a blank owner
                10: FResponseBuffer.WriteFields(False, ['']);
                else FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 3 to FieldCount - 1 do begin
            // next family member
            Next;
          end; // while (not Eof) and...
          // skip the rest - all older revisions - of this module
          while (not Eof) and
                (CurrentModuleID = FQuery.FieldByName('moduleid').AsInteger)
            do Next;
          // -> next module
        end; // while not Eof do begin
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
    on E: Exception do
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
{===============================================================================
  Report information about all projects in the archive

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Project ID             - Integer
               [1]Project name           - String(50)
               [2]P. created at          - Double (TDateTime)
               [3]P. created by          - String(50)
               [4]P. last access at      - Double (TDateTime)
               [5]P. last access by      - String(50)
               [6]assigned files         - Integer
               [7]Sum. file size         - Integer
               [8]Sum. compr. file size  - Integer
               [9]Project deleted?       - String(1)
                  ('1' = deleted, '0' = not deleted)
              [10]Description            - String(2000)
              [11]assigned modules       - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
===============================================================================}
procedure TServerObjectREPORT_ALL_PROJECTS.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
  DT: Double;
  ModuleCount: TStringList;
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

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        {  Step 1: count project modules and store the values in a Stringlist }
        Close;
        SQL.Clear;
        SQL.Add('SELECT DISTINCT projectid, COUNT(*) AS modcount');
        SQL.Add('FROM pjmodule');
        SQL.Add('GROUP BY projectid');
        Open;
        ModuleCount := TStringList.Create;
        try
          while not Eof do
          begin
            // Store projectid/modulecount in StringList (Name = Value)
            ModuleCount.Add(IntToStr(FQuery.FieldByName('projectid').AsInteger) + '=' +
                                         IntToStr(FQuery.FieldByName('modcount').AsInteger));
            Next;
          end; // while not Eof do begin
          Close;
          // Step 2: get all other information
          SQL.Clear;
          SQL.Add('SELECT projects.projectid, projects.name,');
          SQL.Add('projects.created, U1.login createdby, projects.lastaccess,');
          SQL.Add('U2.login lastuser, COUNT(*),');
          SQL.Add('SUM(blobs.origsize), SUM(blobs.compsize)');
          {$IFNDEF MSSQLSRV} //-- MG
          SQL.Add(', projects.deleted, projects.description');
          {$ENDIF ~MSSQLSRV}
          SQL.Add('FROM projects, users U1, users U2, pjmodule, revision,');
          SQL.Add('blobs');
          SQL.Add('WHERE projects.createdby = U1.userid');
          SQL.Add('AND projects.lastuser = U2.userid');
          SQL.Add('AND pjmodule.projectid = projects.projectid');
          SQL.Add('AND revision.moduleid = pjmodule.moduleid');
          SQL.Add('AND blobs.revisionid = revision.revisionid');
          SQL.Add('GROUP BY projects.projectid, projects.name,');
          SQL.Add('projects.created, U1.login, projects.lastaccess, U2.login');
          {$IFNDEF MSSQLSRV} //-- MG
          SQL.Add(', projects.deleted, projects.description'); //-- hdo
          {$ENDIF ~MSSQLSRV}
          SQL.Add('ORDER BY projects.name');
          Open;
          while not Eof do
          begin
            // Copy the record to the response
            for Fld := 0 to FieldCount - 1 do
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
                else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
              end; // case Fld of
            end; // for Fld := 0 to FieldCount - 1 do begin
            // get module count from StringList
            FResponseBuffer.WriteFields(False,
                  [ModuleCount.Values[IntToStr(FQuery.FieldByName('projectid').AsInteger)]]);
            Next;
          end; // while not Eof do begin
        finally;
          ModuleCount.Free;
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
    on E: Exception do
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
{===============================================================================
  Report information about the bug state of a projects and his modules

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Bug ID                 - Integer
                  (0 = report all bugs)
               [2]Severity               - Integer
                  (-1 = ignore severity)
               [3]Status                 - Integer
                  (-1 = ignore status)

  Response: 0: [0]Type                   - String(1)
                  ('p' = project, 'm' = module)
               [1]Project/Module ID      - Integer
               [2]Project/Module name    - String(250)
               [3]Path                   - String(250)
                  (blank for projects)
               [4]Bug name               - String(250)
               [5]Severity               - Integer
               [6]Status                 - Integer
               [7]Bug description        - String(2000)
               [8]Reported by            - String(2000)
               [9]Workaround             - String(2000)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
===============================================================================}
procedure TServerObjectREPORT_PROJECT_BUGS.Execute;
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
  BugID,
  Severity,
  Status: Integer;
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
    BugID := StrToInt(FRequestBuffer.Fields[1]);
    Severity := StrToInt(FRequestBuffer.Fields[2]);
    Status := StrToInt(FRequestBuffer.Fields[3]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // Step 1: return the project related bugs
        Close;
        SQL.Clear;
        SQL.Add('SELECT pjbugs.projectid, projects.name, bugs.bug,');
        SQL.Add('bugs.severity, bugs.status, bugs.description,');
        SQL.Add('bugs.reportedby, bugs.workaround');
        SQL.Add('FROM pjbugs, bugs, projects');
        SQL.Add('WHERE pjbugs.projectid = :projectid');
        if BugID > 0 then
          SQL.Add('AND bugs.bugid = :bugid');
        if Severity > -1 then
          SQL.Add('AND bugs.severity >= :severity');
        if Status > -1 then
          SQL.Add('AND bugs.status >= :status');
        SQL.Add('AND projects.projectid = pjbugs.projectid');
        SQL.Add('AND bugs.bugid = pjbugs.bugid');
        SQL.Add('ORDER BY pjbugs.projectid, bugs.severity');
        if BugID > 0 then
          ParamByName('bugid').AsInteger := BugID;
        if Severity > -1 then
          ParamByName('severity').AsInteger := Severity;
        if Status > -1 then
          ParamByName('status').AsInteger := Status;
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        while not Eof do
        begin
          // insert 'p' (project)
          FResponseBuffer.WriteFields(True, ['p']);
          // Copy the first part of the record to the response
          for Fld := 0 to 1 do
            FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
          // insert blank path (project)
          FResponseBuffer.WriteFields(False, ['']);
          // Copy the second part of the record to the response
          for Fld := 2 to FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
          Next;
        end; // while not Eof do begin
        Close;
        // Step 2: return the module related bugs
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name, modules.path,');
        SQL.Add('bugs.bug, bugs.severity, bugs.status, bugs.description,');
        SQL.Add('bugs.reportedby, bugs.workaround');
        SQL.Add('FROM pjmodule, mdbugs, bugs, modules');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        if BugID > 0 then
          SQL.Add('AND bugs.bugid = :bugid');
        if Severity > -1 then
          SQL.Add('AND bugs.severity >= :severity');
        if Status > -1 then
          SQL.Add('AND bugs.status >= :status');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND mdbugs.moduleid = pjmodule.moduleid');
        SQL.Add('AND bugs.bugid = mdbugs.bugid');
        SQL.Add('ORDER BY modules.moduleid, bugs.severity');
        if BugID > 0 then
          ParamByName('bugid').AsInteger := BugID;
        if Severity > -1 then
          ParamByName('severity').AsInteger := Severity;
        if Status > -1 then
          ParamByName('status').AsInteger := Status;
        ParamByName('projectid').AsInteger := ProjectID;
        Open;
        while not Eof do
        begin
          // insert 'm' (modules)
          FResponseBuffer.WriteFields(True, ['m']);
          // Copy the record to the response
          for Fld := 0 to FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [Fields[Fld].AsString]);
          Next;
        end; // while not Eof do begin
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
    on E: Exception do
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
{===============================================================================
  Report information about the milestones in the archive

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Project ID             - Integer
               [1]Project name           - String(250)
               [2]Milestone level        - Integer
               [3]Milestone name         - String(50)
               [4]Milestone reached at   - Double (TDateTime)
               [5]Milestone confirmed    - String(50)
               [6]Milestone description  - String(2000)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
===============================================================================}
procedure TServerObjectREPORT_MILESTONES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
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
        SQL.Add('SELECT projects.projectid, projects.name,');
        SQL.Add('mstones.milestone, mstones.name, pjmstone.reached,');
        SQL.Add('pjmstone.confirm, pjmstone.description');
        SQL.Add('FROM projects, mstones, pjmstone');
        SQL.Add('WHERE pjmstone.projectid = projects.projectid');
        SQL.Add('AND mstones.milestoneid = pjmstone.milestoneid');
        SQL.Add('ORDER BY projects.name, mstones.milestone');
        Open;
        while not Eof do
        begin
          // Copy the record to the response
          for Fld := 0 to FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   DT := FQuery.FieldByName('reached').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FieldCount - 1 do begin
          Next;
        end; // while not Eof do begin
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
    on E: Exception do
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

