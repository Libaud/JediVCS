{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SvrOBJLog_ToDo.pas

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
2003/02/21  THuber   - Add  F I B P L U S S R V  port which uses  F I B p l u s  components
                     - dangerous casts (eg. Integer(FQuery[...])) changed.
2003/02/28  THuber   - now use FieldByName everywhere
2003/03/07  THuber   - removed A CCESSCTRL IFDEF
2003/03/26  THuber   - some preparations for SQL92 style (only comments)
                     - removed special  d b i s a m  part *seems to be fixed with 3.x*
                       in GET_LOG_FILTER

--- branchpoint for 2.5 dev server ---

2004/12/08  USchuster- style cleaning
                     - simplified creation of insert statements
2004/12/21  USchuster- GET_LOG_ENTRIES does now return also entries with empty
                       projectid or moduleid (remove project or module) (mantis #1197)
2005/05/06  USchuster- added jedi.inc
2005/05/08  USchuster- removed unused units
2005/07/04  USchuster- TFVCSServerObject.VCSLog is used now to create log entries(mantis #2631)
2009/12/06  THuber   #4253 changed rights check for todo items

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5064 support for  I n t e r b a s e 6  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
                     special Oracle J O I N  - Syntax was removed we now only
                     support the A N S I - J o i n Syntax and therefore removed
                     this compiler setting.
-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SvrOBJLog_ToDo - ServerObjects for JVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvOBJLog_ToDo;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  {$IFDEF DELPHI6_UP}            //-- LB
  Variants,
  {$ENDIF DELPHI6_UP}
  SysUtils, SrvConst, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef, SrvAccessDef;

type
  TServerObjectGET_TODO_FILTER = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_TODO_ENTRIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_TODO_ENTRIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_TODO_ENTRIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LOG_FILTER = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LOG_ENTRIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LOG_COMMENT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_LOG_ENTRIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_LOG_ENTRIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

implementation

{==============================================================================
  Add entries to the VCS log file

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Module ID              - Integer
               [2]User ID                - Integer
               [3]Type                   - String(1)
               [4]Description            - String(2000)

            2: [next entry...]

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_LOG_ENTRIES.Execute;
const
  RequestedRights = LevelRW;
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

    if UData.WriteVCSLog then
    begin
      while not FRequestBuffer.Eof do
      begin
        VCSLog(UData.DBPath, StrToInt(FRequestBuffer.Fields[0]),
          StrToInt(FRequestBuffer.Fields[2]), StrToInt(FRequestBuffer.Fields[1]),
          0, AnsiLowerCase(FRequestBuffer.Fields[3])[1], FRequestBuffer.Fields[4]);
        FRequestBuffer.Next;
      end; // while not FRequestBuffer.EoF do begin
    end; // if UData.WriteVCSLog then begin
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
  Get entries from the VCS log file

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
                  (zero = all projects - zero means 0, not NULL!)
               [1]Module ID              - Integer
                  (zero = all modules)
               [2]User ID                - Integer
                  (zero = all users)
               [3]StartDate              - Double (TDateTime)
                  (zero = from beginning)
               [4]EndDate                - Double (TDateTime)
                  (zero = until now)
               [5]Type                   - String
                  (blank = all types)

               Type: Characters seperated with semicolons representing
                     the type of the entry: (i.e 'i;o;u;g;')
                     'h' = security/administration
                     'a' = module added to project
                     'r' = module removed from project
                     'i' = check in module
                     'o' = check out module
                     'u' = undo checkout module
                     'g' = get module
                     'c' = change module
                     's' = synchronized module
                     'v' = module moved
                     'm' = merge project
                     'b' = branch project
                     't' = test project
                     'p' = project removed
                     'n' = project renamed
                     'x' = undefined

  Response: 0: [0]Project name           - String(50)
               [1]Module name            - String(50)
               [2]User name              - String(50)
               [3]Timestamp              - Double (TDateTime)
               [4]Type                   - String(1)
               [5]Description            - String(2000)
               [6]Project ID             - Integer
               [7]Module ID              - Integer
               [8]Log ID                 - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LOG_ENTRIES.Execute;
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
  ProjectID,
  UserID: Integer;
  StartDate,
  EndDate,
  DT: Double;
  EntryType,
  SQLParam: string;
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
    ModuleID := StrToInt(FRequestBuffer.Fields[1]);
    UserID := StrToInt(FRequestBuffer.Fields[2]);
    StartDate := _StrToFloat(FRequestBuffer.Fields[3]);
    EndDate := _StrToFloat(FRequestBuffer.Fields[4]);
    EntryType := AnsiLowerCase(FRequestBuffer.Fields[5]);

    // parse Entrytype and build SQL string
    SQLParam := '';  //-- hdo
    if (Length(EntryType) > 1) then
    begin
      // "type" is a reserved word in Firebird / Interbase 6/7
      //todo check if condition was missed for UIB
        //--- {$IFNDEF UIBSRV}
        SQLParam := 'AND vcslog.type in (''' + EntryType[1] + '''';
        //--- {$ELSE}
        //--- SQLParam := 'AND vcslog.logtype in (''' + EntryType[1] + ''''; //-- THu
        //--- {$ENDIF ~UIBSRV}
      Delete(EntryType, 1, 2);
      while (Pos(';', EntryType) > 0) and (Length(EntryType) > 1) do
      begin
        SQLParam := SQLParam + ',''' + EntryType[1] + '''';
        Delete(EntryType, 1, 2);
      end; // while (Pos(';', EntryType) > 0) and...
      SQLParam := SQLParam + ',''_'')';
    end; // if (Length(EntryType) > 1) then begin

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT projects.name, modules.name, users.login,');
        // "type" is a reserved word in Firebird / Interbase 6/7
        //todo uib?
          //--- {$IFNDEF UIBSRV}
          SQL.Add('vcslog.tstamp, vcslog.type, vcslog.description,');
          //--- {$ELSE}
          //--- SQL.Add('vcslog.tstamp, vcslog.logtype, vcslog.description,');
          //--- {$ENDIF ~UIBSRV}
        SQL.Add('projects.projectid, modules.moduleid, vcslog.logid');
        SQL.Add('FROM users, vcslog');
        //USc 21.12.2004 - no idea if the is any performance difference between
        // INNER JOIN and LEFT OUTER JOIN if not we could always use LEFT OUTER JOIN
        if ProjectID > 0 then
          SQL.Add('INNER JOIN projects ON vcslog.projectid = projects.projectid')
        else
          SQL.Add('LEFT OUTER JOIN projects ON vcslog.projectid = projects.projectid');
        if ModuleID > 0 then
          SQL.Add('INNER JOIN modules ON vcslog.moduleid = modules.moduleid')
        else
          SQL.Add('LEFT OUTER JOIN modules ON vcslog.moduleid = modules.moduleid');
        SQL.Add('WHERE');
        if ProjectID > 0 then
          SQL.Add('    projects.deleted = ''0''')
        else
          SQL.Add('    ((projects.projectid is null) OR (projects.deleted = ''0''))');
        SQL.Add('AND users.userid = vcslog.userid');
        if ProjectID > 0 then
          SQL.Add('AND vcslog.projectid = :projectid');
        if ModuleID > 0 then
          SQL.Add('AND vcslog.moduleid = :moduleid');
        if UserID > 0 then
          SQL.Add('AND vcslog.userid = :userid');
        if SQLParam <> '' then
          SQL.Add(SQLParam);
        if StartDate > 0 then
          SQL.Add('AND vcslog.tstamp > :startdate');
        if EndDate > 0 then
          SQL.Add('AND vcslog.tstamp < :enddate');
        SQL.Add('ORDER BY vcslog.tstamp');
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if ModuleID > 0 then
          ParamByName('moduleid').AsInteger := ModuleID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        if StartDate > 0 then
          ParamByName('startdate').AsDateTime := StartDate;
        if EndDate > 0 then
          ParamByName('enddate').AsDateTime := EndDate;
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              3: begin
                   DT := FQuery.FieldByName('tstamp').AsDateTime;
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
  Get the checkin comment for a specific log entry

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Log ID                 - Integer

  Response: 0: [0]Check In comment       - String(2000)
                  (or blank if not found)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LOG_COMMENT.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  LogID: Integer;
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

    LogID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT revision.comment_i');
        SQL.Add('FROM logcomm, revision');
        SQL.Add('WHERE logcomm.logid = :logid');
        SQL.Add('AND revision.revisionid = logcomm.revisionid');
        ParamByName('logid').AsInteger := LogID;
        Open;
        if not Eof then
          FResponseBuffer.WriteFields(True, [FieldByName('comment_i').AsString])
        else
          FResponseBuffer.WriteFields(True, ['blank']);
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
  Remove entries from the VCS log

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
                  (zero = all projects)
               [1]Module ID              - Integer
                  (zero = all modules)
               [2]User ID                - Integer
                  (zero = all users)
               [3]Date                   - Double (TDateTime)
                  (remove all entries older than Date)

  Response: 0: [0]Affected entries       - Integer

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_LOG_ENTRIES.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery,
  FQuery2: TSrvQuery;
  TANr,
  AccessID,
  ProjectID,
  ModuleID,
  UserID,
  GrantedRights: Integer;
  ExpDate: Double;
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
    ModuleID := StrToInt(FRequestBuffer.Fields[1]);
    UserID := StrToInt(FRequestBuffer.Fields[2]);
    ExpDate := _StrToFloat(FRequestBuffer.Fields[3]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        // get all log ID's affected by this operation
        SQL.Add('SELECT logid FROM vcslog');
        SQL.Add('WHERE tstamp < :tstamp');
        if ProjectID > 0 then
          SQL.Add('AND projectid = :projectid');
        if ModuleID > 0 then
          SQL.Add('AND moduleid = :moduleid');
        if UserID > 0 then
          SQL.Add('AND userid = :userid');
        ParamByName('tstamp').AsDateTime := ExpDate;
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if ModuleID > 0 then
          ParamByName('moduleid').AsInteger := ModuleID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        Open;
        FQuery2 := TSrvQuery.Create(Self);
        try
          FQuery2.DatabaseName := UData.DBPath;
          while not FQuery.Eof do
          begin
            // delete all log comment links assigned with this module
            FQuery2.SQL.Clear;
            FQuery2.SQL.Add('DELETE FROM logcomm');
            FQuery2.SQL.Add('WHERE logid = :logid');
            FQuery2.ParamByName('logid').AsInteger := FQuery.FieldByName('logid').AsInteger;
            FQuery2.ExecSQL;
            FQuery.Next;
          end; // while not EoF do
        finally
          FQuery2.Free;
        end;
        Close;
        SQL.Clear;
        SQL.Add('DELETE FROM vcslog');
        SQL.Add('WHERE tstamp < :tstamp');
        if ProjectID > 0 then
          SQL.Add('AND projectid = :projectid');
        if ModuleID > 0 then
          SQL.Add('AND moduleid = :moduleid');
        if UserID > 0 then
          SQL.Add('AND userid = :userid');
        ParamByName('tstamp').AsDateTime := ExpDate;
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if ModuleID > 0 then
          ParamByName('moduleid').AsInteger := ModuleID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        ExecSQL;
        // return Nr. of removed entries
        FResponseBuffer.WriteFields(True, [RowsAffected]);
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
  Add/ Update ToDo list entries

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]ToDo ID                - Integer
                  (zero = add a new entry)
               [1]User ID                - Integer
               [2]Project ID             - Integer
               [3]Responsible            - Integer
               [4]Priority               - Integer
               [5]State                  - String(1)
               [6]Category               - String(50)
               [7]Description            - String(2000)
               [8]TargetDate             - Double (TDateTime)
               [9]DoneDate               - Double (TDateTime)

            2: [next entry...]

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_TODO_ENTRIES.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  ToDoID,
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

    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        while not FRequestBuffer.Eof do
        begin
          ToDoID := StrToInt(FRequestBuffer.Fields[0]);
          if ToDoID = 0 then
          begin
            // add a new entry
            SQL.Clear;
            SQL.Add('INSERT INTO todo');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('todo')]));            
            SQL.Add(':userid, :projectid, :category, :responsible, :created,');
            SQL.Add(':priority, null, :description, :targetdate, :donedate)');
            // state = null
            ParamByName('userid').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[1]);
            ParamByName('projectid').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[2]);
            ParamByName('responsible').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[3]);
            ParamByName('created').AsDateTime := LocalDT2GMTDT(Now);
            ParamByName('priority').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[4]);
            ParamByName('category').AsString := FRequestBuffer.Fields[6];
            ParamByName('description').AsString := FRequestBuffer.Fields[7];
            ParamByName('targetdate').AsDateTime :=
                                          _StrToFloat(FRequestBuffer.Fields[8]);
            ParamByName('donedate').AsDateTime :=
                                          _StrToFloat(FRequestBuffer.Fields[9]);
            ExecSQL;
          end // if ToDoID = 0 then begin
          else
          begin
            // update an old entry
            SQL.Clear;
            SQL.Add('UPDATE todo');
            SQL.Add('SET userid = :userid, projectid = :projectid,');
            SQL.Add('category = :category, responsible = :responsible,');
            SQL.Add('created = :created, priority = :priority,');
            SQL.Add('state = null, description = :description,');
            SQL.Add('targetdate = :targetdate, donedate = :donedate');
            SQL.Add('WHERE todoid = :todoid');
            // state = null
            ParamByName('userid').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[1]);
            ParamByName('projectid').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[2]);
            ParamByName('responsible').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[3]);
            ParamByName('created').AsDateTime := LocalDT2GMTDT(Now);
            ParamByName('priority').AsInteger :=
                                             StrToInt(FRequestBuffer.Fields[4]);
            ParamByName('category').AsString := FRequestBuffer.Fields[6];
            ParamByName('description').AsString := FRequestBuffer.Fields[7];
            ParamByName('targetdate').AsDateTime :=
                                          _StrToFloat(FRequestBuffer.Fields[8]);
            ParamByName('donedate').AsDateTime :=
                                          _StrToFloat(FRequestBuffer.Fields[9]);
            ParamByName('todoid').AsInteger := ToDoID;
            ExecSQL;
          end; // if ToDoID = 0 then begin
          FRequestBuffer.Next;
        end; // while not FRequestBuffer.EoF do begin
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
  Remove ToDo list entries

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]ToDo ID                - Integer

            2: [next entry...]

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
function GetUserProjectIdOfToDoItem(const jvcsDatabase : TJVCSArchive; const idTodo : Integer; var idUser, idProject : Integer) : boolean;
var
  fQuery : TSrvQuery;
begin
  result := false;
  idUser := -1;
  idProject := -1;
  FQuery := TSrvQuery.Create(nil);
  try
    FQuery.DataBaseName := jvcsDatabase;
    with FQuery do
    begin
      SQL.Clear;
      SQL.Add('SELECT userid,projectid FROM todo');
      SQL.Add('WHERE todoid = :todoid');
      ParamByName('todoid').AsInteger := idTodo;
      Open;
      if not Eof then
      begin
        result := true;
        idUser := FieldByName('userid').AsInteger;
        idProject := FieldByName('projectid').AsInteger;
      end;
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
end;

function GetProjectRight(const jvcsDatabase : TJVCSArchive; const idUser, idProject : Integer) : integer;
var
  fQuery : TSrvQuery;
begin
  result := -1;
  FQuery := TSrvQuery.Create(nil);
  try
    with FQuery do
    begin
      DataBaseName := jvcsDatabase;
      Close;
      SQL.Clear;
      SQL.Add('SELECT rights');
      SQL.Add('FROM pjusers');
      SQL.Add('WHERE userid = :userid');
      SQL.Add('AND   projectid = :projectid');
      ParamByName('userid').AsInteger := idUser;
      ParamByName('projectid').AsInteger := idProject;
      Open;
      if not EoF then
        result := FieldByName('rights').AsInteger;
      Close;
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
end;

procedure TServerObjectREMOVE_TODO_ENTRIES.Execute;
const
    RequestedRights = LevelRW;
var
    UData : PUserDataRecord;
    FQuery : TSrvQuery;
    nCntDeleted,
    TANr,
    AccessID,
    idUserTodo,
    idProjectTodo,
    GrantedProjectRights,
    GrantedArchiveRights : Integer;
    bDeleteItem : Boolean;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);

    GrantedArchiveRights := GetArchiveRelatedRight(UData.DBPath, AccessID, TANr,
                                         RequestedRights, UData.LogAccessFault);

    // #4253 change check from archive related right this way:
    // 1. userid of todo item EQUAL user who requests delete
    // 2. user has read/write right for project the todo item is related to
    // 3. if there are no project related rights and user has at least read/write archive right
    //
    nCntDeleted := 0;

    FRequestBuffer.Next;  // position at ToDo Item
    FQuery := TSrvQuery.Create(self);
    try
      FQuery.DataBaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        while not FRequestBuffer.EoF do
        begin
          bDeleteItem := false; // default not to delete

          if GetUserProjectIdOfToDoItem(UData.DBPath, StrToIntDef(FRequestBuffer.Fields[0],-1), idUserTodo, idProjectTodo) then
          begin
            // Check condition 1. (user has added item himself)
            if AccessID = idUserTodo then
              bDeleteItem := true; // can delete it

            // it is not condition 1., so check condition 2. & 3. (check project related from todo id
            if not bDeleteItem then
            begin
              GrantedProjectRights := GetProjectRight(UData.DBPath, AccessId, idProjectTodo);
              // if there are no project rights we need to check archive rights
              // TODO SrvCustomSrvObj: check archive rights if no special rights are defined for the project rights!
              if GrantedProjectRights = -1 then
              begin
                if not (GrantedArchiveRights < RequestedRights) then  // no project rights but have enough archive rights
                  bDeleteItem := true;  // can delete it
              end
              else if not (GrantedProjectRights < RequestedRights) then // have enough project rights
                bDeleteItem := true;
            end;

            if bDeleteItem then
            begin
              inc(nCntDeleted);
              SQL.Clear;
              SQL.Add('DELETE FROM todo');
              SQL.Add('WHERE todoid = :todoid');
              ParamByName('todoid').AsInteger := StrToInt(FRequestBuffer.Fields[0]);
              ExecSQL;
            end;
          end;
          FRequestBuffer.Next;
        end; // while not FRequestBuffer.EoF do begin
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;

    // we could return a special return code to determine if no or not all todo items
    // were deleted due to missing rights.
    // as this would need also a change in the client, we just check if nothing was deleted
    // and flag this as todo in a future version
    //TODO handling if more than one todo should deleted but due to missing rights not all can be deleted

    if nCntDeleted = 0 then
    begin
      FResultStatus := 403; // forbidden
      FResponseBuffer.Rewrite;
      if GrantedArchiveRights > -1 then
        FResponseBuffer.WriteFields ( false
                                    , [Format ( Levels
                                              , [AccLevel[RequestedRights], AccLevel[GrantedArchiveRights]])])
      else
        FResponseBuffer.WriteFields(false, [Unknown]);
      Finish;
      Exit;
    end;

    FResultStatus := 200; // OK
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF}
  except
    on E:Exception do
    begin
      {$IFDEF LOGEXCEPTION}
      TriggerDisplay(' !! ' + Format(SrvException, [FFunctionCode, E.Message]));
      {$ENDIF}
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(FALSE,
                            [Format(SrvException, [FFunctionCode, E.Message])]);
    end;
  end;
  Finish;
end;
{ ==============================================================================

  Get entries from the ToDo list

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer
                  (zero = all users - zero means 0, not NULL!)
               [1]Project ID             - Integer
                  (zero = all projects)
               [2]Category               - String(50)
                  (blank = all categories)
               [3]Priority               - Integer
                  (zero = all priorities)
               [4]only items not done    - Boolean
               [5]Target date            - Double (TDateTime)
                  (zero = ignore target date)

  Response: 0: [0]ToDo ID                - Integer
               [1]Timestamp              - Double (TDateTime)
               [2]User name              - String(50)
               [3]Project name           - String(50)
               [4]Priority               - Integer
               [5]State                   - String(1)
               [6]Category               - String(50)
               [7]Targetdate             - Double (TDateTime)
               [8]Donedate               - Double (TDateTime)
               [9]Description            - String(2000)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_TODO_ENTRIES.Execute;
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
  Priority: Integer;
  NotDone: Boolean;
  TargetDate,
  DT: Double;
  Category: string;
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
    Category := FRequestBuffer.Fields[2];
    Priority := StrToInt(FRequestBuffer.Fields[3]);
    NotDone := (FRequestBuffer.Fields[4] = '1');
    TargetDate := _StrToFloat(FRequestBuffer.Fields[5]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT todo.todoid, todo.created, users.login,');
        SQL.Add('projects.name, todo.priority,');
        SQL.Add('todo.state, todo.category, todo.targetdate, todo.donedate,');
        SQL.Add('todo.description');
        SQL.Add('FROM projects, users, todo');
        SQL.Add('WHERE projects.projectid = todo.projectid');
        SQL.Add('AND projects.deleted = ''0''');
        SQL.Add('AND users.userid = todo.responsible');
        if UserID > 0 then
          SQL.Add('AND todo.responsible = :userid');
        if ProjectID > 0 then
          SQL.Add('AND todo.projectid = :projectid');
        if Category <> '' then
          SQL.Add('AND todo.category = :category');
        if Priority > 0 then
          SQL.Add('AND todo.priority = :priority');
        if NotDone then
          SQL.Add('AND todo.donedate = :nulldate');
        if TargetDate > 0 then
        begin
          SQL.Add('AND todo.targetdate > :nulldate');
          SQL.Add('AND todo.targetdate < :targetdate');
        end;
        SQL.Add('ORDER BY todo.priority, projects.name,');
        SQL.Add('todo.created');
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if Category <> '' then
          ParamByName('category').AsString := Category;
        if Priority > 0 then
          ParamByName('priority').AsInteger := Priority;
        if NotDone or (TargetDate > 0) then
          ParamByName('nulldate').AsDateTime := 0;
        if TargetDate > 0 then
          ParamByName('targetdate').AsDateTime := TargetDate;
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              1: begin
                   DT := FQuery.FieldByName('created').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              7: begin // date may be blank!
                   if not FQuery.FieldByName('targetdate').IsNull then
                   begin
                     DT := FQuery.FieldByName('targetdate').AsDateTime;
                     FResponseBuffer.WriteFields(False, [DT]);
                   end else FResponseBuffer.WriteFields(False, [0]);
                 end;
              8: begin // date may be blank!
                   if not FQuery.FieldByName('donedate').IsNull then
                   begin
                     DT := FQuery.FieldByName('donedate').AsDateTime;
                     FResponseBuffer.WriteFields(False, [DT]);
                   end else FResponseBuffer.WriteFields(False, [0]);
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
  Get the set of possible filter values for the current log file content

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Filter type            - String(1)
                  ('p' = projects, 'm' = modules, 'u' = users)
               [1]ID                     - Integer
               [1]Name                   - String(255)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LOG_FILTER.Execute;
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
        // Step1 : All projects
        Close;
        SQL.Clear;
        //THu this would be SQL92 for changing later
        { 
          SQL.Add('SELECT DISTINCT vcslog.projectid, projects.name');
          SQL.Add('FROM vcslog');
          SQL.Add('INNER JOIN projects on projects.projectid = vcslog.projectid');
          SQL.Add('WHERE projects.deleted = ''0''');
          SQL.Add('ORDER BY projects.name');
         }
        SQL.Add('SELECT DISTINCT vcslog.projectid, projects.name');
        SQL.Add('FROM projects, vcslog');
        SQL.Add('WHERE projects.projectid = vcslog.projectid');
        SQL.Add('AND projects.deleted = ''0''');
        SQL.Add('ORDER BY projects.name');
        Open;
        while not Eof do
        begin
          // insert 'p' -> projects
          FResponseBuffer.WriteFields(True, ['p']);
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        SQL.Clear;
        //THu this would be SQL92 for changing later
        { 
          SQL.Add('SELECT DISTINCT vcslog.moduleid, modules.name');
          SQL.Add('FROM vcslog');
          SQL.Add('INNER JOIN modules on vcslog.moduleid = modules.moduleid');
          SQL.Add('ORDER BY modules.name');
         }
        SQL.Add('SELECT DISTINCT vcslog.moduleid, modules.name');
        SQL.Add('FROM modules, vcslog');
        SQL.Add('WHERE modules.moduleid = vcslog.moduleid');
        SQL.Add('ORDER BY modules.name');
        Open;
        while not Eof do
        begin
          // insert 'm' -> modules
          FResponseBuffer.WriteFields(True, ['m']);
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        // Step3 : All users
        SQL.Clear;
        //THu this would be SQL92 for changing later
        { 
          SQL.Add('SELECT DISTINCT vcslog.userid, users.login');
          SQL.Add('FROM vcslog');
          SQL.Add('INNER JOIN users on users.userid = vcslog.userid');
          SQL.Add('ORDER BY users.login');
         }
        SQL.Add('SELECT DISTINCT vcslog.userid, users.login');
        SQL.Add('FROM users, vcslog');
        SQL.Add('WHERE users.userid = vcslog.userid');
        SQL.Add('ORDER BY users.login');
        Open;
        while not Eof do
        begin
          // insert 'u' -> users
          FResponseBuffer.WriteFields(True, ['u']);
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
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
  Get the set of possible filter values for the current todo list content

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Filter type            - String(1)
                  ('p' = projects, 'u' = users, 'c' = categories)
               [1]ID                     - Integer
                  (catogeries do not have an ID, return 0)
               [2]Name                   - String(50)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_TODO_FILTER.Execute;
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
        // Step1 : All projects
        Close;
        SQL.Clear;
        SQL.Add('SELECT DISTINCT todo.projectid, projects.name');
        SQL.Add('FROM projects, todo');
        SQL.Add('WHERE projects.projectid = todo.projectid');
        SQL.Add('AND projects.deleted = ''0''');
        SQL.Add('ORDER BY projects.name');
        Open;
        while not Eof do
        begin
          // insert 'p' -> projects
          FResponseBuffer.WriteFields(True, ['p']);
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        // Step2 : All users
        SQL.Clear;
        SQL.Add('SELECT DISTINCT todo.userid, users.login');
        SQL.Add('FROM users, todo');
        SQL.Add('WHERE users.userid = todo.userid');
        SQL.Add('ORDER BY users.login');
        Open;
        while not Eof do
        begin
          // insert 'u' -> users
          FResponseBuffer.WriteFields(True, ['u']);
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        // Step3 : All categories
        SQL.Clear;
        SQL.Add('SELECT DISTINCT category');
        SQL.Add('FROM todo');
        SQL.Add('ORDER BY category');
        Open;
        while not Eof do
        begin
          // insert 'c' -> categories
          FResponseBuffer.WriteFields(True, ['c']);
          // categories do not have an ID, insert 0
          FResponseBuffer.WriteFields(False, [0]);
          FResponseBuffer.WriteFields(False, [FQuery.Fields[0].AsString]);
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


end.

