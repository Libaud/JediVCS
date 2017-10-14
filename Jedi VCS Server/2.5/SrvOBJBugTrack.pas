{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvOBJBugTrack.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

  Mar '99 by Thomas Hensle - http://www.freevcs.de
  Based on François PIETTE's SvrTest demo application.

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI VCS
2003/02/09  HDors    - Add MySQL Server Port by Ludo Brands 
2003/02/22  THuber   - Add FIBPLUSSRV port which uses FIBplus components
                       therefor dangerous casts changed:
                        * TDateTime(...) => .AsDateTime
                        * Integer(...) => .AsInteger
                        * FQuery.Fields[Fld] => FQuery.Fields[Fld].AsString
                     - removed Interbase5 support
                     - D4_UP directive removed
2003/02/28  THuber   - now use FieldByName everywhere
2003/03/07  THuber   - removed A CCESSCTRL IFDEF

--- branchpoint for 2.5 dev server ---

2004/12/08  USchuster- style cleaning
                     - simplified creation of insert statements
2005/05/06  USchuster- added jedi.inc
2005/05/08  USchuster- removed unused units                     

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5066 support for  F l a s h F i l e r  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SrvOBJBugTrack - ServerObjects for JVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvOBJBugTrack;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, SrvConst, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef, SrvAccessDef;

type
  TServerObjectGET_BUGS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_BUGS_BY_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_BUGS_BY_MODULE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_MOST_SEVERE_BUGS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_BUG = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_BUG = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectASSIGN_REMOVE_BUG = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectENABLE_MODULE_PROJECT_BUG = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBUGS_USED_BY = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

implementation

{==============================================================================
  Add or remove a bug to a project/module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project/Module ID      - Integer
               [1]Bug ID                 - Integer
               [2]Project related        - Boolean
               [3]add the bug            - Boolean

  Response: 0: [0]Bug added              - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectASSIGN_REMOVE_BUG.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ItemID,
  BugID: Integer;
  ProjectRelated,
  AddBug,
  AlreadyAssiged: Boolean;
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

    ItemID := StrToInt(FRequestBuffer.Fields[0]);
    BugID := StrToInt(FRequestBuffer.Fields[1]);
    ProjectRelated := (FRequestBuffer.Fields[2] = '1');
    AddBug := (FRequestBuffer.Fields[3] = '1');

    FResponseBuffer.Rewrite;
    AlreadyAssiged := False;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        if AddBug then
        begin
          // add bug link
          // check if the bug is already assinged to the project
          SQL.Clear;
          if ProjectRelated then
          begin
            SQL.Add('SELECT projectid FROM pjbugs');
            SQL.Add('WHERE projectid = :itemid');
          end
          else
          begin
            SQL.Add('SELECT moduleid FROM mdbugs');
            SQL.Add('WHERE moduleid = :itemid');
          end;
          SQL.Add('AND bugid = :bugid');
          ParamByName('itemid').AsInteger := ItemID;
          ParamByName('bugid').AsInteger := BugID;
          Open;
          AlreadyAssiged := not Eof;
          Close;
          if not AlreadyAssiged then
          begin
            // no, add it
            SQL.Clear;
            if ProjectRelated then
            begin
              {$IFNDEF MSSQLSRV}
              SQL.Add('INSERT INTO pjbugs (recordid, bugid, projectid)');
              {$ELSE}
              SQL.Add('INSERT INTO pjbugs (bugid, projectid)');
              {$ENDIF ~MSSQLSRV}
              SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjbugs')]));              
              SQL.Add(':bugid, :projectid)');
              ParamByName('projectid').AsInteger := ItemID;
              ParamByName('bugid').AsInteger := BugID;
            end // if ProjectRelated then
            else
            begin
              {$IFNDEF MSSQLSRV}
              SQL.Add('INSERT INTO mdbugs (recordid, bugid, moduleid) ');
              {$ELSE}
              SQL.Add('INSERT INTO mdbugs (bugid, moduleid) ');
              {$ENDIF ~MSSQLSRV}
              SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('mdbugs')]));
              SQL.Add(':bugid, :moduleid)');
              ParamByName('moduleid').AsInteger := ItemID;
              ParamByName('bugid').AsInteger := BugID;
            end; // else if ProjectRelated then
            ExecSQL;
          end; // if not AlreadyAssiged then
        end // if AddBug then begin
        else
        begin
          // remove the bug link
          SQL.Clear;
          if ProjectRelated then
          begin
            SQL.Add('DELETE FROM pjbugs');
            SQL.Add('WHERE projectid = :itemid');
          end // if ProjectRelated then
          else
          begin
            SQL.Add('DELETE FROM mdbugs');
            SQL.Add('WHERE moduleid = :itemid');
          end; // else if ProjectRelated then
          SQL.Add('AND bugid = :bugid');
          ParamByName('itemid').AsInteger := ItemID;
          ParamByName('bugid').AsInteger := BugID;
          ExecSQL;
        end; // else if AddBug then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    if AddBug then
      FResponseBuffer.WriteFields(True, [not AlreadyAssiged])
    else
      FResponseBuffer.WriteFields(True, [True]);
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
  Mark a project/module specific bug as Done/not Done

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project/Module ID      - Integer
               [1]Bug ID                 - Integer
               [2]Project related        - Boolean
               [3]Done                   - Boolean

  Response: 0: [0]Bug added              - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectENABLE_MODULE_PROJECT_BUG.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ItemID,
  BugID: Integer;
  ProjectBug,
  EnableBug: Boolean;
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

    ItemID := StrToInt(FRequestBuffer.Fields[0]);
    BugID := StrToInt(FRequestBuffer.Fields[1]);
    ProjectBug := (FRequestBuffer.Fields[2] = '1');
    EnableBug := (FRequestBuffer.Fields[3] = '0');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        if ProjectBug then
        begin
          SQL.Add('UPDATE pjbugs');
          SQL.Add('SET done = :done');
          SQL.Add('WHERE projectid = :itemid');
        end // if ProjectBug then
        else
        begin
          SQL.Add('UPDATE mdbugs');
          SQL.Add('SET done = :done');
          SQL.Add('WHERE moduleid = :itemid');
        end; // else if ProjectBug then
        SQL.Add('AND bugid = :bugid');
        ParamByName('itemid').AsInteger := ItemID;
        if EnableBug then
          ParamByName('done').AsString := '0'
        else
          ParamByName('done').AsString := '1';
        ParamByName('bugid').AsInteger := BugID;
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
  Add or update a bug
  - if Bug ID (Request) = 0 add a new bug (check bug name before)
  - if Bug ID (Request) <> 0 change bug name, description... etc.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            0: [0]Bug ID                 - Integer
               [1]Bug                    - String(250)
               [2]Severity               - Integer
               [3]Flags                  - Integer
               [4]Bug description        - String(2000)
               [5]Bug keywords           - String(250)
               [6]Reported by            - String(2000)
               [7]Workaround             - String(2000)
               [8]Status                 - Integer

  Response: 0: [0]new bug?               - Boolean
               [1]Bug ID                 - Integer
                  (if request bug = 0 but bug name is already in use,
                   return false and the old ID)
               [2]Error message          - String
                  (if request bug = 0 but bug name is already in use)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_BUG.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  BugID,
  Severity,
  Status,
  BugFlags: Integer;
  BugName,
  BugDescr,
  BugKeywords,
  ReportedBy,
  Workaround: string;
  IsNewBug: Boolean;
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

    BugID := StrToInt(FRequestBuffer.Fields[0]);
    BugName := FRequestBuffer.Fields[1];
    Severity := StrToInt(FRequestBuffer.Fields[2]);
    BugFlags := StrToInt(FRequestBuffer.Fields[3]);
    BugDescr := FRequestBuffer.Fields[4];
    BugKeywords := FRequestBuffer.Fields[5];
    ReportedBy := FRequestBuffer.Fields[6];
    Workaround := FRequestBuffer.Fields[7];
    Status := StrToInt(FRequestBuffer.Fields[8]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        if BugID <> 0 then
        begin
          SQL.Clear;
          SQL.Add('UPDATE bugs');
          SQL.Add('SET bug = :bug, keywords = :keywords, severity = :severity,');
          SQL.Add('flags = :flags, description = :description,');
          SQL.Add('reportedby = :reportedby, workaround = :workaround,');
          SQL.Add('status = :status');
          SQL.Add('WHERE bugid = :bugid');
          ParamByName('bug').AsString := BugName;
          ParamByName('keywords').AsString := BugKeywords;
          ParamByName('severity').AsInteger := Severity;
          ParamByName('flags').AsInteger := BugFlags;
          ParamByName('description').AsString := BugDescr;
          ParamByName('reportedby').AsString := ReportedBy;
          ParamByName('workaround').AsString := Workaround;
          ParamByName('status').AsInteger := Status;
          ParamByName('bugid').AsInteger := BugID;
          ExecSQL;
          // return false
          FResponseBuffer.WriteFields(True, [False]);
          // return the old ID
          FResponseBuffer.WriteFields(False, [BugID]);
          // return blank error message
          FResponseBuffer.WriteFields(False, ['']);
        end // if BugID <> 0 then begin
        else
        begin
          {  create a new bug, check if we have already this name.
             BUG1 and bug1 are equal for the client !  }
          SQL.Clear;
          SQL.Add('SELECT bugid');
          SQL.Add('FROM bugs');
          SQL.Add('WHERE bug = :bug');
          ParamByName('bug').AsString := BugName;
          Open;
          IsNewBug := Eof;
          if not Eof then BugID := FQuery.FieldByName('bugid').AsInteger;
          Close;
          if IsNewBug then
          begin
            // create the new bug
            SQL.Clear;
            SQL.Add('INSERT INTO bugs');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('bugs')]));
            SQL.Add(':bug, :keywords, :severity, :created, :createdby,');
            SQL.Add(':flags, :description, :reportedby, :workaround, :status)');
            ParamByName('bug').AsString := BugName;
            ParamByName('keywords').AsString := BugKeywords;
            ParamByName('severity').AsInteger := Severity;
            ParamByName('created').AsDateTime := LocalDT2GMTDT(Now);
            ParamByName('createdby').AsInteger := AccessID;
            ParamByName('flags').AsInteger := BugFlags;
            ParamByName('description').AsString := BugDescr;
            ParamByName('reportedby').AsString := ReportedBy;
            ParamByName('workaround').AsString := Workaround;
            ParamByName('status').AsInteger := Status;
            ExecSQL;
            // get the new created bug ID
            SQL.Clear;
            SQL.Add('SELECT bugid');
            SQL.Add('FROM bugs');
            SQL.Add('WHERE bug = :bug');
            ParamByName('bug').AsString := BugName;
            Open;
            if not Eof then BugID := FQuery.FieldByName('bugid').AsInteger;
            Close;
            // return true
            FResponseBuffer.WriteFields(True, [True]);
            // return the new ID
            FResponseBuffer.WriteFields(False, [BugID]);
            // return blank error message
            FResponseBuffer.WriteFields(False, ['']);
          end // if IsNewBug then begin
          else
          begin
            // return false
            FResponseBuffer.WriteFields(True, [False]);
            // return the old ID
            FResponseBuffer.WriteFields(False, [BugID]);
            // return error message
            FResponseBuffer.WriteFields(False,
                           ['A bug with this name is already in the archive.']);
          end; // else if IsNewBug then begin
        end; // else if BugID <> 0 then begin
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
  Get a list of all bugs

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]incl. description      - Boolean

  Response: 0: [0]Bug ID                 - Integer
               [1]Bug                    - String(250)
               [2]Bug keywords           - String(250)
               [3]Severity               - Integer
               [4]Created                - Double (TDateTime)
               [5]Createdby              - String(50)
               [6]Flags                  - Integer
               [7]Status                 - Integer
               [8]Bug description        - String(2000)
                  (if incl. description = true, otherwise return a blank)
               [9]Reported by            - String(2000)
                  (if incl. description = true, otherwise return a blank)
              [10]Workaround             - String(2000)
                  (if incl. description = true, otherwise return a blank)

            1: [next bug...]
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_BUGS.Execute;
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
  InclDescr: Boolean;
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

    InclDescr := (FRequestBuffer.Fields[0] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        if InclDescr then
          SQL.Add('SELECT bugid, bug, keywords, severity, created, ' +
            'createdby, flags, status, description, reportedby, workaround')
        else
          SQL.Add('SELECT bugid, bug, keywords, severity, created, ' +
            'createdby, flags, status');
        SQL.Add('FROM bugs');
        SQL.Add('ORDER BY bug');
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              4: begin
                   DT := FQuery.FieldByName('created').AsDateTime;
                   FResponseBuffer.WriteFields(False, [DT]);
                 end;
              else FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
            end; // case Fld of
          end; // for Fld := 0 to FQuery.FieldCount - 1 do begin
          // return blanks instead of the description
          if not InclDescr then
          begin
            FResponseBuffer.WriteFields(False, ['']);
            FResponseBuffer.WriteFields(False, ['']);
            FResponseBuffer.WriteFields(False, ['']);
          end;
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
  Get a list of all bugs from one project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer

  Response: 0: [0]Bug ID                 - Integer
               [1]Bug name               - String(250)
               [2]Bug description        - String(2000)
               [3]Severity               - Integer
               [4]Done                   - Boolean

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_BUGS_BY_PROJECT.Execute;
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
        SQL.Add('SELECT pjbugs.bugid, bugs.bug,');
        SQL.Add('bugs.description, bugs.severity, pjbugs.done');
        SQL.Add('FROM bugs, pjbugs');
        SQL.Add('WHERE pjbugs.projectid = :projectid');
        SQL.Add('AND bugs.bugid = pjbugs.bugid');
        SQL.Add('ORDER BY pjbugs.bugid');
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
  Get a list of all bugs assigned to a single module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer

  Response: 0: [0]Bug ID                 - Integer
               [1]Bug name               - String(250)
               [2]Bug description        - String(2000)
               [3]Severity               - Integer
               [4]Done                   - Boolean

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_BUGS_BY_MODULE.Execute;
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
        SQL.Add('SELECT mdbugs.bugid, bugs.bug,');
        SQL.Add('bugs.description, bugs.severity, mdbugs.done');
        SQL.Add('FROM bugs, mdbugs');
        SQL.Add('WHERE mdbugs.moduleid = :moduleid');
        SQL.Add('AND bugs.bugid = mdbugs.bugid');
        SQL.Add('ORDER BY mdbugs.bugid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        while not Eof do
        begin // yes
          // Copy the record to the response
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
  Get a list of the bug with the highest severity for all modules or projects

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]ModuleBased            - Boolean

  Response: 0: [0]Module/Project ID      - Integer
               [1]Bug ID                 - Integer
               [2]Bug severity           - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_MOST_SEVERE_BUGS.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  CurrItemID,
  CurrBugID,
  MaxSeverity: Integer;
  ModuleBased: Boolean;
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

    ModuleBased := (FRequestBuffer.Fields[0] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        if ModuleBased then
        begin
          // related to modules
          SQL.Add('SELECT mdbugs.moduleid AS itemid,');
          SQL.Add('bugs.bugid, bugs.severity');
          SQL.Add('FROM bugs, mdbugs');
          SQL.Add('WHERE bugs.bugid = mdbugs.bugid');
          SQL.Add('AND mdbugs.done <> ''1''');
          SQL.Add('ORDER BY mdbugs.moduleid, bugs.bugid');
        end // if ModuleBased then
        else
        begin
          // related to projects
          SQL.Add('SELECT pjbugs.projectid AS itemid,');
          SQL.Add('bugs.bugid, bugs.severity');
          SQL.Add('FROM bugs, pjbugs');
          SQL.Add('WHERE bugs.bugid = pjbugs.bugid');
          SQL.Add('AND pjbugs.done <> ''1''');
          SQL.Add('ORDER BY pjbugs.projectid, bugs.bugid');
        end; // else if ModuleBased then
        Open;
        while not Eof do
        begin
          MaxSeverity := -1;
          CurrBugID := 0;
          CurrItemID := FQuery.FieldByName('itemid').AsInteger;
          // search for the bug with highest severity
          while (not Eof) and
                (CurrItemID = FQuery.FieldByName('itemid').AsInteger) do
          begin
            if FQuery.FieldByName('severity').AsInteger > MaxSeverity then
            begin
              MaxSeverity := FQuery.FieldByName('severity').AsInteger;
              CurrBugID := FQuery.FieldByName('bugid').AsInteger;
            end;
            Next;
          end; // while (not EoF) and...
          // return information
          FResponseBuffer.WriteFields(True, [CurrItemID]);
          FResponseBuffer.WriteFields(False, [CurrBugID]);
          FResponseBuffer.WriteFields(False, [MaxSeverity]);
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
  Get a list of all modules/projects assigned to a single bug

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Bug ID                 - Integer


  Response: 0: [0]Type of use            - String(1)
                  ('m' = module, 'p' = project)
               [1]Module/Project ID      - Integer
               [2]Module/Project name    - String(250)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectBUGS_USED_BY.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  BugID: Integer;
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

    BugID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // Step 1 : Get information about projects
        Close;
        SQL.Clear;
        SQL.Add('SELECT projects.projectid, projects.name');
        SQL.Add('FROM pjbugs, projects');
        SQL.Add('WHERE pjbugs.bugid = :bugid');
        SQL.Add('AND pjbugs.projectid = projects.projectid');
        ParamByName('bugid').AsInteger := BugID;
        Open;
        while not Eof do
        begin
          // insert 'p' (projects)
          FResponseBuffer.WriteFields(True, ['p']);
          // copy record to response
          for Fld := 0 to FQuery.FieldCount - 1 do
            FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
          Next;
        end; // while not EoF do begin
        Close;
        // Step 2 : Get information about modules
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, modules.name');
        SQL.Add('FROM mdbugs, modules');
        SQL.Add('WHERE mdbugs.bugid = :bugid');
        SQL.Add('AND mdbugs.moduleid = modules.moduleid');
        ParamByName('bugid').AsInteger := BugID;
        Open;
        while not Eof do
        begin
          // insert 'm' (modules)
          FResponseBuffer.WriteFields(True, ['m']);
          // copy record to response
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
  Remove a bug from 'Bugs'
  - if the bug is assigned to any revision or project, return false & do nothing

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Bug ID                 - Integer

  Response: 0: [0]bug removed?           - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_BUG.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  BugID: Integer;
  CanRemove: Boolean;
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

    BugID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if the bug is used in any project
        Close;
        SQL.Clear;
        SQL.Add('SELECT * FROM pjbugs');
        SQL.Add('WHERE bugid = :bugid');
        ParamByName('bugid').AsInteger := BugID;
        Open;
        CanRemove := Eof;
        Close;
        if CanRemove then
        begin
          // not used, check if the bug is used in any module
          SQL.Clear;
          SQL.Add('SELECT bugid FROM mdbugs');
          SQL.Add('WHERE bugid = :bugid');
          ParamByName('bugid').AsInteger := BugID;
          Open;
          CanRemove := Eof;
          Close;
        end; // if CanRemove then begin
        if CanRemove then
        begin
          SQL.Clear;
          SQL.Add('DELETE FROM bugs');
          SQL.Add('WHERE bugid = :bugid');
          ParamByName('bugid').AsInteger := BugID;
          ExecSQL;
          // return true
          FResponseBuffer.WriteFields(True, [True]);
        end // if CanRemove then begin
        else
        begin
          // return false
          FResponseBuffer.WriteFields(True, [False]);
        end; // else if CanRemove then begin
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

