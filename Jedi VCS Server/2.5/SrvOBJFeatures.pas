{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SvrOBJFeatures.pas

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
2004/12/18  USchuster- changes for Zeos DBO 6.x
2005/05/08  USchuster- removed unused units
2005/07/04  USchuster- TFVCSServerObject.VCSLog is used now to create log entries(mantis #2631)

  --- 2.50 Beta 2 was released...
2009/12/27  THuber   #5067 support for  D B I S A M removed

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SvrOBJFeatures - ServerObjects for JVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvOBJFeatures;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, SrvConst, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef, SrvAccessDef;

type
  TServerObjectGET_LABELS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LABELS_BY_PROJECT = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LABELS_BY_REVISION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_FILE_FAMILIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_FAMILY_EXTENSIONS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_MILESTONES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_MILESTONES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_MILESTONE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_MILESTONES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectASSIGN_PROJECT_MILESTONE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_PROJECT_MILESTONE = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectFAMILY_USED_BY = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_LABEL = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_UPDATE_FILE_FAMILIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_FILE_FAMILIES = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_LABEL = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_REMOVE_REVISION_LABEL = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectLABEL_USED_BY = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_UPDATE_DESCRIPTION = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

implementation

{==============================================================================
  Add or remove a label from a single revision ID
  - Add: if the label is assigned to any other revision of the Module return
         false & do nothing (only one revision of a module can be assigned with
         the same label)
  - Remove: if the label is not assigned do nothing

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer
               [1]Revision ID            - Integer
               [2]Label ID               - Integer
               [3]add the label          - Boolean
               [4]check only             - Boolean
                 (check only: check if we can assign this label,
                  but do not nothing)

  (only Add operation needs a return value)

  Response: 0: [0]label added?           - Boolean
                 (if Add is requested and the label is already assigned
                  return false)
               [1]Revision ID            - Integer
                 (if Add is requested.. ..return the old revision ID)
               [2]Version                - Integer
                 (if Add is requested.. .. return the old version)
               [3]Revision               - Integer
                 (if Add is requested.. .. return the old revision)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_REMOVE_REVISION_LABEL.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  ModuleID,
  RevisionID,
  LabelID: Integer;
  AddLabel,
  CheckOnly,
  CanAdd: Boolean;
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
    RevisionID := StrToInt(FRequestBuffer.Fields[1]);
    LabelID := StrToInt(FRequestBuffer.Fields[2]);
    AddLabel := (FRequestBuffer.Fields[3] = '1');
    CheckOnly := (FRequestBuffer.Fields[4] = '1');

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        if AddLabel then
        begin
          // check if the label is used in any revision of this module
          Close;
          SQL.Clear;
          SQL.Add('SELECT revision.revisionid, revision.version,');
          SQL.Add('revision.revision');
          SQL.Add('FROM rvlabels, revision');
          SQL.Add('WHERE rvlabels.labelid = :labelid');
          SQL.Add('AND revision.moduleid = :moduleid');
          SQL.Add('AND rvlabels.revisionid = revision.revisionid');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('labelid').AsInteger := LabelID;
          Open;
          CanAdd := Eof;
          if not Eof then
          begin
            // label is used in an other revision, return false
            FResponseBuffer.WriteFields(True, [False]);
            // return the old ID, version & revision
            for Fld := 0 to FQuery.FieldCount - 1 do
              FResponseBuffer.WriteFields(False, [FQuery.Fields[Fld].AsString]);
          end; // if not EoF then begin
          Close;
          if CanAdd then
          begin
            // label is not used in an other revision, return true
            FResponseBuffer.WriteFields(True, [True]);
            // assign the label now?
            if not CheckOnly then
            begin // yes
              // add label link
              SQL.Clear;
              SQL.Add('INSERT INTO rvlabels');
              SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('rvlabels')]));
              SQL.Add(':revisionid, :labelid)');
              ParamByName('revisionid').AsInteger := RevisionID;
              ParamByName('labelid').AsInteger := LabelID;
              ExecSQL;
            end; // if not CheckOnly then begin
          end; // if CanAdd then begin
        end // if AddLabel then begin
        else
        begin
          // remove the label link, no return value required
          SQL.Clear;
          SQL.Add('DELETE FROM rvlabels');
          SQL.Add('WHERE revisionid = :revisionid');
          SQL.Add('AND labelid = :labelid');
          ParamByName('revisionid').AsInteger := RevisionID;
          ParamByName('labelid').AsInteger := LabelID;
          ExecSQL;
        end; // else if AddLabel then begin
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
  Add or update a file family
  - if family ID (Request) = 0 add a new family (check label name before)
  - if family ID (Request) <> 0 change family name & other values

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            0: [0]Family ID              - Integer
               [1]Family Name            - String(50)
               [2]Parent Ext             - String(10)
               [2]Childs Ext             - String(255)
               [2]Family description     - String

  Response: 0: [0]new Family?            - Boolean
               [1]Family ID              - Integer
                  (if request family = 0 but family name is already in use,
                   return false and the old ID)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_FILE_FAMILIES.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  FamilyID: Integer;
  FamilyName,
  ParentExt,
  ChildsExt,
  FamilyDescr: string;
  IsNewFamily: Boolean;
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

    FamilyID := StrToInt(FRequestBuffer.Fields[0]);
    FamilyName := FRequestBuffer.Fields[1];
    ParentExt := FRequestBuffer.Fields[2];
    ChildsExt := FRequestBuffer.Fields[3];
    FamilyDescr := FRequestBuffer.Fields[4];

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        if FamilyID <> 0 then
        begin
          // update an old label
          with FQuery do
          begin
            Close;
            SQL.Clear;
            SQL.Add('UPDATE ffamily');
            SQL.Add('SET name = :name, parentext = :parentext,');
            SQL.Add('familyext = :familyext, description = :description');
            SQL.Add('WHERE familyid = :familyid');
            ParamByName('name').AsString := FamilyName;
            ParamByName('parentext').AsString := ParentExt;
            ParamByName('familyext').AsString := ChildsExt;
            ParamByName('description').AsString := FamilyDescr;
            ParamByName('familyid').AsInteger := FamilyID;
            ExecSQL;
          end; // with FQuery do begin
          // return false
          FResponseBuffer.WriteFields(True, [False]);
          // return the old ID
          FResponseBuffer.WriteFields(False, [FamilyID]);
        end // if FamilyID <> 0 then begin
        else
        begin
          {  create a new family, check if we have already this name.
             FAMILY1 and family1 are equal for the client !  }
          SQL.Clear;
          SQL.Add('SELECT familyid');
          SQL.Add('FROM ffamily');
          SQL.Add('WHERE name = :name');
          ParamByName('name').AsString := AnsiLowerCase(FamilyName);
          Open;
          IsNewFamily := Eof;
          if not Eof then FamilyID := FQuery.FieldByName('familyid').AsInteger;
          Close;
          if IsNewFamily then
          begin
            // create the new family
            with FQuery do
            begin
              Close;
              SQL.Clear;
              SQL.Add('INSERT INTO ffamily');
              SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('ffamily')]));
              SQL.Add(':name, :parentext, :familyext, :description)');
              ParamByName('name').AsString := FamilyName;
              ParamByName('parentext').AsString := ParentExt;
              ParamByName('familyext').AsString := ChildsExt;
              ParamByName('description').AsString := FamilyDescr;
              ExecSQL;
              // get the new created family ID
              SQL.Clear;
              SQL.Add('SELECT familyid FROM ffamily');
              SQL.Add('WHERE name = :name');
              ParamByName('name').AsString := FamilyName;
              Open;
              FamilyID := FQuery.FieldByName('familyid').AsInteger;
              Close;
            end; // with FQuery do begin
            // return true
            FResponseBuffer.WriteFields(True, [True]);
            // return the new ID
            FResponseBuffer.WriteFields(False, [FamilyID]);
          end // if IsNewFamily then begin
          else
          begin
            // return false
            FResponseBuffer.WriteFields(True, [False]);
            // return the old ID
            FResponseBuffer.WriteFields(False, [FamilyID]);
          end; // else if IsNewFamily then begin
        end; // else if FamilyID <> 0 then begin
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
  Add or update a label
  - if Label ID (Request) = 0 add a new label (check label name before)
  - if Label ID (Request) <> 0 change label name & description

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            0: [0]Label ID               - Integer
               [1]Label                  - String(255)
               [2]Label description      - String

  Response: 0: [0]new label?             - Boolean
               [1]Label ID               - Integer
                  (if request label = 0 but label name is already in use,
                   return false and the old ID)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_LABEL.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  LabelID: Integer;
  LabelName,
  LabelDescr: string;
  IsNewLabel: Boolean;
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

    LabelID := StrToInt(FRequestBuffer.Fields[0]);
    LabelName := FRequestBuffer.Fields[1];
    LabelDescr := FRequestBuffer.Fields[2];

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        if LabelID <> 0 then
        begin
          SQL.Clear;
          SQL.Add('UPDATE labels');
          SQL.Add('SET label = :label, description = :description');
          SQL.Add('WHERE labelid = :labelid');
          ParamByName('label').AsString := LabelName;
          ParamByName('description').AsString := LabelDescr;
          ParamByName('labelid').AsInteger := LabelID;
          ExecSQL;
          // return false
          FResponseBuffer.WriteFields(True, [False]);
          // return the old ID
          FResponseBuffer.WriteFields(False, [LabelID]);
        end // if LabelID <> 0 then begin
        else
        begin
          {  create a new label, check if we have already this name.
             LABEL1 and Label1 are equal for the client !  }
          SQL.Clear;
          SQL.Add('SELECT labelid');
          SQL.Add('FROM labels');
          SQL.Add('WHERE label = :label');
          ParamByName('label').AsString := AnsiLowerCase(LabelName);
          Open;
          IsNewLabel := Eof;
          if not Eof then LabelID := FQuery.FieldByName('labelid').AsInteger;
          Close;
          if IsNewLabel then
          begin
            // create the new label
            SQL.Clear;
            SQL.Add('INSERT INTO labels');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('labels')]));
            SQL.Add(':label, :description)');
            ParamByName('label').AsString := LabelName;
            ParamByName('description').AsString := LabelDescr;
            ExecSQL;
            // return true
            FResponseBuffer.WriteFields(True, [True]);
            // return the new ID
            FResponseBuffer.WriteFields(False, [LabelID]);
          end // if IsNewLabel then begin
          else
          begin
            // return false
            FResponseBuffer.WriteFields(True, [False]);
            // return the old ID
            FResponseBuffer.WriteFields(False, [LabelID]);
          end; // else if IsNewLabel then begin
        end; // else if LabelID <> 0 then begin
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
  Add or update a milestone
  - if milestone ID (Request) = 0 add a new milestone
  - if milestone ID (Request) <> 0 change milestone name & other values

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            0: [0]Milestone ID           - Integer
               [1]Milestone              - Integer
               [2]Milestone Name         - String(50)
               [3]Milestone description  - String

  Response: 0: [0]new milestone?            - Boolean
               [1]Milestone ID              - Integer
                  (if request family = 0 but family name is already in use,
                   return false and the old ID)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_MILESTONES.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Milestone,
  MilestoneID: Integer;
  MilestoneName,
  MilestoneDescr: string;
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

    MilestoneID := StrToInt(FRequestBuffer.Fields[0]);
    Milestone := StrToInt(FRequestBuffer.Fields[1]);
    MilestoneName := FRequestBuffer.Fields[2];
    MilestoneDescr := FRequestBuffer.Fields[3];

    FResponseBuffer.Rewrite;
    if MilestoneID <> 0 then
    begin
      // update an old milestone
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('UPDATE mstones');
          SQL.Add('SET milestone = :milestone, name = :name,');
          SQL.Add('description = :description');
          SQL.Add('WHERE milestoneid = :milestoneid');
          ParamByName('milestone').AsInteger := Milestone;
          ParamByName('name').AsString := MilestoneName;
          ParamByName('description').AsString := MilestoneDescr;
          ParamByName('milestoneid').AsInteger := MilestoneID;
          ExecSQL;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
      // return false
      FResponseBuffer.WriteFields(True, [False]);
      // return the old ID
      FResponseBuffer.WriteFields(False, [MilestoneID]);
    end // if MilestoneID <> 0 then begin
    else
    begin
      // create the new milestone
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO mstones');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('mstones')]));
          SQL.Add(':milestone, :name, :description)');
          ParamByName('milestone').AsInteger := Milestone;
          ParamByName('name').AsString := MilestoneName;
          ParamByName('description').AsString := MilestoneDescr;
          ExecSQL;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
      // return true
      FResponseBuffer.WriteFields(True, [True]);
      // return the new ID
      FResponseBuffer.WriteFields(False, [MilestoneID]);
    end; // else if MilestoneID <> 0 then begin
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
  Assign a milestone to a project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            0: [0]ProjectID              - Integer
               [1]Milestone ID           - Integer
               [2]Confirm                - String(50)
               [3]Description            - String

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectASSIGN_PROJECT_MILESTONE.Execute;
const
  RequestedRights = LevelPAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  MilestoneID: Integer;
  Confirm,
  Descr: string;
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
    MilestoneID := StrToInt(FRequestBuffer.Fields[1]);
    Confirm := FRequestBuffer.Fields[2];
    Descr := FRequestBuffer.Fields[3];

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        SQL.Add('INSERT INTO pjmstone');
        SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjmstone')]));
        SQL.Add(':milestoneid, :projectid, :confirm, :reached, :description)');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('milestoneid').AsInteger := MilestoneID;
        ParamByName('confirm').AsString := Confirm;
        ParamByName('reached').AsDateTime := LocalDT2GMTDT(Now);
        ParamByName('description').AsString := Descr;
        ExecSQL;
      end; // with FQuery do begin
      if UData.WriteVCSLog then
        VCSLog(UData.DBPath, ProjectID, AccessID, 0, 0, '_', 'Milestone ' +
          IntToStr(MilestoneID) + 'assigned by ' + Confirm);
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
  Get a list of all modules checked in/out using a specified file family

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer  

            1: [0]Family ID              - Integer


  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectFAMILY_USED_BY.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  FamilyID: Integer;
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

    FamilyID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT moduleid, name FROM modules');
        SQL.Add('WHERE familyid = :familyid');
        ParamByName('familyid').AsInteger := FamilyID;
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
  Get family extensions to a parent extension

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Parent extension       - String(10)

  Response: 0: [0]Family extensions      - String(255)
                  (return a blank if parent extension is undefined,
                   complete the string - if any - with a following
                   semicolon [i.e. '.dfm;' ] )

               [1]Family ID              - Integer

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_FAMILY_EXTENSIONS.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  FamilyID: Integer;
  ParentExt,
  FamilyExt: string;
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

    ParentExt := AnsiLowerCase(FRequestBuffer.Fields[0]);

    FamilyExt := '';
    FamilyID := 0;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT familyext, familyid');
        SQL.Add('FROM ffamily');
        SQL.Add('WHERE parentext = :parentext');
        ParamByName('parentext').AsString := ParentExt;
        Open;
        if not Eof then
        begin
          FamilyExt := FieldByName('familyext').AsString;
          FamilyID := FQuery.FieldByName('familyid').AsInteger;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;      
    if (FamilyExt <> '') and (FamilyExt[Length(FamilyExt)] <> ';')
      then FamilyExt := FamilyExt + ';';
    FResponseBuffer.WriteFields(True, [FamilyExt]);
    FResponseBuffer.WriteFields(False, [FamilyID]);
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
  Get a list of all file families

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]incl. description      - Boolean

  Response: 0: [0]Family ID              - Integer(50)
               [1]Name                   - String(50)
               [2]Parent                 - String(10)
               [3]Childs                 - String(255)
               [4]Family description     - String
                  (if incl. description = true, otherwise return a blank)

            1: [next record...]
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_FILE_FAMILIES.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
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
        if InclDescr
          then SQL.Add('SELECT familyid, name, parentext, familyext, description')
            else SQL.Add('SELECT familyid, name, parentext, familyext');
        SQL.Add('FROM ffamily');
        Open;
        while not Eof do
        begin
          // Copy the record to the response
          for Fld := 0 to FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
          // return a blank instead of the description
          if not InclDescr then FResponseBuffer.WriteFields(False, ['']);
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
  Get a list of all labels

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]incl. description      - Boolean

  Response: 0: [0]Label ID               - Integer
               [1]Label                  - String(255)
               [2]Label description      - String
                  (if incl. description = true, otherwise return a blank)

            1: [next label...]
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LABELS.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
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
        if InclDescr
          then SQL.Add('SELECT labelid, label, description')
            else SQL.Add('SELECT labelid, label');
        SQL.Add('FROM labels');
        SQL.Add('ORDER BY label');
        Open;
        while not Eof do
        begin
          // Copy the record to the response
          for Fld := 0 to FieldCount - 1 do
            FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
          // return a blank instead of the description
          if not InclDescr then FResponseBuffer.WriteFields(False, ['']);
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

  Get a list of all revision labels from one project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer

  Response: 0: [0]Module ID              - Integer
               [1]Revision ID            - Integer
               [2]Label ID               - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_LABELS_BY_PROJECT.Execute;
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
        SQL.Add('SELECT DISTINCT pjmodule.moduleid,');
        SQL.Add('revision.revisionid, rvlabels.labelid');
        SQL.Add('FROM pjmodule, rvlabels, revision');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND rvlabels.revisionid = revision.revisionid');
        SQL.Add('ORDER BY pjmodule.moduleid, revision.revisionid');
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
  Get a list of all labels assigned to a single revision of a module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Revision ID            - Integer

  Response: 0: [0]Label ID               - Integer
               [1]Label name             - String(50)
               [2]Label description      - String

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LABELS_BY_REVISION.Execute;
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
        Close;
        SQL.Clear;
        SQL.Add('SELECT labels.labelid, labels.label, labels.description');
        SQL.Add('FROM rvlabels, labels');
        SQL.Add('WHERE rvlabels.revisionid = :revisionid');
        SQL.Add('AND labels.labelid = rvlabels.labelid');
        ParamByName('revisionid').AsInteger := RevisionID;
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
  Get a list of all milestones

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Milestone ID           - Integer
               [1]Milestone              - Integer
               [2]Milestone name         - String(50)
               [3]Milestone description  - String

            1: [next milestone...]
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_MILESTONES.Execute;
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
        Close;
        SQL.Clear;
        SQL.Add('SELECT milestoneid, milestone, name, description');
        SQL.Add('FROM mstones');
        SQL.Add('ORDER BY milestone');
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
  Get a list of all milestones assigned to one project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               (zero = all projects)

  Response: 0: [0]Project ID             - Integer
               [1]Milestone ID           - Integer
               [2]Confirm by             - String(50)
               [3]Reached at             - Double (TDateTime)
               [4]Description            - String
               [5]Milestone              - Integer
               [6]Milestone name         - String(50)

            1: [next milestone...]
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_PROJECT_MILESTONES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
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
        SQL.Add('SELECT pjmstone.projectid, pjmstone.milestoneid,');
        {$IFNDEF MSSQLSRV}
        SQL.Add('pjmstone.confirm, pjmstone.reached,');
        {$ELSE} // MG: confirm is a reserved word in MSSQL
        SQL.Add('pjmstone.pjconfirm, pjmstone.reached,');
        {$ENDIF ~MSSQLSRV}
        SQL.Add('pjmstone.description,');
        SQL.Add('mstones.milestone, mstones.name');
        SQL.Add('FROM pjmstone, mstones');
        if ProjectID > 0 then
        begin
          SQL.Add('WHERE pjmstone.projectid = :projectid');
          SQL.Add('AND mstones.milestoneid = pjmstone.milestoneid');
        end else SQL.Add('WHERE mstones.milestoneid = pjmstone.milestoneid');
        SQL.Add('ORDER BY pjmstone.projectid, mstones.milestone');
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            case Fld of
              // MWBuffer workaround for DateTime fields
              3: begin
                   DT := FQuery.FieldByName('reached').AsDateTime;
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
  Get or update a project or module description

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Type                   - Integer
                  (1 = project
                   2 = module)
               [1]ID                     - Integer
               [2]Update                 - Boolean
               [3]Description            - String
                  (only if update is requested)

  Response: 0: [0]Description (if requested) - String

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_UPDATE_DESCRIPTION.Execute;
const
  RequestedRightsGet = LevelNone;
  RequestedRightsUpdate = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  RequestedRights,
  DescrType,
  DescrID: Integer;
  Param1,
  Param2,
  DescriptionTxt: string;
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
    FRequestBuffer.Next;
    Update := (FRequestBuffer.Fields[2] = '1');
    if Update then RequestedRights := RequestedRightsUpdate
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

    DescrType := StrToInt(FRequestBuffer.Fields[0]);
    DescrID := StrToInt(FRequestBuffer.Fields[1]);
    DescriptionTxt := FRequestBuffer.Fields[3];

    FResponseBuffer.Rewrite;
    // description type?
    case DescrType of
      1: begin // project
           Param1 := 'projects';
           Param2 := 'projectid';
         end;
      2: begin // module
           Param1 := 'modules';
           Param2 := 'moduleid';
         end;
    end; // case DescrType of
    if not Update then
    begin
      // Get is requested
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          Close;
          SQL.Clear;
          SQL.Add('SELECT description');
          SQL.Add('FROM ' + Param1);
          SQL.Add('WHERE ' + Param2 + ' = :id');
          ParamByName('id').AsInteger := DescrID;
          Open;
          while not Eof do
          begin
            FResponseBuffer.WriteFields(True,
                                         [FieldByName('description').AsString]);
            Next;
          end; // while not EoF do begin
          Close;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
    end // if not Update then begin
    else
    begin
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('UPDATE ' + Param1);
          SQL.Add('SET description = :description');
          SQL.Add('WHERE ' + Param2 + '  = :id');
          ParamByName('description').AsString := DescriptionTxt;
          ParamByName('id').AsInteger := DescrID;
          ExecSQL;
        end; // with FQuery do begin
      finally
        FQuery.Free;
      end;
    end; // if not Update then begin
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
  Get a list of all versions/revisions assigned to a single label

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Label ID               - Integer


  Response: 0: [0]Module name            - String(255)
               [1]Module version         - Integer
               [3]Module revision        - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectLABEL_USED_BY.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld,
  LabelID: Integer;
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

    LabelID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.name, revision.version,');
        SQL.Add('revision.revision');
        SQL.Add('FROM rvlabels, modules, revision');
        SQL.Add('WHERE rvlabels.labelid = :labelid');
        SQL.Add('AND rvlabels.revisionid = revision.revisionid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        ParamByName('labelid').AsInteger := LabelID;
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
  Remove a file family from 'FFamily'

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Family ID              - Integer

  Response: 0: [0]Deleted?               - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_FILE_FAMILIES.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  FamilyID: Integer;
  FamilyInUse: Boolean;
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

    FamilyID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if there are any links to this family
        SQL.Add('SELECT moduleid FROM modules');
        SQL.Add('WHERE familyid = :familyid');
        ParamByName('familyid').AsInteger := FamilyID;
        Open;
        FamilyInUse := not Eof;
        Close;
        if FamilyInUse then
          // Family used by some modules, we cannot delete it, return false
          FResponseBuffer.WriteFields(False, [False])
        else // if FamilyInUse then begin
        begin
          // remove the family, return true
          FResponseBuffer.WriteFields(False, [True]);
          SQL.Clear;
          SQL.Add('DELETE FROM ffamily');
          SQL.Add('WHERE familyid = :familyid');
          ParamByName('familyid').AsInteger := FamilyID;
          ExecSQL;
        end; // else if FamilyInUse then begin
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
  Remove a label from 'Labels'
  - if the label is assigned to any revision, return false & do nothing

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Label ID               - Integer

  Response: 0: [0]label removed?         - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_LABEL.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  LabelID: Integer;
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

    LabelID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if the label is used in any revision
        Close;
        SQL.Clear;
        SQL.Add('SELECT * FROM rvlabels');
        SQL.Add('WHERE labelid = :labelid');
        ParamByName('labelid').AsInteger := LabelID;
        Open;
        CanRemove := Eof;
        Close;
        if CanRemove then
        begin
          SQL.Clear;
          SQL.Add('DELETE FROM labels');
          SQL.Add('WHERE labelid = :labelid');
          ParamByName('labelid').AsInteger := LabelID;
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
{==============================================================================
  Remove a milestone from the archive

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Milestone ID              - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_MILESTONE.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  MilestoneID: Integer;
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

    MilestoneID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // delete all links in PJMStone
        SQL.Clear;
        SQL.Add('DELETE FROM pjmstone');
        SQL.Add('WHERE milestoneid = :milestoneid');
        ParamByName('milestoneid').AsInteger := MilestoneID;
        ExecSQL;
        // delete the milestone
        SQL.Clear;
        SQL.Add('DELETE FROM mstones');
        SQL.Add('WHERE milestoneid = :milestoneid');
        ParamByName('milestoneid').AsInteger := MilestoneID;
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
  Remove a milestone from one project

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Milestone ID           - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_PROJECT_MILESTONE.Execute;
const
  RequestedRights = LevelPAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  MilestoneID: Integer;
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
    MilestoneID := StrToInt(FRequestBuffer.Fields[1]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        SQL.Add('DELETE FROM pjmstone');
        SQL.Add('WHERE milestoneid = :milestoneid');
        SQL.Add('AND projectid = :projectid');
        ParamByName('milestoneid').AsInteger := MilestoneID;
        ParamByName('projectid').AsInteger := ProjectID;
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

end.

