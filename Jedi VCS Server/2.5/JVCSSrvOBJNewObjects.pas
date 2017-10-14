(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSSrvOBJNewObjects.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
Dec/04
- USc: - only tested with MySQL and partly with Oracle
ToDo
- enter functions into client code generator (2008/11/07: almost done)
-----------------------------------------------------------------------------

Unit history:

2004/12/11  USchuster - new unit
2005/10/31  USchuster - extended LIST_FILES
2006/11/13  USchuster - added SEARCH_REVISIONS_BY_COMMENT, SEARCH_MODULE_REVISIONS_BY_CRC
                        and GET_LATEST_REVISIONS
2006/11/23  USchuster - added first implementation of property functions
2006/11/26  USchuster - added support for module, revision and user properties
2007/02/04  USchuster - implemented some todo's in REMOVE_PROPERTY and ADD_UPDATE_PROPERTIES
2008/09/28  USchuster - some minor fixes for Oracle
2008/11/05  USchuster - fixed content handling in ADD_UPDATE_PROPERTIES and GET_PROPERTIES
                      - added GET_MODULE_REVISION_PROPERTIES
2008/11/07  USchuster - changes in ADD_UPDATE_PROPERTIES, GET_PROPERTIES and
                        GET_MODULE_REVISION_PROPERTIES for property header and content size
                      - improved speed of GET_MODULE_REVISION_PROPERTIES
2008/11/10  USchuster - GET_MODULE_REVISION_PROPERTIES does now use join
2008/11/13  USchuster - added GET_LABELS_BY_MODULE
2008/11/15  USchuster - added GET_SUPPORTED_FUNCTIONS
2008/11/25  USchuster - extended LIST_FILES (returns now all revision extension)
2009/01/17  USchuster - moved property functions to JVCSSrvObjProperties.pas
                      - moved TJVCSServerObjectEx to SrvCustomSrvOBJ.pas
2009/04/11  USchuster - improved GET_SUPPORTED_FUNCTIONS (it does now return more information
                        and does use TJVCSServerObjectEx.GetInformation)
                      - implemented TJVCSServerObjectEx.DoGetInformation for GET_LABELS_BY_MODULE
2010/01/17  USchuster - changes in LIST_FILES for branch tables
2010/01/23  USchuster - added GET_LATESTREVISION_BY_ID (Mantis #5082)

-----------------------------------------------------------------------------*)

unit JVCSSrvOBJNewObjects;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Classes, RBroker, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef, SrvAccessDef,
  JclFileUtils;

procedure AddNewServerObjects(ARequestBroker: TRequestBroker);

implementation

{$IFDEF BRANCHOBJECTS}
uses
  JVCSSrvObjBranches, JVCSSessionList;
{$ENDIF BRANCHOBJECTS}

type
  TServerObjectLIST_FILES = class(TJVCSServerObjectEx)
  protected
    class procedure DoGetInformation(var AInformation: TJVCSServerFunctionInformation); override;
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectSEARCH_REVISIONS_BY_COMMENT = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectSEARCH_MODULE_REVISIONS_BY_CRC = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectGET_LATEST_REVISIONS = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectGET_LABELS_BY_MODULE = class(TJVCSServerObjectEx)
  protected
    class procedure DoGetInformation(var AInformation: TJVCSServerFunctionInformation); override;
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectGET_SUPPORTED_FUNCTIONS = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectGET_LATESTREVISION_BY_ID = class(TJVCSServerObjectEx)
  protected
    class procedure DoGetInformation(var AInformation: TJVCSServerFunctionInformation); override;
  public
    procedure ExecuteEx; override;
  end;

procedure AddNewServerObjects(ARequestBroker: TRequestBroker);
begin
  with ARequestBroker do
  begin
    AddServerObject(TServerObjectLIST_FILES);
    AddServerObject(TServerObjectSEARCH_REVISIONS_BY_COMMENT);
    AddServerObject(TServerObjectSEARCH_MODULE_REVISIONS_BY_CRC);
    AddServerObject(TServerObjectGET_LATEST_REVISIONS);
    AddServerObject(TServerObjectGET_LABELS_BY_MODULE);
    AddServerObject(TServerObjectGET_SUPPORTED_FUNCTIONS);
    AddServerObject(TServerObjectGET_LATESTREVISION_BY_ID);
  end;
end;

function FixBackSlash(AString: string; BackSlashCount: Integer): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AString) do
    if AString[I] <> '\' then
      Result := Result + AString[I]
    else
      Result := Result + StringOfChar('\', BackSlashCount);
end;

procedure GetPathList(APathList: TStringList; UData: TUserDataRecord; ABaseDir: string{$IFDEF BRANCHOBJECTS}; ABranchID: Integer = -1{$ENDIF});
var
  FQuery: TSrvQuery;
  I, J, P1, NIdx: Integer;
  S, S2: string;
  TempStrings: TStringList;
begin
  if ABaseDir <> '' then
    ABaseDir := PathAddSeparator(ABaseDir);
  FQuery := TSrvQuery.Create(nil);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      Close;
      SQL.Clear;
      {$IFDEF BRANCHOBJECTS}
      SQL.Add('SELECT path FROM brmodule');
      SQL.Add('WHERE brmodule.branchid = :branchid');
      if ABaseDir <> '' then
      begin
        //Like as parameter seems not to work with Firebird
        //SQL.Add('AND path like :path');
        {$IFDEF MYSQLSRV}
        S := FixBackSlash(ABaseDir, 2);
        {$ELSE}
        S := ABaseDir;
        {$ENDIF MYSQLSRV}
        //Like as parameter seems not to work with Firebird
        //ParamByName('path').AsString := S + '%';
        SQL.Add('AND path like ' + QuotedStr(S + '%'));
      end;
      SQL.Add('GROUP BY path');
      ParamByName('branchid').AsInteger := ABranchID;
      {$ELSE ~BRANCHOBJECTS}
      SQL.Add('SELECT path FROM modules');
      if ABaseDir <> '' then
      begin
        SQL.Add('WHERE path like :path');
        {$IFDEF MYSQLSRV}
        S := FixBackSlash(ABaseDir, 2);
        {$ELSE}
        S := ABaseDir;
        {$ENDIF MYSQLSRV}
        ParamByName('path').AsString := S + '%';
      end;
      SQL.Add('GROUP BY path');
      {$ENDIF ~BRANCHOBJECTS}
      Open;
      while not Eof do
      begin
        APathList.Add(Fields[0].AsString);
        Next;
      end;
      Close;
    end;
  finally
    FQuery.Free;
  end;
  TempStrings := TStringList.Create;
  try
    TempStrings.Sorted := True;
    for I := Pred(APathList.Count) downto 0 do
    begin
      S := APathList[I];
      if ABaseDir <> '' then
        Delete(S, 1, Length(ABaseDir));
      P1 := Pos('\', S);
      if P1 > 0 then
        Delete(S, P1 + 1, Length(S) - P1);
      if (S <> '') and (TempStrings.IndexOf(S) = - 1) then
      begin
        NIdx := TempStrings.Add(S);
        S2 := ABaseDir + S;
        for J := 0 to Pred(APathList.Count) do
          if (Pos(S2, APathList[J]) = 1) and
            (Length(APathList[J]) > Length(S2)) then
          begin
            TempStrings.Objects[NIdx] := TObject(1);
            Break;
          end;
      end;
    end;
    APathList.Assign(TempStrings);
  finally
    TempStrings.Free;
  end;
end;

{$IFNDEF BRANCHOBJECTS}
var
  LastArchiveTimeStamp: TDateTime = 0;

procedure UpdateLatestRevisions(UData: TUserDataRecord);
var
  FQuery, FQuery2: TSrvQuery;
  LastModuleID: Integer;
begin
  if LastArchiveTimeStamp <> UData.ArchiveTimeStamp then
  begin
    FQuery2 := TSrvQuery.Create(nil);
    try
      FQuery2.DatabaseName := UData.DBPath;
      with FQuery2 do
      begin
        SQL.Clear;
        SQL.Add('DELETE from latestrevision');
        ExecSQL;
        SQL.Clear;
        SQL.Add('INSERT INTO latestrevision(moduleid, revisionid)');
        SQL.Add('VALUES(:moduleid,:revisionid)');
      end;
      FQuery := TSrvQuery.Create(nil);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          Close;
          SQL.Clear;
          SQL.Add('SELECT moduleid, version, revision, revisionid FROM revision');
          SQL.Add('ORDER BY moduleid, version DESC, revision DESC');
          Open;
          LastModuleID := -1;
          while not Eof do
          begin
            if Fields[0].AsInteger <> LastModuleID then
            begin
              LastModuleID := Fields[0].AsInteger;
              FQuery2.ParamByName('moduleid').AsInteger := LastModuleID;
              FQuery2.ParamByName('revisionid').AsInteger := Fields[3].AsInteger;
              FQuery2.ExecSQL;
            end;
            Next;
          end;
          Close;
        end;
      finally
        FQuery.Free;
      end;
    finally
      FQuery2.Free;
    end;
    LastArchiveTimeStamp := UData.ArchiveTimeStamp;
  end;
end;
{$ENDIF ~BRANCHOBJECTS}

function GetPathLatestRevisionInfo(UData: TUserDataRecord; const APath: string;
  var AVersion, ARevision: Integer; var AUser, AComment_I: string; var ADate: TDateTime{$IFDEF BRANCHOBJECTS}; ABranchID: Integer = -1{$ENDIF}): Boolean;
var
  FQuery: TSrvQuery;
  LastModuleID: Integer;
  S: string;
begin
  Result := False;
  FQuery := TSrvQuery.Create(nil);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      SQL.Clear;
      {$IFDEF BRANCHOBJECTS}
      SQL.Add('SELECT brmodule.name, brmodule.moduleid, users.login, revision.version, revision.revision, revision.comment_i, blobs.origtime');
      SQL.Add('FROM modules, brmodule, latestrevision, revision, users, blobs');
      //Like as parameter seems not to work with Firebird
      //SQL.Add('WHERE modules.path like :path');
      SQL.Add('WHERE modules.path like ' + QuotedStr(APath + '%'));
      SQL.Add('AND brmodule.branchid = :branchid');
      SQL.Add('AND brmodule.moduleid = modules.moduleid');
      SQL.Add('AND latestrevision.branchid = :branchid');
      SQL.Add('AND latestrevision.moduleid = modules.moduleid');
      SQL.Add('AND revision.revisionid = latestrevision.revisionid');
      SQL.Add('AND users.userid = revision.userid');
      SQL.Add('AND blobs.revisionid = revision.revisionid');
      SQL.Add('ORDER BY brmodule.name, modules.moduleid, blobs.origtime DESC');
      ParamByName('branchid').AsInteger := ABranchID;
      {$ELSE ~BRANCHOBJECTS}
      SQL.Add('SELECT modules.name, modules.moduleid, users.login, revision.version, revision.revision, revision.comment_i, blobs.origtime');
      SQL.Add('FROM modules, latestrevision, revision, users, blobs');
      SQL.Add('WHERE modules.path like :path');
      SQL.Add('AND latestrevision.moduleid = modules.moduleid');
      SQL.Add('AND revision.revisionid = latestrevision.revisionid');
      SQL.Add('AND users.userid = revision.userid');
      SQL.Add('AND blobs.revisionid = revision.revisionid');
      SQL.Add('ORDER BY modules.name, modules.moduleid, blobs.origtime DESC');
      {$ENDIF ~BRANCHOBJECTS}
      {$IFDEF MYSQLSRV}
      S := FixBackSlash(APath, 2);
      {$ELSE}
      S := APath;
      {$ENDIF MYSQLSRV}
      //Like as parameter seems not to work with Firebird
      //ParamByName('path').AsString := S + '%';
      Open;
      LastModuleID := -1;
      if not Eof then
      begin
        ADate := 0;
        Result := True;
      end;
      while not Eof do
      begin
        if LastModuleID <> Fields[1].AsInteger then
        begin
          LastModuleID := Fields[1].AsInteger;
          if Fields[6].AsDateTime > ADate then
          begin
            ADate := Fields[6].AsDateTime;
            AUser := Fields[2].AsString;
            AVersion := Fields[3].AsInteger;
            ARevision := Fields[4].AsInteger;            
            AComment_I := Fields[5].AsString;
          end;
        end;
        Next;
      end;
      Close;
    end;
  finally
    FQuery.Free;
  end;
end;

{==============================================================================
  List all files and directories in a specific directory

  TODO:
  - branch filter
  - project filter

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Dir                    - string[???]

  Response: 0: [0]name                   - string[???]
               [1]kind                   - Integer
                  0 file
                  1 directory
                  2 directory with subdirs
               [2]ModuleID               - Integer
               [3]LatestRevisionID       - Integer
               [4]LatestRevisionUser     - string
               [5]LatestRevisionVersion  - Integer
               [6]LatestRevisionRevision - Integer
               [7]LatestRevisionComment_I- string
               [8]LatestRevisionExtension- string[20]
               [9]LatestRevisionTS       - DateTime
              [10]LatestRevisionOrigSize - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
class procedure TServerObjectLIST_FILES.DoGetInformation(var AInformation: TJVCSServerFunctionInformation);
begin
  AInformation.Publishable := True;
  AInformation.RequiredRights := LevelRO;
  AInformation.RequiredRightsDomain := rrdArchive;
end;

procedure TServerObjectLIST_FILES.ExecuteEx;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights: Integer;
  BaseDir: string;
  PathList: TStringList;
  I: Integer;
  LastModuleID: Integer;
  DT: Double;
  PathLatestRevisionVersion, PathLatestRevisionRevision: Integer;
  PathLatestRevisionUser, PathLatestRevisionComment_I: string;
  PathLatestRevisionDate: TDateTime;
  PathKind: Integer;
  {$IFDEF BRANCHOBJECTS}
  BranchID: Integer;
  {$ENDIF BRANCHOBJECTS}
begin
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

  BaseDir := FRequestBuffer.Fields[0];

  {$IFDEF BRANCHOBJECTS}
  BranchID := SessionList.GetSessionBranchID(AccessID, TANr);
  {$ENDIF BRANCHOBJECTS}

  FResponseBuffer.Rewrite;
  PathList := TStringList.Create;
  try
    UpdateLatestRevisions(UData^{$IFDEF BRANCHOBJECTS}, BranchID{$ENDIF});
    GetPathList(PathList, UData^, BaseDir {$IFDEF BRANCHOBJECTS}, BranchID{$ENDIF});
    if BaseDir <> '' then
    begin
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          SQL.Clear;
          {$IFDEF BRANCHOBJECTS}
          SQL.Add('SELECT brmodule.name, brmodule.moduleid, revision.revisionid, users.login,');
          SQL.Add('revision.version, revision.revision, revision.comment_i, blobs.extension, blobs.origtime, blobs.origsize');
          SQL.Add('FROM modules, brmodule, latestrevision, revision, users, blobs');
          SQL.Add('WHERE modules.path = :path');
          SQL.Add('AND brmodule.branchid = :branchid');
          SQL.Add('AND brmodule.moduleid = modules.moduleid');
          SQL.Add('AND latestrevision.branchid = :branchid');
          SQL.Add('AND latestrevision.moduleid = modules.moduleid');
          SQL.Add('AND revision.revisionid = latestrevision.revisionid');
          SQL.Add('AND users.userid = revision.userid');
          SQL.Add('AND blobs.revisionid = revision.revisionid');
          SQL.Add('ORDER BY brmodule.name, brmodule.moduleid, blobs.origtime DESC');
          ParamByName('branchid').AsInteger := BranchID;
          {$ELSE ~BRANCHOBJECTS}
          SQL.Add('SELECT modules.name, modules.moduleid, revision.revisionid, users.login,');
          SQL.Add('revision.version, revision.revision, revision.comment_i, blobs.extension, blobs.origtime, blobs.origsize');
          SQL.Add('FROM modules, latestrevision, revision, users, blobs');
          SQL.Add('WHERE modules.path = :path');
          SQL.Add('AND latestrevision.moduleid = modules.moduleid');
          SQL.Add('AND revision.revisionid = latestrevision.revisionid');
          SQL.Add('AND users.userid = revision.userid');
          SQL.Add('AND blobs.revisionid = revision.revisionid');
          SQL.Add('ORDER BY modules.name, modules.moduleid, blobs.origtime DESC');
          {$ENDIF ~BRANCHOBJECTS}
          ParamByName('path').AsString := BaseDir;
          Open;
          //LastModuleID := -1;
          while not Eof do
          begin
            //if LastModuleID <> Fields[1].AsInteger then
            begin
              LastModuleID := Fields[1].AsInteger;
              DT := Fields[8].AsDateTime;
              FResponseBuffer.WriteFields(True, [Fields[0].AsString, 0,
                LastModuleID, Fields[2].AsInteger, Fields[3].AsString, Fields[4].AsInteger, Fields[5].AsInteger,
                Fields[6].AsString, Fields[7].AsString, DT, Fields[9].AsInteger]);
            end;
            Next;
          end;
          Close;
        end;
      finally
        FQuery.Free;
      end;
    end;
    for I := 0 to Pred(PathList.Count) do
    begin
      if not GetPathLatestRevisionInfo(UData^, BaseDir + PathList[I], PathLatestRevisionVersion,
        PathLatestRevisionRevision, PathLatestRevisionUser, PathLatestRevisionComment_I, PathLatestRevisionDate{$IFDEF BRANCHOBJECTS}, BranchID{$ENDIF}) then
      begin
        PathLatestRevisionVersion := 0;
        PathLatestRevisionRevision := 0;
        PathLatestRevisionUser := '';
        PathLatestRevisionComment_I := '';
        PathLatestRevisionDate := 0;
      end;
      DT := PathLatestRevisionDate;
      if PathList.Objects[I] <> TObject(1) then
        PathKind := 1
      else
        PathKind := 2;
      FResponseBuffer.WriteFields(True, [PathList[I], PathKind, -1, -1, PathLatestRevisionUser, PathLatestRevisionVersion,
        PathLatestRevisionRevision, PathLatestRevisionComment_I, '', DT, 0]);
    end;
  finally
    PathList.Free;
  end;
  FResultStatus := 200; // OK
end;

{==============================================================================
  Search all revisions with the given comment

  TODO:
  - branch filter
  - project filter

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Mask string            - String(255)
               [1]Include Module Info    - Boolean

  Response: 0: [0]RevisionID             - Integer
               [1]Version                - Integer
               [2]Revision               - Integer
               [3]Comment_I              - String(2000)
               [4]Comment_O              - String(2000)
               [5]Module ID              - Integer
               [6]Module name            - String(255)
               [7]Module path            - String(255)               

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectSEARCH_REVISIONS_BY_COMMENT.ExecuteEx;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights: Integer;
  MaskString: string;
  IncludeModuleInfo: Boolean;
begin
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

  MaskString := FRequestBuffer.Fields[0];
  IncludeModuleInfo := FRequestBuffer.Fields[1] = '1';

  FResponseBuffer.Rewrite;
  if MaskString <> '' then
  begin
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        if IncludeModuleInfo then
        begin
          SQL.Add('SELECT revision.revisionid, version, revision, comment_i, comment_o, revision.moduleid, modules.name, modules.path');
          SQL.Add('FROM revision, modules');
          SQL.Add('WHERE (revision.comment_i LIKE :MASKSTRING');
          SQL.Add('OR revision.comment_o LIKE :MASKSTRING)');
          SQL.Add('AND modules.moduleid = revision.moduleid');
        end
        else
        begin
          SQL.Add('SELECT revisionid, version, revision, comment_i, comment_o, moduleid');
          SQL.Add('FROM revision');
          SQL.Add('WHERE comment_i LIKE :MASKSTRING');
          SQL.Add('OR comment_o LIKE :MASKSTRING');
        end;
        ParamByName('MASKSTRING').AsString := MaskString;
        Open;
        while not Eof do
        begin
          FResponseBuffer.WriteFields(True, [Fields[0].AsInteger, Fields[1].AsInteger,
            Fields[2].AsInteger, Fields[3].AsString, Fields[4].AsString, Fields[5].AsString]);
          if IncludeModuleInfo then
            FResponseBuffer.WriteFields(False, [Fields[6].AsString, Fields[7].AsString]);
          Next;
        end;
        Close;
      end;
    finally
      FQuery.Free;
    end;
  end;
  FResultStatus := 200; // OK
end;

{==============================================================================
  Search all module revisions for the the given Module ID, extension and CRC

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer
               [1]Module Extension       - String(20)
               [2]Module orig. CRC       - Integer

            2: [0]next Module Extension
               [...]

  Response: 0: [0]Module ID              - Integer
               [1]Module Extension       - String(20)
               [2]RevisionID             - Integer
               [3]Version                - Integer
               [4]Revision               - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectSEARCH_MODULE_REVISIONS_BY_CRC.ExecuteEx;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights: Integer;
  ModuleID: Integer;
  Extension: string;
begin
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
      SQL.Clear;
      SQL.Add('SELECT revision.revisionid, version, revision');
      SQL.Add('FROM revision, blobs');
      SQL.Add('WHERE revision.moduleid = :MODULEID');
      SQL.Add('AND blobs.revisionid = revision.revisionid');
      SQL.Add('AND blobs.extension = :EXTENSION');
      SQL.Add('AND blobs.origcrc = :CRC');
      while not FRequestBuffer.Eof do
      begin
        ModuleID := StrToIntDef(FRequestBuffer.Fields[0], 0);
        if ModuleID <> 0 then
        begin
          Extension := FRequestBuffer.Fields[1];
          ParamByName('MODULEID').AsInteger := ModuleID;
          ParamByName('EXTENSION').AsString := Extension;
          ParamByName('CRC').AsInteger := StrToIntDef(FRequestBuffer.Fields[2], 0);
          Open;
          if not (Bof and Eof) then
          begin
            while not Eof do
            begin
              FResponseBuffer.WriteFields(True, [ModuleID, Extension, Fields[0].AsInteger, Fields[1].AsInteger, Fields[2].AsInteger]);
              Next;
            end;
          end
          else
            FResponseBuffer.WriteFields(True, [ModuleID, Extension, 0, 0, 0]);
          Close;
        end;
        FRequestBuffer.Next;
      end;
    end;
  finally
    FQuery.Free;
  end;
  FResultStatus := 200; // OK
end;

{==============================================================================
  This version of GET_LATEST_REVISION is extended by Field 9, 10, 11

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
               [9]Hidden                 - String(1)
                  ('1' = hidden, '0' = visible)
              [10]Checked Out            - Boolean
              [11]UserID                 - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectGET_LATEST_REVISIONS.ExecuteEx;
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
      SQL.Add('blobs.extension, blobs.origcrc, pjmodule.hidden, modules.readonly, modules.userid');
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
end;

{==============================================================================
  Get a list of all labels assigned to all revisions of a module

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer

  Response: 0: [0]Revision ID            - Integer
               [1]Label ID               - Integer
               [2]Label name             - String(50)
               [3]Label description      - String

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
class procedure TServerObjectGET_LABELS_BY_MODULE.DoGetInformation(var AInformation: TJVCSServerFunctionInformation);
begin
  AInformation.Publishable := True;
  AInformation.RequiredRights := LevelRO;
  AInformation.RequiredRightsDomain := rrdProject;
end;

procedure TServerObjectGET_LABELS_BY_MODULE.ExecuteEx;
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
      SQL.Add('SELECT revision.revisionid, labels.labelid, labels.label, labels.description');
      SQL.Add('FROM revision, rvlabels, labels');
      SQL.Add('WHERE revision.moduleid = :moduleid');
      SQL.Add('AND rvlabels.revisionid = revision.revisionid');
      SQL.Add('AND labels.labelid = rvlabels.labelid');
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
end;

{==============================================================================
  Get a list of supported functions

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Function name          - String(100)
               [1]Version                - String
               [2]Required Rights Domain - String(1)
                 ('a' = archive, 'p' = project, 'u' = unknown)
               [3]Required Rights        - Integer

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
type
  TRequestBrokerFunctionEnumerator = class(TObject)
  private
    FFunctionList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function AddFunctions(Sender: TObject; FunctionCode: string): Boolean;
    function ExistsServerObject(const AFunctionCode: string): Boolean;
  end;

constructor TRequestBrokerFunctionEnumerator.Create;
begin
  inherited Create;
  FFunctionList := TStringList.Create;
  FFunctionList.Sorted := True;
end;

destructor TRequestBrokerFunctionEnumerator.Destroy;
begin
  FFunctionList.Free;
  inherited Destroy;
end;

function TRequestBrokerFunctionEnumerator.AddFunctions(Sender: TObject; FunctionCode: string): Boolean;
begin
  FFunctionList.Add(FunctionCode);
  Result := True;
end;

function TRequestBrokerFunctionEnumerator.ExistsServerObject(const AFunctionCode: string): Boolean;
begin
  Result := FFunctionList.IndexOf(AFunctionCode) <> -1;
end;

procedure TServerObjectGET_SUPPORTED_FUNCTIONS.ExecuteEx;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  TANr,
  AccessID,
  GrantedRights: Integer;
  //RequestBrokerFunctionEnumerator: TRequestBrokerFunctionEnumerator;
  I: Integer;
  DispatchObjectPtr: PDispatchObject;
  ServerFunctionInformation: TJVCSServerFunctionInformation;
  RightsDomainStr: string;
begin
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
  {
  RequestBrokerFunctionEnumerator := TRequestBrokerFunctionEnumerator.Create;
  try
    FRequestBroker.EnumServerFunctions(RequestBrokerFunctionEnumerator.AddFunctions);
    if RequestBrokerFunctionEnumerator.ExistsServerObject('GET_LABELS_BY_MODULE') then
      FResponseBuffer.WriteFields(True, ['GET_LABELS_BY_MODULE']);
  finally
    RequestBrokerFunctionEnumerator.Free;
  end;
  }
  for I := 0 to Pred(FRequestBroker.DispatchList.Count) do
  begin
    DispatchObjectPtr := FRequestBroker.DispatchList[I];
    if Assigned(DispatchObjectPtr) and DispatchObjectPtr^.ServerObject.InheritsFrom(TJVCSServerObjectEx) then
    begin
      ServerFunctionInformation := TJVCSServerObjectExClass(DispatchObjectPtr^.ServerObject).GetInformation;
      if ServerFunctionInformation.Publishable then
      begin
        case ServerFunctionInformation.RequiredRightsDomain of
          rrdArchive: RightsDomainStr := 'a';
          rrdProject: RightsDomainStr := 'p';
          else
            RightsDomainStr := 'u';
        end;
        FResponseBuffer.WriteFields(True, [DispatchObjectPtr^.FunctionCode, ServerFunctionInformation.Version,
          RightsDomainStr, ServerFunctionInformation.RequiredRights]);
      end;
    end;
  end;
  FResultStatus := 200; // OK
end;

{==============================================================================
  Get the latest revision for a single file (by module ID)
  (is similar to GET_REVISION_LIST_BY_ID, but without without Project relation
   and delivers only the latest revision)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer

  Response: 0: [0]Module ID              - Integer
                  (Server should return 0 if no module matches the query)
               [1]Module name            - String(255)
               [2]Path                   - String(255)
               [3]Checked Out            - Boolean
               [4]TimeStamp              - Double (TDateTime)
               [5]UserID                 - Integer
               [6]Revision ID            - Integer
               [7]Version                - Integer
               [8]Revision               - Integer
               [9]Owner                  - String(50)
                  (Server may return a blank if not Checked Out)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
class procedure TServerObjectGET_LATESTREVISION_BY_ID.DoGetInformation(var AInformation: TJVCSServerFunctionInformation);
begin
  AInformation.Publishable := True;
  AInformation.RequiredRights := LevelRO;
  AInformation.RequiredRightsDomain := rrdProject;
end;

procedure TServerObjectGET_LATESTREVISION_BY_ID.ExecuteEx;
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
  CheckedOut: Boolean;
  DT: Double;
begin
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
      SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
      SQL.Add('brmodule.readonly, modules.tstamp, brmodule.userid,');
      SQL.Add('revision.revisionid, revision.version, revision.revision,');
      SQL.Add('users.login');
      SQL.Add('FROM modules');
      SQL.Add('INNER JOIN brmodule    ON (brmodule.branchid = :branchid) and (brmodule.moduleid = modules.moduleid)');
      {$IFDEF USELATESTREVISION}
      if UData.UseLatestRevisions then
      begin
        SQL.Add('INNER JOIN latestrevision    ON (latestrevision.branchid = :branchid) and (latestrevision.moduleid = modules.moduleid)');
        SQL.Add('INNER JOIN revision    ON revision.revisionid = latestrevision.revisionid');
      end
      else
      begin
      {$ENDIF USELATESTREVISION}
        SQL.Add('INNER JOIN revision    ON revision.moduleid = modules.moduleid');
        SQL.Add('INNER JOIN rvbranch    ON (rvbranch.branchid = :branchid) and (rvbranch.revisionid = revision.revisionid)');
      {$IFDEF USELATESTREVISION}
      end;
      {$ENDIF USELATESTREVISION}
      SQL.Add('LEFT OUTER JOIN users  ON brmodule.userid = users.userid');
      SQL.Add('WHERE modules.moduleid = :moduleid');
      SQL.Add('ORDER BY revision.version, revision.revision');
      ParamByName('moduleid').AsInteger := ModuleID;
      ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
      Open;
      while not Eof do
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
        {$IFDEF USELATESTREVISION}
        if UData.UseLatestRevisions then
          Break
        else
        {$ENDIF USELATESTREVISION}
        Next;
        if not Eof then
          FResponseBuffer.Rewrite;
      end; // if not EoF do begin
      Close;
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
  FResultStatus := 200; // OK
end;

end.
