(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSSrvOBJProperties.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
Dec/04
- USc: - only tested with MySQL and partly with Oracle, Firebird
ToDo
- enter functions into client code generator (2008/11/07: almost done)
-----------------------------------------------------------------------------

Unit history:

2009/01/17  USchuster - new unit (functions moved from JVCSSrvOBJNewObjects.pas)
2009/04/10  USchuster - changes for branching
2009/04/11  USchuster - implemented TJVCSServerObjectEx.DoGetInformation for GET_PROPERTY_LIST

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
2013/02/17  USchuster - Fixed scope issue with TableName

-----------------------------------------------------------------------------*)

unit JVCSSrvOBJProperties;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Classes, RBroker, SrvCustomSrvOBJ, SrvUDRec, SrvDBDef, SrvAccessDef,
  RFormat, DB;

procedure AddPropertyServerObjects(ARequestBroker: TRequestBroker);

implementation

{$IFDEF BRANCHOBJECTS}
uses
  JVCSSessionList;
{$ENDIF BRANCHOBJECTS}

type
  TServerObjectADD_UPDATE_PROPERTY = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectGET_PROPERTY_LIST = class(TJVCSServerObjectEx)
  protected
    class procedure DoGetInformation(var AInformation: TJVCSServerFunctionInformation); override;
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectREMOVE_PROPERTY = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectADD_UPDATE_PROPERTIES = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectGET_PROPERTIES = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectREMOVE_PROPERTIES = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

  TServerObjectGET_MODULE_REVISION_PROPERTIES = class(TJVCSServerObjectEx)
  public
    procedure ExecuteEx; override;
  end;

procedure AddPropertyServerObjects(ARequestBroker: TRequestBroker);
begin
  with ARequestBroker do
  begin
    AddServerObject(TServerObjectADD_UPDATE_PROPERTY);
    AddServerObject(TServerObjectGET_PROPERTY_LIST);
    AddServerObject(TServerObjectREMOVE_PROPERTY);
    AddServerObject(TServerObjectADD_UPDATE_PROPERTIES);
    AddServerObject(TServerObjectGET_PROPERTIES);
    AddServerObject(TServerObjectREMOVE_PROPERTIES);
    AddServerObject(TServerObjectGET_MODULE_REVISION_PROPERTIES);
  end;
end;

{==============================================================================
  Add a new property / change settings for an existing property

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Property ID            - Integer
                  (0 = add a new property,
                   x = change property ID x)
               [1]Property name          - String(100)
               [2]Property description   - String

  Response: 0: [0]Request accepted?      - Boolean
               [1]Error message          - String
                  (if not accepted)
               [2]new Property ID        - Integer
                  (if this is a new property, otherwise return old ID)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_PROPERTY.ExecuteEx;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID,
  PropertyID,
  NewPropertyID: Integer;
  PropertyName,
  PropertyDescr: string;
  IsNewProperty,
  IsNewName: Boolean;
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

  PropertyID := StrToInt(FRequestBuffer.Fields[0]);
  PropertyName := FRequestBuffer.Fields[1];
  PropertyDescr := FRequestBuffer.Fields[2];

  FResponseBuffer.Rewrite;
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      // add new or change existing property?
      SQL.Clear;
      SQL.Add('SELECT propertyid FROM property');
      SQL.Add('WHERE propertyid = :propertyid');
      ParamByName('propertyid').AsInteger := PropertyID;
      Open;
      IsNewProperty := Eof;
      Close;
    end; // with FQuery do begin
    if IsNewProperty then
    begin
      // check if this name is already in use
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT propertyid FROM property');
        SQL.Add('WHERE name = :name');
        ParamByName('name').AsString := PropertyName;
        Open;
        IsNewName := Eof;
        Close;
      end; // with FQuery do begin
      if IsNewName then
      begin
        // ok, add the new property
        with FQuery do
        begin
          Close;
          SQL.Clear;
          SQL.Add('INSERT INTO property');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('property')]));
          SQL.Add(':name, :description)');
          ParamByName('name').AsString := PropertyName;
          ParamByName('description').AsString := PropertyDescr;
          ExecSQL;
          // get the new property ID
          SQL.Clear;
          SQL.Add('SELECT propertyid FROM property');
          SQL.Add('WHERE name = :name');
          ParamByName('name').AsString := PropertyName;
          Open;
          NewPropertyID := FieldByName('propertyid').AsInteger;
          Close;
          // return true
          FResponseBuffer.WriteFields(True, [True]);
          // return blank error message
          FResponseBuffer.WriteFields(False, [True]);
          // return the new property ID
          FResponseBuffer.WriteFields(False, [NewPropertyID]);
        end; // with FQuery do begin
      end // if IsNewName then begin
      else
      begin
        // return false
        FResponseBuffer.WriteFields(True, [False]);
        // return error message
        FResponseBuffer.WriteFields(False, ['<' + PropertyName +
                                           '> This name is already in use.']);
        // return zero
        FResponseBuffer.WriteFields(False, [0]);
      end;
    end // if IsNewProperty then begin
    else
    begin
      // change existing property
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('UPDATE property');
        SQL.Add('SET name = :name, description = :description');
        SQL.Add('WHERE propertyid = :propertyid');
        ParamByName('name').AsString := PropertyName;
        ParamByName('description').AsString := PropertyDescr;
        ParamByName('propertyid').AsInteger := PropertyID;
        ExecSQL;
      end; // with FQuery do begin
      // return false
      FResponseBuffer.WriteFields(True, [True]);
      // return blank error message
      FResponseBuffer.WriteFields(False, [True]);
      // return the old property ID
      FResponseBuffer.WriteFields(False, [PropertyID]);
    end; // else if IsNewProperty then begin
  finally
    FQuery.Free;
  end;
  FResultStatus := 200;
end;

class procedure TServerObjectGET_PROPERTY_LIST.DoGetInformation(var AInformation: TJVCSServerFunctionInformation);
begin
  inherited DoGetInformation(AInformation);
  AInformation.Publishable := True;
  AInformation.RequiredRights := LevelRO;
  AInformation.RequiredRightsDomain := rrdArchive;
end;

{==============================================================================
  Get a list from all properties

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]incl. description      - Boolean

  Response: 0: [0]Property ID            - Integer
               [1]Property name          - String(100)
               [2]Property description   - String(2000)

            1: [next property...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROPERTY_LIST.ExecuteEx;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  Fld,
  AccessID: Integer;
  InclDescr: Boolean;
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
        SQL.Add('SELECT propertyid, name, description')
      else
        SQL.Add('SELECT propertyid, name');
      SQL.Add('FROM property');
      SQL.Add('ORDER BY property.name');
      Open;
      while not Eof do
      begin
        // Copy all fields from the record to the response
        for Fld := 0 to FQuery.FieldCount - 1 do
          FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString]);
        // return a blank instead of the description
        if not InclDescr then FResponseBuffer.WriteFields(False, ['']);
        Next;
      end;
      Close;
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
  FResultStatus := 200;
end;

function GetPropertyTableName(ATableTypID: Integer): string; forward;

{==============================================================================
  Remove a property

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Property ID            - Integer

  Response: 0: [0]property removed?      - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_PROPERTY.ExecuteEx;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID,
  PropertyID: Integer;
  I: Integer;
  S: string;
  CanRemove: Boolean;
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

  PropertyID := StrToInt(FRequestBuffer.Fields[0]);

  FResponseBuffer.Rewrite;
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      CanRemove := True;
      // check if the property is in use
      S := '';
      for I := 1 to 4 do
      begin
        S := GetPropertyTableName(I);
        Close;
        SQL.Clear;
        SQL.Add(Format('SELECT * FROM %s', [S]));
        SQL.Add('WHERE propertyid = :propertyid');
        ParamByName('propertyid').AsInteger := PropertyID;
        Open;
        if not Eof then
        begin
          CanRemove := False;
          Break;
        end;
        Close;
      end;
      if CanRemove then
      begin
        // remove property
        SQL.Clear;
        SQL.Add('DELETE FROM property');
        SQL.Add('WHERE propertyid = :propertyid');
        ParamByName('propertyid').AsInteger := PropertyID;
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
  FResultStatus := 200;
end;

function GetPropertyTableName(ATableTypID: Integer): string;
begin
  case ATableTypID of
    1: Result := 'projectproperty';
    2: Result := 'moduleproperty';
    3: Result := 'revisionproperty';
    4: Result := 'userproperty';
  end;
end;

function GetPropertyTableIDName(ATableTypID: Integer): string;
begin
  case ATableTypID of
    1: Result := 'projectid';
    2: Result := 'moduleid';
    3: Result := 'revisionid';
    4: Result := 'userid';
  end;
end;

function GetPropertyTablePropertyIDName(ATableTypID: Integer): string;
begin
  case ATableTypID of
    1: Result := 'projectpropertyid';
    2: Result := 'modulepropertyid';
    3: Result := 'revisionpropertyid';
    4: Result := 'userpropertyid';
  end;
end;

procedure ContentBlobToMWBuffer(AQuery: TSrvQuery; ABuffer: TMWBuffer);
var
  tempStream: TStream;
  nAutoExpand: Integer;
begin
  nAutoExpand := ABuffer.AutoExpand;
  try
    tempStream := AQuery.CreateBlobStream(AQuery.FieldByName('content'), bmRead);
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

{==============================================================================
  Add a new property / change settings for an existing property

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Table                  - Integer
                  (0 = project)
               [1]Table ID               - Integer
               [2]Table Property ID      - Integer
                  (0 = add a new property,
                   x = change property ID x)
               [3]Property ID            - Integer
               [4]Property header        - String
               [5]Property content       - Stream

  Response: 0: [0]Request accepted?      - Boolean
               [1]Error message          - String
                  (if not accepted)
               [2]new Table Property ID  - Integer
                  (if this is a new property, otherwise return old ID)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_UPDATE_PROPERTIES.ExecuteEx;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID,
  TableTypeID,
  TableID,
  TablePropertyID,
  PropertyID,
  NewTablePropertyID: Integer;
  PropertyHeader,
  LTableName,
  TableIDName,
  TablePropertyIDName: string;
  IsNewProperty, IsExistingProperty: Boolean;
  MS: TMemoryStream;
  FldType: TMWFieldType;  
  BlobStreamSize: Integer; //for MySQL max_allowed_packet check
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

  TableTypeID := StrToInt(FRequestBuffer.Fields[0]);
  TableID := StrToInt(FRequestBuffer.Fields[1]);
  TablePropertyID := StrToInt(FRequestBuffer.Fields[2]);
  PropertyID := StrToInt(FRequestBuffer.Fields[3]);
  PropertyHeader := FRequestBuffer.Fields[4];

  LTableName := GetPropertyTableName(TableTypeID);
  TableIDName := GetPropertyTableIDName(TableTypeID);
  TablePropertyIDName := GetPropertyTablePropertyIDName(TableTypeID);

  FResponseBuffer.Rewrite;
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      // does the property exist?
      SQL.Clear;
      SQL.Add('SELECT * FROM property');
      SQL.Add('WHERE propertyid = :propertyid');
      ParamByName('propertyid').AsInteger := PropertyID;
      Open;
      IsExistingProperty := not Eof;
      Close;
    end; // with FQuery do begin
    if IsExistingProperty then
    begin
      with FQuery do
      begin
        // add new or change existing property?
        SQL.Clear;
        SQL.Add(Format('SELECT %s FROM %s', [TablePropertyIDName, LTableName]));
        SQL.Add(Format('WHERE %s = :tablepropertyid', [TablePropertyIDName]));
        ParamByName('tablepropertyid').AsInteger := TablePropertyID;
        Open;
        IsNewProperty := Eof;
        Close;
      end; // with FQuery do begin
      if IsNewProperty then
      begin
        // ok, add the new property
        with FQuery do
        begin
          Close;
          SQL.Clear;
          SQL.Add(Format('INSERT INTO %s', [LTableName]));
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr(LTableName)]));
          {$IFDEF BRANCHOBJECTS}
          SQL.Add(':branchid, :tableid, :propertyid, :header, :contentsize, :content)');
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);          
          {$ELSE}
          SQL.Add(':tableid, :propertyid, :header, :contentsize, :content)');
          {$ENDIF BRANCHOBJECTS}
          ParamByName('tableid').AsInteger := TableID;
          ParamByName('propertyid').AsInteger := PropertyID;
          ParamByName('header').AsString := PropertyHeader;
          MS := TMemoryStream.Create;
          try
            FRequestBuffer.GetStreamField(5, MS, FldType);
            BlobStreamSize := MS.Size;
            ParamByName('contentsize').AsInteger := BlobStreamSize;
            ParamByName('content').LoadFromStream(MS, ftBlob);
          finally
            MS.Free;
          end;
          ExecSQL;
          // get the new property ID
          SQL.Clear;
          SQL.Add(Format('SELECT MAX(%s) FROM %s', [TablePropertyIDName, LTableName]));
          Open;
          NewTablePropertyID := Fields[0].AsInteger;
          Close;
          // return true
          FResponseBuffer.WriteFields(True, [True]);
          // return blank error message
          FResponseBuffer.WriteFields(False, ['']);
          // return the new property ID
          FResponseBuffer.WriteFields(False, [NewTablePropertyID]);
        end; // with FQuery do begin
      end // if IsNewProperty then begin
      else
      begin
        // change existing property
        with FQuery do
        begin
          Close;
          SQL.Clear;
          SQL.Add(Format('UPDATE %s', [LTableName]));
          SQL.Add(Format('SET %s = :tableid, propertyid = :propertyid, header = :header, contentsize = :contentsize, content = :content', [TableIDName]));
          SQL.Add(Format('WHERE %s = :tablepropertyid', [TablePropertyIDName]));
          ParamByName('tableid').AsInteger := TableID;
          ParamByName('propertyid').AsInteger := PropertyID;
          ParamByName('header').AsString := PropertyHeader;          
          MS := TMemoryStream.Create;
          try
            FRequestBuffer.GetStreamField(5, MS, FldType);
            BlobStreamSize := MS.Size;
            ParamByName('contentsize').AsInteger := BlobStreamSize;
            ParamByName('content').LoadFromStream(MS, ftBlob);
          finally
            MS.Free;
          end;
          ParamByName('tablepropertyid').AsInteger := TablePropertyID;
          ExecSQL;
        end; // with FQuery do begin
        // return true
        FResponseBuffer.WriteFields(True, [True]);
        // return blank error message
        FResponseBuffer.WriteFields(False, ['']);
        // return the old property ID
        FResponseBuffer.WriteFields(False, [TablePropertyID]);
      end; // else if IsNewProperty then begin
    end
    else
    begin
      // return false
      FResponseBuffer.WriteFields(True, [False]);
      // return error message
      FResponseBuffer.WriteFields(False, [Format('The property %d does not exist.', [PropertyID])]);
      // return zero as property ID
      FResponseBuffer.WriteFields(False, [0]);
    end;
  finally
    FQuery.Free;
  end;
  FResultStatus := 200;
end;

{==============================================================================
  Get a list from all properties

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Table                  - Integer
                  (0 = project)
               [1]Table ID               - Integer
                  (0 = all)
               [2]Table Property ID      - Integer
                  (0 = all)
               [3]Property ID            - Integer
                  (0 = all)
               [4]incl. content          - Boolean

  Response: 0: [0]Table ID               - Integer
               [1]Property ID            - Integer
               [2]Table Property ID      - Integer
               [3]Property header        - String
               [4]Property content size  - Integer
               [5]Property content       - Stream

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROPERTIES.ExecuteEx;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  Fld,
  AccessID: Integer;
  TableTypeID,
  TableID,
  TablePropertyID,
  PropertyID: Integer;
  LTableName,
  TableIDName,
  TablePropertyIDName: string;
  InclContent: Boolean;
  WhereStr: string;
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

  TableTypeID := StrToInt(FRequestBuffer.Fields[0]);
  TableID := StrToInt(FRequestBuffer.Fields[1]);
  TablePropertyID := StrToInt(FRequestBuffer.Fields[2]);
  PropertyID := StrToInt(FRequestBuffer.Fields[3]);
  InclContent := (FRequestBuffer.Fields[4] = '1');

  LTableName := GetPropertyTableName(TableTypeID);
  TableIDName := GetPropertyTableIDName(TableTypeID);
  TablePropertyIDName := GetPropertyTablePropertyIDName(TableTypeID);

  FResponseBuffer.Rewrite;
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      Close;
      SQL.Clear;
      if InclContent then
        SQL.Add(Format('SELECT %s, propertyid, %s, header, contentsize, content', [TableIDName, TablePropertyIDName]))
      else
        SQL.Add(Format('SELECT %s, propertyid, %s, header, contentsize', [TableIDName, TablePropertyIDName]));
      SQL.Add(Format('FROM %s', [LTableName]));
      WhereStr := '';
      if TableID <> 0 then
        WhereStr := Format('(%s = %d)', [TableIDName, TableID]);
      if TablePropertyID <> 0 then
      begin
        if WhereStr <> '' then
          WhereStr := WhereStr + ' AND ';
        WhereStr := WhereStr + Format('(%s = %d)', [TablePropertyIDName, TablePropertyID]);
      end;
      if PropertyID <> 0 then
      begin
        if WhereStr <> '' then
          WhereStr := WhereStr + ' AND ';
        WhereStr := WhereStr + Format('(propertyid = %d)', [PropertyID]);
      end;
      {$IFDEF BRANCHOBJECTS}
      if WhereStr <> '' then
        WhereStr := WhereStr + ' AND ';
      WhereStr := WhereStr + Format('(branchid = %d)', [SessionList.GetSessionBranchID(AccessID, TANr)]);
      {$ENDIF BRANCHOBJECTS}
      if WhereStr <> '' then
      begin
        SQL.Add('WHERE');
        SQL.Add(WhereStr);
      end;
      Open;
      while not Eof do
      begin
        // Copy all fields from the record to the response
        for Fld := 0 to FQuery.FieldCount - 1 do
        begin
          if Fld < 5 then
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString])
          else
            ContentBlobToMWBuffer(FQuery, FResponseBuffer);
        end;
        // return a blank instead of the description
        if not InclContent then FResponseBuffer.WriteFields(False, ['']);
        Next;
      end;
      Close;
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
  FResultStatus := 200;
end;

{==============================================================================
  Remove a property

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Table                  - Integer
                  (0 = project)
               [1]Table Property ID      - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_PROPERTIES.ExecuteEx;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  AccessID,
  TableTypeID,
  TablePropertyID: Integer;
  LTableName,
  TablePropertyIDName: string;
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

  TableTypeID := StrToInt(FRequestBuffer.Fields[0]);
  TablePropertyID := StrToInt(FRequestBuffer.Fields[1]);

  LTableName := GetPropertyTableName(TableTypeID);
  TablePropertyIDName := GetPropertyTablePropertyIDName(TableTypeID);

  FResponseBuffer.Rewrite;
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      // remove property
      SQL.Clear;
      SQL.Add(Format('DELETE FROM %s', [LTableName]));
      SQL.Add(Format('WHERE %s = :tablepropertyid', [TablePropertyIDName]));
      ParamByName('tablepropertyid').AsInteger := TablePropertyID;
      ExecSQL;
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
  FResultStatus := 200;
end;

{==============================================================================
  Get a list from all properties

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer
               [1]Property ID            - Integer
                  (0 = all)
               [2]incl. content          - Boolean

  Response: 0: [0]Revision ID            - Integer
               [1]Property ID            - Integer
               [2]Table Property ID      - Integer
               [3]Property header        - String
               [4]Property content size  - Integer
               [5]Property content       - Stream

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_MODULE_REVISION_PROPERTIES.ExecuteEx;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  Fld,
  AccessID: Integer;
  ModuleID,
  PropertyID: Integer;
  InclContent: Boolean;
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

  ModuleID := StrToInt(FRequestBuffer.Fields[0]);
  PropertyID := StrToInt(FRequestBuffer.Fields[1]);
  InclContent := (FRequestBuffer.Fields[2] = '1');

  FResponseBuffer.Rewrite;
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := UData.DBPath;
    with FQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT revisionproperty.revisionid, revisionproperty.propertyid,');
      SQL.Add('revisionproperty.revisionpropertyid, revisionproperty.header, revisionproperty.contentsize');
      if InclContent then
        SQL.Add(', revisionproperty.content');
      SQL.Add('FROM revisionproperty, revision');
      SQL.Add('WHERE revision.moduleid = :MODULEID');
      SQL.Add('AND revisionproperty.revisionid = revision.revisionid');
      if PropertyID <> 0 then
        SQL.Add('AND revisionproperty.propertyid = :PROPERTYID');
      {$IFDEF BRANCHOBJECTS}
      SQL.Add('AND revisionproperty.branchid = :branchid');
      ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
      {$ENDIF BRANCHOBJECTS}
      ParamByName('MODULEID').AsInteger := ModuleID;
      if PropertyID <> 0 then
        ParamByName('PROPERTYID').AsInteger := PropertyID;
      Open;
      while not Eof do
      begin
        // Copy all fields from the record to the response
        for Fld := 0 to FQuery.FieldCount - 1 do
        begin
          if Fld < 5 then
            FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString])
          else
            ContentBlobToMWBuffer(FQuery, FResponseBuffer);
        end;
        // return a blank instead of the description
        if not InclContent then FResponseBuffer.WriteFields(False, ['']);
        Next;
      end;
      Close;
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
  FResultStatus := 200;
end;

end.
