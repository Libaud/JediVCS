{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvCustomSrvOBJ.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
ToDo
- we need a GetTickCount equivalent function for Linux
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

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster- style cleaning
                     - changes for Firebird port over JVCL UIB components 
                       by Pierre Y. (partly adapted)
2005/05/06  USchuster- added jedi.inc
2005/05/08  USchuster- removed unused units
                     - made some changes for Linux
2006/05/01  USchuster- changes for new session handling(solves mantis #1941; depends on BRANCHOBJECTS)
2008/01/23  USchuster- added TFVCSServerObject.BranchVCSLog (move from TJVCSBranchServerObject.BranchVCSLog
                       in JVCSSrvOBJBranches.pas) to be able to write old log entries with branchid
                       (for exam. ADD_NEW_PROJECT crashed)
2008/04/13  USchuster- added IBPORT defines in order to make BranchVCSLog work properly
2009/01/17  USchuster- added TJVCSServerObjectEx from JVCSSrvOBJNewObjects.pas
2009/04/11  USchuster- added TJVCSServerObjectEx.GetInformation as source for the server function GET_SUPPORTED_FUNCTIONS

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SrvCustomSrvOBJ - Custom ServerObject for JVCS application server.
                    This is the ancestor for all JVCS ServerObjects.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvCustomSrvOBJ;

// Added by F. Libaud
{$ifdef FPC}
  {$MOD DELPHI}
{$endif}

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFNDEF MSWINDOWS}
  JclDateTime,
  {$ENDIF ~MSWINDOWS}
  {$IFDEF ORACLESRV}
  DB,
  {$ENDIF ORACLESRV}
  SysUtils,
  ApSrvCli,
  {$ifndef LCL}
  RBroker,
  {$endif}
  SrvDBDef, SrvAccessDef;

type
  TFVCSServerObject = class(TServerObject)
  {$IFDEF DEBUG}
  private
    Ticks: DWORD;
  {$ENDIF DEBUG}
  public
    {$IFDEF DEBUG}
    // Debug help
    procedure InitDebugMessage;
    procedure ExitDebugMessage;
    procedure Finish; {override;}         { TODO : A revoir }
    procedure QueryDisplay(Sender: TObject; Msg: string);
    {$ENDIF DEBUG}
    // Access control
    function GetArchiveRelatedRight(const Archive: TJVCSArchive;
      const AccessID, TANr, Requested: Integer; const LogFault: Boolean): Integer;
    function GetProjectRelatedRight(const Archive: TJVCSArchive;
      const AccessID, TANr, Requested: Integer; const LogFault: Boolean): Integer;
    // Language independent StrToFloat
    function _StrToFloat(Value: string): Double;
    // Convert server's local timestamp to GMT
    function LocalDT2GMTDT(const LocalDT: Double): Double;

    procedure VCSLog(const Archive: TJVCSArchive;
      const ProjectID, UserID, ModuleID, RevisionID: Integer;
      const LogType: Char; const Description: string);
    {$IFDEF BRANCHOBJECTS}
    procedure BranchVCSLog(const Archive: TJVCSArchive;
      const ABranchID, ProjectID, UserID, ModuleID, RevisionID: Integer;
      const LogType: Char; const Description: string);
    {$ENDIF BRANCHOBJECTS}
  end; // TFVCSServerObject

  TRequiredRightsDomain = (rrdUnknown, rrdArchive, rrdProject);

  TJVCSServerFunctionInformation = record
    Publishable: Boolean;
    RequiredRights: Integer;
    RequiredRightsDomain: TRequiredRightsDomain;
    Version: string;
  end;

  TJVCSServerObjectEx = class(TFVCSServerObject)
  protected
    procedure ExecuteEx; virtual;
    class procedure DoGetInformation(var AInformation: TJVCSServerFunctionInformation); virtual;
  public
    procedure Execute; { TODO : a revoir override; }
    class function GetInformation: TJVCSServerFunctionInformation;
  end;

  TJVCSServerObjectExClass = class of TJVCSServerObjectEx;

implementation

{$IFDEF BRANCHOBJECTS}
uses
  JVCSSessionList;
{$ENDIF BRANCHOBJECTS}

{$IFDEF DEBUG}
{$IFNDEF MSWINDOWS}
function GetTickCount: DWord;
begin
  Result := 0;
end;
{$ENDIF ~MSWINDOWS}
{$ENDIF DEBUG}

{==============================================================================
 Debug help - show client's IP Addr, ServerObject's FunctionCode,
              Request- / Response buffer size and a (improved) execution time
              counter.

 ServerObject Init
 ==============================================================================}
{$IFDEF DEBUG}
procedure TFVCSServerObject.InitDebugMessage;
{$IFDEF DETAIL_DEBUG}
var
  I: Integer;
{$ENDIF DETAIL_DEBUG}
begin
  {$IFDEF DETAIL_DEBUG}
  TriggerDisplay('>> ========================================================');
  TriggerDisplay('>> START OF ' + FFunctionCode);
  FRequestBuffer.First;
  I := 0;
  while not FRequestBuffer.Eof do
  begin
    TriggerDisplay('>> Request ' + IntToStr(I) + ': ' +
                                                 FRequestBuffer.RecordToString);
    FRequestBuffer.Next;
    Inc(I);
  end;
  FRequestBuffer.First;
  {$ENDIF DETAIL_DEBUG}
  // improved perfomance check
  Ticks := GetTickCount;
  TriggerDisplay('>> ' + TClientWSocket(FORBDataPtr.Tag).PeerAddr +  ' ' +
                                                                 FFunctionCode);
  TriggerDisplay('>> Request size: ' +
                                      IntToStr(FRequestBuffer.DataBufferCount));
end;

//=== ServerObject Exit ========================================================

procedure TFVCSServerObject.ExitDebugMessage;
{$IFDEF DETAIL_DEBUG}
var
  I: Integer;
{$ENDIF DETAIL_DEBUG}
begin
  // improved perfomance check
  Ticks := GetTickCount - Ticks;
  TriggerDisplay('>> Ticks: ' + IntToStr(Ticks));
  TriggerDisplay('>> Answer size: ' +
                                     IntToStr(FResponseBuffer.DataBufferCount));
  {$IFDEF DETAIL_DEBUG}
  FResponseBuffer.First;
  I := 0;
  while not FResponseBuffer.Eof do
  begin
    TriggerDisplay('>> Response ' + IntToStr(I) + ': ' +
                                                FResponseBuffer.RecordToString);
    FResponseBuffer.Next;
    Inc(I);
  end;
  FResponseBuffer.First;
  {$ENDIF DETAIL_DEBUG}
end;

//=== ServerObject Finish ======================================================

procedure TFVCSServerObject.Finish;
begin
  Assert((FResultStatus = 200) or
         (FResultStatus = 400) or
         (FResultStatus = 403), 'FResultStatus - Return value undefined.');
  TriggerDisplay('>> Result status: ' + IntToStr(FResultStatus));
  {$IFDEF DETAIL_DEBUG}
  TriggerDisplay('>> END OF ' + FFunctionCode);
  TriggerDisplay('>> ========================================================');
  {$ENDIF DETAIL_DEBUG}
  inherited Finish;
end;

//=== Show FQuery debug messages ===============================================

procedure TFVCSServerObject.QueryDisplay(Sender: TObject; Msg: string);
begin
  TriggerDisplay(Msg);
end;
{$ENDIF DEBUG}
{==============================================================================
 Access control is handled on ServerObject level, that means that each
 ServerObject decides independent from the application server if a request is
 allowed to be proceeded or not.
 Result: Access level 0 - 4 (as defined in SrvAccessDef.pas)

 Global or not restricted function -> get access rights from Users.
 ==============================================================================}
function TFVCSServerObject.GetArchiveRelatedRight(const Archive: TJVCSArchive;
  const AccessID, TANr, Requested: Integer; const LogFault: Boolean): Integer;
var
  FQuery: TSrvQuery;
  {$IFDEF BRANCHOBJECTS}
  Session: TJVCSSession;
  {$ENDIF BRANCHOBJECTS}
begin
  {$IFNDEF ADOSRV} //--PL
    {$IFNDEF MSSQLSRV}
          {$IFNDEF MYSQLSRV} //-- LB
              {$IFNDEF UIBSRV}
              Assert((Archive <> ''), 'FUserData.DBPath not initialized.');
              {$ELSE}
              Assert((Archive <> nil), 'FUserData.DBPath not initialized.');
              {$ENDIF ~UIBSRV}
          {$ELSE}
          Assert((Archive <> nil), 'FUserData.DBPath not initialized.');
          {$ENDIF ~MYSQLSRV}
    {$ENDIF ~MSSQLSRV}
  {$ELSE}
  Assert((Archive <> nil), 'FUserData.DBPath not initialized.');
  {$ENDIF ~ADOSRV}
  Result := -1; // no rights
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := Archive;
    with FQuery do
    begin
      Close;
      {$IFDEF BRANCHOBJECTS}
      Session := SessionList.FindSession(AccessID, TANr);
      if Assigned(Session) then
      begin
        SQL.Clear;
        SQL.Add('SELECT users.rights');
        SQL.Add('FROM users');
        SQL.Add('WHERE users.userid = :accessid');
        SQL.Add('AND users.deleted <> ''1''');
        ParamByName('accessid').AsInteger := AccessID;
        Open;
        if not Eof then Result := FQuery.FieldByName('rights').AsInteger;
        Close;
        if Result > -1 then
          Session.Expires := Now;
      end;
      {$ELSE}
      SQL.Clear;
      SQL.Add('SELECT users.rights');
      SQL.Add('FROM transact, users');
      SQL.Add('WHERE transact.accessid = :accessid');
      SQL.Add('AND transact.tanr = :tanr');
      SQL.Add('AND users.userid = transact.accessid');
      SQL.Add('AND users.deleted <> ''1''');
      ParamByName('accessid').AsInteger := AccessID;
      ParamByName('tanr').AsInteger := TANr;
      Open;
      if not Eof then Result := FQuery.FieldByName('rights').AsInteger;
      Close;
      // reset login idle timer
      if Result > -1 then
      begin
        SQL.Clear;
        SQL.Add('UPDATE transact');
        SQL.Add('SET expires = :expires');
        SQL.Add('WHERE accessid = :accessid');
        ParamByName('expires').AsDateTime := Now;
        ParamByName('accessid').AsInteger := AccessID;
        ExecSQL;
      end; // if Result > 0 then begin
      {$ENDIF BRANCHOBJECTS}
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
  {$IFDEF DEBUG}
  if Result > -1 then
    TriggerDisplay('>> Granted Right (Archive): ' + AccLevel[Result])
  else
    TriggerDisplay('>> Granted Right (Archive): Unknown user');
  {$ENDIF DEBUG}
  if LogFault and (Result < Requested) then
    TriggerDisplay(Format(AccessFault,
          [TClientWSocket(FORBDataPtr.Tag).PeerAddr, AccessID, FFunctionCode]));
end;

//=== Restricted function -> get access rights from Transact. ==================

function TFVCSServerObject.GetProjectRelatedRight(const Archive: TJVCSArchive;
  const AccessID, TANr, Requested: Integer; const LogFault: Boolean): Integer;
var
  {$IFDEF BRANCHOBJECTS}
  Session: TJVCSSession;
  {$ELSE}
  FQuery: TSrvQuery;
  {$ENDIF BRANCHOBJECTS}
begin
  {$IFNDEF ADOSRV} // PL
    {$IFNDEF MSSQLSRV}
          {$IFNDEF MYSQLSRV} //-- LB
              {$IFNDEF UIBSRV}
              Assert((Archive <> ''), 'FUserData.DBPath not initialized.');
              {$ELSE}
              Assert((Archive <> nil), 'FUserData.DBPath not initialized.');
              {$ENDIF ~UIBSRV}
          {$ELSE}
          Assert((Archive <> nil), 'FUserData.DBPath not initialized.');
          {$ENDIF ~MYSQLSRV}
    {$ENDIF ~MSSQLSRV}
  {$ELSE}
  Assert((Archive <> nil), 'FUserData.DBPath not initialized.');
  {$ENDIF ~ADOSRV}
  Result := -1; // no rights
  {$IFDEF BRANCHOBJECTS}
  Session := SessionList.FindSession(AccessID, TANr);
  if Assigned(Session) then
  begin
    Result := Session.Rights;
    if Result > -1 then
      Session.Expires := Now;
  end;
  {$ELSE}
  FQuery := TSrvQuery.Create(Self);
  try
    FQuery.DatabaseName := Archive;
    with FQuery do
    begin
      Close;
      SQL.Clear;
      SQL.Add('SELECT rights');
      SQL.Add('FROM transact');
      SQL.Add('WHERE accessid = :accessid');
      SQL.Add('AND tanr = :tanr');
      ParamByName('accessid').AsInteger := AccessID;
      ParamByName('tanr').AsInteger := TANr;
      Open;
      if not Eof then Result := FQuery.FieldByName('rights').AsInteger;
      Close;
      // reset login idle timer
      if Result > -1 then
      begin
        SQL.Clear;
        SQL.Add('UPDATE transact');
        SQL.Add('SET expires = :expires');
        SQL.Add('WHERE accessid = :accessid');
        ParamByName('expires').AsDateTime := Now;
        ParamByName('accessid').AsInteger := AccessID;
        ExecSQL;
      end; // if Result > 0 then begin
    end; // with FQuery do begin
  finally
    FQuery.Free;
  end;
  {$ENDIF BRANCHOBJECTS}  
  {$IFDEF DEBUG}
  if Result > -1 then
    TriggerDisplay('>> Granted Right (Project): ' + AccLevel[Result])
  else
    TriggerDisplay('>> Granted Right (Project): Unknown user');
  {$ENDIF DEBUG}
  if LogFault and (Result < Requested) then
    TriggerDisplay(Format(AccessFault,
          [TClientWSocket(FORBDataPtr.Tag).PeerAddr, AccessID, FFunctionCode]));
end;
{==============================================================================
 FreeVCS handles all date values as doubles. Since Midware can transmit data
 only in string or binary format, float values are also transmitted as string.
 This custom StrToFloat function fixes the problem if client and server
 working with different decimal separators (i.e. working on different OS
 language versions). The client uses a similar function to retrieve the dates.
 ==============================================================================}
function TFVCSServerObject._StrToFloat(Value: string): Double;
var
  I: Integer;
begin
  Result := 0;
  if Value = '' then Exit;
  for I := 1 to Length(Value) do
    if not (Value[I] in ['0'..'9', '+', '-', 'E']) then
    begin
      System.Delete(Value, I, 1);
      System.Insert(DecimalSeparator, Value, I);
      Break;
    end;
  Result := StrToFloat(Value);
end;
{==============================================================================
  To avoid problems with clients from different timezones or local
  summer time settings all dates/times are handeled in GMT format.
  This function converts the server's local timestamp to GMT.
  * from ESBDates (c) 1999 by ESB Consultancy *
 ==============================================================================}
{$IFDEF MSWINDOWS}
function TFVCSServerObject.LocalDT2GMTDT(const LocalDT: Double): Double;
var
  TZ: TTimeZoneInformation;
  TZBias: Longint;
begin
  case GetTimeZoneInformation (TZ) of
    TIME_ZONE_ID_STANDARD: TZBias := TZ.Bias + TZ.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: TZBias := TZ.Bias + TZ.DaylightBias;
  else
    TZBias := TZ.Bias;
  end;
  Result := LocalDT + TZBias / (24 * 60);
end;
{$ELSE}
function TFVCSServerObject.LocalDT2GMTDT(const LocalDT: Double): Double;
begin
  Result := LocalDateTimeToDateTime(LocalDT);
end;
{$ENDIF MSWINDOWS}

procedure TFVCSServerObject.VCSLog(const Archive: TJVCSArchive;
  const ProjectID, UserID, ModuleID, RevisionID: Integer; const LogType: Char;
  const Description: string);
{$IFNDEF BRANCHOBJECTS}
var
  FQuery: TSrvQuery;
  NewLogID: Integer;
{$ENDIF ~BRANCHOBJECTS}
begin
  {$IFDEF BRANCHOBJECTS}
  BranchVCSLog(Archive, 1, ProjectID, UserID, ModuleID, RevisionID, LogType, Description);
  {$ELSE}
  FQuery := TSrvQuery.Create(Self);
  with FQuery do
  begin
    DatabaseName := Archive;
    try
      SQL.Clear;
      SQL.Add('INSERT INTO vcslog');
      SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('vcslog')]));
      SQL.Add(':projectid, :userid, :moduleid, :tstamp,');
      SQL.Add(':type, :description)');

      // moduleid = null
      if ProjectID > 0 then
        ParamByName('projectid').AsInteger := ProjectID
      else
        ParamByName('projectid').Clear;
      if UserID > 0 then
        ParamByName('userid').AsInteger := UserID
      else
        ParamByName('userid').Clear;
      if ModuleID > 0 then
        ParamByName('moduleid').AsInteger := ModuleID
      else
        ParamByName('moduleid').Clear;

      ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
      ParamByName('type').AsString := LogType;
      ParamByName('description').AsString := Description;
      ExecSQL;

      if RevisionID > 0 then
      begin
        SQL.Clear;
        SQL.Add('SELECT MAX(LOGID) FROM VCSLOG');
        Open;
        NewLogID := Fields[0].AsInteger;
        Close;

        SQL.Clear;
        SQL.Add('INSERT INTO logcomm');
        SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('logcomm')]));
        SQL.Add(':logid, :revisionid)');
        ParamByName('logid').AsInteger := NewLogID;
        ParamByName('revisionid').AsInteger := RevisionID;
        ExecSQL;
      end;
    finally
      FQuery.Free;
    end;
  end; // with FQuery do begin
  {$ENDIF BRANCHOBJECTS}
end;

{$IFDEF BRANCHOBJECTS}
procedure TFVCSServerObject.BranchVCSLog(const Archive: TJVCSArchive;
  const ABranchID, ProjectID, UserID, ModuleID, RevisionID: Integer;
  const LogType: Char; const Description: string);
var
  FQuery: TSrvQuery;
  NewLogID: Integer;
  IDFieldNameAndComma, LogTypName: string;
begin
  FQuery := TSrvQuery.Create(Self);
  with FQuery do
  begin
    DatabaseName := Archive;
    try
      SQL.Clear;
      IDFieldNameAndComma := '';
      if GetInsertKeyFieldValueStr('vcslog') <> '' then
        IDFieldNameAndComma := 'logid, ';
      {$IFNDEF UIBSRV}
      LogTypName := 'type';
      {$ELSE}
      LogTypName := 'logtype';
      {$ENDIF ~UIBSRV}
      SQL.Add(Format('INSERT INTO vcslog(%sbranchid, projectid, userid, moduleid, tstamp, %s, description)',
        [IDFieldNameAndComma, LogTypName]));
      SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('vcslog')]));
      SQL.Add(':branchid, :projectid, :userid, :moduleid, :tstamp,');
      SQL.Add(':type, :description)');

      // moduleid = null
      if ABranchID > 0 then
        ParamByName('branchid').AsInteger := ABranchID
      else
      begin
        {$IFDEF ORACLESRV}
        ParamByName('branchid').DataType := ftInteger;
        {$ENDIF ORACLESRV}
        ParamByName('branchid').Clear;
      end;
      if ProjectID > 0 then
        ParamByName('projectid').AsInteger := ProjectID
      else
      begin
        {$IFDEF ORACLESRV}
        ParamByName('projectid').DataType := ftInteger;
        {$ENDIF ORACLESRV}
        ParamByName('projectid').Clear;
      end;
      if UserID > 0 then
        ParamByName('userid').AsInteger := UserID
      else
      begin
        {$IFDEF ORACLESRV}
        ParamByName('userid').DataType := ftInteger;
        {$ENDIF ORACLESRV}
        ParamByName('userid').Clear;
      end;
      if ModuleID > 0 then
        ParamByName('moduleid').AsInteger := ModuleID
      else
      begin
        {$IFDEF ORACLESRV}
        ParamByName('moduleid').DataType := ftInteger;
        {$ENDIF ORACLESRV}
        ParamByName('moduleid').Clear;
      end;

      ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
      ParamByName('type').AsString := LogType;
      ParamByName('description').AsString := Description;
      ExecSQL;

      if RevisionID > 0 then
      begin
        SQL.Clear;
        SQL.Add('SELECT MAX(LOGID) FROM VCSLOG');
        Open;
        NewLogID := Fields[0].AsInteger;
        Close;

        SQL.Clear;
        SQL.Add('INSERT INTO logcomm');
        SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('logcomm')]));
        SQL.Add(':logid, :revisionid)');
        ParamByName('logid').AsInteger := NewLogID;
        ParamByName('revisionid').AsInteger := RevisionID;
        ExecSQL;
      end;
    finally
      FQuery.Free;
    end;
  end; // with FQuery do begin
end;
{$ENDIF BRANCHOBJECTS}

class procedure TJVCSServerObjectEx.DoGetInformation(var AInformation: TJVCSServerFunctionInformation);
begin
//
end;

procedure TJVCSServerObjectEx.ExecuteEx;
begin
  raise Exception.Create('It is not allowed the create an instance of TJVCSServerObjectEx');
end;

procedure TJVCSServerObjectEx.Execute;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    ExecuteEx;
    {$IFDEF DEBUG}
    ExitDebugMessage;
    {$ENDIF DEBUG}
  except
    on E: Exception do
    begin
      FResultStatus := 400; // bad request
      FResponseBuffer.Rewrite;
    end;
  end;
  Finish;
end;

class function TJVCSServerObjectEx.GetInformation: TJVCSServerFunctionInformation;
begin
  Result.Publishable := False;
  Result.RequiredRights := -1;
  Result.RequiredRightsDomain := rrdUnknown;
  Result.Version := '';
  DoGetInformation(Result);
end;

end.

