{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvDBDef.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Purpose: Server Query/Table and database definition for JVCS application server.

-----------------------------------------------------------------------------

Unit history:

  Aug '99 by Thomas Hensle - http://www.freevcs.de
  Based on François PIETTE's SvrTest demo application.

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI VCS
2003/02/09  HDors    - Add MySQL Server Port by Ludo Brands
2003/03/01  THuber   - Added  F I B +  port
                     - fixed mantis #0000687 - message from Bojidar Alexandrov
                     - code cleanup, SQL_DEBUG now inside implementation
                     - removed TIBParameter for IBX (IB6) port
2003/03/02  THuber   - fixed wrong ExecSQL-definition
2003/03/09  THuber   - removed d b isam200 ifdef's
2003/04/04  THuber   - changed transaction handling for IB/FB due to bug in merge
2003/12/21  THuber   #1270 fixed, again changes in transaction handling for FB/IB
2004/01/03  THuber   - started work on automatic versioning upgrade process
                       of database archive.
2004/01/03  THuber   - simplified IFDEF's and removed TQuery as Default for TJVCSQuery
                       as well as TDatabase as default for TJVCSDatabase
                     - removed again dependency from SrvMain & SrvServiceUnit,
                       SrvDatabase has now DBPath property for linking with Qry
2004/01/06  THuber   - removed unused vcsprocbase from uses
2004/01/17  THuber   - MySQL Upgrade240 procedure
                     - new index IX_VCSLOG_TSTAMP renamed to I_VCSLOG_TSTAMP
                     - introducted checks if db changes were successfull
                       and better result handling in upgrade procedure
                     - introduced new functions db dependend
                       IndexExists methods in TSrvDatabase
                     #1289 server side: ffamily.parentext enlarged to 20chars
2004/06/27  USchuster- finished Oracle 2.41 upgrade
                     - changed check in MySQL 2.41 upgrade
                     - minor style cleaning

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster- minor style cleaning
                     - changes for Firebird port over JVCL UIB components
                       by Pierre Y. (partly adapted)
2004/12/18  USchuster- changes for Zeos DBO 6.x (is now default component version
                       for MySQL port)
                     - implemented TSrvDatabase.IndexExists for Zeos DBO 6.x
2004/12/19  THuber   - added upgrade 241 procedure for mssql port
2005/01/14  THuber   #2502 added mssql option for trusted nt connections
2005/03/14  USchuster- the creation of the index IX_VCSLOG_TSTAMP for MySQL is
                       now done in the MySQL version of TSrvDatabase.Upgrade240
                       to avoid problems with casesensitive OS(mantis #2699)
2005/04/15  CSchuette- fixed timeout problems with  F l a s h F i l e r  (mantis #2880)
2005/05/06  USchuster- changes for FPC
2005/05/08  USchuster- removed unused units
2006/07/22  USchuster- fixed ParamByName in UIB port
                     - fixed ParamByName in MSSQL port
2006/07/29  USchuster- fixed compilation of the MSSQL port with at least D9(was no prob with D5)
2006/10/11  USchuster- changes for Oracle performance tweak "UseRuleOptimizerMode" (mantis #3950)
2007/07/15  USchuster- fixed memory leak in UIB port
                     - added upgrade 242 procedure for MSSQL port (Mantis #3573 and #3978)
2007/10/06  USchuster- changes for (MySQL) connection timeout solution (Mantis #4260)
2008/09/20  USchuster- implemented AquireConnection for the  F I B P l u s  port (Mantis #4443)
2009/04/10  USchuster- added branch and property sequences/generators to SequenceNames constant
2009/07/11  USchuster- changes for latest UIB from SVN (JvUIB -> UIB)

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5066 support for  F l a s h F i l e r  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
                     - also removed outdated Z E O S 5 4 directive
2010/01/09  THuber   #5085 Added Firebird Indexes to Upgrade250
2010/01/09  THuber   #5099 changed Transaction handling to Autocommit in UIBSRV port to prevent deadlock situations
2010/01/22  THuber   #5099 Transaction hack for Firebird, Defaults to AutoCommit but with possiblitiy to run Queries in one transaction
                     Attention: Use SetTransactionNoAuto and SetTransaction only if you know what happens
2010/01/27  THuber   #5099 check added in SetTransaction
2012/02/19  AKroeber - fixed Oracle service shutdown (NCOCI break window caused AV on shutdown)

Known Issues:
- overwork of upgrade procedure (2.40 & 2.41 needs heavy testing.
  One can think of various DBMS related problems (permissions etc) occuring
  during upgrade process.

ToDo
- Use FieldExists with Datatype and Size check for every port
- incorporate trigger stuff by Pierre Y. for IB ports
- finish AquireConnection
  Done:
  - MySQL (Mantis #4260)
  - Firebird (Mantis #4443)
  - MSSQL (not necessary [tested])
  - UIB
  ToDo:
  - Oracle (still done in before process request)
- Rework transacation handling (for Firebird port), at the moment autocommit is used 
-----------------------------------------------------------------------------*)

unit SrvDBDef;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  Classes,
  {$IFDEF UIBSRV}
  DB,
  {$IFNDEF FPC}
  DbConsts,
  {$ELSE}
  dbconst,
  {$ENDIF ~FPC}
  UIB,
  UIBDataset,
  UIBLib;
  {$ENDIF UIBSRV}

  {$IFDEF ORACLESRV}
  //-- hdo
  DB,
  {NCOciDB;}
  {$ENDIF ORACLESRV}

  {$IFDEF MSSQLSRV}
  Contnrs,
  //-- MG
  DB,
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  ADOdb;
  {$ENDIF MSSQLSRV}

  {$IFDEF MYSQLSRV}
  //-- LB
  DB,
  ZConnection,
  // Modified by F. Libaud
  ZDataset;
  {$ENDIF MYSQLSRV}

  {$IFDEF ADOSRV}
  //-- PL
  DB,
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  ADOdb;
  {$ENDIF ADOSRV}

const
  {$IFDEF UIBSRV}
  DBExt = '.fdb';
  {$ENDIF UIBSRV}
  {$IFDEF ORACLESRV}
  //-- hdo
  DBExt = '.dbf';
  {$ENDIF ORACLESRV}
  {$IFDEF MSSQLSRV}
  //-- MG
  DBExt = '.dbf';
  {$ENDIF MSSQLSRV}
  {$IFDEF MYSQLSRV}
  //-- LB
  DBExt = '.dbf';
  {$ENDIF MYSQLSRV}
  {$IFDEF ADOSRV}
  //-- PL
  DBExt = ''; // no extension for generic port
  {$ENDIF ADOSRV}

type

// ------------------------- TJVCSDatabase -------------------------------------
// TJVCSDatabase is the base class for all Database components
// it implements general methodes common for all databases

  {$IFDEF UIBSRV}
  TJVCSDatabase = class(TUIBDatabase)
  {$ENDIF UIBSRV}
  {$IFDEF ORACLESRV}
  TJVCSDatabase = class(TOCIDatabase)
  {$ENDIF ORACLESRV}
  {$IFDEF MYSQLSRV}
  TJVCSDatabase = class(TZConnection)
  {$ENDIF MYSQLSRV}
  {$IFDEF MSSQLSRV}
  TJVCSDatabase = class(TADOConnection)
  {$ENDIF MSSQLSRV}
  {$IFDEF ADOSRV}
  TJVCSDatabase = class(TADOConnection)
  {$ENDIF ADOSRV}
  private
    FArchiveVersion: Integer;
  protected
    function GetArchiveVersion: Integer;
    function SetUpgradeDone(const nUpgradeVersion: Integer): Boolean;
    function CreateDBVersionTable: Boolean; virtual;
    function PrepareUpgrade: Boolean; virtual;
    function Upgrade240: Boolean; virtual;
    function Upgrade241: Boolean; virtual;
    function Upgrade242: Boolean; virtual;
    {$IFDEF BRANCHOBJECTS}
    function Upgrade250: Boolean; virtual;
    {$ENDIF BRANCHOBJECTS}
    function UnprepareUpgrade: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AcquireConnection; virtual;
    function UpgradeArchive: Boolean; virtual;
    function TableExists(const sTableName: string): Boolean; virtual;
    function IndexExists(const sTableName: string
                        ; const sIndexName: string
                        ; const sIndexedFields: string
                        ; const bUnique: Boolean = False
                        ; const bCheckIndexedFields: Boolean = False
                        ; const bCheckUnique: Boolean = False
                        ): Boolean; virtual; abstract;
    property ArchiveVersion: Integer read GetArchiveVersion;
  end;

type  
  {$IFNDEF ADOSRV} // PL
    {$IFNDEF MSSQLSRV}
          {$IFNDEF MYSQLSRV} //-- LB
              {$IFNDEF UIBSRV}
                TJVCSArchive = string;
              {$ELSE}
                TJVCSArchive = TUIBDatabase;
              {$ENDIF ~UIBSRV}
          {$ELSE}
            TJVCSArchive = TZConnection;
          {$ENDIF ~MYSQLSRV}
    {$ELSE}
      TJVCSArchive = TADOConnection;
    {$ENDIF ~MSSQLSRV}
   {$ELSE}
     TJVCSArchive = TADOConnection;
  {$ENDIF ~ADOSRV}

  //-- hdo
  TDisplayEvent = procedure(Sender: TObject; Msg: string) of object;

// ------------------------- TJCVSQuery ----------------------------------------
//  TJVCSQuery is the base class for all Queries.
//  Define {SQL_DEBUG} to show all SQL statements in server's display

  {$IFDEF UIBSRV}
  TJVCSQuery = class(TUIBDataset)
  {$ENDIF UIBSRV}
  {$IFDEF ORACLESRV}
  TJVCSQuery = class(TOCIQuery)
  {$ENDIF ORACLESRV}
  {$IFDEF MYSQLSRV} //-- LB
  TJVCSQuery = class(TZQuery)
  {$ENDIF MYSQLSRV}
  {$IFDEF MSSQLSRV} //-- MG
  TJVCSQuery = class(TADOQuery)
  {$ENDIF MSSQLSRV}
  {$IFDEF ADOSRV} //-- PL
  TJVCSQuery = class(TADOQuery)
  {$ENDIF ADOSRV}
  private
    FOnDisplay: TDisplayEvent;
    {$IFDEF UIBSRV}
    FParams: TParams;
    FHackDoSQLChange: TNotifyEvent;
    procedure ReMapParams;
    procedure SQLChanged(Sender: TObject);
    {$ENDIF UIBSRV}
    procedure TriggerDisplay(aMsg: string);
  public
    constructor Create(AOwner: TComponent); override;
    {$IFDEF UIBSRV}
    destructor Destroy; override;
    {$ENDIF UIBSRV}
    procedure Open; overload;
    procedure Close; overload;
    {$IFDEF MYSQLSRV}
    procedure ExecSQL; override;
    {$ELSE}
    procedure ExecSQL; overload;
    {$ENDIF MYSQLSRV}
    procedure SetTransaction(const aQry : TJVCSQuery);overload;
    procedure SetTransactionNoAuto;overload;
    property OnDisplay: TDisplayEvent read FOnDisplay write FOnDisplay;
  end;


  (*
    The server uses a custom database/ query/ table type (TSrvDatabase/
    TSrvQuery/ TSrvTable) to prevent the need for a lot of {$IFDEF..}
    statements in the source code.
    This is the place where you should make derivations from your type of
    database/ query/ table.
    In the server objects you should not delete or replace code, instead,
    uncomment it.
  *)

type
  // ==============================
  // Firebird over UIB
  // ------------------------------
  {$IFDEF UIBSRV}
  TSrvDatabase = class(TJVCSDatabase)
  private
    function GetDBPath: TUIBDatabase;
  public
    constructor Create(AOwner: TComponent); override;
    function TableExists(const sTableName: string): Boolean; override;
    function FieldExists(const sTableName, sFieldName: string): Boolean;
    function IndexExists(const sTableName: string
      ; const sIndexName: string
      ; const sIndexedFields: string
      ; const bUnique: Boolean = False
      ; const bCheckIndexedFields: Boolean = False
      ; const bCheckUnique: Boolean = False
      ): Boolean; override;
    function TriggerExists(const sTriggerName: string
      ; const bCheckActive: Boolean = False): Boolean; //todo (trigger stuff) override;
    procedure CreateGenIDTrigger(const sTableName, sKeyName, sGenName: string); //todo (trigger stuff) override;
    property DBPath: TUIBDatabase read GetDBPath;
  end;

  TSrvQuery = class(TJVCSQuery)
  private
    FUIBTransaction: TUIBTransaction;
    function GetDatabase: TUIBDatabase;
    procedure SetDatabase(Value: TUIBDatabase);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(const ParamName: string): TParam;
    procedure SetTransaction(const aQry : TSrvQuery);
    procedure SetTransactionNoAuto;
    property DatabaseName: TUIBDatabase read GetDatabase write SetDatabase;
  end;
  {$ENDIF UIBSRV}

  // ==============================
  // Oracle (NCOCI)
  // ------------------------------
  {$IFDEF ORACLESRV}
  //-- hdo
  TSrvDatabase = class(TJVCSDatabase)
  private
    FUseRuleOptimizerMode: Boolean;
    function GetDBPath: string;
  protected
    function Upgrade241: Boolean; override;
    procedure HandleAfterConnect(ASender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function IndexExists( const sTableName: string
                        ; const sIndexName: string
                        ; const sIndexedFields: string
                        ; const bUnique: Boolean = False
                        ; const bCheckIndexedFields: Boolean = False
                        ; const bCheckUnique: Boolean = False
                        ): Boolean; override;
    function FieldExists(const sTableName, sFieldName: string;
      ADataType: TFieldType = ftUnknown; ASize: Integer = 0): Boolean;
    property DBPath: string read GetDBPath;
    property UseRuleOptimizerMode: Boolean read FUseRuleOptimizerMode write FUseRuleOptimizerMode;
  end;

  TSrvQuery = class(TJVCSQuery)
    constructor Create(AOwner: TComponent); override;
  end;
  {$ENDIF ORACLESRV}

  // ==============================
  // MySQL over ZEOS
  // ------------------------------
  {$IFDEF MYSQLSRV}
  //-- LB
  TSrvDatabase = class(TJVCSDatabase)
  private
    function GetDBPath: TZConnection;
  protected
    function Upgrade240: Boolean; override;
    function Upgrade241: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AcquireConnection; override;
    function FieldExists(const sTableName, sFieldName: string;
      ADataType: TFieldType = ftUnknown; ASize: Integer = 0): Boolean;
    function IndexExists(const sTableName: string
                        ; const sIndexName: string
                        ; const sIndexedFields: string
                        ; const bUnique: Boolean = False
                        ; const bCheckIndexedFields: Boolean = False
                        ; const bCheckUnique: Boolean = False
                        ): Boolean; override;
    property DBPath: TZConnection read GetDBPath;
  end;

  TSrvQuery = class(TJVCSQuery)
  private
    function GetDatabaseName: TZConnection;
    procedure SetDatabaseName(NewValue: TZConnection);
  published
    property DatabaseName: TZConnection read GetDatabaseName write SetDatabaseName;
  end;
    
  TSrvTable = class(TZTable)
  end;
  {$ENDIF MYSQLSRV}

  // ==============================
  // MS-SQL over ADO
  // ------------------------------
  {$IFDEF MSSQLSRV}
  //-- MG
  TADOParameter = class(TObject)
  private
    FParameters: TList;
    function GetAsIntValue: Integer;
    function GetAsStringValue: string;
    function GetAsDateTimeValue: TDateTime;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TParameter;
    procedure SetAsIntValue(NewValue: Integer);
    procedure SetAsStringValue(NewValue: string);
    procedure SetAsDateTimeValue(NewValue: TDateTime);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddParameter(AParameter: TParameter);
    procedure Clear;
    procedure LoadFromStream(Stream: TStream; DataType: TDataType);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TParameter read GetItems; default;
  published
    property AsInteger: Integer read GetAsIntValue write SetAsIntValue;
    property AsString: string read GetAsStringValue write SetAsStringValue;
    property AsDateTime: TDateTime read GetAsDateTimeValue write SetAsDateTimeValue;
  end;

  TSrvDatabase = class(TJVCSDatabase)
  private
    FParams: TStringList;
    procedure BeforeConnection(Sender: TObject);
    function GetDBPath: TADOConnection;
  protected
    function Upgrade241: Boolean; override;
    function Upgrade242: Boolean; override;
    {$IFDEF BRANCHOBJECTS}
    function Upgrade250: Boolean; virtual;
    {$ENDIF BRANCHOBJECTS}
  public
    Drivername: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FieldExists(const sTableName, sFieldName: string;
                        ADataType: TFieldType = ftUnknown; ASize: Integer = 0): Boolean;
    function IndexExists(const sTableName: string
                        ; const sIndexName: string
                        ; const sIndexedFields: string
                        ; const bUnique: Boolean = False
                        ; const bCheckIndexedFields: Boolean = False
                        ; const bCheckUnique: Boolean = False
                        ): Boolean; override;
    property Params: TStringList read FParams;
    property DBPath: TADOConnection read GetDBPath;
  end;

  TSrvQuery = class(TJVCSQuery)
  private
    FADOParameterList: TObjectList;
    function GetConnection: TADOConnection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ParamByName(const Value: string): TADOParameter;
    property DatabaseName: TADOConnection read GetConnection write SetConnection;
  end;
  {$ENDIF MSSQLSRV}
  // ==============================
  // Generic port for ADO connection
  // ------------------------------
  {$IFDEF ADOSRV}
  //-- PL
  TADOParameter = class(TParameter)
  private
    function GetAsIntValue: Integer;
    function GetAsStringValue: string;
    function GetAsDateTimeValue: TDateTime;
    procedure SetAsIntValue(NewValue: Integer);
    procedure SetAsStringValue(NewValue: string);
    procedure SetAsDateTimeValue(NewValue: TDateTime);
  public
    procedure Clear;
  published
    property AsInteger: Integer read GetAsIntValue write SetAsIntValue;
    property AsString: string read GetAsStringValue write SetAsStringValue;
    property AsDateTime: TDateTime read GetAsDateTimeValue write SetAsDateTimeValue;
  end;

  TSrvDatabase = class(TJVCSDatabase)
  private
    FParams: TStringList;
    procedure BeforeConnection(Sender: TObject);
    function GetDBPath: TADOConnection;
  public
    Drivername: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Params: TStringList read FParams;
    property DBPath: TADOConnection read GetDBPath;
  end;

  TSrvQuery = class(TJVCSQuery)
  private
    function GetConnection: TADOConnection;
  protected
    procedure OpenCursor(InfoQuery: Boolean); override;
  public
    function ParamByName(const Value: string): TADOParameter;
    property DatabaseName: TADOConnection read GetConnection write SetConnection;
  end;
  {$ENDIF ADOSRV}

function GetInsertKeyFieldValueStr(const ATableName: string): string;

implementation

uses
  SysUtils,
  vcsCommon,
  SrvCustomSrvOBJ;

const
  cNoConnectionVersion = 0;
  // for DB version 2.41
  c241_parentext = 20;

{$IFDEF UIBSRV}
  {$DEFINE HAS_SEQUENCES}
const
  SequenceNames: array [1..22
    {$IFDEF BRANCHOBJECTS} + 3 {$ENDIF}
    {$IFDEF PROPERTYOBJECTS} + 2 {$ENDIF}
    ] of array [0..1] of string = (
    ('BLOBS',    'GEN_BLOB_ID'),
    ('BUGS',     'GEN_BUG_ID'),
    ('FFAMILY',  'GEN_FAMILY_ID'),
    ('GROUPS',   'GEN_GROUP_ID'),
    ('LABELS',   'GEN_LABEL_ID'),
    ('LOGCOMM',  'GEN_LOGCOMMREC_ID'),
    ('MDBUGS',   'GEN_MDBUGSREC_ID'),
    ('MODULES',  'GEN_MODULE_ID'),
    ('MSTONES',  'GEN_MILESTONE_ID'),
    ('PJBUGS',   'GEN_PJBUGSREC_ID'),
    ('PJGROUPS', 'GEN_PJGROUPS_ID'),
    ('PJMODULE', 'GEN_PJMODULEREC_ID'),
    ('PJMSTONE', 'GEN_PJMSTONEREC_ID'),
    ('PJREF',    'GEN_PJREFREC_ID'),
    ('PJUSERS',  'GEN_PJUSERSREC_ID'),
    ('PROJECTS', 'GEN_PROJECT_ID'),
    ('REVISION', 'GEN_REVISION_ID'),
    ('RVLABELS', 'GEN_RVLABELREC_ID'),
    ('TODO',     'GEN_TODO_ID'),
    ('TRANSACT', 'GEN_TRANSACT_ID'),
    ('USERS',    'GEN_USER_ID'),
    ('VCSLOG',   'GEN_LOG_ID')
    {$IFDEF BRANCHOBJECTS}
    , ('BRANCHES', 'GEN_BRANCH_ID')
    , ('BRMODULE', 'GEN_BRMODULE_ID')
    , ('RVBRANCH', 'GEN_RVBRANCH_ID')
    {$ENDIF BRANCHOBJECTS}
    {$IFDEF PROPERTYOBJECTS}
    , ('PROPERTY',         'GEN_PROPERTY_ID')
    , ('REVISIONPROPERTY', 'GEN_REVISIONPROPERTY_ID')
    {$ENDIF PROPERTYOBJECTS}
    );
{$ENDIF UIBSRV}

{$IFDEF ORACLESRV}
  {$DEFINE HAS_SEQUENCES}
const
  SequenceNames: array [1..22
    {$IFDEF BRANCHOBJECTS} + 3 {$ENDIF}
    {$IFDEF PROPERTYOBJECTS} + 2 {$ENDIF}
    ] of array [0..1] of string = (
    ('BLOBS',    'S_BLOB_ID'),
    ('BUGS',     'S_BUG_ID'),
    ('FFAMILY',  'S_FAMILY_ID'),
    ('GROUPS',   'S_GROUP_ID'),
    ('LABELS',   'S_LABEL_ID'),
    ('LOGCOMM',  'S_LOGCOMM_ID'),
    ('MDBUGS',   'S_MDBUGS_ID'),
    ('MODULES',  'S_MODULE_ID'),
    ('MSTONES',  'S_MILESTONE_ID'),
    ('PJBUGS',   'S_PJBUGS_ID'),
    ('PJGROUPS', 'S_PJGROUPS_ID'),
    ('PJMODULE', 'S_PJMODULE_ID'),
    ('PJMSTONE', 'S_PJMSTONE_ID'),
    ('PJREF',    'S_PJREF_ID'),
    ('PJUSERS',  'S_PJUSERS_ID'),
    ('PROJECTS', 'S_PROJECT_ID'),
    ('REVISION', 'S_REVISION_ID'),
    ('RVLABELS', 'S_RVLABELS_ID'),
    ('TODO',     'S_TODO_ID'),
    ('TRANSACT', 'S_TRANSACT_ID'),
    ('USERS',    'S_USER_ID'),
    ('VCSLOG',   'S_LOG_ID')
    {$IFDEF BRANCHOBJECTS}
    , ('BRANCHES', 'S_BRANCH_ID')
    , ('BRMODULE', 'S_BRMODULE_ID')
    , ('RVBRANCH', 'S_RVBRANCH_ID')
    {$ENDIF BRANCHOBJECTS}
    {$IFDEF PROPERTYOBJECTS}
    , ('PROPERTY',         'S_PROPERTY_ID')
    , ('REVISIONPROPERTY', 'S_REVISIONPROPERTY_ID')
    {$ENDIF PROPERTYOBJECTS}
    );
{$ENDIF ORACLESRV}

{$IFDEF HAS_SEQUENCES}
function GetSequenceName(const ATableName: string): string;
var
  I, Idx: Integer;
begin
  Idx := -1;
  for I := Low(SequenceNames) to High(SequenceNames) do
    if SameText(SequenceNames[I][0], ATableName) then
    begin
      Idx := I;
      Break;
    end;
  if Idx <> -1 then
    Result := SequenceNames[I][1];
end;
{$ENDIF HAS_SEQUENCES}

function GetInsertKeyFieldValueStr(const ATableName: string): string;
begin
  {$IFDEF UIBSRV} //-- VR
  Result := Format('GEN_ID(%s, 1),', [GetSequenceName(ATableName)]);
  {$ENDIF UIBSRV}
  {$IFDEF ORACLESRV}
  Result := Format('%s.NextVal,', [GetSequenceName(ATableName)]); //-- hdo
  {$ENDIF ORACLESRV}
  {$IFDEF MSSQLSRV}
  Result := ''; //-- MG
  {$ENDIF MSSQLSRV}
  {$IFDEF MYSQLSRV}
  Result := '0,'; //-- MG
  {$ENDIF MYSQLSRV}
  {$IFDEF ADOSRV}
  Result := ''; //-- PL
  {$ENDIF ADOSRV}
end;

// =============================================================================
// TJVCSDatabase
// =============================================================================
{
  Versiontable DBARCHIVEVERSION
    archiveversion   integer (eg. 110 or 240 or 241) Unique!
    isactualversion  0 = false, 1=true
    upgradedatetime  varchar(14) format yyyymmddhhnnss (eg. 20040101161059)
 }

// -----------------------------------------------------------------------------

constructor TJVCSDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TJVCSDatabase.AcquireConnection;
begin
  //to be done in descendant
end;

// -----------------------------------------------------------------------------

function TJVCSDatabase.CreateDBVersionTable: Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add('CREATE TABLE DBARCHIVEVERSION (');
        SQL.Add('ARCHIVEVERSION   integer,');
        SQL.Add('ISACTUALVERSION  integer,');
        SQL.Add('UPGRADEDATETIME  varchar(14)');
        SQL.Add(')');
        try
          ExecSQL;
          Result := True;
        except
        end;
      end;
    finally
      aQry.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------
//
// - Only call if database connected
// - Override if there is a better function for the specific database port
//
function TJVCSDatabase.TableExists(const sTableName: string): Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  Assert(Connected, 'SrvDatabase.TableExists needs connected database!');
  aQry := TSrvQuery.Create(nil);
  try
    aQry.DatabaseName := TSrvDatabase(Self).DBPath;
    with aQry do
    begin
      SQL.Clear;
      SQL.Add(Format('SELECT * FROM %s', [sTableName]));
      SQL.Add('WHERE -1=1');
      try
        Open;
        Result := True;
      except
      end;
    end;
  finally
    aQry.Free;
  end;
end;

// -----------------------------------------------------------------------------

function TJVCSDatabase.GetArchiveVersion: Integer;
var
  aQry: TSrvQuery;
begin
  // default is first version without dbversion table
  Result := cNoConnectionVersion;
  if Connected then
  begin
    Result := BaseArchiveVersion;
    if TableExists('DBARCHIVEVERSION') then
    begin
      aQry := TSrvQuery.Create(nil);
      try
        aQry.DatabaseName := TSrvDatabase(Self).DBPath;
        with aQry do
        begin
          SQL.Clear;
          SQL.Add('SELECT ARCHIVEVERSION FROM DBARCHIVEVERSION');
          SQL.Add('WHERE ISACTUALVERSION = 1');
          try
            Open;
            if not Eof then
            begin
              Result := FieldByName('ARCHIVEVERSION').AsInteger;
            end;
          except
          end;
          Close;
        end;
      finally
        aQry.Free;
      end;
    end
    else
    begin
      if CreateDBVersionTable then
      begin
        //--- insert base record
        if SetUpgradeDone(BaseArchiveVersion) then
        begin
          Result := BaseArchiveVersion;
        end;
      end;
    end;
  end;
end;

// -----------------------------------------------------------------------------

function TJVCSDatabase.SetUpgradeDone(const nUpgradeVersion: Integer): Boolean;
var
  aQry: TSrvQuery;
  bVersionExists: Boolean;
begin
  Result:=True;
  bVersionExists := False;
  aQry := TSrvQuery.Create(nil);
  try
    aQry.DatabaseName := TSrvDatabase(Self).DBPath;
    with aQry do
    begin
      SQL.Clear;
      SQL.Add('SELECT * FROM DBARCHIVEVERSION');
      Open;
      try
        while not Eof do
        begin
          if FieldByName('ARCHIVEVERSION').AsInteger = nUpgradeVersion then
          begin
            bVersionExists := True;
          end;
          Next;
        end;
      finally
        Close;
      end;
      // IF upgrade version already exists?
      if bVersionExists then
      begin
        // upgraded version is the actual version
        SQL.Clear;
        SQL.Add('UPDATE DBARCHIVEVERSION SET');
        SQL.Add('ISACTUALVERSION = 1');
        SQL.Add('WHERE ARCHIVEVERSION = :ARCHIVEVERSION');
        ParamByName('ARCHIVEVERSION').AsInteger := nUpgradeVersion;
        ExecSQL;
      end
      else
      begin
        // have to Insert new record
        SQL.Clear;
        SQL.Add('INSERT INTO DBARCHIVEVERSION');
        SQL.Add('(ARCHIVEVERSION, ISACTUALVERSION, UPGRADEDATETIME)');
        SQL.Add('VALUES');
        SQL.Add('(:ARCHIVEVERSION, :ISACTUALVERSION, :UPGRADEDATETIME)');
        ParamByName('ARCHIVEVERSION').AsInteger   := nUpgradeVersion;
        ParamByName('ISACTUALVERSION').AsInteger  := 1;
        ParamByName('UPGRADEDATETIME').AsString   := FormatDateTime('yyyymmddhhnnss', Now);
        ExecSQL;
      end;
      // Set actual to 0 on other versions
      SQL.Clear;
      SQL.Add('UPDATE DBARCHIVEVERSION SET');
      SQL.Add('ISACTUALVERSION = 0');
      SQL.Add('WHERE');
      SQL.Add('ARCHIVEVERSION <> :ARCHIVEVERSION');
      ParamByName('ARCHIVEVERSION').AsInteger := nUpgradeVersion;
      ExecSQL;

      FArchiveVersion := nUpgradeVersion;
    end;
  finally
    aQry.Free;
  end;
end;

// -----------------------------------------------------------------------------
// Implement default updates for Archive version 240
//
// 1) add new index to vcslog table
//
function TJVCSDatabase.Upgrade240: Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add('CREATE INDEX I_VCSLOG_TSTAMP ON VCSLOG(TSTAMP)');
        try
          ExecSQL;
        except
        end;
        // close query before checking to force commit
        Close;
        // check here!
        Result := IndexExists('VCSLOG', 'I_VCSLOG_TSTAMP', 'TSTAMP');
      end;
    finally
      aQry.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------
// Implement default updates for Archive version 241
//
// 1) to implement change ffamily.parentext in descent
//
function TJVCSDatabase.Upgrade241: Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add(Format('ALTER TABLE FFAMILY ALTER PARENTEXT TYPE VARCHAR(%d)', [c241_parentext]));
        try
          ExecSQL;
        except
        end;
        // close query before checking to force commit
        Close;
        // check here!
        Result := IndexExists('VCSLOG', 'I_VCSLOG_TSTAMP', 'TSTAMP');
      end;
    finally
      aQry.Free;
    end;
  end;
end;

// -----------------------------------------------------------------------------
// Implement default updates for Archive version 242
//
function TJVCSDatabase.Upgrade242: Boolean;
begin
  Result := True;//no default updates (only MSSQL is fixed in 242)
end;

{$IFDEF BRANCHOBJECTS}
// -----------------------------------------------------------------------------
// Implement default updates for Archive version 250 (first branch)
//
// 1) to implement change ffamily.parentext in descent
//
function TJVCSDatabase.Upgrade250: Boolean;
{$IFDEF UIBSRV}
var
  aQry: TSrvQuery;
{$ENDIF UIBSRV}
begin
{$IFDEF UIBSRV}
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add('CREATE INDEX I_RVBRANCH_BID_RID ON RVBRANCH(BRANCHID, REVISIONID)');
        try
          ExecSQL;
        except
        end;
        // close query before checking to force commit
        Close;
        // check here!
        Result := IndexExists('RVBRANCH', 'I_RVBRANCH_BID_RID', 'BRANCHID, REVISIONID');
        If Result then
        begin
          with aQry do
          begin
            SQL.Clear;
            SQL.Add('CREATE INDEX I_BRMODULE_BID_MID ON BRMODULE(BRANCHID, MODULEID)');
            try
              ExecSQL;
            except
            end;
            // close query before checking to force commit
            Close;
            // check here!
            Result := IndexExists('BRMODULE', 'I_BRMODULE_BID_MID', 'BRANCHID, MODULEID');
            If Result then
            begin
            end;
          end;
        end;
      end;
    finally
      aQry.Free;
    end;
  end;
{$ENDIF UIBSRV}
end;
{$ENDIF BRANCHOBJECTS}

// -----------------------------------------------------------------------------

function TJVCSDatabase.PrepareUpgrade: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TJVCSDatabase.UnprepareUpgrade: Boolean;
begin
  Result := True;
end;

// -----------------------------------------------------------------------------

function TJVCSDatabase.UpgradeArchive: Boolean;
var
  nArchiveVersion: Integer;
begin
  Result := False;
  nArchiveVersion := ArchiveVersion;
  if (nArchiveVersion <> cNoConnectionVersion) then
  begin
    Result := True;
    // Need upgraded to latest archiv version?
    if (nArchiveVersion < ExpectedArchiveVersion) then
    begin
      Result := PrepareUpgrade;
      if Result then ; // eg. drop indexes/other db objects
      begin
        try
          // all upgrade steps/versions below
          // Upgrade to 240
          if (nArchiveVersion < 240) then
          begin
            // Change Metadata
            Result := Upgrade240;
            if Result then
            begin
              // Write back to archive table (nArchiveVersion will be updated if ok)
              Result := SetUpgradeDone(240);
            end;
          end;
          // Upgrade to 241
          if Result and (nArchiveVersion < 241) then
          begin
            Result := Upgrade241;
            if Result then
            begin
              // Write back to archive table (nArchiveVersion will be updated if ok)
              Result := SetUpgradeDone(241);
            end;
          end;
          // Upgrade to 242
          if Result and (nArchiveVersion < 242) then
          begin
            Result := Upgrade242;
            if Result then
            begin
              // Write back to archive table (nArchiveVersion will be updated if ok)
              Result := SetUpgradeDone(242);
            end;
          end;
{$IFDEF BRANCHOBJECTS}
          // Upgrade to 250 only for BRANCHING
          if Result and (nArchiveVersion < 250) then
          begin
            Result := Upgrade250;
            if Result then
            begin
              // Write back to archive table (nArchiveVersion will be updated if ok)
              Result := SetUpgradeDone(250);
            end;
          end;
{$ENDIF BRANCHOBJECTS}
          // ------------------------------------------------
          // insert below next upgrade run (eg. 244 or 300...)
          // ------------------------------------------------
        finally
          Result := Result and UnprepareUpgrade; // eg. restore indexes/other db objects
        end;
      end;
    end;
  end;
end;

// =============================================================================
//  TJVCSQuery
// =============================================================================
constructor TJVCSQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnDisplay := nil;
  {$IFDEF SQL_DEBUG}
  if AOwner is TFVCSServerObject then
    FOnDisplay := TFVCSServerObject(AOwner).QueryDisplay;
  {$ENDIF SQL_DEBUG}
  {$IFDEF UIBSRV}
  FParams := TParams.Create(Self);
  FHackDoSQLChange := TStringList(SQL).OnChange;
  TStringList(SQL).OnChange := SQLChanged;
  {$ENDIF UIBSRV}
end;

{$IFDEF UIBSRV}
destructor TJVCSQuery.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;
{$ENDIF UIBSRV}

procedure TJVCSQuery.TriggerDisplay(aMsg: string);
begin
  {$IFDEF SQL_DEBUG}
  if Assigned(FOnDisplay) then
    FOnDisplay(Self, '>> '  + aMsg);
  {$ENDIF SQL_DEBUG}
end;

{$IFDEF UIBSRV}
procedure TJVCSQuery.ReMapParams;
var
  I: Integer;
  BS: string;
begin
  for I := 0 to FParams.Count - 1 do
  begin
    with FParams.Items[I] do
    begin
      if IsNull then
        Params.ByNameIsNull[Name] := True
      else
      begin
        case DataType of
        ftDateTime: Params.ByNameAsDateTime[Name] := AsDateTime;
        ftInteger: Params.ByNameAsInteger[Name] := AsInteger;
        ftSmallint: Params.ByNameAsSmallint[Name] := AsSmallInt;
        ftFloat: Params.ByNameAsDouble[Name] := AsFloat;
        ftBlob:
        begin
          BS := AsBlob;
          ParamsSetBlob(Name,BS);
        end
        else
          Params.ByNameAsString[Name] := AsString;
        end;
      end;
    end;
  end;
end;
{$ENDIF UIBSRV}

procedure TJVCSQuery.Open;
{$IFDEF SQL_DEBUG}
var
  I: Integer;
{$ENDIF SQL_DEBUG}
begin
  {$IFDEF SQL_DEBUG}
  TriggerDisplay('--------------------------------------------------------');
  TriggerDisplay('OPEN WITH SQL: ');
  for I := 0 to SQL.Count - 1 do
    TriggerDisplay(SQL[I]);
  TriggerDisplay('--------------------------------------------------------');
  {$ENDIF SQL_DEBUG}
  {$IFDEF UIBSRV}
  ReMapParams;
  {$ENDIF UIBSRV}
  inherited Open;
end;

procedure TJVCSQuery.Close;
begin
  inherited Close;
  {$IFDEF UIBSRV}
  FParams.Clear;
  {$ENDIF UIBSRV}
end;

procedure TJVCSQuery.ExecSQL;
var
  I: Integer;
begin
  TriggerDisplay('--------------------------------------------------------');
  TriggerDisplay('EXECSQL WITH SQL: ');
  for I := 0 to SQL.Count - 1 do
    TriggerDisplay(SQL[I]);
  TriggerDisplay('--------------------------------------------------------');
  {$IFDEF UIBSRV}
  ReMapParams;
  inherited Execute;
  {$ELSE}
  inherited ExecSQL;
  {$ENDIF UIBSRV}
end;

procedure TJVCSQuery.SetTransaction(const aQry : TJVCSQuery);
begin
  // Just a place holder to avoid IFDEFs
  // Used actually only for UIB port
end;

procedure TJVCSQuery.SetTransactionNoAuto;
begin
  // Just a place holder to avoid IFDEFs
  // Used actually only for UIB port
end;


{$IFDEF UIBSRV}
procedure TJVCSQuery.SQLChanged(Sender: TObject);
begin
  FParams.Clear;
  if Assigned(FHackDoSQLChange) then
    FHackDoSQLChange(Self);
end;
{$ENDIF UIBSRV}

//=== Oracle server ============================================================

{$IFDEF ORACLESRV}
//-- hdo
constructor TSrvDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DatabaseName := 'LocalSQLDB';
  LoginPrompt := False;
  FUseRuleOptimizerMode := False;
  AfterConnect := HandleAfterConnect;
  {$IFDEF NTSERVICESRV}
  SilentMode := True;
  {$ENDIF NTSERVICESRV}
end;

procedure TSrvDatabase.HandleAfterConnect(ASender: TObject);
var
  Query: TSrvQuery;
begin
  if FUseRuleOptimizerMode then
  begin
    Query := TSrvQuery.Create(Self);
    try
      Query.DataBaseName := Self.DBPath;
      with Query do
      begin
        if Pos('10G', UpperCase(ServerVersion)) > 0 then
          SQL.Add('ALTER SESSION SET OPTIMIZER_MODE=RULE')
        else
          SQL.Add('ALTER SESSION SET OPTIMIZER_GOAL=RULE');
        ExecSQL;
      end;
    finally
      Query.Free;
    end;
  end;
end;

function TSrvDatabase.GetDBPath: string;
begin
  Result := DatabaseName;
end;

// ---------------------------------------------------------------------------
//
function TSrvDatabase.Upgrade241: Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add(Format('ALTER TABLE FFAMILY MODIFY (PARENTEXT VARCHAR2(%d))', [c241_parentext]));
        try
          ExecSQL;
        except
        end;
        // close query before checking to force commit
        Close;
        // check here!
        Result := FieldExists('FFAMILY', 'PARENTEXT', ftString, c241_parentext);
      end;
    finally
      aQry.Free;
    end;
  end;
end;

function TSrvDatabase.FieldExists(const sTableName, sFieldName: string;
  ADataType: TFieldType = ftUnknown; ASize: Integer = 0): Boolean;
var
  ii: Integer;
  aSrvQry: TSrvQuery;
begin
  Result := False;
  aSrvQry := TSrvQuery.Create(nil);
  try
    aSrvQry.DatabaseName := Self.DBPath;
    with aSrvQry do
    begin
      SQL.Add(Format('SELECT * FROM %s WHERE -1=1', [sTableName]));
      try
        Open;
        for ii := 0 to Pred(FieldDefs.Count) do
        begin
          if SameText(FieldDefs[ii].Name, sFieldName) then
          begin
            Result := (ADataType = ftUnknown) or ((FieldDefs[ii].DataType = ADataType) and
              ((ADataType <> ftString) or (FieldDefs[ii].Size = ASize)));
            Break;
          end;
        end;
      except
      end;
    end;
  finally
    aSrvQry.Free;
  end;
end;

function TSrvDatabase.IndexExists ( const sTableName: string
                                  ; const sIndexName: string
                                  ; const sIndexedFields: string
                                  ; const bUnique: Boolean = False
                                  ; const bCheckIndexedFields: Boolean = False
                                  ; const bCheckUnique: Boolean = False
                                  ): Boolean;
var
  aQry: TSrvQuery;
  FoundIndex: Boolean;
  UniqueOkay: Boolean;
  IndexFieldsOkay: Boolean;
  sFoundedIndexFields: string;
begin
  aQry := TSrvQuery.Create(nil);
  try
    aQry.DatabaseName := Self.DBPath;
    with aQry do
    begin
      SQL.Clear;
      SQL.Add('SELECT UNIQUENESS FROM ALL_INDEXES');
      SQL.Add(Format('WHERE (UPPER(TABLE_NAME) = UPPER(%s))', [QuotedStr(sTableName)]));
      SQL.Add(Format('AND (UPPER(INDEX_NAME) = UPPER(%s))', [QuotedStr(sIndexName)]));
      Open;
      FoundIndex := not (Bof and Eof);
      UniqueOkay := (not bCheckUnique) or
         ((FoundIndex) and ((FieldByName('UNIQUENESS').AsString = 'UNIQUE') = bUnique));
      Close;
      if FoundIndex and bCheckIndexedFields then
      begin
        SQL.Clear;
        SQL.Add('SELECT COLUMN_NAME FROM ALL_IND_COLUMNS');
        SQL.Add(Format('WHERE (UPPER(TABLE_NAME) = UPPER(%s))', [QuotedStr(sTableName)]));
        SQL.Add(Format('AND (UPPER(INDEX_NAME) = UPPER(%s))', [QuotedStr(sIndexName)]));
        SQL.Add('ORDER BY COLUMN_POSITION');
        Open;
        while not Eof do
        begin
          if sFoundedIndexFields <> '' then
            sFoundedIndexFields := sFoundedIndexFields + ';';
          sFoundedIndexFields := sFoundedIndexFields + FieldByName('COLUMN_NAME').AsString;
          Next;
        end;
        IndexFieldsOkay := UpperCase(sFoundedIndexFields) = UpperCase(sIndexedFields);
        Close;
      end
      else
        IndexFieldsOkay := not bCheckIndexedFields;
      Result := FoundIndex and UniqueOkay and IndexFieldsOkay;
    end;
  finally
    aQry.Free;
  end;
end;

constructor TSrvQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.DataFormat.EnableFixedString := False;
  Self.DataFormat.EnableLongString := True;
end;
{$ENDIF ORACLESRV}


//=== Firebird, Interbase or Yaffil Server (UIB) ===============================

{$IFDEF UIBSRV}
//-- PrY
// --- TSrvDatabase ---
constructor TSrvDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SQLDialect := 3;
end;

function TSrvDatabase.GetDBPath: TUIBDatabase;
begin
  Result := Self;
end;

function TSrvDatabase.TableExists(const sTableName: string): Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  Assert(Connected, 'SrvDatabase.TableExists needs connected database!');
  aQry := TSrvQuery.Create(nil);
  try
    aQry.DatabaseName := TSrvDatabase(Self).DBPath;
    with aQry do
    begin
      SQL.Clear;
      SQL.Add('SELECT RDB$RELATION_NAME from RDB$RELATIONS');
      SQL.Add('where (RDB$VIEW_SOURCE is NULL) and (RDB$SYSTEM_FLAG = 0) and (RDB$RELATION_NAME=?)');
      try
        Params.AsString[0] := sTableName;
        Open;
        Result := not Eof;
      except
        on E: Exception do
          Assert(False, E.Message);
      end;
    end;
  finally
    aQry.Free;
  end;
end;

function TSrvDatabase.FieldExists(const sTableName, sFieldName: string): Boolean;
var
  aSrvQry: TSrvQuery;
begin
  Result := False;
  Assert(Connected, 'SrvDatabase.FieldExists needs connected database!');
  aSrvQry := TSrvQuery.Create(nil);
  try
    aSrvQry.DatabaseName := TSrvDatabase(Self).DBPath;
    with aSrvQry do
    begin
      SQL.Clear;
      SQL.Add('SELECT f.rdb$field_name FROM rdb$relations r');
      SQL.Add('left join rdb$relation_fields f on (f.rdb$relation_name = r.rdb$relation_name)');
      SQL.Add('WHERE r.rdb$relation_name=? AND f.rdb$field_name=?');
      try
        Params.AsString[0] := sTableName;
        Params.AsString[1] := sFieldName;
        Open;
        Result := not Eof;
      except
      end;
    end;
  finally
    aSrvQry.Free;
  end;
end;

function TSrvDatabase.IndexExists(const sTableName: string
  ; const sIndexName: string
  ; const sIndexedFields: string
  ; const bUnique: Boolean = False
  ; const bCheckIndexedFields: Boolean = False
  ; const bCheckUnique: Boolean = False
  ): Boolean;
var
  aSrvQuery: TSrvQuery;
  sListFields: string;
begin
  Assert(Connected, 'SrvDatabase.IndexExists needs connected database!');
  aSrvQuery := TSrvQuery.Create(nil);
  try
    aSrvQuery.DatabaseName := TSrvDatabase(Self).DBPath;
    with aSrvQuery do
    begin
      SQL.Clear;
      SQL.Add('SELECT i.rdb$index_name, i.rdb$unique_flag, i.rdb$index_inactive, isg.rdb$field_name FROM rdb$indices i');
      SQL.Add('LEFT JOIN rdb$index_segments isg on (isg.rdb$index_name = i.rdb$index_name)');
      SQL.Add('WHERE (not i.rdb$index_name starting with ''RDB$'') and (i.rdb$relation_name = ?) and (i.rdb$index_name = ?)');
      SQL.Add('ORDER BY i.rdb$index_name, isg.rdb$field_position');

      try
        Params.AsString[0] := sTableName;
        Params.AsString[1] := sIndexName;

        Open;

        Result := not Eof;

        if Result and bCheckUnique then
          Result := (bUnique and (Fields[1].AsInteger = 1)) or ((not bUnique) and (Fields[1].AsInteger = 0));

        if Result and bCheckIndexedFields then
        begin
          while not Eof do
          begin
            sListFields := sListFields + Fields[3].AsString + ';';
            Next;
          end;
          System.Delete(sListFields, Length(sListFields), 1);

          Result := SameText(sListFields, sIndexedFields);
        end;
      finally
        Close;
      end;
    end;
  finally
    FreeAndNil(aSrvQuery);
  end;
end;

function TSrvDatabase.TriggerExists(const sTriggerName: string;
  const bCheckActive: Boolean): Boolean;
var
  aSrvQuery: TSrvQuery;
begin
  Assert(Connected, 'SrvDatabase.TriggerExists needs connected database!');
  aSrvQuery := TSrvQuery.Create(nil);
  try
    aSrvQuery.DatabaseName := TSrvDatabase(Self).DBPath;
    with aSrvQuery do
    begin
      SQL.Clear;
      SQL.Add('SELECT T.RDB$TRIGGER_NAME, T.RDB$TRIGGER_INACTIVE from RDB$TRIGGERS T');
      SQL.Add('left join RDB$CHECK_CONSTRAINTS C ON (C.RDB$TRIGGER_NAME = T.RDB$TRIGGER_NAME)');
      SQL.Add('where ((T.RDB$SYSTEM_FLAG = 0) or (T.RDB$SYSTEM_FLAG is null))');
      SQL.Add('  and (c.rdb$trigger_name is null) and (t.rdb$trigger_name=?)');

      try
        Params.AsString[0] := sTriggerName;
        Open;
        Result := not Eof;
        if Result and bCheckActive then
          Result := Fields[1].AsInteger = 0;
      finally
        Close;
      end;
    end;
  finally
    FreeAndNil(aSrvQuery);
  end;
end;

procedure TSrvDatabase.CreateGenIDTrigger(const sTableName, sKeyName,
  sGenName: string);
var
  aSrvQuery: TSrvQuery;
begin
  Assert(Connected, 'SrvDatabase.CreateGenIDTrigger needs connected database!');
  aSrvQuery := TSrvQuery.Create(nil);
  try
    aSrvQuery.DatabaseName := TSrvDatabase(Self).DBPath;
    with aSrvQuery do
    begin
      SQL.Add(Format('CREATE TRIGGER TRG_%s_GEN_ID FOR %s',[sTableName,sTableName]));
      SQL.Add('ACTIVE BEFORE INSERT POSITION 0');
      SQL.Add('AS');
      SQL.Add('begin');
      SQL.Add(Format('if ((new.%s is null) or (new.%s = 0)) then',[sKeyName,sKeyName]));
      SQL.Add(Format('  new.%s = gen_id(%s,1);',[sKeyName,sGenName]));
      SQL.Add('end');
      ExecSQL;
    end;
  finally
    aSrvQuery.Free;
  end;
end;

// --- TSrvQuery ---

constructor TSrvQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUIBTransaction := nil;
end;

destructor TSrvQuery.Destroy;
begin
  if Assigned(FUIBTransaction) then
  begin
    if FUIBTransaction.InTransaction then
      FUIBTransaction.Commit;
    FreeAndNil(FUIBTransaction);
  end;
  inherited;
end;

function TSrvQuery.GetDatabase: TUIBDatabase;
begin
  Result := Database;
end;

function TSrvQuery.ParamByName(const ParamName: string): TParam;
var
  I, J, Idx: Integer;
  P: TParam;
begin
  Result := nil;

  if FParams.Count = 0 then
  begin
    { Map Params (TSQLParams) to TParams }
    for I := 0 to Params.ParamCount - 1 do
    begin
      Idx := -1;
      for J := 0 to Pred(Params.FieldCount) do
        if Params.Data^.sqlvar[J].ID = I + 1 then
        begin
          Idx := J;
          Break;
        end;
      if Idx <> -1 then
      begin
        P := TParam.Create(FParams);
        P.Name := Params.FieldName[Idx];
        P.Value := Params.Values[P.Name];
        FParams.AddParam(P);

        if SameText(P.Name, ParamName) then
          Result := P;
      end;
    end;
  end
  else
    Result := FParams.ParamByName(ParamName);

  { From Db.pas }
  if Result = nil then
    DatabaseErrorFmt(SParameterNotFound, [ParamName]);
end;

procedure TSrvQuery.SetDatabase(Value: TUIBDatabase);
begin
  if Assigned(Transaction) then
    FreeAndNil(FUIBTransaction);

  FUIBTransaction := TUIBTransaction.Create(nil);
  // don't change from without complete overwork of firebird transacation handling!!!
  //todo rework transacation handling
  FUIBTransaction.Options := [tpAutoCommit];
  FUIBTransaction.DataBase := Value;

  Transaction := FUIBTransaction;
end;

procedure TSrvQuery.SetTransaction(const aQry : TSrvQuery);
begin
  // Check valid Query and if Query has an active transaction
  if Assigned(aQry) then
    if aQry.Transaction.InTransaction then
      Transaction := aQry.Transaction;
end;

procedure TSrvQuery.SetTransactionNoAuto;
begin
  Transaction.Options := [tpNoWait,tpReadCommitted,tpRecVersion];
end;

{$ENDIF UIBSRV}

//=== MSSQL server =============================================================

{$IFDEF MSSQLSRV}
//-- MG

constructor TADOParameter.Create;
begin
  inherited Create;
  FParameters := TList.Create;
end;

destructor TADOParameter.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

procedure TADOParameter.AddParameter(AParameter: TParameter);
begin
  FParameters.Add(AParameter);
end;

procedure TADOParameter.Clear;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    Items[I].Value := Null;
end;

function TADOParameter.GetAsIntValue: Integer;
begin
  Result := Items[0].Value;
end;

function TADOParameter.GetAsStringValue: string;
begin
  Result := Items[0].Value;
end;

function TADOParameter.GetAsDateTimeValue: TDateTime;
begin
  Result := Items[0].Value;
end;

function TADOParameter.GetCount: Integer;
begin
  Result := FParameters.Count;
end;

function TADOParameter.GetItems(AIndex: Integer): TParameter;
begin
  Result := FParameters[AIndex];
end;

procedure TADOParameter.LoadFromStream(Stream: TStream; DataType: TDataType);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    Items[I].LoadFromStream(Stream, DataType);
end;

procedure TADOParameter.SetAsIntValue(NewValue: Integer);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    with Items[I] do
    begin
      DataType := ftInteger;
      Value := NewValue;
    end;
end;

procedure TADOParameter.SetAsStringValue(NewValue: string);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    with Items[I] do
    begin
      DataType := ftString;
      if NewValue = '' then
        Value := Null
      else
        Value := NewValue;
    end;
end;

procedure TADOParameter.SetAsDateTimeValue(NewValue: TDateTime);
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
    with Items[I] do
    begin
      DataType := ftDateTime;
      Value := NewValue;
    end;
end;

procedure TSrvDatabase.BeforeConnection(Sender: TObject);
begin
  if Params.Values['integrated security'] <> '' then
  begin
    ConnectionString := 'Provider=sqloledb'     +
                        ';Initial Catalog='     + Params.Values['database name']  +
                        ';Data Source='         + Params.Values['server name']    +
                        ';Integrated Security=' + Params.Values['integrated security'];
  end
  else
  begin
    ConnectionString := 'Provider=SQLOLEDB.1;Password=' + Params.Values['password'] +
                        ';Persist Security Info=True;User ID=' + Params.Values['user name'] +
                        ';Initial Catalog=' + Params.Values['database name'] +
                        ';Data Source=' + Params.Values['server name'];
  end;
end;

function TSrvDatabase.GetDBPath: TADOConnection;
begin
  Result := Self;
end;

function TSrvDatabase.FieldExists(const sTableName, sFieldName: string;
  ADataType: TFieldType = ftUnknown; ASize: Integer = 0): Boolean;
var
  ii: Integer;
  aSrvQry: TSrvQuery;
begin
  Result := False;
  aSrvQry := TSrvQuery.Create(nil);
  try
    aSrvQry.DatabaseName := Self;
    with aSrvQry do
    begin
      SQL.Clear;
      SQL.Add(Format('SELECT * FROM %s WHERE -1=1', [sTableName]));
      try
        Open;
        for ii := 0 to Pred(FieldDefs.Count) do
        begin
          if SameText(FieldDefs[ii].Name, sFieldName) then
          begin
            if (FieldDefs[ii].DataType = ADataType) and
               (FieldDefs[ii].Size = ASize) then
            begin
              Result := True;
              Break;
            end;
          end;
        end;
      except
      end;
    end;
  finally
    aSrvQry.Free;
  end;
end;

function TSrvDatabase.IndexExists ( const sTableName: string
                                  ; const sIndexName: string
                                  ; const sIndexedFields: string
                                  ; const bUnique: Boolean = False
                                  ; const bCheckIndexedFields: Boolean = False
                                  ; const bCheckUnique: Boolean = False
                                  ): Boolean;
var
  aAdoDataSet: TAdoDataSet;
  bPassedUniqueCheck: Boolean;
  sTmpIndexFields: string;
begin
  aAdoDataSet := TAdoDataSet.Create(nil);
  try
    OpenSchema( siIndexes
              , VarArrayOf([Null, Null, Null, Null, Null])
              , EmptyParam
              , aAdoDataSet
              );
    with aAdoDataSet do
    begin
      bPassedUniqueCheck := not bCheckUnique;
      Filtered := False;
      Sort := 'COLUMN_NAME ASC';
      Filter := 'TABLE_NAME = ' + QuotedStr(sTableName) + ' AND ' +
                'INDEX_NAME = ' + QuotedStr(sIndexName);
      Filtered := True;
      First;
      sTmpIndexFields := '';
      while not Eof do
      begin
        // IF check unique state
        if bCheckUnique then
        begin
          if FieldByName('UNIQUE').AsBoolean = bUnique then
            bPassedUniqueCheck := True
          else
            Break;  // no further testing
        end;
        // Collect columns
        sTmpIndexFields := sTmpIndexFields + FieldByName('COLUMN_NAME').AsString;
        Next;
        if not Eof then
        begin
          sTmpIndexFields := sTmpIndexFields + ',';
        end;
        Next;
      end;
      Result := bPassedUniqueCheck;
      // IF check indexfields
      if bCheckIndexedFields then
      begin
        Result := Result and SameText(sTmpIndexFields, sIndexedFields);
      end;
    end;
  finally
    aAdoDataSet.Free;
  end;
end;

function TSrvDatabase.Upgrade241: Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add(Format('ALTER TABLE FFAMILY ALTER COLUMN PARENTEXT VARCHAR(%d)', [c241_parentext]));
        try
          ExecSQL;
        except
        end;
        // close query before checking to force commit
        Close;
        // check here!
        Result := FieldExists('FFAMILY', 'PARENTEXT', ftString, c241_parentext);
      end;
    finally
      aQry.Free;
    end;
  end;
end;

// fix indexes (Mantis #3573 and #3978)
function TSrvDatabase.Upgrade242: Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add('DROP INDEX groups.names');
        try
          ExecSQL;
        except
        end;
        SQL.Clear;
        SQL.Add('CREATE INDEX [names] ON [dbo].[groups]([name]) ON [PRIMARY]');
        try
          ExecSQL;
        except
        end;
        SQL.Clear;
        SQL.Add('DROP INDEX pjgroups.projectid');
        try
          ExecSQL;
        except
        end;
        SQL.Clear;
        SQL.Add('CREATE INDEX [projectid] ON [dbo].[pjgroups]([projectid]) ON [PRIMARY]');
        try
          ExecSQL;
        except
        end;
      end;
      Result := IndexExists('groups', 'names', 'name', False, True, True) and
        IndexExists('pjgroups', 'projectid', 'projectid', False, True, True);
    finally
      aQry.Free;
    end;
  end;
end;

{$IFDEF BRANCHOBJECTS}
function TSrvDatabase.Upgrade250: Boolean;
begin
  result := true;
end;
{$ENDIF BRANCHOBJECTS}

constructor TSrvDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoginPrompt := False;
  FParams := TStringList.Create;
  BeforeConnect := BeforeConnection;
end;

destructor TSrvDatabase.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

constructor TSrvQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FADOParameterList := TObjectList.Create;
end;

destructor TSrvQuery.Destroy;
begin
  FADOParameterList.Free;
  inherited Destroy;
end;

function TSrvQuery.ParamByName(const Value: string): TADOParameter;
var
  I, J, K, Cnt: Integer;
  FoundedParameters: TList;
  ADOParam: TADOParameter;
begin
  //Result := TADOParameter(Parameters.ParamByName(Value));
  Result := nil;
  FoundedParameters := TList.Create;
  try
    for I := 0 to Pred(Parameters.Count) do
      if SameText(Parameters[I].Name, Value) then
        FoundedParameters.Add(Parameters[I]);
    for I := 0 to Pred(FADOParameterList.Count) do
    begin
      ADOParam := TADOParameter(FADOParameterList[I]);
      if ADOParam.Count = FoundedParameters.Count then
      begin
        Cnt := 0;
        for J := 0 to Pred(FoundedParameters.Count) do
          for K := 0 to Pred(ADOParam.Count) do
            if ADOParam[K] = FoundedParameters[J] then
            begin
              Inc(Cnt);
              Break;
            end;
        if Cnt = FoundedParameters.Count then
        begin
          Result := ADOParam;
          Break;
        end;
      end;
    end;
    if not Assigned(Result) then
    begin
      FADOParameterList.Add(TADOParameter.Create);
      Result := TADOParameter(FADOParameterList.Last);
      for I := 0 to Pred(FoundedParameters.Count) do
        Result.AddParameter(FoundedParameters[I]);
    end;
  finally
    FoundedParameters.Free;
  end;
end;

function TSrvQuery.GetConnection: TADOConnection;
begin
  Result := Connection;
end;
{$ENDIF MSSQLSRV}

//=== MySql server =============================================================

{$IFDEF MYSQLSRV}
//-- LB
constructor TSrvDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Protocol := 'mysql';
  Database := 'vcs';
//test
  HostName := 'LocalHost';
//end
  LoginPrompt := False;
end;

procedure TSrvDatabase.AcquireConnection;
var
  MustReconnect: Boolean;
begin
  MustReconnect := False;
  try
    if (not Connected) or (not Ping) then
      MustReconnect := True;
  except
    MustReconnect := True;
  end;
  if MustReconnect then
  begin
    Disconnect;
    Connect;
  end;
end;

// Rename mdbugs.recordnr => mdbugs.recordid
// Drop Index from MySQL Beta
function TSrvDatabase.Upgrade240: Boolean;
var
  aQry: TSrvQuery;
begin
  //USc 14.03.2005 as of now we do the default 240 upgrade here to avoid #2699
  //Result := inherited Upgrade240;
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add('CREATE INDEX i_vcslog_tstamp ON vcslog(tstamp)');
        try
          ExecSQL;
        except
        end;
        // close query before checking to force commit
        Close;
        // check here!
        Result := IndexExists('vcslog', 'i_vcslog_tstamp', 'tstamp');
      end;
    finally
      aQry.Free;
    end;
  end;
  if Result then
  begin
    if Connected then
    begin
      aQry := TSrvQuery.Create(nil);
      try
        aQry.DatabaseName := TSrvDatabase(Self).DBPath;
        with aQry do
        begin
          SQL.Clear;
          SQL.Add('ALTER TABLE mdbugs CHANGE recordnr recordid int(11) NOT NULL auto_increment');
          try
            ExecSQL;
          except
          end;
          // changes for FreeVCS MySQL beta1
          SQL.Clear;
          // use standard syntax (DROP INDEX .. only in MySQL > 3.22 working)
          SQL.Add('ALTER TABLE groups DROP i_groups_names');
          try
            ExecSQL;
          except
          end;
          SQL.Clear;
          SQL.Add('ALTER TABLE groups ADD INDEX i_groups_names(name)');
          try
            ExecSQL;
          except
          end;
          SQL.Clear;
          SQL.Add('ALTER TABLE pjgroups DROP i_pjgroups_project');
          try
            ExecSQL;
          except
          end;
        end;
        Result := ( FieldExists('mdbugs', 'recordid') and
                    IndexExists('groups', 'i_groups_names', 'name', False, True, True) and
                    (not IndexExists('pjgroups', 'i_pjgroups_project', ''))
                  );
      finally
        aQry.Free;
      end;
    end;
  end;
end;

// Alter ffamily.parentext
function TSrvDatabase.Upgrade241: Boolean;
var
  aQry: TSrvQuery;
begin
  Result := False;
  if Connected then
  begin
    aQry := TSrvQuery.Create(nil);
    try
      aQry.DatabaseName := TSrvDatabase(Self).DBPath;
      with aQry do
      begin
        SQL.Clear;
        SQL.Add(Format('ALTER TABLE ffamily CHANGE parentext parentext varchar(%d)', [c241_parentext]));
        try
          ExecSQL;
        except
        end;
      end;
      Result := FieldExists('ffamily', 'parentext', ftString, c241_parentext);
    finally
      aQry.Free;
    end;
  end;
end;

function TSrvDatabase.GetDBPath: TZConnection;
begin
  Result := Self;
end;

function TSrvDatabase.FieldExists(const sTableName, sFieldName: string;
  ADataType: TFieldType = ftUnknown; ASize: Integer = 0): Boolean;
var
  ii: Integer;
  aSrvQry: TSrvQuery;
begin
  Result := False;
  aSrvQry := TSrvQuery.Create(nil);
  try
    aSrvQry.DatabaseName := Self;
    with aSrvQry do
    begin
      SQL.Clear;
      SQL.Add(Format('SELECT * FROM %s WHERE -1=1', [sTableName]));
      try
        Open;
        for ii := 0 to Pred(FieldDefs.Count) do
        begin
          if SameText(FieldDefs[ii].Name, sFieldName) then
          begin
            Result := (ADataType = ftUnknown) or ((FieldDefs[ii].DataType = ADataType) and
              ((ADataType <> ftString) or (FieldDefs[ii].Size = ASize)));
            Break;
          end;
        end;
      except
      end;
    end;
  finally
    aSrvQry.Free;
  end;
end;

function TSrvDatabase.IndexExists ( const sTableName: string
                                  ; const sIndexName: string
                                  ; const sIndexedFields: string
                                  ; const bUnique: Boolean = False
                                  ; const bCheckIndexedFields: Boolean = False
                                  ; const bCheckUnique: Boolean = False
                                  ): Boolean;
var
  ii: Integer;
  {$IFNDEF ZEOS54}
  FoundIndex: Boolean;
  IndexUnique: Boolean;
  IndexFieldList: TStringList;
  IndexFieldStr: string;
  {$ENDIF ~ZEOS54}
  aSrvQry: TSrvQuery;
begin
  Result := False;
  aSrvQry := TSrvQuery.Create(nil);
  try
    aSrvQry.DatabaseName := Self;
    with aSrvQry do
    begin
      SQL.Clear;
      {$IFDEF ZEOS54}
      SQL.Add(Format('select * from %s where -1=1', [sTableName]));
      try
        Open;
        for ii := 0 to Pred(IndexDefs.Count) do
        begin
          // If Index found
          if SameText(IndexDefs[ii].Name, sIndexName) then
          begin
            Result := True;
            if bCheckUnique then
            begin
              if bUnique then
                Result := (ixUnique in IndexDefs[ii].Options)
              else
                Result := not (ixUnique in IndexDefs[ii].Options);
            end;
            if (bCheckIndexedFields and Result) then
            begin
              Result := SameText(IndexDefs[ii].Fields, sIndexedFields);
            end;
            Break;
          end;
        end;
      except
      end;
      {$ELSE}
      SQL.Add(Format('show keys from %s', [sTableName]));
      try
        Open;
        First;
        IndexUnique := False;
        FoundIndex := False;
        IndexFieldList := TStringList.Create;
        try
          while not Eof do
          begin
            if SameText(FieldByName('Key_name').AsString, sIndexName) then
            begin
              FoundIndex := True;
              IndexUnique := FieldByName('Non_unique').AsInteger <> 1;
              IndexFieldList.Add(FieldByName('Column_name').AsString);
            end;
            Next;
          end;
          if FoundIndex then
          begin
            Result := True;
            if bCheckUnique then
              Result := bUnique = IndexUnique;
            if (bCheckIndexedFields and Result) then
            begin
              IndexFieldStr := '';
              for ii := 0 to Pred(IndexFieldList.Count) do
              begin
                if ii > 0 then
                  IndexFieldStr := IndexFieldStr + ',';
                IndexFieldStr := IndexFieldStr + IndexFieldList[ii];
              end;
              Result := SameText(IndexFieldStr, sIndexedFields);
            end;
          end;
        finally
          IndexFieldList.Free;
        end;
      except
      end;
      {$ENDIF ZEOS54}
    end;
  finally
    aSrvQry.Free;
  end;
end;

function TSrvQuery.GetDatabaseName: TZConnection;
begin
  Result := Connection;
end;

procedure TSrvQuery.SetDatabaseName(NewValue: TZConnection);
begin
  Connection := NewValue;
end;
{$ENDIF MYSQLSRV}

//=== ADO Connection ===========================================================

{$IFDEF ADOSRV}
//-- PL
procedure TADOParameter.Clear;
begin
  Value := Null;
end;

function TADOParameter.GetAsIntValue: Integer;
begin
  Result := Value;
end;

function TADOParameter.GetAsStringValue: string;
begin
  Result := Value;
end;

function TADOParameter.GetAsDateTimeValue: TDateTime;
begin
  Result := Value;
end;

procedure TADOParameter.SetAsIntValue(NewValue: Integer);
begin
  DataType := ftInteger;
  Value := NewValue;
end;

procedure TADOParameter.SetAsStringValue(NewValue: string);
begin
  DataType := ftString;
  if NewValue = '' then
    Value := Null
  else
    Value := NewValue;
end;

procedure TADOParameter.SetAsDateTimeValue(NewValue: TDateTime);
begin
  DataType := ftDateTime;
  Value := NewValue;
end;

procedure TSrvDatabase.BeforeConnection(Sender: TObject);
begin
  ConnectionString := 'Provider=' + Params.Values['Provider'] + ';Password=' + Params.Values['password'] +
                      ';User ID=' + Params.Values['user ID'] +
                      ';Data Source=' + Params.Values['Data Source'] +
                      ';Catalog Library List=???' ;
end;

function TSrvDatabase.GetDBPath: TADOConnection;
begin
  Result := Self;
end;

constructor TSrvDatabase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LoginPrompt := False;
  FParams := TStringList.Create;
  BeforeConnect := BeforeConnection;
end;

destructor TSrvDatabase.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TSrvQuery.OpenCursor(InfoQuery: Boolean);
begin
  if not Prepared then
    Prepared := True;
  inherited;
end;

function TSrvQuery.ParamByName(const Value: string): TADOParameter;
begin
  Result := TADOParameter(Parameters.ParamByName(Value));
end;

function TSrvQuery.GetConnection: TADOConnection;
begin
  Result := Connection;
end;
{$ENDIF ADOSRV}

end.

