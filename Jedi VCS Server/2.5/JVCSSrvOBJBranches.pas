(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSSrvOBJBranches.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
ToDo
- enter functions into client code generator
- adjust vcslog deletion in BRANCH_REMOVE_MODULE?
- add check for duplicate files to ADD_BRANCH if path substitution is enabled
-----------------------------------------------------------------------------

Unit history:

2004/12/11  USchuster - new unit
2005/04/17  USchuster - changed several statements to use brmodulelocation
                      - updated ADD_NEW_BRANCH and INITIAL_ADD_HEADBRANCH
2005/04/25  USchuster - brmodulelock and brmodulelocation -> brmodule
                      - fixed BRANCH_GET_REVISION_LIST_BY_ID
                      - added a lot of functions
2005/04/27  USchuster - added some more functions
2005/05/10  USchuster - (BRANCH_)RENAME_MODULE does now check if there is already a module
                        with the new name in the same directory (mantis #2941)
2005/06/12  USchuster - D6+ fix
2005/07/04  USchuster - finished BRANCH_COPY_REVISION, BRANCH_GET_BLANK_MODULE_LIST
                      - adjusted BRANCH_MERGE_VER_REV_NR for the case that the
                        revision is not a shared revision
                      - simplified project last access update and vcslog statement
                        in BRANCH_CHECKIN_MODULE
                      - fixed BRANCH_GET_REVISION_LIST and BRANCH_GET_REVISION_LIST_BY_ID
                        (pjmodule.branchid was left in where clause)
                      - some preparations in BRANCH_REMOVE_REVISION, BRANCH_REMOVE_VERSION
                      - added BRANCH_PURGE_PROJECT, BRANCH_LABEL_USED_BY, BRANCH_FAMILY_USED_BY,
                        BRANCH_BUGS_USED_BY, BRANCH_GET_DESERTED_MODULES, BRANCH_GET_LOG_ENTRIES,
                        BRANCH_GET_LOG_FILTER
                      - fixed BRANCH_GET_LABELS_BY_PROJECT(rvbranch was left)
2005/07/09  USchuster - BranchVCSLog is used now to create log entries(analogue to mantis #2631)
                      - simplified creation of insert statements
2005/07/10  USchuster - fix to compile without  S Q L D B M S
2005/07/11  USchuster - finished BRANCH_REMOVE_REVISION, BRANCH_REMOVE_VERSION
2005/09/04  USchuster - finished BRANCH_REMOVE_MODULE, BRANCH_MERGE_VER_REV_NR
2005/09/22  THuber     #3212 adjust AutoExpand Buffer for Blob transfer
                            (BRANCH_)GET_CHECKOUT_MODULE, (BRANCH_)GET_SINGLE_BLOB
                            added min buffer size to size calculation
2005/10/01  USchuster - improved blob access(reading) (mantis #3239)
2006/02/05  USchuster - fixed some problems which turned out through first tests
2006/02/12  USchuster - some more fixes
2006/04/16  USchuster - changes for branch specific crossref
2006/05/01  USchuster - changes for new session handling(solves mantis #1941) and
                        the selected branch will now be stored in the new Session
2006/06/18  USchuster - changes for branch specific vcslog
2006/06/28  USchuster - fixed GET_PROJECT_ID(selected branch got lost)
                      - renamed ADD_NEW_BRANCH to ADD_BRANCH
                      - added REMOVE_BRANCH and UPDATE_BRANCH
2006/07/21  USchuster - some fixes
2006/07/22  USchuster - fixes for UIB port
2006/11/08  USchuster - removed ADD_NEW_BRANCH
2007/02/11  USchuster - added MySQL max_allowed_packet check functions
                      - changes for new column rvbranch.origin
2007/06/16  USchuster - adjusted REMOVE_MODULE to write ProjectID and ModuleID into log if possible
                        and as of now if a module is removed completely the log entries "survive" (mantis #3639)
                      - adjusted order in GET_MODULE_HISTORY (mantis #4151)
2008/01/23  USchuster - moved TJVCSBranchServerObject.BranchVCSLog to
                        TFVCSServerObject in SrvCustomSrvOBJ.pas
2008/10/12  USchuster - increased performance of GET_VERSION_LIST and GET_LATEST_REVISIONS
                        with new table latestrevision (depends on define USELATESTREVISION)
2008/10/14  USchuster - params for Oracle
                      - another "needs update" solution since other function also update the
                        archive timestamp, but do not update the latest revision + state per branch
2008/12/06  USchuster - fixed compilation without USELATESTREVISION
2008/12/23  USchuster - added GET_BRANCHPOINT_REVISIONS (modified version of GET_LATEST_REVISIONS)
2009/04/04  USchuster - added Tag Only to ADD_BRANCH (does add only the latest revision to the branch)
2009/04/10  USchuster - removed GetInsertKeyFieldValueStr stuff here, because the branch sequences/generators
                        has been added to GetInsertKeyFieldValueStr in SrvDBDef.pas
2009/09/05  USchuster - fixed duplicate revision problem for checked out modules (Mantis #779)
                        (old solution was unfortunately added one "end" to early)
2009/09/06  USchuster - added path substitution options to ADD_BRANCH
2009/12/13  USchuster - changes for opt-out of using table latestrevision for performance increase
2009/12/14  USchuster - increased performance of GET_BLANK_MODULE_LIST

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     #5066 support for  F l a s h F i l e r  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version
                     special Oracle J O I N  - Syntax was removed we now only
                     support the A N S I - J o i n Syntax and therefore removed
                     this compiler setting.
2010/01/17  USchuster - published UpdateLatestRevisions for use in LIST_FILES function
                      - GetLatestRevisionState for later use in LIST_FILES function
2010/01/22  THuber   #5099 Transaction hack for Firebird, some methods need to run in one transaction!
                     Eg: see UpdateLatestRevisions(...)
                     Normally if we have two queries running in the same method, otherwise we use AutoCommit
2010/01/24  USchuster - finished REMOVE_BRANCH
2010/01/27  THuber   #5099 fix in SEARCH_MODULES for Firebird port  

-----------------------------------------------------------------------------*)

{
changed tables:
  pjmodule
    added branchid
  pjref
    added branchid
  vcslog
    added branchid
additional tables:
  branches
    branchid
    parentbranchid
    name
    description
    created_by
    created_in
  rvbranch
    recordid
    branchid
    revisionid
  /*
  brmodulelock
    recordid
    branchid
    moduleid
    userid
    timestamp
    -> ToDo: remove from modules ReadOnly, LockTStamp und UserID
  brmodulelocation
    recordid
    branchid
    moduleid
    name
    path
  */
  brmodule
    recordid
    branchid
    moduleid
    name
    path
    readonly
    userid
    locktstamp
problems/ToDo:
  *DONE*- modules path and name should be branch specific
  *DONE*- vcslog needs branchid
    *DONE*- solve mantis #2631 before
    *DONE*- needs BRANCH_REMOVE_LOG_ENTRIES
  - checkout comment on shared revisions
  - restrictions: bugs, milestones, todo, labels, project groups,
    project user are not branch specific
  *DONE*- branch specific crossref
  - projects branch specific(optional see concept)??
}
unit JVCSSrvOBJBranches;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  {$IFDEF USELATESTREVISION}
  Contnrs,
  {$ENDIF USELATESTREVISION}
  SysUtils, Classes, ApSrvCli, RBroker, RFormat, SrvConst, SrvCustomSrvOBJ,
  SrvUDRec, SrvDBDef, SrvAccessDef, DB, IniFiles, VCSCommon, ServerCryptInt;

type
  TBranchClientWSocket = class(TClientWSocket)
  private
    FBranchID: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    property BranchID: Integer read FBranchID write FBranchID;
  end;

procedure AddBranchServerObjects(ARequestBroker: TRequestBroker);
function GetLatestRevisionState(ABranchID: Integer): Int64;
procedure UpdateLatestRevisions(UData: TUserDataRecord; ABranchID: Integer);

implementation

uses
  SrvObjModules, JclStrings, JVCSSessionList;

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

type
  EJVCSNotImplemented = class(Exception)
  public
    constructor Create(const Msg: string);
  end;

constructor EJVCSNotImplemented.Create(const Msg: string);
begin
  inherited CreateFmt('Function %s is not implemented yet!', [Msg]);
end;  

constructor TBranchClientWSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBranchID := 1;
end;

type
  TJVCSBranchServerObject = class(TFVCSServerObject)
  {
  private
    function GetSelectedBranchID: Integer;
    procedure SetSelectedBranchID(ABranchID: Integer);
  }
  public
    function BranchModuleVersionRevisionExists(ABranchID, AModuleID, AVersion, ARevision: Integer): Boolean;
    function GetRevisionBranchCount(ARevisionID: Integer): Integer;
    function IsSharedRevision(ARevisionID: Integer): Boolean;
    function IsOnlyRevisionOfBranch(ARevisionID, ABranchID: Integer): Boolean;    
    procedure UpdateProjectLastAccess(ABranchID, AProjectID, AUserID: Integer);
    //property SelectedBranchID: Integer read GetSelectedBranchID write SetSelectedBranchID;
  end;

  TServerObjectSELECT_BRANCH = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectADD_BRANCH = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectREMOVE_BRANCH = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectUPDATE_BRANCH = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectINITIAL_ADD_HEADBRANCH = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_BRANCH_LIST = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_LATEST_REVISIONS = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_VERSION_LIST = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_BLANK_MODULE_LIST = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_MODULE_HISTORY = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_CHECKIN_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_REVISION_LIST_BY_ID = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_MODULE_REVISIONS = class(TFVCSServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_BLOB_STATUS = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_REVISION_STATUS = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_CHECKOUT_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_CHECKOUT_ONLY_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_UNDO_CHECKOUT_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_RENAME_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_PROJECT_MODULE_LIST = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;
  
  TServerObjectBRANCH_MOVE_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_SHARED_BY = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_SHARED_MODULES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_LABELS_BY_PROJECT = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_ADD_NEW_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_MODULE_NAME = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_MODULE_ID = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_IS_MEMBER_OF_PROJECT = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_REMOVE_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_REVISION_LIST = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_LOCKED_MODULES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_SEARCH_MODULES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_REVISION_LIST_BY_VERSION = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_REMOVE_REVISION = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_REMOVE_VERSION = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_HIDE_UNHIDE_MODULE = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_COPY_REVISION = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_MERGE_VER_REV_NR = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_VERSION_REVISION = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_ROLLBACK_REVISIONS = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_PURGE_PROJECT = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_LABEL_USED_BY = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_FAMILY_USED_BY = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_BUGS_USED_BY = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_DESERTED_MODULES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_LOG_ENTRIES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_LOG_FILTER = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_REMOVE_LOG_ENTRIES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_ADD_UPDATE_PROJECT_REFERENCES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_GET_PROJECT_REFERENCES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectBRANCH_REMOVE_PROJECT_REFERENCES = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectLOGIN = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectLOGOUT = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectWHOAMI = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_USERLIST = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_PROJECT_ID = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_BRANCHPOINT_REVISIONS = class(TJVCSBranchServerObject)
  public
    procedure Execute; override;
  end;

  TServerObjectGET_LATEST_REVISIONS = class(TServerObjectBRANCH_GET_LATEST_REVISIONS);
  TServerObjectGET_VERSION_LIST = class(TServerObjectBRANCH_GET_VERSION_LIST);
  TServerObjectGET_BLANK_MODULE_LIST = class(TServerObjectBRANCH_GET_BLANK_MODULE_LIST);
  TServerObjectGET_MODULE_HISTORY = class(TServerObjectBRANCH_GET_MODULE_HISTORY);
  TServerObjectCHECKIN_MODULE = class(TServerObjectBRANCH_CHECKIN_MODULE);
  TServerObjectGET_REVISION_LIST_BY_ID = class(TServerObjectBRANCH_GET_REVISION_LIST_BY_ID);
  TServerObjectOLD_GET_MODULE_HISTORY = class(SrvObjModules.TServerObjectGET_MODULE_HISTORY);
  TServerObjectGET_BLOB_STATUS = class(TServerObjectBRANCH_GET_BLOB_STATUS);
  TServerObjectGET_REVISION_STATUS = class(TServerObjectBRANCH_GET_REVISION_STATUS);
  TServerObjectGET_CHECKOUT_MODULE = class(TServerObjectBRANCH_GET_CHECKOUT_MODULE);
  TServerObjectCHECKOUT_ONLY_MODULE = class(TServerObjectBRANCH_CHECKOUT_ONLY_MODULE);
  TServerObjectUNDO_CHECKOUT_MODULE = class(TServerObjectBRANCH_UNDO_CHECKOUT_MODULE);
  TServerObjectRENAME_MODULE = class(TServerObjectBRANCH_RENAME_MODULE);
  TServerObjectGET_PROJECT_MODULE_LIST = class(TServerObjectBRANCH_GET_PROJECT_MODULE_LIST);
  TServerObjectMOVE_MODULE = class(TServerObjectBRANCH_MOVE_MODULE);
  TServerObjectGET_SHARED_BY = class(TServerObjectBRANCH_GET_SHARED_BY);
  TServerObjectGET_SHARED_MODULES = class(TServerObjectBRANCH_GET_SHARED_MODULES);
  TServerObjectGET_LABELS_BY_PROJECT = class(TServerObjectBRANCH_GET_LABELS_BY_PROJECT);
  TServerObjectADD_NEW_MODULE = class(TServerObjectBRANCH_ADD_NEW_MODULE);
  TServerObjectGET_MODULE_NAME = class(TServerObjectBRANCH_GET_MODULE_NAME);
  TServerObjectGET_MODULE_ID = class(TServerObjectBRANCH_GET_MODULE_ID);
  TServerObjectIS_MEMBER_OF_PROJECT = class(TServerObjectBRANCH_IS_MEMBER_OF_PROJECT);
  TServerObjectREMOVE_MODULE = class(TServerObjectBRANCH_REMOVE_MODULE);
  TServerObjectGET_REVISION_LIST = class(TServerObjectBRANCH_GET_REVISION_LIST);
  TServerObjectGET_LOCKED_MODULES = class(TServerObjectBRANCH_GET_LOCKED_MODULES);
  TServerObjectSEARCH_MODULES = class(TServerObjectBRANCH_SEARCH_MODULES);
  TServerObjectGET_REVISION_LIST_BY_VERSION = class(TServerObjectBRANCH_GET_REVISION_LIST_BY_VERSION);
  TServerObjectREMOVE_REVISION = class(TServerObjectBRANCH_REMOVE_REVISION);
  TServerObjectREMOVE_VERSION = class(TServerObjectBRANCH_REMOVE_VERSION);
  TServerObjectHIDE_UNHIDE_MODULE = class(TServerObjectBRANCH_HIDE_UNHIDE_MODULE);
  TServerObjectCOPY_REVISION = class(TServerObjectBRANCH_COPY_REVISION);
  TServerObjectMERGE_VER_REV_NR = class(TServerObjectBRANCH_MERGE_VER_REV_NR);
  TServerObjectGET_VERSION_REVISION = class(TServerObjectBRANCH_GET_VERSION_REVISION);
  TServerObjectGET_ROLLBACK_REVISIONS = class(TServerObjectBRANCH_GET_ROLLBACK_REVISIONS);
  TServerObjectPURGE_PROJECT = class(TServerObjectBRANCH_PURGE_PROJECT);
  TServerObjectLABEL_USED_BY = class(TServerObjectBRANCH_LABEL_USED_BY);
  TServerObjectFAMILY_USED_BY = class(TServerObjectBRANCH_FAMILY_USED_BY);
  TServerObjectBUGS_USED_BY = class(TServerObjectBRANCH_BUGS_USED_BY);
  TServerObjectGET_DESERTED_MODULES = class(TServerObjectBRANCH_GET_DESERTED_MODULES);
  TServerObjectGET_LOG_ENTRIES = class(TServerObjectBRANCH_GET_LOG_ENTRIES);
  TServerObjectGET_LOG_FILTER = class(TServerObjectBRANCH_GET_LOG_FILTER);
  TServerObjectREMOVE_LOG_ENTRIES = class(TServerObjectBRANCH_REMOVE_LOG_ENTRIES);
  TServerObjectADD_UPDATE_PROJECT_REFERENCES = class(TServerObjectBRANCH_ADD_UPDATE_PROJECT_REFERENCES);
  TServerObjectGET_PROJECT_REFERENCES = class(TServerObjectBRANCH_GET_PROJECT_REFERENCES);
  TServerObjectREMOVE_PROJECT_REFERENCES = class(TServerObjectBRANCH_REMOVE_PROJECT_REFERENCES);

procedure AddBranchServerObjects(ARequestBroker: TRequestBroker);
begin
  with ARequestBroker do
  begin
    AddServerObject(TServerObjectSELECT_BRANCH);
    AddServerObject(TServerObjectADD_BRANCH);
    AddServerObject(TServerObjectREMOVE_BRANCH);    
    AddServerObject(TServerObjectUPDATE_BRANCH);
    AddServerObject(TServerObjectINITIAL_ADD_HEADBRANCH);
    AddServerObject(TServerObjectGET_BRANCH_LIST);
    AddServerObject(TServerObjectBRANCH_GET_LATEST_REVISIONS);
    AddServerObject(TServerObjectBRANCH_GET_VERSION_LIST);
    AddServerObject(TServerObjectBRANCH_GET_BLANK_MODULE_LIST);
    AddServerObject(TServerObjectBRANCH_GET_MODULE_HISTORY);
    AddServerObject(TServerObjectBRANCH_CHECKIN_MODULE);
    AddServerObject(TServerObjectBRANCH_GET_REVISION_LIST_BY_ID);

    AddServerObject(TServerObjectBRANCH_GET_MODULE_REVISIONS);

    AddServerObject(TServerObjectBRANCH_GET_BLOB_STATUS);

    AddServerObject(TServerObjectBRANCH_GET_REVISION_STATUS);
    AddServerObject(TServerObjectBRANCH_GET_CHECKOUT_MODULE);
    AddServerObject(TServerObjectBRANCH_CHECKOUT_ONLY_MODULE);
    AddServerObject(TServerObjectBRANCH_UNDO_CHECKOUT_MODULE);
    AddServerObject(TServerObjectBRANCH_RENAME_MODULE);
    AddServerObject(TServerObjectBRANCH_GET_PROJECT_MODULE_LIST);
    AddServerObject(TServerObjectBRANCH_MOVE_MODULE);
    AddServerObject(TServerObjectBRANCH_GET_SHARED_BY);
    AddServerObject(TServerObjectBRANCH_GET_SHARED_MODULES);
    AddServerObject(TServerObjectBRANCH_GET_LABELS_BY_PROJECT);
    AddServerObject(TServerObjectBRANCH_ADD_NEW_MODULE);
    AddServerObject(TServerObjectBRANCH_GET_MODULE_NAME);
    AddServerObject(TServerObjectBRANCH_GET_MODULE_ID);
    AddServerObject(TServerObjectBRANCH_IS_MEMBER_OF_PROJECT);
    AddServerObject(TServerObjectBRANCH_REMOVE_MODULE);
    AddServerObject(TServerObjectBRANCH_GET_REVISION_LIST);
    AddServerObject(TServerObjectBRANCH_GET_LOCKED_MODULES);
    AddServerObject(TServerObjectBRANCH_SEARCH_MODULES);
    AddServerObject(TServerObjectBRANCH_GET_REVISION_LIST_BY_VERSION);
    AddServerObject(TServerObjectBRANCH_REMOVE_REVISION);
    AddServerObject(TServerObjectBRANCH_REMOVE_VERSION);
    AddServerObject(TServerObjectBRANCH_HIDE_UNHIDE_MODULE);
    AddServerObject(TServerObjectBRANCH_COPY_REVISION);
    AddServerObject(TServerObjectBRANCH_MERGE_VER_REV_NR);
    AddServerObject(TServerObjectBRANCH_GET_VERSION_REVISION);
    AddServerObject(TServerObjectBRANCH_GET_ROLLBACK_REVISIONS);
    AddServerObject(TServerObjectBRANCH_PURGE_PROJECT);
    AddServerObject(TServerObjectBRANCH_LABEL_USED_BY);
    AddServerObject(TServerObjectBRANCH_FAMILY_USED_BY);
    AddServerObject(TServerObjectBRANCH_BUGS_USED_BY);
    AddServerObject(TServerObjectBRANCH_GET_DESERTED_MODULES);
    AddServerObject(TServerObjectBRANCH_GET_LOG_ENTRIES);
    AddServerObject(TServerObjectBRANCH_GET_LOG_FILTER);
    AddServerObject(TServerObjectBRANCH_REMOVE_LOG_ENTRIES);
    AddServerObject(TServerObjectBRANCH_ADD_UPDATE_PROJECT_REFERENCES);
    AddServerObject(TServerObjectBRANCH_GET_PROJECT_REFERENCES);
    AddServerObject(TServerObjectBRANCH_REMOVE_PROJECT_REFERENCES);

    AddServerObject(TServerObjectGET_LATEST_REVISIONS);
    AddServerObject(TServerObjectGET_VERSION_LIST);
    AddServerObject(TServerObjectGET_BLANK_MODULE_LIST);
    AddServerObject(TServerObjectGET_MODULE_HISTORY);
    AddServerObject(TServerObjectCHECKIN_MODULE);
    AddServerObject(TServerObjectGET_REVISION_LIST_BY_ID);

    AddServerObject(TServerObjectOLD_GET_MODULE_HISTORY);

    AddServerObject(TServerObjectGET_BLOB_STATUS);

    AddServerObject(TServerObjectGET_REVISION_STATUS);
    AddServerObject(TServerObjectGET_CHECKOUT_MODULE);
    AddServerObject(TServerObjectCHECKOUT_ONLY_MODULE);
    AddServerObject(TServerObjectUNDO_CHECKOUT_MODULE);
    AddServerObject(TServerObjectRENAME_MODULE);
    AddServerObject(TServerObjectGET_PROJECT_MODULE_LIST);
    AddServerObject(TServerObjectMOVE_MODULE);
    AddServerObject(TServerObjectGET_SHARED_BY);
    AddServerObject(TServerObjectGET_SHARED_MODULES);
    AddServerObject(TServerObjectGET_LABELS_BY_PROJECT);
    AddServerObject(TServerObjectADD_NEW_MODULE);
    AddServerObject(TServerObjectGET_MODULE_NAME);
    AddServerObject(TServerObjectGET_MODULE_ID);
    AddServerObject(TServerObjectIS_MEMBER_OF_PROJECT);
    AddServerObject(TServerObjectREMOVE_MODULE);
    AddServerObject(TServerObjectGET_REVISION_LIST);
    AddServerObject(TServerObjectGET_LOCKED_MODULES);
    AddServerObject(TServerObjectSEARCH_MODULES);
    AddServerObject(TServerObjectGET_REVISION_LIST_BY_VERSION);
    AddServerObject(TServerObjectREMOVE_REVISION);
    AddServerObject(TServerObjectREMOVE_VERSION);
    AddServerObject(TServerObjectHIDE_UNHIDE_MODULE);
    AddServerObject(TServerObjectCOPY_REVISION);
    AddServerObject(TServerObjectMERGE_VER_REV_NR);
    AddServerObject(TServerObjectGET_VERSION_REVISION);
    AddServerObject(TServerObjectGET_ROLLBACK_REVISIONS);
    AddServerObject(TServerObjectPURGE_PROJECT);
    AddServerObject(TServerObjectLABEL_USED_BY);
    AddServerObject(TServerObjectFAMILY_USED_BY);
    AddServerObject(TServerObjectBUGS_USED_BY);
    AddServerObject(TServerObjectGET_DESERTED_MODULES);
    AddServerObject(TServerObjectGET_LOG_ENTRIES);
    AddServerObject(TServerObjectGET_LOG_FILTER);
    AddServerObject(TServerObjectREMOVE_LOG_ENTRIES);
    AddServerObject(TServerObjectADD_UPDATE_PROJECT_REFERENCES);
    AddServerObject(TServerObjectGET_PROJECT_REFERENCES);
    AddServerObject(TServerObjectREMOVE_PROJECT_REFERENCES);

    AddServerObject(TServerObjectLOGIN);
    AddServerObject(TServerObjectLOGOUT);
    AddServerObject(TServerObjectWHOAMI);
    AddServerObject(TServerObjectGET_USERLIST);
    AddServerObject(TServerObjectGET_PROJECT_ID);

    AddServerObject(TServerObjectGET_BRANCHPOINT_REVISIONS);
  end;
end;

procedure CopyProjectModules(var AUserData: TUserDataRecord; AParentBranch, ANewBranch: Integer);
var
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select projectid, moduleid, hidden from pjmodule');
      SQL.Add('where branchid = :branchid');
      ParamByName('branchid').AsInteger := AParentBranch;
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            if GetInsertKeyFieldValueStr('pjmodule') <> '' then
              SQL.Add('INSERT INTO pjmodule(recordid, branchid, projectid, moduleid, hidden)')
            else
              SQL.Add('INSERT INTO pjmodule(branchid, projectid, moduleid, hidden)');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjmodule')]));
            SQL.Add(':branchid, :projectid, :moduleid, :hidden)');
          end;
          while not Eof do
          begin
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('projectid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ParamByName('moduleid').AsInteger := Fields[1].AsInteger;
            FDestQuery.ParamByName('hidden').AsString := Fields[2].AsString;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure CopyRevisions(AUserData: TUserDataRecord; AParentBranch, ANewBranch: Integer; ATagOnly: Boolean);
var
  FSourceQuery, FDestQuery: TSrvQuery;
  LastModuleID: Integer;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      if ATagOnly then
      begin
        {$IFDEF USELATESTREVISION}
        if AUserData.UseLatestRevisions then
        begin
          SQL.Add('select revisionid from latestrevision');
          SQL.Add('where branchid = :branchid');
        end
        else
        begin
        {$ENDIF USELATESTREVISION}
        SQL.Add('SELECT revision.moduleid, revision.revisionid FROM revision, rvbranch');
        SQL.Add('WHERE rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('ORDER BY revision.moduleid, revision.version desc, revision.revision desc');
        {$IFDEF USELATESTREVISION}
        end;
        {$ENDIF USELATESTREVISION}
      end
      else
      begin
        SQL.Add('select revisionid from rvbranch');
        SQL.Add('where branchid = :branchid');
      end;
      ParamByName('branchid').AsInteger := AParentBranch;
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            SQL.Add('INSERT INTO rvbranch');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('rvbranch')]));
            SQL.Add(':branchid, :revisionid, ''p'')');
          end;
          if ATagOnly then
          begin
            LastModuleID := -1;
            while not Eof do
            begin
              {$IFDEF USELATESTREVISION}
              if AUserData.UseLatestRevisions then
              begin
                FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
                FDestQuery.ParamByName('revisionid').AsInteger := Fields[0].AsInteger;
                FDestQuery.ExecSQL;
              end
              else
              begin
              {$ENDIF USELATESTREVISION}
              if (LastModuleID = -1) or (Fields[0].AsInteger <> LastModuleID) then
              begin
                FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
                FDestQuery.ParamByName('revisionid').AsInteger := Fields[1].AsInteger;
                FDestQuery.ExecSQL;
                LastModuleID := Fields[0].AsInteger;
              end;
              {$IFDEF USELATESTREVISION}
              end;
              {$ENDIF USELATESTREVISION}
              Next;
            end;
          end
          else
          begin
            while not Eof do
            begin
              FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
              FDestQuery.ParamByName('revisionid').AsInteger := Fields[0].AsInteger;
              FDestQuery.ExecSQL;
              Next;
            end;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure CopyModuleLocation(AUserData: TUserDataRecord; AParentBranch, ANewBranch: Integer);
var
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select moduleid, name, path from brmodulelocation');
      SQL.Add('where branchid = :branchid');
      ParamByName('branchid').AsInteger := AParentBranch;
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            SQL.Add('INSERT INTO brmodulelocation');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('brmodulelocation')]));
            SQL.Add(':branchid, :moduleid, :name, :path)');
          end;
          while not Eof do
          begin
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('moduleid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ParamByName('name').AsString := Fields[1].AsString;
            FDestQuery.ParamByName('path').AsString := Fields[2].AsString;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure CopyBranchModule(AUserData: TUserDataRecord; AParentBranch, ANewBranch: Integer; ASubstitudeRootPath: Boolean;
  const AOldRootPath, ANewRootPath: string);
var
  ModulePath: string;
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select moduleid, name, path, userid from brmodule');
      SQL.Add('where branchid = :branchid');
      ParamByName('branchid').AsInteger := AParentBranch;
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            SQL.Add('INSERT INTO brmodule');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('brmodule')]));
            SQL.Add(':branchid, :moduleid, :name, :path, :readonly, :userid, :locktstamp)');
          end;
          while not Eof do
          begin
            ModulePath := Fields[2].AsString;
            if ASubstitudeRootPath and (Pos(AOldRootPath, ModulePath) = 1) then
            begin
              System.Delete(ModulePath, 1, Length(AOldRootPath));
              ModulePath := ANewRootPath + ModulePath;
            end;
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('moduleid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ParamByName('name').AsString := Fields[1].AsString;
            FDestQuery.ParamByName('path').AsString := ModulePath;
            FDestQuery.ParamByName('readonly').AsString := '0';
            FDestQuery.ParamByName('userid').AsInteger := Fields[3].AsInteger;
            {$IFDEF ORACLESRV}
            FDestQuery.ParamByName('locktstamp').DataType := ftDateTime;
            {$ENDIF ORACLESRV}
            FDestQuery.ParamByName('locktstamp').Clear;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure CopyProjectReferences(var AUserData: TUserDataRecord; AParentBranch, ANewBranch: Integer);
var
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select projectid, reference from pjref');
      SQL.Add('where branchid = :branchid');
      ParamByName('branchid').AsInteger := AParentBranch;
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            if GetInsertKeyFieldValueStr('pjref') <> '' then
              SQL.Add('INSERT INTO pjref(recordid, branchid, projectid, reference)')
            else
              SQL.Add('INSERT INTO pjref(branchid, projectid, reference)');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjref')]));
            SQL.Add(':branchid, :projectid, :reference)');
          end;
          while not Eof do
          begin
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('projectid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ParamByName('reference').AsInteger := Fields[1].AsInteger;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure FillProjectModules(var AUserData: TUserDataRecord; ANewBranch: Integer);
var
  FQuery: TSrvQuery;
begin
  FQuery := TSrvQuery.Create(nil);
  try
    FQuery.DatabaseName := AUserData.DBPath;
    with FQuery do
    begin
      SQL.Clear;
      SQL.Add('update pjmodule');
      SQL.Add('set branchid = :branchid');
      ParamByName('branchid').AsInteger := ANewBranch;
      ExecSQL;
    end;
  finally
    FQuery.Free;
  end;
end;

procedure CreateRevisions(AUserData: TUserDataRecord; ANewBranch: Integer);
var
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select revisionid from revision');
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            SQL.Add('INSERT INTO rvbranch');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('rvbranch')]));
            SQL.Add(':branchid, :revisionid, ''s'')');
          end;
          while not Eof do
          begin
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('revisionid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure CreateModuleLock(AUserData: TUserDataRecord; ANewBranch: Integer);
var
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select moduleid, userid, locktstamp from modules');
      SQL.Add('WHERE readonly = ''1''');
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            SQL.Add('INSERT INTO brmodulelock');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('brmodulelock')]));
            SQL.Add(':branchid, :moduleid, :userid, :locktstamp)');
          end;
          while not Eof do
          begin
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('moduleid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ParamByName('userid').AsInteger := Fields[1].AsInteger;
            FDestQuery.ParamByName('locktstamp').AsDateTime := Fields[2].AsDateTime;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure CreateModuleLocation(AUserData: TUserDataRecord; ANewBranch: Integer);
var
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select moduleid, name, path from modules');
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            SQL.Add('INSERT INTO brmodulelocation');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('brmodulelocation')]));
            SQL.Add(':branchid, :moduleid, :name, :path)');
          end;
          while not Eof do
          begin
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('moduleid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ParamByName('name').AsString := Fields[1].AsString;
            FDestQuery.ParamByName('path').AsString := Fields[2].AsString;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

procedure CreateBranchModule(AUserData: TUserDataRecord; ANewBranch: Integer);
var
  FSourceQuery, FDestQuery: TSrvQuery;
begin
  FSourceQuery := TSrvQuery.Create(nil);
  try
    FSourceQuery.DatabaseName := AUserData.DBPath;
    FSourceQuery.SetTransactionNoAuto;
    with FSourceQuery do
    begin
      SQL.Clear;
      SQL.Add('select moduleid, name, path, readonly, userid, locktstamp from modules');
      Open;

      if not (Eof and Bof) then
      begin
        FDestQuery := TSrvQuery.Create(nil);
        try
          FDestQuery.DatabaseName := AUserData.DBPath;
          FDestQuery.SetTransaction(FSourceQuery);
          with FDestQuery do
          begin
            SQL.Add('INSERT INTO brmodule');
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('brmodule')]));
            SQL.Add(':branchid, :moduleid, :name, :path, :readonly, :userid, :locktstamp)');
          end;
          while not Eof do
          begin
            FDestQuery.ParamByName('branchid').AsInteger := ANewBranch;
            FDestQuery.ParamByName('moduleid').AsInteger := Fields[0].AsInteger;
            FDestQuery.ParamByName('name').AsString := Fields[1].AsString;
            FDestQuery.ParamByName('path').AsString := Fields[2].AsString;
            FDestQuery.ParamByName('readonly').AsString := Fields[3].AsString;
            FDestQuery.ParamByName('userid').AsString := Fields[4].AsString;
            FDestQuery.ParamByName('locktstamp').AsDateTime := Fields[5].AsDateTime;
            FDestQuery.ExecSQL;
            Next;
          end;
        finally
          FDestQuery.Free;
        end;
      end;
    end;
  finally
    FSourceQuery.Free;
  end;
end;

{$IFDEF USELATESTREVISION}
type
  TBranchRevisionState = class(TObject)
  private
    FBranchID: Integer;
    FLastLatestRevisionState: Int64;
    FLatestRevisionState: Int64;
  public
    constructor Create(ABranchID: Integer);
    property BranchID: Integer read FBranchID;
    property LastLatestRevisionState: Int64 read FLastLatestRevisionState write FLastLatestRevisionState;
    property LatestRevisionState: Int64 read FLatestRevisionState write FLatestRevisionState;
  end;

  TBranchRevisionStateList = class(TObject)
  private
    FItems: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetBranchRevisionState(ABranchID: Integer): TBranchRevisionState;
  end;

constructor TBranchRevisionState.Create(ABranchID: Integer);
begin
  inherited Create;
  FBranchID := ABranchID;
  FLastLatestRevisionState := 0;
  FLatestRevisionState := 0;
end;

constructor TBranchRevisionStateList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TBranchRevisionStateList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TBranchRevisionStateList.GetBranchRevisionState(ABranchID: Integer): TBranchRevisionState;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Pred(FItems.Count) do
    if TBranchRevisionState(FItems[I]).BranchID = ABranchID then
    begin
      Result := TBranchRevisionState(FItems[I]);
      Break;
    end;
  if not Assigned(Result) then
  begin
    FItems.Add(TBranchRevisionState.Create(ABranchID));
    Result := TBranchRevisionState(FItems.Last);
  end;
end;

var
  GBranchRevisionStateList: TBranchRevisionStateList = nil;

function BranchRevisionStateList: TBranchRevisionStateList;
begin
  if not Assigned(GBranchRevisionStateList) then
    GBranchRevisionStateList := TBranchRevisionStateList.Create;
  Result := GBranchRevisionStateList;
end;

function GetLatestRevisionState(ABranchID: Integer): Int64;
begin
  Result := BranchRevisionStateList.GetBranchRevisionState(ABranchID).LatestRevisionState;
end;

function GetNewLatestRevisionState(ABranchID: Integer): Int64;
var
  BranchRevisionState: TBranchRevisionState;
begin
  BranchRevisionState := BranchRevisionStateList.GetBranchRevisionState(ABranchID);
  if BranchRevisionState.LastLatestRevisionState = BranchRevisionState.LatestRevisionState then
  begin
    BranchRevisionState.LatestRevisionState := BranchRevisionState.LatestRevisionState + 1;
    Result := BranchRevisionState.LatestRevisionState;
  end
  else
    Result := 0;
end;

procedure UpdateLatestRevisions(UData: TUserDataRecord; ABranchID: Integer);
var
  FQuery, FQuery2: TSrvQuery;
  CurrentModuleID, LastModuleID, LastRevisionID, RevisionCount: Integer;
  ModuleField, RevisionField: TField;
  BranchRevisionState: TBranchRevisionState;
begin
  if UData.UseLatestRevisions then
  begin
    BranchRevisionState := BranchRevisionStateList.GetBranchRevisionState(ABranchID);
    if (BranchRevisionState.LatestRevisionState = 0) or (BranchRevisionState.LastLatestRevisionState <> BranchRevisionState.LatestRevisionState) then
    begin
      FQuery2 := TSrvQuery.Create(nil);
      try
        FQuery2.DatabaseName := UData.DBPath;
        FQuery2.SetTransactionNoAuto;  // we want next all in one transaction
        with FQuery2 do
        begin
          SQL.Clear;
          SQL.Add('DELETE from latestrevision');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := ABranchID;
          ExecSQL;
          SQL.Clear;
          SQL.Add('INSERT INTO latestrevision(branchid, moduleid, revisionid, revisioncount)');
          SQL.Add(Format('VALUES(%d, :moduleid, :revisionid, :revisioncount)', [ABranchID]));
          {$IFDEF ORACLESRV}
          ParamByName('moduleid').DataType := ftInteger;
          ParamByName('revisionid').DataType := ftInteger;
          ParamByName('revisioncount').DataType := ftInteger;
          Prepare;
          {$ENDIF ORACLESRV}
        end;
        FQuery := TSrvQuery.Create(nil);
        try
          FQuery.DatabaseName := UData.DBPath;
          FQuery.SetTransaction(FQuery2); // we want next all in one transaction
          with FQuery do
          begin
            Close;
            SQL.Clear;
            SQL.Add('SELECT revision.moduleid, revision.revisionid FROM revision, rvbranch');
            SQL.Add('WHERE rvbranch.branchid = :branchid');
            SQL.Add('AND rvbranch.revisionid = revision.revisionid');
            SQL.Add('ORDER BY revision.moduleid, revision.version, revision.revision');
            ParamByName('branchid').AsInteger := ABranchID;
            Open;
            LastModuleID := -1;
            LastRevisionID := -1;
            RevisionCount := 0;
            ModuleField := Fields[0];
            RevisionField := Fields[1];
            while not Eof do
            begin
              CurrentModuleID := ModuleField.AsInteger;
              if (LastModuleID = -1) or (CurrentModuleID = LastModuleID) then
              begin
                if LastModuleID = -1 then
                  LastModuleID := CurrentModuleID;
                LastRevisionID := RevisionField.AsInteger;
                Inc(RevisionCount);
              end
              else
              begin
                FQuery2.ParamByName('moduleid').AsInteger := LastModuleID;
                FQuery2.ParamByName('revisionid').AsInteger := LastRevisionID;
                FQuery2.ParamByName('revisioncount').AsInteger := RevisionCount;
                FQuery2.ExecSQL;
                LastModuleID := CurrentModuleID;
                LastRevisionID := RevisionField.AsInteger;
                RevisionCount := 1;
              end;
              Next;
            end;
            if (RevisionCount > 0) and (LastModuleID <> -1) then
            begin
              FQuery2.ParamByName('moduleid').AsInteger := LastModuleID;
              FQuery2.ParamByName('revisionid').AsInteger := LastRevisionID;
              FQuery2.ParamByName('revisioncount').AsInteger := RevisionCount;
              FQuery2.ExecSQL;
            end;
            Close;
          end;
        finally
          FQuery.Free;
        end;
      finally
        FQuery2.Free;
      end;
      if BranchRevisionState.LatestRevisionState = 0 then
        BranchRevisionState.LatestRevisionState := 1;
      BranchRevisionState.LastLatestRevisionState := BranchRevisionState.LatestRevisionState;
    end;
  end;
end;

procedure UpdateLatestModuleRevisions(UData: TUserDataRecord; ANewState: Int64; ABranchID, AModuleID: Integer);
var
  FQuery, FQuery2: TSrvQuery;
  CurrentModuleID, LastModuleID, LastRevisionID, RevisionCount: Integer;
  ModuleField, RevisionField: TField;
  BranchRevisionState: TBranchRevisionState;
begin
  if UData.UseLatestRevisions then
  begin
    BranchRevisionState := BranchRevisionStateList.GetBranchRevisionState(ABranchID);
    if ANewState = BranchRevisionState.LatestRevisionState then
    begin
      FQuery2 := TSrvQuery.Create(nil);
      try
        FQuery2.DatabaseName := UData.DBPath;
        FQuery2.SetTransactionNoAuto; // we want next all in one transaction
        with FQuery2 do
        begin
          SQL.Clear;
          SQL.Add('DELETE from latestrevision');
          SQL.Add('WHERE branchid = :branchid');
          SQL.Add('AND moduleid = :moduleid');
          ParamByName('branchid').AsInteger := ABranchID;
          ParamByName('moduleid').AsInteger := AModuleID;
          ExecSQL;
          SQL.Clear;
          SQL.Add('INSERT INTO latestrevision(branchid, moduleid, revisionid, revisioncount)');
          SQL.Add(Format('VALUES(%d, :moduleid, :revisionid, :revisioncount)', [ABranchID]));
          {$IFDEF ORACLESRV}
          ParamByName('moduleid').DataType := ftInteger;
          ParamByName('revisionid').DataType := ftInteger;
          ParamByName('revisioncount').DataType := ftInteger;
          Prepare;
          {$ENDIF ORACLESRV}
        end;
        FQuery := TSrvQuery.Create(nil);
        try
          FQuery.DatabaseName := UData.DBPath;
          FQuery.SetTransaction(FQuery2); // we want next all in one transaction
          with FQuery do
          begin
            Close;
            SQL.Clear;
            SQL.Add('SELECT revision.moduleid, revision.revisionid FROM revision, rvbranch');
            SQL.Add('WHERE revision.moduleid = :moduleid');
            SQL.Add('AND rvbranch.branchid = :branchid');
            SQL.Add('AND rvbranch.revisionid = revision.revisionid');
            SQL.Add('ORDER BY revision.moduleid, revision.version, revision.revision');
            ParamByName('moduleid').AsInteger := AModuleID;          
            ParamByName('branchid').AsInteger := ABranchID;
            Open;
            LastModuleID := -1;
            LastRevisionID := -1;
            RevisionCount := 0;
            ModuleField := Fields[0];
            RevisionField := Fields[1];
            while not Eof do
            begin
              CurrentModuleID := ModuleField.AsInteger;
              if (LastModuleID = -1) or (CurrentModuleID = LastModuleID) then
              begin
                if LastModuleID = -1 then
                  LastModuleID := CurrentModuleID;
                LastRevisionID := RevisionField.AsInteger;
                Inc(RevisionCount);
              end
              else
              begin
                FQuery2.ParamByName('moduleid').AsInteger := LastModuleID;
                FQuery2.ParamByName('revisionid').AsInteger := LastRevisionID;
                FQuery2.ParamByName('revisioncount').AsInteger := RevisionCount;
                FQuery2.ExecSQL;
                LastModuleID := CurrentModuleID;
                LastRevisionID := RevisionField.AsInteger;
                RevisionCount := 1;
              end;
              Next;
            end;
            if (RevisionCount > 0) and (LastModuleID <> -1) then
            begin
              FQuery2.ParamByName('moduleid').AsInteger := LastModuleID;
              FQuery2.ParamByName('revisionid').AsInteger := LastRevisionID;
              FQuery2.ParamByName('revisioncount').AsInteger := RevisionCount;
              FQuery2.ExecSQL;
            end;
            Close;
          end;
        finally
          FQuery.Free;
        end;
      finally
        FQuery2.Free;
      end;
      BranchRevisionState.LastLatestRevisionState := ANewState;
    end;
  end;
end;
{$ELSE}
function GetLatestRevisionState(ABranchID: Integer): Int64;
begin
  Result := 0;
end;

function GetNewLatestRevisionState(ABranchID: Integer): Int64;
begin
  Result := 0;
end;

procedure UpdateLatestRevisions(UData: TUserDataRecord; ABranchID: Integer);
begin
end;

procedure UpdateLatestModuleRevisions(UData: TUserDataRecord; ANewState: Int64; ABranchID, AModuleID: Integer);
begin
end;
{$ENDIF USELATESTREVISION}

{
function TJVCSBranchServerObject.GetSelectedBranchID: Integer;
begin
  Result := 1;
  if Assigned(FOrbDataPtr) and Assigned(FOrbDataPtr^.Tag) and
    (FOrbDataPtr^.Tag is TBranchClientWSocket)
  then
    Result := TBranchClientWSocket(FOrbDataPtr^.Tag).BranchID;
end;

procedure TJVCSBranchServerObject.SetSelectedBranchID(ABranchID: Integer);
begin
  if Assigned(FOrbDataPtr) and Assigned(FOrbDataPtr^.Tag) and
    (FOrbDataPtr^.Tag is TBranchClientWSocket)
  then
    TBranchClientWSocket(FOrbDataPtr^.Tag).BranchID := ABranchID;
end;
}

function TJVCSBranchServerObject.BranchModuleVersionRevisionExists(ABranchID, AModuleID,
  AVersion, ARevision: Integer): Boolean;
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
      SQL.Add('SELECT revision.revisionid');
      SQL.Add('FROM revision, rvbranch');
      SQL.Add('WHERE revision.moduleid = :moduleid');
      SQL.Add('AND revision.version = :version');
      SQL.Add('AND revision.revision = :revision');
      SQL.Add('AND rvbranch.branchid = :branchid');
      SQL.Add('AND rvbranch.revisionid = revision.revisionid');
      ParamByName('branchid').AsInteger := ABranchID;
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

function TJVCSBranchServerObject.GetRevisionBranchCount(ARevisionID: Integer): Integer;
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
      SQL.Add('SELECT COUNT(*) FROM rvbranch');
      SQL.Add('WHERE revisionid = :revisionid');
      ParamByName('revisionid').AsInteger := ARevisionID;
      Open;
      Result := Fields[0].AsInteger;
      Close;
    end;
  finally
    FQuery.Free;
  end;
end;

function TJVCSBranchServerObject.IsSharedRevision(ARevisionID: Integer): Boolean;
begin
  Result := GetRevisionBranchCount(ARevisionID) > 1;
end;

function TJVCSBranchServerObject.IsOnlyRevisionOfBranch(ARevisionID, ABranchID: Integer): Boolean;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
begin
  if GetRevisionBranchCount(ARevisionID) <> 1 then
    Result := False
  else
  begin
    UData := PUserDataRecord(FUserData);
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        SQL.Clear;
        SQL.Add('SELECT COUNT(*) FROM rvbranch');
        SQL.Add('WHERE revisionid = :revisionid');
        SQL.Add('AND branchid = :branchid');
        ParamByName('revisionid').AsInteger := ARevisionID;
        ParamByName('branchid').AsInteger := ABranchID;
        Open;
        Result := Fields[0].AsInteger = 1;
        Close;
      end;
    finally
      FQuery.Free;
    end;
  end;
end;

procedure TJVCSBranchServerObject.UpdateProjectLastAccess(ABranchID, AProjectID, AUserID: Integer);
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
      SQL.Add('UPDATE projects');
      SQL.Add('SET lastuser = :lastuser, lastaccess = :lastaccess');
      SQL.Add('WHERE projectid = :projectid');
      ParamByName('lastuser').AsInteger := AUserID;
      ParamByName('lastaccess').AsDateTime := LocalDT2GMTDT(Now);
      ParamByName('projectid').AsInteger := AProjectID;
      ExecSQL;
    end;
  finally
    FQuery.Free;
  end;
end;

{==============================================================================
  Select a branch

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Branch ID              - Integer

  Response: 0: [0]branch found?          - Boolean

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectSELECT_BRANCH.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  BranchID: Integer;
  FoundBranch: Boolean;
  Session: TJVCSSession;
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

    BranchID := StrToInt(FRequestBuffer.Fields[0]);

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if we have already this project in the archive
        Close;
        SQL.Clear;
        //"SELECT 1 FROM branches" does not work in UIB port thatswhy we use branchid
        SQL.Add('SELECT branchid FROM branches');
        SQL.Add('WHERE branchid = :branchid');
        ParamByName('branchid').AsInteger := BranchID;
        Open;
        FoundBranch := not (Bof and Eof);
        Close;
      end; // with FQuery do begin
      if FoundBranch then
      begin
        // return true
        Session := SessionList.FindSession(AccessID, TANr);
        if Assigned(Session) then
          Session.BranchID := BranchID;
        FResponseBuffer.WriteFields(True, [True]);
      end
      else
        FResponseBuffer.WriteFields(True, [False]);
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
  Add a new branch

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer
               [1]Parent Branch ID       - Integer
               [2]Branch name            - String(100)
               [3]Description            - String(2000)
               [4]Tag only               - Boolean
               [5]Substitute root path?  - Boolean
               [6]Old root path          - String(255)
               [7]New root path          - String(255)

  Response: 0: [0]new branch?            - Boolean
               [1]Branch ID              - Integer

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectADD_BRANCH.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  UserID,
  ParentBranchID,
  BranchID: Integer;
  BranchName,
  BranchDescr: string;
  BranchTagOnly: Boolean;
  SubstitudeRootPath: Boolean;
  OldRootPath, NewRootPath: string;
  IsNewBranch, IsValidParent: Boolean;
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
    ParentBranchID := StrToInt(FRequestBuffer.Fields[1]);
    BranchName := FRequestBuffer.Fields[2];
    BranchDescr := FRequestBuffer.Fields[3];
    BranchTagOnly := (FRequestBuffer.Fields[4] = '1');
    SubstitudeRootPath := (FRequestBuffer.Fields[5] = '1');
    OldRootPath := AnsiLowerCase(FRequestBuffer.Fields[6]);
    NewRootPath := AnsiLowerCase(FRequestBuffer.Fields[7]);
    BranchID := 0;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // check if we have already this project in the archive
        Close;
        SQL.Clear;
        SQL.Add('SELECT branchid FROM branches');
        SQL.Add('WHERE name = :branch');
        ParamByName('branch').AsString := BranchName;
        Open;
        IsNewBranch := Eof;
        if not Eof then
          BranchID := FQuery.FieldByName('branchid').AsInteger;
        Close;
        SQL.Clear;
        SQL.Add('SELECT name FROM branches');
        SQL.Add('WHERE branchid = :branchid');
        ParamByName('branchid').AsInteger := ParentBranchID;
        Open;
        IsValidParent := not (Bof and Eof);
        Close;
      end; // with FQuery do begin
      if IsNewBranch and IsValidParent then
      begin
        // return true
        FResponseBuffer.WriteFields(True, [True]);
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO branches');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('branches')]));
          SQL.Add(':parentbranchid, :name, :description, :createdby, :createdin)');
          ParamByName('parentbranchid').AsInteger := ParentBranchID;
          ParamByName('name').AsString := BranchName;
          ParamByName('description').AsString := BranchDescr;
          ParamByName('createdby').AsInteger := UserID;
          ParamByName('createdin').AsDateTime := LocalDT2GMTDT(Now);
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // get the new created project ID
          SQL.Clear;
          SQL.Add('SELECT branchid FROM branches');
          SQL.Add('WHERE name = :branch');
          ParamByName('branch').AsString := BranchName;
          Open;
          BranchID := FieldByName('branchid').AsInteger;
          Close;
        end; // with FQuery do begin
        CopyProjectModules(UData^, ParentBranchID, BranchID);
        UpdateLatestRevisions(UData^, ParentBranchID);
        CopyRevisions(UData^, ParentBranchID, BranchID, BranchTagOnly);
        {
        //CopyModuleLock(... //there should be no need
        CopyModuleLocation(UData^, ParentBranchID, BranchID);
        }
        CopyBranchModule(UData^, ParentBranchID, BranchID, SubstitudeRootPath, OldRootPath, NewRootPath);
        CopyProjectReferences(UData^, ParentBranchID, BranchID);
        UpdateLatestRevisions(UData^, BranchID);
        UData^.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        // return the new project ID
        FResponseBuffer.WriteFields(False, [BranchID]);
      end // if IsNewProject then begin
      else
      begin
        // return false
        FResponseBuffer.WriteFields(True, [False]);
        // return the old project ID
        FResponseBuffer.WriteFields(False, [BranchID]);
      end; // else if IsNewProject then begin
      // update log file
      if UData.WriteVCSLog and IsNewBranch then
      begin
      //later
      end; // if UData.WriteVCSLog then begin
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
  Remove a branch

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Branch ID              - Integer

  Response: 0: [0]Removed                - Boolean
               [1]Error message          - String
                  (only if Removed = false)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectREMOVE_BRANCH.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery,
  FRevisionsQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  BranchID: Integer;
  CanRemove, FoundBranch, FoundChildBranches, IsHead: Boolean;
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

    BranchID := StrToInt(FRequestBuffer.Fields[0]);

    FoundChildBranches := False;
    IsHead := False;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        //"SELECT 1 FROM branches" does not work in UIB port thatswhy we use branchid
        SQL.Add('SELECT branchid FROM branches');
        SQL.Add('WHERE branchid = :branchid');
        ParamByName('branchid').AsInteger := BranchID;
        Open;
        FoundBranch := not (Bof and Eof);
        Close;
        if FoundBranch then
        begin
          Close;
          SQL.Clear;
          SQL.Add('SELECT COUNT(*) FROM branches');
          SQL.Add('WHERE parentbranchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          Open;
          FoundChildBranches := Fields[0].AsInteger > 0;
          Close;
          SQL.Clear;
          SQL.Add('SELECT parentbranchid FROM branches');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          Open;
          IsHead := Fields[0].AsInteger < 1;
          Close;
        end;
      end;
      CanRemove := FoundBranch and (not FoundChildBranches) and (not IsHead);
      if CanRemove then
      begin
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('DELETE FROM latestrevision');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          SQL.Clear;
          SQL.Add('DELETE FROM revisionproperty');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          { TODO: is there a better option then always using the same two subselects
            and to avoid FRevisionsQuery loop (a temporary table could be a solution for
            at least Firebird >= 2.1 and Oracle) }
          SQL.Clear;
          SQL.Add('DELETE FROM rvlabels');
          SQL.Add('WHERE revisionid in');
          SQL.Add('(SELECT revisionid FROM rvbranch');
          SQL.Add('WHERE branchid = :branchid');
          SQL.Add('AND revisionid in');
          SQL.Add('(SELECT revisionid FROM rvbranch');
          SQL.Add('GROUP BY revisionid');
          SQL.Add('HAVING COUNT(*) = 1))');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          SQL.Clear;
          SQL.Add('DELETE FROM blobs');
          SQL.Add('WHERE revisionid in');
          SQL.Add('(SELECT revisionid FROM rvbranch');
          SQL.Add('WHERE branchid = :branchid');
          SQL.Add('AND revisionid in');
          SQL.Add('(SELECT revisionid FROM rvbranch');
          SQL.Add('GROUP BY revisionid');
          SQL.Add('HAVING COUNT(*) = 1))');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          SQL.Clear;
          SQL.Add('DELETE FROM logcomm');
          SQL.Add('WHERE revisionid in');
          SQL.Add('(SELECT revisionid FROM rvbranch');
          SQL.Add('WHERE branchid = :branchid');
          SQL.Add('AND revisionid in');
          SQL.Add('(SELECT revisionid FROM rvbranch');
          SQL.Add('GROUP BY revisionid');
          SQL.Add('HAVING COUNT(*) = 1))');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          FRevisionsQuery := TSrvQuery.Create(Self);
          try
            FRevisionsQuery.DatabaseName := UData.DBPath;
            FRevisionsQuery.SetTransaction(FQuery);
            FRevisionsQuery.SQL.Add('SELECT revisionid FROM revision');
            FRevisionsQuery.SQL.Add('WHERE revisionid in');
            FRevisionsQuery.SQL.Add('(SELECT revisionid FROM rvbranch');
            FRevisionsQuery.SQL.Add('WHERE branchid = :branchid');
            FRevisionsQuery.SQL.Add('AND revisionid in');
            FRevisionsQuery.SQL.Add('(SELECT revisionid FROM rvbranch');
            FRevisionsQuery.SQL.Add('GROUP BY revisionid');
            FRevisionsQuery.SQL.Add('HAVING COUNT(*) = 1))');
            FRevisionsQuery.ParamByName('branchid').AsInteger := BranchID;
            FRevisionsQuery.Open;

            SQL.Clear;
            SQL.Add('DELETE FROM rvbranch');
            SQL.Add('WHERE branchid = :branchid');
            ParamByName('branchid').AsInteger := BranchID;
            ExecSQL;

            SQL.Clear;
            SQL.Add('DELETE FROM revision');
            SQL.Add('WHERE revisionid = :revisionid');
            while not FRevisionsQuery.Eof do
            begin
              ParamByName('revisionid').AsInteger := FRevisionsQuery.Fields[0].AsInteger;
              ExecSQL;
              FRevisionsQuery.Next;
            end;
          finally
            FRevisionsQuery.Free;
          end;

          SQL.Clear;
          SQL.Add('DELETE FROM pjmodule');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          SQL.Clear;
          SQL.Add('DELETE FROM brmodule');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          SQL.Clear;
          SQL.Add('DELETE FROM pjref');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          SQL.Clear;
          SQL.Add('DELETE FROM vcslog');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;

          SQL.Clear;
          SQL.Add('DELETE FROM branches');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;
        end;
        // removed, return true
        FResponseBuffer.WriteFields(True, [True]);
        // return blank error message
        FResponseBuffer.WriteFields(False, ['']);
      end // if CanRemove then begin
      else
      begin
        // return false
        FResponseBuffer.WriteFields(True, [False]);
        // return error message
        if not FoundBranch then
          FResponseBuffer.WriteFields(False,
                                      ['Branch does not exist.'])
        else
        if FoundChildBranches then
          FResponseBuffer.WriteFields(False,
                                      ['The branch has child branches.'])
        else
          FResponseBuffer.WriteFields(False,
                                      ['The branch is the HEAD branch.']);
      end; // else if CanRemove then begin
      // update log file
      if UData.WriteVCSLog and CanRemove then
      begin
      //later
      end; // if UData.WriteVCSLog then begin
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
  update a branch

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Branch ID              - Integer
               [1]Branch name            - String(100)
               [2]Description            - String(2000)

  Response: 0: [0]Updated                - Boolean
               [1]Error message          - String
                  (only if Updated = false)

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectUPDATE_BRANCH.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  BranchID: Integer;
  BranchName,
  BranchDescr: string;
  CanUpdate, FoundBranch, IsNameUnique: Boolean;
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

    BranchID := StrToInt(FRequestBuffer.Fields[0]);
    BranchName := FRequestBuffer.Fields[1];
    BranchDescr := FRequestBuffer.Fields[2];

    IsNameUnique := False;
    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        //"SELECT 1 FROM branches" does not work in UIB port thatswhy we use branchid
        SQL.Add('SELECT branchid FROM branches');
        SQL.Add('WHERE branchid = :branchid');
        ParamByName('branchid').AsInteger := BranchID;
        Open;
        FoundBranch := not (Bof and Eof);
        Close;
        if FoundBranch then
        begin
          SQL.Clear;
          SQL.Add('SELECT COUNT(*) FROM branches');
          SQL.Add('WHERE name = :branch');
          SQL.Add('AND branchid <> :branchid');
          ParamByName('branch').AsString := BranchName;
          ParamByName('branchid').AsInteger := BranchID;
          Open;
          IsNameUnique := Fields[0].AsInteger = 0;
          Close;
        end;
      end; // with FQuery do begin
      CanUpdate := FoundBranch and IsNameUnique;
      if CanUpdate then
      begin
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('UPDATE branches');
          SQL.Add('SET name=:name,');
          SQL.Add('description = :description');
          SQL.Add('WHERE branchid = :branchid');
          ParamByName('name').AsString := BranchName;
          ParamByName('description').AsString := BranchDescr;
          ParamByName('branchid').AsInteger := BranchID;
          ExecSQL;
        end;
        // updated, return true
        FResponseBuffer.WriteFields(True, [True]);
        // return blank error message
        FResponseBuffer.WriteFields(False, ['']);
      end // if CanUpdate then begin
      else
      begin
        // return false
        FResponseBuffer.WriteFields(True, [False]);
        // return error message
        if not FoundBranch then
          FResponseBuffer.WriteFields(False,
                                      ['Branch does not exist.'])
        else
          FResponseBuffer.WriteFields(False,
                                      ['The name is not unique.'])
      end; // else if CanUpdate then begin
      // update log file //td1 - no necessary here?
      if UData.WriteVCSLog and CanUpdate then
      begin
      //later
      end; // if UData.WriteVCSLog then begin
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
  Initial add HEAD branch

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer

  Response: 0: [0]new branch?            - Boolean
               [1]Branch ID              - Integer

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectINITIAL_ADD_HEADBRANCH.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  UserID,
  BranchID: Integer;
  CanAddHEAD: Boolean;
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
    BranchID := 0;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // check if we have already this project in the archive
        Close;
        SQL.Clear;
        SQL.Add('SELECT branchid FROM branches');
        SQL.Add('WHERE name = :branch');
        ParamByName('branch').AsString := 'HEAD';
        Open;
        CanAddHEAD := Eof;
        if not Eof then
          BranchID := FQuery.FieldByName('branchid').AsInteger;
        Close;
      end; // with FQuery do begin
      if CanAddHEAD then
      begin
        // return true
        FResponseBuffer.WriteFields(True, [True]);
        with FQuery do
        begin
          SQL.Clear;
          SQL.Add('INSERT INTO branches');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('branches')]));
          SQL.Add(':parentbranchid, :name, :description, :createdby, :createdin)');
          ParamByName('parentbranchid').AsInteger := 0;
          ParamByName('name').AsString := 'HEAD';
          ParamByName('description').AsString := 'HEAD branch';
          ParamByName('createdby').AsInteger := UserID;
          ParamByName('createdin').AsDateTime := LocalDT2GMTDT(Now);
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
          // get the new created project ID
          SQL.Clear;
          SQL.Add('SELECT branchid FROM branches');
          SQL.Add('WHERE name = :branch');
          ParamByName('branch').AsString := 'HEAD';
          Open;
          BranchID := FieldByName('branchid').AsInteger;
          Close;
        end; // with FQuery do begin
        FillProjectModules(UData^, BranchID);
        CreateRevisions(UData^, BranchID);
        {
        CreateModuleLock(UData^, BranchID);
        CreateModuleLocation(UData^, BranchID);
        }
        CreateBranchModule(UData^, BranchID);
        UData^.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        // return the new project ID
        FResponseBuffer.WriteFields(False, [BranchID]);
      end // if IsNewProject then begin
      else
      begin
        // return false
        FResponseBuffer.WriteFields(True, [False]);
        // return the old project ID
        FResponseBuffer.WriteFields(False, [BranchID]);
      end; // else if IsNewProject then begin
      // update log file
      if UData.WriteVCSLog and CanAddHEAD then
      begin
      //later
      end; // if UData.WriteVCSLog then begin
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
  Get a list of all projects in the version archive

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

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
procedure TServerObjectGET_BRANCH_LIST.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
  InclDetails: Boolean;
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
          then SQL.Add('SELECT branchid, parentbranchid, name, created_by, created_in, description')
            else SQL.Add('SELECT branchid, parentbranchid, name, created_by, created_in');
        SQL.Add('FROM branches');
        SQL.Add('ORDER BY name');
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            if Fld <> 4 then
              FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString])
            else
            begin
              DT := FQuery.FieldByName('created_in').AsDateTime;
              FResponseBuffer.WriteFields(false, [DT]);
            end;
          end;
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
procedure TServerObjectBRANCH_GET_LATEST_REVISIONS.Execute;
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

    UpdateLatestRevisions(UData^, SessionList.GetSessionBranchID(AccessID, TANr));

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // First step: get ALL revisions & revision members
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
        SQL.Add('revision.revisionid, revision.version,');
        SQL.Add('revision.revision, blobs.origtime,');
        SQL.Add('blobs.extension, blobs.origcrc');
        if LabelID > 0 // filter by label?
          then SQL.Add('FROM pjmodule, modules, brmodule, revision, rvbranch, blobs, rvlabels')
            else
            begin
              {$IFDEF USELATESTREVISION}
              if UData^.UseLatestRevisions then
                SQL.Add('FROM pjmodule, modules, brmodule, latestrevision, revision, blobs')
              else
              {$ENDIF USELATESTREVISION}
              SQL.Add('FROM pjmodule, modules, brmodule, revision, rvbranch, blobs');
            end;
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        {$IFDEF USELATESTREVISION}
        if UData^.UseLatestRevisions and (LabelID = 0) then
        begin
          SQL.Add('AND latestrevision.branchid = :branchid');
          SQL.Add('AND latestrevision.moduleid = modules.moduleid');
          SQL.Add('AND revision.revisionid = latestrevision.revisionid');
        end;
        {$ENDIF USELATESTREVISION}
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        {$IFDEF USELATESTREVISION}
        if not UData^.UseLatestRevisions or (LabelID > 0) then
        begin
        {$ENDIF USELATESTREVISION}
          SQL.Add('AND rvbranch.branchid = :branchid');
          SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        {$IFDEF USELATESTREVISION}
        end;
        {$ENDIF USELATESTREVISION}        
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        if LabelID > 0 then
        begin // filter by label?
          SQL.Add('AND rvlabels.labelid = :labelid');
          SQL.Add('AND rvlabels.revisionid = revision.revisionid');
        end;
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');

        (*//extrem slow down in branch version
        //-- added by hdo
        {$IFDEF ORACLESRV}
        //-- faster version with these lines added, at least
        //-- if the project has a reasonable amount of revisions,
        //-- because the sorting slows down things a lot, and less
        //-- records have to be transmitted
        if LabelID <= 0 then //-- don't do this when filtering  for labels!
        begin
          SQL.Add('AND revision.version = (SELECT MAX(version) FROM revision WHERE moduleid = modules.moduleid)');
          SQL.Add('AND revision.revision = (SELECT MAX(revision) FROM revision WHERE moduleid = modules.moduleid');
          SQL.Add(' AND version = (SELECT MAX(version) FROM revision WHERE moduleid = modules.moduleid))');
        end;
        {$ENDIF ORACLESRV}
        *)

        SQL.Add('ORDER BY modules.name DESC, modules.moduleid DESC,');
        SQL.Add('revision.version DESC, revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_GET_VERSION_LIST.Execute;
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

    UpdateLatestRevisions(UData^, SessionList.GetSessionBranchID(AccessID, TANr));

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
        {$IFDEF USELATESTREVISION}
        if UData^.UseLatestRevisions then
        begin
          SQL.Add('pjmodule.hidden, users.login, latestrevision.revisioncount');
          SQL.Add('FROM pjmodule, modules, brmodule, latestrevision, revision, users');
        end
        else
        begin
        {$ENDIF USELATESTREVISION}
        SQL.Add('pjmodule.hidden, users.login');
        SQL.Add('FROM pjmodule, modules, brmodule, revision, rvbranch, users');
        {$IFDEF USELATESTREVISION}
        end;
        {$ENDIF USELATESTREVISION}
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        {$IFDEF USELATESTREVISION}
        if UData^.UseLatestRevisions then
        begin
          SQL.Add('AND latestrevision.branchid = :branchid');
          SQL.Add('AND latestrevision.moduleid = pjmodule.moduleid');
          SQL.Add('AND revision.revisionid = latestrevision.revisionid');
        end
        else
        begin
        {$ENDIF USELATESTREVISION}
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        {$IFDEF USELATESTREVISION}
        end;
        {$ENDIF USELATESTREVISION}        
        SQL.Add('AND users.userid = brmodule.userid');
        SQL.Add('ORDER BY modules.moduleid DESC, revision.version DESC,');
        SQL.Add('revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
          {$IFDEF USELATESTREVISION}
          if UData^.UseLatestRevisions then
            Next
          else
          begin
          {$ENDIF USELATESTREVISION}
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
          {$IFDEF USELATESTREVISION}
          end;
          {$ENDIF USELATESTREVISION}
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
procedure TServerObjectBRANCH_GET_BLANK_MODULE_LIST.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name,');
        SQL.Add('brmodule.path, pjmodule.hidden');
        SQL.Add('FROM pjmodule, modules, brmodule');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        if ExclHidden then // skip hiddden modules
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND NOT EXISTS');
        SQL.Add('(SELECT 1 FROM revision, rvbranch');
        SQL.Add('WHERE revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid)');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('ORDER BY modules.moduleid');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_GET_MODULE_HISTORY.Execute;
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
      FQuery.SetTransactionNoAuto;
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
        SQL.Add('SELECT brmodule.readonly, revision.version,');
        SQL.Add('revision.revision, users.login, blobs.origtime,');
        SQL.Add('blobs.origsize, blobs.extension, revision.comment_i,');
        SQL.Add('revision.comment_o, revision.revisionid');
        SQL.Add('FROM modules, brmodule, revision, rvbranch, blobs, users');
        SQL.Add('WHERE modules.moduleid = :moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND revision.revisionid = blobs.revisionid');
        SQL.Add('AND revision.userid = users.userid');
        SQL.Add('ORDER BY revision.version, revision.revision, blobs.blobid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_CHECKIN_MODULE.Execute;
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
  NewLatestRevisionState: Int64;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // check if the module is new to the archive
        SQL.Clear;
        SQL.Add('SELECT brmodule.readonly, brmodule.userid, users.login');
        SQL.Add('FROM revision, rvbranch, modules, brmodule, users');
        SQL.Add('WHERE revision.moduleid = :moduleid');
        SQL.Add('AND modules.moduleid = revision.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND users.userid = brmodule.userid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
            SQL.Add('SELECT revision.revisionid');
            SQL.Add('FROM revision, rvbranch');
            SQL.Add('WHERE revision.moduleid = :moduleid');
            SQL.Add('AND rvbranch.branchid = :branchid');
            SQL.Add('AND rvbranch.revisionid = revision.revisionid');
            SQL.Add('AND revision.version = :version');
            SQL.Add('AND revision.revision = :revision');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('version').AsInteger := StrToInt(FRequestBuffer.Fields[5]);
            ParamByName('revision').AsInteger := StrToInt(FRequestBuffer.Fields[6]);
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
        NewLatestRevisionState := GetNewLatestRevisionState(SessionList.GetSessionBranchID(AccessID, TANr));
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
          SQL.Add('SELECT MAX(revisionid) FROM revision');
          SQL.Add('WHERE moduleid = :moduleid AND version = :version');
          SQL.Add('AND revision = :revision');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('version').AsInteger := StrToInt(FRequestBuffer.Fields[5]);
          ParamByName('revision').AsInteger := StrToInt(FRequestBuffer.Fields[6]);
          Open;
          NewRevisionID := FQuery.Fields[0].AsInteger;
          Close;

          SQL.Clear;
          SQL.Add('INSERT INTO rvbranch');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('rvbranch')]));
          SQL.Add(':branchid, :revisionid, ''s'')');
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ParamByName('revisionid').AsInteger := NewRevisionID;
          ExecSQL;

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
          SQL.Add('SET lastuser = :lastuser,');
          SQL.Add('tstamp = :tstamp,');
          if FamilyID > 0 then
            SQL.Add('familyid = :familyid')
          else
            SQL.Add('familyid = null');
          SQL.Add('WHERE moduleid = :moduleid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('moduleid').AsInteger := ModuleID;
          if FamilyID > 0 then
            ParamByName('familyid').AsInteger := FamilyID;
          ExecSQL;
          SQL.Clear;
          SQL.Add('UPDATE brmodule');
          if PutOnly then
            SQL.Add('SET readonly = ''1'',')
          else
            SQL.Add('SET readonly = ''0'',');
          SQL.Add('userid = :userid,');
          SQL.Add('locktstamp = :locktstamp');
          SQL.Add('WHERE moduleid = :moduleid');
          SQL.Add('AND branchid = :branchid');
          ParamByName('userid').AsInteger := UserID;
          if PutOnly then
            ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now)
          else
            ParamByName('locktstamp').AsDateTime := 0;
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ExecSQL;
          Close; //Commit Transaction to prevent Deadlock in following Updates

          // update project information
          UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
          UpdateLatestModuleRevisions(UData^, NewLatestRevisionState, SessionList.GetSessionBranchID(AccessID, TANr), ModuleID);
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
      BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, NewRevisionID, 'i', LogMessage);
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
procedure TServerObjectBRANCH_GET_REVISION_LIST_BY_ID.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
        SQL.Add('brmodule.readonly, modules.tstamp, brmodule.userid,');
        SQL.Add('revision.revisionid, revision.version, revision.revision,');
        SQL.Add('users.login, projects.projectid, pjmodule.hidden');
        SQL.Add('FROM pjmodule');
        SQL.Add('INNER JOIN projects    ON projects.projectid = pjmodule.projectid');
        SQL.Add('INNER JOIN modules     ON modules.moduleid = pjmodule.moduleid');

        SQL.Add('INNER JOIN brmodule    ON (brmodule.branchid = :branchid) and (brmodule.moduleid = modules.moduleid)');

        SQL.Add('INNER JOIN revision    ON revision.moduleid = modules.moduleid');
        {
        SQL.Add('INNER JOIN rvbranch    ON rvbranch.branchid = :branchid');
        SQL.Add('INNER JOIN rvbranch    ON rvbranch.revisionid = revision.revisionid');
        }
        SQL.Add('INNER JOIN rvbranch    ON (rvbranch.branchid = :branchid) and (rvbranch.revisionid = revision.revisionid)');
        SQL.Add('LEFT OUTER JOIN users  ON brmodule.userid = users.userid');
        SQL.Add('WHERE pjmodule.moduleid = :moduleid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        if ProjectID <> 0 then
          SQL.Add('AND pjmodule.projectid = :projectid');
        SQL.Add('ORDER BY revision.version, revision.revision, projects.projectid');
        ParamByName('moduleid').AsInteger := ModuleID;
        if ProjectID <> 0 then
        begin
          ParamByName('projectid').AsInteger := ProjectID;
        end;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
  Get ...

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Module ID              - Integer

  Response: 0: [0]Branch ID              - Integer
               [1]Revision ID            - Integer 

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectBRANCH_GET_MODULE_REVISIONS.Execute;
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
        SQL.Clear;
        SQL.Add('SELECT rvbranch.branchid, rvbranch.revisionid ');
        SQL.Add('FROM revision, rvbranch');
        SQL.Add('WHERE revision.moduleid = :moduleid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        while not Eof do
        begin
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
procedure TServerObjectBRANCH_GET_BLOB_STATUS.Execute;
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
        SQL.Add('FROM revision, rvbranch, blobs');
        SQL.Add('WHERE revision.moduleid = :moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        SQL.Add('ORDER BY revision.version, revision.revision');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_GET_REVISION_STATUS.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name,');
        SQL.Add('brmodule.path, brmodule.readonly,');
        SQL.Add('revision.version, revision.revision,');
        SQL.Add('blobs.origtime, blobs.origsize, blobs.origcrc,');
        SQL.Add('blobs.compsize, blobs.extension');
        SQL.Add('FROM revision, modules, brmodule, blobs');
        SQL.Add('WHERE revision.revisionid = :revisionid');
        SQL.Add('AND modules.moduleid = revision.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        ParamByName('revisionid').AsInteger := RevisionID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_GET_CHECKOUT_MODULE.Execute;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        IsMostRecent := True;
        {  be sure that the requested revision ID is already the most recent
           one. Probably things have changed in the meantime...  }
        if CheckOut then
        begin
          // however, this is only meaningful for Check Out operations...
          IsMostRecent := False;
          SQL.Clear;
          {  RevisionID is always increasing, therefore the most recent revision
             has the highest revisionid  }
          SQL.Add('SELECT MAX(revision.revisionid) AS maxrevid');
          SQL.Add('FROM revision, rvbranch');
          SQL.Add('WHERE moduleid = :moduleid');
          SQL.Add('AND rvbranch.branchid = :branchid');
          SQL.Add('AND rvbranch.revisionid = revision.revisionid');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          Open;
          // compare requested revision to most recent
          if not Eof then
            IsMostRecent := (RevisionID = FieldByName('maxrevid').AsInteger);
          Close;
        end; // if CheckOut then
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
//todo - remove modules table from statement
            SQL.Clear;
            SQL.Add('SELECT brmodule.readonly, brmodule.locktstamp, users.login');
            SQL.Add('FROM modules, brmodule, users');
            SQL.Add('WHERE modules.moduleid = :moduleid');
            SQL.Add('AND brmodule.branchid = :branchid');
            SQL.Add('AND brmodule.moduleid = modules.moduleid');
            SQL.Add('AND users.userid = brmodule.userid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
              SQL.Add('SET lastuser = :lastuser,');
              SQL.Add('tstamp = :tstamp');
              SQL.Add('WHERE moduleid = :moduleid');
              ParamByName('lastuser').AsInteger := UserID;
              ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
              ParamByName('moduleid').AsInteger := ModuleID;
              ExecSQL;
              SQL.Clear;
              SQL.Add('UPDATE brmodule');
              SQL.Add('SET readonly = ''1'',');
              SQL.Add('userid = :userid,');
              SQL.Add('locktstamp = :locktstamp');
              SQL.Add('WHERE moduleid = :moduleid');
              SQL.Add('AND branchid = :branchid');              
              ParamByName('userid').AsInteger := UserID;
              ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now);
              ParamByName('moduleid').AsInteger := ModuleID;
              ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);              
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
            // save statistic data
            Inc(UData.GetCount);
          end; // else if CheckOut then begin
          Close;
          // update project information
          if CheckOut and CanCheckOut then
          begin
            // Get is not a write access, leave project information unchanged
            UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
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
        BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, 0, LogType, LogMessage);
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
procedure TServerObjectBRANCH_CHECKOUT_ONLY_MODULE.Execute;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // check if the module is available for check out
        Close;
//todo - remove modules table from statement
        SQL.Clear;
        SQL.Add('SELECT brmodule.readonly, brmodule.locktstamp, users.login');
        SQL.Add('FROM modules, brmodule, users');
        SQL.Add('WHERE modules.moduleid = :moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND users.userid = brmodule.userid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
          SQL.Add('SET lastuser = :lastuser,');
          SQL.Add('tstamp = :tstamp');
          SQL.Add('WHERE moduleid = :moduleid');
          ParamByName('lastuser').AsInteger := UserID;
          ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('moduleid').AsInteger := ModuleID;
          ExecSQL;
          SQL.Clear;
          SQL.Add('UPDATE brmodule');
          SQL.Add('SET readonly = ''1'',');
          SQL.Add('userid = :userid,');
          SQL.Add('locktstamp = :locktstamp');
          SQL.Add('WHERE moduleid = :moduleid');
          SQL.Add('AND branchid = :branchid');
          ParamByName('userid').AsInteger := UserID;
          ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now);
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
          UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
        end; // if CanCheckOut then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
    begin
      if CanCheckOut then
        BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, 0, 'o',
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
procedure TServerObjectBRANCH_UNDO_CHECKOUT_MODULE.Execute;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // check if the module is available for undo check out
//todo - remove modules table from statement
        SQL.Clear;
        SQL.Add('SELECT brmodule.readonly, brmodule.userid,');
        SQL.Add('users.login');
        SQL.Add('FROM modules, brmodule, users');
        SQL.Add('WHERE modules.moduleid = :moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND users.userid = brmodule.userid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
            SQL.Add('SET lastuser = :lastuser,');
            SQL.Add('tstamp = :tstamp');
            SQL.Add('WHERE moduleid = :moduleid');
            ParamByName('lastuser').AsInteger := UserID;
            ParamByName('tstamp').AsDateTime := LocalDT2GMTDT(Now);
            ParamByName('moduleid').AsInteger := ModuleID;
            ExecSQL;
            SQL.Clear;
            SQL.Add('UPDATE brmodule');
            SQL.Add('SET readonly = ''0'',');
            SQL.Add('userid = :userid,');
            SQL.Add('locktstamp = :locktstamp');
            SQL.Add('WHERE moduleid = :moduleid');
            SQL.Add('AND branchid = :branchid');
            ParamByName('userid').AsInteger := UserID;
            ParamByName('locktstamp').AsDateTime := 0;
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
            ExecSQL;

            //get revisionid of latest revision
            RevisionID := -1;
            SQL.Clear;
            //USc 25.12.2004 normally the fields version and revision are obsolete
            // in the SELECT clause but DBISAM requires them
            // in order to get the ORDER BY working (otherwise exception)
            SQL.Add('SELECT revision.revisionid, version, revision FROM revision, rvbranch');
            SQL.Add('WHERE moduleid = :moduleid');
            SQL.Add('AND rvbranch.branchid = :branchid');
            SQL.Add('AND rvbranch.revisionid = revision.revisionid');
            SQL.Add('ORDER BY revision.version DESC, revision.revision DESC');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
        Close;
        // update project information
        if CanUndo then
        begin
          UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
        end; // if CanUndo then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and CanUndo then
      BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, 0, 'u',
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
procedure TServerObjectBRANCH_RENAME_MODULE.Execute;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // check if the module is locked
        Close;
//todo - remove modules table from statement        
        SQL.Clear;
        SQL.Add('SELECT brmodule.name, brmodule.readonly, brmodule.path FROM modules, brmodule');
        SQL.Add('WHERE modules.moduleid = :moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
          SQL.Add('SELECT moduleid FROM brmodule');
          SQL.Add('WHERE branchid = :branchid');
          SQL.Add('AND   path = :path');
          SQL.Add('AND   name = :name');
          ParamByName('path').AsString := ModulePath;
          ParamByName('name').AsString := NewName;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
            SQL.Add('UPDATE brmodule SET name = :newname');
            SQL.Add('WHERE moduleid = :moduleid');
            SQL.Add('AND branchid = :branchid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
        BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, AccessID, ModuleID, 0, 'c',
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
procedure TServerObjectBRANCH_GET_PROJECT_MODULE_LIST.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path');
        SQL.Add('FROM pjmodule, modules, brmodule');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');        
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        if ExclHidden then // skip hiddden modules
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY modules.moduleid');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
procedure TServerObjectBRANCH_MOVE_MODULE.Execute;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        {  #2015 - same module must not exist in target folder:
           Check: would the move lead to the existance of a module with same
                  path/name but with a different moduleid?
           => Raise 400 exception
           Remark: shared modules are not stored in modules table so I can check
                   the modules table
         }
        SQL.Clear;
        SQL.Add('SELECT moduleid FROM brmodule');
        SQL.Add('WHERE branchid = :branchid');
        SQL.Add('AND   path = :path');
        SQL.Add('AND   name = :name');
        ParamByName('path').AsString := NewPath;
        ParamByName('name').AsString := ExtractFileName(ModuleName);
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
          SQL.Add('SELECT readonly FROM brmodule');
          SQL.Add('WHERE moduleid = :moduleid');
          SQL.Add('AND branchid = :branchid');          
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
            SQL.Add('UPDATE brmodule');
            SQL.Add('SET path = :path');
            SQL.Add('WHERE moduleid = :moduleid');
            SQL.Add('AND branchid = :branchid');
            ParamByName('path').AsString := NewPath;
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
            ExecSQL;
            // update project information
            UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
          end; // if (not IsReadOnly) and (not IsSharedModule) then begin
        end;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and (not IsReadOnly) and (not IsExistingModule) {and (not IsSharedModule)} then
      BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, 0, 'v',
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
procedure TServerObjectBRANCH_GET_SHARED_BY.Execute;
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
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND projects.projectid = pjmodule.projectid');
        SQL.Add('AND projects.deleted = ''0''');
        SQL.Add('ORDER BY projects.name');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_GET_SHARED_MODULES.Execute;
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
        SQL.Add('WHERE pjmodule.branchid = :branchid');
        SQL.Add('GROUP BY moduleid');
        //mantis #685 - performance suggestion from HDo
        //actually tested with Ora, Ib, Fb, DBIsam:
        //Todo: Other DBMS port maintainers please check if working!
        SQL.Add('HAVING COUNT(moduleid) > 1');
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);      
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
procedure TServerObjectBRANCH_GET_LABELS_BY_PROJECT.Execute;
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
        SQL.Add('FROM pjmodule, rvlabels, revision, rvbranch');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND rvlabels.revisionid = revision.revisionid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('ORDER BY pjmodule.moduleid, revision.revisionid');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
procedure TServerObjectBRANCH_ADD_NEW_MODULE.Execute;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT moduleid FROM brmodule');
        SQL.Add('WHERE branchid = :branchid');
        SQL.Add('AND name = :module');
        SQL.Add('AND path = :path');
        ParamByName('module').AsString := ExtractFileName(Module);
        ParamByName('path').AsString := ExtractFilePath(Module);
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
        Open;
        IsNewModule := Eof;
        if not Eof then ModuleID := FQuery.FieldByName('moduleid').AsInteger;
        Close;
        if IsNewModule then
        begin
          // return true
          FResponseBuffer.WriteFields(True, [True]);
          SQL.Clear;
//todo - remove brmodule fields
          SQL.Add('INSERT INTO modules');
          SQL.Add(Format('VALUES (%s',[GetInsertKeyFieldValueStr('modules')]));
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
          {
          SQL.Clear;
          SQL.Add('SELECT moduleid FROM brmodule');
          SQL.Add('WHERE branchid = :branchid AND name = :name AND path = :path');
          ParamByName('name').AsString := ExtractFileName(Module);
          ParamByName('path').AsString := ExtractFilePath(Module);
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          Open;
          ModuleID := FQuery.FieldByName('moduleid').AsInteger;
          Close;
          }
          SQL.Clear;
          SQL.Add('SELECT MAX(moduleid) AS maxmoduleid FROM modules');
          Open;
          ModuleID := FQuery.FieldByName('maxmoduleid').AsInteger;
          Close;

          SQL.Clear;
          SQL.Add('INSERT INTO brmodule');
          SQL.Add(Format('VALUES (%s :branchid, :moduleid,', [GetInsertKeyFieldValueStr('brmodule')]));
          SQL.Add(':name,:path, ''0'', :userid,');
          SQL.Add(':locktstamp)');
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ParamByName('moduleid').AsInteger := ModuleID;          
          ParamByName('name').AsString := ExtractFileName(Module);
          ParamByName('path').AsString := ExtractFilePath(Module);
          ParamByName('userid').AsInteger := UserID;
          ParamByName('locktstamp').AsDateTime := LocalDT2GMTDT(Now);
          ExecSQL;
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
          SQL.Add('AND pjmodule.branchid = :branchid');          
          SQL.Add('AND projects.projectid = pjmodule.projectid');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);          
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
          SQL.Add('AND pjmodule.branchid = :branchid');          
          SQL.Add('AND moduleid = :moduleid');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);          
          Open;
          IsAlreadyAssigned := not Eof;
          Close;
          if not IsAlreadyAssigned then
          begin
            SQL.Clear;
            if GetInsertKeyFieldValueStr('pjmodule') <> '' then
              SQL.Add('INSERT INTO pjmodule(recordid, branchid, projectid, moduleid, hidden)')
            else
              SQL.Add('INSERT INTO pjmodule(branchid, projectid, moduleid, hidden)');            
            SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjmodule')]));
            SQL.Add(':branchid, :projectid, :moduleid, :hidden)');
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);            
            ParamByName('projectid').AsInteger := ProjectID;
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('hidden').AsString := '0';
            ExecSQL;
          end; // if not IsAlreadyAssigned then begin
        end; // if NeedAssign then begin
        // update project information
        if IsNewModule or (NeedAssign and not IsAlreadyAssigned) then
        begin
          UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
        end; // if IsNewModule or NeedAssign then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog and
       (IsNewModule or (NeedAssign and not IsAlreadyAssigned)) then
      BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, 0, 'a', 'Added as new module');
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
procedure TServerObjectBRANCH_GET_MODULE_NAME.Execute;
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
        SQL.Add('SELECT path, name FROM brmodule');
        SQL.Add('WHERE moduleid = :moduleid');
        SQL.Add('AND branchid = :branchid');        
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
procedure TServerObjectBRANCH_GET_MODULE_ID.Execute;
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
        SQL.Add('SELECT moduleid FROM brmodule');
        SQL.Add('WHERE branchid = :branchid AND name = :name');
        SQL.Add('AND path = :path');
        ParamByName('name').AsString := ExtractFileName(Module);
        ParamByName('path').AsString := ExtractFilePath(Module);
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
procedure TServerObjectBRANCH_IS_MEMBER_OF_PROJECT.Execute;
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
        SQL.Add('SELECT brmodule.moduleid');
        SQL.Add('FROM pjmodule, brmodule');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.name = :module');
        SQL.Add('AND brmodule.path = :path');
        SQL.Add('AND pjmodule.moduleid = brmodule.moduleid');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('module').AsString := ExtractFileName(Module);
        ParamByName('path').AsString := ExtractFilePath(Module);
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
procedure TServerObjectBRANCH_REMOVE_MODULE.Execute;
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
  IsSharedBetweenBranches,
  RemoveVersions: Boolean;
  ModuleName,
  LogMessage: string;
  {$IFDEF MYSQLSRV}
  sAllRevisionIDs: string;
  sSubSelect: string;
  {$ENDIF MYSQLSRV}
  ProjectIDForLog, ModuleIDForLog: Integer;
  NewLatestRevisionState: Int64;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // is this a shared module?
        Close;
        SQL.Clear;
        SQL.Add('SELECT count(*) from brmodule');
        SQL.Add('WHERE moduleid = :moduleid');
        ParamByName('moduleid').AsInteger := ModuleID;
        Open;
        IsSharedBetweenBranches := Fields[0].AsInteger > 1;
        // is this a shared module?
        Close;
        SQL.Clear;
        SQL.Add('SELECT pjmodule.projectid, projects.name');
        SQL.Add('FROM pjmodule, projects');
        SQL.Add('WHERE pjmodule.moduleid = :moduleid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND pjmodule.projectid <> :projectid');
        SQL.Add('AND pjmodule.projectid = projects.projectid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
        NewLatestRevisionState := GetNewLatestRevisionState(SessionList.GetSessionBranchID(AccessID, TANr));
        if not IsSharedModule then
        begin
          // this is not a shared module, we can delete it, return true
          FResponseBuffer.WriteFields(True, [True]);
          // remove the module physically from the archive?
          if RemoveVersions then
          begin
            if not IsSharedBetweenBranches then
            begin
              {  delete all bug links for this moduleid to prevent foreign
                 key violation  }
              SQL.Clear;
              SQL.Add('DELETE FROM mdbugs');
              SQL.Add('WHERE moduleid = :moduleid');
              ParamByName('moduleid').AsInteger := ModuleID;
              ExecSQL;
            end;
            {  delete all log comment links assigned with this module to
               prevent foreign key violation }
            {$IFDEF MYSQLSRV} // no subselects in MySQL 3.x
            SQL.Text := 'SELECT revisionid FROM revision WHERE moduleid = :moduleid';
            Open;
            sSubSelect := '(';
            sAllRevisionIDs := '(';
            while not Eof do
            begin
              if IsOnlyRevisionOfBranch(Fields[0].AsInteger, SessionList.GetSessionBranchID(AccessID, TANr)) then
                sSubSelect := sSubSelect + Fields[0].AsString + ',';
              sAllRevisionIDs := sAllRevisionIDs + Fields[0].AsString + ',';
              Next;
            end; // while not EoF do begin
            sSubSelect[Length(sSubSelect)] := ')';
            sAllRevisionIDs[Length(sAllRevisionIDs)] := ')';
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
            if not IsSharedBetweenBranches then
            begin
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
            end;
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

            {$IFDEF MYSQLSRV}
            if Length(sAllRevisionIDs) > 2 then
            begin
              SQL.Clear;
              SQL.Add('DELETE FROM rvbranch');
              SQL.Add('WHERE revisionid in ' + sAllRevisionIDs);
              SQL.Add('AND branchid = :branchid');
              ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
              ExecSQL;
              // return the removed revisions
              FResponseBuffer.WriteFields(True, [RowsAffected]);
            end
            else
            begin
              // return the removed revisions
              FResponseBuffer.WriteFields(True, [0]);
            end;

            // delete all revisions assigned with this module
            if not IsSharedBetweenBranches then
            begin
              SQL.Clear;
              SQL.Add('DELETE FROM revision');
              SQL.Add('WHERE moduleid = :moduleid');
              ParamByName('moduleid').AsInteger := ModuleID;
              ExecSQL;
            end
            else
            if Length(sSubSelect) > 2 then
            begin
              SQL.Clear;
              SQL.Add('DELETE FROM revision');
              SQL.Add('WHERE revisionid in ' + sSubSelect);
              ExecSQL;
            end;
            {$ELSE}
            //todo
            {$ENDIF MYSQLSRV}
          end;
          // delete the module link
          SQL.Clear;
          SQL.Add('DELETE FROM pjmodule');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND pjmodule.branchid = :branchid');
          SQL.Add('AND moduleid = :moduleid');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ExecSQL;
          if RemoveVersions then
          begin
            SQL.Clear;
            SQL.Add('DELETE FROM brmodule');
            SQL.Add('WHERE moduleid = :moduleid');
            SQL.Add('AND branchid = :branchid');
            ParamByName('moduleid').AsInteger := ModuleID;
            ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
            ExecSQL;
            if not IsSharedBetweenBranches then
            begin
              // finally delete the module
              SQL.Clear;
              SQL.Add('DELETE FROM modules');
              SQL.Add('WHERE moduleid = :moduleid');
              ParamByName('moduleid').AsInteger := ModuleID;
              ExecSQL;
            end;
          end; // if RemoveVersions then begin
        end // if not IsSharedModule then begin
        else
        begin
          // shared module: delete only the module link
          SQL.Clear;
          SQL.Add('DELETE FROM pjmodule');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND pjmodule.branchid = :branchid');
          SQL.Add('AND moduleid = :moduleid');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ExecSQL;
        end; // else if not IsSharedModule then begin
        Close;  // commit transaction to prevent error in following updates 
        // update project information
        UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
        UpdateLatestModuleRevisions(UData^, NewLatestRevisionState, SessionList.GetSessionBranchID(AccessID, TANr), ModuleID);
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
      if RemoveVersions and (not IsSharedBetweenBranches) then
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
      BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectIDForLog, UserID, ModuleIDForLog, 0, 'r', LogMessage);
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
procedure TServerObjectBRANCH_GET_REVISION_LIST.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
        SQL.Add('brmodule.readonly, modules.tstamp, brmodule.userid,');
        SQL.Add('revision.revisionid, revision.version, revision.revision,');
        SQL.Add('pjmodule.hidden, users.login');
        SQL.Add('FROM pjmodule');
        SQL.Add('INNER JOIN modules  ON pjmodule.moduleid = modules.moduleid');
        SQL.Add('INNER JOIN brmodule ON (brmodule.branchid = :branchid) and (brmodule.moduleid = modules.moduleid)');
        SQL.Add('INNER JOIN revision ON pjmodule.moduleid = revision.moduleid');
        SQL.Add('INNER JOIN rvbranch ON (rvbranch.branchid = :branchid) and (rvbranch.revisionid = revision.revisionid)');
        SQL.Add('LEFT OUTER JOIN users ON brmodule.userid = users.userid');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        if ExclHidden then // skip hiddden modules
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY brmodule.name, modules.moduleid,');
        SQL.Add('revision.version, revision.revision');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
procedure TServerObjectBRANCH_GET_LOCKED_MODULES.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name,');
        SQL.Add('brmodule.path, revision.version, revision.revision,');
        SQL.Add('brmodule.locktstamp, users.login, users.userid');
        SQL.Add('FROM pjmodule, modules, brmodule, revision, rvbranch, users');
        if ProjectID > 0 then
        begin
          SQL.Add('WHERE pjmodule.projectid = :projectid');
          SQL.Add('AND pjmodule.branchid = :branchid');
          SQL.Add('AND brmodule.readonly = ''1''');
        end else SQL.Add('WHERE brmodule.readonly = ''1''');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        if UserID > 0 then
          SQL.Add('AND users.userid = :userid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND users.userid = brmodule.userid');
        SQL.Add('ORDER BY brmodule.name DESC, modules.moduleid DESC,');
        SQL.Add('revision.version DESC, revision.revision DESC');
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);          
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
  Search for modules in the version archive - wildcard (*, ?) support

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Mask string            - String(255)

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Path                   - String(255)
               [3]Assigned to project    - String(50)

            1: [next record...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectBRANCH_SEARCH_MODULES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery,
  FQuery2: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
  MaskStr,
  MaskPath,
  MaskName,
  MaskExt,
  ModulePath,
  ModuleName,
  ModuleExt,
  SQLParam: string;
  Match: Boolean;
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

    MaskStr := AnsiLowerCase(FRequestBuffer.Fields[0]);
    MaskPath := ExtractFilePath(MaskStr);
    MaskName := ChangeFileExt(ExtractFileName(MaskStr), '');
    if MaskName = '*' then
      MaskName := '';
    MaskExt := ExtractFileExt(MaskStr);
    if (MaskExt <> '') and (MaskExt[1] = '.') then
      MaskExt := Copy(MaskExt, 2, Length(MaskExt)-1);
    if MaskExt = '*' then
      MaskExt := '';

    SQLParam := 'WHERE (modules.moduleid IN (';

    FResponseBuffer.Rewrite;
    // get all module ID's and names in the archive
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      FQuery.SetTransactionNoAuto;  // we want next all in one transaction
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT moduleid, name, path');
        SQL.Add('FROM brmodule');
        SQL.Add('WHERE branchid = :branchid');
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
        Open;
        while not Eof do
        begin
          // compare the module name to our mask string
          ModuleName := AnsiLowerCase(FieldByName('name').AsString);
          ModulePath := AnsiLowerCase(FieldByName('path').AsString);

          // CS, 11.01.2005: Use StrMatches function from JCL
          // maybe there is a better way to solve this, but it works
          if MaskPath<>'' then
            Match := StrMatches(MaskPath, ModulePath)
          else
            Match := True;
          if Match and (MaskName <> '') then
            Match := StrMatches(MaskName, ChangeFileExt(ExtractFileName(ModuleName), ''));
          if Match and (MaskExt <> '') then
          begin
            ModuleExt := ExtractFileExt(ModuleName);
            if (ModuleExt <> '') and (ModuleExt[1] = '.') then
              ModuleExt:= Copy(ModuleExt, 2, Length(ModuleExt)-1);
            Match := StrMatches(MaskExt, ModuleExt);
          end;

          // module name matches mask string?
          if Match then
          begin
            FQuery2 := TSrvQuery.Create(Self);
            try
              FQuery2.DatabaseName := UData.DBPath;
              // don't assign transaction from FQuery as FQuery2 will be closed
              // and FQuery needs it's transaction in it's loop!

              {(* at least one module matches our search string *)
              ModulesFound := true;
              (* insert module ID into SQL string *)
              SQLParam := SQLParam + ' ' +
                                IntToStr(Integer(FieldByName('moduleid'))) + ',';}
              // yes, get information for this module
              FQuery2.Close;
              FQuery2.SQL.Clear;
              FQuery2.SQL.Add('SELECT brmodule.path, projects.name');
              FQuery2.SQL.Add('FROM brmodule, projects, pjmodule');
              FQuery2.SQL.Add('WHERE brmodule.moduleid = :moduleid');
              FQuery2.SQL.Add('AND brmodule.branchid = :branchid');
              FQuery2.SQL.Add('AND pjmodule.moduleid = brmodule.moduleid');
              FQuery2.SQL.Add('AND pjmodule.branchid = :branchid');
              FQuery2.SQL.Add('AND projects.projectid = pjmodule.projectid');
              FQuery2.ParamByName('moduleid').AsInteger := FieldByName('moduleid').AsInteger;
              FQuery2.ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
              FQuery2.Open;
              while not FQuery2.Eof do
              begin
                // return module ID & name
                FResponseBuffer.WriteFields(True, [FieldByName('moduleid').AsInteger]);
                FResponseBuffer.WriteFields(False, [ModuleName]);
                // Copy the record to the response
                for Fld := 0 to FQuery2.FieldCount - 1 do
                  FResponseBuffer.WriteFields(False, [FQuery2.Fields[Fld].AsString]);
                FQuery2.Next;
              end; // while not FQuery2.EoF do begin
              FQuery2.Close;
            finally
              FQuery2.Free;
            end;
          end; // if Match then begin
          Next; // FQuery
        end; // while not EoF do begin
        Close; // FQuery
        // any modules matches our search string?
        {if ModulesFound then
        begin
          (* remove the last ',' *)
          if SQLParam[Length(SQLParam)] = ',' then
            System.Delete(SQLParam, Length(SQLParam), 1);
          (* complete SQL string *)
          SQLParam := SQLParam + '))';
          (* get more information for all these modules *)
          Close;
          SQL.Clear;
          SQL.Add('SELECT Modules.moduleid, Modules.name,');
          SQL.Add('Modules.path, Projects.name');
          SQL.Add('FROM Modules, Projects, PJModule');
          SQL.Add(SQLParam);
          SQL.Add('AND PJModule.moduleid = Modules.moduleid');
          SQL.Add('AND Projects.projectid = PJModule.projectid');}
          {Open;
          while not EoF do
          begin
            (* Copy the record to the response *)
            for Fld := 0 to FQuery2.FieldCount - 1 do
              FResponseBuffer.WriteFields(Fld = 0, [Fields[Fld].AsString]);
            Next;
          end; // while not FQuery2.EoF do begin
          Close;
        end; // if ModulesFound then begin}
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
procedure TServerObjectBRANCH_GET_REVISION_LIST_BY_VERSION.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
        SQL.Add('brmodule.readonly, brmodule.locktstamp, revision.revisionid,');
        SQL.Add('revision.version, revision.revision, users.login');
        SQL.Add('FROM pjmodule, revision, rvbranch, modules, brmodule, users');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');        
        SQL.Add('AND revision.version = :version');
        if Revision > -1 then // all revisions?
          SQL.Add('AND revision.revision = :revision');
        SQL.Add('AND pjmodule.moduleid = modules.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND users.userid = brmodule.userid');
        SQL.Add('ORDER BY modules.moduleid DESC, revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('version').AsInteger := Version_;
        if Revision > -1 then
          ParamByName('revision').AsInteger := Revision;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_REMOVE_REVISION.Execute;
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
  RevisionID,
  ProjectID: Integer;
  IsSharedModule: Boolean;
  ModuleName: string;
  WasSharedRevision: Boolean;
  NewLatestRevisionState: Int64;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // is this a shared module?
        IsSharedModule := False;//for now we don't care about a shared more(see mantis #1279)
        {
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
        }
        NewLatestRevisionState := GetNewLatestRevisionState(SessionList.GetSessionBranchID(AccessID, TANr));
        if not IsSharedModule then
        begin
          // this is not a shared module, we can delete it, return true
          FResponseBuffer.WriteFields(True, [True]);
          WasSharedRevision := IsSharedRevision(RevisionID);
          SQL.Clear;
          SQL.Add('DELETE FROM rvbranch');
          SQL.Add('WHERE branchid = :branchid');
          SQL.Add('AND revisionid = :revisionid');
          ParamByName('revisionid').AsInteger := RevisionID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ExecSQL;

          if not WasSharedRevision then
          begin
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
          end;
        end; // if not IsSharedModule then begin
        // update project information
        UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
        UpdateLatestModuleRevisions(UData^, NewLatestRevisionState, SessionList.GetSessionBranchID(AccessID, TANr), ModuleID);
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
      BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, 0, 'r',
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
procedure TServerObjectBRANCH_REMOVE_VERSION.Execute;
const
  RequestedRights = LevelRW;
var
  UData: PUserDataRecord;
  FQuery,
  FQuery2: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
//Fld,
  UserID,
  ModuleID,
  ProjectID,
  Version_ : Integer; // clash with TFFQuery.Version
{$IFDEF UIBSRV}
  RemovedFiles: Cardinal;
{$ELSE}
  RemovedFiles: Integer;
{$ENDIF}
  IsSharedModule: Boolean;
  ModuleName: string;
  WasSharedRevision: Boolean;
  NewLatestRevisionState: Int64;
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
      FQuery.SetTransactionNoAuto; // we want next all in one transaction
      with FQuery do
      begin
        // is this a shared module?
        IsSharedModule := False;//for now we don't care about a shared more(see mantis #1279)
        {
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
        }
        NewLatestRevisionState := GetNewLatestRevisionState(SessionList.GetSessionBranchID(AccessID, TANr));
        if not IsSharedModule then
        begin
          // this is not a shared module, we can delete it, return true
          FResponseBuffer.WriteFields(True, [True]);
          FQuery2 := TSrvQuery.Create(Self);
          try
            FQuery2.DatabaseName := UData.DBPath;
            FQuery2.SetTransaction(FQuery);  // we want next all in one transaction
            // get all revisions assigned to this version
            FQuery2.Close;
            FQuery2.SQL.Clear;
            FQuery2.SQL.Add('SELECT revision.revisionid');
            FQuery2.SQL.Add('FROM revision, rvbranch');
            FQuery2.SQL.Add('WHERE revision.moduleid = :moduleid');
            FQuery2.SQL.Add('AND revision.version = :version');
            FQuery2.SQL.Add('AND rvbranch.branchid = :branchid');
            FQuery2.SQL.Add('AND rvbranch.revisionid = revision.revisionid');
            FQuery2.ParamByName('moduleid').AsInteger := ModuleID;
            FQuery2.ParamByName('version').AsInteger := Version_;
            FQuery2.ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
            FQuery2.Open;
            while not FQuery2.Eof do
            begin
              // delete all log comment links assigned with this module
              WasSharedRevision := IsSharedRevision(FQuery2.FieldByName('revisionid').AsInteger);
              SQL.Clear;
              SQL.Add('DELETE FROM rvbranch');
              SQL.Add('WHERE branchid = :branchid');
              SQL.Add('AND revisionid = :revisionid');
              ParamByName('revisionid').AsInteger := FQuery2.FieldByName('revisionid').AsInteger;
              ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
              ExecSQL;
              if not WasSharedRevision then
              begin
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
                // delete the revision assigned with this module
                SQL.Clear;
                SQL.Add('DELETE FROM revision');
                SQL.Add('WHERE revisionid = :revisionid');
                ParamByName('revisionid').AsInteger := FQuery2.FieldByName('revisionid').AsInteger;
                ExecSQL;
              end;
              FQuery2.Next;
            end; // while not FQuery2.EoF do begin
            FQuery2.Close;
          finally
            FQuery2.Free;
          end;
          // return the removed revisions
          FResponseBuffer.WriteFields(True, [RemovedFiles]);
        end; // if not IsSharedModule then begin
        // update project information
        UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
        UpdateLatestModuleRevisions(UData^, NewLatestRevisionState, SessionList.GetSessionBranchID(AccessID, TANr), ModuleID);
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // update log file
    if UData.WriteVCSLog then
      BranchVCSLog(UData.DBPath, SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID, ModuleID, 0, 'r',
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
procedure TServerObjectBRANCH_HIDE_UNHIDE_MODULE.Execute;
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
      FQuery.SetTransactionNoAuto;
      with FQuery do
      begin
        // check if the module is checked out
        Close;
        SQL.Clear;
        SQL.Add('SELECT readonly FROM brmodule');
        SQL.Add('WHERE moduleid = :moduleid');
        SQL.Add('AND branchid = :branchid');
        ParamByName('moduleid').AsInteger := ModuleID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
          SQL.Add('AND branchid = :branchid');
          if ProjectID > 0 then
            SQL.Add('AND projectid = :projectid');
          ParamByName('moduleid').AsInteger := ModuleID;
          if ProjectID > 0 then
            ParamByName('projectid').AsInteger := ProjectID;
          if Hide
            then ParamByName('hidden').AsString := '1'
              else ParamByName('hidden').AsString := '0';
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ExecSQL;
          // update project information
          UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
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
procedure TServerObjectBRANCH_COPY_REVISION.Execute;
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
  {$ENDIF}
  ExceptionMessage: string;
  NewLatestRevisionState: Int64;
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

    if not BranchModuleVersionRevisionExists(SessionList.GetSessionBranchID(AccessID, TANr), ModuleID, TargetVersion, TargetRevision) then
    begin
      NewLatestRevisionState := GetNewLatestRevisionState(SessionList.GetSessionBranchID(AccessID, TANr));
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        FQuery.SetTransactionNoAuto;
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
          SQL.Add('SELECT MAX(revisionid) FROM revision');
          SQL.Add('WHERE moduleid = :moduleid AND version = :version');
          SQL.Add('AND revision = :revision');
          ParamByName('moduleid').AsInteger := ModuleID;
          ParamByName('version').AsInteger := TargetVersion;
          ParamByName('revision').AsInteger := TargetRevision;
          Open;
          NewRevisionID := FQuery.Fields[0].AsInteger;
          Close;

          SQL.Clear;
          SQL.Add('INSERT INTO rvbranch');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('rvbranch')]));
          SQL.Add(':branchid, :revisionid, ''s'')');
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ParamByName('revisionid').AsInteger := NewRevisionID;
          ExecSQL;

          FQuery2 := TSrvQuery.Create(Self);
          try
            FQuery2.DatabaseName := UData.DBPath;
            FQuery2.SetTransaction(FQuery);
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
          UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
          UpdateLatestModuleRevisions(UData^, NewLatestRevisionState, SessionList.GetSessionBranchID(AccessID, TANr), ModuleID);
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
procedure TServerObjectBRANCH_MERGE_VER_REV_NR.Execute;
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
  Comment_I, Comment_O: string;
  RevisionBranchCount: Integer;
  FoundRevision: Boolean;
  ExceptionMessage: string;
  NewLatestRevisionState: Int64;
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

    if not BranchModuleVersionRevisionExists(SessionList.GetSessionBranchID(AccessID, TANr), ModuleID, TargetVersion, TargetRevision) then
    begin
      NewLatestRevisionState := GetNewLatestRevisionState(SessionList.GetSessionBranchID(AccessID, TANr));
      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        FQuery.SetTransactionNoAuto;
        with FQuery do
        begin
          RevisionBranchCount := GetRevisionBranchCount(SourceRevisionID);
          if RevisionBranchCount = 1 then
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
          end
          else
          if RevisionBranchCount > 1 then
          begin
            SQL.Clear;
            SQL.Add('SELECT ideversion, comment_i, comment_o from revision');
            SQL.Add('WHERE revisionid = :revisionid');
            ParamByName('revisionid').AsInteger := SourceRevisionID;
            Open;
            FoundRevision := not Eof;
            IDEVersion := 0;
            if FoundRevision then
            begin
              IDEVersion := Fields[0].AsInteger;
              Comment_I := Fields[1].AsString;
              Comment_O := Fields[2].AsString;
            end;
            Close;

            if FoundRevision then
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
              ParamByName('comment_i').AsString := Comment_I;
              ParamByName('comment_o').AsString := Comment_O;
              ExecSQL;
              // get the new revision ID
              SQL.Clear;
              SQL.Add('SELECT MAX(revisionid) FROM revision');
              SQL.Add('WHERE moduleid = :moduleid AND version = :version');
              SQL.Add('AND revision = :revision');
              ParamByName('moduleid').AsInteger := ModuleID;
              ParamByName('version').AsInteger := TargetVersion;
              ParamByName('revision').AsInteger := TargetRevision;
              Open;
              NewRevisionID := FQuery.Fields[0].AsInteger;
              Close;

              SQL.Clear;
              SQL.Add('INSERT INTO rvbranch');
              SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('rvbranch')]));
              SQL.Add(':branchid, :revisionid, ''s'')');
              ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
              ParamByName('revisionid').AsInteger := NewRevisionID;
              ExecSQL;

              FQuery2 := TSrvQuery.Create(Self);
              try
                FQuery2.DatabaseName := UData.DBPath;
                FQuery2.SetTransaction(FQuery);
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

              SQL.Clear;
              SQL.Add('DELETE FROM rvbranch');
              SQL.Add('WHERE branchid = :branchid');
              SQL.Add('AND revisionid = :revisionid');
              ParamByName('revisionid').AsInteger := SourceRevisionID;
              ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
              ExecSQL;
            end;
          end;
          // update project information
          UpdateProjectLastAccess(SessionList.GetSessionBranchID(AccessID, TANr), ProjectID, UserID);
          UpdateLatestModuleRevisions(UData^, NewLatestRevisionState, SessionList.GetSessionBranchID(AccessID, TANr), ModuleID);
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
procedure TServerObjectBRANCH_GET_VERSION_REVISION.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
        SQL.Add('revision.revisionid, revision.version,');
        SQL.Add('revision.revision, blobs.origtime,');
        SQL.Add('blobs.extension, blobs.origcrc');
        SQL.Add('FROM pjmodule, modules, brmodule, revision, rvbranch, blobs');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        SQL.Add('AND revision.version = :version');
        if Revision > -1 then // filter by revision?
          SQL.Add('AND revision.revision = :revision');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY brmodule.name DESC, revision.version DESC,');
        SQL.Add('revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('version').AsInteger := Version_;
        if Revision > -1 then // filter by revision?
          ParamByName('revision').AsInteger := Revision;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);          
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
procedure TServerObjectBRANCH_GET_ROLLBACK_REVISIONS.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
        SQL.Add('revision.revisionid, revision.version,');
        SQL.Add('revision.revision, blobs.origtime,');
        SQL.Add('blobs.extension, blobs.origcrc');
        SQL.Add('FROM pjmodule, modules, brmodule, revision, rvbranch, blobs');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY brmodule.name DESC, modules.moduleid DESC,');
        SQL.Add('revision.revisionid DESC, revision.version DESC,');
        SQL.Add('revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
  Purge - free space by removing unnecessary (older) modules from the DB.
  Purge all revisions from the specified project, keep max. "Number of Rev. to
  keep" in the archive.
  When called with "Execute = false" the object does not really delete anything.
  Clients call this object twice - one time to get the number of affected files,
  second time (if the user has confirmed that he/she really wants to delete) to
  remove the revisions.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project ID             - Integer
               [1]Number of Rev. to keep - Integer
               [2]Execute                - Boolean

  Response: 0: [0]Number of affected files - Integer
               [1]Number of deleted Rev.   - Integer

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectBRANCH_PURGE_PROJECT.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQueryModules,
  FQueryDelete,
  FQueryExecute: TSrvQuery;
  I,
  liDel,
  liRevID,
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  KeepRevisions,
  DeletedRevisions: Integer;
  ExecutePurge: Boolean;
  nRecCount: Integer;
  WasSharedRevision: Boolean;
  NewLatestRevisionState: Int64;
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    NewLatestRevisionState := 0;
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
    KeepRevisions := StrToInt(FRequestBuffer.Fields[1]);
    ExecutePurge := (FRequestBuffer.Fields[2] = '1');

    FResponseBuffer.Rewrite;
    FQueryModules := TSrvQuery.Create(Self);
    try
      FQueryModules.DatabaseName := UData.DBPath;
      FQueryModules.SetTransactionNoAuto;
      with FQueryModules do
      begin
        { @@@ DBISAM: to be tested before use
          select count(*) as revcount, modules.moduleid, modules.name
          from modules, revision, pjmodule
          where pjmodule.projectid = 1000
          and pjmodule.moduleid = modules.moduleid
          and revision.moduleid = modules.moduleid
          group by modules.moduleid, modules.name
          having count(revcount) > 0
        }

        {  Step 1: get all modules containing more revisions than
           'KeepRevisions' }
        SQL.Clear;
        {  DBISAM does not accept the * (all columns) reference in 'having',
           therefore we must use 'revcount' later instead of count(*) }
        SQL.Add('select count(*) as revcount, modules.moduleid');
        SQL.Add('from modules, revision, rvbranch, pjmodule');
        SQL.Add('where pjmodule.projectid = :projectid');
        SQL.Add('and pjmodule.branchid = :branchid');
        SQL.Add('and pjmodule.moduleid = modules.moduleid');
        SQL.Add('and revision.moduleid = modules.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('group by modules.moduleid');
        SQL.Add('having count(*) > ' + IntToStr(KeepRevisions));
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
        Open;
        // are there any modules affected?
        nRecCount := RecordCount; // get recordcount one time
        FResponseBuffer.WriteFields(True, [nRecCount]);
        // really Purge now?
        if ExecutePurge and (nRecCount>0) then
        begin
          // yes
          // Step 2: get the revisions for all affected modules
          FQueryDelete := TSrvQuery.Create(Self);
          FQueryExecute := TSrvQuery.Create(Self);
          DeletedRevisions := 0;
          try
            FQueryDelete.DatabaseName := UData.DBPath;
            FQueryExecute.DatabaseName := UData.DBPath;
            FQueryDelete.SetTransaction(FQueryModules);
            FQueryExecute.SetTransaction(FQueryModules);
            while not Eof do
            begin
              // caculate the number of revisions to delete
              liDel := FieldByName('revcount').AsInteger - KeepRevisions;
              if liDel > 0 then
                NewLatestRevisionState := GetNewLatestRevisionState(SessionList.GetSessionBranchID(AccessID, TANr));
              FQueryDelete.SQL.Clear;
              FQueryDelete.SQL.Add('select revision.revisionid, revision.version, revision.revision'); //MG
              FQueryDelete.SQL.Add('from revision, rvbranch');
              FQueryDelete.SQL.Add('where revision.moduleid = ' +
                                              FieldByName('moduleid').AsString);
              FQueryDelete.SQL.Add('AND rvbranch.branchid = :branchid');
              FQueryDelete.SQL.Add('AND rvbranch.revisionid = revision.revisionid');
              FQueryDelete.SQL.Add('order by revision.version asc, revision.revision asc');  //MG
              FQueryDelete.ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
              FQueryDelete.Open;
              {  Step 3: delete all revisions and all records in other tables
                 dependend on the revision  }
              for I := 1 to liDel do
              begin
                Inc(DeletedRevisions);
                liRevID := FQueryDelete.FieldByName('revisionid').AsInteger;
                {  to avoid foreign key violations start at the bottom of the
                   dependencies  }
                WasSharedRevision := IsSharedRevision(liRevID);
                FQueryExecute.SQL.Clear;
                FQueryExecute.SQL.Add('DELETE FROM rvbranch');
                FQueryExecute.SQL.Add('WHERE branchid = :branchid');
                FQueryExecute.SQL.Add('AND revisionid = :revisionid');
                FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                FQueryExecute.ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
                FQueryExecute.ExecSQL;
                if not WasSharedRevision then
                begin
                  FQueryExecute.SQL.Text :=
                               'delete from blobs where revisionid = :revisionid';
                  FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                  FQueryExecute.ExecSQL;
                  FQueryExecute.SQL.Text :=
                            'delete from rvlabels where revisionid = :revisionid';
                  FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                  FQueryExecute.ExecSQL;
                  FQueryExecute.SQL.Text :=
                            'delete from logcomm where revisionid = :revisionid';
                  FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                  FQueryExecute.ExecSQL;
                  FQueryExecute.SQL.Text :=
                            'delete from revision where revisionid = :revisionid';
                  FQueryExecute.ParamByName('revisionid').AsInteger := liRevID;
                  FQueryExecute.ExecSQL;
                end;
                FQueryDelete.Next; // -> next revision
                if FQueryDelete.Eof then Break;
              end;
              FQueryDelete.Close;
              if liDel > 0 then
                UpdateLatestModuleRevisions(UData^, NewLatestRevisionState, SessionList.GetSessionBranchID(AccessID, TANr), FieldByName('moduleid').AsInteger);
              Next; // -> next module
            end; // while not (FQueryModules) EOF do
          finally
            FQueryDelete.Free;
            FQueryExecute.Free;
          end;
          FResponseBuffer.WriteFields(False, [DeletedRevisions]);
          // update archive time stamp
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);
        end; // if ExecutePurge and (nRecCount>0) then ...
        Close; // FQueryModules
      end; // with FQueryModules do
    finally
      FQueryModules.Free;
    end;
    FResultStatus := 200;
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
procedure TServerObjectBRANCH_LABEL_USED_BY.Execute;
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
        SQL.Add('SELECT brmodule.name, revision.version,');
        SQL.Add('revision.revision');
        SQL.Add('FROM rvlabels, modules, brmodule, revision, rvbranch');
        SQL.Add('WHERE rvlabels.labelid = :labelid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND rvlabels.revisionid = revision.revisionid');
        SQL.Add('AND revision.moduleid = modules.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        ParamByName('labelid').AsInteger := LabelID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_FAMILY_USED_BY.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name FROM modules, brmodule');
        SQL.Add('WHERE modules.familyid = :familyid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        ParamByName('familyid').AsInteger := FamilyID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
procedure TServerObjectBRANCH_BUGS_USED_BY.Execute;
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
        SQL.Add('SELECT modules.moduleid, brmodule.name');
        SQL.Add('FROM mdbugs, modules, brmodule');
        SQL.Add('WHERE mdbugs.bugid = :bugid');
        SQL.Add('AND mdbugs.moduleid = modules.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        ParamByName('bugid').AsInteger := BugID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
  Get a list of all 'deserted' modules (modules without a parent project)

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]Module ID              - Integer
               [1]Module name            - String(255)
               [2]Module path            - String(255)

            1: [next record...]
               [...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectBRANCH_GET_DESERTED_MODULES.Execute;
const
  RequestedRights = LevelRO;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  Fld: Integer;
  slSqlIn, slSqlJoin: TStringList;
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

    slSqlIn := TStringList.Create;
    slSqlJoin := TStringList.Create;
    try
      // SQL, if DBMS supports subselect
      //todo
      slSqlIn.Add('SELECT modules.moduleid, brmodule.name, brmodule.path FROM modules, brmodule');
      slSqlIn.Add('WHERE modules.moduleid NOT IN (SELECT pjmodule.moduleid FROM pjmodule where branchid = :branchid)');
      slSqlIn.Add('AND brmodule.branchid = :branchid');
      slSqlIn.Add('AND brmodule.moduleid = modules.moduleid');
      // SQL, if DBMS needs Join workaround
      slSqlJoin.Add('SELECT modules.moduleid, brmodule.name, brmodule.path FROM modules');
      slSqlJoin.Add('INNER JOIN brmodule ON (brmodule.branchid = :branchid) and (brmodule.moduleid = modules.moduleid)');
      slSqlJoin.Add('LEFT OUTER JOIN pjmodule ON (modules.moduleid=pjmodule.moduleid) AND (pjmodule.branchid = :branchid)');
      slSqlJoin.Add('WHERE pjmodule.moduleid IS NULL');

      FQuery := TSrvQuery.Create(Self);
      try
        FQuery.DatabaseName := UData.DBPath;
        with FQuery do
        begin
          Close;
          {$IFNDEF MYSQLSRV}
            {$IFNDEF UIBSRV}
            SQL.Assign(slSqlIn);
            {$ELSE}
            SQL.Assign(slSqlJoin);
            {$ENDIF ~UIBSRV}
          {$ELSE}
          SQL.Assign(slSqlJoin);
          {$ENDIF ~MYSQLSRV}
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          Open;
          // Step 3: return only modules w/o project link
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
    finally
      slSqlIn.Free;
      slSqlJoin.Free;
    end;
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
procedure TServerObjectBRANCH_GET_LOG_ENTRIES.Execute;
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
      {$IFNDEF UIBSRV}
      SQLParam := 'AND vcslog.type in (''' + EntryType[1] + '''';
      {$ELSE}
      SQLParam := 'AND vcslog.logtype in (''' + EntryType[1] + '''';
      {$ENDIF ~UIBSRV}
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
        SQL.Add('SELECT projects.name, brmodule.name, users.login,');
        // "type" is a reserved word in Firebird / Interbase 6/7
        {$IFNDEF UIBSRV}
        SQL.Add('vcslog.tstamp, vcslog.type, vcslog.description,');
        {$ELSE}
        SQL.Add('vcslog.tstamp, vcslog.logtype, vcslog.description,');
        {$ENDIF ~UIBSRV}
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
        if ModuleID > 0 then
          SQL.Add('INNER JOIN brmodule ON (brmodule.branchid = :branchid) and (brmodule.moduleid = modules.moduleid)')
        else
          SQL.Add('LEFT OUTER JOIN brmodule ON (brmodule.branchid = :branchid) and (brmodule.moduleid = modules.moduleid)');
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
        SQL.Add('AND vcslog.branchid = :branchid');
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
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_GET_LOG_FILTER.Execute;
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
      FQuery.SetTransactionNoAuto;
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
        SQL.Add('AND vcslog.branchid = :branchid');
        SQL.Add('ORDER BY projects.name');
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
        SQL.Add('SELECT DISTINCT vcslog.moduleid, brmodule.name');
        SQL.Add('FROM modules, brmodule, vcslog');
        SQL.Add('WHERE modules.moduleid = vcslog.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND vcslog.branchid = :branchid');
        SQL.Add('ORDER BY brmodule.name');
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
        SQL.Add('AND vcslog.branchid = :branchid');
        SQL.Add('ORDER BY users.login');
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_REMOVE_LOG_ENTRIES.Execute;
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
      FQuery.SetTransactionNoAuto; // we want next all in one transaction
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
        SQL.Add('AND branchid = :branchid');
        ParamByName('tstamp').AsDateTime := ExpDate;
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if ModuleID > 0 then
          ParamByName('moduleid').AsInteger := ModuleID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
        Open;
        FQuery2 := TSrvQuery.Create(Self);
        try
          FQuery2.DatabaseName := UData.DBPath;
          FQuery2.SetTransaction(FQuery);
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
        SQL.Add('AND branchid = :branchid');          
        ParamByName('tstamp').AsDateTime := ExpDate;
        if ProjectID > 0 then
          ParamByName('projectid').AsInteger := ProjectID;
        if ModuleID > 0 then
          ParamByName('moduleid').AsInteger := ModuleID;
        if UserID > 0 then
          ParamByName('userid').AsInteger := UserID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);          
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
procedure TServerObjectBRANCH_ADD_UPDATE_PROJECT_REFERENCES.Execute;
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
      FQuery.SetTransactionNoAuto;
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
          SQL.Add('AND branchid = :branchid');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
          ExecSQL;
          UData.ArchiveTimeStamp := LocalDT2GMTDT(Now);   // signal updated archive
        end; // if UpDate then begin
        // add new entries
        while not FRequestBuffer.Eof do
        begin
          SQL.Clear;
          if GetInsertKeyFieldValueStr('pjref') <> '' then
            SQL.Add('INSERT INTO pjref(recordid, branchid, projectid, reference)')
          else
            SQL.Add('INSERT INTO pjref(branchid, projectid, reference)');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('pjref')]));
          SQL.Add(':branchid, :projectid, :reference)');
          ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_GET_PROJECT_REFERENCES.Execute;
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
        SQL.Add('AND pjref.branchid = :branchid');
        SQL.Add('ORDER BY projects.name');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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
procedure TServerObjectBRANCH_REMOVE_PROJECT_REFERENCES.Execute;
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
        SQL.Add('AND pjref.branchid = :branchid');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('reference').AsInteger := ReferenceID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);        
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
  Server Login
  Login with user name & password.
  User's password is encrypted using the servers local timestamp as (a part
  of) the encryption key.
  Due to this a password submitted by a client is only valid within one minute.
  Server login may be also restricted to the stored client IP address. All users
  with a stored IP of '0' can login from any client.
  Server return accepted or not accepted as boolean value, the user's ID, a
  transaction number for client requests and the server's login message (any
  text message like 'user accepted...')
  For each request client sends user ID and transaction number back to the
  server objects. The combination of user ID and transaction number is the
  access key for all server objects.
  Transaction numbers are valid until the user logs out, changes the project
  (ask for a new projectID) or the predifined expiration time was reached.
  The server removes such expired numbers automatically.

        Record  Field
        ======  =====
  Request:  0: [0]User name              - String(50)
               [1]User password          - String(200)
               [2]Client  version        - Integer
               [3]Clint timestamp        - Double

  Response: 0: [0]accepted?              - Boolean
               [1]User ID                - Integer
                  (return -1 if not accepted)
               [2]Transaction number     - Integer
                  (access key for client request - client will include this
                   number in any server requests, return -1 if not accepted)
               [3]Login message           - String(50)
                  (optional, may be blank)
               [4]Server version          - Integer
               [5]Server type             - String

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectLOGIN.Execute;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  User,
  Password,
  CliVersion,
  IPAddr,
  StoredPassword,
  StoredIPAddr,
  ErrorMsg,
  LevelMsg: string;
  CliTimeStamp: Double;
  Rights,
  UserID,
  TANr: Integer;
  ForceGlobalIni,
  Accepted: Boolean;
  IniFile: TIniFile;
  Session: TJVCSSession;  

  {  compare the clients IP address to the stored IP address
     - this function allows the use of IP masks like 192.168.0.* or
       192.168.0.2?  }
  function CheckIPMask(ClientIP, StoredIP: string): Boolean;
  var
    I, J: Integer;
  begin
    Result := True;
    try
      ClientIP := AnsiLowerCase(ClientIP);
      StoredIP := AnsiLowerCase(StoredIP);
      I := 0;
      J := 0;
      if (Pos('*', StoredIP) = 0) and (Length(StoredIP) <> Length(ClientIP))
        then Result := False
      else
      begin
        repeat // until (not Result) or...
          Inc(I);
          Inc(J);
          if StoredIP[I] = '*' then
          begin
            // '*' = last position of StoredIP?
            if (I < Length(StoredIP)) then
            begin // no
              Inc(I);
              while (J < Length(ClientIP)) and
                    (StoredIP[I] <> ClientIP[J]) do Inc(J);
              Result := (StoredIP[I] = ClientIP[J]);
            end;
          end // if StoredIP[i] = '*' then begin
          else
          begin
            if (StoredIP[I] <> '?') then
              Result := (StoredIP[I] = ClientIP[J]);
          end;
        until (not Result) or
              (J >= Length(ClientIP)) or (I >= Length(StoredIP));
      end; // else if (Pos('*', StoredIP) = 0) and...
      if Result then
        if (StoredIP[I] <> '*') then
          Result := (J = Length(ClientIP)) and (I = Length(StoredIP));
    except
      Result := False;
    end;
  end; // function CheckIPMask(...

begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    UserID := 0;
    TANr := 0;
    Rights := 0;
    LevelMsg := '';
    ErrorMsg := '';
    UData := PUserDataRecord(FUserData);
    User := FRequestBuffer.Fields[0];
    Password := FRequestBuffer.Fields[1];
    IPAddr := TClientWSocket(FORBDataPtr.Tag).PeerAddr;
    if UData.ShowServerMsg then
    begin
      TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr + ' [LOGIN] ' +
                                                                          User);
    end;
    {  check for additional client version information, to be sure that we
       don't allow connects from outdatet clients. Only clients as from 2.2.0
       provides such information, older clients will provide a blank string  }
    CliVersion := FRequestBuffer.Fields[2];
    if UData.ShowServerMsg then
    begin
      if CliVersion <> '' then
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                                       ' [LOGIN] Client version: ' + CliVersion)
      else
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
         ' [LOGIN] Client version: unknown or older than 2.2.0. Access denied');
    end; // if UData.ShowServerMsg then
    // check if the client's version number is ok
    if (CliVersion = '') or
       (StrToInt(CliVersion) < ExpectedCliVersion) then
    begin
      // no, client version is wrong, reject the client
      FResultStatus := 403;
      FResponseBuffer.Rewrite;
      FResponseBuffer.WriteFields(False,
            [Format(InvClient, [FFunctionCode, (ExpectedCliVersion div 100),
                                               (ExpectedCliVersion mod 100)])]);
      Finish;
      Exit;
    end; // if (CliVersion = '') or...
    // check if the client's timestamp is ok
    CliTimeStamp := _StrToFloat(FRequestBuffer.Fields[3]);
    if UData.CheckCliTimeStamp and (CliTimeStamp <> 0) then
    begin
      if UData.ShowServerMsg then
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
          ' [LOGIN] Client timestamp: ' + DateTimeToStr(CliTimeStamp) + ' GMT');
      {  max. 2 hours difference (GMT) between server & client allowed,
         otherwise the clients date is invalid  }
      if (CliTimeStamp < (LocalDT2GMTDT(Now) - 1/12)) or
         (CliTimeStamp > (LocalDT2GMTDT(Now) + 1/12)) then
      begin
        // no, client's timestamp is invalid, reject the client
        if UData.ShowServerMsg then
          TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                         ' [LOGIN] Client timestamp is invalid. Access denied');
        FResultStatus := 403;
        FResponseBuffer.Rewrite;
        FResponseBuffer.WriteFields(False,
                                      [Format(InvClientTime, [FFunctionCode])]);
        Finish;
        Exit;
      end;
    end; // if UData.CheckCliTimeStamp and (CliTimeStamp <> 0) then
    // user access management
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT userid, pw, rights, ipaddr FROM users');
        SQL.Add('WHERE login = :login');
        ParamByName('login').AsString := User;
        Open;
        if not Eof then
        begin
          UserID := FieldByName('userid').AsInteger;
          StoredPassword := FieldByName('pw').AsString;
          Rights := FieldByName('rights').AsInteger;
          StoredIPAddr := FieldByName('ipaddr').AsString;
        end else ErrorMsg := ' Unknown user.';
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // local login w/o password enabled?
    if UData.LocalLogin and
      ((IPAddr = '127.0.0.1') or (IPAddr = UData.ServerIPNo)) then // yes
      Accepted := (UserID <> 0)
    else
    begin
      // login restricted to stored IP address?
      if UData.CheckIPAddr then
      begin // yes
        Accepted := (UserID <> 0) and
                    (fvcsCryptCompare(Password, StoredPassword));
        if (not Accepted) and (ErrorMsg = '') then
          ErrorMsg := ' Invalid Password.';
        // check IP only if StoredIPAddr <> '0'
        if Accepted then
          Accepted := CheckIPMask(IPAddr, StoredIPAddr) or
                      (StoredIPAddr = '0');
      end // if UData.CheckIPAddr then begin
      else
      begin
        Accepted := (UserID <> 0) and
                       (fvcsCryptCompare(Password, StoredPassword));
        if (not Accepted) and (ErrorMsg = '') then
          ErrorMsg := ' Invalid Password.';
      end; // else if UData.CheckIPAddr then begin
    end; // else if UData.LocalLogin and (IPAddr = '127.0.0.0') then
    if Accepted then
    begin
      // create random transaction number
      Randomize;
      TANr := Round(Random * High(TANr));
      Session := SessionList.AddNewSession(UserID);
      Session.Rights := Rights;
      Session.TANr := TANr;
      // save statistic data
      Inc(UData.UserCount);
    end; // if Accepted then begin
    FResponseBuffer.Rewrite;
    // Clients Ini
    IniFile := TIniFile.Create(FIniFileName);
    try
      ForceGlobalIni := IniFile.ReadBool(GlobalUserData, ForceGlobalSettings,
                                                                         False);
    finally
      IniFile.Free;
    end;
    // User accepted ?
    FResponseBuffer.WriteFields(True, [Accepted]);
    if Accepted then
    begin
      LevelMsg := ' Access level: ' + IntToStr(Rights);
      case Rights of
        0: LevelMsg := LevelMsg + ' [Guest]';
        1: LevelMsg := LevelMsg + ' [read-only]';
        2: LevelMsg := LevelMsg + ' [read-write]';
        3: LevelMsg := LevelMsg + ' [Project Admin]';
        4: LevelMsg := LevelMsg + ' [Archive Admin]';
      end; // case Rights of
      FResponseBuffer.WriteFields(False, [UserID]);
      FResponseBuffer.WriteFields(False, [TANr]);
      FResponseBuffer.WriteFields(False, ['User accepted.' + LevelMsg]);
      FResponseBuffer.WriteFields(False, [GetSrvVersion(cServerId)]);
      FResponseBuffer.WriteFields(False, [GetSrvPortFromId(cServerId)]);
      FResponseBuffer.WriteFields(False, [ForceGlobalIni]);
      if UData.ShowServerMsg then
      begin
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                             ' [LOGIN] User accepted,' + LevelMsg + ' ' + User);
      end;
    end // if Accepted then begin
    else
    begin
      if ErrorMsg = '' then ErrorMsg := ' Invalid IPAddr.';
      FResponseBuffer.WriteFields(False, [-1]);
      FResponseBuffer.WriteFields(False, [-1]);
      FResponseBuffer.WriteFields(False, ['Access denied.' + ErrorMsg]);
      FResponseBuffer.WriteFields(False, [GetSrvVersion(cServerId)]);
      FResponseBuffer.WriteFields(False, [GetSrvPortFromId(cServerId)]);
      FResponseBuffer.WriteFields(False, ['0']);
      if UData.ShowServerMsg then
      begin
        TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                             ' [LOGIN] Access denied,' + ErrorMsg + ' ' + User);
      end; // if UData.ShowServerMsg then begin
    end; // else if Accepted then begin
    FResultStatus := 200;
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
  Server Logout
  Remove current transaction number assigned to this user
  (Administrators may use this object to log out any user)

        Record  Field
        ======  =====

  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]User ID                - Integer

  Response: 0: none

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectLOGOUT.Execute;
const
  RequestedRightsSelf = LevelNone;
  RequestedRightsAdmin = LevelAAdmin;
var
  UData: PUserDataRecord;
  //FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  RequestedRights,
  UserID: Integer;
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
    UserID := StrToInt(FRequestBuffer.Fields[0]);
    if UserID = AccessID then RequestedRights := RequestedRightsSelf
      else RequestedRights := RequestedRightsAdmin;
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

    (*
    // remove transaction number
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('DELETE FROM transact');
        SQL.Add('WHERE accessid = :accessid');
        ParamByName('accessid').AsInteger := UserID;
        ExecSQL;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    *)
    SessionList.DeleteSession(UserID, TANr);
    // server display
    if UData.ShowServerMsg then
    begin
      TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr + ' [LOGOUT]');
    end;
    FResultStatus := 200;
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
  Whoami (User information)

        Record  Field
        ======  =====

  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]User ID                - Integer
               [1]User name              - String(50)
               [2]Access level           - Integer
               [3]Access label           - String
               [4]Login expires?         - Boolean
               [5]Login time             - Double (TDateTime)
               [6]Login time out         - Double (TDateTime)
               [7]Stored User IPAddr     - String(50)
               [8]Current User IPAddr    - String
               [9]Server label           - String(255)
              [10]DB type                - String
              [11]Project access level   - Integer
              [12]Project access label   - String
              [13]Server's GMT difference- String

            1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
  String(xx) -> do not return longer string values!
 ==============================================================================}
procedure TServerObjectWHOAMI.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  ProjectRights,
  AccessID: Integer;
  DT: Double;
  ServerGMTStr,
  DBVersionInfo: string;
  Session: TJVCSSession;  
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    //GrantedRights := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // global or not restricted function -> get access rights from Users
        Close;
        SQL.Clear;
        {
        SQL.Add('SELECT users.rights, users.login,');
        SQL.Add('transact.tanr, transact.expires, users.ipaddr');
        SQL.Add('FROM transact, users');
        SQL.Add('WHERE transact.accessid = :accessid');
        SQL.Add('AND users.userid = transact.accessid');
        SQL.Add('AND users.deleted <> ''1''');
        }
        SQL.Add('SELECT users.rights, users.login, users.ipaddr');
        SQL.Add('FROM users');
        SQL.Add('WHERE users.userid = :accessid');
        SQL.Add('AND users.deleted <> ''1''');
        ParamByName('accessid').AsInteger := AccessID;
        Open;
        Session := SessionList.FindSession(AccessID, TANr);
        if Assigned(Session) and (not Eof) then
        begin
          GrantedRights := FieldByName('rights').AsInteger;
          FResponseBuffer.Rewrite;
          FResponseBuffer.WriteFields(True, [AccessID]);
          FResponseBuffer.WriteFields(False, [FieldByName('login').AsString]);
          FResponseBuffer.WriteFields(False, [GrantedRights]);
          case GrantedRights of
            0: FResponseBuffer.WriteFields(False, ['Guest']);
            1: FResponseBuffer.WriteFields(False, ['read-only']);
            2: FResponseBuffer.WriteFields(False, ['read-write']);
            3: FResponseBuffer.WriteFields(False, ['Project administrator']);
            4: FResponseBuffer.WriteFields(False, ['Archive administrator']);
          end; // case GrantedRights of
          FResponseBuffer.WriteFields(False, [UData.LoginExpires]);
          DT := LocalDT2GMTDT(Session.Expires);
          FResponseBuffer.WriteFields(False, [DT]);
          FResponseBuffer.WriteFields(False, [UData.LoginTimeOut]);
          FResponseBuffer.WriteFields(False, [FieldByName('ipaddr').AsString]);
          FResponseBuffer.WriteFields(False,
                                    [TClientWSocket(FORBDataPtr.Tag).PeerAddr]);
          FResponseBuffer.WriteFields(False, [UData.Serverlabel]);
        end // if not EoF then begin
        else
        begin
          FResultStatus := 403; // forbidden
          FResponseBuffer.Rewrite;
          FResponseBuffer.WriteFields(False, [Unknown]);
          Finish;
          Exit;
        end; // else if not EoF then begin
        Close;
        {$IFDEF UIBSRV} //-- PrY
        DBVersionInfo := 'Firebird (UIB) 1.2';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF UIBSRV}
        {$IFDEF ORACLESRV}
        //-- hdo
        DBVersionInfo := 'ORACLE Ver 1.1';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF ORACLESRV}
        {$IFDEF MSSQLSRV} //-- MG
        DBVersionInfo := 'MSSQL Ver 1.1';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF MSSQLSRV}
        {$IFDEF MYSQLSRV} //-- LB
        DBVersionInfo := 'MySQL Ver 1.1';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF MYSQLSRV}
        {$IFDEF ADOSRV} //-- PL
        DBVersionInfo := 'ADO Ver 1.0';
        FResponseBuffer.WriteFields(False, [DBVersionInfo]);
        {$ENDIF ADOSRV}
        if Assigned(Session) then
          ProjectRights := Session.Rights
        else
          ProjectRights := -1;
        FResponseBuffer.WriteFields(False, [ProjectRights]);
        case ProjectRights of
         -1: FResponseBuffer.WriteFields(False, ['Default rights']);
          0: FResponseBuffer.WriteFields(False, ['Guest']);
          1: FResponseBuffer.WriteFields(False, ['read-only']);
          2: FResponseBuffer.WriteFields(False, ['read-write']);
          3: FResponseBuffer.WriteFields(False, ['Project administrator']);
          4: FResponseBuffer.WriteFields(False, ['Archive administrator']);
        end; // case ProjectRights of
      end; // with FQuery do begin
      ServerGMTStr := FloatToStr(UData.GMTDifference);
      if UData.GMTDifference >= 0 then
        ServerGMTStr := '(GMT+' + ServerGMTStr + ')'
      else
        ServerGMTStr := '(GMT' + ServerGMTStr + ')';
      FResponseBuffer.WriteFields(False, [ServerGMTStr]);
    finally
      FQuery.Free;
    end;
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
    FResultStatus := 200;
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
  Get a list from all users / all logged in users / Admin use

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

  Response: 0: [0]User ID                - Integer
               [1]User name              - String(50)
               [2]User access level      - Integer
               [3]User access ID         - Integer
                  (logged in -> User access ID = User ID,
                   not logged in -> User access ID = '')
               [4]User deleted?          - String(1)
                  (0 = User not deleted, 1 = User deleted)
               [5]User description       - String
               [6]User IPAddr            - String(50)

            1: [next user...]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_USERLIST.Execute;
const
  RequestedRights = LevelAAdmin;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  GrantedRights,
  Fld,
  AccessID: Integer;
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
        SQL.Add('SELECT users.userid, users.login,');
        SQL.Add('users.rights, 0 as accessid, users.deleted,');
        SQL.Add('users.description, users.ipaddr');
        SQL.Add('FROM users');
        SQL.Add('ORDER BY users.login');
        Open;
        while not Eof do
        begin
          // Copy all fields from the record to the response
          for Fld := 0 to FQuery.FieldCount - 1 do
          begin
            if Fld <> 3 then
              FResponseBuffer.WriteFields(Fld = 0, [FQuery.Fields[Fld].AsString])
            else
            if SessionList.GetUserSessionCount(Fields[0].AsInteger) > 0 then
              FResponseBuffer.WriteFields(Fld = 0, [Fields[0].AsString])
            else
              FResponseBuffer.WriteFields(Fld = 0, ['']);
          end;
          Next;
        end;
        Close;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    FResultStatus := 200;
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
  Get project ID: user enters a new project.

  Server object has two functions:
    - Get project's ID by name
    - Check if there is any project related access level defined for this user.
      In this case:
      - The default access level (given by login) will be overwritten by the
        project related access level and the user will get a new transaction
        number.
      - Old transaction number will be cleared.
      - Login time out will be resetted.

        Record  Field
        ======  =====
  Request:  0: [0]Transaction number     - Integer
               [1]Access ID              - Integer

            1: [0]Project name           - String

  Response: 0: [0]Project ID             - Integer
                  (return 0 if the project was not found)
               [1]Project deleted?        - String(1)
                  ('1' = deleted, '0' = not deleted)
               [2]Transaction number      - Integer
               [3]Access level            - Integer

              1: [Object should return only one record,
                Client will ignore further records]

  Expected Result Status: 200 = OK/ 400 = bad request/ 403 = forbidden
 ==============================================================================}
procedure TServerObjectGET_PROJECT_ID.Execute;
const
  RequestedRights = LevelNone;
var
  UData: PUserDataRecord;
  FQuery: TSrvQuery;
  TANr,
  AccessID,
  GrantedRights,
  ProjectID,
  ProjectAccessLevel: Integer;
  LastBranchID: Integer;
  Project: string;
  Session: TJVCSSession;  
begin
  {$IFDEF DEBUG}
  InitDebugMessage;
  {$ENDIF DEBUG}
  try
    FRequestBuffer.First;
    TANr := StrToInt(FRequestBuffer.Fields[0]);
    AccessID := StrToInt(FRequestBuffer.Fields[1]);
    UData := PUserDataRecord(FUserData);
    GrantedRights := 0;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        // get default rights from Users
        SQL.Clear;
        {
        SQL.Add('SELECT users.rights, transact.tanr');
        SQL.Add('FROM transact, users');
        SQL.Add('WHERE transact.accessid = :accessid');
        SQL.Add('AND users.userid = transact.accessid');
        SQL.Add('AND users.deleted <> ''1''');
        ParamByName('accessid').AsInteger := AccessID;
        Open;
        if not Eof then
          if FieldByName('tanr').AsInteger = TANr
            then GrantedRights := FieldByName('rights').AsInteger;
        }
        Session := SessionList.FindSession(AccessID, TANr);
        if Assigned(Session) then
        begin
          SQL.Add('SELECT users.rights');
          SQL.Add('FROM users');
          SQL.Add('WHERE users.userid = :accessid');
          SQL.Add('AND users.deleted <> ''1''');
          ParamByName('accessid').AsInteger := AccessID;
          Open;
          if not Eof then
            GrantedRights := FieldByName('rights').AsInteger;
          Close;
        end;
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
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

    Project := AnsiLowerCase(FRequestBuffer.Fields[0]);
    ProjectID := 0;
    ProjectAccessLevel := 0;

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        Close;
        SQL.Clear;
        SQL.Add('SELECT projects.projectid, projects.deleted');
        SQL.Add('FROM projects');
        SQL.Add('WHERE projects.name = :project');
        ParamByName('project').AsString := Project;
        Open;
        if not Eof then
        begin
          // return project ID & deleted flag
          ProjectID := FieldByName('projectid').AsInteger;
          FResponseBuffer.WriteFields(True, [ProjectID]);
          FResponseBuffer.WriteFields(False, [FieldByName('deleted').AsString]);
        end else
        begin
          // unknown project -> return zero
          FResponseBuffer.WriteFields(True, [0]);
          FResponseBuffer.WriteFields(False, ['0']);
        end;
        Close;
        if ProjectID <> 0 then
        begin
          // known project
          // check for project related access level
          Close;
          SQL.Clear;
          SQL.Add('SELECT rights FROM pjusers');
          SQL.Add('WHERE projectid = :projectid');
          SQL.Add('AND userid = :userid');
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('userid').AsInteger := AccessID;
          Open;
          if not Eof then // project related access level is defined
            ProjectAccessLevel := FieldByName('rights').AsInteger;
          Close;
          // new transact record
          if ProjectAccessLevel = 0 then // assign default rights
            ProjectAccessLevel := GrantedRights;
          // create random transaction number
          Randomize;
          LastBranchID := SessionList.GetSessionBranchID(AccessID, TANr);
          SessionList.DeleteSession(AccessID, TANr);
          TANr := Round(Random * High(TANr));
          Session := SessionList.AddNewSession(AccessID);
          Session.ProjectID := ProjectID;
          Session.Rights := ProjectAccessLevel;
          Session.TANr := TANr;
          Session.BranchID := LastBranchID;
          (*
          // remove all old entries
          SQL.Clear;
          SQL.Add('DELETE FROM transact');
          SQL.Add('WHERE accessid = :accessid');
          ParamByName('accessid').AsInteger := AccessID;
          ExecSQL;
          // store user ID, transaction number & new rights
          SQL.Clear;
          SQL.Add('INSERT INTO transact');
          SQL.Add(Format('VALUES (%s', [GetInsertKeyFieldValueStr('transact')]));
          SQL.Add(':accessid, :projectid, :rights, :tanr,');
          SQL.Add(':faults, :expires)');
          ParamByName('accessid').AsInteger := AccessID;
          ParamByName('projectid').AsInteger := ProjectID;
          ParamByName('rights').AsInteger := ProjectAccessLevel;
          ParamByName('tanr').AsInteger := TANr;
          ParamByName('faults').AsInteger := 0;
          ParamByName('expires').AsDateTime := Now;
          ExecSQL;
          *)
          if UData.ShowServerMsg then
            TriggerDisplay(TClientWSocket(FORBDataPtr.Tag).PeerAddr +
                                             ' Connect to project: ' + Project);
        end; // if ProjectID <> 0 then begin
      end; // with FQuery do begin
    finally
      FQuery.Free;
    end;
    // return (new) transaction number & rights
    FResponseBuffer.WriteFields(False, [TANr]);
    FResponseBuffer.WriteFields(False, [ProjectAccessLevel]);
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
  Get list of the branchpoint revisions from one project
  (by version/revision)
  - include ALL blob information - also file family members

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
procedure TServerObjectGET_BRANCHPOINT_REVISIONS.Execute;
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

    UpdateLatestRevisions(UData^, SessionList.GetSessionBranchID(AccessID, TANr));

    FResponseBuffer.Rewrite;
    FQuery := TSrvQuery.Create(Self);
    try
      FQuery.DatabaseName := UData.DBPath;
      with FQuery do
      begin
        // First step: get ALL revisions & revision members
        Close;
        SQL.Clear;
        SQL.Add('SELECT modules.moduleid, brmodule.name, brmodule.path,');
        SQL.Add('revision.revisionid, revision.version,');
        SQL.Add('revision.revision, blobs.origtime,');
        SQL.Add('blobs.extension, blobs.origcrc');
        SQL.Add('FROM pjmodule, modules, brmodule, revision, rvbranch, blobs');
        SQL.Add('WHERE pjmodule.projectid = :projectid');
        SQL.Add('AND pjmodule.branchid = :branchid');
        SQL.Add('AND modules.moduleid = pjmodule.moduleid');
        SQL.Add('AND brmodule.branchid = :branchid');
        SQL.Add('AND brmodule.moduleid = modules.moduleid');
        SQL.Add('AND revision.moduleid = pjmodule.moduleid');
        SQL.Add('AND rvbranch.branchid = :branchid');
        SQL.Add('AND rvbranch.origin = ''p''');
        SQL.Add('AND rvbranch.revisionid = revision.revisionid');
        SQL.Add('AND blobs.revisionid = revision.revisionid');
        if ExclHidden then // skip hiddden modules?
          SQL.Add('AND NOT (pjmodule.hidden = ''1'')');
        SQL.Add('ORDER BY modules.name DESC, modules.moduleid DESC,');
        SQL.Add('revision.version DESC, revision.revision DESC');
        ParamByName('projectid').AsInteger := ProjectID;
        ParamByName('branchid').AsInteger := SessionList.GetSessionBranchID(AccessID, TANr);
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

{$IFDEF USELATESTREVISION}
initialization

finalization
  GBranchRevisionStateList.Free;
{$ENDIF USELATESTREVISION}

end.

