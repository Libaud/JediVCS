{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvConst.pas

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

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI VCS
2003/02/09  HDors    - Add MySQL Server Port by Ludo Brands 
2003/02/22  THuber   - Add  F I B P L U S S R V  port which uses  F I B p l u s  components
                       therefor dangerous casts changed:
                        * TDateTime(...) => .AsDateTime
                        * Integer(...) => .AsInteger
                        * FQuery.Fields[Fld] => FQuery.Fields[Fld].AsString
                     - removed Interbase5 support
                     - D4_UP directive removed
2003/03/03  THuber   - changed handling for server port version detecting
2003/11/16  THuber   - added firebird related const for server configuration file 
2003/12/26  THuber   - Version consts moved to VCSCommon
2003/12/26  THuber   - DlgCaption => c_DlgCaption and added a space
2004/10/31  USchuster- removed space from c_DlgCaption
2004/11/03  CSchuette- added constant for "Hide Window on Close" setting

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster- style cleaning
                     - changes for Firebird port over JVCL UIB components 
                       by Pierre Y. (partly adapted)
2005/01/14  THuber   #2502 added mssql option for trusted nt connections
2005/02/13  THuber   #2570 added  F i b CharSet const
2006/10/11  USchuster-added constant for Oracle performance tweak "UseRuleOptimizerMode" (mantis #3950)

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     #5066 support for  F l a s h F i l e r  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SrvConst.pas - Constant definition for JVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 
unit SrvConst;

{$I compopt.inc}

interface

uses
  VCSCommon;

const
  {  Application/Messagebox caption, program version (100 = 1.00)
     & highest archive version supported by this server (20 = 2.0)  }
  {  Don't change the constant 'Srv'. The FreeVCS clients will check this value
     to determine the required server version!  }

  {$IFDEF UIBSRV} //-- PrY
  cServerId = svIdUIB;
  ArchMaxVersion = 20;
  {$ENDIF UIBSRV}
  {$IFDEF ORACLESRV}
  cServerId = svIdOraOci;
  ArchMaxVersion = 20;
  {$ENDIF ORACLESRV}
  {$IFDEF MSSQLSRV}
  cServerId = svIdMSSQL;
  ArchMaxVersion = 20;
  SupportedDBServerVersion = 7;
  {$ENDIF MSSQLSRV}
  {$IFDEF MYSQLSRV}
  cServerId = svIdMySQL;
  ArchMaxVersion = 20;
  {$ENDIF MYSQLSRV}
  {$IFDEF ADOSRV} // -- PL
  cServerId = svIdADO;
  ArchMaxVersion = 20;
  {$ENDIF ADOSRV}

  // Server object exception message
  SrvException          = '[%s]:' + #10#13 + '%s';

  // Server Inifile sections/ value names
  // Settings
  SectionData                   = 'Data';
  ShowMsg                       = 'ShowMsg';
  WriteSrvLog                   = 'WriteSrvLog';
  MaxSrvLog                     = 'MaxSrvLog';
  WriteVCSLog                   = 'WriteVCSLog';
  LoginTimeOut                  = 'LoginTimeOut';
  LoginExpires                  = 'LoginExpires';
  CasuallyCheckin               = 'CasuallyCheckin';
  CheckIPAddr                   = 'CheckIPAddr';
  LocalLogin                    = 'LocalLogin';
  LogAccessFault                = 'LogAccessFault';
  DisableSQLDirekt              = 'DisableSQLDirekt';
  GlobalUserData                = 'GlobalUserData';
  ForceGlobalSettings           = 'ForceGlobalSettings';
  Dependency                    = 'Dependency';
  ServiceName                   = 'ServiceName';
  ServiceDisplayName            = 'ServiceDisplayName';
  DisableServerBanner           = 'DisableServerBanner';
  StartHidden                   = 'StartHidden';
  HideOnClose                   = 'HideOnClose';
  VerifyClientTimestamp         = 'VerifyClientTimestamp';
  FibUseEmbeded                 = 'FirebirdUseEmbeded';
  FibCharSet                    = 'FirebirdCharacterSet';
  FmssqlUseNTTrustedConnection  = 'UseTrustedNTConnection';
  ServerBanner                  = 'ServerBanner';
  UseRuleOptimizerMode          = 'UseRuleOptimizerMode';
  {$IFDEF DEBUG}
  ShowDebugMsg = 'ShowDebugMsg';
  {$ENDIF DEBUG} // {$IFDEF DEBUG}

var
// ----------------------------------------------------
// Common vars for Ntservice AND Standalone Application
// ----------------------------------------------------
  c_DlgCaption    : string;
  MainTitle       : string;
  KeyPort         : string;
  ClientTO        : string;
  ConnectTimeOut  : string;
  DBPath          : string;
  BackupPath      : string;
  LastLiveBackup  : string;
  ArchiveTStamp   : string;
  AutoBackupActive: string;
  AutoBackupTime  : string;
  LastBackup      : string;
// ----------------------------------------------
// Connection variables
// ----------------------------------------------
  DBUser        : string;
  DBPassword    : string;
  DBServer      : string;
  DBAutoLogon   : string;
  DBDatabase    : string;
// ----------------------------------------------------------
// Separation for vars only specific for one of both apptypes
// ----------------------------------------------------------
  {$IFDEF NTSERVICESRV}
  // here only vars for Ntservice
  Priority        : string;
  MaximumRetries  : string;
  {$ELSE}
  // here only vars for StandaloneApp
  SectionWindow   : string;
  KeyTop          : string;
  KeyLeft         : string;
  KeyWidth        : string;
  KeyHeight       : string;
  HideWindow      : string;
  {$ENDIF NTSERVICESRV}

implementation

var
  Srv: string;
  BaseStr: string;

initialization

  Srv := GetSrvPortFromId(cServerId);

  // first of all BaseStr needs to be defined...

  {$IFDEF NTSERVICESRV}
  //---------------------------
  BaseStr         := 'NTService';
  //---------------------------
  c_DlgCaption    := Srv + ' NT Service';
  // only related to Serviceapplication
  Priority        := BaseStr + 'ThreadPriority';
  MaximumRetries  := BaseStr + 'DBMaxRetries';
  {$ELSE}
  //---------------------------
  BaseStr         := Srv;
  //---------------------------
  c_DlgCaption    := Srv + ' Server';
  // -- Window position & size
  SectionWindow   := 'Window';
  KeyTop          := 'Top';
  KeyLeft         := 'Left';
  KeyWidth        := 'Width';
  KeyHeight       := 'Height';
  HideWindow      := 'HideWindow';
  {$ENDIF NTSERVICESRV}

  // ...now common vars can be set...
  MainTitle       := 'JEDI VCS ' + c_DlgCaption;
  KeyPort         := BaseStr + 'Port';
  ClientTO        := BaseStr + 'ClientTO';
  ConnectTimeOut  := BaseStr + 'ConnectTimeOut';
  DBPath          := BaseStr + 'DBPath';
  BackupPath      := BaseStr + 'BackupPath';
  LastLiveBackup  := BaseStr + 'LastLiveBackup';
  ArchiveTStamp   := BaseStr + 'ArchiveTStamp';
  AutoBackupActive:= BaseStr + 'AutoBackupActive';
  AutoBackupTime  := BaseStr + 'AutoBackupTime';
  LastBackup      := BaseStr + 'LastBackup';


  // connection parameters
  DBUser      := BaseStr + 'DBUser';
  DBPassword  := BaseStr + 'DBPassword';
  DBServer    := BaseStr + 'DBServer';
  DBAutoLogon := BaseStr + 'DBAutoLogin';
  case cServerId of
    svIdUIB:
      begin
        DBDatabase := BaseStr + 'DBDatabase';
      end;
    svIdOraOci:
      begin
      end;
    svIdMSSQL:
      begin
        DBDatabase := BaseStr + 'DBDatabase';
      end;
    svIdMySQL:
      begin
        DBDatabase := BaseStr + 'DBDatabase';
      end;
    svIdADO:
      begin
        DBDatabase := BaseStr + 'DBDatabase';
      end;
  end;

end.

