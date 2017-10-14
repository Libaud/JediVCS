{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SvrUDRec.pas

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
2003/02/22  THuber   - Add  F I B P L U S S R V   port which uses  F I B p l u s  components
                       therefor dangerous casts changed:
                        * TDateTime(...) => .AsDateTime
                        * Integer(...) => .AsInteger
                        * FQuery.Fields[Fld] => FQuery.Fields[Fld].AsString
                     - removed Interbase5 support
                     - D4_UP directive removed

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster- style cleaning
                     - changes for Firebird port over JVCL UIB components 
                       by Pierre Y. (partly adapted)
2005/05/08  USchuster- removed unused units
2009/12/13  USchuster- changes for opt-out of using table latestrevision for performance increase

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I n f o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     #5066 support for  F l a s h F i l e r  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  SvrUDRec - UserData record definition for FreeVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit SrvUDRec;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SrvDBDef;

type
  // Replacement for some global variables
  PUserDataRecord = ^TUserDataRecord;
  TUserDataRecord = record
    ServerLabel,                // server version/type information
    ServerIPNo,                 // server machines real IP number
    UpTimeStr: ShortString;     // server uptime
    DBPath: TJVCSArchive;       // current connection
    {$IFDEF ADOSRV} // PL
    DBDatabase,                 // current database name
    ServerName,                 // current server name
    {$ELSE}
      {$IFDEF MSSQLSRV}
      DBDatabase,               // current database name
      ServerName,               // current server name
      {$ELSE}
        {$IFDEF UIBSRV} //-- PrY
        ServerName,              // the Firebird server name if not local
                                   // needed for TServerObjectLIVE_BACKUP
        {$ELSE}
                  {$IFDEF MYSQLSRV}
                  DBDatabase,               // current database name
                  ServerName,               // current server name
                  {$ENDIF MYSQLSRV}
        {$ENDIF UIBSRV}          
      {$ENDIF MSSQLSRV}
    {$ENDIF ADOSRV}
    ServerPath,                 // server program path
    BackupPath: string;         // version archive backup path
    DisableSQLDirekt,           // enable the "EXEC_SQL" server object
    ShowServerMsg,              // show messages on the servers display
    WriteServerLog,             // write server log file (server.log)
    LogAccessFault,             // log client access faults (Access denied)
    WriteVCSLog,                // write version archive log (vcslog table)
    CheckIPAddr,                // login restricted to stored IP address
    CheckCliTimeStamp,          // login restricted to valid client dates
    LoginExpires,               // max. login time is restricted
    LocalLogin,                 // enable local login w/o password
    CasuallyCheckin,            // casually/restrictive check in
    SettingsChanged: Boolean;   // 'settings need to be saved' flag
    ArchiveTimeStamp,           // last version archive write access
    LoginTimeOut,               // max. login time
    LastLiveBackup,             // last version archive backup
    GMTDifference: Double;      // Difference between local time and GMT
    ConnectTimeOut,             // client connection idle time out
    // statistic information
    RequestCount,               // number of client requests
    UserCount,                  // number of users (sum)
    CheckinCount,               // number of checked in modules
    CheckoutCount,              // number of checked out modules
    GetCount,                   // number of modules got by a user
    NewFilesCount: Integer;     // number of new modules
    TransmittedBytes,           // number of transmitted bytes
    ReceivedBytes: Int64;       // number of received bytes
    {$IFNDEF NTSERVICESRV}
    //-- hdo
    DBAutoLogon: Boolean;     // if true, Server will logon automatically
                              // on start
    {$ENDIF ~NTSERVICESRV}
    {$IFDEF USELATESTREVISION}
    UseLatestRevisions: Boolean;
    {$ENDIF USELATESTREVISION}
  end;

implementation
end.

