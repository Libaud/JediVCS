{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FVCSAppSrv.dpr

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

2003/02/04  HDors     - 1st Migrationstep from FreeVCS code to JEDI VCS
2003/12/23  CSchuette - modified uses clauses to make project manager in
                        Delphi 7 show correct information
2003/12/27  THuber    - added missing SrvAbout to uses
2004/01/17  USchuster - added new units
                      - use COMPOPT.INC instead of COND_DEF.INC
2005/04/18  CSchuette - added new memory manager to increase performance
                      - added some missing units to service
2005/06/12  USchuster - minor style cleaning
2005/07/10  THuber    - now use FastMM as memorymanager
                        (add v:\projects\jedivcs\src\comp\fastmm to your
                         search path)
2005/07/10  USchuster - D5 fix (excluded FastMove and FastcodeCompareText because
                        they contain asm instructions which doesn't compile with D5)
2006/06/03  USchuster - JVCSSrv25 -> JVCSSrv
2006/07/05  USchuster - changes for eventlog Event ID description in service version by AK(mantis #3789)
2009/11/30  THuber    - #5032 change server.log location

--- 2.50 Beta 2 was released...
2009/12/27  THuber  #5067 support for  D B I S A M removed
2010/02/07  THuber  - added missing unit for standalone server ports

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  FVCSAppServer - project file for FreeVCS application server /
                  FreeVCS NT service application server.

  This is an altered source version of François PIETTE's SvrTest application.
  For original version & complete MidWare source/documentation see the
  adresses in SrvMain.pas.
  NT service application code by Marco Gosselink (--mg) - mgosselink@yahoo.com
  Service enum code by Holger Dors (--hd) - dors@kittelberger.de
}

{
  2.5 replaces Borland default memory manager with FastMM 4.x.
  FastMM does not fragment memory and suites better for 24/7 use.
  If there are any problems with FastMM the default (borland) mm can be
  just by defining DEFAULTMM for make.  
}

program JVCSSrv;

{$I jedi.inc}
{$I compopt.inc}

{$IFNDEF NTSERVICESRV}

uses
  FastMM4,
  FastMove,
  FastcodeCompareText,
  Windows,
  Forms,
  SysUtils,
  VCScommon in '..\common\vcscommon.pas',
  SrvMain in 'SrvMain.pas' {ServerForm},
  SrvAbout in 'SrvAbout.pas' {VCSAbout},
  SrvAutoBkp in 'SrvAutoBkp.pas' {SrvAutoBackup},
  SrvLogon in 'SrvLogon.pas' {FormLogon},
  SrvConst in 'SrvConst.pas',
  SrvDBDef in 'SrvDBDef.pas',
  SrvAccessDef in 'SrvAccessDef.pas',
  SrvCustomSrvOBJ in 'SrvCustomSrvOBJ.pas',
  SrvUDRec in 'SrvUDRec.pas',
  ServerCryptInt in 'ServerCryptInt.pas',
  SrvOBJMain in 'SrvOBJMain.pas',
  SrvOBJReport in 'SrvOBJReport.pas',
  SrvOBJProjects in 'SrvOBJProjects.pas',
  SrvOBJModules in 'SrvOBJModules.pas',
  SrvOBJFeatures in 'SrvOBJFeatures.pas',
  SrvOBJLog_ToDo in 'SrvOBJLog_ToDo.pas',
  SrvOBJBugTrack in 'SrvOBJBugTrack.pas',
  EnumServices in 'EnumServices.pas',
  JVCSForms in '..\common\JVCSForms.pas',
  JVCSBaseAboutDialog in '..\common\JVCSBaseAboutDialog.pas' {JVCSBaseAboutDlg},
  JVCSServerFunctions in '..\common\server\JVCSServerFunctions.pas',
  JVCSFunctions in '..\common\JVCSFunctions.pas';

{$ELSE}

uses
  // --MG
  {$IFNDEF DEFAULTMM}
  FastMM4,
  {$IFDEF DELPHI6_UP}
  FastMove,
  FastcodeCompareText,
  {$ENDIF DELPHI6_UP}
  {$ENDIF ~DEFAULTMM}
  SvcMgr,
  VCScommon in '..\common\vcscommon.pas',
  SrvServiceUnit in 'SrvServiceUnit.pas' {FreeVCSService: TService},
  JVCSServerFunctions in '..\common\server\JVCSServerFunctions.pas',
  JVCSFunctions in '..\common\JVCSFunctions.pas';

{$R JVCSEventLogMessagetext.res}

{$ENDIF ~NTSERVICESRV}

{$R *.res}
{$R jvcsver.res}

{$IFNDEF NTSERVICESRV}
var
  I: Integer;
  SvcStatusArray: TServiceStatusArray = nil;
  SrvMutex: THandle;
{$ENDIF ~NTSERVICESRV}

begin
  {$IFNDEF NTSERVICESRV}
  // Check for an already running NT service
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    SvcStatusArray := GetServices(TServiceState(ssActive));
    if Assigned(SvcStatusArray) then
    begin
      for I := 0 to Length(SvcStatusArray) - 1 do
      begin
        if SvcStatusArray[I].ServiceName = 'FreeVCSService' then
        begin
          Windows.Beep(500, 100);
          MessageBox(Application.Handle,
            PChar('There is already a JVCS NT service running on this machine.' +
              #10#13 + 'You cannot use both server types together at one time.'),
             'JVCS server', MB_OK or MB_ICONWARNING);
          Halt(1);
        end; // if SvcStatusArray[I].ServiceName = 'FreeVCSService' then begin
      end; // for I := 0 to Length(SvcStatusArray) - 1 do begin
    end; // if Assigned(SvcStatusArray) then begin
  end; // if Win32Platform = VER_PLATFORM_WIN32_NT then...
  // Prevent the server from running twice
  SrvMutex := CreateMutex(nil, False, PChar(GetSrvPortFromId(cServerId) + 'SrvMutex'));
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    Windows.Beep(500, 100);
    MessageBox(Application.Handle,
                 PChar('Server is already started! Only one instance allowed.'),
                                     'JVCS server', MB_OK or MB_ICONWARNING);
    Halt(1);
  end;
  Application.Initialize;
  Application.Title := 'JVCS Appserver';
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
  ReleaseMutex(SrvMutex);
  {$ELSE}
  // service applications cannot be started more than one time
  // MG
  Application.Initialize;
  Application.CreateForm(TFreeVCSService, FreeVCSService);
  Application.Run;
  {$ENDIF ~NTSERVICESRV}
end.
