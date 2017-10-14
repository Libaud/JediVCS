{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvAutoBkp.pas

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
 
  4-Mrz-2000 05:51:56 (GMT+1) > [sysdba on K6] checked in Beta v. 04.Mar.00
   - Version Dors/Gossselink  

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI-VCS
2003/02/09  HDors    - Add MySQL Server Port by Ludo Brands 
2003/12/26  THuber   - DlgCaption => c_DlgCaption
2004/11/16  USchuster- style cleaning
                     - added dxGetText support for localization with over IFDEF LANGUAGE
                     - res strings and displayed strings changed to resourcestrings
2011/01/15  USchuster- changed font to Tahoma                     

-----------------------------------------------------------------------------*)

unit SrvAutoBkp;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TSrvAutoBackup = class(TForm)
    dtpBkpTime: TDateTimePicker;
    cbAutoBkpActive: TCheckBox;
    Label1: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    BackupActive: Boolean;
    BackupTime: TDateTime;
    {$IFDEF MYSQLSRV}
    BackupPath: string;
    {$ENDIF MYSQLSRV}
  end;

var
  SrvAutoBackup: TSrvAutoBackup;

implementation

uses
  {$IFDEF LANGUAGE}
  JvGnugettext,
  {$ENDIF LANGUAGE}
  SrvConst, JVCSSrvResources;

{$R *.dfm}

{$IFDEF MYSQLSRV}
const
  cAddHeight = 60;
var
  LblArchPath: TLabel;
  EdtArchPath: TEdit;
{$ENDIF MYSQLSRV}

//------------------------------------------------------------------------------

procedure TSrvAutoBackup.FormShow(Sender: TObject);
begin
  cbAutoBkpActive.Checked := BackupActive;
  dtpBkpTime.Time := BackupTime;
  {$IFDEF MYSQLSRV}
// as we have the possibility to run the appserver on a different machine from
// the one running MySQL we can't assume the archive directory is on the local
// machine. Selectdirectory won't work, prompt for path here.
  if not Assigned(LblArchPath) then
  begin
    Height := Height + cAddHeight;
    LblArchPath := TLabel.Create(Self);
    LblArchPath.Parent := SrvAutoBackup;
    LblArchPath.Top := btnOK.Top;
    LblArchPath.Left := 8;
    LblArchPath.WordWrap := True;
    LblArchPath.Width := Width - 16;
    LblArchPath.Caption := JVCSRES_Archive_path_40path_needs_to_exist_on_the_Database_Server334158;
    btnOK.Top := btnOK.Top + cAddHeight;
    btnCancel.Top := btnCancel.Top + cAddHeight;
    EdtArchPath := TEdit.Create(Self);
    EdtArchPath.Parent := SrvAutoBackup;
    EdtArchPath.Top := btnOK.Top - 30;
    EdtArchPath.Left := 8;
    EdtArchPath.Width := Width - 16;
    EdtArchPath.Text := BackupPath;
  end;
  {$ENDIF MYSQLSRV}
end;

//------------------------------------------------------------------------------

procedure TSrvAutoBackup.btnOKClick(Sender: TObject);
begin
  BackupActive := cbAutoBkpActive.Checked;
  BackupTime := dtpBkpTime.Time - Int(dtpBkpTime.Time);
  {$IFDEF MYSQLSRV}
  BackupPath := EdtArchPath.Text;
  {$ENDIF MYSQLSRV}
  if BackupActive and (BackupTime = 0) then
  begin
    MessageBox(WindowHandle, PChar(JVCSRES_You_cannot_set_the_backup_time_to_00580046 +
      #10#13 + JVCSRES_Select_any_time_different_from_005800_40005801_to_2358594146),
      PChar(c_DlgCaption), MB_OK or MB_ICONWARNING);
    Exit;
  end;
  ModalResult := mrOK;
end;

procedure TSrvAutoBackup.FormCreate(Sender: TObject);
begin
  try
  finally
    {$IFDEF LANGUAGE}
    //Translate form
    TranslateComponent(Self);
    {$ENDIF LANGUAGE}
  end;
end;

end.
