{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvLogon.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

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
2003/11/16  THuber   - added firebird embeded option (needs firebird embeded
                       files installed in application folder)
2004/02/23  USchuster- Update to JVCL 3.0 / JCL 1.90 (mantis #1353)
                       -> this update solved the problem that quotes where added to
                          the filename if it contained one ore more spaces and that
                          caused the error "unknown database"
                     - Typo embeded => embedded fixed (only if text is visible)
                     - minor style cleaning
2004/11/16  USchuster- added dxGetText support for localization with over IFDEF LANGUAGE
                     - res strings and displayed strings changed to resourcestrings

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster- changes for Firebird port over JVCL UIB components 
                       by Pierre Y. (partly adapted)
2005/01/14  THuber   #2502 added mssql option for trusted nt connections
2005/02/13  THuber   #2570 added option characterset (lc_ctype) for firebird

                     ! use same characterset as used while you created database
                     ! ---
                     ! if not you'll notice this error on the server side:
                     !
                     ! Arithmetic overflow or division by zero has occurred.
                     ! arithmetic exception, numeric overflow, or string
                     ! truncation.
                     ! Cannot transliterate character between character sets.
                     ! ---
                     ! and this error on the client side:
                     !
                     ! error 400 (bad request)

--- 2.50 Beta 2 was released...
2009/12/22  THuber   #5062 add support for firebird 2.x databases, fixed broken embedded support
                     #4085 add support for connection of unicode databases over UIB
2009/12/23  THuber    #5063 support for  I n f o r m i x  port removed
                      #5064 support for  I n t e r b a s e 6  port removed
                      #5066 support for  F l a s h F i l e r  port removed
                      #5065 support for  F i b p l u s  port removed
2011/01/15  USchuster- changed font to Tahoma
                      
-----------------------------------------------------------------------------*)

unit SrvLogon;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, JvToolEdit, JvExMask;

type
  TFormLogon = class(TForm)
    LabelUser: TLabel;
    LabelPassword: TLabel;
    LabelServer: TLabel;
    EditUser: TEdit;
    EditPassword: TEdit;
    EditServer: TEdit;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    CheckBoxAutoLogon: TCheckBox;
    DatabaseLabel: TLabel;
    EditDatabase: TJvFilenameEdit;
    FFServerLabel: TLabel;
    cbFFServerMode: TComboBox;
    cbUseFirebirdEmbeded: TCheckBox;
    cbUseTrustedNTConnection: TCheckBox;
    cbFIBCharSet: TComboBox;
    lbFIBCharSet: TLabel;
    procedure FormShow(Sender: TObject);
    procedure cbUseFirebirdEmbededClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbUseTrustedNTConnectionClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLogon: TFormLogon;

implementation

{$R *.dfm}

uses
  {$IFDEF LANGUAGE}
  JvGnugettext,
  {$ENDIF LANGUAGE}
  VCSCommon, SrvConst, JVCSSrvResources;

resourcestring
  RsProviderName = 'P&rovider :';
  RsDataSource   = '&Datasource:';

procedure TFormLogon.FormCreate(Sender: TObject);
begin
  try
    {$IFDEF UIBSRV}
    lbFIBCharSet.Visible := True;
    lbFIBCharSet.Top := EditDatabase.Top + EditDatabase.Height + 3;
    cbFIBCharSet.Visible := True;
    cbFIBCharSet.Top := lbFIBCharSet.Top;
    cbUseFirebirdEmbeded.Visible := True;
    cbUseFirebirdEmbeded.Top := cbFIBCharSet.Top + cbFIBCharSet.Height + 8;
    {$ENDIF UIBSRV}
    {$IFDEF MSSQLSRV}
    with cbUseTrustedNTConnection do
    begin
      Top := cbUseFirebirdEmbeded.Top;
      Visible := True;
    end;
    cbUseTrustedNTConnectionClick(cbUseTrustedNTConnection);
    {$ENDIF MSSQLSRV}
  finally
    {$IFDEF LANGUAGE}
    //Translate form
    TranslateComponent(Self);
    {$ENDIF LANGUAGE}
  end;
end;

procedure TFormLogon.FormShow(Sender: TObject);
begin
  if EditPassword.Visible then
  begin
    if (EditPassword.Text = '') and (EditUser.Text <> '') then
    begin
      EditPassword.SetFocus;
    end;
  end;

  Caption := Format(JVCSRES_Logon_to_37s, [GetSrvPortFromId(cServerId)]);

  {$IFDEF MYSQLSRV}
  DatabaseLabel.Enabled := True;
  EditDatabase.Enabled := DatabaseLabel.Enabled;
  EditDatabase.Color := clWindow;
  {$ENDIF MYSQLSRV}

  {$IFDEF MSSQLSRV} // (-- LB)
  DatabaseLabel.Enabled := True;
  EditDatabase.Enabled := DatabaseLabel.Enabled;
  EditDatabase.Color := clWindow;
  {$ENDIF MSSQLSRV}

  {$IFDEF UIBSRV} //-- THu
  DatabaseLabel.Enabled := True;
  EditDatabase.Enabled := DatabaseLabel.Enabled;
  EditDatabase.Color := clWindow;
  cbUseFirebirdEmbededClick(cbUseFirebirdEmbeded);
  // No charset was default in FreeVCS and early JVCS 2.40 RC's
  if cbFIBCharSet.Text = '' then
    cbFIBCharSet.ItemIndex := cbFIBCharSet.Items.IndexOf('NONE');
  {$ENDIF UIBSRV}

  {$IFDEF ORACLESRV}
  {$ENDIF ORACLESRV}

  {$IFDEF MSSQLSRV}
  {$ENDIF MSSQLSRV}

  {$IFDEF ADOSRV} // -- PL
  LabelServer.Caption := RsProviderName;
  DatabaseLabel.Caption :=RsDataSource;
  DatabaseLabel.Enabled := True;
  EditDatabase.Enabled := DatabaseLabel.Enabled;
  EditDatabase.Color := clWindow;
  {$ENDIF ADOSRV}
end;

procedure TFormLogon.cbUseFirebirdEmbededClick(Sender: TObject);
begin
  {$IFDEF UIBSRV}
  if cbUseFirebirdEmbeded.Checked then
  begin
    LabelUser.Visible := False;
    EditUser.Visible := False;
    LabelPassword.Visible := False;
    EditPassword.Visible := False;
    LabelServer.Visible := False;
    EditServer.Visible := False;
    EditUser.Text := 'SYSDBA';
  end
  else
  begin
    LabelUser.Visible := True;
    EditUser.Visible := True;
    LabelPassword.Visible := True;
    EditPassword.Visible := True;
    LabelServer.Visible := True;
    EditServer.Visible := True;
  end;
  {$ENDIF UIBSRV}
end;

procedure TFormLogon.cbUseTrustedNTConnectionClick(Sender: TObject);
begin
  {$IFDEF MSSQLSRV}
  //if checked authentication is done over windows user instead of DBMS
  //user management. So we need no user, and no password
  if cbUseTrustedNTConnection.Checked then
  begin
    LabelUser.Visible := False;
    EditUser.Visible := False;
    LabelPassword.Visible := False;
    EditPassword.Visible := False;
  end
  else
  begin
    LabelUser.Visible := True;
    EditUser.Visible := True;
    LabelPassword.Visible := True;
    EditPassword.Visible := True;
  end;
  {$ENDIF MSSQLSRV}
end;
end.

