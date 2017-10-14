(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: About.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is:
  Thomas Hensle (freevcs@thensle.de)
Code move to JEDI VCS: Thomas Huber (Thomas_D_Huber@gmx.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
Nov/03:
- some credits are missing (there are some additional components listed in
  SrvMain.pas but not all of them are still in use)
-----------------------------------------------------------------------------

Unit history:

2003/11/16  USchuster - derived from the GUI client About.pas and included
                        the content from the About MessageDlg (mantis #658)
2003/12/27  THuber    - removed Src Version const (no use)
                      - Updated Contributors list according to Releasenotes
2004/01/17  USchuster - is now based on TJVCSBaseAboutDlg
                      - now the current port maintainers will be listed in credits
2004/11/16  USchuster - added dxGetText support for localization with over IFDEF LANGUAGE
                      - res strings and displayed strings changed to resourcestrings

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster - changes for Firebird port over JVCL UIB components
                        by Pierre Y. (partly adapted)
2007/01/14  USchuster - updated FIB port mail address and "synced" .dfm with parent

--- 2.50 Beta 2 was released...
2009/12/23  THuber    #5063 support for  I  n f  o r m i x  port removed
                      #5066 support for  F l a s h F i l e r  port removed
                      #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
-----------------------------------------------------------------------------*)

unit SrvAbout;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  JVCSBaseAboutDialog, StdCtrls, ExtCtrls, ComCtrls, JvExStdCtrls,
  JvRichEdit;

type
  TJVCSSrvAbout = class(TJVCSBaseAboutDlg)
    procedure FormCreate(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    { Private declarations }
    FCreatedCredits: Boolean;
  protected
    function GetCredits: string;
    function GetVersionInformation(AwithFileDate: Boolean): string; override;
  public
    { Public declarations }
  end;

var
  JVCSSrvAbout: TJVCSSrvAbout;

procedure ShowSrvAboutForm;

implementation

uses
  {$IFDEF LANGUAGE}
  JvGnugettext,
  {$ENDIF LANGUAGE}
  VCSCommon, SrvConst, ServerCryptInt, JVCSSrvResources;

{$R *.dfm}

procedure ShowSrvAboutForm;
begin
  JVCSSrvAbout := TJVCSSrvAbout.Create(Application);
  try
    JVCSSrvAbout.ShowModal;
  finally
    JVCSSrvAbout.Free;
  end;
end;

type
  TSrvAuthorDescRec = record
    SrvId: TSrvIdentifier;
    SrvPort: string[50];
    OriginalAuthor: string;
    OriginalAuthorEmail: string;
  end;

  TSrvComponentCreditsDescRec = record
    SrvId: TSrvIdentifier;
    SrvComponents: string;
  end;

const
  cSrvAuthorInfo: array [0..4] of TSrvAuthorDescRec =
  (
   (SrvId: svIdOraOci; SrvPort: 'Oracle';
     OriginalAuthor: 'Holger Dors'; OriginalAuthorEmail: 'dors@kittelberger.de'),
   (SrvId: svIdMSSQL; SrvPort: 'MSSQL';
     OriginalAuthor: 'Marco Gosselink'; OriginalAuthorEmail: 'mgosselink@zonnet.nl'),
   (SrvId: svIdMySQL; SrvPort: 'MySQL';
     OriginalAuthor: 'Ludo Brands'; OriginalAuthorEmail: 'freevcsmysql@free.fr'),
   (SrvId: svIdADO; SrvPort: 'ADO';
     OriginalAuthor: 'Pascal Lauret'; OriginalAuthorEmail: 'p.golem@wanadoo.fr'),
   (SrvId: svIdUIB; SrvPort: 'Firebird/Interbase/Yaffil';
     OriginalAuthor: 'Pierre Y.'; OriginalAuthorEmail: 'pierre.y@gmail.com')
  );

procedure TJVCSSrvAbout.FormCreate(Sender: TObject);
begin
  try
    inherited;
    {$IFDEF BETAVER}
    pnWarning.Visible := True;
    pnWarning.Caption := JVCSRES_Beta_Version;
    {$ENDIF BETAVER}
    Caption := Caption + ' (' + GetSrvPortFromId(cServerId) + ')';
    FCreatedCredits := False;
  finally
    {$IFDEF LANGUAGE}
    //Translate form
    TranslateComponent(Self);
    {$ENDIF LANGUAGE}
  end;
end;

function TJVCSSrvAbout.GetCredits: string;
var
  I,J: Integer;
  S: string;
  CurrentMaintainer: string;
begin
  Result :=
    JVCSRES_Based_on_FreeVCS_server_source_by_Thomas_Hensle + #13 +
    '(freevcs@thensle.de) - http://www.freevcs.de' + #13 +
    #13 +
    JVCSRES_Based_on_ICS47MidWare_Components_38_SrvTst_Demo_Application + #13 +
    JVCSRES_169_2000_45_2004_by_Fran231ois_Piette_45_http584747www46overbyte46be + #13 + #13;

  for I := Low(cSrvAuthorInfo) to High(cSrvAuthorInfo) do
  begin
    S := Format(JVCSRES_37s_port_by + #13 + '  %s (%s)', [cSrvAuthorInfo[I].SrvPort,
      cSrvAuthorInfo[I].OriginalAuthor, cSrvAuthorInfo[I].OriginalAuthorEmail]);
    CurrentMaintainer := '';
    for J := Low(cSrvInfo) to High(cSrvInfo) do
      if cSrvInfo[J].SrvId = cSrvAuthorInfo[I].SrvId then
      begin
        CurrentMaintainer := cSrvInfo[J].SrvMaintainer;
        Break;
      end;
    if (CurrentMaintainer = '') or (CurrentMaintainer = 'not named') then //do not localize
      S := S + #13 + JVCSRES_currently_not_maintained
    else
      S := S + #13 + Format(JVCSRES_currently_maintained_by + #13 + '  %s', [CurrentMaintainer]);
    Result := Result + S;

    if I < High(cSrvAuthorInfo) then
      Result := Result + #13 + #13;
  end;
end;

function TJVCSSrvAbout.GetVersionInformation(AwithFileDate: Boolean): string;
var
  Msg: string;

  LFileName: string;
  SrvCryptVersion: Integer;
begin
  Msg := Format(JVCSRES_JEDI_VCS_Application_Server_4037s41_, [GetSrvVersionString(cServerId, False)])
    {$IFDEF BETAVER} + JVCSRES_Beta {$ENDIF} + #13;

  Msg := Msg + lblProductVersion.Caption + #13;

  if AwithFileDate then
  begin
    LFileName := Application.ExeName;
    Msg := Msg + Format(JVCSRES_Filedate58_37s, [DateTimeToStr(FileDateToDateTime(FileAge(LFileName)))])
           + #13;
  end;
  Msg := Msg + #13;

  Msg := Msg + JVCSRES_This_server_is_also_available_as_NT_service_application46 + #13#13;

  Msg := Msg + JVCSRES_Installed_versions58 + #13;

  SrvCryptVersion := fvcsCryptGetVersion;
  Msg := Msg + Format('ServerCrypt.dll (%d.%2.2d)', [SrvCryptVersion div 100,
    SrvCryptVersion mod 100]) + #13;

  Result := Msg;
end;

procedure TJVCSSrvAbout.PageControl1Change(Sender: TObject);
begin
  inherited;
  if (PageControl1.ActivePage = tsCredits) and (not FCreatedCredits) then
  begin
    Memo3.Lines.Text := GetCredits;
    FCreatedCredits := True;
  end;
end;

end.
