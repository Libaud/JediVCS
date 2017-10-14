{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: ServerCryptInt.pas

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
 4-Mrz-2000 05:51:53 (GMT+1) > [sysdba on K6] checked in Beta v. 04.Mar.00
   - Version Dors/Gossselink  

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI-VCS

--- branchpoint for 2.5 dev server ---

2005/05/06  USchuster- completed function in implementation section to make
                       it compiling with FPC
2005/05/29  USchuster- changes for Unix                       

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  ServerCryptInt - Interface unit for ServerCrypt.dll
  Password decryption / compare for FreeVCS application server application.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit ServerCryptInt;

{$I jedi.inc}
{$I compopt.inc}

interface

function fvcsCryptGetVersion: Integer;
function fvcsCryptCompare(Transmitted, Stored: ShortString): Boolean;
function fvcsSrvDeCrypt(PW: ShortString): ShortString;
function fvcsSrvEnCrypt(PW: ShortString): ShortString;

implementation

const
  {$IFDEF MSWINDOWS}
  ServerCrypt = 'ServerCrypt.dll';
  {$ENDIF MSWINDOWS}
  {$IFDEF UNIX}
  ServerCrypt = 'ServerCrypt.so';
  {$ENDIF UNIX}

{$IFNDEF SRVCRYPTINTERNALDUMMY}
function fvcsCryptGetVersion: Integer; external ServerCrypt name 'fvcs_CryptGetVersion';
function fvcsCryptCompare(Transmitted, Stored: ShortString): Boolean; external ServerCrypt name 'fvcs_CryptCompare';
function fvcsSrvDeCrypt(PW: ShortString): ShortString; external ServerCrypt name 'fvcs_SrvDeCrypt';
function fvcsSrvEnCrypt(PW: ShortString): ShortString; external ServerCrypt name 'fvcs_SrvEnCrypt';
{$ELSE}
function fvcsCryptGetVersion: Integer;
begin
  Result := 101;
end;

function fvcsCryptCompare(Transmitted, Stored: ShortString): Boolean;
begin
  Result := True;
end;

function fvcsSrvDeCrypt(PW: ShortString): ShortString;
begin
  Result := PW;
end;

function fvcsSrvEnCrypt(PW: ShortString): ShortString;
begin
  Result := PW;
end;
{$ENDIF ~SRVCRYPTINTERNALDUMMY}

end.
