(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSServerFunctions.pas

The Initial Developer of the original code (JEDI VCS) is:
  Thomas Huber (thomas_d_huber@gmx.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2009/11/22  THuber - new unit (new home for some common server functions)

-----------------------------------------------------------------------------*)
{$I JEDI.INC}

unit JVCSServerFunctions;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I COMPOPT.INC}

interface

uses
  windows, classes;

  function GetJvcsServerLogFile : String;
  function GetServerLogFileSizeKByte : Integer;
  procedure SaveServerLogFile(slLogLines : TStrings);
  procedure ShowServerLogFile(const hWindow : HWND);
  procedure MoveServerLog2CommonAppFolder;

implementation

uses
    Sysutils, ShellApi, JVCSFunctions, HashUnit, jclSysinfo;

const
  COMMON_APP_SUB_DIR = '\JEDI\jvcs\';
  SERVERLOGFILE = 'server.log';
  DEFAULT_EDITOR = 'notepad.exe';


//------------------------------------------------------------------------------

function GetJvcsServerLogFile : String;
var
  sPrefix : String;
begin
  // As there could be more several service instances, file name need to be
  // adjusted. We use the pathname as hashed prefix.
  sPrefix := IntToStr(Dword(HashLine(GetSelfFileLocation,true,false))) + '_';
  result := jclSysInfo.GetCommonAppdataFolder + COMMON_APP_SUB_DIR + sPrefix + SERVERLOGFILE;
  ForceDirectories(ExtractFileDir(result));
end;

//------------------------------------------------------------------------------
// up to Version 2.44 the server log file was located as server.log under the
// server directory which is not "allowed" any more under VISTA & Windows 7.
// therefore we move/copy it into the right directory (if not already existing there)
//
procedure MoveServerLog2CommonAppFolder;
begin
  if not fileexists(GetJvcsServerLogFile) then
  begin
    CopyFile( PChar(GetSelfFileLocation + SERVERLOGFILE)
            , PChar(GetJvcsServerLogFile)
            , False
            );
    DeleteFile(GetSelfFileLocation + SERVERLOGFILE);
  end;
end;

//------------------------------------------------------------------------------

function GetServerLogFileSizeKByte : Integer;
var
  SearchRec: TSearchRec;
begin
  result := -1;
  try
    if (FindFirst ( GetJvcsServerLogFile
                  , faAnyFile
                  , SearchRec) = 0) then
      result := (SearchRec.Size div 1024);
  finally
    FindClose(SearchRec);
  end;
end;

//------------------------------------------------------------------------------

procedure ShowServerLogFile(const hWindow : HWND);
begin
  if FileExists(GetJvcsServerLogFile) then
     OpenDocument(PChar(DEFAULT_EDITOR)
                ); { *Converti depuis ShellExecute* }
end;

//------------------------------------------------------------------------------
// Exception handling to be handled in calling function

procedure SaveServerLogFile(slLogLines : TStrings);
var
  LogFile: TextFile;
  ii: Integer;
begin
  if Assigned(slLogLines) then
  begin
    AssignFile(LogFile, GetJvcsServerLogFile);
    if FileExists(GetJvcsServerLogFile) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    try
      for ii := 0 to slLogLines.Count - 1 do
        WriteLn(LogFile, slLogLines[ii]);
    finally
      CloseFile(LogFile);
    end;
  end;
end;

end.

