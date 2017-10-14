(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSSessionList.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
ToDo
- faster IndexOf
-----------------------------------------------------------------------------

Unit history:

2006/05/01  USchuster - new unit

-----------------------------------------------------------------------------*)

unit JVCSSessionList;

{$I jedi.inc}
{$I compopt.inc}

interface

uses
  SysUtils, Contnrs;

type
  TJVCSSession = class(TObject)
  private
    FAccessID: Integer;
    FTANr: Integer;
    FRights: Integer;
    FBranchID: Integer;
    FProjectID: Integer;
    FFaults: Integer;
    FExpires: TDateTime;
  public
    constructor Create(AccessID: Integer);
    property AccessID: Integer read FAccessID;
    property TANr: Integer read FTANr write FTANr;
    property Rights: Integer read FRights write FRights;
    property BranchID: Integer read FBranchID write FBranchID;
    property ProjectID: Integer read FProjectID write FProjectID;
    property Faults: Integer read FFaults write FFaults;
    property Expires: TDateTime read FExpires write FExpires;
  end;

  TJVCSSessionList = class(TObject)
  private
    FItems: TObjectList;
    function IndexOf(AccessID, ATANr: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function AddNewSession(AccessID: Integer): TJVCSSession;
    procedure DeleteSession(AccessID, ATANr: Integer);
    function FindSession(AccessID, ATANr: Integer): TJVCSSession;
    function GetSessionBranchID(AccessID, ATANr: Integer): Integer;
    function GetUserSessionCount(AccessID: Integer): Integer;
  end;

function SessionList: TJVCSSessionList;

implementation

var
  FJVCSSessionList: TJVCSSessionList = nil;

function SessionList: TJVCSSessionList;
begin
  if not Assigned(FJVCSSessionList) then
    FJVCSSessionList := TJVCSSessionList.Create;
  Result := FJVCSSessionList;
end;

constructor TJVCSSession.Create(AccessID: Integer);
begin
  inherited Create;
  FAccessID := AccessID;
  FTANr := 0;
  FRights := -1;
  FBranchID := 1;
  FProjectID := -1;
  FFaults := 0;
  FExpires := Now;
end;

constructor TJVCSSessionList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TJVCSSessionList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJVCSSessionList.AddNewSession(AccessID: Integer): TJVCSSession;
begin
  FItems.Add(TJVCSSession.Create(AccessID));
  Result := TJVCSSession(FItems.Last);
end;

procedure TJVCSSessionList.DeleteSession(AccessID, ATANr: Integer);
var
  Idx: Integer;
begin
  Idx := IndexOf(AccessID, ATANr);
  if Idx <> -1 then
    FItems.Delete(Idx);
end;

function TJVCSSessionList.FindSession(AccessID, ATANr: Integer): TJVCSSession;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOf(AccessID, ATANr);
  if Idx <> -1 then
    Result := TJVCSSession(FItems[Idx]);
end;

function TJVCSSessionList.GetSessionBranchID(AccessID, ATANr: Integer): Integer;
var
  Idx: Integer;
begin
  Result := 1;
  Idx := IndexOf(AccessID, ATANr);
  if Idx <> -1 then
    Result := TJVCSSession(FItems[Idx]).BranchID;
end;

function TJVCSSessionList.GetUserSessionCount(AccessID: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Pred(FItems.Count) do
    if TJVCSSession(FItems[I]).AccessID = AccessID then
      Inc(Result);
end;

function TJVCSSessionList.IndexOf(AccessID, ATANr: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Pred(FItems.Count) do
    if (TJVCSSession(FItems[I]).AccessID = AccessID) and
      (TJVCSSession(FItems[I]).TANr = ATANr) then
    begin
      Result := I;
      Break;
    end;
end;

initialization

finalization
   FJVCSSessionList.Free;

end.