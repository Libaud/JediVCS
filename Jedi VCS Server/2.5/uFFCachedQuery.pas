{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: uFFCachedQuery.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is: Nils Seidel
Code move to JEDIVCS: Eivind Bakkestuen (eivind.bakkestuen@nexusdb.com)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History below

Known Issues:
-----------------------------------------------------------------------------

Unit history:

{
 Jan 13, 2002 - Added TffCachedQuery by Lachlan Gemmell
    Done simply by a copy, paste and then search and replace operation but
    appears to work fine. Interfaces could be used toprovide a more elegant
    way of sharing common code.

 March 8, 2001 - Adapted for FF 2.01 by Pleun van der Lugt
                [pleun.van.der.lugt@acdbv.nl]
   - InternalOpen still needs to be modified for automatic detection
     of ctMaxMsgSize! (See InternalOpen)

 March 23, 2000 - Adapted for FF 1.57 by Chris Wyszkowski
 November 19-21/99 Cleanup and changes to reflect ff coding
   style by Thorsten Engler [thorsten.engler@gmx.net]
   - removed HAS_WW (this should go into its own unit...)
   - optimizations for batch append
   - adaptions for serverside filters
   - added CacheMode

 November 12/99 Update from Sean Winstead [sean@surehand.com]:
    -new ctEndOfBatch flag saves an unncessary batch read when you
     reach the end of the record batch
    -TffCachedTable and TffCachedWWTable implemented using HAS_WW define

 Delphi 3/4 Version of Transparent Cache Table for TffwwTable
 Chris Wyszkowski 19990503
 Chris@ICGResearch.com

 -CacheOnOptimum Implemented
 -Modified for D3/D4
 -Componentized, incude in your own package

 Thanks to all earlier contributors:
 Extended by Richard Wilson 21 Sept 1998 - Batch Append Support
 Thanks to NILS SEIDEL modified 9 May 1998 - Original Development

Template for use
      for tableX
        CacheOn(RecordCount) ; .. or try a number to suit
        or
        CacheOnOptimum;
        for n := 1 to nOfData
          BatchAppend ;
          tableXID.Value := ?? ;
          tableXValue.Value := ???
          etc
          BatchPost ;
        end ;
        BatchFinalPost ;
        CacheOff ;
      end ;
My speed measures on a local machine have it about twice as fast as
Append .... Post sequences
BUT it only has ONE message per call to FFDbiInsertRecordBatch
NB FFDbiInsertRecordBatch is called every time the buffer is full
    and at the end if there is unwritten data.
Hence network traffic should be considerably reduced.
   }

2003/02/11  EBakkest    - Added to JediVCL

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  uFFCachedQuery - speeds up FF queries by doing batch reads.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit uFFCachedQuery;



interface

uses
  {$IFDEF Windows}
  WinTypes, WinProcs, DBIErrs, DBITypes, DBIProcs,
  {$ELSE}
  Windows, BDE,
  {$ENDIF}
  SysUtils,
  Classes,
  DB,
  FFLLBase,
  FFLLDict,
  FFClIntf,
  FFLLProt,
  FFDB;

const
  ffc_MaxCache :LongInt = 65536;  // default maximum cache buffer size

type
  TffCacheMode = (
    cmNone,
    cmRead,
    cmWrite
  );


	TffCachedQuery = class(TffQuery)
	protected                             {private}
		ctCacheMode: TffCacheMode;
		ctUpTo: LongInt;
		ctFullBuffer: PffByteArray;
		ctRecordBuffer: PffByteArray;
		ctErrors: PffLongIntArray;
		ctOneRec: PChar;
		ctNumRecs: LongInt;
		ctEndOfBatch: Boolean;
		ctMaxMsgSize: LongInt;

		procedure ctSetCaching(aValue   : TffCacheMode;
													 aHowMany : Integer;
													 aOptimum : Boolean);
	protected
		procedure InternalOpen; override;

		procedure InternalCancel; override;
		procedure InternalEdit; override;
		procedure InternalInsert; override;
		procedure InternalRefresh; override;
		procedure InternalDelete; override;
		procedure InternalFirst; override;
		procedure InternalPost; override;
  public
    procedure Next;

    procedure CacheOn(aCacheMode : TffCacheMode;
                      aHowMany   : Integer);
    procedure CacheOnOptimum(aCacheMode : TffCacheMode);
    procedure CacheOff;

    procedure BatchAppend;
    procedure BatchPost;
    procedure BatchFinalPost;

    property CacheMode: TffCacheMode read
      ctCacheMode;
  end;


implementation

uses
  FFConst,
  FFDBBase;


procedure TffCachedQuery.BatchAppend;
begin
  {if caching is deactivated, normal behaviour}
  if ctCacheMode = cmNone then
    Append
  else begin
    Assert(ctCacheMode = cmWrite, 'BatchAppend can only be called in cmWrite CacheMode');
    if not (State = dsInsert) then begin
      if ctUpTo = 0 then
        ctRecordBuffer := ctFullBuffer;
      DoBeforeInsert;
      DoBeforeScroll;
      InitRecord(ctOneRec);
      SetBookmarkFlag(ctOneRec, bfEOF);
      Move(ctOneRec^, ActiveBuffer^, dsRecBufSize);
      SetState(dsInsert);
      try
        DoOnNewRecord;
      except
        SetState(dsBrowse);
        raise;
      end;
      DataEvent(deDataSetChange, 0);
      DoAfterInsert;
      DoAfterScroll;
    end;
  end;
end;

procedure TffCachedQuery.BatchFinalPost;
begin
  if ctCacheMode <> cmNone then begin
    Assert(ctCacheMode = cmWrite, 'BatchFinalPost can only be called in cmWrite CacheMode');
    if ctUpTo > 0 then
      Check(InsertRecordBatch(ctUpTo, ctFullBuffer, ctErrors));
    ctUpTo := 0;
  end;
end;

procedure TffCachedQuery.BatchPost;
begin
  {if caching is deactivated, normal behaviour}
  if ctCacheMode = cmNone then
    Post
  else begin
    Assert(ctCacheMode = cmWrite, 'BatchPost can only be called in cmWrite CacheMode');
    if State = dsInsert then begin
      DoBeforePost;
      Move(ActiveBuffer^, ctRecordBuffer^, dsPhyRecSize);
      Inc(ctUpTo);
      Inc(PChar(ctRecordBuffer), dsPhyRecSize); {This moves the pointer along}
      SetState(dsBrowse);
      DoAfterPost;
      if ctUpTo = ctNumRecs then
        BatchFinalPost;
    end;
  end;
end;

procedure TffCachedQuery.CacheOff;
begin
  ctSetCaching(cmNone, 1, False);
end;

procedure TffCachedQuery.CacheOn(aCacheMode : TffCacheMode; aHowMany   : 
    Integer);
begin
  ctSetCaching(aCacheMode, aHowMany, False);
end;

procedure TffCachedQuery.CacheOnOptimum(aCacheMode : TffCacheMode);
begin
  ctSetCaching(aCacheMode, 1, True);
end;

procedure TffCachedQuery.ctSetCaching(aValue   : TffCacheMode; aHowMany : 
    Integer; aOptimum : Boolean);
begin
  if aValue <> ctCacheMode then begin
    {if caching should be activated}
    if aValue <> cmNone then begin
      {initialize the internal parameters}
      ctUpTo := 0;

      if aOptimum then begin            {determine optimum number of records }
        if dsPhyRecSize <= (ctMaxMsgSize - 4) then
          ctNumRecs := ((ctMaxMsgSize - 4) div dsPhyRecSize)
        else if dsPhyRecSize <= ffc_MaxCache then
          ctNumRecs := (ffc_MaxCache div dsPhyRecSize)
        else
          ctNumRecs := 1;
      end else begin
        if (aHowMany * dsPhyRecSize) > ffc_MaxCache then
          ctNumRecs := ffc_MaxCache div dsPhyRecSize
        else
          ctNumRecs := aHowMany;
      end;

      if ctNumRecs <= 1 then begin
        ctSetCaching(cmNone, 1, False);
        Exit;
      end;

      {get a Buffer for the readcache}
      FFGetZeroMem(ctFullBuffer, ctNumRecs * dsPhyRecSize);
      ctOneRec := AllocRecordBuffer;
      FFGetZeroMem(ctErrors, ctNumRecs * dsPhyRecSize);
      ctEndOfBatch := False;
      // ensure Eof / Bof is not set...
      ActivateBuffers;
      if aValue = cmRead then begin
        // position on the first record
        Check(ServerEngine.CursorSetToBegin(CursorID));
        Next;
      end;
    end else begin
      if ctCacheMode = cmWrite then
        BatchFinalPost;
      {if caching should not be activated, free the buffer}
      FreeMem(ctFullBuffer, ctNumRecs * dsPhyRecSize);
      FreeRecordBuffer(ctOneRec);
      FreeMem(ctErrors, ctNumRecs * dsPhyRecSize);
      ctEndOfBatch := False;
    end;
    ctCacheMode := aValue;
  end;
end;

procedure TffCachedQuery.InternalCancel;
begin
  Assert(ctCacheMode = cmNone,'InternalCancel not allowed in caching mode');
  inherited;
end;

procedure TffCachedQuery.InternalDelete;
begin
  Assert(ctCacheMode = cmNone,'InternalDelete not allowed in caching mode');
  inherited;
end;

procedure TffCachedQuery.InternalEdit;
begin
  Assert(ctCacheMode = cmNone,'InternalEdit not allowed in caching mode');
  inherited;
end;

procedure TffCachedQuery.InternalFirst;
begin
  Assert(ctCacheMode = cmNone,'InternalFirst not allowed in caching mode');
  inherited;
end;

procedure TffCachedQuery.InternalInsert;
begin
  Assert(ctCacheMode = cmNone,'InternalInsert not allowed in caching mode');
  inherited;
end;

procedure TffCachedQuery.InternalOpen;
begin
  inherited;
   ctMaxMsgSize := ffc_MaxWinsockMsgSize;     //TCP/IP or IPX
//   ctMaxMsgSize := ffc_MaxSingleUserMsgSize;  //SingleUser
end;

procedure TffCachedQuery.InternalPost;
begin
  Assert(ctCacheMode = cmNone,'InternalPost not allowed in caching mode');
  inherited;
end;

procedure TffCachedQuery.InternalRefresh;
begin
  Assert(ctCacheMode = cmNone,'InternalRefresh not allowed in caching mode');
  inherited;
end;

procedure TffCachedQuery.Next;
var
  Error                       : TffResult;
  GNError                     : TffResult;
  Cancel                      : Boolean;
begin
  {if caching is deactivated, normal behaviour}
  if ctCacheMode = cmNone then
    inherited
  else begin
    Assert(ctCacheMode = cmRead, 'Next can only be called in cmRead CacheMode');
    {if there are records in the cache}
    if ctUpTo <> 0 then begin
      {increment the record in the buffer}
      Inc(PChar(ctRecordBuffer), dsPhyRecSize);
    end
    else begin
      {no records in cache, fill it and return the first one}
      {copy n records from the server and store them in the buffer}
      if not ctEndOfBatch then begin
        Cancel := false;
        repeat
          Error := GetRecordBatchEx(ctNumRecs, ctUpTo, ctFullBuffer, GNError);
          if Error = DBIERR_FF_FilterTimeout then begin
            if dsCancelServerFilter then
              break;
          end else
            break;
        until false;
        if GNError <> DBIERR_FF_FilterTimeout then {ccw}
          ctEndOfBatch := GNError <> DBIERR_NONE;
        if (Error <> DBIERR_NONE) or (ctUpTo = 0) then begin
          ClearBuffers; // this sets eof!
          ctEndOfBatch := False;
          Exit;
        end;
        {return the first record}
        ctRecordBuffer := ctFullBuffer;
      end
      else begin
        Last;
        ctEndOfBatch := False;
        Exit;
      end;
    end;
    {move the Values of the next record to the activebuffer of the table}
    Move(ctRecordBuffer^, ActiveBuffer^, dsPhyRecSize);
    {there is one record less in the buffer}
    Dec(ctUpTo);
  end;
end;

{===Initialization routine===========================================}
initialization
	RegisterClass(TffCachedQuery);
end.
