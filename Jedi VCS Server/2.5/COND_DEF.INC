{ $NoKeywords } // Prevent FreeVCS from expanding keywords in this file
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Cond_def.inc - Compiler directive for FreeVCS application server.

  Aug '99 by Thomas Hensle - http://www.thensle.de

  Enable/Disable only definitions marked by [changeable]
  All other conditionals should NOT be changed!

  2003/02/27  THuber   - new DB-port F I B P L U S S R V  added
                         (Firebird port over F I B + components)
  2003/03/02  THuber   - removed dfsbrowsedialog cond
                       - removed D4_UP definition
  2003/03/07  THuber   - removed A CCESSCTRL IFDEF
  2003/08/15  THuber   - added JVCS_DEFINE_IN_MAKEFILE part for use in makefile

  --- branchpoint for 2.5 dev server ---

  2004/12/07  USchuster- changes for Firebird port over JVCL UIB components
                         by Pierre Y. (partly adapted)

  --- 2.50 Beta 2 was released...
  2009/12/23  THuber  #5063 support for  I n f o r m i x  port removed
                      #5064 support for  I n t e r b a s e 6  port removed
                      Removed unnecessary L Z H Define as it's always used
                      #5066 support for  F l a s h F i l e r  port removed
                      #5065 support for  F i b p l u s  port removed
  2009/12/27  THuber  #5067 support for  D B I S A M removed
                      Removed S Q L D B M S  and S Q L D B M S L O G O N Defines
                      as by dropping  D B I S A M there are no file based
                      databases any more.
                      #5077 support for  Oracle < 9x removed, also B D E  version
                      special Oracle J O I N  - Syntax was removed we now only
                      support the A N S I - J o i n Syntax and therefore removed
                      this compiler setting.
  2010/01/24  THuber  Shortened JVCS_DEFINE_IN_MAKEFILE to JVCS_INMAKE to avoid compiler error.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

(* Common settings *)

// Add by F. Libaud

{.$define ZEOS_DISABLE_MYSQL}
{$define ZEOS_DISABLE_POSTGRESQL}
{$define ZEOS_DISABLE_DBLIB}
{$define ZEOS_DISABLE_ADO}
{$define ZEOS_DISABLE_INTERBASE}
{$define ZEOS_DISABLE_SQLITE}
{$define ZEOS_DISABLE_ORACLE}
{$define ZEOS_DISABLE_ASA}
{$define ZEOS_DISABLE_POOLED}

{$ifdef FPC}              // FPC
  {$if FPC_FULLVERSION<=20204}
    {$DEFINE OLDFPC}
  {$ifend}
{$endif}

{$IFDEF VER130}           // Delphi5 and Above?
  {$DEFINE D5_Up}
{$ENDIF}
{$IFDEF VER140}           // Delphi6 and Above?   -- LB
  {$DEFINE D5_Up}
  {$DEFINE D6_Up}
{$ENDIF}
{$IFDEF VER150}           // Delphi7 and Above?
  {$DEFINE D5_Up}
  {$DEFINE D6_Up}
  {$DEFINE D7_Up}
{$ENDIF}

{$A+,B-,E-,F-,G+,H+,I+,J+,K-,M-,N+,P+,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}

{$IFDEF JVCS_INMAKE}
  // ********************************************************************** //
  //                                                                        //
  //                   !!! Don't put here any DEFINE's !!!                  //
  //                                                                        //
  //    It's for use in makefile which DEFINE's JVCS_DEFINE_IN_MAKEFILE     //
  //    and all necessary other DEFINE's for the target server application  //
  //                                                                        //
  // ********************************************************************** //
{$ELSE}
  (*----------------------------------------------------------------------------*)
  (* Compile server as NT Service application *)
  (* Depending on the database server version, you need at least a professional
     version of D4 to compile the NT service!

     The name of the ini file depends on the name of executable, so JVCSSrv.exe
     reads JVCSSrv.ini.

     The name of the service is default "JediVCSService" but can be changed via
     the ini file with ServiceName= and ServiceDisplayName=
     *)

  {.$DEFINE NTSERVICESRV} // [changeable]
  (*----------------------------------------------------------------------------*)
  (* Basic debug information/ show debug messages in server display.
     Remember that the server's display line count & log file size will grow very
     fast if you set these values to true.
     Enable them only temporary for debug purposes!
     These options will significant slow down the server! *)

  {$DEFINE BETAVER}        // Beta reminder at start up [changeable]
  {$DEFINE DEBUG}          // basic debug information [changeable]

  {.$DEFINE DETAIL_DEBUG}   // detailed debug information [changeable]
                           // (show every field in Request/Response buffers)

  {.$DEFINE SQL_DEBUG}      // SQL debug information [changeable]
                           // (show all Open/Exec SQL statements)

  {$IFDEF NTSERVICESRV}    // NT service?
    {$UNDEF DEBUG}         // NT service cannot show debug messages
  {$ENDIF}

  {$IFDEF DEBUG}
    {$D+,C+,L+,O-,Q+,R-}
    {$HINTS ON}
    {$WARNINGS ON}
  {$ELSE}
    {$D-,C-,L-,O+,Q-,R-}
    {$UNDEF DETAIL_DEBUG}
    {$UNDEF SQL_DEBUG}
  {$ENDIF}

  (*--------------------------------------------------------------------------*)
  (* Database engine - enable only ONE condition                              *)
  (*--------------------------------------------------------------------------*)
  {.$DEFINE UIBSRV}         // JvUIB Port for Firebird, Interbase and Yaffil [changeable]
                            // Port by Pierre Y. (-- PrY)
  {.$DEFINE ORACLESRV}      // Oracle 9/10/11 (native version) [changeable]
                            // Port by Holger Dors (--hd)
  {.$DEFINE MSSQLSRV}       // MSSQL (ADO version) [changeable]
                            // Port by Marco Gosselink (--mg)
  {.$DEFINE MYSQLSRV}       // MySQL [changeable]
                            // Port by Ludo Brands (-- LB)

  {.$DEFINE ADOSRV}         // Generic port for ADO connection  [changeable]
                            // Port by Pascal Lauret(-- PL)  (from MSSQLSRV implementation)
  (*----------------------------------------------------------------------------*)
  (* Database engine - Local SQL does not support INSERT and UPDATE operations on
                       autoincrement fields (along with some other SQL features).
                       Use Tables for Insert/Update operations (DBISAM)
                       or Queries (Firebird/Oracle...) *)

  (*----------------------------------------------------------------------------*)
  (* Version archive live backup support.
     Only provided by Firebird, MSSQL, MySQL servers *)

  {$IFDEF UIBSRV}
    {$DEFINE DBBACKUPSUPPORT}
  {$ENDIF}
  {$IFDEF MSSQLSRV}
    {$DEFINE DBBACKUPSUPPORT}
  {$ENDIF}
  {$IFDEF MYSQLSRV}
    {$DEFINE DBBACKUPSUPPORT}
  {$ENDIF}
  (*----------------------------------------------------------------------------*)
  (* Show ServerObject exception messages in server display. *)

  {$DEFINE LOGEXCEPTION}
  (*----------------------------------------------------------------------------*)
  (* Uptime timer resolution 1sec / 1min *)

  {$DEFINE UPTIME1S}

  {$IFDEF NTSERVICESRV}    // NT service?
    {$UNDEF UPTIME1S}      // NT service has no GUI - 1min is enough
  {$ENDIF}
  (*----------------------------------------------------------------------------*)

{$ENDIF JVCS_INMAKE}

