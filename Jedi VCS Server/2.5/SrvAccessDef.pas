{ $NoKeywords }
(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SrvAccessDef.pas

The Initial Developer of the original FreeVCS-Code (FreeVCS) is: Thomas Hensle (freevcs@thensle.de)
Code move to JEDIVCS: Holger Dors (dors@kittelberger.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

 Aug '99 by Thomas Hensle - http://www.freevcs.de

2003/02/04  HDors    - 1st Migrationstep from FreeVCS code to JEDI-VCS

-----------------------------------------------------------------------------*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  AccessDef.inc - Access level definition for FreeVCS application server.
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

 
(******************************************************************************
  Levels: (for now)
    - (-1) ?:
      unknown (or expired) user account.
    - (0) Guest:
      Per default no rights (except log into the server and view a list of the
      projects from the archive). Additional rights for Guest accounts must be
      granted for every project by an Administrator via
      "Project related rights".
    - (1) read-only:
      View all information, except for userlist & server options.
      Change nothing.
    - (2) read-write:
      View and change all information, except for userlist & server options.
      Check in only modules checked out self.
    - (3) Project administrator:
      Do whatever he/she wants related to a project.
      View and change all project information.
    - (4) Archive administrator:
      Do whatever he/she wants.
      View and change all project, archive & server information.
      Check in modules checked out by other users.
      Add new users, remove users, log out any user.

  Access levels may be granted in two ways:
    - As the default access right, stored in Users & assigned in the login
      procedure.
    - As a project related right, stored in PJUsers & assigned if entering a
      new project. (If such a project related level is not defined the user will
      get his default rights related to the login procedure.)

 ******************************************************************************)

unit SrvAccessDef;

{$I COND_DEF.INC}

interface

const
  (* Return values of GetArchiveRelatedRight/ GetProjectRelatedRight *)
                   // -1 = unknown user/ transaction ID
  LevelNone   = 0; // 0: Guest account
  LevelRO     = 1; // 1 - 3: these levels may be granted by default or related
  LevelRW     = 2; //        to single projects.
  LevelPAdmin = 3; //
  LevelAAdmin = 4; // 4: level cannot be granted related to single projects.
                   //    Archive administrators keep your rights.

  (* You should not change the strings below!
     The client searches for the substring 'Granted level: None' when parsing
     the server's error message! Changing this string may lead to immediate
     client log out after any access fault. *)

  (* Unauthorized client request - message transmitted to client *)
  (* operation needs a higher access level *)
  Levels = 'You don''t have the required access right for this operation.' +
            #10#13 + 'Requested level: %s / Granted level: %s';

  (* server cannot find a valid account *)
  Unknown = 'Server reports an unknown or expired user account.';

  (* invalid client version *)
  InvClient = 'Server exception in object [%s]:' + #10#13 +
              'You are probably using an outdated client version.' + #10#13 +
              'This server requires at least client version %d.%2.2d.' + #10#13 +
              'Please contact your archive administrator.';

  (* invalid client timestamp *)
  InvClientTime = 'Server exception in object [%s]:' + #10#13 +
                  'You local time is outside of the required range.' + #10#13 +
                  '(+/- 2h Servertime max).' + #10#13 +
                  'Please contact your archive administrator.';

  (* Unauthorized client request - server log file message *)
  AccessFault = ' !! Access denied (403): %s - ID: %d - %s';

var
  AccLevel : Array[0..4] of String = ('Guest.',
                                      'Read-Only.',
                                      'Read-Write.',
                                      'Project Administrator.',
                                      'Archive Administrator.');

implementation
end.
