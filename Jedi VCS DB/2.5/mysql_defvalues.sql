/* ==========================================================================
   Database name:  JEDI VCS
   DBMS name:      MySQL
   ==========================================================================
   Description:    Insert default values in new MySQL database
   DB Version:     1.1
   ==========================================================================
   History
     18.12.2002 LBrands   - created (stdvalues_mysql.sql)
     13.01.2004 USchuster - changed to mysql_defvalues.sql
                          - prepared for first JVCS release
   ========================================================================== */

/* ============================================================ */
/*   Standard admin/ File Family                                */
/* ============================================================ */

insert into users
   values (0,
           'sysdba',
           '#60#1#14#17#95#77#96#17#5#56#60#14#108#96#95#63#36#12#31#58#91#125#86#0#20#30#27#33#124#116#65#7#4#31#61#51#115#83#106#11#61#38#237#64#186#73#94#14#9#12',
           '0',
           4,
           '0',
           'JEDI VCS default administrator - create a new administrator and delete sysdba after installation or change the default password immediately!');

insert into ffamily
   values (0, 
           'Delphi forms',
           '.pas',
           '.dfm',
           'Delphi units & associated form files. This file family is neccessary for backward compatibility to earlier versions and should not be deleted or changed!');

/* ============================================================ */
/*   Milestone examples                                         */
/* ============================================================ */

insert into mstones
   values (0,
           '1',
           'Alpha 1',
           'Descriptions, Diagrams, Flowcharts, Customer''s specification...');

insert into mstones
   values (0,
           '2',
           'Alpha 2',
           'Basic design, Interfaces, Core units...');

insert into mstones
   values (0,
           '3',
           'Alpha 3',
           'Graphical user interface');

insert into mstones
   values (0,
           '4',
           'Beta 1',
           'Test by developer, Bug fixes...');

insert into mstones
   values (0,
           '5',
           'Beta 2',
           'Field test by development team, inhouse tests...');

insert into mstones
   values (0,
           '6',
           'Beta 3',
           'Field test by customer...');

insert into mstones
   values (0,
           '7',
           'Pre release',
           'Approval by customer...');

insert into mstones
   values (0,
           '8',
           'Release',
           'Official release');
