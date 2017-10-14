/* ==========================================================================
   Database name:  JEDI VCS
   DBMS name:      MySQL
   ==========================================================================
   Description:    Create all necessary database objects for JEDI VCS DB
   DB Version:     1.1
   ==========================================================================
   History
     07.01.2003 LBrands   - created (mysql.sql)
     17.04.2003 LBrands   - minor fixes
     13.01.2004 USchuster - changed to mysql_create.sql
     17.01.2004 USchuster - changed keyfield in MDBUGS from RECORDNR to RECORDID
   ========================================================================== */

CREATE TABLE blobs (
  blobid int(11) NOT NULL auto_increment,
  revisionid int(11) default NULL,
  extension varchar(20) default NULL,
  origtime datetime default NULL,
  origsize int(11) default NULL,
  origcrc int(11) default NULL,
  compsize int(11) default NULL,
  compcrc int(11) default NULL,
  filedata longtext,
  PRIMARY KEY  (blobid),
  KEY i_blobs_ext (extension),
  KEY i_blobs_rev (revisionid),
  KEY I_BLOBS_REVISIONID (revisionid)
) ;

CREATE TABLE bugs (
  bugid int(11) NOT NULL auto_increment,
  bug varchar(250) NOT NULL default '',
  keywords varchar(250) default NULL,
  severity int(11) NOT NULL default '0',
  created datetime default NULL,
  createdby int(11) NOT NULL default '0',
  flags int(11) default NULL,
  description text,
  reportedby text,
  workaround text,
  status int(11) default NULL,
  PRIMARY KEY  (bugid),
  KEY i_bugs_bug (bug)
);

CREATE TABLE ffamily (
  familyid int(11) NOT NULL auto_increment,
  name varchar(50) default NULL,
  parentext varchar(10) default NULL,
  familyext varchar(250) default NULL,
  description text,
  PRIMARY KEY  (familyid),
  KEY i_ffamily_name (name)
);

CREATE TABLE groups (
  groupid int(11) NOT NULL auto_increment,
  parentid int(11) default NULL,
  treelevel int(11) default NULL,
  flags int(11) default NULL,
  name varchar(50) default NULL,
  description text,
  PRIMARY KEY  (groupid),
  KEY i_groups_names (name),
  KEY i_groups_parent (parentid),
  KEY I_GROUPS_PARENTID (parentid)
);

CREATE TABLE labels (
  labelid int(11) NOT NULL auto_increment,
  label varchar(250) NOT NULL default '',
  description text,
  PRIMARY KEY  (labelid),
  KEY i_labels_label (label)
);

CREATE TABLE logcomm (
  recordnr int(11) NOT NULL auto_increment,
  logid int(11) NOT NULL default '0',
  revisionid int(11) NOT NULL default '0',
  PRIMARY KEY  (recordnr),
  KEY I_LOGCOMM_LOGID (logid),
  KEY I_LOGCOMM_REVISIONID (revisionid)
);

CREATE TABLE mdbugs (
  recordid int(11) NOT NULL auto_increment,
  bugid int(11) NOT NULL default '0',
  moduleid int(11) NOT NULL default '0',
  done enum('0','1') default '0',
  PRIMARY KEY  (recordid),
  KEY I_MDBUGS_BUGID (bugid),
  KEY I_MDBUGS_MODULEID (moduleid)
);

CREATE TABLE modules (
  moduleid int(11) NOT NULL auto_increment,
  name varchar(250) default NULL,
  path varchar(250) default NULL,
  tstamp datetime default NULL,
  readonly enum('0','1') default '0',
  userid int(11) default NULL,
  locktstamp datetime default NULL,
  description text,
  lastuser int(11) default NULL,
  familyid int(11) default NULL,
  flags int(11) default NULL,
  PRIMARY KEY  (moduleid),
  KEY i_modules_ro (readonly),
  KEY i_modules_path (path),
  KEY i_modules_user (userid),
  KEY i_modules_name (name),
  KEY I_MODULES_USERID (userid),
  KEY I_MODULES_FAMILYID (familyid)
);

CREATE TABLE mstones (
  milestoneid int(11) NOT NULL auto_increment,
  milestone int(11) default NULL,
  name varchar(50) default NULL,
  description text,
  PRIMARY KEY  (milestoneid),
  KEY i_mstones_name (name),
  KEY i_mstones_mileston (milestone)
);

CREATE TABLE pjbugs (
  recordid int(11) NOT NULL auto_increment,
  bugid int(11) NOT NULL default '0',
  projectid int(11) NOT NULL default '0',
  done enum('0','1') default '0',
  PRIMARY KEY  (recordid),
  KEY I_PJBUGS_BUGID (bugid),
  KEY I_PJBUGS_PROJECTID (projectid)
);

CREATE TABLE pjgroups (
  recordid int(11) NOT NULL auto_increment,
  projectid int(11) default NULL,
  groupid int(11) default NULL,
  PRIMARY KEY  (recordid),
  KEY i_pjgroups_groupid (groupid),
  KEY I_PJGROUPS_PROJECTID (projectid)
);

CREATE TABLE pjmodule (
  recordid int(11) NOT NULL auto_increment,
  projectid int(11) default NULL,
  moduleid int(11) default NULL,
  hidden enum('0','1') default '0',
  PRIMARY KEY  (recordid),
  KEY i_pjmodule_module (moduleid),
  KEY i_pjmodule_project (projectid),
  KEY I_PJMODULE_PROJECTID (projectid),
  KEY I_PJMODULE_MODULEID (moduleid)
);

CREATE TABLE pjmstone (
  recordnr int(11) NOT NULL auto_increment,
  milestoneid int(11) default NULL,
  projectid int(11) default NULL,
  confirm varchar(50) default NULL,
  reached datetime default NULL,
  description text,
  PRIMARY KEY  (recordnr),
  KEY i_pjmstone_project (projectid),
  KEY i_pjmstone_milesto (milestoneid),
  KEY I_PJMSTONE_MILESTONEID (milestoneid),
  KEY I_PJMSTONE_PROJECTID (projectid)
);

CREATE TABLE pjref (
  recordid int(11) NOT NULL auto_increment,
  projectid int(11) default NULL,
  reference int(11) default NULL,
  PRIMARY KEY  (recordid),
  KEY i_pjref_project (projectid),
  KEY I_PJREF_PROJECTID (projectid),
  KEY I_PJREF_REFERENCE (reference)
);

CREATE TABLE pjusers (
  recordid int(11) NOT NULL auto_increment,
  userid int(11) default NULL,
  projectid int(11) default NULL,
  rights int(11) default NULL,
  PRIMARY KEY  (recordid),
  KEY i_pjusers_project (projectid),
  KEY i_pjusers_user (userid),
  KEY I_PJUSERS_USERID (userid),
  KEY I_PJUSERS_PROJECTID (projectid)
);

CREATE TABLE projects (
  projectid int(11) NOT NULL auto_increment,
  name varchar(50) NOT NULL default '',
  created datetime default NULL,
  createdby int(11) default NULL,
  lastuser int(11) default NULL,
  lastaccess datetime default NULL,
  history text,
  deleted enum('0','1') default '0',
  description text,
  PRIMARY KEY  (projectid),
  KEY i_projects_del (deleted),
  KEY i_projects_name (name),
  KEY I_PROJECTS_CREATEDBY (createdby),
  KEY I_PROJECTS_LASTUSER (lastuser)
);

CREATE TABLE revision (
  revisionid int(11) NOT NULL auto_increment,
  moduleid int(11) default NULL,
  userid int(11) default NULL,
  version int(11) default NULL,
  verstate int(11) default NULL,
  revision int(11) default NULL,
  ideversion int(11) default NULL,
  comment_i text,
  comment_o text,
  PRIMARY KEY  (revisionid),
  KEY i_revision_user (userid),
  KEY i_revision_revisio (revision),
  KEY i_revision_version (version),
  KEY i_revision_module (moduleid),
  KEY I_REVISION_MODULEID (moduleid),
  KEY I_REVISION_USERID (userid),
  KEY I_REVISION_REVISION (revision)
);

CREATE TABLE rvlabels (
  recordid int(11) NOT NULL auto_increment,
  revisionid int(11) default NULL,
  labelid int(11) default NULL,
  PRIMARY KEY  (recordid),
  KEY i_rvlabels_label (labelid),
  KEY i_rvlabels_revisio (revisionid),
  KEY I_RVLABELS_REVISIONID (revisionid),
  KEY I_RVLABELS_LABELID (labelid)
);

CREATE TABLE todo (
  todoid int(11) NOT NULL auto_increment,
  userid int(11) default NULL,
  projectid int(11) default NULL,
  category varchar(50) default NULL,
  responsible int(11) default NULL,
  created datetime default NULL,
  priority int(11) default NULL,
  state char(1) default NULL,
  description text,
  targetdate datetime default NULL,
  donedate datetime default NULL,
  PRIMARY KEY  (todoid),
  UNIQUE KEY pk_todo (todoid,userid,projectid,category),
  KEY i_todo_cat (category),
  KEY i_todo_project (projectid),
  KEY i_todo_user (userid),
  KEY I_TODO_USERID (userid),
  KEY I_TODO_PROJECTID (projectid),
  KEY I_TODO_RESPONSIBLE (responsible)
);

CREATE TABLE transact (
  recordid int(11) NOT NULL auto_increment,
  accessid int(11) default NULL,
  projectid int(11) default NULL,
  rights int(11) default NULL,
  tanr int(11) default NULL,
  faults int(11) default NULL,
  expires datetime default NULL,
  PRIMARY KEY  (recordid),
  KEY i_transact_project (projectid),
  KEY i_transact_tan (tanr),
  KEY i_transact_user (accessid)
);

CREATE TABLE users (
  userid int(11) NOT NULL auto_increment,
  login varchar(50) default NULL,
  pw text,
  ipaddr varchar(50) default NULL,
  rights int(11) NOT NULL default '0',
  deleted enum('0','1') default '0',
  description text,
  PRIMARY KEY  (userid),
  KEY i_users_ro (deleted),
  KEY i_users_login (login)
);

CREATE TABLE vcslog (
  logid int(11) NOT NULL auto_increment,
  projectid int(11) default NULL,
  userid int(11) default NULL,
  moduleid int(11) default NULL,
  tstamp datetime default NULL,
  type char(1) default NULL,
  description text,
  PRIMARY KEY  (logid),
  KEY i_vcslog_module (moduleid),
  KEY i_vcslog_user (userid),
  KEY i_vcslog_project (projectid)
);


