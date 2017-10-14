(*-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JVCSSrvResources.pas

The Initial Developer of the original code (JEDI VCS) is:
  Uwe Schuster (jedivcs@bitcommander.de)

Componentes and used code which is used in this code are explictly stated to
be copyright of the respective author(s).

Last Modified: see History

Known Issues:
-----------------------------------------------------------------------------

Unit history:

2004/11/16  USchuster - new unit
2004/11/20  USchuster - new strings

--- branchpoint for 2.5 dev server ---

2004/12/07  USchuster - new strings

--- 2.50 Beta 2 was released...
2009/12/23  THuber   #5063 support for  I  n f  o r m i x  port removed
                     #5064 support for  I n t e r b a s e 6  port removed
                     #5066 support for  F l a s h F i l e r  port removed
                     #5065 support for  F i b p l u s  port removed
2009/12/27  THuber   #5067 support for  D B I S A M removed
                     #5077 support for  Oracle < 9x removed, also B D E  version

-----------------------------------------------------------------------------*)
{$I jedi.inc}

unit JVCSSrvResources;

{$I compopt.inc}

interface

resourcestring
  //SrvAbout.pas
  JVCSRES_Beta_Version = 'Beta Version';
  JVCSRES_Based_on_FreeVCS_server_source_by_Thomas_Hensle =
    'Based on FreeVCS server source by Thomas Hensle';
  JVCSRES_Based_on_ICS47MidWare_Components_38_SrvTst_Demo_Application =
    'Based on ICS/MidWare Components & SrvTst Demo Application';
  JVCSRES_169_2000_45_2004_by_Fran231ois_Piette_45_http584747www46overbyte46be =
    '© 2000 - 2004 by François Piette - http://www.overbyte.be';
  JVCSRES_37s_port_by = '%s port by';
  JVCSRES_currently_not_maintained = 'currently not maintained';
  JVCSRES_currently_maintained_by = 'currently maintained by';
  JVCSRES_JEDI_VCS_Application_Server_4037s41_ = 'JEDI VCS Application Server (%s) ';
  JVCSRES_Beta = 'Beta';
  JVCSRES_Filedate58_37s = 'Filedate: %s';
  JVCSRES_This_server_is_also_available_as_NT_service_application46 =
    'This server is also available as NT service application.';
  JVCSRES_Installed_versions58 = 'Installed versions:';

  //SrvAboutBkp.pas
  JVCSRES_Archive_path_40path_needs_to_exist_on_the_Database_Server334158 =
    'Archive path (path needs to exist on the Database Server!):';
  JVCSRES_You_cannot_set_the_backup_time_to_00580046 = 'You cannot set the backup time to 00:00.';
  JVCSRES_Select_any_time_different_from_005800_40005801_to_2358594146 =
    'Select any time different from 00:00 (00:01 to 23:59).';

(*
  //SrvDBISAMUtil.pas
  JVCSRES_I_haven39t_realized_any_problems_for_now44_anyway_it_is_highly =
    'I haven''t realized any problems for now, anyway it is highly';
  JVCSRES_recommended_that_you39ve_made_a_backup_copy_of_the_archive_before =
    'recommended that you''ve made a backup copy of the archive before';
  JVCSRES_starting_37s33_If_not44_you_should_cancel_and_make_it_now33 =
    'starting %s! If not, you should cancel and make it now!';
  JVCSRES_Continue_with_37s63 = 'Continue with %s?';
  JVCSRES_DBISAM_37s = 'DBISAM %s';
  JVCSRES_Processing58_37s_4037d4737d41 = 'Processing: %s (%d/%d)';
  JVCSRES_Error58_37s = 'Error: %s';
  JVCSRES_37s_ready = '%s ready';
  JVCSRES_Log_file58_37s = 'Log file: %s';
  JVCSRES_Unable_to_save_the_log_file = 'Unable to save the log file';
*)

  //SrvLogon.pas
  JVCSRES_Logon_to_37s = 'Logon to %s';

  //SrvMain.pas
  JVCSRES_Logon_to_38database464646 = 'Logon to &database...';
  JVCSRES_Starting_37s = 'Starting %s';
  JVCSRES_37d_sec = '%d sec';
  JVCSRES_not_active = 'not active';
  JVCSRES_3333_Debug_build = '!! Debug build';
  JVCSRES_3333_Remember_that_this_is_beta_stuff46 = '!! Remember that this is beta stuff.';
  JVCSRES_3333_Not_guaranteed_to_do_anything_useful_at_all33 =
    '!! Not guaranteed to do anything useful at all!';
  JVCSRES_JEDI_VCS_37s_version_37s = 'JEDI VCS %s version %s';
  JVCSRES_JEDI_VCS_Encryption_Algorithm_version_37d46372462d =
    'JEDI VCS Encryption Algorithm version %d.%2.2d';
  JVCSRES_ServerCrypt46dll_4037d46372462d41_is_not_from_the_expected_version_4037d46372462d4146 =
    'ServerCrypt.dll (%d.%2.2d) is not from the expected version (%d.%2.2d).';
  JVCSRES_Clients_may_not_be_able_to_connect_the_server33 =
    'Clients may not be able to connect the server!';
  JVCSRES_3333_Fatal_Error58_Invalid_ServerCrypt46dll_version46 =
    '!! Fatal Error: Invalid ServerCrypt.dll version.';
  JVCSRES_JEDI_VCS_Client_version_required_6261_37d46372462d =
    'JEDI VCS Client version required >= %d.%2.2d';
  JVCSRES_MidWare_TAppServer_version_37d46372462d = 'MidWare TAppServer version %d.%2.2d';
  JVCSRES_MidWare_TRequestBroker_version_37d46372462d = 'MidWare TRequestBroker version %d.%2.2d';
  JVCSRES_MidWare_TServerObject_version_37d46372462d = 'MidWare TServerObject version %d.%2.2d';
  JVCSRES_MidWare_TMWBuffer_version_37d46372462d = 'MidWare TMWBuffer version %d.%2.2d';
  JVCSRES_ICS_TWSocket_version_37d46372462d = 'ICS TWSocket version %d.%2.2d';
  JVCSRES_Windows_TCP47IP_40Winsock41_version_37d4637d = 'Windows TCP/IP (Winsock) version %d.%d';
  JVCSRES_Settings58 = 'Settings:';
  JVCSRES_45__Application_server_port58_37s = '-  Application server port: %s';
  JVCSRES_45__Application_server_location58_37s = '-  Application server location: %s';
  JVCSRES_Direct_SQL_object58_disabled = 'Direct SQL object: disabled';
  JVCSRES_Direct_SQL_object58_enabled = 'Direct SQL object: enabled';
  JVCSRES_45__Client_time_out58_37d_sec = '-  Client time out: %d sec';
  JVCSRES_Server_banner58_disabled = 'Server banner: disabled';
  JVCSRES_Server_banner58_34Welcome_to_37s34 = 'Server banner: "Welcome to %s"';
  JVCSRES_Client_timestamp_required58_434745_2h_Servertime =
    'Client timestamp required: +/- 2h Servertime';
  JVCSRES_Client_timestamp_required58_not_active = 'Client timestamp required: not active';
  JVCSRES_active = 'active';
  JVCSRES_not_supported = 'not supported';
  JVCSRES_Version_archive_not_found46 = 'Version archive not found.';
  JVCSRES_Unable_to_start_the_server_component46 = 'Unable to start the server component.';
  JVCSRES_3333_DBMS_Login_failed = '!! DBMS Login failed';
  JVCSRES_DBMS_Login464646 = 'DBMS Login...';
  JVCSRES_DBISAM_engine_version_37s = 'DBISAM engine version %s';
  JVCSRES_Firebird_engine_40UIB41 = 'Firebird engine (UIB)';  
  JVCSRES_Oracle_engine_version_846x = 'Oracle engine version 8.x';
  //JVCSRES_Oracle_engine_version_746x47846x = 'Oracle engine version 7.x/8.x';
  JVCSRES_Oracle_client_version_37d = 'Oracle client version %d';
  JVCSRES_MSSQL_engine_version = 'MSSQL engine version';
  JVCSRES_MySQL_engine_version_37s = 'MySQL engine version %s';
  JVCSRES_ADO_engine_version = 'ADO engine version';
  JVCSRES_Exception58_37s = 'Exception: %s';
  JVCSRES_Unable_to_connect_DBMS46 = 'Unable to connect DBMS.';
  JVCSRES_37s_45_DBMS_connected = '%s - DBMS connected';
  JVCSRES_45__Version_archive_server58_37s = '-  Version archive server: %s';
  (*
  JVCSRES_45__Version_archive_path58_37s = '-  Version archive path: %s';
  *)
  JVCSRES_37s_37s_Beta_45_Running = '%s %s Beta - Running';
  JVCSRES_37s_37s_45_Running = '%s %s - Running';
  JVCSRES_37s_45_Server_started46 = '%s - Server started.';
  JVCSRES_37d_clients_already_connected33 = '%d clients already connected!';
  JVCSRES_The_requested_action_will_disconnect_all_clients46 =
    'The requested action will disconnect all clients.';
  JVCSRES_Terminate_server63 = 'Terminate server?';
  JVCSRES_Invalidate_all_login_acounts63 = 'Invalidate all login acounts?';
  JVCSRES_DBMS_error58 = 'DBMS error:';
  JVCSRES_37s_45_Server_closed46 = '%s - Server closed.';
  JVCSRES_Uptime58_37s = 'Uptime: %s';
  JVCSRES_Requests58_37s = 'Requests: %s';
  JVCSRES_Received_bytes58_37n_MB = 'Received bytes: %n MB';
  JVCSRES_Transmitted_bytes58_37n_MB = 'Transmitted bytes: %n MB';
  JVCSRES_Users58_37d = 'Users: %d';
  JVCSRES_New_files58_37d = 'New files: %d';
  JVCSRES_Get_modules58_37d = 'Get modules: %d';
  JVCSRES_Checked_In_modules58_37d = 'Checked In modules: %d';
  JVCSRES_Checked_Out_modules58_37d = 'Checked Out modules: %d';
  JVCSRES_37s_45_Configuration_file_37s_stored46 = '%s - Configuration file %s stored.';
  JVCSRES_Port_37s_is_already_used_by_another_process46 = 'Port %s is already used by another process.';
  JVCSRES_Winsock_error_3437s34_occurred_starting_server46 =
    'Winsock error "%s" occurred starting server.';
  JVCSRES_Error_37s_occurred_starting_server46 = 'Error %s occurred starting server.';
  JVCSRES_37s_45_AppServer_Component58_started = '%s - AppServer Component: started';
  JVCSRES_This_server_expects_archive_version_37d46372462d46 =
    'This server expects archive version %d.%2.2d.';
  JVCSRES_You_have_connected_to_an_archive_of_version_37d46372462d =
    'You have connected to an archive of version %d.%2.2d';
  JVCSRES_You_may_now_upgrade_your_archive_to_the_most_recent_archive_or_close_JVCS_Appserver_and_upgrade_later46 =
    'You may now upgrade your archive to the most recent archive or close JVCS Appserver and upgrade later.';
  JVCSRES_Attention58_You_need_to_be_logged_on_to_the_database_as_database_administrator46 =
    'Attention: You need to be logged on to the database as database administrator.';
  JVCSRES_If_you_are_logged_on_without_permission_to_change_database_structure44_upgrade_will_fail33 =
    'If you are logged on without permission to change database structure, upgrade will fail!';
  JVCSRES_Continue_with_archive_upgrade_now63 = 'Continue with archive upgrade now?';
  JVCSRES_Database_archive_successfully_upgraded46 = 'Database archive successfully upgraded.';
  JVCSRES_It_is_recommended_to_make_now_a_database_backup46 =
    'It is recommended to make now a database backup.';
  JVCSRES_There_was_an_error_on_upgrading_your_archive46 =
    'There was an error on upgrading your archive.';
  JVCSRES_Please_read_the_release_notes_about_database_upgrading_procedure =
    'Please read the release notes about database upgrading procedure';
  JVCSRES_It_is_strongly_recommended_not_to_continue_without_having_archive_upgrading_successfully_completed33 =
    'It is strongly recommended not to continue without having archive upgrading successfully completed!';
  JVCSRES_Error_on_upgrading_archive_3333 = 'Error on upgrading archive !!';
  JVCSRES_45__Archive_structure_version58_37d46372462d = '-  Archive structure version: %d.%2.2d';
  JVCSRES_37s_45_3333_Errors_reported58 = '%s - !! Errors reported:';
  JVCSRES_37d46_NativeError58 = '%d. NativeError:';
  JVCSRES_Re45Connecting_with_DB464646 = 'Re-Connecting with DB...';
  JVCSRES_Connected46 = 'Connected.';
  JVCSRES_Version_archive_not_connected46 = 'Version archive not connected.';
  JVCSRES_37s_45_AppServer_Component58_stopped = '%s - AppServer Component: stopped';
  JVCSRES_37s_37s_Beta = '%s %s Beta';
  JVCSRES_37s_45_Server_stopped46 = '%s - Server stopped.';
  JVCSRES_Client_list_45_All_clients58_4037s41 = 'Client list - All clients: (%s)';
  JVCSRES_No_clients_defined46 = 'No clients defined.';
  JVCSRES_37s_45_37s_45_Level58_37d_45_IP58_37s_45_37s = '%s - %s - Level: %d - IP: %s - %s';
  JVCSRES_Client_list_45_logged_in58_4037s41 = 'Client list - logged in: (%s)';
  JVCSRES_No_clients_logged_in46 = 'No clients logged in.';
  JVCSRES_Are_you_sure_to_log_out_all_clients63 = 'Are you sure to log out all clients?';
  JVCSRES_37s_45_Display_line_count_62_100044_cleared_by_server =
    '%s - Display line count > 1000, cleared by server';
  JVCSRES_37s_45_3333_Exception_3437s34_in_ServerForm46AddMessage46_Display_cleared =
    '%s - !! Exception "%s" in ServerForm.AddMessage. Display cleared';
  JVCSRES_Welcome_to_37s = 'Welcome to %s';
  JVCSRES_Are_you_sure_you_want_to_enable_the_direct_SQL_server_object63 =
    'Are you sure you want to enable the direct SQL server object?';
  JVCSRES_This_is_a_high45risk_function_and_therefore_usually_not_recommend33 =
    'This is a high-risk function and therefore usually not recommend!';
  JVCSRES_Error_while_writing_to_clipboard33 = 'Error while writing to clipboard!';
  JVCSRES_Text_files_404246txt411244246txt124All_files_4042464241124424642 =
    'Text files (*.txt)|*.txt|All files (*.*)|*.*';
  JVCSRES_Error_while_saving_the_file33 = 'Error while saving the file!';
  JVCSRES_Continue63 = 'Continue?';
  JVCSRES_Enter_TCP_port47service_name58 = 'Enter TCP port/service name:';
  JVCSRES_37s_45_Server_port_changed_to_37s = '%s - Server port changed to %s';
  JVCSRES_Enter_client_timeout58_40sec41 = 'Enter client timeout: (sec)';
  JVCSRES_Integer_expected33 = 'Integer expected!';
  JVCSRES_37s_45_Client_time_out_changed_to_37d_sec = '%s - Client time out changed to %d sec';
  (*
  JVCSRES_Select_database_folder = 'Select database folder';
  *)
  JVCSRES_No_archive_files_found_in_37s = 'No archive files found in %s';
  JVCSRES_37s_45_Version_archive_path_changed_to_37s = '%s - Version archive path changed to %s';
  JVCSRES_37s_45_DBMS_connected46 = '%s - DBMS connected.';
  JVCSRES_37s_45_Auto_backup_start46 = '%s - Auto backup start.';
  JVCSRES_37s_45_Auto_backup_ready46 = '%s - Auto backup ready.';
  JVCSRES_37s_45_3333_Backup_folder_not_found46 = '%s - !! Backup folder not found.';
  JVCSRES_37s_45_Login_idle_timer_overflow46_37s_logged_out_by_server46_ =
    '%s - Login idle timer overflow. %s logged out by server. ';
  JVCSRES_37s_45_3333_Log_file_access_error58_37s = '%s - !! Log file access error: %s';
  JVCSRES_37s_45_Log_file_37s_stored46 = '%s - Log file %s stored.';
  JVCSRES_active44_37d_KB = 'active, %d KB';
  JVCSRES_active44_63_KB = 'active, ? KB';
  JVCSRES_inactive = 'inactive';
  JVCSRES_File_37s_not_found46 = 'File %s not found.';
  JVCSRES_Are_you_sure_to_clear_the_logfile63 = 'Are you sure to clear the logfile?';
  JVCSRES_37s_45_Log_file_37s_deleted46 = '%s - Log file %s deleted.';
  JVCSRES_Registry_write_access_error58_ = 'Registry write access error: ';
  JVCSRES_Select_backup_target_folder = 'Select backup target folder';
  JVCSRES_You_cannot_use_the_archive_folder_for_that33 = 'You cannot use the archive folder for that!';
  JVCSRES_37s_45_Backup_target_folder_changed_to_37s = '%s - Backup target folder changed to %s';
  JVCSRES_Warning33_Backup_target_folder_is_undefined33 = 'Warning! Backup target folder is undefined!';
  JVCSRES_Start_version_archive_backup_now63 = 'Start version archive backup now?';
  JVCSRES_Not_supported_by_this_type_of_server46 = 'Not supported by this type of server.';
  JVCSRES__45_Table_found58_37s_45_37d_KB = ' - Table found: %s - %d KB';
  JVCSRES__45_Processing_table58_37s = ' - Processing table: %s';
  JVCSRES__45_Success58_37s = ' - Success: %s';
  JVCSRES_37s_45_Database_backup_started = '%s - Database backup started';
  JVCSRES_37s_45_Database_backup_completed = '%s - Database backup completed';
  JVCSRES_37s_45_Database_backup_started_to_37s = '%s - Database backup started to %s';
  JVCSRES_37s_45_3333_Exception_in_Backup58 = '%s - !! Exception in Backup:';
  JVCSRES_37s_45_3333_Backup_failed46 = '%s - !! Backup failed.';
  JVCSRES_37s_45_Backup_ready46_Success46_37d_KB = '%s - Backup ready. Success. %d KB';
  JVCSRES_This_server_uses_the_new_DBISAM_engine_346x46 = 'This server uses the new DBISAM engine 3.x.';
  JVCSRES_Therefore_your_tables_must_be_upgraded_to_the_new_format46 =
    'Therefore your tables must be upgraded to the new format.';
  JVCSRES_Continue_with_Upgrade63 = 'Continue with Upgrade?';
  JVCSRES_DBISAM_Upgrade = 'DBISAM Upgrade';
  JVCSRES_3333Wrong_DBISAM_table_structure_version3333 = '!!Wrong DBISAM table structure version!!';
  JVCSRES_DBISAM_version_table_structure_still_6062_v3460033 =
    'DBISAM version table structure still <> v3.00!';
  JVCSRES_Connecting_from_JVCS_clients_will_cause_errors33 =
    'Connecting from JVCS clients will cause errors!';
  JVCSRES_Better_upgrade_or_select_another_DBISAM_database_folder46 =
    'Better upgrade or select another DBISAM database folder.';
  JVCSRES_All_tables_up45to45date33 = 'All tables up-to-date!';
  JVCSRES_Nothing_to_do46 = 'Nothing to do.';
  JVCSRES_Invalid_DBISAM_table_format33 = 'Invalid DBISAM table format!';
  JVCSRES_All_clients_must_be_disconnected_and_logged_out_first46 =
    'All clients must be disconnected and logged out first.';
  JVCSRES_Upgrading_your_database_archive_is_normally_done_automatically_on_Appserver_startup46 =
    'Upgrading your database archive is normally done automatically on Appserver startup.';
  JVCSRES_If_this_failed_for_any_reason_you_might_upgrade_now_manually46 =
    'If this failed for any reason you might upgrade now manually.';
  JVCSRES_Please_make_sure_that_there_are_no_JVCS_clients_connected_during_upgrade33 =
    'Please make sure that there are no JVCS clients connected during upgrade!';
  JVCSRES_Server_banner = 'Server banner';
  JVCSRES_Server_banner58_3437s34 = 'Server banner: "%s"';

implementation

end.