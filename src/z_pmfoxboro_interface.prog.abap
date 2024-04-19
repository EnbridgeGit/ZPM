*&---------------------------------------------------------------------*
* Report  Z_PMFOXBORO_INTERFACE
* Author             : Shiladitya Ghosh                                *
* Date               : 27/06/2014 (dd/mm/yyyy)                         *
* Technical Contact  : Shiladitya Ghosh                                *
* Business Contact   : Rai Y.Ijaz                                      *
* Purpose            : This is an inbound interface to create          *
*                      Measurement documents. Files will be generated  *
*                      by Foxboro system.                              *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By           CTS          Description           *
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_pmfoxboro_interface.
TABLES: zpm_foxboro_file , zpm_foxboro_inte.

*----------------------------------------------------------------------*
* Declaration
*----------------------------------------------------------------------*

TYPES : BEGIN OF ty_status.      " structure for alv display
        INCLUDE TYPE zpm_foxboro_file AS file.
TYPES:  err_dsc TYPE bapi_msg,
        END OF ty_status,

        BEGIN OF ty_trtab,                 "structure for pulling data from translation table
          source   TYPE zpm_foxboro_inte-source,
          location TYPE zpm_foxboro_inte-location,
          sequence TYPE zpm_foxboro_inte-sequence,
          point    TYPE zpm_foxboro_inte-point,
        END OF ty_trtab.

DATA:   ta_file     TYPE STANDARD TABLE OF zpm_foxboro_file ##needed, "internal table to load data to zpm_foxboro_file
        ta_trtab    TYPE STANDARD TABLE OF ty_trtab ##needed,        "internal table for translation table
        ta_status   TYPE STANDARD TABLE OF ty_status ##needed, "internal table to be passed to ALV
        ta_file_tbl TYPE TABLE OF salfldir ##needed, "table to hold the files
        ta_existing TYPE STANDARD TABLE OF zpm_foxboro_file,

        wa_status TYPE ty_status ##needed,
        wa_file   TYPE zpm_foxboro_file ##needed,
        wa_trtab  TYPE ty_trtab ##needed,

        gv_file_name TYPE string ##needed,
        gv_file_path TYPE dxfields-longpath,"salfile-longname ##needed,

        rf_alv           TYPE REF TO cl_salv_table ##needed, "varaibles for ALV display
        rf_layout        TYPE REF TO cl_salv_layout,
        rf_alv_functions TYPE REF TO cl_salv_functions_list ##needed,
        rf_columns       TYPE REF TO cl_salv_columns_table ##needed,
        rf_column        TYPE REF TO cl_salv_column_table ##needed.

CONSTANTS : c_x       TYPE c VALUE 'X',
            c_p       TYPE c VALUE 'P',
            c_np      TYPE c LENGTH 2 VALUE 'NP',
            c_e       TYPE c VALUE 'E',
            c_c       TYPE c VALUE 'C'.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
**--**Input parameters
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS : p_rba    TYPE char1 DEFAULT 'X' RADIOBUTTON GROUP rb1 USER-COMMAND uc1,             "Radiobuton - Application Server
             p_rbp    TYPE char1 RADIOBUTTON GROUP rb1, "Radiobutton - Presentation server
             p_fpath  TYPE salfile-longname MODIF ID app. "file path
SELECTION-SCREEN SKIP.

PARAMETERS : p_rbq  TYPE char1 RADIOBUTTON GROUP rb1, " Radiobutton - Reprocessing
             p_stat TYPE zpm_foxboro_file-status MODIF ID sta. "status
SELECT-OPTIONS : s_flname FOR zpm_foxboro_file-filename MODIF ID sta, "filename
                 s_unit   FOR zpm_foxboro_inte-location MODIF ID sta ,  "unit
                 s_source FOR zpm_foxboro_file-source MODIF ID sta,  "source plant
                 s_point  FOR zpm_foxboro_inte-point MODIF ID sta, "Measurement point
                 s_pos    FOR zpm_foxboro_inte-sequence MODIF ID sta. "position of the file
PARAMETERS : cb_01  AS CHECKBOX DEFAULT 'x'.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
**--- Presentation server file path.
  PERFORM default_fpath.

*----------------------------------------------------------------------*
* At Selection Screen Value -Request
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fpath.
**--**Application Server/presentation server - Get file name and path for the input file
  PERFORM f4_app_path.

*----------------------------------------------------------------------*
* At Selection Screen Output
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
**--**Show/Hide Selection screen field
  PERFORM set_fields.

*----------------------------------------------------------------------*
* At Selection Screen
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  PERFORM screen_validation.

  PERFORM status_validation. " Add validation for user to enter status . Only E or R
  IF p_fpath IS NOT INITIAL AND sy-ucomm NE 'UC1'.
    PERFORM validate_file_path.
  ENDIF.
  IF p_fpath IS NOT INITIAL AND sy-ucomm <> 'UC1'.
    PERFORM validate_file_patha.
  ENDIF.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM read_lookup_table.
  IF p_rbp IS NOT INITIAL.
    PERFORM read_file_prn.                       "reading presentation server files
  ENDIF.

  IF  p_rba IS NOT INITIAL.
    PERFORM read_file_names.                     "read all the file names from application server
    PERFORM convert_files.                       "read the files and create entries in ta_files,application server
  ENDIF.
  IF p_rbq IS NOT INITIAL.
    PERFORM select_reprocess.                    "Add logic to select data from zpm_foxboro_file wheres status eq p_Stat
  ENDIF.

  PERFORM create_stat.                           "create mdoc feasibility status in zpm_foxboro_inte
  PERFORM delete_entries.                        "delete entries from zpm_foxboro_file < 14
  IF cb_01 IS INITIAL.
    PERFORM send_email.                            "sending email for unsuccessfull posting
  ENDIF.

*----------------------------------------------------------------------*
* End of Selection
*----------------------------------------------------------------------*
END-OF-SELECTION.

  PERFORM display_results.                       "display result in ALV

*&---------------------------------------------------------------------*
*&      Form  SET_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_fields .                           "dynamic selection screen

  IF p_rbq IS INITIAL.
    CLEAR: p_stat.
    REFRESH: s_flname[], s_pos[], s_point[], s_unit[], s_source[].
  ENDIF.

  LOOP AT SCREEN.
    IF p_rba = c_x.
      IF screen-group1 = 'STA'.
        screen-input = 0.
        screen-invisible = 1.
      ELSE.
        screen-input = 1.
        screen-invisible = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSEIF p_rbp = c_x.
      IF screen-group1 = 'APP'.
        screen-input = 1.
      ELSE.
        screen-input = 1.
      ENDIF.
      IF  screen-group1 = 'STA'.
        screen-input = 0.
        screen-invisible = 1.
      ELSE.
        screen-input = 1.
        screen-invisible = 0.
      ENDIF.
      MODIFY SCREEN.
    ELSE.
      IF screen-group1 = 'APP'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ELSE.
        screen-input = 1.
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " SET_FIELDS


*&---------------------------------------------------------------------*
*&      Form  CONVERT_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_files . "converting the files in to a table structure

  DATA: lv_file TYPE string,
        lv_file_name TYPE string,
        lv_file_name1 TYPE string,
        lv_left TYPE string,
        lv_count TYPE i,
        lv_right TYPE string,
        lw_files TYPE salfldir,
        lv_yy    TYPE string,
        lv_dd    TYPE string,
        lv_mm    TYPE string,
        lv_hr    TYPE string,
        lv_min1  TYPE string,
        lv_min2  TYPE string,
        lv_min   TYPE string,
        lv_sec1  TYPE string,
        lv_sec2  TYPE string,
        lv_sec   TYPE string,
        lv_min_n TYPE n LENGTH 2,
        lv_sec_n TYPE n LENGTH 2,
        lv_hr_n  TYPE n LENGTH 2,
        lv_yy_n  TYPE n LENGTH 4,
        lv_mm_n  TYPE n LENGTH 2,
        lv_dd_n  TYPE n LENGTH 2.

*ta_file_tbl TYPE TABLE OF SALFLDIR.
* storing data from application server file, creating backups and deleting
* original diles
  LOOP AT ta_file_tbl INTO lw_files.
    READ TABLE ta_trtab WITH KEY source  = lw_files-name+0(4)
                        TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    CLEAR lv_file_name.
    CONCATENATE gv_file_path lw_files-name INTO lv_file_name.
    OPEN DATASET lv_file_name FOR INPUT IN TEXT MODE ENCODING DEFAULT.
    IF sy-subrc NE 0.
      CONTINUE.
    ENDIF.

    IF cb_01 IS INITIAL.
      CLEAR lv_file_name1.
      CONCATENATE gv_file_path '/BACKUP/' lw_files-name INTO lv_file_name1.
      OPEN DATASET lv_file_name1 FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.   "creating the back up files
      IF sy-subrc NE 0.
        CLEAR lv_file_name1.
      ENDIF.
    ENDIF.

    DO.
      READ DATASET lv_file_name INTO lv_file.
      IF sy-subrc NE 0.
        EXIT.
      ENDIF.

      IF lv_file_name1 IS NOT INITIAL.
        TRANSFER lv_file TO lv_file_name1.   "Transferring the files to back up detination
      ENDIF.

      DO.

        SPLIT lv_file AT space INTO lv_left lv_right.
        IF lv_right IS INITIAL AND lv_left IS INITIAL.
          EXIT.
        ENDIF.

        lv_file = lv_right.
        IF lv_left IS INITIAL.
          CONTINUE.
        ELSE.

          IF strlen( lv_left ) EQ 34.
            CLEAR: wa_file, lv_count.
          ENDIF.

          lv_count = lv_count + 1.
          IF lv_count EQ 1.
            wa_file-hdata = lv_left.
            wa_file-unit = lv_left+0(3).
            wa_file-filename = lw_files-name.
            wa_file-source = lw_files-name+0(4).
            lv_yy = lv_left+4(4).
            lv_mm = lv_left+8(2).
            lv_dd = lv_left+10(2).
            lv_hr = lv_left+12(2).
            lv_min1 = lv_left+14(7) * 60.
            SPLIT lv_min1 AT '.' INTO lv_min lv_min2.
            CONCATENATE '.' lv_min2 INTO lv_min2.
            lv_sec1 = lv_min2 * 60.
            SPLIT lv_sec1 AT '.' INTO lv_sec lv_sec2.
            lv_hr_n  = lv_hr.
            lv_min_n = lv_min.
            lv_sec_n = lv_sec.
            lv_yy_n  = lv_yy.
            lv_mm_n  = lv_mm.
            lv_dd_n  = lv_dd.
            CONCATENATE lv_hr_n lv_min_n lv_sec_n INTO wa_file-c_time.
            CONCATENATE lv_yy_n lv_mm_n lv_dd_n INTO wa_file-c_date.
          ELSE.
            wa_file-pos = lv_count - 1.
            wa_file-value = lv_left.

            READ TABLE ta_existing WITH KEY filename = wa_file-filename
                                            hdata    = wa_file-hdata
                                            pos      = wa_file-pos
                                            BINARY SEARCH
                                            TRANSPORTING NO FIELDS.
            IF sy-subrc NE 0.
              APPEND wa_file TO ta_file.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDDO.
    ENDDO.

    CLOSE DATASET lv_file_name.
    IF lv_file_name1 IS NOT INITIAL.
      CLOSE DATASET lv_file_name1.
      CLEAR: lv_file_name1.
      DELETE DATASET lv_file_name.
    ENDIF.
    CLEAR: lv_file_name.

  ENDLOOP.
ENDFORM.                    " CONVERT_FILES


*&---------------------------------------------------------------------*
*&      Form  CREATE_STAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_stat .

  DATA: lv_value TYPE rimr0-recdc,
        lv_point TYPE imrg-point,
        lv_mdoc TYPE imrg-mdocm,
        lv_date TYPE erdat,
        lv_time TYPE uzeit,
        lv_source TYPE c LENGTH 4.

  FIELD-SYMBOLS: <lfs_file> TYPE zpm_foxboro_file.

*Creation of measurement documents using position values and measurement points
  LOOP AT ta_file ASSIGNING <lfs_file>.
    CLEAR lv_source.
    lv_source = <lfs_file>-filename+0(4).
    TRANSLATE lv_source TO UPPER CASE.
    READ TABLE ta_trtab INTO wa_trtab WITH KEY source = lv_source
                                              location = <lfs_file>-hdata+0(3)
                                              sequence = <lfs_file>-pos.

    IF sy-subrc EQ 0.
      <lfs_file>-mdoc = c_p.                         "if yes, mdoc is possible
      lv_value = <lfs_file>-value.
      lv_point = wa_trtab-point.
      lv_date = <lfs_file>-c_date.
      lv_time = <lfs_file>-c_time.
      IF cb_01 IS NOT INITIAL.                     "if display only, no database update
        <lfs_file>-status = 'R'.
        wa_status-file = <lfs_file>.
        wa_status-err_dsc = text-022.
        APPEND wa_status TO ta_status.
      ELSE.

        CONDENSE lv_value.

        CALL FUNCTION 'MEASUREM_DOCUM_RFC_SINGLE_001'
          EXPORTING
            measurement_point    = lv_point
            recorded_value       = lv_value
            reading_date         = lv_date
            reading_time         = lv_time
          IMPORTING
            measurement_document = lv_mdoc
          EXCEPTIONS
            no_authority         = 1
            point_not_found      = 2
            index_not_unique     = 3
            type_not_found       = 4
            point_locked         = 5
            point_inactive       = 6
            timestamp_in_future  = 7
            timestamp_duprec     = 8
            unit_unfit           = 9
            value_not_fltp       = 10
            value_overflow       = 11
            value_unfit          = 12
            value_missing        = 13
            code_not_found       = 14
            notif_type_not_found = 15
            notif_prio_not_found = 16
            notif_gener_problem  = 17
            update_failed        = 18
            invalid_time         = 19
            invalid_date         = 20
            OTHERS               = 21.

        IF sy-subrc <> 0.
          <lfs_file>-status = c_e.                                  "Error while processing
          wa_status-file = <lfs_file>.                    "In case mdoc = possible but still could not create, exception raised
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO wa_status-err_dsc.
          APPEND wa_status TO ta_status.
          CLEAR wa_status.
        ELSE.
          <lfs_file>-status = 'S'.
*        <lfs_file>-c_date = lv_date.                                   "Succesfully posted
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = lv_mdoc
            IMPORTING
              output = lv_mdoc.

          CONCATENATE 'Measurement document'(019) lv_mdoc 'created'(020) INTO wa_status-err_dsc SEPARATED BY space.
          wa_status-file = <lfs_file>.
          APPEND wa_status TO ta_status.
        ENDIF.
        CLEAR: lv_point,lv_value,lv_date.
      ENDIF.
    ELSE.
      <lfs_file>-mdoc = c_np.                        " else, mdoc is not possible
      <lfs_file>-status = c_e.                       "mdoc status = ERROR
*      <lfs_file>-c_date = lv_date.
      wa_status-file = <lfs_file>.
      wa_status-err_dsc = text-003.
      APPEND wa_status TO ta_status.
      CLEAR wa_status.
    ENDIF.
  ENDLOOP.

*
  IF cb_01 IS INITIAL.
    MODIFY zpm_foxboro_file FROM TABLE ta_file.
    IF sy-subrc NE 0.
      MESSAGE 'Table ZPM_FOXBORO_FILE update failed'(001) TYPE c_e.
    ENDIF.
    CLEAR : wa_file,wa_trtab,wa_status.
  ENDIF.


ENDFORM.                    " CREATE_STAT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_RESULTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_results .

  DATA: lt_salv_not_found   TYPE REF TO   cx_salv_not_found,
        lv_msg              TYPE          string.

  DATA ls_layout_key TYPE salv_s_layout_key.

  SORT ta_status BY filename hdata pos c_date c_time.            "Result display in ALV
*  break sghosh.
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = rf_alv
        CHANGING
          t_table      = ta_status.
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  rf_columns = rf_alv->get_columns( ).

  TRY.
      rf_column ?= rf_columns->get_column( 'MANDT' ).  "hiding the clint field
    CATCH cx_salv_not_found INTO lt_salv_not_found.
      lv_msg = lt_salv_not_found->get_text( ).
      MESSAGE lv_msg TYPE c_e.
  ENDTRY.

  rf_column->set_visible( abap_false ).
  rf_columns->set_optimize( 'X' ).

** Functions
  rf_alv_functions = rf_alv->get_functions( ).
  rf_alv_functions->set_default( 'X' ).
  rf_alv_functions->set_all( 'X' ).
** Display the table

  rf_layout = rf_alv->get_layout( ).
  ls_layout_key-report = sy-repid.
  rf_layout->set_key( ls_layout_key ).
  rf_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  rf_layout->set_default( if_salv_c_bool_sap=>true ).

  rf_alv->display( ).

ENDFORM                    "DISPLAY_RESULTS
.                    " DISPLAY_RESULTS
*&---------------------------------------------------------------------*
*&      Form  READ_FILE_NAMES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_file_names .

  DATA : lwa_file_tab TYPE salfldir,
         lv_file_path TYPE salfile-longname.

  lv_file_path = gv_file_path.

  IF gv_file_name IS INITIAL.

    CALL FUNCTION 'RZL_READ_DIR_LOCAL'     "getting all the files from directory
      EXPORTING
        name               = lv_file_path
      TABLES
        file_tbl           = ta_file_tbl
      EXCEPTIONS
        argument_error     = 1
        not_found          = 2
        no_admin_authority = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      MESSAGE 'Invalid Directory'(015) TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
*
  ELSE.
    lwa_file_tab-name = gv_file_name.
    APPEND lwa_file_tab TO ta_file_tbl.
    CLEAR lwa_file_tab.
  ENDIF.

  IF ta_file_tbl IS NOT INITIAL.
    SELECT * FROM zpm_foxboro_file INTO TABLE ta_existing
                                   FOR ALL ENTRIES IN ta_file_tbl
                                   WHERE filename = ta_file_tbl-name AND
                                         status = 'S'.

    IF sy-subrc EQ 0.
      SORT ta_existing BY filename hdata pos.
    ENDIF.
  ENDIF.


ENDFORM.                    " READ_FILE_NAMES
*&---------------------------------------------------------------------*
*&      Form  READ_LOOKUP_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_lookup_table .

  SELECT source location sequence point FROM zpm_foxboro_inte
           INTO TABLE ta_trtab
             WHERE location IN s_unit
              AND  sequence IN s_pos
              AND  point IN s_point.
*                                        WHERE SOURCE = gv_so<urce.
  IF sy-subrc EQ 0.
    SORT ta_trtab BY source location sequence.
  ELSE.
    MESSAGE 'No matching records found in table ZPM_FOXBORO_INTE'(014) TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.
ENDFORM.                    " READ_LOOKUP_TABLE
*&---------------------------------------------------------------------*
*&      Form  STATUS_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM status_validation .

  IF p_rba IS INITIAL AND p_rbp IS INITIAL AND sy-ucomm NE 'UC1'.

    IF p_stat IS INITIAL.                                          "Processing status is mandatory
      MESSAGE 'Please Enter Processing Status'(006) TYPE c_e.
    ELSEIF p_stat NE c_e AND p_stat NE 'R'.                        "should be E or R
      MESSAGE 'Please Select E or R from the list'(004) TYPE c_e.
    ENDIF.

  ENDIF.

ENDFORM.                    " STATUS_VALIDATION
*&---------------------------------------------------------------------*
*&      Form  SELECT_REPROCESS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_reprocess .

  SELECT * FROM zpm_foxboro_file INTO TABLE ta_file
                             FOR ALL ENTRIES IN ta_trtab
                             WHERE status = p_stat
                               AND filename IN s_flname
                               AND unit = ta_trtab-location
                               AND source = ta_trtab-source
                               AND pos = ta_trtab-sequence.  "only for reprocessing with filters from selections screen

  IF sy-subrc <> 0.
    MESSAGE 'No records found for reprocessing'(005) TYPE c_e.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " SELECT_REPROCESS
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_FPATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM default_fpath .

  DATA: lv_file_path TYPE string.

  CALL FUNCTION 'FILE_GET_NAME'  "Getting physical path from logical path
    EXPORTING
      logical_filename = 'Z_INTERFACES_FOXBORO'
    IMPORTING
      file_name        = lv_file_path
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE text-000 TYPE 'E'.
  ENDIF.

  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH' "splitting the filename and directory
    EXPORTING
      full_name     = lv_file_path
    IMPORTING
      file_path     = gv_file_path
    EXCEPTIONS
      x_error       = 0
      OTHERS        = 0.

  p_fpath = gv_file_path.

ENDFORM.                    " DEFAULT_FPATH
*&---------------------------------------------------------------------*
*&      Form  F4_APP_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_app_path .

*******File from Presentation Server / Application server ************
  DATA : lv_filetab TYPE filetable,
         lv_ret     TYPE i.

  IF p_rbp IS NOT INITIAL.   " presentation server look up
    CLEAR: p_fpath.
    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        file_filter             = cl_gui_frontend_services=>filetype_text  "Filer for Text files
      CHANGING
        file_table              = lv_filetab
        rc                      = lv_ret
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.
    IF sy-subrc = 0.
      READ TABLE lv_filetab INDEX 1 INTO p_fpath.
    ENDIF.


  ENDIF.

  IF p_rba IS NOT INITIAL.    "Application server look up
    DATA: lv_path TYPE dxfields-longpath.

    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = 'A'
        i_server        = ''
        i_path          = gv_file_path "'/usr/sap/interfaces/S01/FOXBORO/'
*       FILEMASK        = '*.*'
        fileoperation   = 'R'
      IMPORTING
*       O_LOCATION_FLAG =
*       O_SERVER        =
        o_path          = lv_path
*       ABEND_FLAG      =
      EXCEPTIONS
        rfc_error       = 0
        error_with_gui  = 0
        OTHERS          = 0.

    p_fpath = lv_path.
  ENDIF.
ENDFORM.                    " F4_APP_PATH
*&---------------------------------------------------------------------*
*&      Form  SCREEN_VALIDATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM screen_validation .

  IF p_rbq  IS INITIAL  AND
      sy-ucomm EQ 'UC1'.
    CLEAR p_fpath.
  ENDIF.

  IF p_rbq IS INITIAL.
    IF p_fpath IS INITIAL AND sy-ucomm NE 'UC1'. "ENTER VALID PATH
      MESSAGE 'Enter the file path'(018) TYPE c_e.
    ENDIF.
  ENDIF.

ENDFORM.                    " SCREEN_VALIDATION

*&---------------------------------------------------------------------*
*&      Form  READ_FILE_APP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_file_prn .

  DATA: lt_itab TYPE TABLE OF string,
        lw_itab TYPE string,
        lv_file TYPE string,
        lt_path TYPE string,
        lv_filename TYPE string,
        lv_count TYPE i,
        lv_count1 TYPE i,
        lv_left TYPE string,
        lv_right TYPE string,
        lv_yy    TYPE string,
        lv_dd    TYPE string,
        lv_mm    TYPE string,
        lv_hr    TYPE string,
        lv_min1  TYPE string,
        lv_min2  TYPE string,
        lv_min   TYPE string,
        lv_sec1  TYPE string,
        lv_sec2  TYPE string,
        lv_sec   TYPE string,
        lv_min_n TYPE n LENGTH 2,
        lv_sec_n TYPE n LENGTH 2,
        lv_hr_n  TYPE n LENGTH 2,
        lv_yy_n  TYPE n LENGTH 4,
        lv_mm_n  TYPE n LENGTH 2,
        lv_dd_n  TYPE n LENGTH 2.

  lt_path = p_fpath.

  CALL METHOD cl_gui_frontend_services=>gui_upload  "downloading file from desktop
    EXPORTING
      filename = lt_path
      filetype = 'DAT'
    CHANGING
      data_tab = lt_itab. "ITBL_IN_RECORD[]
  IF sy-subrc <> 0.
    MESSAGE 'Unable to read file from presentation server'(007) TYPE c_e.
  ENDIF.

  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH' "splitting the filename and directory
  EXPORTING
    full_name     = lt_path
  IMPORTING
    stripped_name = lv_filename
    file_path     = gv_file_path
  EXCEPTIONS
    x_error       = 0
    OTHERS        = 0.

  p_fpath = gv_file_path.


  SELECT * FROM zpm_foxboro_file INTO TABLE ta_existing
                                 WHERE filename = lv_filename AND
                                       status = 'S'.

  IF sy-subrc EQ 0.
    SORT ta_existing BY filename hdata pos.
  ENDIF.

  LOOP AT lt_itab INTO lw_itab. " storing the data to ta_file
    lv_count = lv_count + 1.
*    IF lv_count > 3.
    CLEAR : lv_right,lv_left,lv_count1,wa_file.
    MOVE lw_itab TO lv_file.
    DO.
      SPLIT lv_file AT space INTO lv_left lv_right.
      IF lv_right IS INITIAL AND lv_left IS INITIAL.
        EXIT.
      ENDIF.

      lv_file = lv_right.
      IF lv_left IS INITIAL.
        CONTINUE.
      ELSE.

        IF strlen( lv_left ) EQ 34.
          CLEAR: wa_file.
        ENDIF.

        lv_count1 = lv_count1 + 1.
        IF lv_count1 EQ 1.
          wa_file-hdata = lv_left.
          wa_file-unit = lv_left+0(3).
          wa_file-filename = lv_filename.
          wa_file-source = lv_filename+0(4).
          lv_yy = lv_left+4(4).
          lv_mm = lv_left+8(2).
          lv_dd = lv_left+10(2).
          lv_hr = lv_left+12(2).
          lv_min1 = lv_left+14(7) * 60.
          SPLIT lv_min1 AT '.' INTO lv_min lv_min2.
          CONCATENATE '.' lv_min2 INTO lv_min2.
          lv_sec1 = lv_min2 * 60.
          SPLIT lv_sec1 AT '.' INTO lv_sec lv_sec2.
          lv_hr_n  = lv_hr.
          lv_min_n = lv_min.
          lv_sec_n = lv_sec.
          lv_yy_n  = lv_yy.
          lv_mm_n  = lv_mm.
          lv_dd_n  = lv_dd.
          CONCATENATE lv_hr_n lv_min_n lv_sec_n INTO wa_file-c_time.
          CONCATENATE lv_yy_n lv_mm_n lv_dd_n INTO wa_file-c_date.
        ELSE.
          wa_file-pos = lv_count1 - 1.
          wa_file-value = lv_left.


          READ TABLE ta_existing WITH KEY filename = wa_file-filename
                                          hdata    = wa_file-hdata
                                          pos      = wa_file-pos
                                          BINARY SEARCH
                                          TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            APPEND wa_file TO ta_file.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " READ_FILE_APP

*&---------------------------------------------------------------------*
*&      Form  SEND_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM send_email .

  DATA : lv_maildata TYPE sodocchgi1,
         lt_mailhd TYPE STANDARD TABLE OF solisti1,
         lw_mailhd TYPE solisti1,
         lt_mailtxt TYPE STANDARD TABLE OF solisti1,
         lw_mailtxt TYPE solisti1,
         lt_mailrec TYPE STANDARD TABLE OF somlreci1,
         lw_mailrec TYPE somlreci1.


  CLEAR : lv_maildata,lt_mailtxt[],lw_mailtxt,lt_mailrec[],lw_mailrec.


  lv_maildata-obj_name = 'Measurement Document Notification'(011).
  lv_maildata-obj_descr = 'Foxboro-SAP Posting Unsuccessful'(012).
  lv_maildata-obj_langu = sy-langu.

  lw_mailhd-line = text-012.
  APPEND lw_mailhd TO lt_mailhd.

  WRITE sy-datum TO lw_mailtxt-line.
  CONCATENATE 'DATE OF ISSUE:' '' lw_mailtxt-line INTO lw_mailtxt-line.
  APPEND lw_mailtxt TO lt_mailtxt.
  CLEAR lw_mailtxt.
  WRITE sy-uzeit TO lw_mailtxt-line.
  CONCATENATE 'TIME OF ISSUE:' '' lw_mailtxt-line INTO lw_mailtxt-line.
  APPEND lw_mailtxt TO lt_mailtxt.
  CLEAR lw_mailtxt.
  APPEND lw_mailtxt TO lt_mailtxt.

  CONCATENATE 'FILENAME' 'HDATA' 'POS' 'UNIT' 'SOURCE' 'VALUE' 'MDOC' 'STATUS' INTO lw_mailtxt-line
  SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
  APPEND lw_mailtxt TO lt_mailtxt.
  CLEAR :lw_mailtxt.
  LOOP AT ta_status INTO wa_status WHERE status = c_e AND
                                         mdoc NE c_np.
    CONCATENATE wa_status-filename wa_status-hdata wa_status-unit wa_status-source wa_status-value
                wa_status-mdoc wa_status-status
                wa_status-err_dsc INTO lw_mailtxt-line
                SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    APPEND lw_mailtxt TO lt_mailtxt.
    CLEAR lw_mailtxt.
  ENDLOOP.
  IF sy-subrc NE 0.
    RETURN.
  ENDIF.
*
  lw_mailrec-receiver = 'Z_PM_FOXBORO'.
  lw_mailrec-rec_type = c_c.
  APPEND lw_mailrec TO lt_mailrec.
  CLEAR lw_mailrec.

  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      document_data              = lv_maildata
      document_type              = 'RAW'
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      object_header              = lt_mailhd
      object_content             = lt_mailtxt
      receivers                  = lt_mailrec
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  COMMIT WORK.

ENDFORM.                    " SEND_EMAIL
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FILE_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FPATH  text
*----------------------------------------------------------------------*
FORM validate_file_path.

  IF p_rbp = 'X'.

    DATA : lv_result(1) TYPE c,
           lv_filename  TYPE string.

    lv_filename = p_fpath.

    " check file existence
    CALL METHOD cl_gui_frontend_services=>file_exist
      EXPORTING
        file                 = lv_filename
      RECEIVING
        result               = lv_result
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_result IS INITIAL. "lv_result is X if valid file path
      MESSAGE 'Invalid File'(016) TYPE 'E'.
    ENDIF.

  ENDIF.

ENDFORM.                    " VALIDATE_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FILE_PATHA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_file_patha .

  IF p_rba = 'X'.

*splitting file path and name of the file
    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = p_fpath
      IMPORTING
        stripped_name = gv_file_name
        file_path     = gv_file_path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      MESSAGE text-000 TYPE 'E'.
    ENDIF.

    IF gv_file_name IS NOT INITIAL.
      OPEN DATASET p_fpath FOR INPUT IN TEXT MODE ENCODING DEFAULT.
      IF sy-subrc <> 0.
        MESSAGE 'File does not exist'(017) TYPE 'E'.
      ELSE.
        CLOSE DATASET p_fpath.
        IF sy-subrc NE 0.
          MESSAGE e000(zpm) WITH 'Error closing file'(021) gv_file_name.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " VALIDATE_FILE_PATHA
*&---------------------------------------------------------------------*
*&      Form  DELETE_ENTRIES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_entries. "Delete entries from ZPM_FOXBORO_FILE which are older than 14 days

  DATA: lv_date TYPE dats.

  lv_date = sy-datum - 14.
  DELETE FROM zpm_foxboro_file WHERE c_date LE lv_date
                                AND ( status = 'S' OR mdoc = 'NP' ).

ENDFORM.                    " DELETE_ENTRIES
