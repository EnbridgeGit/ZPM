*&---------------------------------------------------------------------*
*& Report  Z_PM_ENTRY_LIST_CREATE
*& Author: Nageshwar Reddy
*&---------------------------------------------------------------------*
*& Purpose: Create Measuring Point Entry List
*& Date: 7-Nov-2014
*&---------------------------------------------------------------------*

REPORT  z_pm_entry_list_create.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_file,
         namel(20) TYPE c,
         meltx(40) TYPE c,
         sortf(20) TYPE c,
         oblig(1) TYPE c,
         point(12) TYPE c,
       END   OF ty_file.

TYPES: BEGIN OF ty_list,
        status  TYPE char1.
        INCLUDE TYPE ty_file AS file.
TYPES:  message TYPE bapi_msg,
       END OF   ty_list.

*&---------------------------------------------------------------------*
*& GLOBAL DATA
*&---------------------------------------------------------------------*
DATA: git_dbtb TYPE SORTED TABLE OF   t499s WITH UNIQUE KEY werks stand ##needed,

      git_bdcdata TYPE STANDARD TABLE OF bdcdata,

      git_file TYPE STANDARD TABLE OF ty_file ##needed,
      git_list TYPE STANDARD TABLE OF ty_list ##needed,
      gr_salv      TYPE REF TO cl_salv_table            ##needed,
      gr_sorts     TYPE REF TO cl_salv_sorts,
      gr_functions TYPE REF TO cl_salv_functions_list   ##needed,
      gr_display   TYPE REF TO cl_salv_display_settings ##needed,
      gr_content   TYPE REF TO cl_salv_form_element     ##needed,
      gr_column    TYPE REF TO cl_salv_column ##needed,
      gr_columns   TYPE REF TO cl_salv_columns_table    ##needed.

*&---------------------------------------------------------------------*
*& SELECTION SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
PARAMETERS : p_flapp TYPE ibipparms-path MODIF ID m1,
             p_flpre TYPE ibipparms-path MODIF ID m2,
             p_slapp TYPE c RADIOBUTTON GROUP r1 USER-COMMAND usr DEFAULT 'X',
             p_slpre TYPE c RADIOBUTTON GROUP r1,
             p_hdext TYPE c AS CHECKBOX.
*             p_sim   TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& EVENT: AT SELECTION SCREEN OUTPUT
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_modify.

*&---------------------------------------------------------------------*
*& EVENT: AT SELECTION SCREEN ON VALUE REQUEST
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_flapp.
  PERFORM f4_app_file USING 'X'
                      CHANGING p_flapp.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_flpre.
  PERFORM f4_app_file USING space
                      CHANGING p_flpre.

*&---------------------------------------------------------------------*
*& EVENT: AT SELECTION SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  PERFORM mandatory_check.

*&---------------------------------------------------------------------*
*& EVENT: AT SELECTION SCREEN ON
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN ON p_flapp.
  PERFORM check_app_file_exist.

AT SELECTION-SCREEN ON p_flpre.
  PERFORM check_pre_file_exist.

*&---------------------------------------------------------------------*
*& EVENT: START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM read_file.
  PERFORM validate_file.
*  PERFORM update_db.
  PERFORM process_entry_list.

*&---------------------------------------------------------------------*
*& EVENT: END OF SELECTION
*&---------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM display_list.

*&---------------------------------------------------------------------*
*&      Form  SCREEN_MODIFY
*&---------------------------------------------------------------------*
*       Modify screen based on radiobutton selection
*----------------------------------------------------------------------*
FORM screen_modify .

  LOOP AT SCREEN.
    IF p_slapp EQ abap_true.
      IF screen-group1 = 'M2'.
        screen-active = 0.
        screen-invisible = 1.
      ELSE.
        screen-active = 1.
        screen-invisible = 0.
      ENDIF.
    ELSEIF p_slpre EQ abap_true.
      IF screen-group1 = 'M1'.
        screen-active = 0.
        screen-invisible = 1.
      ELSE.
        screen-active = 1.
        screen-invisible = 0.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.                    " SCREEN_MODIFY
*&---------------------------------------------------------------------*
*&      Form  F4_APP_FILE
*&---------------------------------------------------------------------*
*       F4 help for input file path
*----------------------------------------------------------------------*
*  -->  p_applserv  'X' if file in app. server, ' ' if presentation
*  <--  p_file_name  Input file path
*----------------------------------------------------------------------*
FORM f4_app_file USING    p_applserv  TYPE as4flag
                 CHANGING p_file_name TYPE ibipparms-path.

  DATA: lv_gui_ext    TYPE string,
        lv_file_name  TYPE string,
        lv_gui_filter TYPE string,
        lv_title      TYPE string.

* Function to provide F4 help
  CALL METHOD cl_rsan_ut_files=>f4
    EXPORTING
      i_applserv         = p_applserv
      i_title            = lv_title
      i_gui_extension    = lv_gui_ext
      i_gui_ext_filter   = lv_gui_filter
      i_applserv_logical = space
      i_applserv_al11    = 'X'
    CHANGING
      c_file_name        = lv_file_name
    EXCEPTIONS
      failed             = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  p_file_name = lv_file_name.

ENDFORM.                    " F4_APP_FILE
*&---------------------------------------------------------------------*
*&      Form  MANDATORY_CHECK
*&---------------------------------------------------------------------*
*       Check if input file path is entered
*----------------------------------------------------------------------*
FORM mandatory_check .

  IF sy-ucomm EQ 'USR'.
    RETURN.
  ENDIF.

  IF ( p_slapp IS NOT INITIAL AND p_flapp IS INITIAL ) OR
     ( p_slpre IS NOT INITIAL AND p_flpre IS INITIAL ).
    MESSAGE 'Please enter the input file path'(001) TYPE 'E'.
  ENDIF.

ENDFORM.                    " MANDATORY_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_APP_FILE_EXIST
*&---------------------------------------------------------------------*
*       Validate input file path in application server
*----------------------------------------------------------------------*
FORM check_app_file_exist .

  IF p_flapp IS INITIAL.
    RETURN.
  ENDIF.

* Try opening the file, if it doesnt, then file does not exist
  OPEN DATASET p_flapp FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc = 0.
    CLOSE DATASET p_flapp.
  ELSE.
    MESSAGE  'File does not exist. Please check your entry'(002) TYPE 'E'.
  ENDIF.

ENDFORM.                    " CHECK_APP_FILE_EXIST
*&---------------------------------------------------------------------*
*&      Form  CHECK_PRE_FILE_EXIST
*&---------------------------------------------------------------------*
*       Check if file exists in user's machine
*----------------------------------------------------------------------*
FORM check_pre_file_exist .

  DATA: lv_file       TYPE string,
        lv_file_exist TYPE xflag.

  IF p_flpre IS INITIAL.
    RETURN.
  ENDIF.

  lv_file = p_flpre.

* Function to check if file exist in user machine
  CALL METHOD cl_gui_frontend_services=>file_exist
    EXPORTING
      file   = lv_file
    RECEIVING
      result = lv_file_exist.

  IF NOT ( sy-subrc = 0 AND lv_file_exist = 'X' ).
    MESSAGE  'File does not exist. Please check your entry'(002) TYPE 'E'.
  ENDIF.

ENDFORM.                    " CHECK_PRE_FILE_EXIST
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       Read contents of the file
*----------------------------------------------------------------------*
FORM read_file .

  DATA: lv_file_name TYPE string,
        lt_data_tab  TYPE rsanm_file_table,
        lwa_data     TYPE rsanm_file_line,
        lwa_file     TYPE ty_file.

  IF p_flapp IS NOT INITIAL.
    lv_file_name = p_flapp.

* Reading file from application server
    CALL METHOD cl_rsan_ut_appserv_file_reader=>appserver_file_read
      EXPORTING
        i_filename   = lv_file_name
*       i_lines      = -1
      CHANGING
        c_data_tab   = lt_data_tab
      EXCEPTIONS
        open_failed  = 1
        read_failed  = 2
        close_failed = 3
        OTHERS       = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* Convert flat file to internal table
    LOOP AT lt_data_tab INTO lwa_data.
      SPLIT lwa_data AT cl_abap_char_utilities=>horizontal_tab
                     INTO lwa_file-namel
                          lwa_file-meltx
                          lwa_file-sortf
                          lwa_file-oblig
                          lwa_file-point.
      APPEND lwa_file TO git_file.
      CLEAR lwa_file.
    ENDLOOP.

  ELSEIF p_flpre IS NOT INITIAL.
    lv_file_name = p_flpre.

* Read file from user's machine
    CALL METHOD cl_gui_frontend_services=>gui_upload
      EXPORTING
        filename                = lv_file_name
        filetype                = 'ASC'
        has_field_separator     = 'X'
      CHANGING
        data_tab                = git_file
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        not_supported_by_gui    = 17
        error_no_gui            = 18
        OTHERS                  = 19.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

* if file contains header, then delete it before processing
  IF p_hdext IS NOT INITIAL.
    DELETE git_file INDEX 1.
  ENDIF.
* Do not proceed if, file is empty
  IF git_file IS INITIAL.
    MESSAGE 'File is empty'(003) TYPE 'I'.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " READ_DATA
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_FILE
*&---------------------------------------------------------------------*
*       Validate contents of the file before processing
*----------------------------------------------------------------------*
FORM validate_file .

  TYPES: BEGIN OF lty_point,
           point TYPE imptt-point,
         END OF lty_point.
*
  DATA:
        lit_file  TYPE STANDARD TABLE OF ty_file,
        lit_point TYPE STANDARD TABLE OF lty_point,
        lwa_list  TYPE ty_list,
        lwa_file  TYPE ty_file.

  FIELD-SYMBOLS: <lfs_file> TYPE ty_file.

* Get all unique plants from the file
  lit_file = git_file.
  SORT lit_file BY point.
  DELETE ADJACENT DUPLICATES FROM lit_file COMPARING point.

  LOOP AT lit_file ASSIGNING <lfs_file>.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <lfs_file>-point
      IMPORTING
        output = <lfs_file>-point.

  ENDLOOP.

* Get measurement points from system for pre-validation
  SELECT point FROM imptt
               INTO TABLE lit_point
               FOR ALL ENTRIES IN lit_file
               WHERE point = lit_file-point.

  IF sy-subrc EQ 0.
    SORT lit_point.
  ENDIF.
*
  LOOP AT git_file INTO lwa_file.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_file-point
      IMPORTING
        output = lwa_file-point.

    lwa_list-file = lwa_file.

    IF lwa_file-point IS NOT INITIAL.
      READ TABLE lit_point WITH KEY point = lwa_file-point
                           TRANSPORTING NO FIELDS
                           BINARY SEARCH.
      IF sy-subrc NE 0.
        lwa_list-status = '1'.
        CONCATENATE 'Mesaurement Point:'(018) lwa_file-point 'does not exist'(008)
                    INTO lwa_list-message SEPARATED BY space.
      ENDIF.
    ENDIF.

    IF lwa_file-oblig NE space AND
       lwa_file-oblig NE abap_true.
      lwa_list-status = '1'.
      IF lwa_list-message IS INITIAL.
        lwa_list-message = 'Obligatory field invalid. Value has to be empty or X'(017).
      ELSE.
        CONCATENATE lwa_list-message 'Obligatory field invalid. Value has to be empty or X'(017)
                    INTO lwa_list-message SEPARATED BY ';'.
      ENDIF.
    ENDIF.

    IF lwa_list-message IS INITIAL.
      lwa_list-status = '3'.
    ENDIF.

    APPEND lwa_list TO git_list.
    CLEAR lwa_list.

  ENDLOOP.

ENDFORM.                    " VALIDATE_PLANT
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_LIST
*&---------------------------------------------------------------------*
*       Display list with status of update
*----------------------------------------------------------------------*
FORM display_list .

  DATA: lv_ltext TYPE scrtext_l,
        lv_mtext TYPE scrtext_m,
        lv_stext TYPE scrtext_s.

* Generate ALV instance
  TRY.
      CALL METHOD cl_salv_table=>factory
        EXPORTING
          list_display = if_salv_c_bool_sap=>false
        IMPORTING
          r_salv_table = gr_salv
        CHANGING
          t_table      = git_list.

      IF gr_salv IS INITIAL.
        MESSAGE 'Error Creating ALV Grid'(013) TYPE 'A'.
      ENDIF.

* Get ALV Functions
      gr_functions = gr_salv->get_functions( ).

* Activate All Buttons in Tool Bar
      gr_functions->set_all( if_salv_c_bool_sap=>true ).

* Get display functions
      gr_display = gr_salv->get_display_settings( ).

* Striped aLV Patther
      gr_display->set_striped_pattern( if_salv_c_bool_sap=>true ).

* Get the columns from ALV Table
      gr_columns = gr_salv->get_columns( ).

      lv_ltext = 'Entry List Name'(020).
      lv_mtext = 'List Name'(021).
      lv_stext = 'List Name'(021).
      gr_column = gr_columns->get_column('NAMEL').
      gr_column->set_long_text( lv_ltext ).
      gr_column->set_medium_text( lv_mtext ).
      gr_column->set_short_text( lv_stext ).

      lv_ltext = 'Short Description'(022).
      lv_mtext = 'Description'(023).
      lv_stext = 'Description'(023).
      gr_column = gr_columns->get_column('MELTX').
      gr_column->set_long_text( lv_ltext ).
      gr_column->set_medium_text( lv_mtext ).
      gr_column->set_short_text( lv_stext ).

      lv_ltext = 'Sort Field'(024).
      lv_mtext = 'Sort Field'(024).
      lv_stext = 'Sort'(025).
      gr_column = gr_columns->get_column('SORTF').
      gr_column->set_long_text( lv_ltext ).
      gr_column->set_medium_text( lv_mtext ).
      gr_column->set_short_text( lv_stext ).

      lv_ltext = 'Obligatory'(026).
      lv_mtext = 'Obligatory'(026).
      lv_stext = 'Obligatory'(026).
      gr_column = gr_columns->get_column('OBLIG').
      gr_column->set_long_text( lv_ltext ).
      gr_column->set_medium_text( lv_mtext ).
      gr_column->set_short_text( lv_stext ).

      lv_ltext = 'Measurement Point'(027).
      lv_mtext = 'Measurement Point'(027).
      lv_stext = 'Point'(028).
      gr_column = gr_columns->get_column('POINT').
      gr_column->set_long_text( lv_ltext ).
      gr_column->set_medium_text( lv_mtext ).
      gr_column->set_short_text( lv_stext ).

* Optimize column widht
      gr_columns->set_optimize( if_salv_c_bool_sap=>true ).

* Fix Key fields of the ALV
      gr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).

* Generate exception column using Status field

      gr_columns->set_exception_column( value = 'STATUS' ).


******* Top of List settings *******
      PERFORM top_of_page CHANGING gr_content.
      gr_salv->set_top_of_list( gr_content ).

      gr_sorts = gr_salv->get_sorts( ).

      CALL METHOD gr_sorts->add_sort
        EXPORTING
          columnname = 'NAMEL'
          subtotal   = if_salv_c_bool_sap=>false.

* Display ALV
      CALL METHOD gr_salv->display.
    CATCH cx_salv_not_found ##no_handler.
    CATCH cx_salv_data_error ##no_handler.
    CATCH cx_salv_msg ##no_handler.

  ENDTRY.

ENDFORM.                    " DISPLAY_LIST
*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_CONTENT  text
*----------------------------------------------------------------------*
FORM top_of_page CHANGING lr_content TYPE REF TO cl_salv_form_element.

  DATA : lr_grid  TYPE REF TO cl_salv_form_layout_grid,
         lr_text  TYPE REF TO cl_salv_form_text,
         lr_label TYPE REF TO cl_salv_form_label,

         lr_head  TYPE string,

         lv_lines TYPE i,
         lv_file_name TYPE string.

  IF p_slapp EQ abap_true.
    lv_file_name = p_flapp.
  ELSE.
    lv_file_name = p_flpre.
  ENDIF.

  DESCRIBE TABLE git_list LINES lv_lines.

  MOVE 'Upload Entry List'(019) TO lr_head.
  CREATE OBJECT lr_grid.
** Header of Top of Page **
  lr_grid->create_header_information( row     = 1
                                      column  = 1
                                      text    = lr_head
                                      tooltip = lr_head ).
** Add Row **
  lr_grid->add_row( ).

** Add Label in Grid **
  lr_label = lr_grid->create_label( row = 2
                                    column = 1
                                    text = 'File Location:'(015)
                                    tooltip = 'File Location:'(015) ).

** Add Text in The Grid **
  lr_text = lr_grid->create_text( row = 2
                                  column = 2
                                  text = lv_file_name
                                  tooltip = lv_file_name ).
** Set Label and Text Link **
  lr_label->set_label_for( lr_text ).

** Add Row **
  lr_grid->add_row( ).

** Add Label in Grid **
  lr_label = lr_grid->create_label( row = 3
                                    column = 1
                                    text = 'No of Records read from file:'(016)
                                    tooltip = 'No of Records read from file:'(016) ).

** Add Text in The Grid **
  lr_text = lr_grid->create_text( row = 3
                                  column = 2
                                  text = lv_lines
                                  tooltip = lv_lines ).
** Set Label and Text Link **
  lr_label->set_label_for( lr_text ).

** Add Row **
  lr_grid->add_row( ).

** Move lr_grid to lr_content **
  lr_content = lr_grid.

ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ENTRY_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_entry_list.

  FIELD-SYMBOLS: <fs_list_item> TYPE  ty_list,
                 <fs_list_hdr> TYPE ty_list.

  DATA: lt_list TYPE STANDARD TABLE OF  ty_list,
        ls_list TYPE ty_list,
        lv_open TYPE char1.

  ls_list-status = '1'.

  lt_list = git_list.
  SORT lt_list BY namel.
  DELETE ADJACENT DUPLICATES FROM lt_list COMPARING namel.

  LOOP AT lt_list ASSIGNING <fs_list_hdr>.

    READ TABLE git_list WITH KEY status = '1'
                                 namel  = <fs_list_hdr>-namel
                                 TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      MODIFY git_list FROM ls_list TRANSPORTING status WHERE namel = <fs_list_hdr>-namel.
      CONTINUE.
    ENDIF.

    IF lv_open IS INITIAL.
      lv_open = abap_true.
      PERFORM open_group.
    ENDIF.

    PERFORM bdc_dynpro      USING 'SAPLIMR7' '1210'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'IMEL-NAMEL'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '/00'.
    PERFORM bdc_field       USING 'IMEL-NAMEL'
                                  <fs_list_hdr>-namel.

    LOOP AT git_list ASSIGNING <fs_list_item> WHERE namel = <fs_list_hdr>-namel.

      <fs_list_item>-message = 'Record added to BDC Session ENTRY_LIST'(029).

*-- Page down
      PERFORM bdc_dynpro      USING 'SAPLIMR7' '4210'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'IMEP-POINT(01)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=P+'.
      PERFORM bdc_field       USING 'IMEL-NAMEL'
                                    <fs_list_item>-namel.

      PERFORM bdc_field       USING  'IMEL-MELTX'
                               <fs_list_item>-meltx.

*-- Enter the values

      PERFORM bdc_dynpro      USING 'SAPLIMR7' '4210'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'IMEP-POINT(02)'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '/00'.
      PERFORM bdc_field       USING 'IMEL-NAMEL'
                                    <fs_list_item>-namel.
      PERFORM bdc_field       USING 'IMEP-SORTF(02)'
                                    <fs_list_item>-sortf.
      PERFORM bdc_field       USING 'IMEP-OBLIG(02)'
                                    <fs_list_item>-oblig.
      PERFORM bdc_field       USING 'IMEP-POINT(02)'
                                    <fs_list_item>-point.

    ENDLOOP.

*--
    PERFORM bdc_dynpro      USING 'SAPLIMR7' '4210'.
    PERFORM bdc_field       USING 'BDC_CURSOR'
                                  'IMEP-POINT(01)'.
    PERFORM bdc_field       USING 'BDC_OKCODE'
                                  '=SAVE'.
    PERFORM bdc_field       USING 'IMEL-NAMEL'
                                  <fs_list_hdr>-namel.
    PERFORM bdc_transaction USING 'IK31'.
  ENDLOOP.

  IF lv_open IS NOT INITIAL.
    PERFORM close_group.
  ENDIF.

ENDFORM.                    " PROCESS_ENTRY_LIST

*&---------------------------------------------------------------------*
*&      Form  BDC_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TCODE      text
*----------------------------------------------------------------------*
FORM bdc_transaction USING tcode.

* batch input session

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode     = tcode
    TABLES
      dynprotab = git_bdcdata.

*  ENDIF.
  REFRESH git_bdcdata.
ENDFORM.                    "BDC_TRANSACTION

*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING p_program
                      p_dynpro .
  DATA: lw_bdcdata TYPE bdcdata.
  lw_bdcdata-program  = p_program.
  lw_bdcdata-dynpro   = p_dynpro.
  lw_bdcdata-dynbegin = 'X'.
  APPEND lw_bdcdata TO git_bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING p_fnam
                     p_fval .
  DATA: lw_bdcdata TYPE bdcdata.
  lw_bdcdata-fnam = p_fnam.
  lw_bdcdata-fval = p_fval.
  APPEND lw_bdcdata TO git_bdcdata.
ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  OPEN_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM open_group.
*   open batchinput group
  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client = sy-mandt
      group  = 'ENTRY_LIST'
      user   = sy-uname
      keep   = space.

ENDFORM.                    "OPEN_GROUP

*----------------------------------------------------------------------*
*   end batchinput session                                             *
*   (call transaction using...: error session)                         *
*----------------------------------------------------------------------*
FORM close_group.
*   close batchinput group
  CALL FUNCTION 'BDC_CLOSE_GROUP'.

ENDFORM.                    "CLOSE_GROUP
