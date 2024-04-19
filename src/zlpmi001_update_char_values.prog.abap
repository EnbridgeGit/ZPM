*&---------------------------------------------------------------------*
*& Report  ZLPMI001_UPDATE_CHAR_VALUES
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* DD-MMM-YYYY  User ID     TR#         Change Description              *
* 09-04-2019   KMB         D30K929759  CHG0140461 Custom tcode ZLPMI001*
*                                      needs to be modified            *
************************************************************************

REPORT  zlpmi001_update_char_values.


TABLES: kssk,
        rfpdo.
TYPES: BEGIN OF ty_data,
       tobject(30),
       char_name(30),
       new_value(30),
       class(18),
       cuobj(18),
       atwrt(30),
       atflv(30),
       END OF ty_data.
TYPES: BEGIN OF ty_output,
       objek TYPE ausp-objek,
       char_name(30), " TYPE ausp-atinn,
       atinn TYPE ausp-atinn,
       cuobj TYPE inob-cuobj,
       atwrt TYPE ausp-atwrt,
*       atflv TYPE ausp-atflv,
*       new_atflv TYPE ausp-atflv,
       new_atwrt TYPE ausp-atwrt,
       obj_table TYPE tabelle,
       klart  TYPE ausp-klart,
       eqart TYPE eqart,
       atfor TYPE cabn-atfor,
       class TYPE klasse_d,
*       cuobj TYPE inob-cuobj,
       END OF ty_output.
DATA: gt_data TYPE TABLE OF ty_data,
      gs_data TYPE ty_data,
      gt_output TYPE TABLE OF ty_output,
      gs_output TYPE ty_output,
      gt_bapiret2 TYPE TABLE OF bapiret2.
DATA: ok_code TYPE sy-ucomm,
      save_ok TYPE ok_code.
DATA: gv_test TYPE xfeld,
      gr_container TYPE REF TO cl_gui_custom_container,
      gr_alvgrid   TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_layout    TYPE lvc_s_layo.
CONSTANTS: gc_name1 TYPE tvarvc-name VALUE 'ZPM_DSGN_CAP_ENH_ACTIVE'.
*DATA: gr_event_handler TYPE REF TO lcl_event_handler.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_klart FOR kssk-klart NO-DISPLAY. "OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(75) text-011.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(74) text-012.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(74) text-013.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(74) text-014.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b2.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS: "p_yes RADIOBUTTON GROUP rad1 DEFAULT 'X',
            "p_no RADIOBUTTON GROUP rad1,
            p_lfile TYPE rfpdo-rfbifile DEFAULT 'H:\'.
SELECTION-SCREEN END OF BLOCK b3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_lfile.
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      mask      = ',All Files ,*.*'
      static    = 'X'
    CHANGING
      file_name = p_lfile.

INITIALIZATION.

  PERFORM populate_selection.

START-OF-SELECTION.

  CLEAR: gv_test,
         gt_data,
         gs_data,
         gt_output,
         gs_output,
         gr_container,
         gr_alvgrid,
         gt_fieldcat,
         gs_layout,
         gt_bapiret2.

  PERFORM upload_file_pc.
  PERFORM prepare_data.
*PERFORM display_alv.
  PERFORM display_alv_cl.


*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_file_pc .

  DATA:   lt_auszug TYPE STANDARD TABLE OF string,
          ls_auszug TYPE string,
          lv_auszug_file TYPE string.
  "ls_msg TYPE ty_msg.

  REFRESH lt_auszug[].
  lv_auszug_file = p_lfile.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = lv_auszug_file
      has_field_separator = 'X'
      filetype            = 'ASC'
    CHANGING
      data_tab            = gt_data "lt_auszug
    EXCEPTIONS
      file_open_error     = 1
      file_read_error     = 2
      OTHERS              = 18.
  CASE sy-subrc.
    WHEN 1.
      WRITE: / 'File open error..'.
      STOP.
    WHEN 2 OR 18.
      WRITE: / 'File Read error..'.
      STOP.
    WHEN OTHERS.

  ENDCASE.
ENDFORM.                    " UPLOAD_FILE_PC
*&---------------------------------------------------------------------*
*&      Form  POPULATE_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM populate_selection .
  DATA: lv_name TYPE tvarvc-name VALUE 'ZPM_CLASS_UPDATE',
        lt_tvarvc TYPE TABLE OF tvarvc,
        ls_tvarvc TYPE tvarvc.

*  SELECT * FROM tvarvc INTO TABLE lt_tvarvc
*    WHERE name = lv_name.
*  LOOP AT lt_tvarvc INTO ls_tvarvc.
*    s_klart-option = ls_tvarvc-opti.
*    s_klart-sign   = ls_tvarvc-sign.
*    s_klart-low    = ls_tvarvc-low.
*    APPEND s_klart.
*  ENDLOOP.

  s_klart-option = 'EQ'.
  s_klart-sign   = 'I'.
  s_klart-low    = '002'.
  APPEND s_klart.
  s_klart-option = 'EQ'.
  s_klart-sign   = 'I'.
  s_klart-low    = '003'.
  APPEND s_klart.

ENDFORM.                    " POPULATE_SELECTION
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM prepare_data .
  DATA: lv_objek TYPE inob-objek,
        lv_cuobj TYPE inob-cuobj,
        lv_atwrt TYPE ausp-atwrt,
        lv_atinn TYPE ausp-atinn,
        lv_objek_ausp TYPE ausp-objek,
        lv_tplnr TYPE iflot-tplnr,
        lv_equnr TYPE equi-equnr,
        lv_table TYPE tabelle,
        lv_eqart TYPE eqart,
        lv_klart TYPE inob-klart,
        lv_atfor TYPE cabn-atfor,
        ls_equi  TYPE equi,
        ls_iflot TYPE iflot,
        ls_ausp TYPE ausp,
        ls_inob TYPE inob.

  LOOP AT gt_data INTO gs_data.
    CLEAR: lv_cuobj,
           lv_atwrt,
           lv_objek,
           lv_klart,
           lv_eqart,
           lv_atfor,
           ls_ausp,
           ls_equi,
           ls_iflot,
           ls_inob,
           gs_output.
    "check if technical object is Equipment or FLOC
    CALL FUNCTION 'CONVERSION_EXIT_TPLNR_INPUT'
      EXPORTING
        input                = gs_data-tobject
*       I_FLG_CHECK_INTERNAL = 'X'
      IMPORTING
        output               = lv_tplnr
      EXCEPTIONS
        not_found            = 1
        OTHERS               = 2.

    SELECT SINGLE * FROM iflot INTO ls_iflot WHERE tplnr = lv_tplnr.
    IF sy-subrc = 0.  "FLOC
      lv_objek = lv_tplnr.
      lv_table = 'IFLOT'.
      lv_eqart = ls_iflot-eqart.
    ELSE.  "Equipment
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gs_data-tobject
        IMPORTING
          output = lv_equnr.
      SELECT SINGLE * FROM equi INTO ls_equi WHERE equnr = lv_equnr.
      CHECK sy-subrc = 0.
      lv_objek = lv_equnr.
      lv_table = 'EQUI'.
      lv_eqart = ls_equi-eqart.
    ENDIF.
    "----------------
*    SELECT SINGLE cuobj INTO lv_cuobj FROM inob
*                                   WHERE objek = lv_objek
*                                     AND klart IN s_klart.
    SELECT SINGLE * INTO ls_inob FROM inob
                                WHERE objek = lv_objek
                                  AND klart IN s_klart.
    lv_cuobj = ls_inob-cuobj.
    lv_klart = ls_inob-klart.
    CHECK sy-subrc = 0.
    "lv_atwrt = gs_data-char_name.
    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = gs_data-char_name
      IMPORTING
        output = lv_atinn.
*    lv_atinn = gs_data-char_name.

    "get the characteristic data type
    SELECT SINGLE atfor INTO lv_atfor
                        FROM cabn
                       WHERE atinn = lv_atinn.

    lv_objek_ausp = lv_cuobj.
    SELECT SINGLE * FROM ausp INTO ls_ausp
                    WHERE objek = lv_cuobj
                      AND atinn = lv_atinn.
*    CHECK sy-subrc = 0.
    gs_output-new_atwrt = gs_data-new_value.
*    IF lv_atfor = 'CHAR'.
*      gs_output-new_atwrt = gs_data-new_value.
*    ELSEIF lv_atfor = 'NUM'.
*      CALL FUNCTION 'CHAR_FLTP_CONVERSION'
*        EXPORTING
**         DYFLD              = ' '
**         MASKN              = ' '
**         MAXDEC             = '16'
**         MAXEXP             = '59+'
**         MINEXP             = '60-'
*          string             = gs_data-new_value
**         MSGTYP_DECIM       = 'W'
*        IMPORTING
**         DECIM              =
**         EXPON              =
*          flstr              = gs_output-new_atflv
**         IVALU              =
*        EXCEPTIONS
*          exponent_too_big   = 1
*          exponent_too_small = 2
*          string_not_fltp    = 3
*          too_many_decim     = 4
*          OTHERS             = 5.
*      IF sy-subrc <> 0.
*        "skip this record.
*        CONTINUE.
*      ENDIF.
*    ELSEIF lv_atfor = 'CURR' OR
*           lv_atfor = 'DATE' OR
*           lv_atfor = 'TIME'.
*      gs_output-new_atflv = gs_data-new_value.
*    ELSE.
*      CONTINUE.
*    ENDIF.
    gs_output-objek = lv_objek.
    gs_output-char_name = gs_data-char_name.
    gs_output-cuobj = lv_cuobj.
*    gs_output-new_value = gs_data-new_value.
    IF lv_atfor = 'CHAR'.
      gs_output-atwrt = ls_ausp-atwrt.
    ELSE.
      gs_output-atwrt = ls_ausp-atflv.
    ENDIF.
*    gs_output-atflv = ls_ausp-atflv.
    gs_output-atinn = lv_atinn.
    gs_output-obj_table = lv_table.
    gs_output-eqart = lv_eqart.
    gs_output-klart = lv_klart.
    gs_output-atfor = lv_atfor.
    gs_output-class = gs_data-class.
    APPEND gs_output TO gt_output.
  ENDLOOP.
ENDFORM.                    " PREPARE_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
*  DATA:   ls_key         TYPE salv_s_layout_key,
*          lo_table       TYPE REF TO cl_salv_table,
*          lo_layout      TYPE REF TO cl_salv_layout,
*          lo_functions   TYPE REF TO cl_salv_functions,
*          lo_display     TYPE REF TO cl_salv_display_settings,
*          lo_columns     TYPE REF TO cl_salv_columns_table,
*          lo_column      TYPE REF TO cl_salv_column_table,  "#EC NEEDED
*          lo_content     TYPE REF TO cl_salv_form_element,
*          lo_grid        TYPE REF TO cl_salv_form_layout_grid,
*          lo_events_salv TYPE REF TO cl_salv_events_table.
*          "lo_event       TYPE REF TO lcl_event_handler.
*data: gr_events type ref to lcl_handle_events.
*  TRY.
*      CALL METHOD cl_salv_table=>factory
**        EXPORTING
**    list_display   = IF_SALV_C_BOOL_SAP=>FALSE
**          r_container    = lr_con1
**          container_name = 'ALV_CON1'
*        IMPORTING
*          r_salv_table   = lo_table
*        CHANGING
*          t_table        = gt_data.
*    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
*  ENDTRY.
**Function settings
*  lo_functions = lo_table->get_functions( ).
*  lo_functions->set_all( abap_true ).
* lo_table->set_screen_status(
*    pfstatus      =  'SALV_STANDARD'
*    report        =  sy-CPROG
*    set_functions = lo_table->c_functions_all ).
*
**... §6 register to the events of cl_salv_table
*  data: lr_events type ref to cl_salv_events_table.
*
*  lr_events = lo_table->get_event( ).
*
*  create object gr_events.
*
**... §6.1 register to the event USER_COMMAND
*  set handler gr_events->on_user_command for lr_events.
*
*  set handler gr_events->on_before_user_command for lr_events.
*
*  set handler gr_events->on_after_user_command for lr_events.
*
**TRY.
**CALL METHOD lo_functions->set_function
**  EXPORTING
**    name    = 'Test1'
**    boolean = 'X'
**    .
** CATCH cx_salv_not_found .
** CATCH cx_salv_wrong_call .
**       CATCH cx_salv_msg .                                 "#EC NO_HANDLER
**
**ENDTRY.
*
**Display Setting
*  lo_display = lo_table->get_display_settings( ).
*
*  lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
**Event
**  lo_events_salv = lo_table->get_event( ).
**  CREATE OBJECT lo_event.
**  SET HANDLER: lo_event->hotspot_click
**               FOR lo_events_salv.
**Set layout
*  lo_layout = lo_table->get_layout( ).
*  ls_key-report = sy-repid.
*  lo_layout->set_key( ls_key ).
*  lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
**  CALL METHOD lo_layout->set_initial_layout
**    EXPORTING
**      value = p_vari.
**Get columns
*  CALL METHOD lo_table->get_columns
*    RECEIVING
*      value = lo_columns.
******Change ALV Fields  - title etc.
*  PERFORM alv_fields USING lo_columns lo_column.
*******Display ALV
*  CALL METHOD lo_table->display.
ENDFORM.                    " DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  ALV_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_fields  USING  io_columns TYPE REF TO cl_salv_columns_table
                        io_column  TYPE REF TO cl_salv_column_table.

  DATA: lv_short_text TYPE scrtext_s,
        lv_med_text TYPE scrtext_m,
        lv_long_text TYPE scrtext_l.

******hot spot
*  TRY.
*      io_column ?= io_columns->get_column( 'ANLN1' ).
*      CALL METHOD io_column->set_cell_type
*        EXPORTING
*      value = if_salv_c_cell_type=>hotspot.
*    CATCH cx_salv_not_found .
*  ENDTRY.
*
*  TRY .
*      io_column ?= io_columns->get_column( 'KNAFA' ).
*      CALL METHOD io_column->set_alignment
*      EXPORTING
*        value  = IF_SALV_C_ALIGNMENT=>RIGHT.
*      lv_short_text = 'Amount'.
*      lv_med_text = 'Amount'.
*      lv_long_text = 'Amount'.
*      CALL METHOD io_column->set_short_text
*      EXPORTING
*        value  = lv_short_text.
*      CALL METHOD io_column->set_medium_text
*      EXPORTING
*        value  = lv_med_text.
*      CALL METHOD io_column->set_long_text
*      EXPORTING
*        value  = lv_long_text.
*  CATCH cx_salv_not_found .
*
*  ENDTRY.

ENDFORM.                    " ALV_FIELDS
*&---------------------------------------------------------------------*
*&      Form  SHOW_FUNCTION_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_SALV_FUNCTION  text
*      -->P_TEXT_I08  text
*----------------------------------------------------------------------*
FORM show_function_info USING i_function TYPE salv_de_function
                              i_text     TYPE string.

  DATA: l_string TYPE string.

  CONCATENATE i_text i_function INTO l_string SEPARATED BY space.

  MESSAGE i000(0k) WITH l_string.

ENDFORM.                    " SHOW_FUNCTION_INFO
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv_cl .
  DATA: ls_variant TYPE disvariant.

  CREATE OBJECT gr_alvgrid
    EXPORTING
*    i_shellstyle      = 0
*    i_lifetime        =
      i_parent          = cl_gui_custom_container=>default_screen "gr_container
*    i_appl_events     = space
*    i_parentdbg       =
*    i_applogparent    =
*    i_graphicsparent  =
*    i_name            =
*    i_fcat_complete   = space
  EXCEPTIONS
    error_cntl_create = 1
    error_cntl_init   = 2
    error_cntl_link   = 3
    error_dp_create   = 4
    OTHERS            = 5.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
****Create and register double click event
*  CREATE OBJECT gr_event_handler.
*  SET HANDLER gr_event_handler->handle_double_click
*              gr_event_handler->handle_hotspot_click
*              FOR gr_alvgrid.
***********
  PERFORM field_catalog.
  PERFORM prepare_layout.
*endif.
  CALL METHOD gr_alvgrid->set_gridtitle
    EXPORTING
      i_gridtitle = 'Mass Change for Characteristics'.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  ls_variant-report = sy-repid.
*  ls_variant-variant = p_vari.
  CALL METHOD gr_alvgrid->set_table_for_first_display
    EXPORTING
*     I_BUFFER_ACTIVE               =
*     I_BYPASSING_BUFFER            =
*     I_CONSISTENCY_CHECK           =
*     I_STRUCTURE_NAME              =
      is_variant                    = ls_variant
      i_save                        = 'A'
      i_default                     = 'X'
      is_layout                     = gs_layout
*     IS_PRINT                      =
*     IT_SPECIAL_GROUPS             =
*     IT_TOOLBAR_EXCLUDING          =
*     IT_HYPERLINK                  =
*     IT_ALV_GRAPHICS               =
*     IT_EXCEPT_QINFO               =
*     IR_SALV_ADAPTER               =
    CHANGING
      it_outtab                     = gt_output[]
      it_fieldcatalog               = gt_fieldcat
*     IT_SORT                       =
*     IT_FILTER                     =
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL SCREEN 100.
ENDFORM.                    " DISPLAY_ALV_CL
*&---------------------------------------------------------------------*
*&      Form  PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_layout .
  DATA: fl_title(50),
         fl_date(10).

  gs_layout-zebra  = 'X'.
  gs_layout-smalltitle = 'X'.
  gs_layout-sel_mode = 'A'.
  gs_layout-grid_title = fl_title.
ENDFORM.                    " PREPARE_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_catalog .

  DATA ls_fcat TYPE lvc_s_fcat.

  ls_fcat-fieldname = 'OBJEK'.
  ls_fcat-ref_table = 'AUSP'.
  ls_fcat-ref_field = 'OBJEK'.
  ls_fcat-outputlen = '30'.
*  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'Technical object'.
*  ls_fcat-seltext = ''.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ATINN'.
  ls_fcat-ref_table = 'AUSP'.
  ls_fcat-ref_field = 'ATINN'.
  ls_fcat-outputlen = '30'.
  ls_fcat-coltext = 'Characteristic Name'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

*  ls_fcat-fieldname = 'CHAR_NAME'.
**  ls_fcat-ref_table = 'AUSP'.
**  ls_fcat-ref_field = 'ATINN'.
*  ls_fcat-outputlen = '30'.
*  ls_fcat-coltext = 'Characteristic Name'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.

  ls_fcat-fieldname = 'CUOBJ'.
  ls_fcat-ref_table = 'INOB'.
  ls_fcat-ref_field = 'CUOBJ'.
  ls_fcat-outputlen = '18'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'CLASS'.
*  ls_fcat-ref_table = 'AUSP'.
  ls_fcat-ref_field = 'KLASSE_D'.
  ls_fcat-outputlen = '18'.
*  ls_fcat-just      = 'X'.
  ls_fcat-coltext = 'Class'.
  ls_fcat-seltext = 'Class'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

  ls_fcat-fieldname = 'ATWRT'.
  ls_fcat-ref_table = 'AUSP'.
  ls_fcat-ref_field = 'ATWRT'.
  ls_fcat-outputlen = '30'.
*  ls_fcat-just      = 'X'.
  ls_fcat-coltext = 'Current Value'.
  ls_fcat-seltext = 'Current Value'.
  APPEND ls_fcat TO gt_fieldcat.

*  CLEAR ls_fcat.
*  ls_fcat-fieldname = 'ATFLV'.
*  ls_fcat-ref_table = 'AUSP'.
*  ls_fcat-ref_field = 'ATFLV'.
*  ls_fcat-outputlen = '24'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'Current Value From'.
*  ls_fcat-seltext = 'Current Value From'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.

  ls_fcat-fieldname = 'NEW_ATWRT'.
  ls_fcat-ref_table = 'AUSP'.
  ls_fcat-ref_field = 'ATWRT'.
  ls_fcat-outputlen = '30'.
  ls_fcat-coltext = 'New Current Value'.
  ls_fcat-seltext = 'New Current Value'.
  APPEND ls_fcat TO gt_fieldcat.
  CLEAR ls_fcat.

*  ls_fcat-fieldname = 'NEW_ATFLV'.
*  ls_fcat-ref_table = 'AUSP'.
*  ls_fcat-ref_field = 'ATFLV'.
*  ls_fcat-outputlen = '24'.
**  ls_fcat-just      = 'X'.
*  ls_fcat-coltext = 'New Current Value From'.
*  ls_fcat-seltext = 'New Current Value From'.
*  APPEND ls_fcat TO gt_fieldcat.
*  CLEAR ls_fcat.


ENDFORM.                    " FIELD_CATALOG
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z01'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: lv_answer(1).
*  break sahmad.
  CASE sy-ucomm.
*    WHEN 'ZTEST'.
*      PERFORM test_action.
    WHEN 'ZPROC'.
*      IF gv_test IS INITIAL.
*        MESSAGE i000(zpm) WITH 'First run in Test mode' '' '' ''.
*      ELSE.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR                    = ' '
*         DIAGNOSE_OBJECT             = ' '
          text_question               = text-017
         text_button_1               = 'Yes'
*         ICON_BUTTON_1               = ' '
         text_button_2               = 'No'
*         ICON_BUTTON_2               = ' '
         default_button              = '2'
         display_cancel_button       = space "'X'
*         USERDEFINED_F1_HELP         = ' '
*         START_COLUMN                = 25
*         START_ROW                   = 6
*         POPUP_TYPE                  =
*         IV_QUICKINFO_BUTTON_1       = ' '
*         IV_QUICKINFO_BUTTON_2       = ' '
       IMPORTING
         answer                      = lv_answer
*       TABLES
*         PARAMETER                   =
       EXCEPTIONS
         text_not_found              = 1
         OTHERS                      = 2 .
      IF sy-subrc <> 0 OR lv_answer <> '1'.
* Implement suitable error handling here
      ELSE.
*        PERFORM process_action.
        PERFORM process_action_fm.
*        LEAVE TO SCREEN 0.
      ENDIF.
*      ENDIF.
    WHEN 'EXIT' OR 'CANCEL' OR
         'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_action .
*  DATA: ls_row      TYPE lvc_s_row,
*        lt_rows     TYPE lvc_t_row,
*        lv_counter  TYPE i.
*
*
*  CALL METHOD gr_alvgrid->get_selected_rows
*    IMPORTING
*      et_index_rows = lt_rows
**     et_row_no     =
*    .
*  IF lt_rows[] IS INITIAL.
*    MESSAGE i000(zpm) WITH 'First select rows to process' '' '' ''.
*    EXIT.
*  ENDIF.
*  LOOP AT lt_rows INTO ls_row.
*    CLEAR gs_output.
*    READ TABLE gt_output INTO gs_output INDEX ls_row-index.
*    CHECK sy-subrc = 0.
*    UPDATE ausp SET atwrt = gs_output-new_atwrt
*                    atflv = gs_output-new_atflv
*                WHERE objek = gs_output-cuobj   "objek
*                  AND atinn = gs_output-atinn. "gs_output-char_name.
**    COMMIT WORK.
*    lv_counter = lv_counter + 1.
*  ENDLOOP.
*  COMMIT WORK.
**  IF p_yes IS NOT INITIAL.
**    UPDATE tvarvc SET low = 'N'
**                  WHERE name = gc_name1.
**  ELSE.
**    UPDATE tvarvc SET low = 'Y'
**                  WHERE name = gc_name1.
**  ENDIF.
**  COMMIT WORK.
*  MESSAGE i000(zpm) WITH 'Update is completed. Record processed ' lv_counter '' ''.
**  gr_alvgrid->refresh_table_display( ).
ENDFORM.                    " PROCESS_ACTION
*&---------------------------------------------------------------------*
*&      Form  TEST_ACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM test_action .
  DATA:
      ls_row      TYPE lvc_s_row,
      lt_rows     TYPE lvc_t_row.

  CALL METHOD gr_alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows
*     et_row_no     =
    .
  IF lt_rows[] IS INITIAL.
    MESSAGE i000(zpm) WITH 'First select rows to test' '' '' ''.
    EXIT.
  ENDIF.
  LOOP AT lt_rows INTO ls_row.
    READ TABLE gt_data INTO gs_data INDEX ls_row-index.

  ENDLOOP.
  gv_test = 'X'.
ENDFORM.                    " TEST_ACTION
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ACTION_FM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM process_action_fm .
  DATA: ls_row      TYPE lvc_s_row,
        lt_rows     TYPE lvc_t_row,
        lv_counter  TYPE i,
        lv_find TYPE xfeld,
        lv_error TYPE xfeld,
        lv_classnum TYPE klasse_d,
        lv_charact  TYPE atnam,
        lv_objek TYPE kssk-objek,
        lt_numval TYPE TABLE OF	bapi1003_alloc_values_num,
        lt_charval TYPE TABLE OF bapi1003_alloc_values_char,
        lt_currval TYPE TABLE OF bapi1003_alloc_values_curr,
        ls_numval TYPE bapi1003_alloc_values_num,
        ls_charval TYPE bapi1003_alloc_values_char,
        ls_currval TYPE bapi1003_alloc_values_curr,
        lt_result TYPE TABLE OF bapiret2,
        ls_result TYPE bapiret2,
        lt_bapiret2 TYPE TABLE OF bapiret2,
        lt_kssk TYPE TABLE OF kssk,
        ls_kssk TYPE kssk,
        lt_msg TYPE TABLE OF string,
        ls_msg TYPE string.

  FIELD-SYMBOLS: <fs_numval> TYPE bapi1003_alloc_values_num,
                 <fs_charval> TYPE bapi1003_alloc_values_char,
                 <fs_currval> TYPE bapi1003_alloc_values_curr.


  CALL METHOD gr_alvgrid->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows
*     et_row_no     =
    .
  IF lt_rows[] IS INITIAL.
    MESSAGE i000(zpm) WITH 'First select rows to process' '' '' ''.
    EXIT.
  ENDIF.
  LOOP AT lt_rows INTO ls_row.
    CLEAR: gs_output,
           lt_kssk.
    READ TABLE gt_output INTO gs_output INDEX ls_row-index.
    CHECK sy-subrc = 0.
    lv_objek = gs_output-cuobj.
*    SELECT * FROM kssk INTO TABLE lt_kssk
*                  WHERE objek = lv_objek
*                    "AND MAFID
*                    AND klart = gs_output-klart.
*    LOOP AT lt_kssk INTO ls_kssk.
    CLEAR: lt_numval,
           lt_charval,
           lt_currval,
           lt_result,
           lv_charact,
           lv_find,
           lv_classnum,
           ls_msg.

*      SELECT SINGLE class INTO lv_classnum FROM klah
*        WHERE clint = ls_kssk-clint.
*        IF sy-subrc <> 0.
*          CONCATENATE 'Unable to find class from KLAH,' ls_kssk-clint
*                      ',' gs_output-objek INTO ls_msg SEPARATED BY space.
*          APPEND ls_msg TO lt_msg.
*          CONTINUE.
*        ENDIF.
*   -- Get the existing object characteristic values
    lv_classnum = gs_output-class.
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey        = gs_output-objek
        objecttable      = gs_output-obj_table
        classnum         = lv_classnum
        classtype        = gs_output-klart
*       unvaluated_chars = 'X'
      TABLES
        allocvaluesnum   = lt_numval
        allocvalueschar  = lt_charval
        allocvaluescurr  = lt_currval
        return           = lt_result.
    IF lt_numval[] IS INITIAL AND
       lt_charval[] IS INITIAL AND
       lt_currval[] IS INITIAL.
*BOC by KMB on 09.04.2019 CHG0140461 Custom tcode ZLPMI001 needs to be modified
*      CONTINUE.
      PERFORM f_create_classification CHANGING lv_counter.
*EOC by KMB on 09.04.2019 CHG0140461 Custom tcode ZLPMI001 needs to be modified
    ENDIF.
    "-------------------------------------------------
    lv_charact = gs_output-char_name.
    IF gs_output-atfor = 'CHAR'.
      READ TABLE lt_charval ASSIGNING <fs_charval> WITH KEY charact = lv_charact.
      IF sy-subrc EQ 0.
*   -- The char has value check and assign the new value if required.
*   -- Change the value
*        IF gs_output-new_atwrt IS NOT INITIAL.
        <fs_charval>-value_char = gs_output-new_atwrt.
        <fs_charval>-value_neutral = gs_output-new_atwrt.
*        ELSE.
*          DELETE lt_charval WHERE charact = lv_charact.
*        ENDIF.
*        lv_find = 'X'.
      ELSE.
        ls_charval-charact = lv_charact.
        ls_charval-value_char = gs_output-new_atwrt.
        ls_charval-value_neutral = gs_output-new_atwrt.
        APPEND ls_charval TO lt_charval.
      ENDIF.
    ELSEIF gs_output-atfor = 'NUM' OR
           gs_output-atfor = 'DATE' OR
           gs_output-atfor = 'TIME'.
      REPLACE ALL OCCURRENCES OF ',' IN gs_output-new_atwrt WITH space.
      CONDENSE gs_output-new_atwrt NO-GAPS.
      READ TABLE lt_numval ASSIGNING <fs_numval> WITH KEY charact = lv_charact.
      IF sy-subrc = 0.
*        IF gs_output-new_atflv IS NOT INITIAL.
        IF gs_output-new_atwrt IS NOT INITIAL.
          <fs_numval>-value_from = gs_output-new_atwrt. "new_atflv.
        ELSE.
          DELETE lt_numval WHERE charact = lv_charact.
        ENDIF.
*        lv_find = 'X'.
      ELSE.
        ls_numval-charact = lv_charact.
        ls_numval-value_from = gs_output-new_atwrt.  "new_atflv.
        APPEND ls_numval TO lt_numval.
      ENDIF.
    ELSEIF gs_output-atfor = 'CURR'.
      REPLACE ALL OCCURRENCES OF ',' IN gs_output-new_atwrt WITH space.
      CONDENSE gs_output-new_atwrt NO-GAPS.
      READ TABLE lt_currval ASSIGNING <fs_currval> WITH KEY charact = lv_charact.
      IF sy-subrc = 0.
*        IF gs_output-new_atflv IS NOT INITIAL.
        IF gs_output-new_atwrt IS NOT INITIAL.
          <fs_currval>-value_from = gs_output-new_atwrt.  "new_atflv.
        ELSE.
          DELETE lt_currval WHERE charact = lv_charact.
        ENDIF.
*        lv_find = 'X'.
      ELSE.
        ls_currval-charact = lv_charact.
        ls_currval-value_from = gs_output-new_atwrt. "new_atflv.
        APPEND ls_currval TO lt_currval.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.
*    IF lv_find IS INITIAL.
*      CONCATENATE 'Unable to find char(may be unvaluated):' lv_charact
*                ',' gs_output-objek INTO ls_msg SEPARATED BY space.
*      APPEND ls_msg TO lt_msg.
*      CONTINUE.
*    ENDIF.
    "Delete unassigned (unvaluated characteristics)
*    LOOP AT lt_charval ASSIGNING <fs_charval>.
*      IF <fs_charval>-value_char IS INITIAL.
*        DELETE lt_charval.
*      ENDIF.
*    ENDLOOP.
*    LOOP AT lt_numval ASSIGNING <fs_numval>.
*      IF <fs_numval>-value_from IS INITIAL.
*        DELETE lt_numval.
*      ENDIF.
*    ENDLOOP.
*    LOOP AT lt_currval ASSIGNING <fs_currval>.
*      IF <fs_currval>-value_from IS INITIAL.
*        DELETE lt_currval.
*      ENDIF.
*    ENDLOOP.
    "------------------
    CLEAR lt_result.
    "-------------------------------------------
    CALL FUNCTION 'BAPI_OBJCL_CHANGE'
      EXPORTING
        objectkey          = gs_output-objek
        objecttable        = gs_output-obj_table
        classnum           = lv_classnum
        classtype          = gs_output-klart
        status             = '1'
      TABLES
        allocvaluesnumnew  = lt_numval
        allocvaluescharnew = lt_charval
        allocvaluescurrnew = lt_currval
        return             = lt_result.
    CLEAR lv_error.
    LOOP AT lt_result INTO ls_result WHERE type = 'E' OR type = 'A'.
*         WRITE: / ls_result-type,
*                  ls_result-id,
*                  ls_result-number,
*                  ls_result-message,
*                  lv_charact.
*         APPEND ls_result to lt_bapiret2.
*         EXIT.
      CONCATENATE ls_result-message ',' lv_charact ','
                  gs_output-objek INTO ls_msg.
      APPEND ls_msg TO lt_msg.
      lv_error = 'X'.
    ENDLOOP.
*       IF sy-subrc = 0.
    IF lv_error = 'X'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait          = 'X'
*      IMPORTING
*        RETURN        =
            .
      lv_counter = lv_counter + 1.
    ENDIF.
*      ENDLOOP. "KSSK
  ENDLOOP. "Row
*  commit WORK.
*  IF p_yes IS NOT INITIAL.
*    UPDATE tvarvc SET low = 'N'
*                  WHERE name = gc_name1.
*  ELSE.
*    UPDATE tvarvc SET low = 'Y'
*                  WHERE name = gc_name1.
*  ENDIF.
*  COMMIT WORK.
  MESSAGE i000(zpm) WITH 'Update is completed. Record processed ' lv_counter '' ''.
  IF lt_msg[] IS NOT INITIAL.
    CALL FUNCTION 'POPUP_WITH_TABLE'
      EXPORTING
        endpos_col         = 120
        endpos_row         = 20
        startpos_col       = 3
        startpos_row       = 2
        titletext          = 'Messages'
*       IMPORTING
*         CHOICE             =
      TABLES
        valuetab           = lt_msg
     EXCEPTIONS
       break_off          = 1
       OTHERS             = 2 .
    IF sy-subrc <> 0.
*     Implement suitable error handling here
    ENDIF.
  ENDIF.

ENDFORM.                    " PROCESS_ACTION_FM
*BOC by KMB on 09.04.2019 CHG0140461 Custom tcode ZLPMI001 needs to be modified
*&---------------------------------------------------------------------*
*&      Form  F_CREATE_CLASSIFICATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_create_classification CHANGING lv_counter TYPE i.
  DATA: lv_classnum TYPE klasse_d,
        lt_num TYPE TABLE OF bapi1003_alloc_values_num,
        lt_char TYPE TABLE OF bapi1003_alloc_values_char,
        lt_curr TYPE TABLE OF bapi1003_alloc_values_curr,
        lt_return TYPE TABLE OF bapiret2,
        ls_return TYPE bapiret2,
        lv_error TYPE c.

  CLEAR lv_error.
  lv_classnum = gs_output-class.
  CALL FUNCTION 'BAPI_OBJCL_CREATE'
    EXPORTING
      objectkeynew    = gs_output-objek
      objecttablenew  = gs_output-obj_table
      classnumnew     = lv_classnum
      classtypenew    = gs_output-klart
    TABLES
      allocvaluesnum  = lt_num
      allocvalueschar = lt_char
      allocvaluescurr = lt_curr
      return          = lt_return.
  LOOP AT lt_return INTO ls_return WHERE type = 'E' OR type = 'A'.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    lv_error = 'X'.
  ENDLOOP.
  IF lv_error IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    lv_counter = lv_counter + 1.
  ENDIF.

ENDFORM.                    " F_CREATE_CLASSIFICATION
*EOC by KMB on 09.04.2019 CHG0140461 Custom tcode ZLPMI001 needs to be modified
