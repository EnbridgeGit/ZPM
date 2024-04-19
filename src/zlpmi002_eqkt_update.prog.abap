REPORT  ZLPMI002_EQKT_UPDATE LINE-SIZE 132 MESSAGE-ID zs
        LINE-COUNT 65 NO STANDARD PAGE HEADING.
*&---------------------------------------------------------------------*
*& Author: Glenn Ymana
*& Date  : March, 2011.
*& Description:
*&
*& This program will set the long text indicator (field KZLTX)
*& on table EQKT.  By setting KZLTX, the long text icon will appear
*& when running tcode IE03 (Display Equipment).  The icon was missing
*& after loading equipment long text via LSMW
*&---------------------------------------------------------------------*
*& Changes:
*&---------------------------------------------------------------------*
TABLES: eqkt.

TYPES: BEGIN OF ty_data,
       refid(4)   TYPE c,
       equipno    TYPE eqkt-equnr,
       ltxtcd(4)  TYPE c,
       spras(1)   TYPE c,
       longtxt    TYPE c,
       END OF ty_data.

TYPES: BEGIN OF ty_msg,
        msgrp TYPE char01,
        msgid TYPE symsgid,
        msgty TYPE symsgty,
        msgno TYPE symsgno,
        msg_text(250),
       END OF ty_msg.
DATA:BEGIN OF gt_tab OCCURS 0,         "Text file format
           text1(208),
     END OF gt_tab.
CONSTANTS:
        w_dmtr TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP'.
DATA: w_returncode  TYPE i,
      w_msg(50)     TYPE c,
      ln_cntr       TYPE i VALUE 99,
      i_count       TYPE i VALUE 0,
      w_err_flag    TYPE c VALUE 'N',
      w_nodata_flag TYPE c VALUE 'N',
      gt_id_msg TYPE TABLE OF ty_msg,
      gv_flag_err_proc TYPE xfeld,
      gs_data TYPE ty_data,
      gt_data TYPE TABLE OF ty_data,
      gs_eqkt TYPE eqkt,
      gt_eqkt TYPE TABLE OF eqkt.

*----------------------------------------------------------------------*
* selection screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS: r_server  RADIOBUTTON GROUP rad1 DEFAULT 'X'  USER-COMMAND cmd,
            p_sfile   LIKE        rfpdo-rfbifile MODIF ID srv,
            r_local   RADIOBUTTON GROUP rad1,
            p_lfile   TYPE        rfpdo-rfbifile DEFAULT 'H:\' MODIF ID lcl.
SELECTION-SCREEN END OF BLOCK b1.
*----------------------------------------------------------------------*
*                  at selection-screen
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.
  PERFORM  f_toggle_functionality.

*----------------------------------------------------------------------*
*                  start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF r_server IS NOT INITIAL.
    IF p_sfile IS INITIAL.
      WRITE: / 'Input Server File path and name.'.
      STOP.
    ELSE.
      PERFORM upload_server_file.
    ENDIF.
  ELSE.
    IF p_lfile IS INITIAL.
      WRITE : / 'Input PC file path and name.'.
      STOP.
    ELSE.
      PERFORM upload_pc_file.
    ENDIF.
  ENDIF.

  PERFORM get_data.

  IF w_err_flag ='N'.
    PERFORM call_function.
  ENDIF.

*----------------------------------------------------------------------*
*       Form  get_data
*----------------------------------------------------------------------*
FORM get_data.

  LOOP AT gt_data INTO gs_data.

    SELECT * from EQKT into gs_eqkt
      WHERE equnr EQ gs_data-equipno.
    ENDSELECT.

    IF sy-subrc EQ 0.
       gs_eqkt-kzltx = 'X'.
       APPEND gs_eqkt to gt_eqkt.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "get_data

*----------------------------------------------------------------------*
*       Form  call_function
*----------------------------------------------------------------------*
FORM call_function.

  DATA: gt_del_eqkt TYPE TABLE OF eqkt,
        gt_ins_eqkt TYPE TABLE OF eqkt.

  CALL FUNCTION 'EQKT_SAVE_UPD_TASK'
    TABLES
      it_eqkt_upd          = gt_eqkt
      it_eqkt_del          = gt_del_eqkt
      it_eqkt_ins          = gt_ins_eqkt
* EXCEPTIONS
*   ERROR_EQKT_UPD       = 1
*   ERROR_EQKT_INS       = 2
*   ERROR_EQKT_DEL       = 3
*   OTHERS               = 4
            .

  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    "call_function



*&---------------------------------------------------------------------*
*&      Form  F_TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_toggle_functionality .
  LOOP AT SCREEN.

* Set the screen fields to display only
    IF  screen-group1 EQ gc_modif_id_dsp.
      screen-input = 0.
    ENDIF.
    IF r_local = 'X'.
      IF screen-group1 = 'LCL'.
        screen-input = 1.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 0.
      ENDIF.
    ELSE.
      IF screen-group1 = 'LCL'.
        screen-input = 0.
      ENDIF.
      IF screen-group1 = 'SRV'.
        screen-input = 1.
      ENDIF.
    ENDIF.
    "-----------------------
    MODIFY   SCREEN.
  ENDLOOP.
ENDFORM.                    " F_TOGGLE_FUNCTIONALITY
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_SERVER_FILE
*&---------------------------------------------------------------------*
*       Upload Server File
*----------------------------------------------------------------------*
FORM upload_server_file .
  DATA: lv_cnt TYPE i,
         ls_msg TYPE ty_msg.

  OPEN DATASET p_sfile FOR INPUT IN TEXT MODE ENCODING DEFAULT.
  IF sy-subrc <> 0.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'E'.
    ls_msg-msgno = '000'.
    MOVE text-011 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Input File: ' p_sfile INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    gv_flag_err_proc = 'X'.
    EXIT.
  ENDIF.

  DO.
    CLEAR: gs_data,
           gt_tab.
    READ DATASET p_sfile INTO gt_tab-text1. "fl_string.
    IF sy-subrc <> 0.
*      WRITE: / 'Unable to read File data..'.
      EXIT.
    ENDIF.
*    "first line is columns header, ignore it.
*    lv_cnt = lv_cnt + 1.
*   IF lv_cnt = 1.
*     CONTINUE.
*   ENDIF.
    PERFORM split_data USING gt_tab
                             gs_data.
    APPEND gs_data TO gt_data.
  ENDDO.
  CLOSE DATASET p_sfile.
  IF sy-subrc <> 0.
*    gt_msg-text1 = 'Unable to close text file for process, Please check.'.
*    APPEND gt_msg.
*    WRITE: / 'Unable to close text file for process, Please check.'.
    ls_msg-msgrp = 'G'.
    ls_msg-msgid = 'ZFI01'.
    ls_msg-msgty = 'E'.
    ls_msg-msgno = '000'.
    MOVE text-002 TO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    CONCATENATE 'Input File: ' p_sfile INTO ls_msg-msg_text.
    APPEND ls_msg TO gt_id_msg.
    gv_flag_err_proc = 'X'.
    EXIT.
  ENDIF.
ENDFORM.                    " UPLOAD_SERVER_FILE
*&---------------------------------------------------------------------*
*&      Form  SPLIT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM split_data  USING   p_gt_tab STRUCTURE gt_tab
                         p_output STRUCTURE gs_data.
  DATA: lv_amnt1(13),
        lv_amnt2 TYPE p LENGTH 13 DECIMALS 2.

  SPLIT p_gt_tab AT w_dmtr
        INTO p_output-refid
             p_output-equipno
             p_output-ltxtcd
             p_output-spras
             p_output-longtxt.

ENDFORM.                    " SPLIT_DATA
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_PC_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_pc_file .

  DATA:   lt_auszug TYPE STANDARD TABLE OF string,
          ls_auszug TYPE string,
          lv_auszug_file TYPE string,
          ls_msg TYPE ty_msg.

  REFRESH lt_auszug[].
  lv_auszug_file = p_lfile.
  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = lv_auszug_file
*     has_field_separator = 'X'
      filetype            = 'ASC'
    CHANGING
      data_tab            = lt_auszug
    EXCEPTIONS
      file_open_error     = 1
      file_read_error     = 2
      OTHERS              = 18.
  CASE sy-subrc.
    WHEN 1.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'File Open error in file' p_lfile
            INTO ls_msg-msg_text
            SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
    WHEN 2.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'Read error in file' p_lfile
      INTO ls_msg-msg_text
      SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
    WHEN 18.
      ls_msg-msgrp = 'G'.
      ls_msg-msgid = 'ZFI01'.
      ls_msg-msgty = 'E'.
      ls_msg-msgno = '000'.
      CONCATENATE 'Read error in file' p_lfile
      INTO ls_msg-msg_text
      SEPARATED BY space.
      APPEND ls_msg TO gt_id_msg.
      gv_flag_err_proc = 'X'.
  ENDCASE.
  LOOP AT lt_auszug INTO ls_auszug.
*    "first line is columns header, ignore it.
*    IF sy-tabix = 1.
*      CONTINUE.
*    ENDIF.
    gt_tab-text1 = ls_auszug.
    PERFORM split_data USING gt_tab
                             gs_data.
    APPEND gs_data TO gt_data.
  ENDLOOP.
ENDFORM.                    " UPLOAD_PC_FILE
