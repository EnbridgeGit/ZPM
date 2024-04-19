*&---------------------------------------------------------------------*
*&  Include           ZCPMAINTCOOO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9001 OUTPUT.
  SET PF-STATUS 'GUI9001'.
  SET TITLEBAR 'TITLE9001'.

  SELECT * FROM zntconfig
           INTO TABLE t_config WHERE
                      controlpanel = 'ZCMCOR'.

ENDMODULE.                 " STATUS_9001  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SUB_GET_PROG_NAME
*&---------------------------------------------------------------------*
*       Get program name attached to a T-Code
*----------------------------------------------------------------------*
*      -->P_TCODE  Transaction Code
*      <--P_CPROG  Attached program name
*----------------------------------------------------------------------*
FORM sub_get_prog_name  USING    p_tcode TYPE sy-tcode
                        CHANGING p_cprog.

  SELECT SINGLE pgmna
         FROM tstc
         INTO p_cprog
         WHERE tcode = p_tcode.

ENDFORM.                    " SUB_GET_PROG_NAME

*&---------------------------------------------------------------------*
*&      Form  sub_submit_report
*&---------------------------------------------------------------------*
*       Submit Report Program with selection screen variant and
*       selection screen parameters
*----------------------------------------------------------------------*
*      -->P_T_SELPARAMS  Internal Table - Seltion Parameters
*      -->P_CPROG        Program Name
*      -->P_VARIANT      Selection Screen Variant
*----------------------------------------------------------------------*
FORM sub_submit_report TABLES p_t_selparams STRUCTURE rsparams
                       USING  p_cprog p_variant p_selscr.
  IF p_selscr IS INITIAL.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     VIA SELECTION-SCREEN AND RETURN.
  ELSE.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     AND RETURN.
  ENDIF.

ENDFORM.                    "sub_submit_report

*&---------------------------------------------------------------------*
*&      Form  SUB_CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_TCODE_IW38  text
*      <--P_L_RET_CODE  text
*----------------------------------------------------------------------*
FORM sub_check_tcode_authority USING    p_tcode  TYPE sy-tcode
                               CHANGING p_return TYPE sy-subrc.

  CLEAR p_return.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = p_tcode
    EXCEPTIONS
      ok     = 1
      not_ok = 2
      OTHERS = 3.

  p_return = sy-subrc.

ENDFORM. "_TCODE_AUTHORITY

*&---------------------------------------------------------------------*
*&      Form  SUB_READ_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_read_default_values .
  DATA : l_key TYPE indx-srtfd.
  SELECT SINGLE *
         INTO CORRESPONDING FIELDS
         OF x_nt_defaults
         FROM zntdeflt
         WHERE uname = sy-uname.

  IF sy-subrc = 0.
    x_defaults = x_nt_defaults.
  ENDIF.
ENDFORM.                    " SUB_READ_DEFAULT_VALUES

*&---------------------------------------------------------------------*
*&      Module  DROP_DOWN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE drop_down OUTPUT.
  DATA: lw_vrm_values TYPE vrm_value,
        lw_vrm_values1 TYPE vrm_value,
          t352r-revnr TYPE c LENGTH 10.
  CONSTANTS: lc_drp TYPE vrm_id VALUE 'w_rev_code'.

  REFRESH :lt_vrm_values, lt_vrm_values1.

*Plant is global used every where
  IF w_r_smc = 'X' .
    SELECT  revnr INTO TABLE lt_vrm_values FROM t352r WHERE iwerk = 'P103'.
  ELSE.
    SELECT  revnr INTO TABLE lt_vrm_values FROM t352r WHERE iwerk = 'P107'.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = lc_drp
      values          = lt_vrm_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  IF w_smc = 'X'." plant only linked to this section
    SELECT arbpl INTO TABLE lt_vrm_values1 FROM crhd WHERE werks = 'P103'.
  ELSE.
    SELECT arbpl INTO TABLE lt_vrm_values1 FROM crhd WHERE werks = 'P107'.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'W_ARBPL'
      values          = lt_vrm_values1
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDMODULE.                 " DROP_DOWN  OUTPUT
*----------------------------------------------------------------------*
*  MODULE change_push_butn_text OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE change_push_butn_text OUTPUT.
  DATA text(30) VALUE ''.
  DATA : repid TYPE sy-repid,
        lv_fnam TYPE d021s-fnam.
  DATA : f1 TYPE TABLE OF trmsg WITH HEADER LINE.
  DATA : dynp_header   TYPE d020s,
        tdynp_fields  TYPE TABLE OF d021s,
         tdynp_logic   TYPE TABLE OF d022s,
         tdynp_matchc  TYPE TABLE OF d023s,
*        dynp_fields  TYPE d021s,
         dynp_logic   TYPE d022s,
         dynp_matchc  TYPE d023s.

  FIELD-SYMBOLS : <dynp_fields>  TYPE d021s.
  DATA: BEGIN OF dynproname,
           prog LIKE d020s-prog,
           dnum LIKE d020s-dnum,
         END OF dynproname.


  CLEAR: dynp_header,
  dynp_logic, dynp_matchc,
  text.
  REFRESH: tdynp_fields, tdynp_logic, tdynp_matchc.
  repid = sy-repid.
  dynproname-prog = repid.
  dynproname-dnum = '9001'.

  IMPORT DYNPRO dynp_header tdynp_fields
                 tdynp_logic  tdynp_matchc ID dynproname.

  LOOP AT t_config.
    text = t_config-btntxt.
    CONDENSE text.
    lv_fnam = t_config-fcode.
    IF text IS NOT INITIAL.
      READ TABLE tdynp_fields ASSIGNING <dynp_fields>
                  WITH KEY fnam = lv_fnam.
      IF sy-subrc IS INITIAL.
        <dynp_fields>-stxt = text.
*        <dynp_fields>-leng = 20.
      ENDIF.
    ENDIF.
  ENDLOOP.

  EXPORT DYNPRO dynp_header tdynp_fields
                 tdynp_logic  tdynp_matchc ID dynproname.
  GENERATE DYNPRO dynp_header tdynp_fields
                 tdynp_logic  tdynp_matchc ID dynproname MESSAGE f1 LINE f1 WORD f1 .

ENDMODULE.                    "change_push_butn_text OUTPUT
