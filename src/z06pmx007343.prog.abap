*&---------------------------------------------------------------------*
*&  Include           Z06PMX007343
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  SELECT * INTO TABLE gt_config
         FROM z06pmt_op_config.

  SELECT *
       FROM zntconfig
       INTO TABLE t_config WHERE
                  controlpanel = 'ZACPPEM'.

*  SELECT SINGLE *
*       INTO CORRESPONDING FIELDS OF x_nt_defaults
*       FROM z06pmt_cp_deflt WHERE uname = sy-uname.
*
*  IF sy-subrc = 0.
*    x_defaults = x_nt_defaults.
*  ENDIF.
*
*  PERFORM display_logo.

  SET PF-STATUS 'PF_9000_01'.
  SET TITLEBAR 'TI_9000_01'.

ENDMODULE.                 " STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_CHART  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display_chart OUTPUT.

  CASE gv_save_code.

    WHEN 'F64'.
      PERFORM display_order_chart_type. " IW38 Ordef type
    WHEN 'F65'.
      PERFORM display_floc_chart_type." IW38 Flocation

    WHEN 'F67'.
      PERFORM display_fail_order_chart."by type

    WHEN 'F68'.
      PERFORM display_fail_floc_chart."by Loca

  ENDCASE.

ENDMODULE.                 " DISPLAY_CHART  OUTPUT
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
  dynproname-dnum = '9000'.

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

*&---------------------------------------------------------------------*
*&      Module  SET_DEFAULTS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_defaults OUTPUT.
  PERFORM sub_read_default_values.
ENDMODULE.                 " SET_DEFAULTS  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SUB_DETERMINE_FUNC_LOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SELPARAMS_LOW  text
*----------------------------------------------------------------------*
FORM sub_determine_func_loc CHANGING p_x_selparams TYPE rsparams.

  DATA : l_x_selparams TYPE rsparams.

  l_x_selparams = p_x_selparams.

  IF  x_defaults-zzunit1 IS INITIAL
  AND x_defaults-zzunit2 IS INITIAL.
    CONCATENATE c_n
                c_asterisk
                c_hyphen
                p_x_selparams-low
                INTO p_x_selparams-low.

    IF p_x_selparams-high IS NOT INITIAL.
      CONCATENATE c_n
                  c_asterisk
                  c_hyphen
                  p_x_selparams-high
                  INTO p_x_selparams-high.
    ENDIF.

    APPEND p_x_selparams TO t_selparams.

  ELSE.

    IF x_defaults-zzunit1 IS NOT INITIAL.
      CONCATENATE c_n
                  c_1
                  c_hyphen
                  p_x_selparams-low
                  INTO l_x_selparams-low.

      IF p_x_selparams-high IS NOT INITIAL.
        CONCATENATE c_n
                    c_1
                    c_hyphen
                    p_x_selparams-high
                    INTO l_x_selparams-high.
      ENDIF.

      APPEND l_x_selparams TO t_selparams.
    ENDIF.

    IF x_defaults-zzunit2 IS NOT INITIAL.
      CONCATENATE c_n
                  c_2
                  c_hyphen
                  p_x_selparams-low
                  INTO l_x_selparams-low.

      IF p_x_selparams-high IS NOT INITIAL.
        CONCATENATE c_n
                    c_2
                    c_hyphen
                    p_x_selparams-high
                    INTO l_x_selparams-high.
      ENDIF.

      APPEND l_x_selparams TO t_selparams.

    ENDIF.

  ENDIF.
ENDFORM.                    " SUB_DETERMINE_FUNC_LOC
" SUB_HANDLE_ORD_AUART
