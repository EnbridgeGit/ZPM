*----------------------------------------------------------------------*
***INCLUDE ZCPOE_GUI_STATUS_9000 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SELECT *
         FROM zntconfig
         INTO TABLE t_config WHERE
                    controlpanel = 'ZCPOPS'.


  SET PF-STATUS 'PF9000'.
  SET TITLEBAR 'TITL9000'.
ENDMODULE.                 " STATUS_9000  OUTPUT



*&---------------------------------------------------------------------*
*&      Module  DROP_DISABLE  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE drop_disable OUTPUT.
**  DATA: w_r TYPE c.
*  IF NOT w_tr = 'X'.
*    REFRESH lt_vrm_values.
*    LOOP AT SCREEN.
*      CHECK screen-name = 'W_REV_CODE'.
*      screen-input = 0.
*      MODIFY SCREEN.
*    ENDLOOP.
*  ENDIF.
ENDMODULE.                 " DROP_DISABLE  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  DROP_DOWN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE drop_down OUTPUT.

  DATA: lw_vrm_values TYPE vrm_value,
        t352r-revnr TYPE c LENGTH 10.
  CONSTANTS: lc_drp TYPE vrm_id VALUE 'w_rev_code'.

  REFRESH lt_vrm_values.

  SELECT  revnr INTO TABLE lt_vrm_values FROM t352r WHERE iwerk = 'P107'.

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
ENDMODULE.                 " DROP_DOWN  OUTPUT
*----------------------------------------------------------------------*
*  MODULE change_push_butn_text
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

ENDMODULE.                    "change_push_butn_text
