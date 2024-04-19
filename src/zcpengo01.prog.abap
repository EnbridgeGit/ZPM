*&---------------------------------------------------------------------*
*&  Include           ZCPENGO01
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9000 OUTPUT.
  SET PF-STATUS 'GUI9000'.
  SET TITLEBAR 'TITLE9000'.
  SELECT *
          FROM zntconfig
          INTO TABLE t_config WHERE
                     controlpanel = 'ZCPENGR'.


ENDMODULE.                 " STATUS_9000  OUTPUT
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
