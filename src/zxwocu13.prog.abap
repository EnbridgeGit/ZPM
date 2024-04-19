*&---------------------------------------------------------------------*
*&  Include           ZXWOCU13
*&---------------------------------------------------------------------*

DATA : st_afvgd TYPE afvgd,
       lv_text(40) TYPE c.
FIELD-SYMBOLS : <fs_afvgd> TYPE any.
lv_text = '(SAPLCOMK)AFVGD'.
ASSIGN  (lv_text) TO <fs_afvgd>.
CHECK sy-subrc = 0.
st_afvgd = <fs_afvgd> .
st_afvgd-usr00 = afvgd_imp-usr00.
st_afvgd-usr01 = afvgd_imp-usr01.
st_afvgd-usr02 = afvgd_imp-usr02.
st_afvgd-usr03 = afvgd_imp-usr03.
st_afvgd-usr04 = afvgd_imp-usr04.
<fs_afvgd> = st_afvgd.
*IMPORT afvgd_imp-usr00 FROM MEMORY ID 'US0'.

**** 92
DATA: ls_messages      TYPE bapiret2,
      lt_messages      TYPE TABLE OF bapiret2.

DATA: lv_origpr        TYPE banfn,
      lv_banfn         TYPE banfn,
      lv_origprtxt     TYPE symsgv.

DATA: lv_ebeln         TYPE ebeln,
      lv_ebelp         TYPE ebelp,
      lv_user          TYPE z_ariba_approver,
      lv_zaribaaprover TYPE z_ariba_approver,
      lv_usertxt       TYPE symsgv,
      lv_agreement     TYPE symsgv.

IF afvgd_imp-slwid = 'P2P_PM' AND ( afvgd_imp-usr00 <> '' OR afvgd_imp-usr02 <> '' ).
  "merge agrement and line
  CONCATENATE afvgd_imp-usr00 '/' afvgd_imp-usr02 INTO lv_agreement.

  "Prepend 0's
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = afvgd_imp-usr00
    IMPORTING
      output = lv_ebeln.

  "Prepend 0's
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = afvgd_imp-usr02
    IMPORTING
      output = lv_ebelp.

  SELECT SINGLE ebeln ebelp
    INTO (lv_ebeln, lv_ebelp)
    FROM ekpo
    WHERE ebeln = lv_ebeln
      AND ebelp = lv_ebelp.


  IF sy-subrc <> 0.
    ls_messages-type    = 'E'.
    ls_messages-id      = 'ME'.
    ls_messages-number  = '303'.
    ls_messages-message_v1 = text-009.
    ls_messages-message_v2 = lv_agreement.
    ls_messages-message_v3 = text-003.

    APPEND ls_messages TO lt_messages.
  ENDIF.
ENDIF.

IF afvgd_imp-slwid = 'P2P_PM' AND afvgd_imp-usr01 <> ''.

  TRANSLATE afvgd_imp-usr01 TO UPPER CASE.
  lv_user = afvgd_imp-usr01.
  SELECT SINGLE zariba_approver
    INTO lv_zaribaaprover
    FROM zaribaaprv
    WHERE zariba_approver = lv_user.

  IF sy-subrc <> 0.
    lv_usertxt = lv_user.

    ls_messages-type    = 'E'.
    ls_messages-id      = 'ME'.
    ls_messages-number  = '303'.
    ls_messages-message_v1 = text-002.
    ls_messages-message_v2 = lv_usertxt.
    ls_messages-message_v3 = text-003.

    APPEND ls_messages TO lt_messages.
  ENDIF.
ENDIF.

IF afvgd_imp-slwid = 'P2P_PM' AND afvgd_imp-usr03 <> ''.
  "Purchase Rec
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = afvgd_imp-usr03
    IMPORTING
      output = lv_origpr.

  SELECT SINGLE banfn
    INTO lv_banfn
    FROM eban
    WHERE banfn = lv_origpr.

  IF sy-subrc <> 0.
    lv_origprtxt = lv_origpr.

    ls_messages-type    = 'E'.
    ls_messages-id      = 'ME'.
    ls_messages-number  = '303'.
    ls_messages-message_v1 = text-001.
    ls_messages-message_v2 = lv_origprtxt.
    ls_messages-message_v3 = text-003.

    APPEND ls_messages TO lt_messages.
  ENDIF.
ENDIF.

IF lt_messages IS NOT INITIAL.
  CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
    TABLES
      i_bapiret2_tab = lt_messages.

  MESSAGE text-100 TYPE 'E'.
ENDIF.
