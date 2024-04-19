*&---------------------------------------------------------------------*
*&  Include           ZXWOCU12
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(CAUFVD_IMP) LIKE  CAUFVD STRUCTURE  CAUFVD
*"             VALUE(AFVGD_IMP) LIKE  AFVGD STRUCTURE  AFVGD
*"             VALUE(HELP_FIELD) LIKE  DD03D-FIELDNAME
*"             VALUE(ACTYP) LIKE  TC10-TRTYP
*"       EXPORTING
*"             VALUE(SELECT_VALUE)
*"----------------------------------------------------------------------

DATA: ls_shlp TYPE shlp_descr,
      lt_interfaces TYPE ddshifaces,
      ls_interfaces TYPE LINE OF ddshifaces.

DATA: it_return TYPE ddshretval OCCURS 1 WITH HEADER LINE.

DATA: lv_ebeln LIKE afvgd_imp-usr01.


*P2P
IF afvgd_imp-slwid = 'P2P_PM'.
  IF help_field = 'AFVGD-USR00'.
    "Agreement
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'ZPM_CONTRACTS'
        shlptype = 'SH'
      IMPORTING
        shlp     = ls_shlp.

    lt_interfaces = ls_shlp-interface.
    ls_interfaces-valfield = 'X'.
    MODIFY lt_interfaces FROM ls_interfaces TRANSPORTING valfield
    WHERE shlpfield = 'EBELN'
      OR  shlpfield = 'EBELP'.
    ls_shlp-interface = lt_interfaces.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = ls_shlp
      TABLES
        return_values = it_return.

    LOOP AT it_return.
      IF it_return-fieldname = 'EBELN'.
        "Set Agreement Number
        select_value =  it_return-fieldval.
      ELSEIF it_return-fieldname = 'EBELP'.
        "Set Agreement Line
        lv_ebeln = it_return-fieldval.
        EXPORT lv_ebeln = lv_ebeln TO MEMORY ID 'ZXWOCU12_MEMORY'.
      ENDIF.
    ENDLOOP.

    CLEAR it_return.
  ELSEIF help_field = 'AFVGD-USR02'.
    "Agreement Line
    IMPORT lv_ebeln = lv_ebeln FROM MEMORY ID 'ZXWOCU12_MEMORY'.
    select_value = lv_ebeln.
*    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
*      EXPORTING
*        shlpname = 'MEKK'
*        shlptype = 'SH'
*      IMPORTING
*        shlp     = ls_shlp.
*
*    lt_interfaces = ls_shlp-interface.
*
*    ls_interfaces-valfield = 'X'.
*    MODIFY lt_interfaces FROM ls_interfaces TRANSPORTING valfield
*    WHERE shlpfield = 'EBELP'.
*
*
*    CLEAR ls_interfaces.
*    ls_interfaces-value = afvgd_imp-usr00.
*    MODIFY lt_interfaces FROM ls_interfaces TRANSPORTING value
*    WHERE shlpfield = 'EBELN'.
*
*
*    ls_shlp-interface = lt_interfaces.
*
*    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
*      EXPORTING
*        shlp          = ls_shlp
*      TABLES
*        return_values = it_return.
*
*    select_value =  it_return-fieldval.
*
*    CLEAR it_return.
  ELSEIF help_field = 'AFVGD-USR01'.
    "Ariba Approver
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'ZARIBA_APPROVER'
        shlptype = 'SH'
      IMPORTING
        shlp     = ls_shlp.

    lt_interfaces = ls_shlp-interface.
    ls_interfaces-valfield = 'X'.
    MODIFY lt_interfaces FROM ls_interfaces TRANSPORTING valfield
    WHERE shlpfield = 'ZARIBA_APPROVER'.
    ls_shlp-interface = lt_interfaces.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = ls_shlp
      TABLES
        return_values = it_return.

    select_value =  it_return-fieldval.

    CLEAR it_return.
  ELSEIF help_field = 'AFVGD-USR03'.
    "Old PR
    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = 'MBAN'
        shlptype = 'SH'
      IMPORTING
        shlp     = ls_shlp.

    lt_interfaces = ls_shlp-interface.
    ls_interfaces-valfield = 'X'.
    MODIFY lt_interfaces FROM ls_interfaces TRANSPORTING valfield
    WHERE shlpfield = 'BANFN'.
    ls_shlp-interface = lt_interfaces.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = ls_shlp
      TABLES
        return_values = it_return.

    select_value =  it_return-fieldval.

    CLEAR it_return.
  ENDIF.

ENDIF.
