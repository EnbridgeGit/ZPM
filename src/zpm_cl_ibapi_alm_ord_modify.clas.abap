class ZPM_CL_IBAPI_ALM_ORD_MODIFY definition
  public
  final
  create public .

public section.
*"* public components of class ZPM_CL_IBAPI_ALM_ORD_MODIFY
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_IBAPI_ALM_ORD_MODIFY .
protected section.
*"* protected components of class ZPM_CL_IBAPI_ALM_ORD_MODIFY
*"* do not include other source files here!!!
private section.
*"* private components of class ZPM_CL_IBAPI_ALM_ORD_MODIFY
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZPM_CL_IBAPI_ALM_ORD_MODIFY IMPLEMENTATION.


METHOD if_ex_ibapi_alm_ord_modify~modify_input_data.

 DATA: LV_AUFNR TYPE AUFK-AUFNR,
        LV_TABIX TYPE SY-TABIX,
        LS_HEADER_INT TYPE ALMB_CAUFVD,
        LS_HEADER_INT_UP TYPE IBAPI_CAUFVD_UPDATE,
        ls_header type bapi_alm_order_headers_i .

  BREAK SAHMAD.

  lv_aufnr = EXTENSION_IN-VALUEPART1+0(12).
  IF EXTENSION_IN-STRUCTURE = 'ZPM_CREATE_WO'.
     READ TABLE CT_HEADER_INT INTO LS_HEADER_INT WITH KEY AUFNR = LV_AUFNR.
     LV_TABIX = SY-TABIX.
      IF SY-SUBRC = 0.
         LS_HEADER_INT-ZZPMBWORKORD   = EXTENSION_IN-VALUEPART1+12(16).
         ls_header_int-zzpmbworktype   = EXTENSION_IN-VALUEPART1+28(4).
         LS_HEADER_INT-ZZPMBSCHEDDATE = EXTENSION_IN-VALUEPART1+32(8).
         ls_header_int-zzpmbstatus    = EXTENSION_IN-VALUEPART1+40(4).
         MODIFY CT_HEADER_INT FROM Ls_HEADER_INT index LV_TABIX TRANSPORTING
                      ZZPMBWORKORD ZZPMbWORKTYPE ZZPMBSCHEDDATE ZZPMBSTATuS.
      ELSE.
         LS_HEADER_INT-AUFNR          = LV_AUFNR.
         LS_HEADER_INT-ZZPMBWORKORD   = EXTENSION_IN-VALUEPART1+12(16).
         ls_header_int-zzpmbworktype   = EXTENSION_IN-VALUEPART1+28(4).
         LS_HEADER_INT-ZZPMBSCHEDDATE = EXTENSION_IN-VALUEPART1+32(8).
         ls_header_int-zzpmbstatus    = EXTENSION_IN-VALUEPART1+40(4).
         APPEND LS_HEADER_INT TO CT_HEADER_INT.
      ENDIF.
      READ TABLE CT_HEADER_INT_UP INTO LS_HEADER_INT_UP WITH KEY AUFNR = LV_AUFNR.
      LV_TABIX = SY-TABIX.
      IF SY-SUBRC = 0.
         LS_HEADER_INT_UP-ZZPMBWORKORD   = 'X'.
         ls_header_int_up-zzpmbworktype   = 'X'.
         LS_HEADER_INT_UP-ZZPMBSCHEDDATE = 'X'.
         ls_header_int_up-zzpmbstatus    = 'X'.
         MODIFY CT_HEADER_INT_UP FROM LS_HEADER_INT_UP index LV_TABIX TRANSPORTING
                            ZZPMBWORKORD ZZPMbWORKTYPE ZZPMBSCHEDDATE ZZPMBSTATuS.
      ELSE.
         IF lv_aufnr <> '%00000000001'.
         LS_HEADER_INT_UP-ZZPMBWORKORD   = 'X'.
         ls_header_int_up-zzpmbworktype   = 'X'.
         LS_HEADER_INT_UP-ZZPMBSCHEDDATE = 'X'.
         ls_header_int_up-zzpmbstatus    = 'X'.
         APPEND LS_HEADER_INT_UP TO CT_HEADER_INT_UP.
         endif.
      ENDIF.
  ENDIF.

ENDMETHOD.
ENDCLASS.
