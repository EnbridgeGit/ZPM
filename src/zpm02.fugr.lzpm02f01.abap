*----------------------------------------------------------------------*
***INCLUDE LZPM02F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  UPDATE_ZFIELDS
*&---------------------------------------------------------------------*
* This include is used For Work Order
* It is used to create notification and update custome fields
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_zfields .
  DATA :ta_msg TYPE TABLE OF bdcmsgcoll.
  refresh : bdcdata[].

  IF lv_created_order IS NOT INITIAL.

*
*    PERFORM bdc_dynpro      USING 'SAPLCOIH' '0101'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'CAUFVD-AUFNR'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '/00'.
*    PERFORM bdc_field       USING 'CAUFVD-AUFNR'
*                                  lv_created_order.
*
*    PERFORM bdc_dynpro      USING 'SAPLCOIH' '3000'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                '=FREI'.
*
*    PERFORM bdc_dynpro      USING 'SAPLCOIH' '3000'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=BU'.
** Mode 'A' = Foreground mode
** Mode 'N' = Background mode
*
*    CALL TRANSACTION 'IW32' USING bdcdata
*                             MODE 'N'
*                             UPDATE 'S'
*                             MESSAGES INTO ta_msg.



    UPDATE aufk SET zzpmbworkord = wa_order_header-zzpmbworkord
                            zzpmbworktype = wa_order_header-zzpmbworktype
                            zzpmbscheddate = wa_order_header-zzpmbscheddate
                            zzpmbstatus = wa_order_header-zzpmbstatus
                        WHERE aufnr = lv_created_order.

  ENDIF.
  CLEAR : lv_created_order.

ENDFORM.                    " UPDATE_ZFIELDS
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = dynpro.
  bdcdata-dynbegin = 'X'.
  APPEND bdcdata.
ENDFORM.                    "BDC_DYNPRO

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> ''.
    CLEAR bdcdata.
    bdcdata-fnam = fnam.
    bdcdata-fval = fval.
    APPEND bdcdata.
  ENDIF.
ENDFORM.                    "BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_date USING p_date
                   CHANGING p_return LIKE ta_return.

  DATA : lv_date  TYPE  sy-datum.

  lv_date+0(4) = p_date(4). " Year
  lv_date+4(2) = p_date+4(2). " Month
  lv_date+6(2) = p_date+6(2). " Day
  CALL FUNCTION 'DATE_CHECK_PLAUSIBILITY'
    EXPORTING
      date                      = lv_date
    EXCEPTIONS
      plausibility_check_failed = 1
      OTHERS                    = 2.
  IF sy-subrc <> 0.

    wa_ret-wrk_order_ref = wa_order_header-zzpmbworkord.
    wa_ret-status_code = '4'.
    CONCATENATE p_date text-005  INTO lv_error .
    wa_ret-description = lv_error.
    APPEND wa_ret TO p_return.
    lv_err_flg  = 'X'.
  ENDIF.
  CLEAR: lv_error,
        wa_ret.
ENDFORM.                    " VALIDATE_DATE
" INSTALL_EQUIPMENT
