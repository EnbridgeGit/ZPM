
*&---------------------------------------------------------------------*
*&  Include           Z_PM_COMPLIANCE_DATE_CALCULATE
*&---------------------------------------------------------------------*
TYPES: gty_wmmpt TYPE STANDARD TABLE OF wc_wmmpt,
       gty_wmhis TYPE STANDARD TABLE OF wc_wmhis.
*{   INSERT         D30K928160                                        2
TYPES: gty_wmhio TYPE STANDARD TABLE OF wc_wmhio.
*}   INSERT

DATA: gt_mmpt  TYPE gty_wmmpt,
      gt_mhis  TYPE gty_wmhis,
      gs_mmpt  TYPE wc_wmmpt,
      gs_mhis  TYPE wc_wmhis.
*{   INSERT         D30K928160                                        3
DATA: gt_mhio  TYPE gty_wmhio,
      gs_mhio  TYPE wc_wmhio.
*}   INSERT

FIELD-SYMBOLS: <fs_mmpt_tab> TYPE gty_wmmpt,
               <fs_mhis_tab> TYPE gty_wmhis.
*{   INSERT         D30K928160                                        4
FIELD-SYMBOLS: <fs_mhio_tab> TYPE gty_wmhio.
*}   INSERT

CONSTANTS: gc_cmpdt_auart TYPE tvarvc-name VALUE 'ZPM_CMPDT_AUART'.

SELECT COUNT(*) FROM tvarvc
                WHERE name = gc_cmpdt_auart AND
                      low = caufvd-auart.                                  .

IF sy-subrc EQ 0 AND " only for order types in TVARVC
   caufvd-abruf_flag EQ abap_true. " only for Orders gen. out of Main. Plan

* Get all the Maint. plan relevant data
  ASSIGN ('(SAPLIWP3)MMPT_TAB[]') TO <fs_mmpt_tab>.
  ASSIGN ('(SAPLIWP3)AMHIS[]') TO <fs_mhis_tab>.
*{   INSERT         D30K928160                                        1
  ASSIGN ('(SAPLIWP3)AMHIO[]') TO <fs_mhio_tab>.
*}   INSERT

  IF <fs_mmpt_tab> IS ASSIGNED AND
     <fs_mhis_tab> IS ASSIGNED.

* Check if it is a counter based call
    IF mhis_tab-termk EQ '3' OR mhis_tab-termk EQ '4'.
      zcl_pm_mnplan_order_update=>gv_ctr_call = abap_true.
    ENDIF.

    READ TABLE <fs_mhis_tab> WITH KEY warpl = caufvd-warpl
                                      abnum = caufvd-abnum
                                      terma = 'M'
                                      TRANSPORTING NO FIELDS.
* check if it is a manual call
    IF sy-subrc EQ 0.
      zcl_pm_mnplan_order_update=>gv_man_call = abap_true.
    ENDIF.

* Get the Cycle frequency value and unit
    gt_mmpt = <fs_mmpt_tab>.
    SORT gt_mmpt BY warpl nummer.
    READ TABLE gt_mmpt INTO gs_mmpt WITH KEY warpl  = mhis_tab-warpl
                                             nummer = mhis_tab-zaehl.
    IF sy-subrc EQ 0.

* Unit of cycle frequency
      zcl_pm_mnplan_order_update=>gv_zeieh = gs_mmpt-zeieh.

* Cylce value is a floating number. To be convered to character
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          char_unit       = zcl_pm_mnplan_order_update=>gv_zeieh
          decimals        = 0
          exponent        = 0
          fltp_value_si   = gs_mmpt-zykl1
          indicator_value = abap_true
          masc_symbol     = space
        IMPORTING
          char_value      = zcl_pm_mnplan_order_update=>gv_cycle_value
        EXCEPTIONS
          no_unit_given   = 1
          unit_not_found  = 2
          OTHERS          = 3.
      IF sy-subrc EQ 0.

* Capture the basic start date - Userd to calculate Compliance date & Basic. fin.date
        zcl_pm_mnplan_order_update=>gv_gstrp = caufvd-gstrp.
        CONDENSE zcl_pm_mnplan_order_update=>gv_cycle_value.

* If annual, get the last inspection completion date
        IF gs_mmpt-zeieh EQ 'MON' AND zcl_pm_mnplan_order_update=>gv_cycle_value EQ '12'.
*{   REPLACE        D30K928160                                        7
*\          gt_mhis = <fs_mhis_tab>.
*\          SORT gt_mhis BY lrmdt DESCENDING.
*\          READ TABLE gt_mhis INTO gs_mhis INDEX 1.
*\          IF sy-subrc EQ 0.
*\            zcl_pm_mnplan_order_update=>gv_lrmdt = gs_mhis-lrmdt.
*\          ENDIF.
          gt_mhio = <fs_mhio_tab>.
          SORT gt_mhio BY warpl wppos addat DESCENDING.
          READ TABLE gt_mhio INTO gs_mhio WITH KEY warpl = caufvd-warpl
                                                   wppos = caufvd-wapos.
          IF sy-subrc EQ 0.
            zcl_pm_mnplan_order_update=>gv_lrmdt = gs_mhio-addat.
          ENDIF.
*}   REPLACE
        ENDIF.
* Calculate compliance date and update order structure
        caufvd-zzpmcompdate = zcl_pm_mnplan_order_update=>set_compliance_date( ).
        caufvd-gltrp = zcl_pm_mnplan_order_update=>set_basic_finish_date( ).

* Free Variables
        zcl_pm_mnplan_order_update=>free( ).

      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.
