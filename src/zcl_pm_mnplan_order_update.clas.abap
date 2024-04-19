class ZCL_PM_MNPLAN_ORDER_UPDATE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_PM_MNPLAN_ORDER_UPDATE
*"* do not include other source files here!!!

  class-data GV_CYCLE_VALUE type CHAR22 .
  class-data GV_MAN_CALL type CHAR1 .
  class-data GV_CTR_CALL type CHAR1 .
  class-data GV_ZEIEH type DZEIEH .
  class-data GV_LRMDT type LRMDT .
  class-data GV_GSTRP type CAUFVD-GSTRP .

  class-methods SET_COMPLIANCE_DATE
    returning
      value(RT_COMPDATE) type ZPMCOMPDATE .
  class-methods SET_BASIC_FINISH_DATE
    returning
      value(RT_GLTRP) type CAUFVD-GLTRP .
  class-methods FREE .
protected section.
*"* protected components of class ZCL_PM_MNPLAN_ORDER_UPDATE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_PM_MNPLAN_ORDER_UPDATE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_PM_MNPLAN_ORDER_UPDATE IMPLEMENTATION.


METHOD free.
  break nikamt.

  CLEAR: gv_cycle_value,
         gv_man_call,
         gv_ctr_call,
         gv_zeieh,
         gv_lrmdt,
         gv_gstrp.
ENDMETHOD.


METHOD set_basic_finish_date.

  DATA:lv_date_in     TYPE sy-datum,
       lv_date_out_2  TYPE sy-datum,
       lv_date_out    TYPE sy-datum.

  CHECK gv_gstrp IS NOT INITIAL AND
        gv_zeieh IS NOT INITIAL AND
        gv_cycle_value IS NOT INITIAL.

  CASE gv_zeieh.
    WHEN 'MON' OR 'JHR'.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = gv_gstrp
        IMPORTING
          last_day_of_month = lv_date_out
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.

      IF sy-subrc NE 0.
        RETURN.
      ENDIF.

      rt_gltrp = lv_date_out.

    WHEN 'WCH'.

      rt_gltrp = gv_gstrp + 6.

  ENDCASE.


ENDMETHOD.


METHOD set_compliance_date.
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 17-JUN-2020  KBANERJEE   D30K930579  CHG0181240_ENHC002868:New       *
*                                      maintenance plan sort field T-4Y*
*                                                                      *
************************************************************************
* 10-FEB-2022  DADIM       D30K932010  CHG0241015_ENHC0036408:New      *
*                                      maintenance plan sort field T-7Y*                                                                    *
************************************************************************
* 27-FEB-2024  NIKAMT      D30K932724  RLSE0024238:New      *
*                                      maintenance plan sort field T-6Y
*                                                                 & T-8Y*
************************************************************************




  DATA:lv_date_in     TYPE sy-datum,
       lv_date_out_2  TYPE sy-datum,
       lv_date_out    TYPE sy-datum.
  CHECK gv_gstrp IS NOT INITIAL AND
        gv_zeieh IS NOT INITIAL AND
        gv_cycle_value IS NOT INITIAL.


  IF NOT ( gv_ctr_call IS INITIAL AND gv_man_call IS INITIAL ).
    RETURN.
  ENDIF.

  CASE gv_zeieh.
    WHEN 'MON'.
      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = gv_gstrp
        IMPORTING
          last_day_of_month = lv_date_out
        EXCEPTIONS
          day_in_no_date    = 1
          OTHERS            = 2.

      IF sy-subrc NE 0.
        RETURN.
      ENDIF.

      CASE gv_cycle_value.
        WHEN '1'.
          rt_compdate = lv_date_out.

        WHEN '3'.
          IF gv_gstrp+4(2) EQ '01' OR
             gv_gstrp+4(2) EQ '02' OR
             gv_gstrp+4(2) EQ '03'.
            CONCATENATE gv_gstrp+0(4) '0331' INTO rt_compdate.
          ELSEIF gv_gstrp+4(2) EQ '04' OR
                 gv_gstrp+4(2) EQ '05' OR
                 gv_gstrp+4(2) EQ '06'.
            CONCATENATE gv_gstrp+0(4) '0630' INTO rt_compdate.
          ELSEIF gv_gstrp+4(2) EQ '07' OR
                 gv_gstrp+4(2) EQ '08' OR
                 gv_gstrp+4(2) EQ '09'.
            CONCATENATE gv_gstrp+0(4) '0930' INTO rt_compdate.
          ELSE.
            CONCATENATE gv_gstrp+0(4) '1231' INTO rt_compdate.
          ENDIF.
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0181240_ENHC002868
        WHEN '4'.
          IF gv_gstrp+4(2) EQ '01' OR
             gv_gstrp+4(2) EQ '02' OR
             gv_gstrp+4(2) EQ '03' OR
             gv_gstrp+4(2) EQ '04'.
            CONCATENATE gv_gstrp+0(4) '0430' INTO rt_compdate.
          ELSEIF gv_gstrp+4(2) EQ '05' OR
                 gv_gstrp+4(2) EQ '06' OR
                 gv_gstrp+4(2) EQ '07' OR
                 gv_gstrp+4(2) EQ '08'.
            CONCATENATE gv_gstrp+0(4) '0831' INTO rt_compdate.
          ELSEIF gv_gstrp+4(2) EQ '09' OR
                 gv_gstrp+4(2) EQ '10' OR
                 gv_gstrp+4(2) EQ '11' OR
                 gv_gstrp+4(2) EQ '12'.
            CONCATENATE gv_gstrp+0(4) '1231' INTO rt_compdate.
          ELSE.
            CONCATENATE gv_gstrp+0(4) '1231' INTO rt_compdate.
          ENDIF.
*END OF CHANGES BY KBANERJEE FOR CHG0181240_ENHC002868
        WHEN '6'.

          IF gv_gstrp+4(2) EQ '01' OR
             gv_gstrp+4(2) EQ '02' OR
             gv_gstrp+4(2) EQ '03' OR
             gv_gstrp+4(2) EQ '04' OR
             gv_gstrp+4(2) EQ '05' OR
             gv_gstrp+4(2) EQ '06'.

            CONCATENATE gv_gstrp+0(4) '0630' INTO rt_compdate.

          ELSE.
            CONCATENATE gv_gstrp+0(4) '1231' INTO rt_compdate.
          ENDIF.

        WHEN '12'.
          CONCATENATE gv_gstrp+0(4) '1231' INTO lv_date_out.
          IF gv_lrmdt IS NOT INITIAL.

            CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
              EXPORTING
                months  = 18
                olddate = gv_lrmdt
              IMPORTING
                newdate = lv_date_out_2.

            IF lv_date_out_2 LT lv_date_out.
              lv_date_out = lv_date_out_2.
            ENDIF.
          ENDIF.
          rt_compdate = lv_date_out.

        WHEN '24' OR '36' OR '60' OR '120'
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0181240_ENHC002868
          OR '48'
*END OF CHANGES BY KBANERJEE FOR CHG0181240_ENHC002868
*Start of change by DADIM for CHG0241015
          OR '84'
*End of change by DADIM for CHG0241015

*BEGIN OF CHANGES BY NIKAMT FOR D30K932724
          OR '72' OR '96'. " Adding for maintenance plan sort field T-6Y
*                                                                 & T-8Y in months*
*END OF CHANGES BY NIKAMT FOR D30K932724

          CONCATENATE gv_gstrp+0(4) '1231' INTO rt_compdate.
      ENDCASE.
    WHEN 'WCH'.

      rt_compdate = gv_gstrp + 6.

    WHEN 'JHR'.
      CASE gv_cycle_value.
        WHEN '1'.

          CONCATENATE gv_gstrp+0(4) '1231' INTO lv_date_out.
          IF gv_lrmdt IS NOT INITIAL.

            CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
              EXPORTING
                months  = 18
                olddate = gv_lrmdt
              IMPORTING
                newdate = lv_date_out_2.

            IF lv_date_out_2 LT lv_date_out.
              lv_date_out = lv_date_out_2.
            ENDIF.
          ENDIF.
          rt_compdate = lv_date_out.

        WHEN '2' OR '3' OR '5' OR '10'
*BEGIN OF CHANGES BY KBANERJEE FOR CHG0181240_ENHC002868
          OR '4'
*END OF CHANGES BY KBANERJEE FOR CHG0181240_ENHC002868
*Start of change by DADIM for CHG0241015
          OR '7'
*End of change by DADIM for CHG0241015

       OR '6'  OR '8'.  " <== Insert D30K932724

          CONCATENATE gv_gstrp+0(4) '1231' INTO rt_compdate.

      ENDCASE.
  ENDCASE.
ENDMETHOD.
ENDCLASS.
