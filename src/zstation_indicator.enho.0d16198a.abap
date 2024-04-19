"Name: \TY:/SMERP/CL_PM_EQUIPMENT_DO\IN:/SYCLO/IF_CORE_DO_HANDLER\ME:UPDATE\SE:END\EI
ENHANCEMENT 0 ZSTATION_INDICATOR.
* SDP85964 - G.Ymana 2015/07/27
* Added this enhancement to update Station Capacity Indicator flag after an Equipment change
* in Work Manager.
* ACR247 - G. Ymana 2016/02/08
* Added changes to only update the indicator if the equipment is engineering relevant

  TRY .
    DATA: lv_tplkz TYPE tplkz, "Structure indicator
          lv_funcloc TYPE tplnr,
          lv_cuobj TYPE cuobj, "Internal number            ACR247
          lv_count(3) TYPE N,  "                           ACR247
          lv_clint_c1 TYPE clint value 2591,              "ACR247
          lv_clint_c2 TYPE clint value 2590.              "ACR247

    IF NOT ( ls_mdo_input_vals-is_data_general->* IS INITIAL AND
             ls_mdo_input_vals-is_data_specific->* IS INITIAL ).

*-- Find the Functional Location

      SELECT b~tplnr into lv_funcloc
        FROM EQUZ as a INNER JOIN ILOA as b
          ON a~iloan = b~iloan
       WHERE a~equnr = ls_mdo_input_vals-iv_equipment->*.
      ENDSELECT.

      IF lv_funcloc IS INITIAL.                          "ACR247
         RETURN.                                         "ACR247
      ENDIF.                                             "ACR247

*-- Get the structure indicator, if it is initial.

      SELECT SINGLE tplkz FROM iflot INTO lv_tplkz
       WHERE tplnr = lv_funcloc.

*-- ACR247 GYMANA start
*-- Validate that the equipment is engineering relevant
      SELECT cuobj FROM inob into lv_cuobj
       WHERE objek = lv_funcloc
         AND klart = '003'.

       IF sy-subrc EQ 0.
          SELECT COUNT( * ) FROM kssk into lv_count
           WHERE objek = lv_cuobj
             AND klart = '003'
             AND ( clint = lv_clint_c1 OR
                   clint = lv_clint_c2 ).
       ENDIF.
       ENDSELECT.
*-- ACR247 GYMANA end

*-- Call method to update design capacity status indicator
       IF lv_count > 0.                                     "ACR247
          zpm_cl_funcloc_util=>handle_equi_loc_change(
            iv_equnr = ls_mdo_input_vals-iv_equipment->*    "Equip.no
            iv_tplnr = lv_funcloc                     "Func. Location
            iv_tplkz = lv_tplkz ).               "FuncLoc Struct. Ind
       ENDIF.                                               "ACR247
    ENDIF.

  CATCH cx_root INTO lref_exception.
      /syclo/cl_core_appl_logger=>logger->catch_class_exception(
        EXPORTING is_bapi_input = me->str_bapi_input
                  iref_exception = lref_exception
                  iref_return_tab = iref_rfc_oo_data->dref_return ).
  ENDTRY.
ENDENHANCEMENT.
