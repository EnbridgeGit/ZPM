FUNCTION zpm_create_equip.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_EQUIP) TYPE  ZPMTT_EQUIP
*"  TABLES
*"      TBL_RETURN_TAB STRUCTURE  ZPMS_EQUIP_RET
*"----------------------------------------------------------------------
*--------------------------------------------------------------------*
*-- Change History
*--------------------------------------------------------------------*
* 01/30/2015 NR Following changes were done:
* 1. There should only be one equipment from each object type at a floc
* 2. Class and Catalog profile should be the same as Object Type
* 3. Description, Class, Catalog Profiles and Object type should never be changed
* 4. Characteristics should be updated only when they are valid and
*       should not return error if they are not valid.
* 2/12/2015 NR Remove the logic related to equipment installation / dismantling
*
* 2015/07/29 - Banner Equipment Interface Changes
* SDP85964
* GYMANA
*--------------------------------------------------------------------*


  DATA: lv_text TYPE char100,
        lv_lines1 TYPE i,
        lv_lines2 TYPE i,
        lv_lines3 TYPE i,
        lv_cr TYPE i,
        lv_er TYPE i,
        lv_ch TYPE i.
  DATA : lv_text2 TYPE string,
         lv_te TYPE string,
         lv_text1 TYPE string,
         lv_rundate TYPE sy-datum,
         lv_runtime TYPE sy-uzeit.
  DATA : lv_equip TYPE bapi_itob_parms-equipment,
         lv_floc  TYPE bapi_itob_parms-funcloc.

  DATA: ta_equip_ret TYPE STANDARD TABLE OF zpmt_equip_ret,
        wa_equip_ret LIKE LINE OF ta_equip_ret.

  DATA: ta_equip_log TYPE STANDARD TABLE OF zpmt_equip_log.


  FIELD-SYMBOLS: <fs_equip> LIKE LINE OF imp_equip,
                 <fs_vequi_fl> TYPE ty_vequi_fl,
                 <fs_return_tab> LIKE LINE OF tbl_return_tab.

*-- Set run date and time
  lv_rundate = sy-datum.
  lv_runtime = sy-uzeit.

  CLEAR: gv_loghandle.
  REFRESH: ta_loghandle.
*-- Get the parameter for logging
  SELECT SINGLE low
         FROM tvarvc
         INTO gv_applog_act
         WHERE name = 'Z_BANNERINT_APPLOG_ACT'.

  REFRESH :    tbl_return_tab[],
               ta_t499s[],
               ta_fl_level[],
               ta_iflot[],
               ta_iloa_e[],
               ta_final[],
               ta_vequi[].
  CLEAR :      lv_err_flg.
  CLEAR :      wa_final.

  IF  imp_equip[] IS NOT INITIAL.
*-- Delete duplicate records and count
    SORT imp_equip BY equnr.
    DESCRIBE TABLE imp_equip LINES lv_lines1.
    DELETE ADJACENT DUPLICATES FROM imp_equip COMPARING equnr.

    LOOP AT imp_equip INTO wa_equip.
      MOVE-CORRESPONDING wa_equip TO wa_equip_log.
      wa_equip_log-proc_status = '5'.
      APPEND wa_equip_log TO ta_equip_log.
      CLEAR wa_equip_log.
    ENDLOOP.
    DELETE FROM zpmt_equip_log.
    MODIFY zpmt_equip_log FROM TABLE ta_equip_log.
    REFRESH ta_equip_log.

    DESCRIBE TABLE imp_equip LINES lv_lines2.
    lv_text2 = lv_lines1.
    CONDENSE lv_text2. "total

    lv_lines3 = lv_lines1 - lv_lines2.
    lv_te = lv_lines3.
    CONDENSE lv_te.

    lv_text = lv_lines2.
    CONDENSE lv_text.
    CONCATENATE 'Total Records' lv_text2 'Duplicate Equipment:' lv_te 'No.of records to process' lv_text INTO lv_text SEPARATED BY space.
    PERFORM appl_log USING 'I' lv_text 'X'.
    CLEAR : lv_text2, lv_text, lv_te.


    PERFORM appl_log USING 'I' 'Begin GET_DATA' space.

*-- get all the data(Floca 5 thelevel )
    PERFORM get_data TABLES imp_equip tbl_return_tab
                     CHANGING lv_err_flg.
    IF lv_err_flg = 'X'.
      EXIT.
    ENDIF."if any error exit.

    PERFORM appl_log USING 'I' 'End GET_DATA' 'X'.

    CLEAR : lv_ch, lv_cr, lv_er.
*--------------------------------------------------------------------*
*-- Start processing of equipments for create / change
*--------------------------------------------------------------------*
    LOOP AT imp_equip INTO wa_equip.

*-- Update the log internal table
      CLEAR: wa_equip_log.
      MOVE-CORRESPONDING wa_equip TO wa_equip_log.
      wa_equip_log-zzdate = lv_rundate.
      wa_equip_log-zztime = lv_runtime.

      lv_text = sy-tabix.
      CONDENSE lv_text.
      CONCATENATE 'Processing Equipment:' wa_equip-equnr 'Record index:' lv_text  INTO lv_text SEPARATED BY space.

      PERFORM appl_log USING 'I' lv_text space.
*Refresh all the tables
      REFRESH : ta_installed_equi.

      CLEAR : wa_general,
              lv_err_flg,
              lv_text,
              wa_specific,
              wa_generalx,
              wa_specificx,
              lv_equip_created,
              wa_return_e,
              wa_vequi,
              wa_t499s,
              wa_iloa_e,
              wa_fl_level.

*check if equipment is blank, then move to next record
      IF wa_equip-equnr   IS INITIAL OR
         wa_equip-premise IS INITIAL OR
         wa_equip-stort   IS INITIAL.
        wa_ret_e-status = '2'.
        CONCATENATE 'Error:' 'Banner Equipment or Premises or Location is blank ' INTO lv_text.
        wa_ret_e-message = lv_text.
        APPEND wa_ret_e TO tbl_return_tab.
        lv_er = lv_er + 1." Error out
        wa_equip_log-proc_status = '3'.
        wa_equip_log-error = lv_text.
        APPEND wa_equip_log TO ta_equip_log.

        CLEAR : wa_ret_e,
                lv_text.
        CONTINUE.
      ENDIF.

*Read the data from all the tables t499s, iloa to fetch correct plant
      PERFORM read_maint_plant TABLES tbl_return_tab
                        CHANGING lv_err_flg.
      IF lv_err_flg = 'X'.
        lv_er = lv_er + 1." Error out
        wa_equip_log-proc_status = '3'.
        APPEND wa_equip_log TO ta_equip_log.
        CONTINUE.
      ENDIF.
*Check the Category and group code in Ztable for Equiopment text
      PERFORM read_equip_desc TABLES tbl_return_tab
                        CHANGING lv_err_flg.
      IF lv_err_flg = 'X'.
        lv_er = lv_er + 1." Error out
        wa_equip_log-proc_status = '3'.
        APPEND wa_equip_log TO ta_equip_log.
        CONTINUE.
      ENDIF.

*check Equipment in SAP system from current banner Equipment,
*its exist or need to create
*      READ TABLE ta_equi INTO wa_equi WITH KEY
      READ TABLE ta_vequi INTO wa_vequi WITH KEY
      equnr = wa_equip-equnr BINARY SEARCH.

*Banner Equipment not exist in SAP system, CREATE ACTION
      IF  wa_vequi-equnr IS INITIAL.
*** Unplugging Create Equipment functionality
*        PERFORM appl_log USING 'I' 'Begin CREATE_EQUIPMENT' space.
*-----------------------------Create Equipment--------------------------------------------
*        PERFORM create_equipment TABLES tbl_return_tab
*          CHANGING lv_err_flg.

*        PERFORM appl_log USING 'I' 'End CREATE_EQUIPMENT' 'X'.

*        IF lv_err_flg = 'X'.
*          lv_er = lv_er + 1." Error out
*          wa_equip_log-proc_status = '3'.
*          APPEND wa_equip_log TO ta_equip_log.
*          CONTINUE.
*        ELSE.
*          lv_cr = lv_cr + 1. "Created  success
*        ENDIF.
*** End of changes

*--- SDP85964 gymana BEGIN
        wa_equip_log-proc_status = '1'.
        CONCATENATE 'Equipment does not exist in SAP:'
                    wa_vequi-equnr
               INTO wa_equip_log-error SEPARATED BY space.
*--- SDP85964 gymana END

        APPEND wa_equip_log TO ta_equip_log.
      ELSE."Banner Equipment exist in SAP system, CHANGE ACTION
*-----------------------------Change Equipment--------------------------------------------

        PERFORM change_equip TABLES tbl_return_tab
                            CHANGING lv_err_flg.

        IF lv_err_flg = 'X'.
          lv_er = lv_er + 1." Error out
          wa_equip_log-proc_status = '3'.
          APPEND wa_equip_log TO ta_equip_log.
          CONTINUE.
        ENDIF.
        IF wa_equip_log-proc_status NE '4'.
          wa_equip_log-proc_status = '2'.
        ENDIF.

        APPEND wa_equip_log TO ta_equip_log.
      ENDIF.
      CLEAR: wa_vequi.

    ENDLOOP.

    lv_text2 = lv_cr.
    lv_text1 = gv_ch.
    lv_text = lv_er.
    lv_te = gv_no_ch.
    CONDENSE : lv_text2 , lv_text1, lv_text, lv_te.
    CONCATENATE "'Equipments Created:' lv_text2
                'Equipments actual Changed:' lv_text1
                'No Change Required' lv_te
                'Error Out:' lv_text INTO lv_text SEPARATED BY space.
    PERFORM appl_log USING 'I' lv_text 'X'.

    SORT tbl_return_tab.
    DELETE ADJACENT DUPLICATES FROM tbl_return_tab COMPARING ALL FIELDS.

*--------------------------------------------------------------------*
*Add result to custom table
*---------------------------------------------------------------------*
    LOOP AT tbl_return_tab ASSIGNING <fs_return_tab>.
      MOVE-CORRESPONDING <fs_return_tab> TO wa_equip_ret.
      wa_equip_ret-zzdate = sy-datum.
      wa_equip_ret-zztime = sy-uzeit.
      APPEND wa_equip_ret TO ta_equip_ret.
    ENDLOOP.
    MODIFY zpmt_equip_ret FROM TABLE ta_equip_ret.

    CLEAR: gv_loghandle.
    REFRESH: ta_loghandle.
  ENDIF.

  IF ta_equip_log IS NOT INITIAL.
*-- Delete all the existing entries
    DELETE FROM zpmt_equip_log.
*-- Insert the data into log table
    MODIFY zpmt_equip_log FROM TABLE ta_equip_log.
    PERFORM start_analysis_job.
  ENDIF.

ENDFUNCTION.
