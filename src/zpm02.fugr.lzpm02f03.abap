*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CREATE_CLASSIFICATION
*&---------------------------------------------------------------------*
*  Equipment Creation
*Routine called from include LZPM02F02
*----------------------------------------------------------------------*
*      -->P_LV_EQUIP_CREATED  text
*----------------------------------------------------------------------*
*Changes:
* 2015/07/29 GYMANA - Banner Equipment Interface changes
* SDP85964
*----------------------------------------------------------------------*
FORM create_classification
                  TABLES tbl_return_tab STRUCTURE zpms_equip_ret
                  USING  lv_equip_created
                         p_create TYPE c "Create scenario
                  CHANGING lv_err_flg
                         p_change_reqd TYPE c.

  DATA : ta_num TYPE STANDARD TABLE OF bapi1003_alloc_values_num,
         ta_char TYPE STANDARD TABLE OF bapi1003_alloc_values_char,
         ta_curr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr,
         ta_result TYPE STANDARD TABLE OF bapiret2,
        lv_atinn TYPE atinn,
        wa_num TYPE bapi1003_alloc_values_num,
        wa_char TYPE bapi1003_alloc_values_char,
        wa_curr TYPE bapi1003_alloc_values_curr,
        lv_obj  TYPE bapi1003_key-object,
        lv_class TYPE klasse_d,
        lv_pfactor_old(10) TYPE p DECIMALS 6,
        wa_result TYPE  bapiret2.

  DATA: lt_classchars TYPE STANDARD TABLE OF bapi1003_charact_r.

  FIELD-SYMBOLS: <fs_char> TYPE  bapi1003_alloc_values_char,
                 <fs_num> TYPE bapi1003_alloc_values_num.

*-- If equipment number or Technical object type is not available, return
  IF lv_equip_created IS INITIAL OR wa_equip-eqart IS INITIAL.
    RETURN.
  ENDIF.
  lv_obj = lv_equip_created.
  lv_class = wa_equip-eqart.

  CALL FUNCTION 'BAPI_CLASS_GETDETAIL'
    EXPORTING
      classtype            = '002'
      classnum             = lv_class
    TABLES
      classcharacteristics = lt_classchars.

  IF lt_classchars IS INITIAL.
    RETURN.
  ELSE.
    SORT lt_classchars BY name_char.
  ENDIF.

  IF p_create = abap_true.
*--------------------------------------------------------------------*
*-- Create scenario
*--------------------------------------------------------------------*
    READ TABLE lt_classchars WITH KEY name_char = 'P_FACTOR'
      BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0 .
      CLEAR : wa_num.
      wa_num-charact = 'P_FACTOR'.
      wa_num-value_from = wa_equip-p_factor.
      APPEND  wa_num TO ta_num.
      p_change_reqd = abap_true.
    ENDIF.

    READ TABLE lt_classchars WITH KEY name_char = 'ICAT_CODE'
      BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0 .
      IF wa_equip-icat_code IS NOT INITIAL.           "SDP85964
         CLEAR : wa_char.
         wa_char-charact = 'ICAT_CODE'.
         wa_char-value_char = wa_equip-icat_code.
         APPEND  wa_char TO ta_char.
         p_change_reqd = abap_true.
      ENDIF.                                          "SDP85964
    ENDIF.

    READ TABLE lt_classchars WITH KEY name_char = 'IGRP_CODE'
      BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0 .
      IF wa_equip-igrp_code IS NOT INITIAL.           "SDP85964
         CLEAR : wa_char.
         wa_char-charact = 'IGRP_CODE'.
         wa_char-value_char = wa_equip-igrp_code.
         APPEND  wa_char TO ta_char.
         p_change_reqd = abap_true.
      ENDIF.                                          "SDP85964
    ENDIF.

    READ TABLE lt_classchars WITH KEY name_char = 'GOVTSEAL_YEAR'
      BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0 .
      IF wa_equip-govtseal_year IS NOT INITIAL.       "SDP85964
         CLEAR : wa_char.
         wa_char-charact = 'GOVTSEAL_YEAR'.
         wa_char-value_char = wa_equip-govtseal_year.
         APPEND  wa_char TO ta_char.
         p_change_reqd = abap_true.
      ENDIF.                                          "SDP85964
    ENDIF.

    READ TABLE lt_classchars WITH KEY name_char = 'DRAT_CODE'
    BINARY SEARCH TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0 .
      IF wa_equip-drat_code IS NOT INITIAL.           "SDP85964
         CLEAR : wa_num.
         wa_num-charact = 'DRAT_CODE'.
         wa_num-value_from = wa_equip-drat_code.
         APPEND  wa_num TO ta_num.
         p_change_reqd = abap_true.
      ENDIF.                                          "SDP85964
    ENDIF.
  ELSE.
*--------------------------------------------------------------------*
*-- Change scenario
*--------------------------------------------------------------------*
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_obj
        objecttable     = 'EQUI'
        classnum        = lv_class
        classtype       = '002'
      TABLES
        allocvaluesnum  = ta_num
        allocvalueschar = ta_char
        allocvaluescurr = ta_curr
        return          = ta_result.

    READ TABLE ta_num ASSIGNING <fs_num> WITH KEY charact = 'P_FACTOR'.
    IF sy-subrc EQ 0.
*-- set the old pfactor value in equipment log
      IF <fs_num>-value_from NE wa_equip-p_factor.
        MOVE <fs_num>-value_from TO lv_pfactor_old.
        wa_equip_log-p_factor_old = lv_pfactor_old.
        p_change_reqd = abap_true.
        <fs_num>-value_from = wa_equip-p_factor.
      ELSE.
        wa_equip_log-p_factor_old = wa_equip-p_factor.
      ENDIF.
    ELSE.
      READ TABLE lt_classchars WITH KEY name_char = 'P_FACTOR'
        BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0 .
        IF wa_equip-p_factor IS NOT INITIAL.          "SDP85964
          CLEAR : wa_num.
          wa_num-charact = 'P_FACTOR'.
          wa_num-value_from = wa_equip-p_factor.
*-- set the old pfactor value in equipment log
          CLEAR wa_equip_log-p_factor_old.
          APPEND  wa_num TO ta_num.
          p_change_reqd = abap_true.
        ENDIF.                                        "SDP85964
      ENDIF.
    ENDIF.

    READ TABLE ta_char ASSIGNING <fs_char> WITH KEY charact = 'ICAT_CODE'.
    IF sy-subrc EQ 0.
      IF <fs_char>-value_char NE wa_equip-icat_code.
        p_change_reqd = abap_true.
        <fs_char>-value_char = wa_equip-icat_code.
        <fs_char>-value_neutral = wa_equip-icat_code.
      ENDIF.
    ELSE.
      READ TABLE lt_classchars WITH KEY name_char = 'ICAT_CODE'
        BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0 .
        IF wa_equip-icat_code IS NOT INITIAL.         "SDP85964
          CLEAR : wa_char.
          wa_char-charact = 'ICAT_CODE'.
          wa_char-value_neutral = wa_equip-icat_code.
          wa_char-value_char = wa_equip-icat_code.
          APPEND  wa_char TO ta_char.
          p_change_reqd = abap_true.
        ENDIF.                                        "SDP85964
      ENDIF.
    ENDIF.

    READ TABLE ta_char ASSIGNING <fs_char> WITH KEY charact = 'IGRP_CODE'.
    IF sy-subrc EQ 0.
      IF <fs_char>-value_char NE wa_equip-igrp_code.
        p_change_reqd = abap_true.
        <fs_char>-value_neutral = wa_equip-igrp_code.
        <fs_char>-value_char = wa_equip-igrp_code.
      ENDIF.
    ELSE.
      READ TABLE lt_classchars WITH KEY name_char = 'IGRP_CODE'
        BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0 .
        IF wa_equip-igrp_code IS NOT INITIAL.         "SDP85964
          CLEAR : wa_char.
          wa_char-charact = 'IGRP_CODE'.
          wa_char-value_neutral = wa_equip-igrp_code.
          wa_char-value_char = wa_equip-igrp_code.
          APPEND  wa_char TO ta_char.
          p_change_reqd = abap_true.
        ENDIF.                                        "SDP85964
      ENDIF.
    ENDIF.

    READ TABLE ta_char ASSIGNING <fs_char> WITH KEY charact = 'GOVTSEAL_YEAR'.
    IF sy-subrc EQ 0.
      IF <fs_char>-value_char NE wa_equip-govtseal_year.
        p_change_reqd = abap_true.
        <fs_char>-value_char =  wa_equip-govtseal_year.
        <fs_char>-value_neutral =  wa_equip-govtseal_year.
      ENDIF.
    ELSE.
      READ TABLE lt_classchars WITH KEY name_char = 'GOVTSEAL_YEAR'
        BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0 .
        IF wa_equip-govtseal_year IS NOT INITIAL.      "SDP85964
          CLEAR : wa_char.
          wa_char-charact = 'GOVTSEAL_YEAR'.
          wa_char-value_neutral = wa_equip-govtseal_year.
          wa_char-value_char = wa_equip-govtseal_year.
          APPEND  wa_char TO ta_char.
          p_change_reqd = abap_true.
        ENDIF.                                        "SDP85964
      ENDIF.
    ENDIF.

    READ TABLE ta_num ASSIGNING <fs_num> WITH KEY charact = 'DRAT_CODE'.
    IF sy-subrc EQ 0.
      IF <fs_num>-value_from NE wa_equip-drat_code.
        p_change_reqd = abap_true.
        <fs_num>-value_from = wa_equip-drat_code.
      ENDIF.
    ELSE.
      READ TABLE lt_classchars WITH KEY name_char = 'DRAT_CODE'
      BINARY SEARCH TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0 .
        IF wa_equip-drat_code IS NOT INITIAL.         "SDP85964
          CLEAR : wa_num.
          wa_num-charact = 'DRAT_CODE'.
          wa_num-value_from = wa_equip-drat_code.
          APPEND  wa_num TO ta_num.
          p_change_reqd = abap_true.
        ENDIF.                                        "SDP85964
      ENDIF.
    ENDIF.
  ENDIF.

*-- If there is no change, just return
  IF p_change_reqd EQ abap_false.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_OBJCL_CHANGE'
    EXPORTING
      objectkey          = lv_obj
      objecttable        = 'EQUI'
      classnum           = lv_class"'MD_MT'
      classtype          = '002'
      status             = '1'
    TABLES
      allocvaluesnumnew  = ta_num
      allocvaluescharnew = ta_char
      allocvaluescurrnew = ta_curr
      return             = ta_result.
  READ TABLE ta_result INTO wa_result WITH KEY type = 'E'.
  IF sy-subrc IS INITIAL.
    lv_err_flg = 'X'.
    CLEAR : lv_text.
    wa_ret_e-equipment = lv_external_no.
    wa_ret_e-status = '2'.
    CONCATENATE 'Error-' 'Assignment of char value to Class ' lv_class 'failed' INTO lv_text SEPARATED BY space.
    wa_ret_e-message = lv_text.
    wa_equip_log-error = lv_text.
    APPEND wa_ret_e TO tbl_return_tab.
    CLEAR : wa_ret_e,
            lv_text.
  ELSE.
    PERFORM commit USING 'X'.
    wa_ret_e-equipment = lv_external_no.
    wa_ret_e-status = '1'.
    CONCATENATE 'Success-' 'Equipment Classification changed:' lv_external_no  INTO lv_text SEPARATED BY space .
    wa_ret_e-message = lv_text.
*    wa_equip_log-error = lv_text.                 "SDP85964
    CLEAR : lv_text.
    APPEND wa_ret_e TO tbl_return_tab.
    CLEAR : wa_ret_e.
  ENDIF.


*  ENDIF.
ENDFORM.                    " CREATE_CLASSIFICATION
*&---------------------------------------------------------------------*
*&      Form  COMMIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM commit USING p_wait TYPE bapita-wait.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = p_wait.

ENDFORM.                    " COMMIT
*&---------------------------------------------------------------------*
*&      Form  APPL_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM appl_log USING p_msgty TYPE sy-msgty
                    p_text  TYPE char100
                    p_commit TYPE c.

  DATA:  ls_log   TYPE bal_s_log,
         lv_text  TYPE char100.

  IF gv_applog_act IS INITIAL.
    RETURN.
  ENDIF.

  IF gv_loghandle IS INITIAL.
* * open log
    ls_log-object    = 'ZPM '.
    ls_log-subobject = 'ZBANINT'.
    ls_log-aluser    = sy-uname.
    ls_log-alprog    = sy-repid.

    CALL FUNCTION 'BAL_LOG_CREATE'                          "#EC *
    EXPORTING
      i_s_log      = ls_log
    IMPORTING
        e_log_handle = gv_loghandle
    EXCEPTIONS
       OTHERS       = 1.
    INSERT gv_loghandle INTO TABLE ta_loghandle.
  ENDIF.

  CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
    EXPORTING
      i_log_handle     = gv_loghandle
      i_msgty          = p_msgty
      i_text           = p_text
    EXCEPTIONS
      log_not_found    = 1
      msg_inconsistent = 2
      log_is_full      = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF p_commit EQ 'X'.


    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = ta_loghandle
      EXCEPTIONS
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.

    ENDIF.
  ENDIF.

ENDFORM.                    " APPL_LOG
*&---------------------------------------------------------------------*
*&      Form  START_ANALYSIS_JOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM start_analysis_job .
  CONSTANTS: lc_rprtname(35) TYPE c VALUE 'Z_PM_BANNER_INTERFACE_ANALYSIS'.

  DATA: lv_job_name TYPE tbtcjob-jobname,
        lv_jobid TYPE tbtcjob-jobcount,
        lv_release(1) TYPE c,
        lt_rspar  TYPE TABLE OF rsparams,
        lw_rspar TYPE rsparams,
        lv_text TYPE char100.

*-- Fill job informations
  CONCATENATE lc_rprtname sy-mandt INTO  lv_job_name.

*-- To be used in future
  "Fill the selection parameters
*  lw_rspar-selname = 'XXXxxxxx'.
*  lw_rspar-kind    = 'P'.
*  lw_rspar-sign    = 'I'.
*  lw_rspar-option  = 'EQ'.
*  lw_rspar-low     = space.
*  APPEND lw_rspar TO lt_rspar.
*  CLEAR lw_rspar.

*-- Open the job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_job_name
    IMPORTING
      jobcount         = lv_jobid
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc  = 0.
*-- Submit the Job and return
    SUBMIT z_pm_banner_interface_analysis
    WITH SELECTION-TABLE lt_rspar
    USER syst-uname
    VIA JOB lv_job_name NUMBER lv_jobid AND RETURN.

    IF sy-subrc = 0.
*-- Start the job immediately
      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = lv_jobid
          jobname              = lv_job_name
          strtimmed            = abap_true
        IMPORTING
          job_was_released     = lv_release
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.
      IF sy-subrc <> 0.
        CONCATENATE 'Failed to close the analysis job ' lv_job_name INTO lv_text RESPECTING BLANKS.
        PERFORM appl_log USING 'I' lv_text 'X'.
      ENDIF.
    ELSE.
      CONCATENATE'Failed to submit the analysis report ' lc_rprtname INTO lv_text RESPECTING BLANKS.
      PERFORM appl_log USING 'I' lv_text 'X'.
    ENDIF.
  ELSE.
    CONCATENATE 'Failed to open the analysis job' lv_job_name INTO lv_text RESPECTING BLANKS.
    PERFORM appl_log USING 'I' lv_text 'X'.
  ENDIF.

ENDFORM.                    " START_ANALYSIS_JOB
