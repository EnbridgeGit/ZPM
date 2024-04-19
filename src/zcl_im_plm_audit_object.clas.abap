class ZCL_IM_PLM_AUDIT_OBJECT definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_OBJECT
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_OBJECT .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_OBJECT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_OBJECT
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_OBJECT IMPLEMENTATION.


METHOD if_ex_plm_audit_object~check.
  DATA: zaudit_object63 TYPE REF TO cl_im_plm_audit_object_63.

  "Call Standard Logic
  CREATE OBJECT zaudit_object63.
  CALL METHOD zaudit_object63->if_ex_plm_audit_object~check
    EXPORTING
      flt_val           = flt_val
      io_audit          = io_audit
      iv_audit_type     = iv_audit_type
      it_plmm_audit_obj = it_plmm_audit_obj
      io_project        = io_project
    IMPORTING
      ev_result         = ev_result.
ENDMETHOD.


method IF_EX_PLM_AUDIT_OBJECT~CHECK_SINGLE.
    DATA: zaudit_object63 TYPE REF TO cl_im_plm_audit_object_63.

  "Call Standard Logic
  CREATE OBJECT zaudit_object63.
  CALL METHOD zaudit_object63->if_ex_plm_audit_object~check_single
    EXPORTING
      flt_val           = flt_val
      io_audit          = io_audit
      iv_audit_type     = iv_audit_type
      is_plmm_audit_obj = is_plmm_audit_obj
      io_project        = io_project
    IMPORTING
      ev_result         = ev_result.

endmethod.


method IF_EX_PLM_AUDIT_OBJECT~DISPLAY.
    DATA: zaudit_object63 TYPE REF TO cl_im_plm_audit_object_63.

  "Call Standard Logic
  CREATE OBJECT zaudit_object63.
  CALL METHOD zaudit_object63->if_ex_plm_audit_object~display
    EXPORTING
      flt_val                 = flt_val
      iv_audit_type           = iv_audit_type
      iv_object_type          = iv_object_type
      iv_object_value         = iv_object_value
      iv_check_object_display = iv_check_object_display
    IMPORTING
      ev_object_display       = ev_object_display.

endmethod.


METHOD if_ex_plm_audit_object~get_value_short_texts.
  DATA: zaudit_object63 TYPE REF TO cl_im_plm_audit_object_63.

  "Call Standard Logic
  CREATE OBJECT zaudit_object63.
  CALL METHOD zaudit_object63->if_ex_plm_audit_object~get_value_short_texts
    EXPORTING
      flt_val                 = flt_val
      io_audit                = io_audit
      iv_audit_type           = iv_audit_type
      it_audit_obj_cust       = it_audit_obj_cust
      io_project              = io_project
    CHANGING
      ct_plmt_audit_object_ui = ct_plmt_audit_object_ui.


  DATA: ls_audit_object_ui TYPE LINE OF plmt_audit_object_ui_tab,
        ls_audit_obj_cust  TYPE LINE OF plmt_audit_obj_tab,
        lv_username        TYPE sy-uname,
        lv_orgid           TYPE zplm_org-orgid,
        lv_orgname         TYPE zplm_org-orgname,
        lv_qapartner       TYPE zplm_qapartner-qa_partner,
        lv_qaemail         TYPE zplm_qapartner-email,
        ls_address         TYPE bapiaddr3,
        lt_return          TYPE TABLE OF bapiret2.

  "Populate any values that are missing.
  LOOP AT ct_plmt_audit_object_ui INTO ls_audit_object_ui
                                 WHERE value_text IS INITIAL "Text is blank
                                   AND object_value IS NOT INITIAL."Value is populated
    CLEAR: ls_audit_obj_cust.
    "Get the value table of the object from the internal table it_audit_obj_cust
    READ TABLE it_audit_obj_cust INTO ls_audit_obj_cust WITH KEY
                    object_type = ls_audit_object_ui-object_type.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
    CASE ls_audit_obj_cust-data_element. "DATA_ELEMENT. "value_tab_name.
      WHEN 'XUBNAME'.   "UserName
        lv_username = ls_audit_object_ui-object_value.
        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            username      = lv_username
            cache_results = 'X'
          IMPORTING
            address       = ls_address
          TABLES
            return        = lt_return.
        IF lt_return IS INITIAL.
          ls_audit_object_ui-value_text = ls_address-fullname.
        ENDIF.
      WHEN 'Z_ORGID'.
        lv_orgid = ls_audit_object_ui-object_value.
        SELECT SINGLE orgname INTO lv_orgname FROM zplm_org WHERE orgid = lv_orgid.
        IF sy-subrc = 0.
          ls_audit_object_ui-value_text = lv_orgname.
        ELSE.
          CLEAR ls_audit_object_ui-value_text.
        ENDIF.
      WHEN 'Z_QAPARTNER'.
        lv_qapartner = ls_audit_object_ui-object_value.
        SELECT SINGLE email INTO lv_qaemail FROM zplm_qapartner WHERE qa_partner = lv_qapartner.
        IF sy-subrc = 0.
          ls_audit_object_ui-value_text = lv_qaemail.
        ELSE.
          CLEAR ls_audit_object_ui-value_text.
        ENDIF.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.
    MODIFY ct_plmt_audit_object_ui FROM ls_audit_object_ui
                                   TRANSPORTING value_text.
  ENDLOOP.
ENDMETHOD.


METHOD if_ex_plm_audit_object~map_objects_to_ui_struc.
  DATA: zaudit_object63 TYPE REF TO cl_im_plm_audit_object_63.

  "Call Standard Logic
  CREATE OBJECT zaudit_object63.
  CALL METHOD zaudit_object63->if_ex_plm_audit_object~map_objects_to_ui_struc
    EXPORTING
      flt_val           = flt_val
      io_audit          = io_audit
      iv_audit_type     = iv_audit_type
      it_audit_obj_cust = it_audit_obj_cust
      it_audit_obj_ui   = it_audit_obj_ui
      io_project        = io_project
    CHANGING
      cs_plmt_audit_ui  = cs_plmt_audit_ui.

ENDMETHOD.


method IF_EX_PLM_AUDIT_OBJECT~PAI_CONVERT_EXT_TO_INT.
  DATA: zaudit_object63 TYPE REF TO cl_im_plm_audit_object_63.

  "Call Standard Logic
  CREATE OBJECT zaudit_object63.
  CALL METHOD zaudit_object63->if_ex_plm_audit_object~pai_convert_ext_to_int
    EXPORTING
      flt_val           = flt_val
      io_audit          = io_audit
      iv_audit_type     = iv_audit_type
      it_audit_obj_cust = it_audit_obj_cust
      it_audit_obj_ui   = it_audit_obj_ui
      io_project        = io_project
    CHANGING
      cs_audit_obj_ui   = cs_audit_obj_ui.

endmethod.


METHOD if_ex_plm_audit_object~value_help.
  DATA: zaudit_object63 TYPE REF TO cl_im_plm_audit_object_63.

  "Call Standard Logic
  CREATE OBJECT zaudit_object63.
  CALL METHOD zaudit_object63->if_ex_plm_audit_object~value_help
    EXPORTING
      flt_val                = flt_val
      io_audit               = io_audit
      iv_audit_type          = iv_audit_type
      iv_object_type         = iv_object_type
      it_audited_objects_ref = it_audited_objects_ref
      iv_object_value        = iv_object_value
      io_project             = io_project
    CHANGING
      p_record_tab           = p_record_tab
      p_shlp_tab             = p_shlp_tab
      p_shlp                 = p_shlp
      p_callcontrol          = p_callcontrol.


ENDMETHOD.
ENDCLASS.
