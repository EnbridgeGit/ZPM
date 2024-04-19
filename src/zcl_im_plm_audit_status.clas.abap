class ZCL_IM_PLM_AUDIT_STATUS definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_STATUS
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_STATUS .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_STATUS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_STATUS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_STATUS IMPLEMENTATION.


method IF_EX_PLM_AUDIT_STATUS~CHECK_ACTIVITY.
endmethod.


METHOD if_ex_plm_audit_status~get_permitted_changes.

  DATA: lt_return   TYPE TABLE OF bapiret2,
        lv_isaction TYPE boolean,
        lv_guid     TYPE bapi_20370_d-guid.

  "Check if this is an action or not.
  lv_guid = iv_objnr.
  CALL FUNCTION 'BAPI_BUS20370_GET_DETAIL'
    EXPORTING
      guid   = lv_guid
    TABLES
      return = lt_return.

  IF lt_return IS INITIAL.
    "This is an action
    lv_isaction = abap_true.
  ELSE.
    lv_isaction = abap_false.
  ENDIF.


  "Check if the user has "DELETE" Authorization to audits, if so let them do the "special" actions
  AUTHORITY-CHECK OBJECT 'AUDIT_AUTH'
           ID 'AUD_AUTHGR' FIELD 'AUDITACTVT'
           ID 'AUDITACTVT' FIELD '3004'
           ID 'AUDIT_TYPE' DUMMY.

  IF sy-subrc <> 0.
    IF lv_isaction = abap_true.
      "Block Action Status
      DELETE ct_set_activities WHERE vrgng = 'AM12'       "Delete
                                       OR vrgng = 'AM21'  "Confirm
                                       OR vrgng = 'AM03'  "Complete
                                       OR vrgng = 'AM28'. "Force Completion
    ELSE.
      "Block Audit Status
      DELETE ct_set_activities WHERE vrgng = 'AM12'     "Delete
                                     OR vrgng = 'AM24'. "Complete with Outstanding
    ENDIF.
  ENDIF.
ENDMETHOD.


method IF_EX_PLM_AUDIT_STATUS~GET_UI_DATA.
endmethod.


method IF_EX_PLM_AUDIT_STATUS~IS_CHANGEABLE.
endmethod.


method IF_EX_PLM_AUDIT_STATUS~IS_USABLE.
endmethod.
ENDCLASS.
