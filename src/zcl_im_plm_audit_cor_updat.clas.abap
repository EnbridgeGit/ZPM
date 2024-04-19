class ZCL_IM_PLM_AUDIT_COR_UPDAT definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_COR_UPDAT
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_COR_UPDATE .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_COR_UPDAT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_COR_UPDAT
*"* do not include other source files here!!!

  methods ON_SAVE
    importing
      value(IR_ACTION) type ref to CL_CGPL_PROJECT
      value(IR_ACTION_ATTR_ASSIGNMENT) type ref to CL_PLM_AUDIT_ACT_ASSIGNMENT .
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_COR_UPDAT IMPLEMENTATION.


METHOD if_ex_plm_audit_cor_update~change_action_at_save.

  CALL METHOD me->on_save
    EXPORTING
      ir_action                 = ir_action
      ir_action_attr_assignment = ir_action_attr_assignment.

ENDMETHOD.


METHOD if_ex_plm_audit_cor_update~change_action_before_update.
ENDMETHOD.


method IF_EX_PLM_AUDIT_COR_UPDATE~CHANGE_ACTION_IN_UPDATE.
endmethod.


METHOD if_ex_plm_audit_cor_update~create_action_at_save.

CALL METHOD me->on_save
  EXPORTING
    ir_action = ir_action
    ir_action_attr_assignment = ir_action_attr_assignment.

ENDMETHOD.


method IF_EX_PLM_AUDIT_COR_UPDATE~CREATE_ACTION_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_COR_UPDATE~CREATE_ACTION_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_COR_UPDATE~DELETE_ACTION_AT_SAVE.
  DELETE FROM zaudit_notif WHERE action_guid = is_action-guid.
endmethod.


method IF_EX_PLM_AUDIT_COR_UPDATE~DELETE_ACTION_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_COR_UPDATE~DELETE_ACTION_IN_UPDATE.
endmethod.


METHOD on_save.
  DATA: ls_actionparams   TYPE cgpl_project,
        ls_action         TYPE plmm_audit_act,
        ls_actionui       TYPE plmt_audit_act_ui,
        lv_ftoday         TYPE facdate,
        lv_ftemp          TYPE facdate,
        lv_date_temp      TYPE sy-datum,
        lv_date_string    TYPE string,
        lv_updates        TYPE boolean,

        ls_audit_notif    TYPE zaudit_notif,
        lv_fstart         TYPE facdate,
        lv_astart         TYPE facdate, "(+)PANUSURI Ticket ACR-4915
        lv_ffinish        TYPE facdate.

  CONSTANTS:  c_factorycalendar TYPE scal-fcalid  VALUE 'UN',
              c_timestamp(6)    TYPE c            VALUE '120000'.

  "Get the ui structure for status code
  CALL METHOD ir_action_attr_assignment->get_attributes
    IMPORTING
      es_attributes = ls_action.
  CALL METHOD ir_action_attr_assignment->get_ui_structure
    IMPORTING
      es_external_structure = ls_actionui.

  "Get todays date in the factory calendar
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      date                = sy-datum
      factory_calendar_id = c_factorycalendar
    IMPORTING
      factorydate         = lv_ftoday.

  "Get the current attributes for the action
  CALL METHOD ir_action->get_attributes
    IMPORTING
      ex_attributes = ls_actionparams.

  IF ls_actionui-u_status CS 'NOTI'.
    """""" Save the Auditor """"""
    IF ls_action-z_leadauditor <> sy-uname.
      ls_action-z_leadauditor = sy-uname.
      lv_updates = abap_true.
    ENDIF.

    """""" Default Dates """"""
    IF ls_actionparams-planstart IS INITIAL.
      CLEAR: lv_date_temp, lv_date_string.
      lv_ftemp = lv_ftoday + 20.
      CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
        EXPORTING
          factorydate         = lv_ftemp
          factory_calendar_id = c_factorycalendar
        IMPORTING
          date                = lv_date_temp.
      IF sy-subrc = '0'.
        CONCATENATE lv_date_temp c_timestamp INTO lv_date_string.
        ls_actionparams-planstart = lv_date_string.
        lv_updates = abap_true.
      ENDIF.
    ENDIF.

    IF ls_actionparams-planfinish IS INITIAL.
      CLEAR: lv_date_temp, lv_date_string.
      lv_ftemp = lv_ftoday + 60.
      CALL FUNCTION 'FACTORYDATE_CONVERT_TO_DATE'
        EXPORTING
          factorydate         = lv_ftemp
          factory_calendar_id = c_factorycalendar
        IMPORTING
          date                = lv_date_temp.
      IF sy-subrc = '0'.
        CONCATENATE lv_date_temp c_timestamp INTO lv_date_string.
        ls_actionparams-planfinish = lv_date_string.
        lv_updates = abap_true.
      ENDIF.
    ENDIF.



    """""" Add to notification table """"""
    "Get the factory calendar version of the start and finish dates
    lv_date_string = ls_actionparams-planstart.
    lv_date_string = lv_date_string(8).
    lv_date_temp = lv_date_string.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
        date                = lv_date_temp
        factory_calendar_id = c_factorycalendar
        correct_option      = '-' "Be safe, give me the day before if this is not a workday
      IMPORTING
        factorydate         = lv_fstart.

    lv_date_string = ls_actionparams-planfinish.
    lv_date_string = lv_date_string(8).
    lv_date_temp = lv_date_string.
    CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
      EXPORTING
        date                = lv_date_temp
        factory_calendar_id = c_factorycalendar
        correct_option      = '-' "Be safe, give me the day before if this is not a workday
      IMPORTING
        factorydate         = lv_ffinish.

    SELECT SINGLE * FROM zaudit_notif
      INTO ls_audit_notif
      WHERE action_guid = ls_action-guid.

    IF sy-subrc = 0 AND ls_audit_notif IS NOT INITIAL.
      "Update the dates
      ls_audit_notif-plannedstart   = lv_fstart.
      ls_audit_notif-plannedfinish  = lv_ffinish.
      ls_audit_notif-responsible    = ls_action-z_assignedto.
      UPDATE zaudit_notif FROM ls_audit_notif.
    ELSE.
      "Insert new record
      ls_audit_notif-action_guid    = ls_action-guid.
      ls_audit_notif-external_id    = ls_actionui-external_id.
      ls_audit_notif-plannedstart   = lv_fstart.
      ls_audit_notif-plannedfinish  = lv_ffinish.
* BOI by PANUSURI Ticket ACR-4915
      IF ls_actionui-actualstartdate IS NOT INITIAL.
        ls_audit_notif-response       = abap_true.
      ELSE.
* EOI by PANUSURI Ticket ACR-4915
        ls_audit_notif-response       = abap_false.
      ENDIF.  "(+)PANUSURI Ticket ACR-4915
      ls_audit_notif-responsible    = ls_action-z_assignedto.
* BOI by PANUSURI Ticket ACR-4915
      IF ls_actionui-actualstartdate IS NOT INITIAL.
        "Get Actual start date Factory date
        CLEAR lv_astart.
        CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
          EXPORTING
            date                = ls_actionui-actualstartdate
            factory_calendar_id = c_factorycalendar
          IMPORTING
            factorydate         = lv_astart.

        ls_audit_notif-lastemail = lv_astart.
      ELSE.
* EOI by PANUSURI Ticket ACR-4915
        ls_audit_notif-lastemail      = '0'.
      ENDIF.  "(+)PANUSURI Ticket ACR-4915
      INSERT zaudit_notif FROM ls_audit_notif.
    ENDIF.
  ENDIF.

  IF ls_actionui-s_status CS 'AM03'.
    """""" Completed Date """"""
    IF ls_actionparams-actualfinish IS INITIAL.
      CLEAR: lv_date_string.
      CONCATENATE sy-datum c_timestamp INTO lv_date_string.
      ls_actionparams-actualfinish = lv_date_string.
      lv_updates = abap_true.
    ENDIF.

    """""" Delete from notification table """"""
    DELETE FROM zaudit_notif WHERE action_guid = ls_action-guid.
  ENDIF.

  "Remove if Deleted or archived
  IF ls_actionui-s_status CS 'AM10' OR ls_actionui-s_status CS 'AM05'.
    "AM10 = Delete
    "AM05 = Archived
    DELETE FROM zaudit_notif WHERE action_guid = ls_action-guid.
  ENDIF.

  IF ls_actionui-u_status CS 'INIT'.
    """""" Delete from notification table """"""
    DELETE FROM zaudit_notif WHERE action_guid = ls_action-guid.
  ENDIF.

  "Update the object if required:
  IF lv_updates = abap_true.
    ir_action->do_not_check_changeability = abap_true.
    CALL METHOD ir_action->set_attributes
      EXPORTING
        im_attributes  = ls_actionparams
        im_update_flag = 'X'.
    CALL METHOD ir_action_attr_assignment->set_attributes
      EXPORTING
        is_attributes = ls_action.
  ENDIF.

ENDMETHOD.
ENDCLASS.
