class ZCL_IM_PLM_AUDIT_AUO_UPDAT definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_AUO_UPDAT
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_AUO_UPDATE .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_AUO_UPDAT
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_AUO_UPDAT
*"* do not include other source files here!!!

  methods QUESTION_AT_SAVE
    importing
      !IR_QUEST type ref to CL_CGPL_TASK
      !IR_QUEST_ATTR_ASSIGNMENT type ref to CL_PLM_AUDIT_QUEST_ASSIGNMENT
      !IS_QUEST type PLMM_QUEST_RES .
  methods AUDIT_AT_SAVE
    importing
      value(IR_AUDIT) type ref to CL_CGPL_PROJECT
      value(IR_AUDIT_ATTR_ASSIGNMENT) type ref to CL_PLM_AUDIT_ASSIGNMENT
    exceptions
      ERROR_WITH_MESSAGE .
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_AUO_UPDAT IMPLEMENTATION.


METHOD audit_at_save.
  DATA: ls_audit          TYPE plmm_audit,
        ls_auditui        TYPE plmt_audit_ui,
        ls_project        TYPE cgpl_project,
        lv_updates        TYPE boolean,
        lv_date_string    TYPE string.

  CONSTANTS:  c_classification  TYPE c LENGTH 2 VALUE 'A1',
              c_calculate       TYPE c LENGTH 2 VALUE '52',
              c_timestamp(6)    TYPE c            VALUE '120000'.

  "Get the ui structure for status code
  CALL METHOD ir_audit_attr_assignment->get_attributes
    IMPORTING
      es_attributes = ls_audit.
  CALL METHOD ir_audit_attr_assignment->get_ui_structure
    IMPORTING
      es_external_structure = ls_auditui.

  "Get the Project Attributes
  CALL METHOD ir_audit->get_attributes
    IMPORTING
      ex_attributes = ls_project.

  "Default Values
  IF ls_audit-classification IS INITIAL.
    ls_audit-classification = c_classification.
    lv_updates = abap_true.
  ENDIF.

  IF ls_audit-audit_calculate IS INITIAL.
    ls_audit-audit_calculate = c_calculate.
    lv_updates = abap_true.
  ENDIF.


  "Signature Provided
  IF ls_auditui-s_status CS 'AM17'.

    """""" Completed Date """"""
    IF ls_project-actualfinish IS INITIAL.
      CLEAR: lv_date_string.
      CONCATENATE sy-datum c_timestamp INTO lv_date_string.
      ls_project-actualfinish = lv_date_string.
      lv_updates = abap_true.
    ENDIF.
    "Actual Auditor
    IF ls_audit-z_auditor IS INITIAL.
      ls_audit-z_auditor = sy-uname.
      lv_updates = abap_true.
    ENDIF.
    IF ls_project-actualstart IS INITIAL.
      sy-msgty = 'E'.
      sy-msgid = 'ZFI01'.
      sy-msgno = '017'.
      sy-msgv1 = 'Enter the Actual date of the audit (Start date)'.
      RAISE error_with_message.
    ENDIF.
  ENDIF.

  "Update the object if required:
  IF lv_updates = abap_true.
    ir_audit->do_not_check_changeability = abap_true.
    CALL METHOD ir_audit->set_attributes
      EXPORTING
        im_attributes = ls_project.
    CALL METHOD ir_audit_attr_assignment->set_attributes
      EXPORTING
        is_attributes = ls_audit.

  ENDIF.




ENDMETHOD.


METHOD if_ex_plm_audit_auo_update~change_audit_at_save.
  CALL METHOD me->audit_at_save
    EXPORTING
      ir_audit                 = ir_audit
      ir_audit_attr_assignment = ir_audit_attr_assignment
    EXCEPTIONS
      error_with_message       = 1.
  IF sy-subrc = 1.
    RAISE error_with_message.
  ENDIF.
ENDMETHOD.


METHOD if_ex_plm_audit_auo_update~change_audit_before_update.
  break btboundy.
ENDMETHOD.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CHANGE_AUDIT_IN_UPDATE.
  break btboundy.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CHANGE_OBJECT_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CHANGE_OBJECT_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CHANGE_OBJECT_IN_UPDATE.
endmethod.


METHOD if_ex_plm_audit_auo_update~change_quest_at_save.

  CALL METHOD me->question_at_save
    EXPORTING
      ir_quest = ir_quest
      ir_quest_Attr_assignment = ir_quest_attr_assignment
      is_quest = is_quest_new.


ENDMETHOD.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CHANGE_QUEST_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CHANGE_QUEST_IN_UPDATE.
endmethod.


METHOD if_ex_plm_audit_auo_update~create_audit_at_save.
  CALL METHOD me->audit_at_save
    EXPORTING
      ir_audit                 = ir_audit
      ir_audit_attr_assignment = ir_audit_attr_assignment
    EXCEPTIONS
      error_with_message       = 1.
  IF sy-subrc = 1.
    RAISE error_with_message.
  ENDIF.
ENDMETHOD.


METHOD if_ex_plm_audit_auo_update~create_audit_before_update.
ENDMETHOD.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CREATE_AUDIT_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CREATE_OBJECT_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CREATE_OBJECT_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CREATE_OBJECT_IN_UPDATE.
endmethod.


METHOD if_ex_plm_audit_auo_update~create_quest_at_save.
  CALL METHOD me->question_at_save
    EXPORTING
      ir_quest                 = ir_quest
      ir_quest_attr_assignment = ir_quest_attr_assignment
      is_quest                 = is_quest.
ENDMETHOD.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CREATE_QUEST_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~CREATE_QUEST_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_AUDIT_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_AUDIT_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_AUDIT_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_OBJECT_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_OBJECT_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_OBJECT_IN_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_QUEST_AT_SAVE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_QUEST_BEFORE_UPDATE.
endmethod.


method IF_EX_PLM_AUDIT_AUO_UPDATE~DELETE_QUEST_IN_UPDATE.
endmethod.


METHOD question_at_save.
  DATA: lv_haschild TYPE xfeld,
        lv_below    TYPE bapi_20350_key,
        lt_text     TYPE STANDARD TABLE OF bapi_bus20310_text,
        ls_text     TYPE bapi_bus20310_text,
        "lt_ltext    TYPE STANDARD TABLE OF bapi_bus20350_long_text,
        "ls_ltext    TYPE bapi_bus20350_long_text,
        lo_audit    TYPE REF TO cl_cgpl_project,
        lv_auditid  TYPE bapi_20350_d-external_id,
        ls_audit    TYPE bapi_20350_d,
        lv_newact   TYPE bapi_20370_c.

  CONSTANTS:  c_nrobject    TYPE inri-object    VALUE 'ZAUO',
              c_nrrange     TYPE inri-nrrangenr VALUE 'AA',
              c_group       TYPE string         VALUE 'NON-CONFORMANCE ACTION REQUEST',
              c_description TYPE string         VALUE 'QA Corrective Action Request',
              c_tnote       TYPE string         VALUE 'NOTE',
              c_tnote1      TYPE string         VALUE 'Corrective Actions:',
              c_tnote2      TYPE string         VALUE 'Preventative Actions:'.

  DATA: ls_auditquestion TYPE plmt_quest_res_ui.
  CALL METHOD ir_quest_attr_assignment->get_ui_structure
    IMPORTING
      es_external_structure = ls_auditquestion.

  "Default Values
  IF ls_auditquestion-assessm_profil IS INITIAL.
    ls_auditquestion-assessm_profil = 'AM10'.
  ENDIF.

  CALL METHOD ir_quest_attr_assignment->import_ui_structure_data
    EXPORTING
      is_external_structure = ls_auditquestion.


  IF is_quest-corr_required = abap_true.

    "Check if the node has existing children (At least 1 action)
    CALL METHOD ir_quest->has_outline_children
      RECEIVING
        re_has_outline_children = lv_haschild.


    IF lv_haschild = abap_false.
      "Get Audit info
      CALL METHOD ir_quest_attr_assignment->if_cgpl_assignment~get_parent
        IMPORTING
          ex_project = lo_audit.

      CALL METHOD lo_audit->get_external_id
        RECEIVING
          re_external_id = lv_auditid.

      IF lv_auditid IS NOT INITIAL.
        CALL FUNCTION 'BAPI_BUS20350_GET_DETAIL'
          EXPORTING
            external_id = lv_auditid
          IMPORTING
            audit       = ls_audit.

        IF ls_audit IS NOT INITIAL.
          lv_newact-z_assignedto = ls_audit-z_manager.
        ENDIF.
      ENDIF.

      "Get the next number for the action
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = c_nrrange
          object      = c_nrobject
        IMPORTING
          number      = lv_newact-external_id.

      "Set action parameters
      CONCATENATE c_nrrange lv_newact-external_id INTO lv_newact-external_id.
      lv_newact-grouping  = c_group.

      "Set the description text object
      ls_text-external_id   = lv_newact-external_id. "Same external id as main object
      ls_text-description   = c_description.
      APPEND ls_text TO lt_text.

*      "Set the longtext
*      ls_ltext-external_id  = lv_newact-external_id. "Same external id as main object
*      ls_ltext-text_id      = c_tdesc. "Text type goes here...
*      ls_ltext-text_line    = c_tdesctext.
*      APPEND ls_ltext TO lt_ltext.

*      "Set the Note default:
*      ls_ltext-external_id  = lv_newact-external_id. "Same external id as main object
*      ls_ltext-text_id      = c_tnote. "Text type goes here...
*
*      CONCATENATE c_tnote1 cl_abap_char_utilities=>newline cl_abap_char_utilities=>newline c_tnote2 into ls_ltext-text_line.
*      APPEND ls_ltext TO lt_ltext.

      "What node do you want it below?
      MOVE is_quest-guid TO lv_below-guid. "Audit GUID

      "Call the badi to create this, your current lock will let this happen in this LUW
      CALL FUNCTION 'BAPI_BUS20370_CREATE'
        EXPORTING
          auditcorrecaction = lv_newact
          below             = lv_below
        TABLES
          texts             = lt_text.
      "longtexts         = lt_ltext.
    ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
