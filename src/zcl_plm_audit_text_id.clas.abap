class ZCL_PLM_AUDIT_TEXT_ID definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_PLM_AUDIT_TEXT_ID
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_PLM_AUDIT_TEXT_ID .
protected section.
*"* protected components of class ZCL_PLM_AUDIT_TEXT_ID
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_PLM_AUDIT_TEXT_ID
*"* do not include other source files here!!!

  constants CO_AUDIT_QUESTION type CGPL_OBJECT_TYPE value 'AQN'. "#EC NOTEXT
  constants CO_AUDIT type CGPL_OBJECT_TYPE value 'AUO'. "#EC NOTEXT
  constants CO_AUDIT_PLAN type CGPL_OBJECT_TYPE value 'AUP'. "#EC NOTEXT
  constants CO_AUDIT_ACTION type CGPL_OBJECT_TYPE value 'COR'. "#EC NOTEXT
  constants CO_QUESTION type CGPL_OBJECT_TYPE value 'QUE'. "#EC NOTEXT
  constants CO_QUESTION_LIST type CGPL_OBJECT_TYPE value 'QUN'. "#EC NOTEXT
  constants CO_TDOBJECT type THEAD-TDOBJECT value 'CGPL_TEXT'. "#EC NOTEXT
  constants CO_DESCRIPTION type THEAD-TDID value 'DESC'. "#EC NOTEXT
  constants CO_NOTE type THEAD-TDID value 'NOTE'. "#EC NOTEXT
  constants CO_FACTS type THEAD-TDID value 'FACT'. "#EC NOTEXT
  constants CO_POSITIVE type THEAD-TDID value 'POSI'. "#EC NOTEXT
  constants CO_NEGATIVE type THEAD-TDID value 'NEGA'. "#EC NOTEXT
  constants CO_LOCATION type THEAD-TDID value 'LOCA'. "#EC NOTEXT
  constants CO_TRUE type BOOLEAN value 'X'. "#EC NOTEXT
  class-data MT_TDID type PLMT_SAPSCRIPT_TDID_T .
  class-data MV_TDID_READ type C .

  methods CHECK_TDID
    importing
      !I_TDID type THEAD-TDID
    returning
      value(R_TDID_OK) type BOOLEAN .
ENDCLASS.



CLASS ZCL_PLM_AUDIT_TEXT_ID IMPLEMENTATION.


METHOD check_tdid .

* check whether TDID are selected
  IF mv_tdid_read IS INITIAL.
    SELECT tdid INTO TABLE mt_tdid FROM ttxid
                WHERE tdobject = co_tdobject.
    SORT mt_tdid BY tdid.
    MOVE 'X' TO mv_tdid_read.
  ENDIF.

* Check whether TDID could be used
  READ TABLE mt_tdid WITH KEY tdid = i_tdid
                     TRANSPORTING NO FIELDS
                     BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    MOVE 'X' TO r_tdid_ok.
  ELSE.
    MOVE '-' TO r_tdid_ok.
  ENDIF.

ENDMETHOD.


method IF_EX_PLM_AUDIT_TEXT_ID~GET_AUDITQUEST_TRANSFER_ID.
endmethod.


method IF_EX_PLM_AUDIT_TEXT_ID~GET_QUESTION_TRANSFER_ID.
endmethod.


method IF_EX_PLM_AUDIT_TEXT_ID~GET_TEXT_IDS.


  DATA ls_tdid             TYPE plmt_sapscript_tdid.
  DATA l_tdid_ok           TYPE boolean.
  DATA lv_assignment_class TYPE seoclname.
  DATA lt_tdid             TYPE plmt_sapscript_tdid_t.

  CASE i_object_type.
    WHEN co_audit_question.
*     Description
      MOVE co_description TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
**     Note
      MOVE co_note TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
*     Facts
      MOVE co_facts TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
**     Positive remarks
*      MOVE co_positive TO ls_tdid-tdid.
*      CALL METHOD check_tdid
*        EXPORTING
*          i_tdid    = ls_tdid-tdid
*        RECEIVING
*          r_tdid_ok = l_tdid_ok.
*      IF l_tdid_ok = co_true.
*        MOVE co_true  TO ls_tdid-changeable.
*        APPEND ls_tdid TO et_tdid.
*      ENDIF.
**     Negative remarks
*      MOVE co_negative TO ls_tdid-tdid.
*      CALL METHOD check_tdid
*        EXPORTING
*          i_tdid    = ls_tdid-tdid
*        RECEIVING
*          r_tdid_ok = l_tdid_ok.
*      IF l_tdid_ok = co_true.
*        MOVE co_true  TO ls_tdid-changeable.
*        APPEND ls_tdid TO et_tdid.
*      ENDIF.
    WHEN co_audit.
*     Description
      MOVE co_description TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
*     Note
      MOVE co_note TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
*     Positive remarks
      MOVE co_positive TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
*     Negative remarks
      MOVE co_negative TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
**     Location
*      MOVE co_location TO ls_tdid-tdid.
*      CALL METHOD check_tdid
*        EXPORTING
*          i_tdid    = ls_tdid-tdid
*        RECEIVING
*          r_tdid_ok = l_tdid_ok.
*      IF l_tdid_ok = co_true.
*        MOVE co_true  TO ls_tdid-changeable.
*        APPEND ls_tdid TO et_tdid.
*      ENDIF.
    WHEN co_audit_plan.
*     Description
      MOVE co_description TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
*     Note
      MOVE co_note TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
    WHEN co_audit_action.
*     Description
      MOVE co_description TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
*     Immediate Disposition
      MOVE 'ZIDP' TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.

*     Note
      MOVE co_note TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.

*     Root Cause
      MOVE 'ZRCA' TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.

    WHEN co_question.
*     Description
      MOVE co_description TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
*     Facts
      MOVE co_facts TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
    WHEN co_question_list.
*     Description
      MOVE co_description TO ls_tdid-tdid.
      CALL METHOD check_tdid
        EXPORTING
          i_tdid    = ls_tdid-tdid
        RECEIVING
          r_tdid_ok = l_tdid_ok.
      IF l_tdid_ok = co_true.
        MOVE co_true  TO ls_tdid-changeable.
        APPEND ls_tdid TO et_tdid.
      ENDIF.
**     Note
*      MOVE co_note TO ls_tdid-tdid.
*      CALL METHOD check_tdid
*        EXPORTING
*          i_tdid    = ls_tdid-tdid
*        RECEIVING
*          r_tdid_ok = l_tdid_ok.
*      IF l_tdid_ok = co_true.
*        MOVE co_true  TO ls_tdid-changeable.
*        APPEND ls_tdid TO et_tdid.
*      ENDIF.
    WHEN OTHERS.
      CALL METHOD cl_plm_audit_services=>get_assignment_class
        EXPORTING
          iv_object_type      = i_object_type
        IMPORTING
          ev_assignment_class = lv_assignment_class
        EXCEPTIONS
          not_supported       = 1
          OTHERS              = 2.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      CALL METHOD
        (lv_assignment_class)=>if_plm_assessment_assignment~get_default_text_ids
        IMPORTING
          et_tdid = lt_tdid.
      LOOP AT lt_tdid INTO ls_tdid.
        CALL METHOD check_tdid
          EXPORTING
            i_tdid    = ls_tdid-tdid
          RECEIVING
            r_tdid_ok = l_tdid_ok.
        IF l_tdid_ok = co_true.
          MOVE co_true  TO ls_tdid-changeable.
          APPEND ls_tdid TO et_tdid.
        ENDIF.
      ENDLOOP.
  ENDCASE.


endmethod.
ENDCLASS.
