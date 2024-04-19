class ZCL_IM_PLM_AUDIT_SIGNATURE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_SIGNATURE
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_SIGNATURE .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_SIGNATURE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_SIGNATURE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_SIGNATURE IMPLEMENTATION.


method IF_EX_PLM_AUDIT_SIGNATURE~DISPLAY_SIGNATURES.
endmethod.


METHOD if_ex_plm_audit_signature~sign_audit.
  DATA: lr_data_ref       TYPE REF TO data,
        lv_data_type      TYPE tabname,
        ls_audit          TYPE plmt_audit_pri,
        ls_questions      TYPE LINE OF plmt_quest_res_prt.

  FIELD-SYMBOLS: <fs> TYPE any.

  ""Ensure all questions are complete
  CALL METHOD cl_plm_audit_services=>collect_data
    EXPORTING
      ir_object           = ir_audit
      iv_application_type = 'AUD'
    IMPORTING
      er_data_ref         = lr_data_ref
      ev_data_type        = lv_data_type
    EXCEPTIONS
      failed              = 1.

  ASSIGN lr_data_ref->* TO <fs> CASTING TYPE (lv_data_type).
  ls_audit = <fs>.
  LOOP AT ls_audit-questions INTO ls_questions.
    IF ls_questions-assessment_rec IS INITIAL AND ls_questions-not_relevant IS INITIAL and ls_questions-task_level = 2.
      MESSAGE e017(zfi01) WITH 'Please answer all questions'.
      RAISE not_signed.
    ENDIF.
  ENDLOOP.
ENDMETHOD.
ENDCLASS.
