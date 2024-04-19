class ZCL_IM_PLM_AUDIT_GOS definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_GOS
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_GOS .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_GOS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_GOS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_GOS IMPLEMENTATION.


method IF_EX_PLM_AUDIT_GOS~SELECT_SERVICES.
data: ls_tgos TYPE LINE OF tgos_sels.
break sahmad.

ls_tgos-sign = 'I'.
ls_tgos-option = 'EQ'.
ls_tgos-low = 'PCATTA_CREA'.
APPEND ls_tgos to rt_services.
"Create Note
ls_tgos-sign = 'I'.
ls_tgos-option = 'EQ'.
ls_tgos-low = 'NOTE_CREA'.
APPEND ls_tgos to rt_services.
"Attachment List
ls_tgos-sign = 'I'.
ls_tgos-option = 'EQ'.
ls_tgos-low = 'VIEW_ATTA'.
APPEND ls_tgos to rt_services.

endmethod.
ENDCLASS.
