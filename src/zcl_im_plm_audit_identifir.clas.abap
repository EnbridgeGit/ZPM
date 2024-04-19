class ZCL_IM_PLM_AUDIT_IDENTIFIR definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_PLM_AUDIT_IDENTIFIR
*"* do not include other source files here!!!

  interfaces IF_EX_PLM_AUDIT_IDENTIFIER .
protected section.
*"* protected components of class ZCL_IM_PLM_AUDIT_IDENTIFIR
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_PLM_AUDIT_IDENTIFIR
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_PLM_AUDIT_IDENTIFIR IMPLEMENTATION.


method IF_EX_PLM_AUDIT_IDENTIFIER~GET_DISPLAY_PROPERTIES.
endmethod.


METHOD if_ex_plm_audit_identifier~get_external_id.
  DATA:   lv_numberrange  TYPE inri-nrrangenr,
          lv_prefix(2),
          lv_timestamp    TYPE timestampl,
          lv_wp_no        TYPE wpinfo-wp_no.

  IF iv_external_id IS NOT INITIAL.
    EXIT.
  ENDIF.

  CASE iv_object_type.
** auditplan
    WHEN 'AUP'.
*     auditplan
      lv_numberrange = 'AP'.
      lv_prefix = 'AP'.
    WHEN 'AUO'.
*     audit
      lv_numberrange = 'AU'.
      lv_prefix = 'AU'.
*    when 'AQN'.
*      lv_numberrange = '
    WHEN 'COR'.
*     corrective action
      lv_numberrange = 'AA'.
      lv_prefix = 'AA'.
*    when 'QUN'.
**     question-list
*      lv_object = lc_qun.
*    when 'QUE'.
** question in question list
*      lv_object = lc_que.
  ENDCASE.

  CALL FUNCTION 'TH_GET_OWN_WP_NO'
    IMPORTING
      wp_no = lv_wp_no.
  REPLACE FIRST OCCURRENCE OF space IN lv_wp_no WITH '0'.

  IF iv_object_type = 'AUO' OR
     iv_object_type = 'AUP' OR
     iv_object_type = 'COR'.
    "ev_external_id = 'TEST123456'.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = lv_numberrange
        object                  = 'ZAUO'
*       QUANTITY                = '1'
*       SUBOBJECT               = ' '
*       TOYEAR                  = '0000'
*       IGNORE_BUFFER           = ' '
      IMPORTING
        number                  = ev_external_id
*       QUANTITY                =
*       RETURNCODE              =
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    CONCATENATE lv_prefix ev_external_id INTO ev_external_id.
  ELSEIF iv_object_type = 'AQN'.
    "Create new External_ID
    GET TIME STAMP FIELD lv_timestamp.
    ev_external_id = lv_timestamp.
    SHIFT ev_external_id LEFT DELETING LEADING space.
    WRITE ev_external_id+15(7) TO ev_external_id+14.
    "Additional information to be sure that key is uniqeue 99,9999%
    WRITE lv_wp_no TO ev_external_id+21.
  ENDIF.

ENDMETHOD.


METHOD if_ex_plm_audit_identifier~is_external_id_changeable.

  break sahmad.
  IF iv_object_type = 'AUO' OR
    iv_object_type = 'AUP' OR
    iv_object_type = 'COR' OR
    iv_object_type = 'AQN' .
    rv_is_changeable = '0'.
  ENDIF.

ENDMETHOD.
ENDCLASS.
