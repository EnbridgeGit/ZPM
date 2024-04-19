*&---------------------------------------------------------------------*
*&  Include           Z_UPDATE_WRK_DESC_01
*&---------------------------------------------------------------------*
DATA : lv_ktext TYPE cr_ktext,
      lv_objid TYPE cr_objid,
      ta_obj type STANDARD TABLE OF gstype_object_tab.
FIELD-SYMBOLS : <fs_obj_tab> TYPE  gstype_object_tab.

refresh : ta_obj[].
ta_obj[] = object_tab[].
LOOP AT ta_obj ASSIGNING <fs_obj_tab>.

  SELECT SINGLE objid INTO lv_objid FROM crhd WHERE
                            objty = 'A' AND
                            arbpl = <fs_obj_tab>-v_arbpl.

  IF lv_objid IS NOT INITIAL.

    CALL FUNCTION 'CR_WORKSTATION_READ'
      EXPORTING
        id        = lv_objid
      IMPORTING
        ktext     = lv_ktext
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 0.
      <fs_obj_tab>-zdescription = lv_ktext.
    ENDIF.
  ENDIF.
ENDLOOP.
clear : lv_objid,
        lv_ktext.
object_tab[] = ta_obj[].
