*&---------------------------------------------------------------------*
*&  Include           ZXCO1U23
*&---------------------------------------------------------------------*
*"     REFERENCE(IS_COMPONENT) TYPE  RESBD
*"     REFERENCE(IS_COMPONENT_OLD) TYPE  RESBD

  DATA: lv_bname     TYPE usr01-bname.

  IF is_component-afnam IS NOT INITIAL.
    IF is_component-afnam NE is_component_old-afnam.
      SELECT SINGLE bname FROM usr01 INTO lv_bname WHERE bname = is_component-afnam.
      IF sy-subrc NE 0.
        MESSAGE e004(zpm) WITH is_component-afnam.
      ENDIF.
    ENDIF.
  ENDIF.
