*----------------------------------------------------------------------*
***INCLUDE ZCPUSER_VR .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_TCODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_tcode INPUT.

  DATA: BEGIN OF lt_value_tcode OCCURS 0,
         tcode TYPE tcode,
         ttext TYPE ttext_stct,
        END OF   lt_value_tcode.

*  SELECT b~tcode a~ttext INTO TABLE lt_value_tcode
*         FROM tstct AS a LEFT OUTER JOIN tstc AS b
*         ON a~tcode = b~tcode
*         WHERE a~sprsl = sy-langu.
*
*  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
*    EXPORTING
*      retfield        = 'TCODE'
*      dynpprog        = sy-repid
*      dynpnr          = sy-dynnr
*      dynprofield     = 'GT_USER_BUTTON-ZTCODE'
*      value_org       = 'S'
*    TABLES
*      value_tab       = lt_value_tcode[]
*    EXCEPTIONS
*      parameter_error = 1
*      no_values_found = 2
*      OTHERS          = 3.
*  IF sy-subrc <> 0.
*    MESSAGE s000(zntemplate) WITH text-t04.
*  ENDIF.

*  CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*    EXPORTING
*      tabname           = 'TCODE'
*      fieldname         = space
*      dynpprog          = sy-repid
*      dynpnr            = sy-dynnr
*      dynprofield       = 'GT_USER_BUTTON-ZTCODE'
*      stepl             = sy-stepl
*      display           = space
*    EXCEPTIONS
*      field_not_found   = 1
*      no_help_for_field = 2
*      inconsistent_help = 3
*      no_values_found   = 4
*      OTHERS            = 5.
*  IF sy-subrc <> 0.
*    MESSAGE s000(zntemplate) WITH text-t04.
*  ENDIF.

ENDMODULE.                 " F4_TCODE  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_VARIANT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_variant INPUT.

  DATA: BEGIN OF lt_value_variant OCCURS 0,
         variant TYPE variant,
*         tcode TYPE tcode,
        END OF   lt_value_variant.

  DATA: lt_dynpfields TYPE TABLE OF dynpread WITH HEADER LINE.

  DATA: lv_variant TYPE variant,
        lv_tcode TYPE tcode.

  CLEAR: gv_line, lt_dynpfields.
  GET CURSOR LINE gv_line.
  lt_dynpfields-stepl     = gv_line.
  lt_dynpfields-fieldname = 'GT_USER_BUTTON-ZTCODE'.
  APPEND lt_dynpfields.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-repid
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      stepl_not_found      = 10
      OTHERS               = 11.
  IF sy-subrc <> 0.
    EXIT.
  ELSE.
    READ TABLE lt_dynpfields WITH KEY fieldname = 'GT_USER_BUTTON-ZTCODE'.
    IF sy-subrc = 0.
      lv_tcode = lt_dynpfields-fieldvalue.
      CLEAR lw_tstc.
      SELECT SINGLE * INTO lw_tstc FROM tstc WHERE tcode = lv_tcode.
      IF sy-subrc = 0.
        SELECT variant INTO TABLE lt_value_variant
               FROM vari WHERE report = lw_tstc-pgmna.
        IF sy-subrc <> 0.
          MESSAGE s000(zntemplate) WITH text-t05.
        ELSE.
          SORT lt_value_variant.
          DELETE ADJACENT DUPLICATES FROM lt_value_variant.

          CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
            EXPORTING
              retfield        = 'VARIANT'
              dynpprog        = sy-repid
              dynpnr          = sy-dynnr
              dynprofield     = 'GT_USER_BUTTON-ZVARIANT'
              value_org       = 'S'
            TABLES
              value_tab       = lt_value_variant
            EXCEPTIONS
              parameter_error = 1
              no_values_found = 2
              OTHERS          = 3.
          IF sy-subrc <> 0.
            MESSAGE s000(zntemplate) WITH text-t05.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDMODULE.                 " F4_VARIANT  INPUT
