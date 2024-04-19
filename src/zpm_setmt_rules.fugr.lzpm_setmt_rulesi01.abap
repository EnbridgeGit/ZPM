*----------------------------------------------------------------------*
***INCLUDE LZPM_SETMT_RULESI01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  F4_KONTY  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_konty INPUT.

  DATA: lv_konty TYPE konty.

  CALL FUNCTION 'HELP_REQUEST_FOR_OBART'
    EXPORTING
      fieldname                  = 'KONTY'
      tabname                    = 'COBRB'
      settlement_receiver_only   = 'X'
      settlement_receiver_actual = 'X'
    IMPORTING
      select_value               = lv_konty.

  zpmt_setmt_rules-konty = lv_konty.

ENDMODULE.                 " F4_KONTY  INPUT
*&---------------------------------------------------------------------*
*&      Module  F4_EMPGE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_empge INPUT.

  DATA: lit_dynpread TYPE TABLE OF dynpread,
        lit_return   TYPE TABLE OF ddshretval,
        lwa_return   TYPE ddshretval,
        lwa_dynpread TYPE dynpread,
        lwa_cobrb        TYPE cobrb,
        lv_form_name(30) TYPE c,
        lv_fieldname TYPE help_info-fieldname,
        lv_tabname   TYPE help_info-tabname,
        lv_search    TYPE char1,
        lv_obart     TYPE  tiva5-vvimkob.

  FIELD-SYMBOLS: <lfs_field> TYPE any.

  REFRESH: lit_dynpread.
  CLEAR: lwa_dynpread, lv_konty, lv_obart, lv_tabname, lv_fieldname, lv_search.

  lwa_dynpread-fieldname = 'ZPMT_SETMT_RULES-KONTY'.

  CALL FUNCTION 'DYNP_GET_STEPL'
    IMPORTING
      povstepl = lwa_dynpread-stepl.

  APPEND lwa_dynpread TO lit_dynpread.
  CLEAR lwa_dynpread.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = 'SAPLZPM_SETMT_RULES'
      dynumb             = '0001'
      translate_to_upper = 'X'
    TABLES
      dynpfields         = lit_dynpread
    EXCEPTIONS
      OTHERS             = 1.

  CHECK sy-subrc IS INITIAL.

  READ TABLE lit_dynpread INTO lwa_dynpread INDEX 1.
  IF NOT sy-subrc IS INITIAL.
    EXIT.
  ENDIF.

  IF lwa_dynpread-fieldvalue IS INITIAL.
    MESSAGE i000(zpm) WITH 'Enter an Account Assignment Category'(001).
    EXIT.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_OBART_INPUT'
      EXPORTING
        input     = lwa_dynpread-fieldvalue
      IMPORTING
        output    = lv_konty
      EXCEPTIONS
        not_found = 1.

    IF sy-subrc = 1.
      MESSAGE ID sy-msgid
              TYPE 'I'
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.
  ENDIF.

  lv_tabname = 'COBL'.
  CASE lv_konty.
    WHEN objektart_ks.
      lv_fieldname = 'KOSTL'.
      lv_search = 'X'.
    WHEN objektart_or.
      lv_fieldname = 'AUFNR'.
      lv_search = 'X'.
    WHEN objektart_pr.
      lv_fieldname = 'PS_PSP_PNR'.
      lv_search = 'X'.
    WHEN objektart_eo.
    WHEN objektart_np.
      lv_fieldname = 'NPLNR'.
      lv_search = 'X'.
    WHEN objektart_nv.
    WHEN objektart_sk.
      lv_fieldname = 'SAKNR'.
      lv_search = 'X'.
    WHEN objektart_vb.
      lv_fieldname = 'KDAUF'.
      lv_search = 'X'.
    WHEN objektart_an.
      lv_fieldname = 'ANLN1'.
      lv_search = 'X'.
    WHEN objektart_hp.
      lv_fieldname = 'KSTRG'.
      lv_search = 'X'.
    WHEN objektart_ma.
    WHEN objektart_bp.
      lv_fieldname = 'PRZNR'.
      lv_search = 'X'.
  ENDCASE.

  IF lv_search = 'X'.
    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
      EXPORTING
        tabname           = lv_tabname
        fieldname         = lv_fieldname
      TABLES
        return_tab        = lit_return
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        OTHERS            = 5.

    IF sy-subrc IS INITIAL AND
          NOT lit_return[] IS INITIAL.
      LOOP AT lit_return INTO lwa_return.
        IF lv_konty = objektart_np AND
          lwa_return-fieldname = 'AUFNR'.
          lwa_cobrb-nplnr = lwa_return-fieldval.

        ELSEIF lv_konty = objektart_pr AND
          lwa_return-fieldname = 'POSID'.
          CALL FUNCTION 'CONVERSION_EXIT_KONPR_INPUT'
            EXPORTING
              input     = lwa_return-fieldval
            IMPORTING
              output    = lwa_cobrb-ps_psp_pnr
            EXCEPTIONS
              not_found = 1.
*          F4-help -> no error or dump
          IF sy-subrc IS INITIAL.
            EXIT.
          ENDIF.
        ELSEIF lv_konty = objektart_sk AND
                         lwa_return-fieldname = 'SAKNR'.
          lwa_cobrb-hkont =  lwa_return-fieldval.
        ELSEIF lv_konty = objektart_vb.
          IF lwa_return-fieldname = 'VBELN'.
            lwa_cobrb-kdauf =  lwa_return-fieldval.
*                                                           "Hw550596 >>
            PERFORM empge_for_kdpos IN PROGRAM saplkobs IF FOUND
                                    TABLES lit_return
                                    USING  lwa_return-fieldval
                                    CHANGING lwa_cobrb-kdpos.
*                                                           "Hw550596 <<
          ELSEIF lwa_return-fieldname = 'POSNR'.
            lwa_cobrb-kdpos =  lwa_return-fieldval.
          ENDIF.
        ELSE.
          ASSIGN COMPONENT lwa_return-fieldname OF STRUCTURE lwa_cobrb
            TO <lfs_field>.
          IF sy-subrc IS INITIAL.
            <lfs_field> = lwa_return-fieldval.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CONCATENATE 'EMPGE_FOR_' lv_fieldname INTO lv_form_name.
      PERFORM (lv_form_name) IN PROGRAM saplkobs IF FOUND
                             USING    lwa_cobrb
                             CHANGING
                                      zpmt_setmt_rules-empge.
    ENDIF.

  ELSEIF lv_konty = objektart_ia OR lv_konty = objektart_ib OR
         lv_konty = objektart_ic OR lv_konty = objektart_ig OR
         lv_konty = objektart_im OR lv_konty = objektart_iv OR
         lv_konty = objektart_iw OR lv_konty = objektart_is OR
         lv_konty = objektart_i1.

    lv_obart = lv_konty.
    CALL FUNCTION 'RE_MATCHCODE_FOR_OBART'
      EXPORTING
        i_obart = lv_obart
      IMPORTING
        e_empge = zpmt_setmt_rules-empge
      EXCEPTIONS
        OTHERS  = 1.
  ENDIF.

ENDMODULE.                 " F4_EMPGE  INPUT
