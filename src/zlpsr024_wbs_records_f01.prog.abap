*&---------------------------------------------------------------------*
*&  Include           ZLPSR024_WBS_RECORDS_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_PRPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_prps .
* Get WBS (Work Breakdown Structure) Element Master Data
  SELECT  pspnr	  "WBS Element
          objnr	  "Object number
          post1	  "PS: Short description (1st text line)
          psphi	  "Current number of the appropriate project
          loevm	  "Deletion Indicator
          belkz	  "Indicator: Account assignment element
    FROM prps
    INTO TABLE gt_prps
    WHERE pspnr IN s_pspnr AND
          psphi IN s_psphi AND
          VERNR IN S_DIV    AND
          BELKZ = 'X'.
  SORT gt_prps BY objnr.

ENDFORM.                    " GET_PRPS
*&---------------------------------------------------------------------*
*&      Form  GET_JEST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_jest .
  IF gt_prps IS NOT INITIAL.
*   Get Individual object Status
    SELECT  objnr	"Object number
            stat  "Object status
      FROM jest
      INTO TABLE gt_jest
      FOR ALL ENTRIES IN gt_prps
      WHERE objnr = gt_prps-objnr AND
            stat IN s_istat AND
            inact EQ space.
    IF SY-SUBRC IS INITIAL.
      SORT gt_jest BY objnr.
      DELETE ADJACENT DUPLICATES FROM gt_jest COMPARING objnr.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_JEST
*&---------------------------------------------------------------------*
*&      Form  GET_COBRB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_cobrb .
  DATA:lt_cosp TYPE TABLE OF ty_cosp,
       ls_cosp LIKE LINE OF lt_cosp.
  IF gt_jest IS NOT INITIAL.
*   If any GL Account specified in selection screen
    IF s_hkont IS NOT INITIAL. "Uncomment to get the WBSE with no GL account
*     Get Distribution Rules Settlement Rule Order Settlement
      SELECT  objnr	"Object number
              perbz	"Settlement type
              gabja	"Valid-from year
              gabpe	"Valid-from period
              konty	"Account assignment category
              hkont	"G/L Account Number
        FROM cobrb
        INTO TABLE gt_cobrb
        FOR ALL ENTRIES IN gt_jest
        WHERE objnr = gt_jest-objnr AND
              hkont IN s_hkont.

      IF gt_cobrb IS NOT INITIAL.
        CLEAR gt_cobrb_unsetl.
        APPEND LINES OF gt_cobrb TO gt_cobrb_unsetl.
        DELETE gt_cobrb_unsetl WHERE gabja IS NOT INITIAL AND gabpe IS NOT INITIAL.
        IF gt_cobrb_unsetl IS NOT INITIAL.
          SORT gt_cobrb_unsetl BY objnr.
          SELECT  objnr	"Object number
                  wTg005"Total Value in Controlling Area Currency
            FROM cosp
            INTO TABLE lt_cosp
            FOR ALL ENTRIES IN gt_cobrb_unsetl
            WHERE objnr = gt_cobrb_unsetl-objnr AND
                  GJAHR = P_FYEAR  and                   "Fiscal Year selected
                  VERSN = P_VERS  and                    "Version
                  wrttp = '04'.                          "Actuals only.
        ENDIF.
      ENDIF.
*   If No GL Account specified in selection screen
    ELSE.
*   un comment the below code to get WBSE which are not having GL Account
**     Get Distribution Rules Settlement Rule Order Settlement
*      SELECT  objnr  "Object number
*              perbz  "Settlement type
*              gabja  "Valid-from year
*              gabpe  "Valid-from period
*              konty  "Account assignment category
*              hkont  "G/L Account Number
*        FROM cobrb
*        INTO TABLE gt_cobrb
*        FOR ALL ENTRIES IN gt_jest
*        WHERE objnr = gt_jest-objnr AND
*              hkont NE space.
*       Get CO Object: Cost Totals for External Postings
      SELECT  objnr  "Object number
               wTg005"Total Value in Controlling Area Currency
         FROM cosp
         INTO TABLE lt_cosp
         FOR ALL ENTRIES IN gt_jest
         WHERE objnr = gt_jest-objnr AND
               GJAHR = P_FYEAR  and                   "Fiscal Year selected
               VERSN = P_VERS  and                    "Version
               wrttp = '04'.                          "Actuals only.
    ENDIF.
  ENDIF.
  IF lt_cosp IS NOT INITIAL.
    LOOP AT lt_cosp INTO ls_cosp.
      COLLECT ls_cosp INTO gt_cosp.
      CLEAR ls_cosp.
    ENDLOOP.
    SORT gt_cosp BY objnr.
  ENDIF.
ENDFORM.                    " GET_COBRB
*&---------------------------------------------------------------------*
*&      Form  PREPARE_FINAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_final_table .
* if the GL Account in selection screen is not blank
  IF s_hkont IS NOT INITIAL.
    LOOP AT gt_cobrb INTO gs_cobrb.
*     Read the status of object
      READ TABLE gt_jest
      INTO gs_jest
      WITH KEY objnr = gs_cobrb-objnr BINARY SEARCH.
      IF sy-subrc = 0.
*       Read WBS element master data
        READ TABLE gt_prps
        INTO gs_prps
        WITH KEY objnr = gs_jest-objnr BINARY SEARCH.
        MOVE: gs_prps-psphi   TO  gs_final-psphi,   "Current number of the appropriate project
              gs_prps-pspnr   TO  gs_final-pspnr,  "WBS Element
              gs_prps-post1   TO  gs_final-post1,  "PS: Short description (1st text line)
              gs_jest-stat    TO  gs_final-stat,   "Object status
              gs_prps-objnr   TO  gs_final-objnr,  "Object number
              gs_prps-loevm   TO  gs_final-loevm,  "Deletion Indicator
              gs_cobrb-perbz   TO	gs_final-perbz,  "Settlement type
              gs_cobrb-konty   TO	gs_final-konty,  "Account assignment category
              gs_cobrb-hkont   TO	gs_final-hkont,  "G/L Account Number
              gs_prps-belkz   TO  gs_final-belkz.  "Indicator: Account assignment element
        READ TABLE gt_cobrb_unsetl
        INTO gs_cobrb_unsetl
        WITH KEY objnr = gs_cobrb-objnr BINARY SEARCH.
        IF sy-subrc = 0.
          READ TABLE gt_cosp
          INTO gs_cosp
          WITH KEY objnr =  gs_cobrb_unsetl-objnr BINARY SEARCH.
          IF sy-subrc = 0.
            gs_final-wTg005 =  gs_cosp-wTg005.
            CLEAR: gs_cosp, gs_cobrb_unsetl.
          ENDIF.
        ENDIF.
        APPEND gs_final TO gt_final.
      ENDIF.
      CLEAR:gs_cobrb, gs_jest, gs_prps, gs_final.
    ENDLOOP.
* If no GL Account given in selection screen
  ELSE.
*  Uncomment the below block for getting the WBSE with no GL Account
*    SORT gt_cobrb BY objnr.
    LOOP AT gt_jest INTO gs_jest.
*  Uncomment the below block for getting the WBSE with no GL Account
*      READ TABLE gt_cobrb
*      INTO gs_cobrb
*      WITH KEY objnr = gs_jest-objnr BINARY SEARCH.
*      IF sy-subrc NE 0.
        READ TABLE gt_prps
        INTO gs_prps
        WITH KEY objnr = gs_jest-objnr BINARY SEARCH.
        MOVE: gs_prps-psphi   TO  gs_final-psphi,   "Current number of the appropriate project
              gs_prps-pspnr   TO  gs_final-pspnr,  "WBS Element
              gs_prps-post1   TO  gs_final-post1,  "PS: Short description (1st text line)
              gs_jest-stat    TO  gs_final-stat,   "Object status
              gs_prps-objnr   TO  gs_final-objnr,  "Object number
              gs_prps-loevm   TO  gs_final-loevm,  "Deletion Indicator
*              gs_cobrb-perbz   TO  gs_final-perbz,  "Settlement type
*              gs_cobrb-konty   TO  gs_final-konty,  "Account assignment category
*              gs_cobrb-hkont   TO  gs_final-hkont,  "G/L Account Number
              gs_prps-belkz   TO  gs_final-belkz.  "Indicator: Account assignment element
        READ TABLE gt_cosp
        INTO gs_cosp
        WITH KEY objnr =  gs_prps-objnr BINARY SEARCH.
        IF sy-subrc = 0.
          gs_final-WTG005 =  gs_cosp-WTG005.
          CLEAR: gs_cosp, gs_cobrb_unsetl.
        ENDIF.

        APPEND gs_final TO gt_final.
*      ENDIF.
      CLEAR:gs_final, gs_prps, gs_jest, gs_cobrb.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " PREPARE_FINAL_TABLE
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_data .
  IF go_alv IS NOT INITIAL.
    go_alv->display( ).
  ENDIF.
ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  INSTANCE_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM instance_alv .
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = gt_final.
    ##NO_HANDLER CATCH cx_salv_msg .
  ENDTRY.
ENDFORM.                    " INSTANCE_ALV
*&---------------------------------------------------------------------*
*&      Form  FIELD_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM field_catalog .
  DATA: lo_functions TYPE REF TO cl_salv_functions.
  IF go_alv IS NOT INITIAL.
*    Get columns of ALV
    CALL METHOD go_alv->get_columns
      RECEIVING
        value = go_columns.

*    Get column
    TRY.
        go_column ?= go_columns->get_column('PSPHI').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Project def. number'(002) ).
    go_column->set_medium_text( 'Project def. number'(003) ).
    go_column->set_short_text( 'Prj def.no'(004) ).

    TRY.
        go_column ?= go_columns->get_column( 'PSPNR' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'WBS element number'(005) ).
    go_column->set_medium_text( 'WBS element number'(006) ).
    go_column->set_short_text( 'WBS No'(007) ).

    TRY.
        go_column ?= go_columns->get_column( 'POST1' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Description'(008) ).
    go_column->set_medium_text( 'Description'(008) ).
    go_column->set_short_text( 'Desc.'(009) ).

    TRY.
        go_column ?= go_columns->get_column( 'STAT' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Status'(010) ).
    go_column->set_medium_text( 'Status'(010) ).
    go_column->set_short_text( 'Status'(010) ).

    TRY.
        go_column ?= go_columns->get_column( 'OBJNR' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Object number'(011) ).
    go_column->set_medium_text( 'Object number'(011) ).
    go_column->set_short_text( 'Obj. no'(012) ).

    TRY.
        go_column ?= go_columns->get_column( 'LOEVM' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Deletion flag'(013) ).
    go_column->set_medium_text( 'Deletion flag'(013) ).
    go_column->set_short_text( 'Del. flag'(014) ).
    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column( 'PERBZ' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Settlement type'(015) ).
    go_column->set_medium_text( 'Settlement type'(015) ).
    go_column->set_short_text( 'Settl.typ'(016) ).
    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column( 'KONTY' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Account assignment category'(017) ).
    go_column->set_medium_text( 'Account ass. cat.'(018) ).
    go_column->set_short_text( 'Act.cat'(019) ).
    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column( 'HKONT' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'G/L Account'(020) ).
    go_column->set_medium_text( 'G/L Account'(020) ).
    go_column->set_short_text( 'G/L Acc.'(021) ).
    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column( 'BELKZ' ).
      ##NO_HANDLER      CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Account Assignment Indicator'(022) ).
    go_column->set_medium_text( 'Acc. Assignment Ind'(023) ).
    go_column->set_short_text( 'Acc. Ind'(024) ).

    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '10'.

    TRY.
        go_column ?= go_columns->get_column('WKG001').
      ##NO_HANDLER     CATCH cx_salv_not_found .
    ENDTRY.
    go_column->set_long_text( 'Net amount'(026) ).
    go_column->set_medium_text( 'Net amount'(026) ).
    go_column->set_short_text( 'Net amount'(026) ).

    CALL METHOD go_column->set_output_length
      EXPORTING
        value = '13'.

    lo_functions = go_alv->get_functions( ).
    lo_functions->set_all( abap_true ).


  ENDIF.
ENDFORM.                    " FIELD_CATALOG
