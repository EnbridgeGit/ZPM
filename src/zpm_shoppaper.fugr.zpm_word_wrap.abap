FUNCTION zpm_word_wrap.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(TEXTLINE) TYPE  STRING
*"     VALUE(OUTPUTLEN) TYPE  I DEFAULT 132
*"  TABLES
*"      OUT_LINES OPTIONAL
*"----------------------------------------------------------------------

  DATA: lwa_output  TYPE string,
        lit_char120 TYPE TABLE OF char120,
        lwa_char120 TYPE char120.

  SPLIT textline AT space INTO TABLE lit_char120.
  LOOP AT lit_char120 INTO lwa_char120.
    IF lwa_output IS INITIAL.
      lwa_output = lwa_char120.
    ELSE.
      IF ( strlen( lwa_output ) + strlen( lwa_char120 ) + 1 ) GT outputlen.
        APPEND lwa_output TO out_lines.
        CLEAR lwa_output.
        lwa_output = lwa_char120.
      ELSE.
        CONCATENATE lwa_output lwa_char120 INTO lwa_output SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF lwa_output IS NOT INITIAL.
    APPEND lwa_output TO out_lines.
  ENDIF.

ENDFUNCTION.                  "
