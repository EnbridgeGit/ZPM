*&---------------------------------------------------------------------*
*&  Include           ZCPOPSO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_GET_PROG_NAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_TCODE_IW38  text
*      <--P_L_PROGRAM  text
*----------------------------------------------------------------------*
FORM sub_get_prog_name USING p_tcode
                       CHANGING p_cprog.

  SELECT SINGLE pgmna
       FROM tstc
       INTO p_cprog
       WHERE tcode = p_tcode.

ENDFORM.                     " SUB_GET_PROG_NAME

*&---------------------------------------------------------------------*
*&      Form  sub_submit_report
*&---------------------------------------------------------------------*
*       Submit Report Program with selection screen variant and
*       selection screen parameters
*----------------------------------------------------------------------*
*      -->P_T_SELPARAMS  Internal Table - Seltion Parameters
*      -->P_CPROG        Program Name
*      -->P_VARIANT      Selection Screen Variant
*----------------------------------------------------------------------*
FORM sub_submit_report TABLES p_t_selparams STRUCTURE rsparams
                       USING  p_cprog p_variant p_selscr.

  IF p_selscr IS INITIAL.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     VIA SELECTION-SCREEN AND RETURN.
  ELSE.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     AND RETURN.
  ENDIF.

ENDFORM.                    " SUB_SUBMIT_REPORT

*&---------------------------------------------------------------------*
*&      Form  sub_submit_report
*&---------------------------------------------------------------------*
*       Submit Report Program with selection screen variant and
*       selection screen parameters
*----------------------------------------------------------------------*
*      -->P_T_SELPARAMS  Internal Table - Seltion Parameters
*      -->P_CPROG        Program Name
*      -->P_VARIANT      Selection Screen Variant
*----------------------------------------------------------------------*
FORM sub_submit_report1 TABLES p_t_selparams STRUCTURE rsparams
                       USING  p_cprog p_variant p_selscr.

*  SUBMIT (p_cprog) USING SELECTION-SET p_variant
*                   WITH SELECTION-TABLE p_t_selparams
*                   AND RETURN.
  IF p_selscr IS INITIAL.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     VIA SELECTION-SCREEN AND RETURN.
  ELSE.
    SUBMIT (p_cprog) USING SELECTION-SET p_variant
                     WITH SELECTION-TABLE p_t_selparams
                     AND RETURN.
  ENDIF.


ENDFORM.                    "sub_submit_report1

*&---------------------------------------------------------------------*
*&      Form  SUB_CHECK_TCODE_AUTHORITY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TCODE_IW38  text
*      <--P_RETURN_CODE  text
*----------------------------------------------------------------------*
FORM sub_check_tcode_authority USING    p_tcode  TYPE sy-tcode
                               CHANGING p_return TYPE sy-subrc.

  CLEAR p_return.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = p_tcode
    EXCEPTIONS
      ok     = 1
      not_ok = 2
      OTHERS = 3.

  p_return = sy-subrc.

ENDFORM. "_TCODE_AUTHORITY

*&---------------------------------------------------------------------*
*&      Form  SUB_START_URL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_URL  text
*----------------------------------------------------------------------*
FORM sub_start_url USING p_url.

  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url                    = p_url
    EXCEPTIONS
      frontend_not_supported = 1
      frontend_error         = 2
      prog_not_found         = 3
      no_batch               = 4
      unspecified_error      = 5
      OTHERS                 = 6.

  IF sy-subrc <> 0.
    MESSAGE i000 WITH 'Can not start web browser'.
  ENDIF.

ENDFORM.                    "sub_start_url

*&---------------------------------------------------------------------*
*&      Form  SUB_READ_DEFAULT_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_read_default_values .
  DATA : l_key TYPE indx-srtfd.
  SELECT SINGLE *
         INTO CORRESPONDING FIELDS
         OF x_nt_defaults
         FROM zntdeflt
         WHERE uname = sy-uname.

  IF sy-subrc = 0.
    x_defaults = x_nt_defaults.
  ENDIF.
ENDFORM.                    " SUB_READ_DEFAULT_VALUES
