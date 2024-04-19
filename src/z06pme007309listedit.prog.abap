*&---------------------------------------------------------------------*
*&  Include           Z06PME007309LISTEDIT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_CORR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_corr .
** new code added by sghosh
  DATA :   l_program  TYPE sy-cprog,
           l_space    TYPE char1,
           l_function TYPE zzfunction,
           l_ret_code TYPE sy-subrc.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    LOOP AT s_strno.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_strno TO t_selparams.
      t_selparams-selname = 'STRNO'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.

    LOOP AT s_eqn.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_eqn TO t_selparams.
      t_selparams-selname = 'EQUNR'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.


    PERFORM sub_populate_seltab USING space space
                                        'S_AUART'   space
                                        space.

    CLEAR t_selparams.
    t_selparams-selname = 'AUART'.
    t_selparams-kind    = 'S'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'PM10'.
    t_selparams-high    = ''.
    APPEND t_selparams.
    CLEAR t_selparams.
    t_selparams-selname = 'DY_OFN'.
    t_selparams-kind    = 'P'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'X'.
    t_selparams-high    = ''.
    APPEND t_selparams.
    CLEAR t_selparams.
    t_selparams-selname = 'DY_IAR'.
    t_selparams-kind    = 'P'.
    t_selparams-sign    = 'I'.
    t_selparams-option  = 'EQ'.
    t_selparams-low     = 'X'.
    t_selparams-high    = ''.
    APPEND t_selparams.

    IF s_date[] IS NOT INITIAL.
      LOOP AT s_date.
        CLEAR t_selparams.
        MOVE-CORRESPONDING s_date TO t_selparams.
        t_selparams-selname = 'ERDAT'.
        t_selparams-kind    = 'S'.
        APPEND t_selparams.
      ENDLOOP.
    ENDIF.

    IF x_defaults-zzunit1 IS NOT INITIAL .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF x_defaults-zzunit2 IS NOT INITIAL.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

    PERFORM sub_get_prog_name USING t_config-tcode
                               CHANGING l_program.

*  PERFORM sub_get_prog_name USING c_tcode_iw38
*                             CHANGING l_program.
    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.
  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_HANDLE_CORR


*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_PMDUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_pmdue .
  DATA : lto_date TYPE syst-datum,
         lfr_date TYPE syst-datum,
         l_program  TYPE sy-cprog,
         l_space    TYPE char1,
         l_function TYPE zzfunction,
         l_ret_code TYPE sy-subrc.
  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    LOOP AT s_strno.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_strno TO t_selparams.
      t_selparams-selname = 'STRNO'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.

    LOOP AT s_eqn.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_eqn TO t_selparams.
      t_selparams-selname = 'EQUNR'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.


*  PERFORM sub_populate_seltab USING space space
*                                      'S_AUART'   space
*                                      space.



    CLEAR t_selparams.
    lto_date = sy-datum.
* To get the period
    PERFORM sub_get_date TABLES t_selparams.

    IF x_defaults-zzunit1 IS NOT INITIAL .

*--  Populate parameter PLANNING PLANT
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P107'.
      APPEND t_selparams.
    ENDIF.

    IF x_defaults-zzunit2 IS NOT INITIAL.
      CLEAR t_selparams.
      t_selparams-kind    = 'S'.
      t_selparams-selname = 'IWERK'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'EQ'.
      t_selparams-low     = 'P103'.
      APPEND t_selparams.
    ENDIF.

    PERFORM sub_get_prog_name USING t_config-tcode
                              CHANGING l_program.
    IF t_config-repvar IS NOT INITIAL.
*    PERFORM sub_get_prog_name USING c_tcode_iw38
*                            CHANGING l_program.

      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_PMDUE

*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_MTBF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_mtbf .
  DATA :l_program  TYPE sy-cprog,
       l_space    TYPE char1,
       l_function TYPE zzfunction,
       l_ret_code TYPE sy-subrc.
  DATA : l_mm     TYPE c LENGTH 2,
           l_dd     TYPE c LENGTH 2,
           l_yy     TYPE c LENGTH 4,
           lv_date  TYPE spmon.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    LOOP AT s_strno.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_strno TO t_selparams.
      t_selparams-selname = 'STRNO'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.

    IF s_date[] IS NOT INITIAL.
      CLEAR t_selparams.
      l_yy = s_date-low+0(4).
      l_mm = s_date-low+4(2).
      l_dd = s_date-low+6(2).
      CONCATENATE l_yy l_mm  INTO  t_selparams-low.
      t_selparams-selname = 'SL_SPMON'.
      t_selparams-kind    = 'S'.
      t_selparams-sign    = 'I'.
      t_selparams-option  = 'BT'.
*    t_selparams-low = lv_date.
      l_yy = s_date-high+0(4).
      l_mm = s_date-high+4(2).
      l_dd = s_date-high+6(2).
      CONCATENATE l_yy l_mm  INTO  t_selparams-high.
      APPEND t_selparams.
    ENDIF.

    IF s_eqn[] IS NOT INITIAL.
      LOOP AT s_eqn.
        CLEAR t_selparams.
        MOVE-CORRESPONDING s_eqn TO t_selparams.
        t_selparams-selname = 'SL_0003'.
        t_selparams-kind    = 'S'.

        APPEND t_selparams.
      ENDLOOP.
    ENDIF.

*  PERFORM sub_get_prog_name USING c_tcode_mci7
*                             CHANGING l_program.
    PERFORM sub_get_prog_name USING t_config-tcode
                               CHANGING l_program.

    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report1 TABLES t_selparams
                                 USING  l_program
                                        t_config-repvar
                                        t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_HANDLE_MTBF        " SUB_HANDLE_MTBF
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_FLOC_PMIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_floc_pmis .

  DATA :l_program  TYPE sy-cprog,
       l_space    TYPE char1,
       l_function TYPE zzfunction,
       l_ret_code TYPE sy-subrc.

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.

    PERFORM sub_populate_seltab USING space space space space space.

    LOOP AT s_strno.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_strno TO t_selparams.
      t_selparams-selname = 'OSTRNO'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.

    PERFORM sub_get_prog_name USING t_config-tcode
                               CHANGING l_program.
*    PERFORM sub_get_prog_name USING c_tcode_mcjc
*                             CHANGING l_program.
    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report1 TABLES t_selparams
                                 USING  l_program
                                        t_config-repvar
                                        t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.
ENDFORM.                    " SUB_HANDLE_FLOC_PMIS


*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_EQN_PMIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_eqn_pmis .

  DATA :l_program  TYPE sy-cprog,
        l_space    TYPE char1,
        l_function TYPE zzfunction,
        l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams[].

  REFRESH :t_selparams[].
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode   " MCI7
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE text-003 TYPE 'I'. " You are not authorized to use this function
      RETURN.
    ENDIF.
    LOOP AT s_eqn.
      CLEAR t_selparams.
      MOVE-CORRESPONDING s_eqn TO t_selparams.
      t_selparams-selname = 'OEQUNR'.
      t_selparams-kind    = 'S'.
      APPEND t_selparams.
    ENDLOOP.

    PERFORM sub_get_prog_name USING t_config-tcode
                               CHANGING l_program.
*    PERFORM sub_get_prog_name USING c_tcode_mcjb
*                             CHANGING l_program.

    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                 USING  l_program
                                        t_config-repvar
                                        t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode does not exist in the table ZNTCONFIG'(000) TYPE 'I'.
  ENDIF.

ENDFORM.                    " SUB_HANDLE_EQN_PMIS
*&      Form  Call_Tcode_Varaint
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_tcode_varaint.

  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode  " IW38
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.
    PERFORM sub_get_prog_name USING   t_config-tcode
                                  CHANGING l_program.

    IF t_config-repvar IS NOT INITIAL.
      PERFORM sub_submit_report TABLES t_selparams
                                USING  l_program
                                       t_config-repvar
                                       t_config-autoexec.
    ELSE.
      CALL TRANSACTION t_config-tcode.
    ENDIF.

  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.
ENDFORM.                    "Call_Tcode_Varaint
*&      Form  Call_Tcode_Varaint
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM call_tcode.

  DATA : l_function TYPE zzfunction,
         l_program  TYPE sy-cprog,
         l_ret_code TYPE sy-subrc.
  REFRESH : t_selparams.
  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code
                               function = l_function.

  IF t_config-tcode IS NOT INITIAL.
*- checks whether user is authorised to use called transaction *-
    PERFORM sub_check_tcode_authority USING    t_config-tcode  " IW38
                                      CHANGING l_ret_code.
    IF l_ret_code <> c_authority_ok.
      MESSAGE s000 WITH text-003. " You are not authorized to use this function
      RETURN.
    ENDIF.
    PERFORM sub_get_prog_name USING   t_config-tcode
                                  CHANGING l_program.
    CALL TRANSACTION t_config-tcode.
*    PERFORM sub_submit_report TABLES t_selparams
*                              USING  l_program
*                                     t_config-repvar
*                                     t_config-autoexec.
  ELSE.
    MESSAGE 'Tcode is not exist in the table ZNTCONFIG' TYPE 'I'.
  ENDIF.
ENDFORM.                    "Call_Tcode_Varaint
