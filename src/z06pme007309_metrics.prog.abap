*----------------------------------------------------------------------*
***INCLUDE Z06PME007309_METRICS .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_ORD_AUART
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_ord_auart .

  TYPES : BEGIN OF ty_object,
    c1 TYPE string,
    c2 TYPE string,
    c3 TYPE string,
    c4 TYPE string,
    c5 TYPE string,
    c6 TYPE string,
    c7 TYPE string,
    c8 TYPE string,
    c9 TYPE string,
    c10 TYPE string,
    c11 TYPE string,
    c12 TYPE string,
    c13 TYPE string,
    c14 TYPE string,
    c15 TYPE string,
    c16 TYPE string,
    c17 TYPE string,
    c18 TYPE string,
    c19 TYPE string,
    c20 TYPE string,
    END OF ty_object.

  DATA : listtab LIKE abaplist OCCURS 1 ,
  wa_object TYPE ty_object,
  ta_object TYPE STANDARD TABLE OF ty_object.

  DATA : txtlines(20000) TYPE c OCCURS 0 WITH HEADER LINE.
  DATA :l_program  TYPE sy-cprog,
       l_space    TYPE char1,
       l_function TYPE zzfunction,
       l_ret_code TYPE sy-subrc.
  FIELD-SYMBOLS : <fs_order> TYPE ty_order,
                  <fs_field> TYPE any,

                  <fs_local> TYPE ty_object,
                  <fs_object> TYPE ty_object.
  DATA : lv_type_index TYPE string,"sy-index,
        lv_label TYPE string,
         lv_fl_index TYPE c."sy-index.

  REFRESH : t_selparams[].
  REFRESH : listtab[],
    txtlines[],
    ta_object[].
**- checks whether user is authorised to use called transaction *-
  PERFORM sub_check_tcode_authority USING    c_tcode_iw38   " iw38
                                    CHANGING l_ret_code.
  IF l_ret_code <> c_authority_ok.
    MESSAGE s000 WITH text-003. " You are not authorized to use this function
    RETURN.
  ENDIF.

*  PERFORM sub_populate_seltab USING space space
  PERFORM sub_populate_seltab USING 'GEWRK' space
                                        'S_AUART'   space
                                        'S_VPERNR'.

  LOOP AT s_eqn.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_eqn TO t_selparams.
    t_selparams-selname = 'EQUNR'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  LOOP AT s_strno.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_strno TO t_selparams.
    t_selparams-selname = 'STRNO'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  CLEAR t_selparams.
  t_selparams-selname = 'DY_OFN'.
  t_selparams-kind    = 'P'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
*  t_selparams-low     = ''.
  t_selparams-low     = 'X'.
  APPEND t_selparams.

  CLEAR t_selparams.
  t_selparams-selname = 'DY_IAR'.
  t_selparams-kind    = 'P'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
  t_selparams-low     = 'X'.
  APPEND t_selparams.

  IF x_defaults-zzunit1 IS NOT INITIAL .
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

  CLEAR t_selparams.
  t_selparams-kind    = 'P'.
  t_selparams-selname = 'DATUV'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
  t_selparams-low     = '00000000'.
  APPEND t_selparams.

  CLEAR t_selparams.
  t_selparams-kind    = 'P'.
  t_selparams-selname = 'DATUB'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
  t_selparams-low     = '99991231'.
  APPEND t_selparams.

***populate date
  LOOP AT s_date.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_date TO t_selparams.
    t_selparams-selname = 'ERDAT'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.


  PERFORM sub_get_prog_name USING c_tcode_iw38
                             CHANGING l_program.

  CALL FUNCTION 'LIST_FREE_MEMORY'.

  SUBMIT (l_program) USING SELECTION-SET t_config-repvar
                       WITH SELECTION-TABLE t_selparams
                       EXPORTING LIST TO MEMORY
                       AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = listtab
    EXCEPTIONS
      not_found  = 0
      OTHERS     = 0.

  CALL FUNCTION 'LIST_TO_ASCI'
    TABLES
      listobject         = listtab
      listasci           = txtlines
    EXCEPTIONS
      empty_list         = 0
      list_index_invalid = 0
      OTHERS             = 0.

  LOOP AT txtlines.
    SPLIT txtlines AT '|' INTO
    wa_object-c1 wa_object-c2 wa_object-c3
    wa_object-c4 wa_object-c5 wa_object-c6
    wa_object-c7 wa_object-c8 wa_object-c9
    wa_object-c10 wa_object-c11 wa_object-c12
    wa_object-c13 wa_object-c14 wa_object-c15
    wa_object-c16 wa_object-c17 wa_object-c18
    wa_object-c19 wa_object-c20.
    APPEND wa_object TO  ta_object.
  ENDLOOP.

  DELETE ta_object INDEX 1.
*Pick the index of type and flocation
  READ TABLE ta_object ASSIGNING <fs_local> INDEX 1.
  IF sy-subrc IS INITIAL.

    DO 20 TIMES.
      CHECK sy-index BETWEEN 0 AND 19.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_local> TO <fs_field>.
      IF <fs_field> IS NOT INITIAL.
        IF <fs_field> = 'Type' OR
           <fs_field> = 'Order Type'.
          lv_type_index = sy-index.
          CONDENSE lv_type_index.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.
*    IF  lv_type_index IS INITIAL.
    IF  lv_type_index IS INITIAL AND ta_object IS NOT INITIAL.
      MESSAGE 'Order Type does not exist in the DEFAULT Layout' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH : ta_order[].
  DELETE ta_object INDEX 1.
  LOOP AT ta_object ASSIGNING  <fs_object>.

    CLEAR lv_label.
    CONCATENATE 'C' lv_type_index  INTO lv_label.
    ASSIGN COMPONENT lv_label OF STRUCTURE <fs_object> TO <fs_field>.
    IF <fs_field> IS NOT INITIAL.
      wa_order-auart = <fs_field>.
      UNASSIGN <fs_field>.
    ENDIF.

    APPEND wa_order TO ta_order.
    CLEAR wa_order. " Added by Eldhose on 4/1/2015
  ENDLOOP.
*sort ta_order by auart.
*Check fcode for Order type
*If
  REFRESH ta_statistics[].
*collect the Order type data
  LOOP AT ta_order ASSIGNING <fs_order>.
    WRITE <fs_order>-auart TO ta_statistics-field.
    ta_statistics-count = 1.
    COLLECT ta_statistics.
  ENDLOOP.
  DELETE ta_statistics WHERE field = ''.
  SORT ta_statistics BY count DESCENDING.

ENDFORM.                    " SUB_HANDLE_ORD_AUART

*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_ORD_FLOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_ord_floc .

  TYPES : BEGIN OF ty_object,
    c1 TYPE string,
    c2 TYPE string,
    c3 TYPE string,
    c4 TYPE string,
    c5 TYPE string,
    c6 TYPE string,
    c7 TYPE string,
    c8 TYPE string,
    c9 TYPE string,
    c10 TYPE string,
    c11 TYPE string,
    c12 TYPE string,
    c13 TYPE string,
    c14 TYPE string,
    c15 TYPE string,
    c16 TYPE string,
    c17 TYPE string,
    c18 TYPE string,
    c19 TYPE string,
    c20 TYPE string,
    END OF ty_object.

  DATA : listtab LIKE abaplist OCCURS 1 ,
  wa_object TYPE ty_object,
  ta_object TYPE STANDARD TABLE OF ty_object.

  DATA : txtlines(20000) TYPE c OCCURS 0 WITH HEADER LINE.
  DATA :l_program  TYPE sy-cprog,
       l_space    TYPE char1,
       l_function TYPE zzfunction,
       l_ret_code TYPE sy-subrc.
  FIELD-SYMBOLS : <fs_order> TYPE ty_order,
                  <fs_field> TYPE any,

                  <fs_local> TYPE ty_object,
                  <fs_object> TYPE ty_object.
  DATA : lv_type_index TYPE c,"sy-index,
        lv_label TYPE string,
         lv_fl_index TYPE string."sy-index.
  REFRESH t_selparams[].
  REFRESH : listtab[],
   txtlines[],
   ta_object[].
*- checks whether user is authorised to use called transaction *-
  PERFORM sub_check_tcode_authority USING    c_tcode_iw38   " MCI7
                                    CHANGING l_ret_code.
  IF l_ret_code <> c_authority_ok.
    MESSAGE s000 WITH text-003. " You are not authorized to use this function
    RETURN.
  ENDIF.

  PERFORM sub_populate_seltab USING 'GEWRK' space
                                        'S_AUART'   space
                                        'S_VPERNR'.

  LOOP AT s_eqn.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_eqn TO t_selparams.
    t_selparams-selname = 'EQUNR'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  LOOP AT s_strno.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_strno TO t_selparams.
    t_selparams-selname = 'STRNO'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  CLEAR t_selparams.
  t_selparams-selname = 'DY_OFN'.
  t_selparams-kind    = 'P'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
*  t_selparams-low     = ''.
  t_selparams-low     = 'X'.
  APPEND t_selparams.
  CLEAR t_selparams.
  t_selparams-selname = 'DY_IAR'.
  t_selparams-kind    = 'P'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
  t_selparams-low     = 'X'.
  APPEND t_selparams.
  CLEAR t_selparams.

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

  CLEAR t_selparams.
  t_selparams-kind    = 'P'.
  t_selparams-selname = 'DATUV'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
  t_selparams-low     = '00000000'.
  APPEND t_selparams.

  CLEAR t_selparams.
  t_selparams-kind    = 'P'.
  t_selparams-selname = 'DATUB'.
  t_selparams-sign    = 'I'.
  t_selparams-option  = 'EQ'.
  t_selparams-low     = '99991231'.
  APPEND t_selparams.

***populate date
  LOOP AT s_date.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_date TO t_selparams.
    t_selparams-selname = 'ERDAT'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  PERFORM sub_get_prog_name USING c_tcode_iw38
                             CHANGING l_program.

  CALL FUNCTION 'LIST_FREE_MEMORY'.

  SUBMIT (l_program) USING SELECTION-SET t_config-repvar
                         WITH SELECTION-TABLE t_selparams
                         EXPORTING LIST TO MEMORY
                         AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = listtab
    EXCEPTIONS
      not_found  = 0
      OTHERS     = 0.

  CALL FUNCTION 'LIST_TO_ASCI'
    TABLES
      listobject         = listtab
      listasci           = txtlines
    EXCEPTIONS
      empty_list         = 0
      list_index_invalid = 0
      OTHERS             = 0.

  LOOP AT txtlines.
    SPLIT txtlines AT '|' INTO
    wa_object-c1 wa_object-c2 wa_object-c3
    wa_object-c4 wa_object-c5 wa_object-c6
    wa_object-c7 wa_object-c8 wa_object-c9
    wa_object-c10 wa_object-c11 wa_object-c12
    wa_object-c13 wa_object-c14 wa_object-c15
    wa_object-c16 wa_object-c17 wa_object-c18
    wa_object-c19 wa_object-c20.
    APPEND wa_object TO  ta_object.
  ENDLOOP.
  DELETE ta_object INDEX 1.

*Pick the index of type and flocation
  READ TABLE ta_object ASSIGNING <fs_local> INDEX 1.
  IF sy-subrc IS INITIAL.
    DO 20 TIMES.
      CHECK sy-index BETWEEN 0 AND 19.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_local> TO <fs_field>.

      IF <fs_field> IS NOT  INITIAL.
        IF <fs_field> = 'Functional Location' OR
           <fs_field> = 'Func. Loc.' OR
           <fs_field> = 'Functional loc.'.

          lv_fl_index = sy-index.
          CONDENSE lv_fl_index.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.

    IF lv_fl_index IS INITIAL AND ta_object IS NOT INITIAL.
      MESSAGE 'Functional Location does not exist in the DEFAULT Layout' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH : ta_order[].
  DELETE ta_object INDEX 1.
  LOOP AT ta_object ASSIGNING  <fs_object>.

    CLEAR : lv_label.
    CONCATENATE 'C' lv_fl_index  INTO lv_label.
    ASSIGN COMPONENT lv_label OF STRUCTURE <fs_object> TO <fs_field>.
    IF <fs_field> IS NOT INITIAL.
      wa_order-tplnr = <fs_field>.
      UNASSIGN <fs_field>.
    ENDIF.
    APPEND wa_order TO ta_order.
    CLEAR wa_order. " Added by Eldhose on 4/1/2015

  ENDLOOP.

  DELETE ta_order WHERE tplnr = ''.
  SORT ta_order BY tplnr.

*Check fcode for functional Location
  REFRESH ta_statistics[].
*collect the Flocation Data
  LOOP AT ta_order ASSIGNING <fs_order>.
    WRITE <fs_order>-tplnr TO ta_statistics-field.
    ta_statistics-count = 1.
    COLLECT ta_statistics.
  ENDLOOP.
  DELETE ta_statistics WHERE field = ''.
  SORT ta_statistics BY count DESCENDING.

ENDFORM.                    " SUB_HANDLE_ORD_FLOC

*&---------------------------------------------------------------------*
*&      Form  SUB_HANDLE_ORD_FAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sub_handle_ord_fail .

  TYPES : BEGIN OF ty_object,
    c1 TYPE string,
    c2 TYPE string,
    c3 TYPE string,
    c4 TYPE string,
    c5 TYPE string,
    c6 TYPE string,
    c7 TYPE string,
    c8 TYPE string,
    c9 TYPE string,
    c10 TYPE string,
    c11 TYPE string,
    c12 TYPE string,
    c13 TYPE string,
    c14 TYPE string,
    c15 TYPE string,
    c16 TYPE string,
    c17 TYPE string,
    c18 TYPE string,
    c19 TYPE string,
    c20 TYPE string,
    END OF ty_object.

  DATA : listtab LIKE abaplist OCCURS 1 ,
  wa_object TYPE ty_object,
  ta_object TYPE STANDARD TABLE OF ty_object.

  DATA : txtlines(20000) TYPE c OCCURS 0 WITH HEADER LINE.
  DATA :l_program  TYPE sy-cprog,
       l_space    TYPE char1,
       l_function TYPE zzfunction,
       l_ret_code TYPE sy-subrc.
  FIELD-SYMBOLS : <fs_notif> TYPE ty_notif,
                   <fs_field> TYPE any,
                   <fs_local> TYPE ty_object,
                   <fs_object> TYPE ty_object.
  DATA : lv_type_index TYPE string,"sy-index,
        lv_label TYPE string,
         lv_fl_index TYPE string."sy-index.
  REFRESH t_selparams[].
*- checks whether user is authorised to use called transaction *-
  PERFORM sub_check_tcode_authority USING    c_tcode_iw65   " iw65
                                    CHANGING l_ret_code.
  IF l_ret_code <> c_authority_ok.
    MESSAGE s000 WITH text-003. " You are not authorized to use this function
    RETURN.
  ENDIF.

  PERFORM sub_populate_seltab USING 'ARBPL' space
                                     space  'QMART'
                                     space.

  LOOP AT s_eqn.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_eqn TO t_selparams.
    t_selparams-selname = 'EQUNR'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  LOOP AT s_strno.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_strno TO t_selparams.
    t_selparams-selname = 'STRNO'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  IF x_defaults-zzunit1 IS NOT INITIAL .

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

***populate date
  LOOP AT s_date.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_date TO t_selparams.
    t_selparams-selname = 'ERDAT'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.


  PERFORM sub_get_prog_name USING c_tcode_iw65
                             CHANGING l_program.

  CALL FUNCTION 'LIST_FREE_MEMORY'.

  SUBMIT (l_program) USING SELECTION-SET t_config-repvar
                           WITH SELECTION-TABLE t_selparams
                           EXPORTING LIST TO MEMORY
                           AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = listtab
    EXCEPTIONS
      not_found  = 0
      OTHERS     = 0.

  CALL FUNCTION 'LIST_TO_ASCI'
    TABLES
      listobject         = listtab
      listasci           = txtlines
    EXCEPTIONS
      empty_list         = 0
      list_index_invalid = 0
      OTHERS             = 0.

  LOOP AT txtlines.
    SPLIT txtlines AT '|' INTO
    wa_object-c1 wa_object-c2 wa_object-c3
    wa_object-c4 wa_object-c5 wa_object-c6
    wa_object-c7 wa_object-c8 wa_object-c9
    wa_object-c10 wa_object-c11 wa_object-c12
    wa_object-c13 wa_object-c14 wa_object-c15
    wa_object-c16 wa_object-c17 wa_object-c18
    wa_object-c19 wa_object-c20.
    APPEND wa_object TO  ta_object.
  ENDLOOP.

  DELETE ta_object INDEX 1.
*Pick the index of type and flocation
  READ TABLE ta_object ASSIGNING <fs_local> INDEX 1.
  IF sy-subrc IS INITIAL.
    DO 20 TIMES.
      CHECK sy-index BETWEEN 0 AND 19.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_local> TO <fs_field>.
      IF <fs_field> = 'CgrCode' OR
         <fs_field> = 'Code group' OR
         <fs_field> = 'Coding'.
        lv_type_index = sy-index.
        CONDENSE lv_type_index.
        EXIT.
      ENDIF.
    ENDDO.
*    IF  lv_type_index IS INITIAL.
    IF  lv_type_index IS INITIAL AND ta_object IS NOT INITIAL.
      MESSAGE 'Code group does not exist in the DEFAULT Layout' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH : ta_notif[].
  DELETE ta_object INDEX 1.
  LOOP AT ta_object ASSIGNING  <fs_object>.

    CLEAR lv_label.
    CONCATENATE 'C' lv_type_index  INTO lv_label.
    ASSIGN COMPONENT lv_label OF STRUCTURE <fs_object> TO <fs_field>.
    IF <fs_field> IS NOT INITIAL.
      wa_notif-qmart = <fs_field>.
      UNASSIGN <fs_field>.
    ENDIF.
    APPEND wa_notif TO ta_notif.
    CLEAR wa_notif.  " Added by Eldhose on 4/1/2015
  ENDLOOP.


  REFRESH ta_statistics[].
  IF ok_code = 'F67'.
*collect the Flocation Data
    LOOP AT ta_notif ASSIGNING <fs_notif>.
      WRITE <fs_notif>-qmart TO ta_statistics-field.
      ta_statistics-count = 1.
      COLLECT ta_statistics.
    ENDLOOP.
    DELETE ta_statistics WHERE field = ''.

  ENDIF.
  SORT ta_statistics BY count DESCENDING.

ENDFORM.                    " SUB_HANDLE_ORD_FAIL

*&---------------------------------------------------------------------*
*&      Form  sub_handle_ord_fail_loc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM sub_handle_ord_fail_loc .

  TYPES : BEGIN OF ty_object,
    c1 TYPE string,
    c2 TYPE string,
    c3 TYPE string,
    c4 TYPE string,
    c5 TYPE string,
    c6 TYPE string,
    c7 TYPE string,
    c8 TYPE string,
    c9 TYPE string,
    c10 TYPE string,
    c11 TYPE string,
    c12 TYPE string,
    c13 TYPE string,
    c14 TYPE string,
    c15 TYPE string,
    c16 TYPE string,
    c17 TYPE string,
    c18 TYPE string,
    c19 TYPE string,
    c20 TYPE string,
    END OF ty_object.

  DATA : listtab LIKE abaplist OCCURS 1 ,
  wa_object TYPE ty_object,
  ta_object TYPE STANDARD TABLE OF ty_object.

  DATA : txtlines(20000) TYPE c OCCURS 0 WITH HEADER LINE.
  DATA :l_program  TYPE sy-cprog,
       l_space    TYPE char1,
       l_function TYPE zzfunction,
       l_ret_code TYPE sy-subrc.
  FIELD-SYMBOLS : <fs_notif> TYPE ty_notif,
                   <fs_field> TYPE any,
                   <fs_local> TYPE ty_object,
                   <fs_object> TYPE ty_object.
  DATA : lv_type_index TYPE string,"sy-index,
        lv_label TYPE string,
         lv_fl_index TYPE string."sy-index.
  REFRESH t_selparams[].
*- checks whether user is authorised to use called transaction *-
  PERFORM sub_check_tcode_authority USING    c_tcode_iw65   " iw65
                                    CHANGING l_ret_code.
  IF l_ret_code <> c_authority_ok.
    MESSAGE s000 WITH text-003. " You are not authorized to use this function
    RETURN.
  ENDIF.

*  PERFORM sub_populate_seltab USING space space
  PERFORM sub_populate_seltab USING 'ARBPL' space
                                       space  'QMART'
                                       space.

  LOOP AT s_eqn.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_eqn TO t_selparams.
    t_selparams-selname = 'EQUNR'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  LOOP AT s_strno.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_strno TO t_selparams.
    t_selparams-selname = 'STRNO'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  IF x_defaults-zzunit1 IS NOT INITIAL .

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

***populate date
  LOOP AT s_date.
    CLEAR t_selparams.
    MOVE-CORRESPONDING s_date TO t_selparams.
    t_selparams-selname = 'ERDAT'.
    t_selparams-kind    = 'S'.
    APPEND t_selparams.
  ENDLOOP.

  CLEAR t_config.
  READ TABLE t_config WITH KEY fcode    = ok_code.

  PERFORM sub_get_prog_name USING c_tcode_iw65
                             CHANGING l_program.

  CALL FUNCTION 'LIST_FREE_MEMORY'.

  SUBMIT (l_program) USING SELECTION-SET t_config-repvar
                           WITH SELECTION-TABLE t_selparams
                           EXPORTING LIST TO MEMORY
                           AND RETURN.

  CALL FUNCTION 'LIST_FROM_MEMORY'
    TABLES
      listobject = listtab
    EXCEPTIONS
      not_found  = 0
      OTHERS     = 0.

  CALL FUNCTION 'LIST_TO_ASCI'
    TABLES
      listobject         = listtab
      listasci           = txtlines
    EXCEPTIONS
      empty_list         = 0
      list_index_invalid = 0
      OTHERS             = 0.

  LOOP AT txtlines.
    SPLIT txtlines AT '|' INTO
    wa_object-c1 wa_object-c2 wa_object-c3
    wa_object-c4 wa_object-c5 wa_object-c6
    wa_object-c7 wa_object-c8 wa_object-c9
    wa_object-c10 wa_object-c11 wa_object-c12
    wa_object-c13 wa_object-c14 wa_object-c15
    wa_object-c16 wa_object-c17 wa_object-c18
    wa_object-c19 wa_object-c20.
    APPEND wa_object TO  ta_object.
  ENDLOOP.

  DELETE ta_object INDEX 1.
*Pick the index of type and flocation
  READ TABLE ta_object ASSIGNING <fs_local> INDEX 1.
  IF sy-subrc IS INITIAL.
    DO 20 TIMES.
      CHECK sy-index BETWEEN 0 AND 19.
      ASSIGN COMPONENT sy-index OF STRUCTURE <fs_local> TO <fs_field>.
      IF <fs_field> = 'Functional Location' OR
         <fs_field> = 'Func. Loc.' OR
         <fs_field> = 'Functional loc.'.
        lv_fl_index = sy-index.
        CONDENSE lv_fl_index.
        EXIT.
      ENDIF.
    ENDDO.
*    IF lv_fl_index IS INITIAL.
    IF lv_fl_index IS INITIAL AND ta_object IS NOT INITIAL.
      MESSAGE 'Functional Location does not exist in the DEFAULT Layout' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  REFRESH : ta_notif[].
  DELETE ta_object INDEX 1.
  LOOP AT ta_object ASSIGNING  <fs_object>.
    CLEAR : lv_label.
    CONCATENATE 'C' lv_fl_index  INTO lv_label.
    ASSIGN COMPONENT lv_label OF STRUCTURE <fs_object> TO <fs_field>.
    IF <fs_field> IS NOT INITIAL.
      wa_notif-tplnr = <fs_field>.
      UNASSIGN <fs_field>.
    ENDIF.
    APPEND wa_notif TO ta_notif.
    CLEAR wa_notif. " Added by Eldhose on 4/1/2015
  ENDLOOP.


  REFRESH ta_statistics[].
  IF ok_code = 'F67'.
*collect the Flocation Data
    LOOP AT ta_notif ASSIGNING <fs_notif>.
      WRITE <fs_notif>-qmart TO ta_statistics-field.
      ta_statistics-count = 1.
      COLLECT ta_statistics.
    ENDLOOP.
    DELETE ta_statistics WHERE field = ''.

  ELSEIF ok_code = 'F68'.
*Check fcode for functional Location
    DELETE ta_notif WHERE tplnr = ''.
*Check fcode for functional Location
    REFRESH ta_statistics[].
*collect the Flocation Data
    LOOP AT ta_notif ASSIGNING <fs_notif>.
      WRITE <fs_notif>-tplnr TO ta_statistics-field.
      ta_statistics-count = 1.
      COLLECT ta_statistics.
    ENDLOOP.
    DELETE ta_statistics WHERE field = ''.

  ENDIF.
  SORT ta_statistics BY count DESCENDING.
ENDFORM.                    " SUB_HANDLE_ORD_FAIL
