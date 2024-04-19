*&---------------------------------------------------------------------*
*&  Include           ZLPMR030_EQUI_CHANGE_REP_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLPMR030_EQUI_CHANGE_REP_F01                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 04-Aug-2017                                    *
*& Object ID          : ACR-4556                                       *
*& Application Area   : PM                                             *
*& Description        : Equipment change report.                       *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_EQUI_CHANGE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_equi_change_data .

  IF rb_equi IS NOT INITIAL.
    PERFORM get_equi_data.
  ELSEIF rb_class IS NOT INITIAL.
    PERFORM get_class_data.
  ENDIF.

ENDFORM.                    " GET_EQUI_CHANGE_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_output_data .

  IF rb_equi IS NOT INITIAL.
    PERFORM get_equi_output.
  ELSEIF rb_class IS NOT INITIAL.
    PERFORM get_class_output.
  ENDIF.

ENDFORM.                    " GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_data .

  DATA: lr_salv_table TYPE REF TO cl_salv_table,
        lr_columns    TYPE REF TO cl_salv_columns_list,
        lr_column     TYPE REF TO cl_salv_column,
        lr_aggreg     TYPE REF TO cl_salv_aggregations,
        lta_fcat      TYPE lvc_t_fcat,
        lr_display    TYPE REF TO cl_salv_display_settings,
        lr_func       TYPE REF TO cl_salv_functions_list.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = lr_salv_table
        CHANGING
          t_table      = ta_final[].
    CATCH cx_salv_msg .
  ENDTRY.

  lr_columns = lr_salv_table->get_columns( ).

  TRY.
      lr_columns->set_optimize( if_salv_c_bool_sap=>true ).

      lr_column = lr_columns->get_column( columnname = 'EQUNR' ).
      lr_column->set_short_text( 'Equi No.' ).
      lr_column->set_medium_text( 'Equi Number' ).
      lr_column->set_long_text( 'Equipment Number' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'EQKTX' ).
      lr_column->set_short_text( 'Equi Desc.' ).
      lr_column->set_medium_text( 'Equi Description' ).
      lr_column->set_long_text( 'Equipment Description' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'EQART' ).
      lr_column->set_short_text( 'Obj Type' ).
      lr_column->set_medium_text( 'Tech Obj Type' ).
      lr_column->set_long_text( 'Tech Obj Type' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'TPLNR' ).
      lr_column->set_short_text( 'Floc.' ).
      lr_column->set_medium_text( 'Functional loc.' ).
      lr_column->set_long_text( 'Functional Location' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'ARBPL' ).
      lr_column->set_short_text( 'Work Ctr.' ).
      lr_column->set_medium_text( 'Work Center' ).
      lr_column->set_long_text( 'Work Center' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'USERNAME' ).
      lr_column->set_short_text( 'User' ).
      lr_column->set_medium_text( 'User' ).
      lr_column->set_long_text( 'User' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'TCODE' ).
      lr_column->set_medium_text( 'TCode' ).
      lr_column->set_long_text( 'TCode' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'UDATE' ).
      lr_column->set_medium_text( 'Date' ).
      lr_column->set_long_text( 'Date' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'UTIME' ).
      lr_column->set_medium_text( 'Time' ).
      lr_column->set_long_text( 'Time' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'FNAME' ).
      lr_column->set_medium_text( 'Field' ).
      lr_column->set_long_text( 'Field' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'VALUE_OLD' ).
      lr_column->set_medium_text( 'Old Value' ).
      lr_column->set_long_text( 'Old Value' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.
  TRY.
      lr_column = lr_columns->get_column( columnname = 'VALUE_NEW' ).
      lr_column->set_medium_text( 'New Value' ).
      lr_column->set_long_text( 'New Value' ).
    CATCH cx_salv_data_error.
    CATCH cx_salv_not_found .
  ENDTRY.

  lr_aggreg   = lr_salv_table->get_aggregations( ).
  lta_fcat    =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                r_columns      = lr_columns
                                r_aggregations = lr_aggreg ).

  "Functions
  lr_func = lr_salv_table->get_functions( ).
  lr_func->set_all( ).

  lr_display = lr_salv_table->get_display_settings( ).

  lr_salv_table->display( ).

ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_EQUI_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_equi_data .
  TYPES: BEGIN OF lty_objectid,
           objectid TYPE equnr,
         END OF lty_objectid.
  DATA: lwa_cond(72)      TYPE c,
        lta_cond          LIKE TABLE OF lwa_cond,
        lta_objectid      TYPE STANDARD TABLE OF lty_objectid,
        lwa_objectid      TYPE lty_objectid,
        lta_fname         TYPE STANDARD TABLE OF ty_cdpos,
        lta_v_equi_tmp    TYPE STANDARD TABLE OF ty_v_equi.

* Build dynamic WHERE clause
  CONCATENATE 'OBJECTCLAS' 'EQ' 'CO_EQUI' INTO lwa_cond SEPARATED BY space.
  APPEND lwa_cond TO lta_cond.
  CLEAR lwa_cond.
  IF pa_uname IS NOT INITIAL.
    CONCATENATE 'AND' 'USERNAME' 'EQ' 'PA_UNAME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_fdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'GE' 'PA_FDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_tdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'LE' 'PA_TDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ftime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'GE' 'PA_FTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ttime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'LE' 'PA_TTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.

* Get the change history header details for equipment
  SELECT objectclas
         objectid
         changenr
         username
         udate
         utime
         tcode
         FROM cdhdr
         INTO TABLE ta_cdhdr
         WHERE (lta_cond).

  IF sy-subrc = 0.
    SORT ta_cdhdr BY objectclas objectid changenr.
  ENDIF.
  IF ta_cdhdr IS NOT INITIAL.
*   Get the change history item details for equipment
    SELECT objectclas
           objectid
           changenr
           fname
           value_new
           value_old
           FROM cdpos
           INTO TABLE ta_cdpos
           FOR ALL ENTRIES IN ta_cdhdr
           WHERE objectclas EQ ta_cdhdr-objectclas
           AND   objectid EQ ta_cdhdr-objectid
           AND   changenr EQ ta_cdhdr-changenr
           AND   fname IN (co_herst, co_typbz, co_serge, co_mapar)
           AND   chngind EQ 'U'.
    IF sy-subrc = 0.
      SORT ta_cdpos BY objectclas objectid changenr.
    ENDIF.
    IF ta_cdpos IS NOT INITIAL.
      lta_fname[] = ta_cdpos[].
      SORT lta_fname BY fname.
      DELETE ADJACENT DUPLICATES FROM lta_fname COMPARING fname.
*     Get field name descriptions
      SELECT rollname
             scrtext_m
             FROM dd04t
             INTO TABLE ta_dd04t
             FOR ALL ENTRIES IN lta_fname
             WHERE rollname = lta_fname-fname
             AND   ddlanguage = 'EN'
             AND   as4local = 'A'.
      IF sy-subrc = 0.
        SORT ta_dd04t BY rollname.
      ENDIF.
    ENDIF.

    LOOP AT ta_cdhdr INTO wa_cdhdr.
      lwa_objectid-objectid = wa_cdhdr-objectid.
      APPEND lwa_objectid TO lta_objectid.
      CLEAR: lwa_objectid,
             wa_cdhdr.
    ENDLOOP.
    SORT lta_objectid BY objectid.
    DELETE ADJACENT DUPLICATES FROM lta_objectid COMPARING objectid.
*   Get equipment details
    SELECT equnr
           eqart
           gewrk
           eqktx
           tplnr
           FROM v_equi
           INTO TABLE ta_v_equi
           FOR ALL ENTRIES IN lta_objectid
           WHERE equnr = lta_objectid-objectid
           AND   eqart IN so_eqart
           AND   datbi GE sy-datum
           AND   iwerk = pa_werk.
    IF sy-subrc = 0.
      SORT ta_v_equi BY equnr eqart.
    ENDIF.
    IF ta_v_equi IS NOT INITIAL.
      lta_v_equi_tmp[] = ta_v_equi[].
      SORT lta_v_equi_tmp BY gewrk.
      DELETE ADJACENT DUPLICATES FROM lta_v_equi_tmp COMPARING gewrk.
*     Get Main work center description
      SELECT objid
             arbpl
             FROM crhd
             INTO TABLE ta_crhd
             FOR ALL ENTRIES IN lta_v_equi_tmp
             WHERE objty = 'A'
             AND   objid = lta_v_equi_tmp-gewrk.
      IF sy-subrc = 0.
        SORT ta_crhd BY objid.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_EQUI_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_CLASS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_class_data .
  TYPES: BEGIN OF lty_cuobj,
           cuobj TYPE cuobj,
         END OF lty_cuobj,
         BEGIN OF lty_atinn,
           atinn TYPE atinn,
         END OF lty_atinn,
         BEGIN OF lty_objek,
           objek TYPE equnr,
         END OF lty_objek.

  DATA: lwa_cond(72)     TYPE c,
        lwa_cond_tmp(72) TYPE c,
        lta_cond         LIKE TABLE OF lwa_cond,
        lta_cuobj        TYPE STANDARD TABLE OF lty_cuobj,
        lwa_cuobj        TYPE lty_cuobj,
        lta_atinn        TYPE STANDARD TABLE OF lty_atinn,
        lwa_atinn        TYPE lty_atinn,
        lta_objek        TYPE STANDARD TABLE OF lty_objek,
        lwa_objek        TYPE lty_objek,
        lta_v_equi_tmp   TYPE STANDARD TABLE OF ty_v_equi.

* Build dynamic WHERE clause
  CONCATENATE 'OBJECTCLAS' 'EQ' 'CO_CLASSIFY' INTO lwa_cond SEPARATED BY space.
  APPEND lwa_cond TO lta_cond.
  CLEAR lwa_cond.
  IF pa_uname IS NOT INITIAL.
    CONCATENATE 'AND' 'USERNAME' 'EQ' 'PA_UNAME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_fdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'GE' 'PA_FDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_tdate IS NOT INITIAL.
    CONCATENATE 'AND' 'UDATE' 'LE' 'PA_TDATE' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ftime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'GE' 'PA_FTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  IF pa_ttime IS NOT INITIAL.
    CONCATENATE 'AND' 'UTIME' 'LE' 'PA_TTIME' INTO lwa_cond SEPARATED BY space.
    APPEND lwa_cond TO lta_cond.
    CLEAR  lwa_cond.
  ENDIF.
  CONCATENATE 'AND' 'TCODE' 'IN' INTO lwa_cond_tmp SEPARATED BY space.
  CONCATENATE '(' 'CO_IE02' ',' INTO lwa_cond.
  CONCATENATE lwa_cond_tmp lwa_cond INTO lwa_cond SEPARATED BY space.
  CLEAR lwa_cond_tmp.
  CONCATENATE 'SPACE' ')' INTO lwa_cond_tmp.
  CONCATENATE lwa_cond lwa_cond_tmp INTO lwa_cond SEPARATED BY space.

  APPEND lwa_cond TO lta_cond.
  CLEAR  lwa_cond.

* Get the change history header details for characteristics
  SELECT objectclas
         objectid
         changenr
         username
         udate
         utime
         tcode
         FROM cdhdr
         INTO TABLE ta_cdhdr
         WHERE (lta_cond).

  IF sy-subrc = 0.
    SORT ta_cdhdr BY objectclas objectid changenr.
  ENDIF.
  IF ta_cdhdr IS NOT INITIAL.
*   Get the change history item details for characteristics
    SELECT objectclas
           objectid
           changenr
           tabkey
           fname
           value_new
           value_old
           FROM cdpos
           INTO TABLE ta_cdpos_cl
           FOR ALL ENTRIES IN ta_cdhdr
           WHERE objectclas EQ ta_cdhdr-objectclas
           AND   objectid EQ ta_cdhdr-objectid
           AND   changenr EQ ta_cdhdr-changenr
           AND   fname IN (co_atwrt, co_atflv)
           AND   chngind EQ 'U'.
    IF sy-subrc = 0.
      SORT ta_cdpos BY objectclas objectid changenr.
    ENDIF.
  ENDIF.
  IF ta_cdpos_cl IS NOT INITIAL.
    LOOP AT ta_cdpos_cl INTO wa_cdpos_cl.
      lwa_cuobj-cuobj = wa_cdpos_cl-tabkey+0(18).
      lwa_atinn-atinn = wa_cdpos_cl-tabkey+18(10).
      APPEND lwa_cuobj TO lta_cuobj.
      APPEND lwa_atinn TO lta_atinn.
      CLEAR: lwa_cuobj,
             lwa_atinn,
             wa_cdpos_cl.
    ENDLOOP.
    SORT lta_cuobj BY cuobj.
    DELETE ADJACENT DUPLICATES FROM lta_cuobj COMPARING cuobj.
*   Get the equipment number
    IF lta_cuobj IS NOT INITIAL.
      SELECT cuobj
             objek
             FROM inob
             INTO TABLE ta_inob
             FOR ALL ENTRIES IN lta_cuobj
             WHERE cuobj = lta_cuobj-cuobj
             AND   klart = co_klart. "'002'.
      IF sy-subrc = 0.
        SORT ta_inob BY objek.
      ENDIF.
    ENDIF.
    SORT lta_atinn BY atinn.
    DELETE ADJACENT DUPLICATES FROM lta_atinn COMPARING atinn.
*   Get the characteristic name
    IF lta_atinn IS NOT INITIAL.
      SELECT atinn
             atnam
             atfor
             FROM cabn
             INTO TABLE ta_cabn
             FOR ALL ENTRIES IN lta_atinn
             WHERE atinn = lta_atinn-atinn.
      IF sy-subrc = 0.
        SORT ta_cabn BY atinn.
      ENDIF.
    ENDIF.
  ENDIF.
  IF ta_inob IS NOT INITIAL.
    LOOP AT ta_inob INTO wa_inob.
      lwa_objek-objek = wa_inob-objek.
      APPEND lwa_objek TO lta_objek.
      CLEAR: lwa_objek,
             wa_inob.
    ENDLOOP.
    SORT lta_objek BY objek.
*   Get the equipment details
    SELECT equnr
           eqart
           gewrk
           eqktx
           tplnr
           FROM v_equi
           INTO TABLE ta_v_equi
           FOR ALL ENTRIES IN lta_objek
           WHERE equnr = lta_objek-objek
           AND   eqart IN so_eqart
           AND   datbi GE sy-datum
           AND   iwerk = pa_werk.
    IF sy-subrc = 0.
      SORT ta_v_equi BY equnr eqart.
    ENDIF.
    IF ta_v_equi IS NOT INITIAL.
      lta_v_equi_tmp[] = ta_v_equi[].
      SORT lta_v_equi_tmp BY gewrk.
      DELETE ADJACENT DUPLICATES FROM lta_v_equi_tmp COMPARING gewrk.
*     Get Main work center description
      SELECT objid
             arbpl
             FROM crhd
             INTO TABLE ta_crhd
             FOR ALL ENTRIES IN lta_v_equi_tmp
             WHERE objty = 'A'
             AND   objid = lta_v_equi_tmp-gewrk.
      IF sy-subrc = 0.
        SORT ta_crhd BY objid.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " GET_CLASS_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_EQUI_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_equi_output .

  LOOP AT ta_cdpos INTO wa_cdpos.
    CLEAR wa_final.
    wa_final-value_new = wa_cdpos-value_new.
    wa_final-value_old = wa_cdpos-value_old.
    CLEAR wa_dd04t.
    READ TABLE ta_dd04t INTO wa_dd04t WITH KEY rollname = wa_cdpos-fname.
    IF sy-subrc = 0.
      wa_final-fname = wa_dd04t-scrtext_m.
    ENDIF.
    CLEAR wa_cdhdr.
    READ TABLE ta_cdhdr INTO wa_cdhdr WITH KEY objectclas = wa_cdpos-objectclas
                                               objectid = wa_cdpos-objectid
                                               changenr = wa_cdpos-changenr.
    IF sy-subrc = 0.
      wa_final-username = wa_cdhdr-username.
      wa_final-tcode = wa_cdhdr-tcode.
      wa_final-udate = wa_cdhdr-udate.
      wa_final-utime = wa_cdhdr-utime.
    ENDIF.
    CLEAR wa_v_equi.
    READ TABLE ta_v_equi INTO wa_v_equi WITH KEY equnr = wa_cdpos-objectid.
    IF sy-subrc = 0.
      IF wa_v_equi-eqart IN so_eqart.
        CLEAR wa_crhd.
        READ TABLE ta_crhd INTO wa_crhd WITH KEY objid = wa_v_equi-gewrk.
        IF sy-subrc = 0.
          wa_final-arbpl = wa_crhd-arbpl.
        ENDIF.
        wa_final-equnr = wa_v_equi-equnr.
        wa_final-eqktx = wa_v_equi-eqktx.
        wa_final-eqart = wa_v_equi-eqart.
        wa_final-tplnr = wa_v_equi-tplnr.
      ELSE.
        CONTINUE.
      ENDIF.
    ELSE.
      CONTINUE.
    ENDIF.

    APPEND wa_final TO ta_final.
    CLEAR: wa_final,
           wa_cdpos.
  ENDLOOP.
  SORT ta_final BY username udate utime.

ENDFORM.                    " GET_EQUI_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  GET_CLASS_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_class_output .
  DATA: ltp_char10 TYPE char10,
        ltp_atflv  TYPE atflv,
        ltp_dec    TYPE i.

  LOOP AT ta_cdpos_cl INTO wa_cdpos_cl.
    CLEAR wa_final.
    wa_final-value_new = wa_cdpos_cl-value_new.
    wa_final-value_old = wa_cdpos_cl-value_old.
    CLEAR wa_cabn.
    READ TABLE ta_cabn INTO wa_cabn WITH KEY atinn = wa_cdpos_cl-tabkey+18(10).
    IF sy-subrc = 0.
      wa_final-fname = wa_cabn-atnam.
      CASE wa_cabn-atfor.
        WHEN 'DATE'.
          ltp_atflv = wa_cdpos_cl-value_new.
          WRITE ltp_atflv TO ltp_char10 EXPONENT 0 DECIMALS 0.
          CONDENSE ltp_char10.
          WRITE ltp_char10 TO wa_final-value_new.
          CLEAR ltp_char10.
          ltp_atflv = wa_cdpos_cl-value_old.
          WRITE ltp_atflv TO ltp_char10 EXPONENT 0 DECIMALS 0.
          CONDENSE ltp_char10.
          WRITE ltp_char10 TO wa_final-value_old.
        WHEN 'NUM'.
          ltp_atflv = wa_cdpos_cl-value_new.
          WRITE ltp_atflv TO ltp_char10 EXPONENT 0 DECIMALS ltp_dec.
          CONDENSE ltp_char10.
          WRITE ltp_char10 TO wa_final-value_new.
          CLEAR ltp_char10.
          ltp_atflv = wa_cdpos_cl-value_old.
          WRITE ltp_atflv TO ltp_char10 EXPONENT 0 DECIMALS ltp_dec.
          CONDENSE ltp_char10.
          WRITE ltp_char10 TO wa_final-value_old.
        WHEN 'CHAR'.
          wa_final-value_new = wa_cdpos_cl-value_new.
          wa_final-value_old = wa_cdpos_cl-value_old.
      ENDCASE.
    ENDIF.
    CLEAR wa_cdhdr.
    READ TABLE ta_cdhdr INTO wa_cdhdr WITH KEY objectclas = wa_cdpos_cl-objectclas
                                               objectid = wa_cdpos_cl-objectid
                                               changenr = wa_cdpos_cl-changenr.
    IF sy-subrc = 0.
      wa_final-username = wa_cdhdr-username.
      wa_final-tcode = wa_cdhdr-tcode.
      wa_final-udate = wa_cdhdr-udate.
      wa_final-utime = wa_cdhdr-utime.
    ENDIF.
    CLEAR wa_inob.
    READ TABLE ta_inob INTO wa_inob WITH KEY cuobj = wa_cdpos_cl-tabkey+0(18).
    IF sy-subrc = 0.
      CLEAR wa_v_equi.
      READ TABLE ta_v_equi INTO wa_v_equi WITH KEY equnr = wa_inob-objek.
      IF sy-subrc = 0.
        IF wa_v_equi-eqart IN so_eqart.
          CLEAR wa_crhd.
          READ TABLE ta_crhd INTO wa_crhd WITH KEY objid = wa_v_equi-gewrk.
          IF sy-subrc = 0.
            wa_final-arbpl = wa_crhd-arbpl.
          ENDIF.
          wa_final-equnr = wa_v_equi-equnr.
          wa_final-eqktx = wa_v_equi-eqktx.
          wa_final-eqart = wa_v_equi-eqart.
          wa_final-tplnr = wa_v_equi-tplnr.
          APPEND wa_final TO ta_final.
          CLEAR wa_final.
        ELSE.
          CONTINUE.
        ENDIF.
      ELSE.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR wa_cdpos_cl.
  ENDLOOP.
  SORT ta_final BY username udate utime.

ENDFORM.                    " GET_CLASS_OUTPUT
