*&---------------------------------------------------------------------*
*&  Include           Z_X_BI_MONITOR_CONTROLLER
*&---------------------------------------------------------------------*

TABLES: apqi.
DATA: gv_okcode TYPE sy-ucomm.

CLASS lcl_controller DEFINITION DEFERRED.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.

SELECT-OPTIONS: s_group FOR apqi-groupid,
                s_user  FOR apqi-userid,
                s_crtor FOR apqi-creator,
                s_cdate FOR apqi-credate,
                s_pstat FOR apqi-qstate NO-DISPLAY,
                s_ctime FOR apqi-cretime.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-001.
PARAMETERS: p_prc TYPE c AS CHECKBOX DEFAULT 'X',
            p_inp TYPE c AS CHECKBOX DEFAULT 'X',
            p_err TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_details DEFINITION.
  PUBLIC SECTION.

    TYPES:   BEGIN OF ty_mttab,
       off(02) TYPE n,
       len(02) TYPE n,
       text(99),
     END OF ty_mttab.

    TYPES:
            BEGIN OF ty_detail,
              indate  TYPE bdclm-indate,
              intime  TYPE bdclm-intime,
              message TYPE c LENGTH 273,
              tcnt    TYPE bdclm-tcnt,
              mcnt    TYPE bdclm-mcnt,
              tcode   TYPE bdclm-tcode,
              module  TYPE bdclm-module,
              dynr    TYPE bdclm-dynr,
              mart    TYPE bdclm-mart,
              mid     TYPE bdclm-mid,
              mnr     TYPE bdclm-mnr,
            END OF ty_detail.

    DATA: gt_detail TYPE STANDARD TABLE OF ty_detail,
          gr_logs TYPE REF TO lcl_log,
          gv_mtext(273) TYPE c,
          gv_mtvaroff TYPE int4,
          gv_do_condense TYPE c,
          gv_mparcnt TYPE bdclm-mparcnt,
          gv_sp_len  TYPE i,
          gv_charcnt TYPE i,
          gv_wcnt TYPE i,
          gr_view TYPE REF TO cl_salv_table,
          gr_functions TYPE REF TO cl_salv_functions,
          gr_columns       TYPE REF TO cl_salv_columns,
          gr_display       TYPE REF TO cl_salv_display_settings,
          gr_events        TYPE REF TO cl_salv_events_table,
          gv_is_displayed TYPE xflag,
          gv_disp_errors_only TYPE c, "Display Error messages only
          gt_mttab     TYPE STANDARD TABLE OF ty_mttab.

    METHODS: constructor IMPORTING im_logs  TYPE REF TO lcl_log
                                   im_disp_err_only TYPE char1,
             prepare IMPORTING im_logs  TYPE REF TO lcl_log,
             replace_var IMPORTING im_vark TYPE char2
                                   im_vari TYPE i
                                   im_varpos TYPE sy-fdpos,
             clear,
             display,
             refresh FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

ENDCLASS.                    "lcl_detailed_log DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_detailed_log IMPLEMENTATION
*----------------------------------------------------------------------*
*
**----------------------------------------------------------------------*
CLASS lcl_details IMPLEMENTATION.
  METHOD clear.

    REFRESH:
    gt_detail.

    CLEAR:
     gv_mtext,
     gv_mtvaroff,
     gv_do_condense,
     gv_mparcnt,
     gv_sp_len,
     gv_charcnt,
     gv_wcnt,
     gt_mttab.

  ENDMETHOD.                    "clear
  METHOD prepare.


    FIELD-SYMBOLS: <f_bdclm> LIKE LINE OF lcl_log=>t_bdclm.
    DATA: lt_t100 TYPE lcl_log=>ty_t_t100,
          ls_t100 LIKE LINE OF lt_t100,
          ls_detail TYPE ty_detail,
          lv_mpar TYPE bdclm-mpar,
          lv_parcnt  TYPE i,
          lv_shiftln TYPE i,
          lv_digits(10) TYPE c VALUE '0123456789',
          lv_fdpos TYPE sy-fdpos,
          lv_vartcnt TYPE int4,
          lv_mtext1(124) TYPE c,
          lv_length TYPE n LENGTH 2,
          ls_mttab TYPE ty_mttab,

  BEGIN OF ls_par,
   len(02)   TYPE n,
   text(254) TYPE c,
 END OF ls_par.

    gr_logs = im_logs.
    im_logs->get_texts( IMPORTING ex_t100 = lt_t100 ).

    LOOP AT im_logs->t_bdclm ASSIGNING <f_bdclm>.

      IF gv_disp_errors_only = 'X' AND <f_bdclm>-mart = 'S'.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <f_bdclm> TO ls_detail.
      READ TABLE lt_t100 INTO ls_t100 WITH KEY arbgb = <f_bdclm>-mid
                                               msgnr = <f_bdclm>-mnr.
      IF sy-subrc EQ 0.
        REFRESH: gt_mttab.
        CLEAR: lv_parcnt,
               gv_mparcnt,
               gv_charcnt,
               gv_wcnt,
               gv_sp_len,
               sy-fdpos.

        MOVE <f_bdclm>-mparcnt TO gv_mparcnt.

        IF NOT ls_t100-text CA '$&'.
          ls_detail-message = ls_t100-text.
          APPEND ls_detail TO gt_detail.
          CLEAR ls_detail.
        ELSE.
          lv_mtext1 = ls_t100-text.
          lv_mpar = <f_bdclm>-mpar.

          CLEAR lv_shiftln.
          DO gv_mparcnt TIMES.
            ls_par-len = lv_mpar+0(2).
            ls_par-text = lv_mpar+2(252).
            IF ls_par-len CN lv_digits OR
               ls_par-len EQ 0.
              ls_par-len = 1.
              ls_par-text = space.
              lv_shiftln = 2.
            ELSE.
              lv_shiftln = ls_par-len + 2.
            ENDIF.
            WRITE ls_par-text TO ls_mttab-text(ls_par-len).
            MOVE ls_par-len TO ls_mttab-len.
            MOVE <f_bdclm>-mparcnt TO ls_mttab-off.
            APPEND ls_mttab TO gt_mttab.
            CLEAR ls_mttab.

            SHIFT lv_mpar BY lv_shiftln PLACES.
          ENDDO.

          gv_mtext = lv_mtext1.

          IF <f_bdclm>-mid EQ  '00' AND    " sonderbehandlung s00368
             <f_bdclm>-mnr EQ '368' AND
             <f_bdclm>-mart EQ 'S'.
            CLEAR gv_mtext.
            CLEAR ls_mttab.
            READ TABLE gt_mttab INTO ls_mttab INDEX 1.
            WRITE ls_mttab-text TO gv_mtext+0(ls_mttab-len).
            CLEAR ls_mttab.
            READ TABLE gt_mttab INTO ls_mttab INDEX 2.
            WRITE ls_mttab-text TO gv_mtext+35(ls_mttab-len).
            ls_detail-message = gv_mtext.
            APPEND ls_detail TO gt_detail.
            CLEAR ls_detail.
          ELSE.

            gv_do_condense = abap_true.
            CLEAR: lv_vartcnt, gv_mtvaroff.

            WHILE lv_vartcnt LE 3.
              lv_vartcnt = lv_vartcnt + 1.
              IF lv_mtext1 CA '$&'.
                lv_parcnt = lv_parcnt + 1.
                IF sy-fdpos GT 0.
                  lv_fdpos = sy-fdpos - 1.
                ELSE.
                  lv_fdpos = sy-fdpos.
                ENDIF.
                SHIFT lv_mtext1 BY sy-fdpos PLACES.
                IF lv_mtext1(1) EQ '&'.
                  SHIFT lv_mtext1 BY 1 PLACES.
                  CASE lv_mtext1(1).
                    WHEN space.
                      replace_var( im_vark = '& '
                                   im_vari = lv_parcnt
                                   im_varpos = lv_fdpos ).
                    WHEN '$'.
                      replace_var( im_vark = '&&'
                                   im_vari = 0
                                   im_varpos = lv_fdpos ).
                    WHEN '1'.
                      replace_var( im_vark = '&1'
                                   im_vari = 1
                                   im_varpos = lv_fdpos ).
                    WHEN '2'.
                      replace_var( im_vark = '&2'
                                   im_vari = 2
                                   im_varpos = lv_fdpos ).
                    WHEN '3'.
                      replace_var( im_vark = '&3'
                                   im_vari = 3
                                   im_varpos = lv_fdpos ).
                    WHEN '4'.
                      replace_var( im_vark = '&4'
                                   im_vari = 4
                                   im_varpos = lv_fdpos ).
                    WHEN OTHERS.
                      replace_var( im_vark = '&<'
                                   im_vari = lv_parcnt
                                   im_varpos = lv_fdpos ).
                  ENDCASE.
                ENDIF.

                IF lv_mtext1(1) EQ '$'.
                  SHIFT lv_mtext1 BY 1 PLACES.
                  CASE lv_mtext1(1).
                    WHEN space.
                      replace_var( im_vark = '$ '
                                   im_vari = lv_parcnt
                                   im_varpos = lv_fdpos ).
                    WHEN '$'.
                      replace_var( im_vark = '$$'
                                   im_vari = 0
                                   im_varpos = lv_fdpos ).
                    WHEN '1'.
                      replace_var( im_vark = '$1'
                                   im_vari = 1
                                   im_varpos = lv_fdpos ).
                    WHEN '2'.
                      replace_var( im_vark = '$2'
                                   im_vari = 2
                                   im_varpos = lv_fdpos ).
                    WHEN '3'.
                      replace_var( im_vark = '$3'
                                   im_vari = 3
                                   im_varpos = lv_fdpos ).
                    WHEN '4'.
                      replace_var( im_vark = '$4'
                                   im_vari = 4
                                   im_varpos = lv_fdpos ).
                    WHEN OTHERS.
                      replace_var( im_vark = '$<'
                                   im_vari = lv_parcnt
                                   im_varpos = lv_fdpos ).
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDWHILE.

            IF gv_mtext CA '%%D%%'.
              REPLACE '%%D%%' WITH '$' INTO gv_mtext.
            ENDIF.
            IF gv_mtext CA '%%A%%'.
              REPLACE '%%A%%' WITH '&' INTO gv_mtext.
            ENDIF.
            IF gv_do_condense EQ space.
              ls_detail-message = gv_mtext.
            ELSE.
              CONDENSE gv_mtext.
              ls_detail-message = gv_mtext.
            ENDIF.
            APPEND ls_detail TO gt_detail.
            CLEAR ls_detail.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "prepare
  METHOD refresh.



  ENDMETHOD.                    "refresh
  METHOD display.

    IF gv_is_displayed IS INITIAL.
      gr_view->display( ).
      gv_is_displayed = abap_true.
    ELSE.
      gr_view->refresh( ).
    ENDIF.

  ENDMETHOD.                    "display
  METHOD replace_var.

    FIELD-SYMBOLS:
                  <f_mtxt> TYPE any,
                  <f_vtxt> TYPE any.

    DATA: lv_var  TYPE char2,
          lv_var1 TYPE char1,
          lv_moff TYPE i,
          ls_mttab TYPE ty_mttab.

    CLEAR: ls_mttab, lv_moff.

    lv_var = im_vark.
    SHIFT lv_var BY 1 PLACES.

    CASE lv_var.
      WHEN space.
        CLEAR ls_mttab.
        READ TABLE gt_mttab INTO ls_mttab INDEX im_vari.
        IF sy-subrc EQ 0.
          lv_moff = im_varpos + gv_mtvaroff.
          ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
          ASSIGN ls_mttab-text(ls_mttab-len) TO <f_vtxt>.
          lv_var1 = im_vark.
          REPLACE lv_var1 WITH <f_vtxt>     INTO <f_mtxt>.
          gv_mtvaroff = ls_mttab-len.
        ELSE.
          IF im_vari GT gv_mparcnt.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '  ' INTO <f_mtxt>.
            gv_mtvaroff = 2.
          ELSE.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '%%Z%%' INTO <f_mtxt>.
            gv_mtvaroff = 7.
          ENDIF.
        ENDIF.
      WHEN '$'.
        lv_moff = im_varpos + gv_mtvaroff.
        ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
        REPLACE im_vark WITH '%%D%%' INTO <f_mtxt>.
        gv_mtvaroff = 7.
      WHEN '&'.
        lv_moff = im_varpos + gv_mtvaroff.
        ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
        REPLACE im_vark WITH '%%A%%' INTO <f_mtxt>.
        gv_mtvaroff = 7.
      WHEN '<'.
        CLEAR ls_mttab.
        READ TABLE gt_mttab INTO ls_mttab INDEX im_vari.
        IF sy-subrc EQ 0.
          IF im_vark EQ '&<'.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            ASSIGN ls_mttab-text(ls_mttab-len) TO <f_vtxt>.
            REPLACE '&' WITH <f_vtxt>     INTO <f_mtxt>.
            gv_mtvaroff = ls_mttab-len.
          ENDIF.
          IF im_vark EQ '$<'.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            ASSIGN ls_mttab-text(ls_mttab-len) TO <f_vtxt>.
            REPLACE '$' WITH <f_vtxt>     INTO <f_mtxt>.
            gv_mtvaroff = ls_mttab-len.
          ENDIF.
        ELSE.
          IF im_vark EQ '&<'.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE '&' WITH ' ' INTO <f_mtxt>.
            gv_mtvaroff = 1.
          ENDIF.
          IF im_vark EQ '$<'.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE '$' WITH ' ' INTO <f_mtxt>.
            gv_mtvaroff = 1.
          ENDIF.
        ENDIF.
      WHEN '1'.
        CLEAR ls_mttab.
        READ TABLE gt_mttab INTO ls_mttab INDEX 1.
        IF sy-subrc EQ 0.
          lv_moff = im_varpos + gv_mtvaroff.
          ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
          ASSIGN ls_mttab-text(ls_mttab-len) TO <f_vtxt>.
          REPLACE im_vark WITH <f_vtxt>     INTO <f_mtxt>.
          gv_mtvaroff = ls_mttab-len.
        ELSE.
          IF im_vari GT gv_mparcnt.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '  ' INTO <f_mtxt>.
            gv_mtvaroff = 2.
          ELSE.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '%%Z%%' INTO <f_mtxt>.
            gv_mtvaroff = 7.
          ENDIF.
        ENDIF.
      WHEN '2'.
        CLEAR ls_mttab.
        READ TABLE gt_mttab INTO ls_mttab INDEX 2.
        IF sy-subrc EQ 0.
          lv_moff = im_varpos + gv_mtvaroff.
          ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
          ASSIGN ls_mttab-text(ls_mttab-len) TO <f_vtxt>.
          REPLACE im_vark WITH <f_vtxt>     INTO <f_mtxt>.
          gv_mtvaroff = ls_mttab-len.
        ELSE.
          IF im_vari GT gv_mparcnt.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '  ' INTO <f_mtxt>.
            gv_mtvaroff = 2.
          ELSE.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '%%Z%%' INTO <f_mtxt>.
            gv_mtvaroff = 7.
          ENDIF.
        ENDIF.
      WHEN '3'.
        CLEAR ls_mttab.
        READ TABLE gt_mttab INTO ls_mttab INDEX 3.
        IF sy-subrc EQ 0.
          lv_moff = im_varpos + gv_mtvaroff.
          ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
          ASSIGN ls_mttab-text(ls_mttab-len) TO <f_vtxt>.
          REPLACE im_vark WITH <f_vtxt>     INTO <f_mtxt>.
          gv_mtvaroff = ls_mttab-len.
        ELSE.
          IF im_vari GT gv_mparcnt.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '  ' INTO <f_mtxt>.
            gv_mtvaroff = 2.
          ELSE.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '%%Z%%' INTO <f_mtxt>.
            gv_mtvaroff = 7.
          ENDIF.
        ENDIF.
      WHEN '4'.
        CLEAR ls_mttab.
        READ TABLE gt_mttab INTO ls_mttab INDEX 4.
        IF sy-subrc EQ 0.
          lv_moff = im_varpos + gv_mtvaroff.
          ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
          ASSIGN ls_mttab-text(ls_mttab-len) TO <f_vtxt>.
          REPLACE im_vark WITH <f_vtxt>     INTO <f_mtxt>.
          gv_mtvaroff = ls_mttab-len.
        ELSE.
          IF im_vari GT gv_mparcnt.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '  ' INTO <f_mtxt>.
            gv_mtvaroff = 2.
          ELSE.
            lv_moff = im_varpos + gv_mtvaroff.
            ASSIGN gv_mtext+lv_moff(*) TO <f_mtxt>.
            REPLACE im_vark WITH '%%Z%%' INTO <f_mtxt>.
            gv_mtvaroff = 7.
          ENDIF.
        ENDIF.
    ENDCASE.
    gv_do_condense = space.

  ENDMETHOD.                    "replace_var
  METHOD constructor.

*-- Set display only indicator
    gv_disp_errors_only = im_disp_err_only.

    clear( ).
    prepare( im_logs = im_logs ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = gr_view
          CHANGING
            t_table      = gt_detail ).
      CATCH cx_salv_msg.
    ENDTRY.
    gr_functions = gr_view->get_functions( ).
    gr_functions->set_all( abap_true ).

    gr_view->set_screen_popup(
      start_column = 30
      end_column  = 200
    start_line  = 10
    end_line    = 20 ).

    gr_view->set_screen_status(
      pfstatus      =  'ZSALV_POPUP'
      report        =  sy-repid
      set_functions = gr_view->c_functions_all ).

    gr_events = gr_view->get_event( ).
    gr_columns = gr_view->get_columns( ).
    gr_columns->set_optimize( ).
    gr_display = gr_view->get_display_settings( ).
    gr_display->set_striped_pattern( abap_true ).
    SET HANDLER refresh FOR gr_events.

  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_detailed_log IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_controller DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller DEFINITION.

  PUBLIC SECTION.

    TYPES:
            BEGIN OF ty_log,
              credate TYPE bdclm-indate,
              cretime TYPE bdclm-intime,
              tcnt    TYPE bdclm-tcnt,
              mcnt    TYPE bdclm-mcnt,
              tcode   TYPE bdclm-tcode,
              module  TYPE bdclm-module,
              dynr    TYPE bdclm-dynr,
              mart    TYPE bdclm-mart,
              mid     TYPE bdclm-mid,
              mnr     TYPE bdclm-mnr,
              message TYPE longtext,
            END OF ty_log,

            ty_t_log TYPE STANDARD TABLE OF ty_log WITH DEFAULT KEY.

    TYPES:
          BEGIN OF ty_out,
            groupid   TYPE apqi-groupid,
            userid    TYPE apqi-userid,
            creator   TYPE apqi-creator,
            credate   TYPE apqi-credate,
            cretime   TYPE apqi-cretime,
            qstate    TYPE char15,
            sdate     TYPE sy-datum,
            stime     TYPE sy-uzeit,
            edate     TYPE sy-datum,
            etime     TYPE sy-uzeit,
            transcnt  TYPE apqi-transcnt,
            transcntf TYPE apqi-transcntf,
            transcnte TYPE apqi-transcnte,
            transcntl TYPE apqi-transcnte,
            telapse   TYPE swl_pm_cvh-duration,
            tremain   TYPE swl_pm_cvh-duration,
            percent   TYPE i,
            qid       TYPE apqi-qid,
            temseid   TYPE apql-temseid,
          END OF ty_out.

    TYPES:
    BEGIN OF ty_logs,
       qid      TYPE apqi-qid,
       temseid  TYPE apql-temseid,
       r_logs   TYPE REF TO lcl_log,
       details  TYPE REF TO lcl_details,
    END OF ty_logs.

    CLASS-METHODS: main,
                   process,
                   first_display,
                   refresh,
                   display_log FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.

  PRIVATE SECTION.
    CLASS-DATA:
      r_model  TYPE REF TO lcl_model,
      r_view   TYPE REF TO lcl_view,
      t_logs   TYPE TABLE OF ty_logs,
      gt_out   TYPE STANDARD TABLE OF ty_out,
      gt_logs  TYPE STANDARD TABLE OF ty_logs.

ENDCLASS.                    "lcl_main DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_controller IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_controller IMPLEMENTATION.
  METHOD display_log.

    FIELD-SYMBOLS: <f_out>  TYPE ty_out,
                   <f_logs> TYPE ty_logs.

    DATA: lt_rows TYPE salv_t_row,
          lv_row  TYPE int4,
          lv_errors_only TYPE c VALUE space.

    CASE e_salv_function.
      WHEN 'DISPLOG'.
        lt_rows = r_view->gr_selections->get_selected_rows( ).
        READ TABLE lt_rows INTO lv_row INDEX 1.
      WHEN 'DISPERRORLOG'.
        lt_rows = r_view->gr_selections->get_selected_rows( ).
        READ TABLE lt_rows INTO lv_row INDEX 1.
        lv_errors_only = 'X'.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    READ TABLE gt_out ASSIGNING <f_out> INDEX lv_row.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_logs ASSIGNING <f_logs> WITH KEY qid     = <f_out>-qid
                                                   temseid = <f_out>-temseid.
    IF sy-subrc EQ 0.
*-- Free up memory from the previous object if already initialised.
      IF <f_logs>-details IS NOT INITIAL.
        FREE <f_logs>-details.
      ENDIF.
      CREATE OBJECT <f_logs>-details
        EXPORTING
          im_logs          = <f_logs>-r_logs
          im_disp_err_only = lv_errors_only.

      <f_logs>-details->display(  ).
    ENDIF.

  ENDMETHOD.                    "display_log
  METHOD process.

    DATA:
    ls_out     TYPE ty_out,
    ls_logs    TYPE ty_logs,
    lv_tns     TYPE int4,
    lv_logcnt  TYPE int4,
    ls_bdclm   LIKE LINE OF lcl_log=>t_bdclm,
    lv_bdclml  TYPE string,
    lv_bdclmr  TYPE string,
    lt_bdclm   TYPE lcl_log=>ty_t_bdclm,
    lv_tremain TYPE p DECIMALS 3 LENGTH 8,
    lv_dration TYPE sy-tabix,
    lv_telapse TYPE int4,
    ls_bdcth   TYPE bdcth.

    FIELD-SYMBOLS: <f_apqi>  LIKE LINE OF r_model->t_apqi,
                   <f_apql>  LIKE LINE OF r_model->t_apql,
                   <f_bdclm> LIKE LINE OF lcl_log=>t_bdclm,
                   <f_apqd>  LIKE LINE OF r_model->t_apqd.

    REFRESH gt_out.

    CREATE OBJECT r_model
      EXPORTING
        im_r_groupid = s_group[]
        im_r_userid  = s_user[]
        im_r_qstate  = s_pstat[]
        im_r_creator = s_crtor[]
        im_r_cretime = s_ctime[]
        im_r_credate = s_cdate[].

*    LOOP AT r_model->t_apqi ASSIGNING <f_apqi>.
*      CLEAR: lv_tns.

*      lv_tns = <f_apqi>-transcnt.

    LOOP AT r_model->t_apql ASSIGNING <f_apql>. " WHERE qid = <f_apqi>-qid.
      CLEAR: ls_out,
             ls_logs,
             lv_logcnt,
             lv_bdclml,
             lv_bdclmr.

      REFRESH: lt_bdclm.

      READ TABLE r_model->t_apqi ASSIGNING <f_apqi>
        WITH KEY qid = <f_apql>-qid.
      IF sy-subrc = 0.
        lv_tns = <f_apqi>-transcnt.
        ls_out-qstate  = <f_apqi>-qstate.

      ENDIF.


      ls_logs-temseid = <f_apql>-temseid.

      CREATE OBJECT ls_logs-r_logs
        EXPORTING
          im_temseid = <f_apql>-temseid.

      ls_out-temseid = <f_apql>-temseid.
      ls_out-groupid = <f_apql>-groupid.
      ls_out-qid     = <f_apql>-qid.
      ls_logs-qid    = <f_apql>-qid.
      ls_out-creator = <f_apql>-creator.
      ls_out-credate = <f_apql>-credate.
      ls_out-cretime = <f_apql>-cretime.
*-- The queue status seems to be wrong some times, Either useit from APQI or from Log
*      ls_out-qstate  = <f_apql>-status.


*      ls_out-userid  = <f_apqi>-userid.


      DESCRIBE TABLE ls_logs-r_logs->t_bdclm LINES lv_logcnt.
      READ TABLE ls_logs-r_logs->t_bdclm ASSIGNING <f_bdclm> INDEX lv_logcnt.
      IF sy-subrc EQ 0.
        ls_out-edate = <f_bdclm>-indate.
        ls_out-etime = <f_bdclm>-intime.
      ENDIF.

      READ TABLE ls_logs-r_logs->t_bdclm ASSIGNING <f_bdclm> INDEX 1.
      IF sy-subrc EQ 0.
        ls_out-sdate = <f_bdclm>-indate.
        ls_out-stime = <f_bdclm>-intime.
      ENDIF.

*-- Read total count from log
      CLEAR ls_bdclm.
      READ TABLE ls_logs-r_logs->t_bdclm INTO ls_bdclm
                                     WITH KEY mid = '00'
                                              mnr = '363'.
      IF sy-subrc EQ 0.
        SPLIT ls_bdclm-mpar AT space INTO lv_bdclml lv_bdclmr.
        CONDENSE lv_bdclmr.
        IF lv_tns IS INITIAL.
          lv_tns = lv_bdclmr.
        ENDIF.



        CLEAR: ls_bdclm,
               lv_bdclml,
               lv_bdclmr.
*-- Successfully processed count from log
        READ TABLE ls_logs-r_logs->t_bdclm INTO ls_bdclm
                                       WITH KEY mid = '00'
                                                mnr = '364'.
        IF sy-subrc EQ 0.
          SPLIT ls_bdclm-mpar AT space INTO lv_bdclml lv_bdclmr.
          CONDENSE lv_bdclmr.

          ls_out-transcntf = lv_bdclmr.

          CLEAR: ls_bdclm,
                 lv_bdclml,
                 lv_bdclmr.
*-- Error count from log
          READ TABLE ls_logs-r_logs->t_bdclm INTO ls_bdclm
                                         WITH KEY mid = '00'
                                                  mnr = '365'.

          IF sy-subrc EQ 0.
            SPLIT ls_bdclm-mpar AT space INTO lv_bdclml lv_bdclmr.
            CONDENSE lv_bdclmr.
            ls_out-transcnte = lv_bdclmr.
          ENDIF.

          IF ls_out-transcnte IS NOT INITIAL.
            ls_out-qstate = 'E'.
          ELSE.
            ls_out-qstate = 'F'.
          ENDIF.
        ENDIF.

      ELSE.
        lt_bdclm = ls_logs-r_logs->t_bdclm.
        SORT lt_bdclm BY tcnt.
        DELETE ADJACENT DUPLICATES FROM lt_bdclm COMPARING tcnt.
        DELETE lt_bdclm WHERE tcnt EQ space.


        LOOP AT lt_bdclm ASSIGNING <f_bdclm>.
          READ TABLE r_model->t_apqd ASSIGNING <f_apqd> WITH KEY qid = <f_apql>-qid
                                                               trans = <f_bdclm>-tcnt
                                                               BINARY SEARCH.

          IF sy-subrc EQ 0.
            CLEAR ls_bdcth.
            MOVE <f_apqd>-vardata TO ls_bdcth.
            IF ls_bdcth-state EQ 'F'.
              ls_out-transcntf = ls_out-transcntf + 1.
            ELSEIF ls_bdcth-state EQ 'E'.
              ls_out-transcnte = ls_out-transcnte +  1.
            ENDIF.
          else.
*-- Successfully processed ones might have been deleted.
              ls_out-transcntf = ls_out-transcntf + 1.
          ENDIF.

        ENDLOOP.
      ENDIF.

      IF ls_out-sdate IS NOT INITIAL AND
         ls_out-edate IS NOT INITIAL AND
         ls_out-stime IS NOT INITIAL AND
         ls_out-etime IS NOT INITIAL.

        CLEAR: lv_telapse,
               lv_tremain,
               lv_dration.

        IF ls_out-qstate EQ 'R'.
          ls_out-edate = sy-datum.
          ls_out-etime = sy-uzeit.
        ENDIF.

        CALL FUNCTION 'SWI_DURATION_DETERMINE'
          EXPORTING
            start_date = ls_out-sdate
            end_date   = ls_out-edate
            start_time = ls_out-stime
            end_time   = ls_out-etime
          IMPORTING
            duration   = lv_telapse.

        IF ( ls_out-transcntf + ls_out-transcnte ) NE 0.
          lv_tremain = lv_telapse  * ( ( lv_tns / ( ls_out-transcntf + ls_out-transcnte ) ) - 1 ).
        ENDIF.

        CALL FUNCTION 'MONI_TIME_CONVERT'
          EXPORTING
            ld_duration        = lv_telapse
          IMPORTING
            lt_output_duration = ls_out-telapse.

        IF ls_out-qstate EQ 'R'.
          lv_dration = lv_tremain.
          CALL FUNCTION 'MONI_TIME_CONVERT'
            EXPORTING
              ld_duration        = lv_dration
            IMPORTING
              lt_output_duration = ls_out-tremain.
        ENDIF.
      ENDIF.

      ls_out-percent = ( ls_out-transcntf + ls_out-transcnte ) * 100 / lv_tns.
      ls_out-transcnt = lv_tns.
      ls_out-transcntl = ls_out-transcnt - ls_out-transcnte - ls_out-transcntf.
      lv_tns = lv_tns - ls_out-transcntf.

      CASE ls_out-qstate.
        WHEN space.
          ls_out-qstate = 'New'.
        WHEN 'E'.
          ls_out-qstate = 'Errors'.
        WHEN 'F'.
          ls_out-qstate = 'Processed'.
        WHEN 'R'.
          ls_out-qstate = 'Processing'.
          CLEAR: ls_out-edate,
                 ls_out-etime.
        WHEN 'S'.
          ls_out-qstate = 'In Background'.
        WHEN OTHERS.
      ENDCASE.

      APPEND ls_out TO gt_out.
      APPEND ls_logs TO gt_logs.
    ENDLOOP.

    SORT gt_out BY credate cretime groupid qid temseid DESCENDING.

  ENDMETHOD.                    "first_display
  METHOD main.

    process( ).
    IF gt_out IS INITIAL.
      MESSAGE 'No records exist' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
    CALL SCREEN  100.

  ENDMETHOD.                    "main
  METHOD first_display.

    IF r_view IS NOT INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT r_view.
    r_view->set_data( CHANGING ch_data = gt_out ).
    SET HANDLER display_log FOR r_view->gr_events.

    r_view->set_column_text( EXPORTING im_column = 'QSTATE'
                                       im_ltext = 'Processing Status'
                                       im_mtext = 'Processing Status'
                                       im_stext = 'Status' ).

    r_view->set_column_text( EXPORTING im_column = 'TEMSEID'
                                       im_ltext = 'Temse ID'
                                       im_mtext = 'Temse ID'
                                       im_stext = 'Temse ID' ).

    r_view->set_column_text( EXPORTING im_column = 'SDATE'
                                       im_ltext = 'Start date'
                                       im_mtext = 'Start Date'
                                       im_stext = 'Start Date' ).

    r_view->set_column_text( EXPORTING im_column = 'EDATE'
                                       im_ltext = 'End date'
                                       im_mtext = 'End Date'
                                       im_stext = 'End Date' ).

    r_view->set_column_text( EXPORTING im_column = 'STIME'
                                       im_ltext = 'Start Time'
                                       im_mtext = 'Start Time'
                                       im_stext = 'Start Time' ).

    r_view->set_column_text( EXPORTING im_column = 'ETIME'
                                       im_ltext = 'End Time'
                                       im_mtext = 'End Time'
                                       im_stext = 'End Time' ).

    r_view->set_column_text( EXPORTING im_column = 'USERID'
                                       im_ltext = 'User'
                                       im_mtext = 'User'
                                       im_stext = 'User' ).

    r_view->set_column_text( EXPORTING im_column = 'CREATOR'
                                       im_ltext = 'Creator'
                                       im_mtext = 'Creator'
                                       im_stext = 'Creator' ).

    r_view->set_column_text( EXPORTING im_column = 'CRETIME'
                                       im_ltext = 'Created Time'
                                       im_mtext = 'Created Time'
                                       im_stext = 'Time' ).

    r_view->set_column_text( EXPORTING im_column = 'CREDATE'
                                       im_ltext = 'Created Date'
                                       im_mtext = 'Created Date'
                                       im_stext = 'Date' ).

    r_view->set_column_text( EXPORTING im_column = 'TRANSCNT'
                                       im_ltext = 'Transactions Read'
                                       im_mtext = 'Transactions Read'
                                       im_stext = 'TransRead' ).

    r_view->set_column_text( EXPORTING im_column = 'TRANSCNTE'
                                       im_ltext = 'Transactions Errored'
                                       im_mtext = 'Trans. Error'
                                       im_stext = 'TansErr' ).

    r_view->set_column_text( EXPORTING im_column = 'TRANSCNTF'
                                       im_ltext = 'Transactions Processed'
                                       im_mtext = 'Trans. Proc'
                                       im_stext = 'TransProc' ).

    r_view->set_column_text( EXPORTING im_column = 'TRANSCNTL'
                                       im_ltext = 'Transactions Left'
                                       im_mtext = 'Trans. Left'
                                       im_stext = 'TransLeft' ).

    r_view->set_column_text( EXPORTING im_column = 'QID'
                                       im_ltext = 'QID'
                                       im_mtext = 'QID'
                                       im_stext = 'QID' ).

    r_view->set_column_text( EXPORTING im_column = 'GROUPID'
                                       im_ltext = 'Session Name'
                                       im_mtext = 'Session Name'
                                       im_stext = 'Session' ).

    r_view->set_column_text( EXPORTING im_column = 'TELAPSE'
                                       im_ltext = 'Time Elapsed'
                                       im_mtext = 'Time Elapsed'
                                       im_stext = 'Elapsed' ).

    r_view->set_column_text( EXPORTING im_column = 'TREMAIN'
                                       im_ltext = 'Time Remaining'
                                       im_mtext = 'Time Remaining'
                                       im_stext = 'Remaining' ).

    r_view->set_column_text( EXPORTING im_column = 'PERCENT'
                                       im_ltext = '% Completed'
                                       im_mtext = '% Completed'
                                       im_stext = '%' ).

    r_view->set_sort_field( 'CREDATE' ).
    r_view->set_sort_field( 'CRETIME' ).
    r_view->set_sort_field( 'GROUPID' ).
    r_view->set_sort_field( 'QID' ).
    r_view->set_sort_field( 'TEMSEID' ).
    r_view->send( ).

  ENDMETHOD.                    "display
  METHOD refresh.

    REFRESH: gt_logs.
    FREE r_model.
    process( ).
    r_view->set_data( CHANGING ch_data = gt_out ).
    r_view->send( ).

  ENDMETHOD.                    "refresh
ENDCLASS.                    "lcl_controller IMPLEMENTATION

INITIALIZATION.

  s_ctime-low = '000000'.
  s_ctime-high = '235959'.
  s_ctime-option = 'BT'.
  s_ctime-sign = 'I'.
  APPEND s_ctime.

  s_cdate-low = sy-datum.
  s_cdate-high = sy-datum.
  s_cdate-option = 'BT'.
  s_cdate-sign = 'I'.
  APPEND s_cdate.
*
START-OF-SELECTION.

  IF p_prc EQ abap_true.
    s_pstat-low = 'F'.
    s_pstat-sign = 'I'.
    s_pstat-option = 'EQ'.
    APPEND s_pstat.
  ENDIF.

  IF p_inp EQ abap_true.
    s_pstat-low = 'S'.
    s_pstat-sign = 'I'.
    s_pstat-option = 'EQ'.
    APPEND s_pstat.
  ENDIF.

  IF p_err EQ abap_true.
    s_pstat-low = 'E'.
    s_pstat-sign = 'I'.
    s_pstat-option = 'EQ'.
    APPEND s_pstat.
  ENDIF.

  lcl_controller=>main( ).

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZBIMONI'.
  SET TITLEBAR 'ZBIMONI'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      lcl_controller=>refresh( ).
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  DISPLAY  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE display OUTPUT.

  lcl_controller=>first_display( ).

  CLEAR gv_okcode.

ENDMODULE.                 " DISPLAY  OUTPUT
