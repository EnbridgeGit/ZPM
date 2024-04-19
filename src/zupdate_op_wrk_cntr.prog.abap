
*&---------------------------------------------------------------------*
*&  Include           ZUPDATE_OP_WRK_CNTR
*&---------------------------------------------------------------------*
*This Enhancement is linked to Object EAM-PM-E-075-N_Update_operation_work_center
*-----------------------------------------------------------------------
*New changes done for Activity Type
*Jsharma  28.08.2014
*Suitability match is eliminated for work center 'STO-TL07'.
*PANUSURI 28.06.2017 ACR-3596
*-----------------------------------------------------------------------

DATA : lv_qualf TYPE qualf,
       lv_objid TYPE objid,
       lv_gewrk TYPE lgwid,
       lv_lgwid TYPE lgwid.
FIELD-SYMBOLS : <fs_afvg> TYPE afvgb,
               <fs_afvg_old> TYPE aafvc.
TYPES : BEGIN OF ty_crhd,
  objty TYPE objty,
  objid TYPE objid,
  qualf TYPE qualf,
  arbpl TYPE crhd-arbpl,
  END OF ty_crhd.

TYPES : BEGIN OF ty_crco,
objty TYPE  cr_objty,
  objid TYPE cr_objid,
lstar TYPE lstar,
END OF ty_crco.

DATA : ta_crco TYPE STANDARD TABLE OF ty_crco,
       wa_crco TYPE ty_crco.

DATA : ta_crhd TYPE STANDARD TABLE OF ty_crhd,
       wa_crhd TYPE ty_crhd.
DATA: ls_caufv_bt TYPE caufvdb.

REFRESH : ta_crhd[],
          ta_crco[].
*Fetch the Task list Work center and Suitability
SELECT objty objid qualf arbpl INTO TABLE ta_crhd FROM crhd
                          FOR ALL ENTRIES IN afvg_bt
                          WHERE objid = afvg_bt-arbid.
IF sy-subrc IS INITIAL AND ta_crhd[] IS NOT INITIAL.
  SORT ta_crhd BY objty objid qualf.
ENDIF.
*fetch QUALF,OBJID from Work center table based on Header Work Center
LOOP AT caufv_bt INTO ls_caufv_bt.
  SELECT SINGLE objid qualf FROM crhd INTO (lv_objid,lv_qualf)
              WHERE objty = 'A' AND
                    arbpl = ls_caufv_bt-vaplz. "caufv_bt-vaplz.
  IF lv_objid IS NOT INITIAL.
*      **code for activity type
    SELECT  objid objid lstar FROM crco INTO TABLE ta_crco
                WHERE objty = 'A' AND
                      objid = lv_objid.
    IF ta_crco[] IS NOT INITIAL.
      SORT ta_crco BY lstar.
      DELETE ta_crco WHERE lstar = ''.
    ENDIF.
*    *end
  ENDIF.
  IF lv_qualf IS NOT INITIAL.
*    check if task list added in the operation line (Task type is general),
*    if added then modify the work center of operation with header work center, if suitability
*     LV_qualf matched with operation lines QUALF
*    Deleted record should not come
    LOOP AT afvg_bt ASSIGNING <fs_afvg> WHERE
                    plnty = 'A' AND phflg <> 'X' AND " records deleted after release status
                    aufpl = ls_caufv_bt-aufpl.    "order
      READ TABLE afvg_bt_old ASSIGNING <fs_afvg_old> WITH KEY
                                aufpl = <fs_afvg>-aufpl
                                aplzl = <fs_afvg>-aplzl.
      IF sy-subrc IS INITIAL. " new line equal to old line
        IF <fs_afvg_old>-plnty <> 'A'."old has task list not equal to task list
          IF <fs_afvg>-plnty = 'A'."new has task list
            READ TABLE ta_crhd INTO wa_crhd WITH KEY objty = 'A'
                                                 objid = <fs_afvg>-arbid BINARY SEARCH.
*      check floc /equp qualif with task list qualif and update
            IF sy-subrc IS INITIAL.
              "execute enhancement if task list work centers
              "start with STO or SMC
              IF wa_crhd-arbpl(3) = 'STO' OR
                 wa_crhd-arbpl(3) = 'SMC'.
*                IF wa_crhd-qualf = lv_qualf. "(-)PANUSURI Ticket ACR-3596
                IF wa_crhd-qualf = lv_qualf OR wa_crhd-arbpl = 'STO-TL07'.  "(+)PANUSURI Ticket ACR-3596
                  <fs_afvg>-arbid = lv_objid.
*            *update activity type
                  READ TABLE ta_crco INTO wa_crco INDEX 1.
                  IF sy-subrc IS INITIAL.
                    <fs_afvg>-larnt = wa_crco-lstar. " changes done by Jsharma on 28.8.2014
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
            CLEAR : wa_crhd,
            wa_crco.
          ENDIF.
        ENDIF.
      ELSE.
*     new line added
        IF <fs_afvg>-plnty = 'A' AND <fs_afvg>-phflg <> 'X'." records deleted after release status
          READ TABLE ta_crhd INTO wa_crhd WITH KEY objty = 'A'
                                           objid = <fs_afvg>-arbid BINARY SEARCH.
*      check floc /equp qualif with task list qualif and update
          IF sy-subrc IS INITIAL.
            "execute enhancement if task list work centers
            "start with STO or SMC
            IF wa_crhd-arbpl(3) = 'STO' OR
                wa_crhd-arbpl(3) = 'SMC'.
*              IF wa_crhd-qualf = lv_qualf. "(-)PANUSURI Ticket ACR-3596
              IF wa_crhd-qualf = lv_qualf OR wa_crhd-arbpl = 'STO-TL07'.  "(+)PANUSURI Ticket ACR-3596
                <fs_afvg>-arbid = lv_objid.
*            *update activity type
                READ TABLE ta_crco INTO wa_crco INDEX 1.
                IF sy-subrc IS INITIAL.
                  <fs_afvg>-larnt = wa_crco-lstar. " changes done by Jsharma on 28.8.2014
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
          CLEAR : wa_crhd,
          wa_crco.
        ENDIF.
      ENDIF.
    ENDLOOP. " new line item
  ENDIF.
ENDLOOP.
