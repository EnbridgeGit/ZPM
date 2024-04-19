*&---------------------------------------------------------------------*
*&  Include           ZXWOCU07
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(CAUFVD_IMP) LIKE  CAUFVD STRUCTURE  CAUFVD
*"     VALUE(TRTYP) LIKE  TC10-TRTYP
*"  EXPORTING
*"     REFERENCE(RELEASE_ORDER) TYPE  XFLAG
*"----------------------------------------------------------------------

************************************************************************
*                              CHANGE LOG                              *
************************************************************************
*  Changed by:    Bassam AlSabri                                       *
*  Date:          Feb 23,2012                                          *
*  Track #:       TR930                                                *
*                                                                      *
*  Description:   Added a warning message if the user didn't specify   *
*                 the Assigned To person in the enchancment tab.       *
*                 The check only applies for the specified             *
*                 transaction and only for plant and planner groups    *
*                 which are defined in the ZPM_WF_MS_CNTRL             *
*BTBOUNDY - 2012/11/12 - Switch from IW31/IW32 checks to use TRTYP
*PANUSURI- 2013/06/05- SDP47426 -Validate the Service (Material Group) *
*or material (Material Number or Material Group) entered on the new    *
*operation.                                                            *
************************************************************************
types : begin of ty_ebanoldpr,
        matnr type matnr,
        matkl type matkl,
        zzorigreq type banfn,
  END OF ty_ebanoldpr.

DATA: ls_afvgd LIKE afvgd,
      lt_afvgd LIKE TABLE OF ls_afvgd,
      lt_afvgd_temp LIKE TABLE OF ls_afvgd,
      ls_resbd LIKE resbd,
      lt_resbd LIKE TABLE OF ls_resbd.

DATA: ls_messages TYPE bapiret2,
      lt_messages TYPE TABLE OF bapiret2,
      lv_msgv     TYPE symsgv.

DATA: lv_requestor TYPE bname.

*BOI by PANUSURI Ticket 47426
data: lv_pr_reference   type BANFN,
      lv_matnr          type MATNR,
      ls_ebanoldpr      TYPE ty_ebanoldpr,
      lt_ebanoldpr      TYPE TABLE OF ty_ebanoldpr,
      lt_ebanoldpr_temp TYPE TABLE OF ty_ebanoldpr,
      lv_msgv1          TYPE symsgv,
      lv_msgv2          TYPE symsgv,
      lv_msgv3          TYPE symsgv,
      lv_msgv4          TYPE symsgv,
      lv_found(1)       TYPE c.

constants: co_x         TYPE char1    VALUE 'X'.
*EOI by PANUSURI Ticket 47426

*-- Declaration for workflow relevant flag for the zz_assigned_to field
DATA: lv_wf_relevant  TYPE flag.

CALL FUNCTION 'PM_ORDER_DATA_READ'
  EXPORTING
    order_number    = caufvd_imp-aufnr
  TABLES
    iafvgd          = lt_afvgd
    iresbd          = lt_resbd
  EXCEPTIONS
    order_not_found = 1
    OTHERS          = 2.

IF sy-tcode = 'IW31' OR sy-tcode = 'IW32' OR sy-tcode = 'IW21' OR sy-tcode = 'IW22' ..
  IF sy-subrc = 0.
    LOOP AT lt_afvgd INTO ls_afvgd.
      CONCATENATE ls_afvgd-vornr ':' INTO lv_msgv.

      "Skip deleted items
      IF ls_afvgd-loekz IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      IF ls_afvgd-steus = 'PM02' OR ls_afvgd-steus = 'PM03'.
        IF ls_afvgd-afnam IS INITIAL.
          "Requisitioner is blank
          ls_messages-type    = 'E'.
          ls_messages-id      = 'ME'.
          ls_messages-number  = '303'.
          ls_messages-message_v1 = text-004.
          ls_messages-message_v2 = lv_msgv.
          ls_messages-message_v3 = text-006.
          APPEND ls_messages TO lt_messages.
        ELSE.
          TRANSLATE ls_afvgd-afnam TO UPPER CASE.

          SELECT SINGLE bname
              INTO lv_requestor
              FROM usr01
              WHERE bname = ls_afvgd-afnam.

          IF sy-subrc <> 0.
            "User does not exist
            ls_messages-type    = 'E'.
            ls_messages-id      = 'ME'.
            ls_messages-number  = '303'.
            ls_messages-message_v1 = text-004.
            ls_messages-message_v2 = lv_msgv.
            ls_messages-message_v3 = text-007.
            APPEND ls_messages TO lt_messages.
          ENDIF.
        ENDIF.

        IF ls_afvgd-usr01 IS INITIAL.
          "SRC is blank.
          ls_messages-type    = 'E'.
          ls_messages-id      = 'ME'.
          ls_messages-number  = '303'.
          ls_messages-message_v1 = text-004.
          ls_messages-message_v2 = lv_msgv.
          ls_messages-message_v3 = text-008.
          APPEND ls_messages TO lt_messages.
        ENDIF.

      ENDIF.

*BOI by PANUSURI Ticket 47426
      if ls_afvgd-usr03 is not initial.
        lv_pr_reference = ls_afvgd-usr03.
        clear lv_msgv1.
        lv_msgv1 = ls_afvgd-vornr.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = lv_pr_reference
          IMPORTING
            OUTPUT = lv_pr_reference.

        "Do Service Check on previous PR
        if ls_afvgd-matkl is not initial.
          "Fetch details of old purchase requisition
          SELECT matnr
                 matkl
                 zzorigreq
            FROM eban
            INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr
            WHERE banfn = lv_pr_reference
              AND loekz <> co_x.

          lv_found = ''.

          "Read old PR table.
          read table lt_ebanoldpr into ls_ebanoldpr with key matkl = ls_afvgd-matkl.
          if sy-subrc = 0.
            lv_found = 'X'.
          else.
            do.
              loop at lt_ebanoldpr into ls_ebanoldpr.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = ls_ebanoldpr-zzorigreq
                  IMPORTING
                    OUTPUT = ls_ebanoldpr-zzorigreq.
                modify lt_ebanoldpr from ls_ebanoldpr.
              endloop.
              SELECT matnr
                   matkl
                   zzorigreq
              FROM eban
              INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr_temp
              for all entries in lt_ebanoldpr
              WHERE banfn = lt_ebanoldpr-zzorigreq
              AND loekz <> co_x.
              if sy-subrc <> 0.
                exit.
              endif.
              clear lt_ebanoldpr.
              refresh lt_ebanoldpr.
              append lines of lt_ebanoldpr_temp to lt_ebanoldpr.
              clear lt_ebanoldpr_temp.
              refresh lt_ebanoldpr_temp.
              read table lt_ebanoldpr into ls_ebanoldpr with key matkl = ls_afvgd-matkl.
              if sy-subrc = 0.
                lv_found = 'X'.
                exit.
              endif.
            enddo.
          endif.

          "Check if no previous material group was found
          IF lv_found = ''.
            clear lv_msgv2.
            concatenate lv_msgv1 ',' into lv_msgv1.
            concatenate ls_afvgd-matkl text-013 into lv_msgv2 separated by space.
            clear ls_messages.
            ls_messages-type    = 'E'.
            ls_messages-id      = 'ME'.
            ls_messages-number  = '303'.
            ls_messages-message_v1 = text-010.
            ls_messages-message_v2 = lv_msgv1.
            ls_messages-message_v3 = text-012.
            ls_messages-message_v4 = lv_msgv2.
            APPEND ls_messages TO lt_messages.
          ENDIF. "lv_found = ''

        else. "ls_afvgd-matkl is initial
          append ls_afvgd to lt_afvgd_temp.
          clear ls_afvgd.
        endif. "ls_afvgd-matkl is not initial

        clear: ls_ebanoldpr,
               lv_pr_reference.
        refresh lt_ebanoldpr.

      endif. "ls_afvgd-usr03 is not initial

*EOI by PANUSURI Ticket 47426

    ENDLOOP.


    LOOP AT lt_resbd INTO ls_resbd.
      CONCATENATE ls_resbd-posnr ':' INTO lv_msgv.

      "Skip deleted items and stock items IE NOT (non stock)
      IF ls_resbd-xloek IS NOT INITIAL OR ls_resbd-postp <> 'N'.
        CONTINUE.
      ENDIF.

      IF ls_resbd-afnam IS INITIAL.
        ls_messages-type    = 'E'.
        ls_messages-id      = 'ME'.
        ls_messages-number  = '303'.
        ls_messages-message_v1 = text-005.
        ls_messages-message_v2 = lv_msgv.
        ls_messages-message_v3 = text-006.
        APPEND ls_messages TO lt_messages.
      ELSE.
        TRANSLATE ls_resbd-afnam TO UPPER CASE.

        SELECT SINGLE bname
            INTO lv_requestor
            FROM usr01
            WHERE bname = ls_resbd-afnam .

        IF sy-subrc <> 0.
          ls_messages-type    = 'E'.
          ls_messages-id      = 'ME'.
          ls_messages-number  = '303'.
          ls_messages-message_v1 = text-005.
          ls_messages-message_v2 = lv_msgv.
          ls_messages-message_v3 = text-007.
          APPEND ls_messages TO lt_messages.
        ENDIF.
      ENDIF.

*BOI by PANUSURI Ticket 47426
      clear ls_afvgd.
      read table lt_afvgd_temp into ls_afvgd with key vornr = ls_resbd-vornr.
      if sy-subrc = 0.
        clear lv_msgv1.
        lv_msgv1 = ls_resbd-vornr.
        lv_pr_reference = ls_afvgd-usr03.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            INPUT  = lv_pr_reference
          IMPORTING
            OUTPUT = lv_pr_reference.
        "Do Material Check on previous PR
        if ls_resbd-matnr is not initial.
          "Fetch details of old purchase requisition
          SELECT matnr
                 matkl
                 zzorigreq
            FROM eban
            INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr
            WHERE banfn = lv_pr_reference
              AND loekz <> co_x.

          lv_found = ''.

          "Read old PR table.
          read table lt_ebanoldpr into ls_ebanoldpr with key matnr = ls_resbd-matnr.
          if sy-subrc = 0.
            lv_found = 'X'.
          else.
            do.
              loop at lt_ebanoldpr into ls_ebanoldpr.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    INPUT  = ls_ebanoldpr-zzorigreq
                  IMPORTING
                    OUTPUT = ls_ebanoldpr-zzorigreq.
                modify lt_ebanoldpr from ls_ebanoldpr.
              endloop.
              SELECT matnr
                   matkl
                   zzorigreq
              FROM eban
              INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr_temp
              for all entries in lt_ebanoldpr
              WHERE banfn = lt_ebanoldpr-zzorigreq
              AND loekz <> co_x.
              if sy-subrc <> 0.
                exit.
              endif.
              clear lt_ebanoldpr.
              refresh lt_ebanoldpr.
              append lines of lt_ebanoldpr_temp to lt_ebanoldpr.
              clear lt_ebanoldpr_temp.
              refresh lt_ebanoldpr_temp.
              read table lt_ebanoldpr into ls_ebanoldpr with key matnr = ls_resbd-matnr.
              if sy-subrc = 0.
                lv_found = 'X'.
                exit.
              endif.
            enddo.
          endif.

          "Check if no previous material was found
          IF lv_found = ''.
            lv_matnr = ls_resbd-matnr.
            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                INPUT  = lv_matnr
              IMPORTING
                OUTPUT = lv_matnr.

            clear lv_msgv3.
            concatenate lv_msgv1 ',' into lv_msgv1.
            concatenate lv_matnr text-013 into lv_msgv3 separated by space.
            clear ls_messages.
            ls_messages-type    = 'E'.
            ls_messages-id      = 'ME'.
            ls_messages-number  = '303'.
            ls_messages-message_v1 = text-010.
            ls_messages-message_v2 = lv_msgv1.
            ls_messages-message_v3 = text-014.
            ls_messages-message_v4 = lv_msgv3.
            APPEND ls_messages TO lt_messages.
          ENDIF.

        else. "ls_resbd-matnr is initial
          "Do Material group Check on previous PR
          if ls_resbd-matkl is not initial.
            "Fetch details of old purchase requisition
            SELECT matnr
                   matkl
                   zzorigreq
              FROM eban
              INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr
              WHERE banfn = lv_pr_reference
              AND loekz <> co_x.

            lv_found = ''.

            "Read old PR table.
            read table lt_ebanoldpr into ls_ebanoldpr with key matkl = ls_resbd-matkl.
            if sy-subrc = 0.
              lv_found = 'X'.
            else.
              do.
                loop at lt_ebanoldpr into ls_ebanoldpr.
                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      INPUT  = ls_ebanoldpr-zzorigreq
                    IMPORTING
                      OUTPUT = ls_ebanoldpr-zzorigreq.
                  modify lt_ebanoldpr from ls_ebanoldpr.
                endloop.
                SELECT matnr
                     matkl
                     zzorigreq
                FROM eban
                INTO CORRESPONDING FIELDS OF TABLE lt_ebanoldpr_temp
                for all entries in lt_ebanoldpr
                WHERE banfn = lt_ebanoldpr-zzorigreq
                AND loekz <> co_x.
                if sy-subrc <> 0.
                  exit.
                endif.
                clear lt_ebanoldpr.
                refresh lt_ebanoldpr.
                append lines of lt_ebanoldpr_temp to lt_ebanoldpr.
                clear lt_ebanoldpr_temp.
                refresh lt_ebanoldpr_temp.
                read table lt_ebanoldpr into ls_ebanoldpr with key matkl = ls_resbd-matkl.
                if sy-subrc = 0.
                  lv_found = 'X'.
                  exit.
                endif.
              enddo.
            endif.

            "Check if no previous material group was found
            IF lv_found = ''.
              clear lv_msgv2.
              concatenate lv_msgv1 ',' into lv_msgv1.
              concatenate ls_resbd-matkl text-013 into lv_msgv2 separated by space.
              clear ls_messages.
              ls_messages-type    = 'E'.
              ls_messages-id      = 'ME'.
              ls_messages-number  = '303'.
              ls_messages-message_v1 = text-010.
              ls_messages-message_v2 = lv_msgv1.
              ls_messages-message_v3 = text-012.
              ls_messages-message_v4 = lv_msgv2.
              APPEND ls_messages TO lt_messages.
            ENDIF.

          else. "ls_resbd-matkl is initial
            clear ls_messages.
            clear lv_msgv4.
            concatenate text-010 lv_msgv1 into lv_msgv4 separated by space.
            concatenate lv_msgv4 ',' into lv_msgv4.
            ls_messages-type    = 'E'.
            ls_messages-id      = 'ME'.
            ls_messages-number  = '303'.
            ls_messages-message_v1 = lv_msgv4.
            ls_messages-message_v2 = text-011.
            ls_messages-message_v3 = text-015.
            APPEND ls_messages TO lt_messages.

          endif. "ls_resbd-matkl is not initial

        endif. "ls_resbd-matnr is not initial

      endif. "ls_resbd-vornr = ls_afvgd-vornr
      clear: ls_ebanoldpr.
      refresh: lt_ebanoldpr.

*EOI by PANUSURI Ticket 47426
    ENDLOOP.
    refresh lt_afvgd_temp. "(+)PANUSURI Ticket 47426
  ENDIF.
ENDIF.

IF lt_messages IS NOT INITIAL.
  CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
    TABLES
      i_bapiret2_tab = lt_messages.

  MESSAGE text-100 TYPE 'E'.
ENDIF.

** Order validation for assigned to field for PM Midstream workflows
*IF ( sy-tcode = 'IW31' OR sy-tcode = 'IW32' OR sy-tcode = 'IW21' OR sy-tcode = 'IW22' )
*  AND sy-batch IS INITIAL AND sy-binpt IS INITIAL.
*
*  CALL METHOD zcl_pm_ms_workflow_uc=>check_valid_plant_plangrp
*    EXPORTING
*      im_plant    = caufvd_imp-iwerk
*      im_plan_grp = caufvd_imp-ingpr
*    RECEIVING
*      re_valid    = lv_wf_relevant.
*
*
*  IF lv_wf_relevant = 'X' AND caufvd_imp-zz_assigned_to IS INITIAL.
*    MESSAGE w005(zpm).
*  ENDIF.
*
*ENDIF.
