*&---------------------------------------------------------------------*
*&  Include           Z_X_BI_MONITOR_MODEL
*&---------------------------------------------------------------------*

CLASS lcl_log DEFINITION.

  PUBLIC SECTION.

    TYPES:
    ty_t_bdclm TYPE STANDARD TABLE OF bdclm WITH DEFAULT KEY,
    ty_t_t100  TYPE STANDARD TABLE OF t100  WITH DEFAULT KEY.

    DATA:
    t_bdclm    TYPE ty_t_bdclm,
    gv_temseid TYPE apql-temseid,
    gt_t100    TYPE ty_t_t100.

    METHODS:
    constructor IMPORTING im_temseid TYPE apql-temseid,
    get_texts EXPORTING ex_t100 TYPE ty_t_t100.

ENDCLASS.                    "lcl_log DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_log IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_log IMPLEMENTATION.
  METHOD get_texts.

    DATA: lt_bdclm  TYPE ty_t_bdclm.

    IF me->t_bdclm IS NOT INITIAL.
      lt_bdclm = me->t_bdclm.
      SORT lt_bdclm BY mid mnr.
      DELETE ADJACENT DUPLICATES FROM lt_bdclm COMPARING mid mnr.

      SELECT * FROM t100
               INTO TABLE ex_t100
               FOR ALL ENTRIES IN lt_bdclm
               WHERE sprsl EQ sy-langu AND
                     arbgb EQ lt_bdclm-mid AND
                     msgnr EQ lt_bdclm-mnr.

      IF sy-subrc EQ 0.
        SORT ex_t100 BY arbgb msgnr.
        gt_t100 = ex_t100.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "get_texts
  METHOD constructor.

    DATA: lv_fbhandle TYPE rststype-fbhandle,
          lv_charcp   TYPE rststype-charco VALUE '0000'.

    gv_temseid = im_temseid.

    CALL FUNCTION 'RSTS_GET_ATTRIBUTES'
      EXPORTING
        authority     = ' '
        client        = sy-mandt
        name          = im_temseid
      IMPORTING
        charco        = lv_charcp
      EXCEPTIONS
        fb_error      = 1
        fb_rsts_other = 2
        no_object     = 3
        no_permission = 4
        OTHERS        = 5.

    CALL FUNCTION 'RSTS_OPEN_RLC'
      EXPORTING
        authority = 'BATCH'
        name      = im_temseid
        client    = sy-mandt
        prom      = 'I'
        rectyp    = 'VNL----'
        charco    = lv_charcp
      EXCEPTIONS
        OTHERS    = 10.

    CALL FUNCTION 'RSTS_READ'
      TABLES
        datatab = t_bdclm
      EXCEPTIONS
        OTHERS  = 0.

    CALL FUNCTION 'RSTS_CLOSE'
      EXCEPTIONS
        OTHERS = 40.

  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_log IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_model DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_model DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
    ty_r_groupid TYPE RANGE OF apqi-groupid,
    ty_r_userid  TYPE RANGE OF apqi-userid,
    ty_r_qstate  TYPE RANGE OF apqi-qstate,
    ty_r_creator TYPE RANGE OF apqi-creator,
    ty_r_credate TYPE RANGE OF apqi-credate,
    ty_r_cretime TYPE RANGE OF apqi-cretime,
    ty_t_apqi    TYPE STANDARD TABLE OF apqi,
    ty_t_apqd    TYPE STANDARD TABLE OF apqd,
    ty_t_apql    TYPE STANDARD TABLE OF apql.

    DATA:
    t_apqi TYPE ty_t_apqi READ-ONLY,
    t_apqd TYPE ty_t_apqd READ-ONLY,
    t_apql TYPE ty_t_apql READ-ONLY.

    METHODS:
    constructor IMPORTING im_r_groupid TYPE ty_r_groupid
                          im_r_userid  TYPE ty_r_userid
                          im_r_qstate  TYPE ty_r_qstate
                          im_r_creator TYPE ty_r_creator
                          im_r_cretime TYPE ty_r_cretime
                          im_r_credate TYPE ty_r_credate.

ENDCLASS.                    "lcl_model DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_model IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_model IMPLEMENTATION.
  METHOD constructor.


    SELECT * FROM apql
             INTO TABLE t_apql
             WHERE  credate IN im_r_credate AND
                    groupid IN im_r_groupid AND
                    creator IN im_r_creator.
*                    status  IN im_r_qstate.
    IF sy-subrc EQ 0.
      SORT t_apql BY qid credate cretime.
    ELSE.
      RETURN.
    ENDIF.

    SELECT * FROM apqi
             INTO TABLE t_apqi
             WHERE groupid IN im_r_groupid AND
                   userid  IN im_r_userid  AND
                   qstate  IN im_r_qstate  AND
                   creator IN im_r_creator AND
                   credate IN im_r_credate AND
                   cretime IN im_r_cretime.
    IF sy-subrc = 0.
      SORT t_apqi BY groupid qid.
    ENDIF.

    SELECT * FROM apqd
          INTO TABLE t_apqd
          FOR ALL ENTRIES IN t_apqi
          WHERE qid = t_apqi-qid AND
                block = '1'.

    IF sy-subrc EQ 0.
      SORT t_apqd BY qid trans.
    ENDIF.

  ENDMETHOD.                    "constructor
ENDCLASS.                    "lcl_model IMPLEMENTATION
