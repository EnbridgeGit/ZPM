*&---------------------------------------------------------------------*
*&  Include           Z_PM_UPDATE_REC
*&---------------------------------------------------------------------*
*TYPES : BEGIN OF ty_equi,
*         equnr TYPE equi-equnr,
*         eqtyp TYPE equi-eqtyp,
*       END OF ty_equi.
*DATA: lv_classnum TYPE klah-class VALUE 'FLOCS',
*      lv_class TYPE klah-klart VALUE '003',
*      lv_objectkey TYPE ausp-objek,
*      lw_sttxt TYPE string,
*      lv_table TYPE tcla-obtab VALUE 'MARA',
*      lt_classstruct TYPE STANDARD TABLE OF sclass,
*      lt_val_char TYPE STANDARD TABLE OF clobjdat,
*      lw_val_char TYPE clobjdat,
*      ta_eqtyp   TYPE STANDARD TABLE OF ty_equi,
*      wa_eqtyp  TYPE ty_equi,
*      lw_objek TYPE objnum.
*
  CONSTANTS: co_0  TYPE i VALUE 0,
             co_1  TYPE string VALUE 'EM10',
             co_2  TYPE string VALUE 'EM20',
             co_3  TYPE string VALUE 'PM10',
             co_4  TYPE string VALUE 'PM20',
             co_5  TYPE string VALUE 'PM40',
             co_6  TYPE string VALUE 'PM60',
             c_cld TYPE string VALUE 'CLSD',
             c_tec TYPE string VALUE 'TECO'.

  FIELD-SYMBOLS : <fs_resb> TYPE resbb.
*
*If NOT caufv_bt-tplnr IS INITIAL.
**fetching eqp.typ and eqpment based on func location
*  SELECT equnr eqtyp FROM v_equi INTO TABLE ta_eqtyp
*                   WHERE tplnr = caufv_bt-tplnr.
* IF SY-SUBRC IS INITIAL.
*
* ENDIF.
*             lw_sttxt = caufv_bt-sttxt. " ORDER STATUS
*             lw_objek  = caufv_bt-tplnr. " FUNCTIONAL LOCATION
*
* " CHECKING ORDER TYPES
*IF ( caufv_bt-auart = co_1 OR
*     caufv_bt-auart = co_2 OR
*     caufv_bt-auart = co_3 OR
*     caufv_bt-auart = co_4 OR
*     caufv_bt-auart = co_5 OR
*     caufv_bt-auart = co_6 ).
*
*" Getting characteristic values
*    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
*         EXPORTING
*              CLASS              = LV_CLASSNUM
*              CLASSTEXT          = 'X'
*              CLASSTYPE          = LV_CLASS
*              LANGUAGE           = SY-LANGU
*              OBJECT             = lw_objek
*         TABLES
*              T_CLASS            = LT_CLASSSTRUCT
*              T_OBJECTDATA       = LT_VAL_CHAR
*         EXCEPTIONS
*              NO_CLASSIFICATION  = 0
*              NO_CLASSTYPES      = 0
*              INVALID_CLASS_TYPE = 0
*              OTHERS             = 0.
*IF sy-subrc = co_0.
* LOOP AT resb_bt ASSIGNING <fs_resb>.
**UNLOADING POINT
*   IF  LT_VAL_CHAR[] IS NOT INITIAL.
*      SORT lt_val_char BY atnam.
*       READ TABLE lt_val_char INTO lw_val_char WITH KEY atnam = c_del BINARY SEARCH.
*        IF sy-subrc EQ co_0.
*          IF <fs_resb>-ablad IS INITIAL.
*           <fs_resb>-ablad = lw_val_char-ausp1.
*          ENDIF.
*        ENDIF.
**PLANT
*        IF NOT ( lw_sttxt CO c_cld OR lw_sttxt CO c_tec ).
*           READ TABLE ta_eqtyp INTO wa_eqtyp WITH KEY eqtyp = c_b.
*           IF sy-subrc EQ co_0.
*              IF wa_eqtyp-equnr IS NOT INITIAL.
*              READ TABLE lt_val_char INTO lw_val_char WITH KEY atnam = c_rpt BINARY SEARCH.
*               IF sy-subrc EQ co_0.
*                 IF <fs_resb>-werks IS INITIAL.
*                  <fs_resb>-werks = lw_val_char-ausp1.       "PLANT
*                 ENDIF.
*               ENDIF.
*              ENDIF.
*          ENDIF.
*       ENDIF.
*    ENDIF.
*   ENDLOOP.
*  ENDIF.
* ENDIF.
*ENDIF.

  TYPES : BEGIN OF ty_objid,
            vornr TYPE afvgd-vornr,
            arbid TYPE afvgd-arbid,
          END OF ty_objid,

          BEGIN OF ty_crtx,
            objid TYPE crtx-objid,
            ktext TYPE crtx-ktext,
          END OF ty_crtx.

  DATA : lv_beber TYPE caufvdb-beber,
         lv_sttxt TYPE string,
         lw_afvg  TYPE AFVGB,
         lt_crtx TYPE TABLE OF ty_crtx,
         lw_crtx  TYPE ty_crtx,
         lv_ktext TYPE crtx-ktext,
         lv_len TYPE i.

   IF ( caufv_bt-auart = co_1 OR
        caufv_bt-auart = co_2 OR
        caufv_bt-auart = co_3 OR
        caufv_bt-auart = co_4 OR
        caufv_bt-auart = co_5 OR
        caufv_bt-auart = co_6 ).
    IF NOT resb_bt[] IS INITIAL.

      lv_beber = caufv_bt-beber+0(3).
      lv_sttxt = caufv_bt-sttxt.

      IF NOT ( lv_sttxt CO c_cld OR lv_sttxt CO c_tec ).
      SELECT objid ktext FROM crtx INTO TABLE lt_crtx FOR ALL ENTRIES IN
        afvg_bt[] WHERE objid = afvg_bt-arbid
                    AND objty = 'A'.
        IF lt_crtx[] IS NOT INITIAL.
          SORT lt_crtx BY objid ktext.
        ENDIF.

        LOOP AT resb_bt ASSIGNING <fs_resb>.
          IF <fs_resb>-wempf IS INITIAL.
            READ TABLE afvg_bt INTO lw_afvg WITH KEY vornr = <fs_resb>-vornr BINARY SEARCH.
            IF sy-subrc EQ cO_0.
              READ TABLE lt_crtx INTO lw_crtx WITH KEY objid = lw_afvg-arbid BINARY SEARCH.
              IF sy-subrc EQ co_0.
                 lv_ktext = lw_crtx-ktext+0(9).
                 lv_len = strlen( lv_beber ).
                 IF lv_len < 3.
                CONCATENATE lv_beber lv_ktext INTO <fs_resb>-wempf SEPARATED BY space.
                 ELSE.
                CONCATENATE lv_beber lv_ktext INTO <fs_resb>-wempf.
                 ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.

        ENDLOOP.
       ENDIF.

      ENDIF.
     ENDIF.
