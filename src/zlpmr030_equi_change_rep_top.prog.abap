*&---------------------------------------------------------------------*
*&  Include           ZLPMR030_EQUI_CHANGE_REP_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLPMR030_EQUI_CHANGE_REP_TOP                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 04-Aug-2017                                    *
*& Object ID          : ACR-4556                                       *
*& Application Area   : PM                                             *
*& Description        : Equipment change report.                       *
*&---------------------------------------------------------------------*
TABLES: equi.

************************************************************************
* TYPES DECLARATION
************************************************************************
TYPES: BEGIN OF ty_cdhdr,
         objectclas TYPE cdobjectcl,
         objectid   TYPE cdobjectv,
         changenr   TYPE cdchangenr,
         username   TYPE cdusername,
         udate      TYPE cddatum,
         utime      TYPE cduzeit,
         tcode      TYPE cdtcode,
       END OF ty_cdhdr,
       BEGIN OF ty_cdpos,
         objectclas TYPE cdobjectcl,
         objectid   TYPE cdobjectv,
         changenr   TYPE cdchangenr,
         fname      TYPE fieldname,
         value_new  TYPE cdfldvaln,
         value_old  TYPE cdfldvalo,
       END OF ty_cdpos,
       BEGIN OF ty_cdpos_cl,
         objectclas TYPE cdobjectcl,
         objectid   TYPE cdobjectv,
         changenr   TYPE cdchangenr,
         tabkey     TYPE cdtabkey,
         fname      TYPE fieldname,
         value_new  TYPE cdfldvaln,
         value_old  TYPE cdfldvalo,
       END OF ty_cdpos_cl,
       BEGIN OF ty_dd04t,
         rollname   TYPE rollname,
         scrtext_m  TYPE scrtext_m,
       END OF ty_dd04t,
       BEGIN OF ty_inob,
         cuobj      TYPE char50,
         objek      TYPE cuobn,
       END OF ty_inob,
       BEGIN OF ty_cabn,
         atinn      TYPE atinn,
         atnam      TYPE atnam,
         atfor      TYPE atfor,
       END OF ty_cabn,
       BEGIN OF ty_v_equi,
         equnr      TYPE equnr,
         eqart      TYPE eqart,
         gewrk      TYPE lgwid,
         eqktx      TYPE ktx01,
         tplnr      TYPE tplnr,
       END OF ty_v_equi,
       BEGIN OF ty_crhd,
         objid     TYPE cr_objid,
         arbpl     TYPE arbpl,
       END OF ty_crhd,
       BEGIN OF ty_final,
         equnr      TYPE equnr,
         eqktx      TYPE ktx01,
         eqart      TYPE eqart,
         tplnr      TYPE tplnr,
         arbpl      TYPE arbpl,
         username   TYPE cdusername,
         tcode      TYPE cdtcode,
         udate      TYPE cddatum,
         utime      TYPE cduzeit,
         fname      TYPE fieldname,
         value_old  TYPE cdfldvalo,
         value_new  TYPE cdfldvaln,
       END OF ty_final.

************************************************************************
* DATA DECLARATION
************************************************************************
DATA: ta_cdhdr    TYPE STANDARD TABLE OF ty_cdhdr,
      wa_cdhdr    TYPE ty_cdhdr,
      ta_cdpos    TYPE STANDARD TABLE OF ty_cdpos,
      wa_cdpos    TYPE ty_cdpos,
      ta_cdpos_cl TYPE STANDARD TABLE OF ty_cdpos_cl,
      wa_cdpos_cl TYPE ty_cdpos_cl,
      ta_dd04t    TYPE STANDARD TABLE OF ty_dd04t,
      wa_dd04t    TYPE ty_dd04t,
      ta_inob     TYPE STANDARD TABLE OF ty_inob,
      wa_inob     TYPE ty_inob,
      ta_cabn     TYPE STANDARD TABLE OF ty_cabn,
      wa_cabn     TYPE ty_cabn,
      ta_v_equi   TYPE STANDARD TABLE OF ty_v_equi,
      wa_v_equi   TYPE ty_v_equi,
      ta_crhd     TYPE STANDARD TABLE OF ty_crhd,
      wa_crhd     TYPE ty_crhd,
      ta_final    TYPE STANDARD TABLE OF ty_final,
      wa_final    TYPE ty_final.
CONSTANTS: co_equi(4)     TYPE c VALUE 'EQUI',
           co_classify(8) TYPE c VALUE 'CLASSIFY',
           co_herst(5)    TYPE c VALUE 'HERST',
           co_typbz(5)    TYPE c VALUE 'TYPBZ',
           co_serge(5)    TYPE c VALUE 'SERGE',
           co_mapar(5)    TYPE c VALUE 'MAPAR',
           co_ie02(4)     TYPE c VALUE 'IE02',
           co_atwrt(5)    TYPE c VALUE 'ATWRT',
           co_atflv(5)    TYPE c VALUE 'ATFLV',
           co_klart       TYPE klassenart VALUE '002'.

************************************************************************
* SELECTION SCREEN DECLARATION
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS: pa_uname     TYPE usr21-bname,
            pa_fdate     TYPE cddatum OBLIGATORY,
            pa_ftime     TYPE cduzeit,
            pa_tdate     TYPE cddatum OBLIGATORY,
            pa_ttime     TYPE cduzeit,
            pa_werk      TYPE werks_d OBLIGATORY.
SELECT-OPTIONS: so_eqart FOR equi-eqart.
PARAMETERS: rb_equi      RADIOBUTTON GROUP rb USER-COMMAND sel DEFAULT 'X',
            rb_class     RADIOBUTTON GROUP rb.
SELECTION-SCREEN END OF BLOCK b1.
