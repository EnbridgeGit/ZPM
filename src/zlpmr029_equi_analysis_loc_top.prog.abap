*&---------------------------------------------------------------------*
*&  Include           ZLPMR029_EQUI_ANALYSIS_LOC_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLPMR029_EQUI_ANALYSIS_LOC_TOP                 *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 10-JUL-2017                                    *
*& Object ID          : ACR-4555                                       *
*& Application Area   : PM                                             *
*& Description        : Equipment Analysis by Location.                *
*&---------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 01-Oct-2018                                          *
* Modified By   : KBANERJEE                                            *
* Correction No : D30K929166                                           *
* Description   : Changes for performance improvement  and correction  *
*                 of timeout error.                                    *
*----------------------------------------------------------------------*
TABLES: equi,
        iloa,
        rihqmel_list.

************************************************************************
* TYPES DECLARATION
************************************************************************
TYPES: BEGIN OF ty_v_equi,
         equnr     TYPE equnr,
         eqart     TYPE eqart,
         herst     TYPE herst,
         serge     TYPE serge,
         typbz     TYPE typbz,
         objnr     TYPE j_objnr,
         mapar     TYPE mapar,
         gewrk     TYPE lgwid,
         tplnr     TYPE tplnr,
       END OF ty_v_equi,
       BEGIN OF ty_jest,
         objnr     TYPE j_objnr,
         stat      TYPE j_status,
       END OF ty_jest,
       BEGIN OF ty_t370k_t,
         eqart     TYPE eqart,
         eartx     TYPE eartx,
       END OF ty_t370k_t,
       BEGIN OF ty_crhd,
         objid     TYPE cr_objid,
         arbpl     TYPE arbpl,
       END OF ty_crhd,
       BEGIN OF ty_klah,
         clint     TYPE clint,
         class     TYPE klasse_d,
       END OF ty_klah,
       BEGIN OF ty_kssk,
         objek     TYPE objnum,
         clint     TYPE clint,
       END OF ty_kssk,
       BEGIN OF ty_ksml,
         clint     TYPE clint,
         posnr     TYPE kposnr,
         imerk     TYPE atinn,
       END OF ty_ksml,
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
*       BEGIN OF ty_inob,
*         cuobj     TYPE char50,
*         objek     TYPE cuobn,
*       END OF ty_inob,
*       BEGIN OF ty_ausp,
*         objek     TYPE objnum,
*         atinn     TYPE atinn,
*       END OF ty_ausp,
       BEGIN OF ty_inob,
         objeki    TYPE cuobn,
         cuobj     TYPE char50,
         objek     TYPE objnum,
         atinn     TYPE atinn,
       END OF ty_inob,
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
       BEGIN OF ty_final,
         tplnr     TYPE string,
         gewrk     TYPE lgwid,
         arbpl     TYPE arbpl,
         eqart     TYPE eqart,
         eartx     TYPE eartx,
         equnr_cnt TYPE i,
         herst_cnt TYPE i,
         typbz_cnt TYPE i,
         mapar_cnt TYPE i,
         serge_cnt TYPE i,
         tchar_cnt TYPE i,
         pchar_cnt TYPE i,
       END OF ty_final.

************************************************************************
* DATA DECLARATION
************************************************************************
DATA: ta_v_equi       TYPE STANDARD TABLE OF ty_v_equi,
      wa_v_equi       TYPE ty_v_equi,
      ta_v_equi_final TYPE STANDARD TABLE OF ty_v_equi,
      wa_v_equi_final TYPE ty_v_equi,
      ta_jest         TYPE STANDARD TABLE OF ty_jest,
      wa_jest         TYPE ty_jest,
      ta_t370k_t      TYPE STANDARD TABLE OF ty_t370k_t,
      wa_t370k_t      TYPE ty_t370k_t,
      ta_crhd         TYPE STANDARD TABLE OF ty_crhd,
      wa_crhd         TYPE ty_crhd,
      ta_klah         TYPE STANDARD TABLE OF ty_klah,
      wa_klah         TYPE ty_klah,
      ta_kssk         TYPE STANDARD TABLE OF ty_kssk,
      wa_kssk         TYPE ty_kssk,
      ta_ksml         TYPE STANDARD TABLE OF ty_ksml,
      wa_ksml         TYPE ty_ksml,
      ta_ksml_final   TYPE STANDARD TABLE OF ty_ksml,
      wa_ksml_final   TYPE ty_ksml,
      ta_inob         TYPE STANDARD TABLE OF ty_inob,
      wa_inob         TYPE ty_inob,
**--START OF CHANGES FOR CHG0125142 BY KBANERJEE
*      ta_ausp         TYPE STANDARD TABLE OF ty_ausp,
*      wa_ausp         TYPE ty_ausp,
**--END OF CHANGES FOR CHG0125142 BY KBANERJEE
      ta_final        TYPE STANDARD TABLE OF ty_final,
      wa_final        TYPE ty_final.

CONSTANTS: co_i0076   TYPE j_status VALUE 'I0076',
           co_i0320   TYPE j_status VALUE 'I0320'.

************************************************************************
* SELECTION SCREEN DECLARATION
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: so_tplnr FOR iloa-tplnr,
                so_eqart FOR equi-eqart,
                so_gewrk FOR rihqmel_list-arbpl.
SELECTION-SCREEN END OF BLOCK b1.
