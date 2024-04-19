*&---------------------------------------------------------------------*
*&  Include           ZLPSR024_WBS_RECORDS_TOP
*&---------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 25-Sep-2018                                          *
* Created By    : AKMADASU                                             *
* Correction No : S01K900775                                           *
* Object ID     :                                                      *
* Description   : Global declarations and selection screen             *
*----------------------------------------------------------------------*

TABLES: prps, tj02t, cobrb.

TYPES: BEGIN OF ty_prps,
  pspnr	  TYPE  ps_posnr,   "WBS Element
  objnr	  TYPE  j_objnr,    "Object number
  post1	  TYPE  ps_post1,   "PS: Short description (1st text line)
  psphi	  TYPE  ps_psphi,   "Current number of the appropriate project
  loevm	  TYPE  loevm,      "Deletion Indicator
  belkz	  TYPE  ps_belkz,   "Indicator: Account assignment element
  END OF ty_prps,

* Individual Object Status
  BEGIN OF ty_jest,
    objnr	TYPE  j_objnr,    "Object number
    stat  TYPE  j_status,   "Object status
    END OF ty_jest,
* Distribution Rules Settlement Rule Order Settlement
  BEGIN OF ty_cobrb,
    objnr	TYPE j_objnr,	    "Object number
    perbz TYPE perbz_ld,    "Settlement type
    gabja	TYPE gabja,       "Valid-from year
    gabpe	TYPE gabpe,       "Valid-from period
    konty	TYPE konty,	      "Account assignment category
    hkont	TYPE saknr,	      "G/L Account Number
    END OF ty_cobrb,

*  CO Object: Cost Totals for External Postings
   BEGIN OF ty_cosp,
     objnr  TYPE  j_objnr,  "Object number
     WTG005	TYPE  wTGxxx,   "Total Value in Controlling Area Currency
     END OF ty_cosp,

  BEGIN OF ty_final,
    psphi	  TYPE  ps_psphi,   "Current number of the appropriate project
    pspnr	  TYPE  ps_posnr,   "WBS Element
    post1	  TYPE  ps_post1,   "PS: Short description (1st text line)
    stat    TYPE  j_status,   "Object status
    objnr	  TYPE  j_objnr,    "Object number
    loevm	  TYPE  loevm,      "Deletion Indicator
    perbz   TYPE  perbz_ld,   "Settlement type
    konty   TYPE  konty,      "Account assignment category
    hkont   TYPE  saknr,      "G/L Account Number
    belkz	  TYPE  ps_belkz,   "Indicator: Account assignment element
    wTg005  TYPE  wTgxxx,   "Total Value in Controlling Area Currency
    END OF ty_final.

*************************************************************************
*                         Global Declarations                           *
*************************************************************************
DATA: gt_prps TYPE TABLE OF ty_prps,
      gs_prps LIKE LINE OF gt_prps,

      gt_jest TYPE TABLE OF ty_jest,
      gs_jest LIKE LINE OF gt_jest,

      gt_cobrb  TYPE TABLE OF ty_cobrb,
      gt_cobrb_unsetl TYPE TABLE OF ty_cobrb,
      gs_cobrb_unsetl LIKE LINE OF gt_cobrb_unsetl,
      gs_cobrb  LIKE LINE OF gt_cobrb,

      gt_cosp TYPE TABLE OF ty_cosp,
      gs_cosp LIKE LINE OF gt_cosp,

      gt_final  TYPE TABLE OF ty_final,
      gs_final  LIKE LINE OF gt_final,

     go_alv TYPE REF TO cl_salv_table,
     go_columns TYPE REF TO cl_salv_columns_table,
     go_column TYPE REF TO cl_salv_column_table.
*************************************************************************
*                         Selection screen                              *
*************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_psphi FOR prps-psphi,     "Project definition
                S_DIV    FOR PRPS-VERNR,     "Division
                s_pspnr FOR prps-pspnr,     "WBS Element
                s_istat FOR tj02t-istat DEFAULT 'I0001',      "Object Status
                s_hkont FOR cobrb-hkont.    "G/L Account Number

  PARAMETERS:   P_FYEAR LIKE COSP-GJAHR DEFAULT SY-DATUM(4),
                 P_VERS  LIKE COSP-VERSN DEFAULT '0'.    "I1076 Month
SELECTION-SCREEN END OF BLOCK b1.
