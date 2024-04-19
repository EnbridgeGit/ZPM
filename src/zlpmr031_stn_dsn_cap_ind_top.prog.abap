*&---------------------------------------------------------------------*
*&  Include           ZLPMR031_STN_DSN_CAP_IND_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZLPMR031_STN_DSN_CAP_IND_TOP                   *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 17-Aug-2017                                    *
*& Object ID          : ACR-4673                                       *
*& Application Area   : PM                                             *
*& Description        : Station design capacity indicator.             *
*&---------------------------------------------------------------------*

************************************************************************
* TYPES DECLARATION
************************************************************************
TYPES: BEGIN OF ty_cdhdr,
         objectclas TYPE cdobjectcl,
         objectid   TYPE cdobjectv,
         changenr   TYPE cdchangenr,
         udate      TYPE cddatum,
         utime      TYPE cduzeit,
       END OF ty_cdhdr,
       BEGIN OF ty_zpmt_stncapind,
         object     TYPE fieldname,
         field      TYPE atnam,
       END OF ty_zpmt_stncapind,
       BEGIN OF ty_cdpos,
         objectclas TYPE cdobjectcl,
         objectid   TYPE cdobjectv,
         changenr   TYPE cdchangenr,
         fname      TYPE fieldname,
       END OF ty_cdpos,
        BEGIN OF ty_cdpos_cl,
         objectclas TYPE cdobjectcl,
         objectid   TYPE cdobjectv,
         changenr   TYPE cdchangenr,
         tabkey     TYPE cdtabkey,
       END OF ty_cdpos_cl,
       BEGIN OF ty_char,
         cuobj      TYPE char50,
         atinn      TYPE atinn,
       END OF ty_char,
       BEGIN OF ty_v_equi,
         equnr      TYPE equnr,
         eqart      TYPE eqart,
         tplnr      TYPE tplnr,
       END OF ty_v_equi,
       BEGIN OF ty_inob,
         cuobj      TYPE char50,
         objek      TYPE cuobn,
       END OF ty_inob,
       BEGIN OF ty_cabn,
         atinn      TYPE atinn,
         atnam      TYPE atnam,
       END OF ty_cabn,
       BEGIN OF ty_objectid,
         objectclas TYPE cdobjectcl,
         objectid   TYPE equnr,
       END OF ty_objectid,
       BEGIN OF ty_iflot,
         tplnr      TYPE tplnr,
         tplkz      TYPE tplkz,
       END OF ty_iflot,
       BEGIN OF ty_inob_floc,
         cuobj      TYPE cuobj,
         objek      TYPE cuobn,
       END OF ty_inob_floc,
       BEGIN OF ty_kssk,
         objek      TYPE objnum,
       END OF ty_kssk.

************************************************************************
* DATA DECLARATION
************************************************************************
DATA: ta_cdhdr          TYPE STANDARD TABLE OF ty_cdhdr,
      wa_cdhdr          TYPE ty_cdhdr,
      ta_cdhdr_cl       TYPE STANDARD TABLE OF ty_cdhdr,
      ta_zpmt_stncapind TYPE STANDARD TABLE OF ty_zpmt_stncapind,
      wa_zpmt_stncapind TYPE ty_zpmt_stncapind,
      ta_cdpos          TYPE STANDARD TABLE OF ty_cdpos,
      wa_cdpos          TYPE ty_cdpos,
      ta_cdpos_final    TYPE STANDARD TABLE OF ty_cdpos,
      ta_cdpos_cl       TYPE STANDARD TABLE OF ty_cdpos_cl,
      wa_cdpos_cl       TYPE ty_cdpos_cl,
      ta_char           TYPE STANDARD TABLE OF ty_char,
      wa_char           TYPE ty_char,
      ta_v_equi         TYPE STANDARD TABLE OF ty_v_equi,
      wa_v_equi         TYPE ty_v_equi,
      ta_inob           TYPE STANDARD TABLE OF ty_inob,
      wa_inob           TYPE ty_inob,
      ta_cabn           TYPE STANDARD TABLE OF ty_cabn,
      wa_cabn           TYPE ty_cabn,
      ta_objectid       TYPE STANDARD TABLE OF ty_objectid,
      wa_objectid       TYPE ty_objectid,
      ta_iflot          TYPE STANDARD TABLE OF ty_iflot,
      wa_iflot          TYPE ty_iflot,
      ta_inob_floc      TYPE STANDARD TABLE OF ty_inob_floc,
      wa_inob_floc      TYPE ty_inob_floc,
      ta_kssk           TYPE STANDARD TABLE OF ty_kssk,
      wa_kssk           TYPE ty_kssk.

CONSTANTS: co_equi(4)     TYPE c VALUE 'EQUI',
           co_classify(8) TYPE c VALUE 'CLASSIFY',
           co_ie02(4)     TYPE c VALUE 'IE02',
           co_fname(5)    TYPE c VALUE 'FNAME',
           co_atwrt(5)    TYPE c VALUE 'ATWRT',
           co_atflv(5)    TYPE c VALUE 'ATFLV',
           co_klart_002   TYPE klassenart VALUE '002',
           co_klart_003   TYPE klassenart VALUE '003',
           co_2591        TYPE clint VALUE '2591',
           co_2590        TYPE clint VALUE '2590'.

************************************************************************
* SELECTION SCREEN DECLARATION
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS: pa_fdate     TYPE cddatum OBLIGATORY,
            pa_ftime     TYPE cduzeit,
            pa_tdate     TYPE cddatum OBLIGATORY,
            pa_ttime     TYPE cduzeit.
SELECTION-SCREEN END OF BLOCK b1.
