*&---------------------------------------------------------------------*

*&  Include           Z_PMODOURANT_TANK_TOP
* Author             : Shiladitya Ghosh                                *
* Date               : 17/07/2014 (dd/mm/yyyy)                         *
* Technical Contact  : Shiladitya Ghosh                                *
* Business Contact   : Eric VanRumybeke                                *
*----------------------------------------------------------------------*
*                      Modification Log                                *
*                                                                      *
* Changed On   Changed By           CTS          Description           *
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


*----------------------------------------------------------------------*
* CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS event_class DEFINITION FINAL.
* Handling hotspot click
  PUBLIC SECTION.
    METHODS:
    handle_double_click
    FOR EVENT double_click OF cl_salv_events_table IMPORTING row "#EC NEEDED
                                                             column . "#EC NEEDED

ENDCLASS. "lcl_event_receiver DEFINITION

**********************************************************
* Tables
**********************************************************
TABLES: equi,
        iloa,
        ausp.

**********************************************************
* Selection Screen
**********************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_staton FOR iloa-stort, "station
                s_equnr  FOR equi-equnr, " Eequipment Number
                s_ttype  FOR ausp-atwrt NO INTERVALS. " Tank Type
PARAMETER:      p_date   TYPE imrg-idate. " Measurement Date
SELECTION-SCREEN END OF BLOCK b1.

**********************************************************
* Declaration For Types (data fetching part)
**********************************************************
TYPES : BEGIN OF ty_final,
          funcloc      TYPE tplnr,             " Function Location
          stationid    TYPE stort,             "stationid/functional location level 3
          stationname  TYPE pltxt,            "stationname/functional location description
          equipment    TYPE equnr,             "equipment number
          equip_desc   TYPE ktx01,             " Equipment description
          techrespn    TYPE tc24-ktext,        "technician responsible/person responsible
          tank_type    TYPE atwrt,             "Tank Type
          head_type    TYPE atwrt,            "Tank type/func loc characteristic
          capacity(10) TYPE p DECIMALS 2,             "Liquid Capacity/func loc characteristic
          capacity_uom TYPE atwrt,           " Capacity UOM
          left(10)     TYPE p DECIMALS 2,             "Tank level Left/func loc characteristics
          level_uom    TYPE atwrt,
          measure_date TYPE imrc_idate,
          volume(10)   TYPE p DECIMALS 2,
          fill(10)     TYPE p DECIMALS 2,
          %(10)        TYPE p DECIMALS 2,
          message      TYPE char200,
        END OF ty_final,

        BEGIN OF ty_equi,
          equnr    TYPE equi-equnr,              "equipment number
          eqart    TYPE equi-eqart,              " Equipment type
          datab    TYPE v_equi-datab,
          objnr    TYPE j_objnr,
          datbi    TYPE v_equi-datbi,
          pm_objty TYPE v_equi-pm_objty,
          gewrk    TYPE v_equi-gewrk,
          eqktx    TYPE ktx01,
          tplnr    TYPE v_equi-tplnr,
          werks    TYPE werks_d,
          stort    TYPE iloa-stort,              "location of maintenance object
          del      TYPE char1,
        END OF ty_equi,

        BEGIN OF ty_tech,
          objty TYPE cr_objty,
          objid TYPE cr_objid,
          ktext TYPE tc24-ktext,
        END OF ty_tech,

         BEGIN OF ty_cawnt,
          atwrt TYPE atwrt,
          atwtb TYPE atwtb,
         END OF ty_cawnt,

      BEGIN OF ty_cabn,
        atinn TYPE cabn-atinn,
        atnam TYPE cabn-atnam,
        atfor TYPE cabn-atfor,
      END OF ty_cabn,

      BEGIN OF ty_ausp,
        objek TYPE ausp-objek,
        atinn TYPE ausp-atinn,
        mafid TYPE ausp-mafid,
        klart TYPE ausp-klart,
        adzhl TYPE ausp-adzhl,
        atwrt TYPE ausp-atwrt,
        atflv TYPE ausp-atflv,
        del   TYPE char1,
      END OF ty_ausp,

      BEGIN OF ty_inob,
        cuobj TYPE inob-cuobj,
        objek TYPE inob-objek,
      END OF ty_inob,

      BEGIN OF ty_t499s,
        werks TYPE t499s-werks,
        stand TYPE t499s-stand,
        ktext TYPE t499s-ktext,
      END OF ty_t499s,

      BEGIN OF ty_objek,
        objek TYPE ausp-objek,
      END OF ty_objek,

      BEGIN OF ty_imptt,
          point TYPE imptt-point,
          mpobj TYPE imptt-mpobj,
          psort TYPE imptt-psort,
          pttxt TYPE imptt-pttxt,
          mrngu TYPE imptt-mrngu,
      END OF ty_imptt,

      BEGIN OF ty_imrg,
        mdocm TYPE imrg-mdocm,
        point TYPE imrg-point,
        idate TYPE imrg-idate,
        itime TYPE imrg-itime,
        recdv TYPE imrg-recdv,
        recdu TYPE imrg-recdu,
        vlcod TYPE imrg-vlcod,
      END OF ty_imrg.

**********************************************************
* Data Declaration
**********************************************************
DATA : ta_final   TYPE STANDARD TABLE OF ty_final,
       ta_equi    TYPE STANDARD TABLE OF ty_equi,
       ta_tech    TYPE STANDARD TABLE OF ty_tech,
       ta_cabn    TYPE STANDARD TABLE OF ty_cabn,
       ta_ausp    TYPE STANDARD TABLE OF ty_ausp,
       ta_inob    TYPE STANDARD TABLE OF ty_inob,
       ta_objek   TYPE STANDARD TABLE OF ty_objek,
       ta_cawnt   TYPE STANDARD TABLE OF ty_cawnt,
       ta_t499s   TYPE STANDARD TABLE OF ty_t499s,
       ta_imrg    TYPE STANDARD TABLE OF ty_imrg,
       ta_imptt   TYPE STANDARD TABLE OF ty_imptt,

       ta_atinn   TYPE RANGE OF ausp-atinn,

       rf_event   TYPE REF TO event_class.

**********************************************************
* Declaration For Variables (used for program logic)
**********************************************************

**********************************************************
* Data declarations for ALV Main list
**********************************************************
DATA :  event_receiver   TYPE REF TO event_class,           "#EC NEEDED
        rf_alv           TYPE REF TO cl_salv_table ##needed, "varaibles for ALV display
        rf_alv_functions TYPE REF TO cl_salv_functions_list ##needed,
        rf_columns       TYPE REF TO cl_salv_columns ##needed,
        rf_column        TYPE REF TO cl_salv_column ##needed,
        rf_display       TYPE REF TO cl_salv_display_settings,
        rf_layout        TYPE REF TO cl_salv_layout,
        rf_col_list      TYPE REF TO cl_salv_column_list,
        rf_events        TYPE REF TO cl_salv_events_table,
        rf_rows          TYPE REF TO salv_t_row.
