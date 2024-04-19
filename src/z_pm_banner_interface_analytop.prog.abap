*&---------------------------------------------------------------------*
*&  Include           Z_PM_BANNER_INTERFACE_ANALYTOP
*&---------------------------------------------------------------------*

**********************************************************
* Declaration For Types (data fetching part)
**********************************************************

TYPE-POOLS: ixml.

TYPES : BEGIN OF ty_final,
          equnr        TYPE equnr,  " Equipment number
          equip_desc   TYPE ktx01,  " Equipment description
          equip_status TYPE zdestatus, " Equipment Status
          update_stat  TYPE char20, " Created/Changed/Update Failed
          tplnr_change TYPE char3,  " Functional location updated
          tplnr_banner TYPE tplnr,  " Banner recommended Floc
          tplnr_sap    TYPE tplnr,  " Current Floc
          p_factor_ch  TYPE char3,  " P_FACTOR Changed
          p_factor_old TYPE zdepfact,  " P_FACTOR Old
          p_factor_new TYPE zdepfact,  " P_FACTOR New
          banner_stort TYPE stort,  " Location in Banner
          sap_stort    TYPE stort,  " Location in SAP
          banner_prm   TYPE raumnr, " Banner Room
          sap_prm      TYPE raumnr, " SAP Room
          banner_grp   TYPE zgrp,   " Banner Cat. Group
          banner_code  TYPE zcat,   " Banner Cat. Code
          install_date TYPE char10, " SAP Installation Date
          banner_notif TYPE qmnum,  " Outstanding Notifications for Banner suggested FLoc
          message      TYPE c LENGTH 1000, " Error messages
        END OF ty_final,

        BEGIN OF ty_iflot ,
          tplnr         TYPE tplnr,
          tplma         TYPE tplma,
          eqart         TYPE eqart,
          stort         TYPE pmloc,
          msgrp         TYPE raumnr,
          del           TYPE char1,
         END OF ty_iflot,

        BEGIN OF ty_qmel,
          qmnum         TYPE viqmel-qmnum,
          tplnr         TYPE viqmel-tplnr,
        END OF ty_qmel,

        BEGIN OF ty_equi,
          equnr        TYPE equnr, " Equipment Number
          datab        TYPE v_equi-datab, " Installation date
          tplnr        TYPE tplnr, " Functional Location
          msgrp        TYPE raumnr, " SAP Room
          stort        TYPE stort, " Location
        END OF ty_equi,

        BEGIN OF ty_iloa,
          tplnr        TYPE iloa-tplnr,
          stort        TYPE iloa-stort,
          msgrp        TYPE iloa-msgrp,
        END OF ty_iloa.

*----------------------------------------------------------------------*
* CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS event_class DEFINITION FINAL.
* Handling hotspot click
  PUBLIC SECTION.
    CLASS-METHODS:
    handle_double_click
    FOR EVENT double_click OF cl_salv_events_table IMPORTING row "#EC NEEDED
                                                             column , "#EC NEEDED
    set_top_of_page
                  CHANGING
                      co_alv TYPE REF TO cl_salv_table. " Set Top of page

ENDCLASS. "lcl_event_receiver DEFINITION
**********************************************************
* Global data
**********************************************************
DATA: gv_equnr TYPE equi-equnr,
      gv_date  TYPE sy-datum,
      gv_time  TYPE sy-uzeit,

      gv_created TYPE int4,
      gv_changed TYPE int4,
      gv_errored TYPE int4,
      gv_nochang TYPE int4,
      gv_missing TYPE int4,

gt_final    TYPE STANDARD TABLE OF ty_final,
gt_email    TYPE STANDARD TABLE OF ty_final,
gt_logs     TYPE STANDARD TABLE OF zpmt_equip_log,
gt_fl_level TYPE STANDARD TABLE OF zpmt_fl_level,
gt_iloa_e   TYPE STANDARD TABLE OF ty_iloa,
gt_iflot    TYPE STANDARD TABLE OF ty_iflot,
gt_qmel     TYPE STANDARD TABLE OF ty_qmel,
gt_equi     TYPE STANDARD TABLE OF ty_equi.

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
        rf_event         TYPE REF TO event_class,
        rf_events        TYPE REF TO cl_salv_events_table,
        rf_rows          TYPE REF TO salv_t_row.

FIELD-SYMBOLS: <fs_logs>     LIKE LINE OF gt_logs,
               <fs_iloa_e>   LIKE LINE OF gt_iloa_e,
               <fs_iflot>    LIKE LINE OF gt_iflot,
               <fs_equi>     LIKE LINE OF gt_equi,
               <fs_qmel>     LIKE LINE OF gt_qmel,
               <fs_final>    LIKE LINE OF gt_final,
               <fs_fl_level> LIKE LINE OF gt_fl_level.

**********************************************************
* Selection Screen
**********************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
SELECT-OPTIONS: s_equnr  FOR gv_equnr. " Eequipment Number
SELECTION-SCREEN END OF BLOCK b1.
