**&---------------------------------------------------------------------*
**&  Include           Z_PM_GLOBAL01
***This include is used for all gobal variable for Equipment
**&---------------------------------------------------------------------*
**TYPES :BEGIN OF ty_equi,
**         equnr TYPE equnr,
**        END OF ty_equi.
*TYPES : BEGIN OF ty_iflot ,
*        tplnr TYPE tplnr,
*        tplma TYPE tplma,
*     END OF ty_iflot.
*TYPES : BEGIN OF ty_iloa ,
*   tplnr TYPE tplnr,
*   swerk TYPE swerk,
*   stort TYPE pmloc,
*   msgrp TYPE raumnr,
*END OF ty_iloa.
*
*DATA : ta_iloa TYPE STANDARD TABLE OF ty_iloa,
*      wa_iloa TYPE ty_iloa,
*      ta_iflot TYPE STANDARD TABLE OF ty_iflot,
*      wa_iflot TYPE ty_iflot,
*      lv_external_no TYPE bapi_itob_parms-equipment ,
*      lv_text TYPE string,
*      lv_tplnr TYPE tplnr,
*      wa_general TYPE bapi_itob,
*      wa_generalx TYPE bapi_itobx,
*      wa_specific TYPE bapi_itob_eq_only,
*      wa_specificx TYPE bapi_itob_eq_onlyx,
*      wa_return TYPE bapiret2,
*      wa_equip TYPE zpms_equip,
*      wa_ret TYPE zpms_equip_ret,
*      ta_ret TYPE STANDARD TABLE OF zpms_equip_ret,
*      lv_equip_created TYPE bapi_itob_parms-equipment,
*      ta_t499s TYPE STANDARD TABLE OF t499s,
*      wa_t499s TYPE t499s,
*      ta_fl_level TYPE STANDARD TABLE OF zpmt_fl_level,
*      wa_fl_level TYPE zpmt_fl_level.
**      wa_equi TYPE ty_equi,
**      ta_equi TYPE STANDARD TABLE OF ty_equi.
*DATA : ta_installed_equi TYPE STANDARD TABLE OF alm_me_installed_equi,
*       wa_installed_equi TYPE alm_me_installed_equi,
*       ts_dis_ret TYPE bapireturn.
