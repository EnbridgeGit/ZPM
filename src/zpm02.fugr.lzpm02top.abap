FUNCTION-POOL zpm02                      MESSAGE-ID sv.
*This include is used for all gobal variable for Work Order creation

TYPES : BEGIN OF ty_iloa ,
       tplnr TYPE tplnr,
       stort TYPE pmloc,
       msgrp TYPE raumnr,
END OF ty_iloa.
TYPES : BEGIN OF ty_aufk,
      aufnr TYPE aufnr,
      zzpmbworkord TYPE zpmbworkord,
      zzpmbworktype TYPE zpmbworktype,
      zzpmbscheddate TYPE zpmbscheddate,
      zzpmbstatus TYPE zpmbstatus,
  END OF ty_aufk.

TYPES : BEGIN OF ty_order,
  aufnr TYPE aufnr ,
  zzpmcompdate  TYPE zpmcompdate,
  zzpmbworkord TYPE zpmbworkord,
  zzpmbworktype  TYPE zpmbworktype,
  zzpmbscheddate TYPE zpmbscheddate,
  zzpmbstatus TYPE zpmbstatus ,

  END OF ty_order .
DATA : wa_order TYPE ty_order.
DATA : ta_iloa TYPE STANDARD TABLE OF ty_iloa,
       wa_iloa TYPE ty_iloa.

DATA: lv_created_order TYPE  aufk-aufnr,
      bdcdata LIKE bdcdata  OCCURS 0 WITH HEADER LINE,
      lv_orderid TYPE aufnr,
      lv_tplnr TYPE tplnr,
      lv_3tplnr TYPE tplnr,
      wa_order_header TYPE zpms_header,
      wa_order_oper TYPE zpms_operation,
      lv_vornr TYPE vornr,
      ta_banner_type TYPE STANDARD TABLE OF zpmt_banner_type ,
      wa_banner_type TYPE zpmt_banner_type,
ta_methods TYPE STANDARD TABLE OF bapi_alm_order_method,
wa_methods LIKE LINE OF ta_methods,
ta_header TYPE STANDARD TABLE OF bapi_alm_order_headers_i,
wa_header LIKE LINE OF ta_header,
ta_header_up TYPE STANDARD TABLE OF bapi_alm_order_headers_up,
wa_header_up LIKE LINE OF ta_header,
ta_header_srv TYPE STANDARD TABLE OF bapi_alm_order_srvdat_e,
ta_operation TYPE STANDARD TABLE OF bapi_alm_order_operation,
wa_operation LIKE LINE OF ta_operation,
ta_operation_up TYPE STANDARD TABLE OF bapi_alm_order_operation_up,
wa_operation_up LIKE LINE OF ta_operation_up,
return TYPE STANDARD TABLE OF bapiret2,
wa_return LIKE LINE OF return,
et_numbers TYPE STANDARD TABLE OF bapi_alm_numbers,
wa_numbers LIKE LINE OF et_numbers,
wa_ret TYPE  zpms_return,
wa_ext TYPE bapiparex,
ta_ext TYPE STANDARD TABLE OF bapiparex,
ta_objectlist TYPE STANDARD TABLE OF bapi_alm_order_objectlist,
wa_objectlist TYPE bapi_alm_order_objectlist,
ta_objectlist_up TYPE STANDARD TABLE OF bapi_alm_order_olist_up,
wa_objectlist_up TYPE bapi_alm_order_olist_up,
ta_aufk TYPE STANDARD TABLE OF ty_aufk,
wa_aufk TYPE ty_aufk.
DATA  lv_error TYPE string.
DATA  ta_return TYPE STANDARD TABLE OF  zpms_return.




INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzpm02t00                               . "view rel. data dcl.

*Equipment Data declaration
**This include is used for all gobal variable for Equipment
*&---------------------------------------------------------------------*
TYPES :BEGIN OF ty_equi,
         equnr TYPE equnr,
        END OF ty_equi.
TYPES : BEGIN OF ty_iflot ,
        tplnr TYPE tplnr,
        tplma TYPE tplma,
        eqart TYPE eqart,
        stort TYPE pmloc, "NREDDY
        msgrp TYPE raumnr,"NREDDY
        iwerk TYPE iwerk,
        equip_install TYPE c,
        del TYPE c,
     END OF ty_iflot.
TYPES : BEGIN OF ty_iloa_e ," for equipment (Suffix _e)
       tplnr TYPE tplnr,
       swerk TYPE swerk,
       stort TYPE pmloc,
       msgrp TYPE raumnr,
END OF ty_iloa_e.
DATA : gv_no_ch TYPE i,
      gv_ch TYPE i.

DATA : ta_iloa_e TYPE STANDARD TABLE OF ty_iloa_e,
wa_iloa_e TYPE ty_iloa_e,
ta_iflot TYPE STANDARD TABLE OF ty_iflot,
wa_iflot TYPE ty_iflot,
lv_external_no TYPE bapi_itob_parms-equipment ,
lv_text TYPE string,
lv_floc TYPE tplnr,
lv_err_flg TYPE c,
wa_general TYPE bapi_itob,
wa_generalx TYPE bapi_itobx,
wa_specific TYPE bapi_itob_eq_only,
wa_specificx TYPE bapi_itob_eq_onlyx,
wa_return_e TYPE bapiret2,
wa_equip TYPE zpms_equip,
wa_equip_log TYPE zpmt_equip_log,
wa_ret_e TYPE zpms_equip_ret,
ta_ret_e TYPE STANDARD TABLE OF zpms_equip_ret,
lv_equip_created TYPE bapi_itob_parms-equipment,
lv_index TYPE sy-tabix,
ta_t499s TYPE STANDARD TABLE OF t499s,
wa_t499s TYPE t499s,
ta_fl_level TYPE STANDARD TABLE OF zpmt_fl_level,
wa_fl_level TYPE zpmt_fl_level.
*wa_equi TYPE equi," ty_equi,
*ta_equi TYPE STANDARD TABLE OF equi,"ty_equi,
*ta_eqkt TYPE STANDARD TABLE OF eqkt,
*wa_eqkt TYPE eqkt,
*ta_equz TYPE STANDARD TABLE OF equz,
*wa_equz TYPE equz.
DATA : ta_installed_equi TYPE STANDARD TABLE OF alm_me_installed_equi,
       wa_installed_equi TYPE alm_me_installed_equi,
       ts_dis_ret TYPE bapireturn,
       ts_dis_ret2 TYPE bapiret2,
       gv_loghandle      TYPE balloghndl,
       gv_applog_act TYPE tvarvc-low,
       gv_atinn TYPE cabn-atinn,
       ta_loghandle TYPE bal_t_logh.

TYPES : BEGIN OF ty_vequi,
  equnr TYPE equnr,
  eqtyp TYPE eqtyp,
  eqart TYPE eqart,
  ansdt TYPE ansdt,
  herst TYPE herst,
  serge TYPE serge,
  typbz TYPE typbz,
  sernr TYPE sernr,
  mapar TYPE mapar,
  tidnr TYPE v_equi-tidnr,
  eqktx TYPE v_equi-eqktx,
  tplnr TYPE tplnr,
  swerk TYPE v_equi-swerk,
  stort TYPE pmloc,
  msgrp TYPE v_equi-msgrp,
END OF ty_vequi.

TYPES : BEGIN OF ty_vequi_fl,
  equnr TYPE equnr,
  eqtyp TYPE eqtyp,
  eqart TYPE eqart,
  tplnr TYPE tplnr,
END OF ty_vequi_fl.


TYPES : BEGIN OF ty_final,
  equnr TYPE equnr,
  tplnr TYPE tplnr,
  eqart TYPE eqart,
  iwerk TYPE iwerk,
  chnge TYPE c, " if X , equipment is for chnge
  installed TYPE c, "if X  Equipment is chnge and need to installed
END OF ty_final.
TYPES : BEGIN OF ty_il,
    iloan TYPE iloan,
    stort TYPE pmloc,
    END OF ty_il.
DATA : ta_iloa1 TYPE STANDARD TABLE OF ty_il,
      wa_iloa1 TYPE ty_il.

TYPES: BEGIN OF ty_objek,
   objek TYPE ausp-objek,
 END OF ty_objek,

      BEGIN OF ty_inob,
        cuobj TYPE inob-cuobj,
        objek TYPE inob-objek,
      END OF ty_inob,

      BEGIN OF ty_ausp,
        objek TYPE ausp-objek,
        atflv TYPE ausp-atflv,
      END OF ty_ausp.

DATA : ta_vequi TYPE STANDARD TABLE OF ty_vequi,
       ta_vequi_fl TYPE STANDARD TABLE OF ty_vequi_fl, "Installations at a specific functional locaitons.
       ta_objek TYPE STANDARD TABLE OF ty_objek,
       ta_ausp    TYPE STANDARD TABLE OF ty_ausp,
       ta_inob    TYPE STANDARD TABLE OF ty_inob,
       wa_objek TYPE ty_objek,
wa_vequi TYPE ty_vequi.

DATA : ta_final TYPE STANDARD TABLE OF ty_final,
      wa_final TYPE ty_final.
FIELD-SYMBOLS : <fs_final> TYPE ty_final.
