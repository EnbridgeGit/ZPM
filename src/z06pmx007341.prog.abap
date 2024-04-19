*&---------------------------------------------------------------------*
*&  Include           Z06PMX007341
*&---------------------------------------------------------------------*

INCLUDE gfw_dc_pres.
TABLES: z06pms_cp_tplnr,rihqmel_list,diqmel.
TYPES : BEGIN OF ty_order,
  auart TYPE auart,
 tplnr TYPE tplnr,
  END OF ty_order.

TYPES : BEGIN OF ty_notif,
qmart TYPE string, "qmart,
tplnr TYPE tplnr,
END OF ty_notif.

TYPES : BEGIN OF ty_stat,
field TYPE char30,
count TYPE i,
END OF ty_stat.
DATA: gv_save_code LIKE sy-ucomm,
      gv_program   TYPE sy-cprog,
      gv_firstcall TYPE i,
      gv_id        TYPE i,
      gv_retval    TYPE symsgno,
      gv_return    LIKE sy-subrc.

DATA: gt_selparams TYPE STANDARD TABLE OF rsparams  WITH HEADER LINE,
      gt_config    TYPE STANDARD TABLE OF z06pmt_cp_config WITH HEADER LINE,
      t_config    TYPE STANDARD TABLE OF zntconfig WITH HEADER LINE,
      t_bdcdata   TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE,
      t_selparams TYPE STANDARD TABLE OF rsparams  WITH HEADER LINE,
      ta_order TYPE STANDARD TABLE OF ty_order,
      wa_order TYPE ty_order,
       ta_notif TYPE STANDARD TABLE OF ty_notif,
      wa_notif TYPE ty_notif,
      ta_statistics TYPE STANDARD TABLE OF ty_stat WITH HEADER LINE,
      gs_defaults    TYPE z06pms_cp_defaults,
      gs_nt_defaults TYPE z06pms_cp_defaults,
      x_defaults    TYPE z06pms_cp_defaults,
      x_nt_defaults TYPE z06pms_cp_defaults,
      r_arbpl     TYPE RANGE OF gewrk,
      r_strno     TYPE RANGE OF  diqmel-shn_funct_loc ,"ilom_strno.
*{   INSERT         D30K924358                                        1
      r_date      TYPE RANGE OF erdat,
*}   INSERT
      r_equnr     TYPE RANGE OF equnr.

*DATA: gs_default   TYPE z06pmt_cp_deflt.

CONSTANTS:  co_all_logo          TYPE zpm_logo   VALUE 'ZUGLOGO_CP',
            co_dy_logo           TYPE zpm_logo   VALUE 'ZUGLOGO_CP',
            co_la_logo           TYPE zpm_logo   VALUE 'ZUGLOGO_CP',
            co_ld_logo           TYPE zpm_logo   VALUE 'ZUGLOGO_CP',
            c_tcode_iw38         TYPE sy-tcode   VALUE 'IW38',
            c_tcode_iw65         TYPE sy-tcode   VALUE 'IW65',
            c_tcode_mci7         TYPE sy-tcode   VALUE 'MCI7',
            c_tcode_mcjc         TYPE sy-tcode   VALUE 'MCJC',
            c_tcode_mcjb         TYPE sy-tcode   VALUE 'MCJB',
            c_mode_error         TYPE char1      VALUE 'E',
            c_sync               TYPE char1      VALUE 'S',
            c_authority_ok      TYPE sy-subrc   VALUE 1,
            c_pattern_n         TYPE char2    VALUE 'M*',
            c_asterisk          TYPE char1    VALUE '*',
            c_1                 TYPE c          VALUE '1',
            c_2                 TYPE c          VALUE '2',
            c_n                 TYPE c          VALUE 'M',
            c_hyphen            TYPE c          VALUE '-',
            c_comma             TYPE c          VALUE ','.

DATA:  ok_code           TYPE sy-ucomm,
       go_picture_control TYPE REF TO cl_gui_picture,
       go_pic_container   TYPE REF TO cl_gui_custom_container,
       go_chart_container TYPE REF TO cl_gui_custom_container,
       go_inst            TYPE REF TO lcl_dc_pres,
       go_manager         TYPE REF TO if_dc_management,
       go_chart_inst      TYPE REF TO cl_gui_gp_pres.

DATA: BEGIN OF gt_notif OCCURS 0,
      qmart TYPE  qmart,
      qmnum TYPE  qmnum,
      END OF   gt_notif.

DATA: BEGIN OF gt_order OCCURS 0,
      auart  TYPE aufart,
      aufnr  TYPE aufnr,
      END OF   gt_order.

DATA: BEGIN OF gt_auart OCCURS 0,
      auart TYPE aufart,
      END OF   gt_auart.

SELECTION-SCREEN : BEGIN OF SCREEN 9100 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-001 FOR FIELD s_arbpl.
SELECT-OPTIONS : s_arbpl FOR rihqmel_list-arbpl.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-002 FOR FIELD s_strno.
SELECT-OPTIONS : s_strno FOR diqmel-shn_funct_loc MATCHCODE OBJECT iflm. " change by jyoti
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-006 FOR FIELD s_eqn.
SELECT-OPTIONS : s_eqn FOR rihqmel_list-equnr MATCHCODE OBJECT equi.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-011 FOR FIELD s_date.
SELECT-OPTIONS : s_date FOR rihqmel_list-erdat.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : END OF SCREEN 9100.
