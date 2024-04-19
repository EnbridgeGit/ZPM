*&---------------------------------------------------------------------*
*& Include ZCPMAINTOP                                        Module Pool      ZCPMAIN
*&
*&---------------------------------------------------------------------*

PROGRAM  zcpmain MESSAGE-ID zntemplate.

TABLES : rihqmel_list, diqmel, zntdeflt, zntlogo.

TYPES : BEGIN OF ty_floc,
         tplnr   TYPE zzsysno,
         pltxt   TYPE zzsysdesc,
        END OF   ty_floc.

*-- Constants declaration
CONSTANTS : c_asterisk          TYPE char1    VALUE '*',
            c_pattern_n         TYPE char2    VALUE '',
            c_tcode_sap_inbox   TYPE sy-tcode VALUE 'SBWP',
            c_tcode_iw66        TYPE sy-tcode VALUE 'IW66',
            c_tcode_iw37n       TYPE sy-tcode VALUE 'IW37N',
            c_tcode_iw38        TYPE sy-tcode VALUE 'IW38',
            c_tcode_il01        TYPE sy-tcode VALUE 'IL01',
            c_tcode_il02        TYPE sy-tcode VALUE 'IL02',
            c_tcode_il03        TYPE sy-tcode VALUE 'IL03',
            c_tcode_il05        TYPE sy-tcode VALUE 'IL05',
            c_tcode_iw21        TYPE sy-tcode VALUE 'IW21',
            c_tcode_iw22        TYPE sy-tcode VALUE 'IW22',
            c_tcode_iw23        TYPE sy-tcode VALUE 'IW23',
            c_tcode_iw28        TYPE sy-tcode VALUE 'IW28',
              c_tcode_ZACPPEM        TYPE sy-tcode VALUE 'ZACPPEM',

            c_tcode_iw31        TYPE sy-tcode VALUE 'IW31',
            c_tcode_iw32        TYPE sy-tcode VALUE 'IW32',
            c_tcode_iw33        TYPE sy-tcode VALUE 'IW33',
            c_tcode_cat2        TYPE sy-tcode VALUE 'CAT2',
            c_tcode_pr04        TYPE sy-tcode VALUE 'PR04',
            c_tcode_iw3d        TYPE sy-tcode VALUE 'IW3D',
            c_tcode_iw41        TYPE sy-tcode VALUE 'IW41',
            c_tcode_iw42        TYPE sy-tcode VALUE 'IW42',
            c_vari_notif_open   TYPE char20   VALUE 'CP-MYW-001',
            c_vari_notif_comp   TYPE char20   VALUE 'CP-MYW-002',
            c_vari_inotif_open  TYPE char20   VALUE 'CP-MYW-005',
            c_vari_inotif_comp  TYPE char20   VALUE 'CP-MYW-006',
            c_vari_ordop_open   TYPE char20   VALUE 'CP-MYW-003',
            c_vari_ordop_comp   TYPE char20   VALUE 'CP-MYW-004',
            c_vari_new_notif    TYPE char20   VALUE 'CP-MW-001',
            c_vari_mwnotif_open TYPE char20   VALUE 'CP-MW-002',
            c_vari_mwnotif_comp TYPE char20   VALUE 'CP-MW-003',
            c_vari_order_open   TYPE char20   VALUE 'CP-MW-004',
            c_vari_order_comp   TYPE char20   VALUE 'CP-MW-005',
            c_vari_ntask_open   TYPE char20   VALUE 'CP-MW-006',
            c_vari_ntask_comp   TYPE char20   VALUE 'CP-MW-007',
            c_vari_led_order    TYPE char20   VALUE 'CP-BT-001',
            c_vari_led_notif    TYPE char20   VALUE 'CP-BT-002',
            c_vari_led_floc     TYPE char20   VALUE 'CP-BT-003',
            c_vari_ord_op       TYPE char20   VALUE 'CP-BT-004',
            c_doc_mywork        TYPE char20   VALUE 'MY_WORK',
            c_doc_defaults      TYPE char20   VALUE 'DEFAULTS',
            c_doc_manage_work   TYPE char20   VALUE 'MANAGE_WORK',
            c_doc_base_transc   TYPE char20   VALUE 'BASE_TRANSC',
            c_func_open         TYPE zzfunction VALUE 'OPEN',
            c_func_comp         TYPE zzfunction VALUE 'COMP',
            c_func_all          TYPE zzfunction VALUE 'ALL' ,"jsharma
            c_func_ordr         TYPE zzfunction VALUE 'ORDR',
            c_func_floc         TYPE zzfunction VALUE 'FLOC',
            c_func_notf         TYPE zzfunction VALUE 'NOTF',
            c_mode_error        TYPE char1      VALUE 'E',
            c_sync              TYPE char1      VALUE 'S',
            c_notif_type_ng     TYPE qmart      VALUE 'M1',
            c_pl_plant_9000     TYPE iwerk      VALUE '9000',
            c_authority_ok      TYPE sy-subrc   VALUE 1,
            c_1                 TYPE c          VALUE '1',
            c_2                 TYPE c          VALUE '2',
            c_n                 TYPE c          VALUE 'M',
            c_hyphen            TYPE c          VALUE '-',
            c_comma             TYPE c          VALUE ',',
            c_k4(2)             TYPE c          VALUE 'K4',
            c_u2(2)             TYPE c          VALUE 'U2'.

DATA : ok_code           TYPE sy-ucomm,
       w_x_open          TYPE char1,
       w_x_complete      TYPE char1,
       w_x_all           TYPE char1,"jsharma
       w_x_order         TYPE char1,
       w_x_notif         TYPE char1,
       w_x_func_loc      TYPE char1,
       w_x_mgwr_open     TYPE char1,
       w_x_mgwr_complete TYPE char1,
       w_x_mgwr_all TYPE char1, "jsharma
       w_wctr_fr         TYPE gewrk,
       w_wctr_to         TYPE gewrk,
       w_cursor_fld      TYPE dynfnam.

*-- Work Area declaration
DATA : x_defaults    TYPE zzntdefaults,
       x_nt_defaults TYPE zzntdefaults,
       x_tab_field   TYPE rstabfield.

*-- Internal Tables
DATA : t_selparams TYPE STANDARD TABLE OF rsparams  WITH HEADER LINE,
       t_bdcdata   TYPE STANDARD TABLE OF bdcdata   WITH HEADER LINE,
       t_config    TYPE STANDARD TABLE OF zntconfig WITH HEADER LINE,
       r_arbpl     TYPE RANGE OF gewrk,
*{   INSERT         D30K924358                                        1
       r_date      TYPE RANGE OF erdat,
*}   INSERT
       r_strno     TYPE RANGE OF  diqmel-shn_funct_loc ."ilom_strno.

TYPE-POOLS: slis.
TYPES:BEGIN OF ty_heading,
        tdline TYPE tline-tdline,
      END OF ty_heading.

DATA : s_fieldtab TYPE slis_fieldcat_alv,
       i_fieldtab TYPE slis_t_fieldcat_alv,
       i_layout   TYPE slis_layout_alv,
       t_heading TYPE TABLE OF ty_heading,
       x_heading TYPE ty_heading,
       t_footer TYPE TABLE OF ty_heading,
       x_footer TYPE ty_heading.
DATA: gt_list_top_of_page TYPE slis_t_listheader,
      ls_line             TYPE slis_listheader,
      top_of_page TYPE slis_formname VALUE 'HEADING_TOP_OF_PAGE',
      end_of_page TYPE slis_formname VALUE 'FOOTER_OF_PAGE',
      top_of_page_def TYPE slis_formname VALUE 'DEF_TOP_OF_PAGE',
      gt_events TYPE slis_t_event,
      x_excld TYPE slis_extab,
      t_excld TYPE slis_t_extab.

DATA:  l_titl1(30) TYPE c,
       l_titl2(30) TYPE c,
       l_titl3(30) TYPE c,
       l_titl4(30) TYPE c,
       l_titl5(30) TYPE c,
       l_titl6(30) TYPE c.

DATA  f11 type string.
SELECTION-SCREEN : BEGIN OF SCREEN 9100 AS SUBSCREEN.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-001 FOR FIELD s_arbpl.
*{   REPLACE        D30K925363                                        2
*\SELECT-OPTIONS : s_arbpl FOR rihqmel_list-arbpl.
SELECT-OPTIONS : s_arbpl FOR rihqmel_list-arbpl.
*}   REPLACE
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-002 FOR FIELD s_strno.
SELECT-OPTIONS : s_strno FOR diqmel-shn_funct_loc MATCHCODE OBJECT iflm. " change by jyoti
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(16) text-011 FOR FIELD s_date.
SELECT-OPTIONS : s_date FOR rihqmel_list-erdat.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN : END OF SCREEN 9100.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_strno-low.
*
*  PERFORM sub_handle_f4_on_strno.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_strno-high.
*
*  PERFORM sub_handle_f4_on_strno.
