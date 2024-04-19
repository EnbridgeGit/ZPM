*&---------------------------------------------------------------------*
*& Include ZCPUSERTOP                                        Module Pool      ZCPUSER
*&
*&---------------------------------------------------------------------*

PROGRAM  zcpuser.

DATA: ok_code TYPE sy-ucomm,
      save_code TYPE sy-ucomm.

TYPES: BEGIN OF ty_buttons,
       b10(20) TYPE c,
       b11(20) TYPE c,
       b12(20) TYPE c,
       b13(20) TYPE c,
       b14(20) TYPE c,
       b15(20) TYPE c,
       b16(20) TYPE c,
       b17(20) TYPE c,
       b18(20) TYPE c,
       b19(20) TYPE c,
       b20(20) TYPE c,
       b21(20) TYPE c,
       b22(20) TYPE c,
       b23(20) TYPE c,
       b24(20) TYPE c,
       b25(20) TYPE c,
       b26(20) TYPE c,
       b27(20) TYPE c,
       b28(20) TYPE c,
       b29(20) TYPE c,
       b30(20) TYPE c,
       b31(20) TYPE c,
       b32(20) TYPE c,
       b33(20) TYPE c,
       END OF ty_buttons.

TYPES: BEGIN OF ty_desc,
       b10(30) TYPE c,
       b11(30) TYPE c,
       b12(30) TYPE c,
       b13(30) TYPE c,
       b14(30) TYPE c,
       b15(30) TYPE c,
       b16(30) TYPE c,
       b17(30) TYPE c,
       b18(30) TYPE c,
       b19(30) TYPE c,
       b20(30) TYPE c,
       b21(30) TYPE c,
       b22(30) TYPE c,
       b23(30) TYPE c,
       b24(30) TYPE c,
       b25(30) TYPE c,
       b26(30) TYPE c,
       b27(30) TYPE c,
       b28(30) TYPE c,
       b29(30) TYPE c,
       b30(30) TYPE c,
       b31(30) TYPE c,
       b32(30) TYPE c,
       b33(30) TYPE c,
       END OF ty_desc.


DATA: gs_buttons  TYPE ty_buttons,
      gs_desc     TYPE ty_desc.

DATA: gv_modified(1) TYPE c,
      gv_uname LIKE sy-uname,
      gv_call  LIKE sy-uname,
      gv_copy(1)  TYPE c,
      gv_line     TYPE i,
      gv_copy_but TYPE c, "Copy button
      gv_check(1) TYPE c.

DATA: gt_user_button TYPE TABLE OF zcpbuttons WITH HEADER LINE,
      gt_buttons TYPE TABLE OF zcpbuttons WITH HEADER LINE.
*      gs_user_button TYPE zcpbuttons.

CONTROLS: tc_buttons TYPE TABLEVIEW USING SCREEN 9001.

DATA: lw_tstc TYPE tstc.

TABLES: zntlogo.
