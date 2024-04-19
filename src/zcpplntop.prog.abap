*&---------------------------------------------------------------------*
*& Include ZCPOETOP                                          Module Pool      ZCPOE
*&
*&---------------------------------------------------------------------*

PROGRAM  zcppln.

TABLES : zntlogo.

DATA: ok_code LIKE sy-ucomm.

DATA: BEGIN OF or_selection,
      outage TYPE c,
      revision TYPE c,
      END OF or_selection.

DATA: BEGIN OF week,
      t0 TYPE c,
      t1 TYPE c,
      t2 TYPE c,
      t3 TYPE c,
      t4 TYPE c,
      t5 TYPE c,
      t6 TYPE c,
      t7 TYPE c,
      t8 TYPE c,
      t9 TYPE c,
      t10 TYPE c,
      t11 TYPE c,
      t12 TYPE c,
      t13 TYPE c,
      t14 TYPE c,
      t0_12 TYPE c,
      t13_26 TYPE c,
      t27_52 TYPE c,
      t_from_to TYPE c,
      t_all TYPE c,
      END OF week.

DATA: BEGIN OF week_range,
      from(2) TYPE c,
      to(2)   TYPE c,
      END OF   week_range.

DATA: gv_from TYPE z_week,
      gv_to   TYPE z_week.
