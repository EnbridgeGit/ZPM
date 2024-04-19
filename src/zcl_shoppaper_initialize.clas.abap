class ZCL_SHOPPAPER_INITIALIZE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_SHOPPAPER_INITIALIZE
*"* do not include other source files here!!!

  class-methods GET_ZONE
    importing
      !IM_TPLNR type TPLNR
    exporting
      !EX_ZONET type PLTXT .
protected section.
*"* protected components of class ZCL_SHOPPAPER_INITIALIZE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_SHOPPAPER_INITIALIZE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_SHOPPAPER_INITIALIZE IMPLEMENTATION.


METHOD get_zone.

  DATA: lv_zone TYPE tplnr,
        lv_value1 TYPE tplnr,
        lv_value2 TYPE tplnr,
        lv_dummy  TYPE c.

  SPLIT im_tplnr at '-' INTO lv_value1 lv_value2 lv_dummy.
  CONCATENATE lv_value1 lv_value2 INTO lv_zone SEPARATED BY '-'.

  SELECT SINGLE pltxt FROM iflotx
         INTO ex_zonet
         WHERE tplnr = lv_zone AND
               spras = sy-langu.

ENDMETHOD.
ENDCLASS.
