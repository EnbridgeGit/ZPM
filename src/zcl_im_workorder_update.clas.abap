class ZCL_IM_WORKORDER_UPDATE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_WORKORDER_UPDATE
*"* do not include other source files here!!!

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_WORKORDER_UPDATE .
protected section.
*"* protected components of class ZCL_IM_WORKORDER_UPDATE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_WORKORDER_UPDATE
*"* do not include other source files here!!!

  methods CHANGEFIELDERROR .
ENDCLASS.



CLASS ZCL_IM_WORKORDER_UPDATE IMPLEMENTATION.


METHOD changefielderror.

  DATA: ls_messages TYPE bapiret2,
      lt_messages TYPE TABLE OF bapiret2,
      lv_msgv     TYPE symsgv.

  DATA: lv_string TYPE string.

  CONCATENATE text-101 text-102 into lv_string SEPARATED BY ' '.

  CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
    EXPORTING
      TITEL              = 'Warning'
      textline1          = text-100
      TEXTLINE2          = lv_string
*     START_COLUMN       = 25
*     START_ROW          = 6
            .


*  CLEAR ls_messages.
*  ls_messages-type    = 'W'.
*  ls_messages-id      = 'ME'.
*  ls_messages-number  = '303'.
*  ls_messages-message_v1 = text-100.
*  ls_messages-message_v2 = text-101.
*  ls_messages-message_v3 = text-102.
*  APPEND ls_messages TO lt_messages.
*  CALL FUNCTION 'C14ALD_BAPIRET2_SHOW'
*    TABLES
*      i_bapiret2_tab = lt_messages.
ENDMETHOD.


method IF_EX_WORKORDER_UPDATE~ARCHIVE_OBJECTS.
endmethod.


method IF_EX_WORKORDER_UPDATE~AT_DELETION_FROM_DATABASE.
endmethod.


method IF_EX_WORKORDER_UPDATE~AT_RELEASE.
endmethod.


method IF_EX_WORKORDER_UPDATE~AT_SAVE.
endmethod.


METHOD if_ex_workorder_update~before_update.
*IT_HEADER  TYPE COBAI_T_HEADER OPTIONAL
*IT_HEADER_OLD  TYPE COBAI_T_HEADER_OLD OPTIONAL
*IT_ITEM  TYPE COBAI_T_ITEM OPTIONAL
*IT_ITEM_OLD  TYPE COBAI_T_ITEM_OLD OPTIONAL
*IT_SEQUENCE  TYPE COBAI_T_SEQUENCE OPTIONAL
*IT_SEQUENCE_OLD  TYPE COBAI_T_SEQUENCE_OLD OPTIONAL
*IT_OPERATION	TYPE COBAI_T_OPERATION OPTIONAL
*IT_OPERATION_OLD_AFVC  TYPE COBAI_T_OPERATION_OLD_AFVC OPTIONAL
*IT_OPERATION_OLD_AFVV  TYPE COBAI_T_OPERATION_OLD_AFVV OPTIONAL
*IT_OPERATION_OLD_AFVU  TYPE COBAI_T_OPERATION_OLD_AFVU OPTIONAL
*IT_COMPONENT	TYPE COBAI_T_COMPONENT OPTIONAL
*IT_COMPONENT_OLD	TYPE COBAI_T_COMPONENT_OLD OPTIONAL
*IT_RELATIONSHIP  TYPE COBAI_T_RELATIONSHIP OPTIONAL
*IT_RELATIONSHIP_OLD  TYPE COBAI_T_RELATIONSHIP_OLD OPTIONAL
*IT_PSTEXT  TYPE COBAI_T_PSTEXT OPTIONAL
*IT_PSTEXT_OLD  TYPE COBAI_T_PSTEXT_OLD OPTIONAL
*IT_MILESTONE	TYPE COBAI_T_MILESTONE OPTIONAL
*IT_MILESTONE_OLD	TYPE COBAI_T_MILESTONE_OLD OPTIONAL
*IT_PLANNED_ORDER	TYPE COBAI_T_PLANNED_ORDER OPTIONAL
*IT_STATUS  TYPE COBAI_T_STATUS OPTIONAL
*IT_STATUS_OLD  TYPE COBAI_T_STATUS_OLD OPTIONAL
*IT_OPR_RELATIONS	TYPE COBAI_T_OPR_RELATIONS OPTIONAL
*IT_OPR_RELATIONS_OLD	TYPE COBAI_T_OPR_RELATIONS_OLD OPTIONAL
*IT_DOCLINK	TYPE COBAI_T_DOCLINK OPTIONAL
*IT_DOCLINK_OLD	TYPE COBAI_T_DOCLINK_OLD OPTIONAL
*IT_PRT_ALLOCATION  TYPE COBAI_T_PRT_ALLOCATION OPTIONAL
*IT_PRT_ALLOCATION_OLD  TYPE COBAI_T_PRT_ALLOCATION_OLD OPTIONAL
*IT_PMPARTNER	TYPE COBAI_T_PMPARTNER OPTIONAL
*IT_PMPARTNER_OLD	TYPE COBAI_T_PMPARTNER_OLD OPTIONAL
*IT_PIINSTRUCTION	TYPE COBAI_T_PIINSTRUCTION OPTIONAL
*IT_PIINSTRUCTIONVALUE  TYPE COBAI_T_PIINSTRUCTIONVALUE OPTIONAL

  DATA: ls_header     LIKE LINE OF it_header,
        ls_header_old LIKE LINE OF it_header_old.

  DATA: ls_operation          LIKE LINE OF it_operation,
        ls_operation_old_afvc LIKE LINE OF it_operation_old_afvc,
        ls_operation_old_afvv LIKE LINE OF it_operation_old_afvv,
        ls_operation_old_afvu LIKE LINE OF it_operation_old_afvu,
        ls_component          LIKE LINE OF it_component,
        ls_component_old      LIKE LINE OF it_component_old,
        ls_resbd              TYPE resbd,
        lt_resbd              LIKE TABLE OF ls_resbd,
        ls_eban               TYPE eban.

  DATA: ls_status_old LIKE LINE OF it_status_old.

  DATA: ls_esuc TYPE esuc,
        lt_esuc TYPE TABLE OF esuc,
        ls_esuh TYPE esuh.

  DATA: lv_on TYPE answer.

**************************************
*** Get Header Data
**************************************
  READ TABLE it_header      INTO ls_header      INDEX 1.
  READ TABLE it_header_old  INTO ls_header_old  INDEX 1.

**************************************
*** Check if validation should occur
**************************************
  IF ls_header-sttxt(3) <> 'REL'.
    "Order is not released
    EXIT.
  ELSE.
    "Order is currently released, check the old status to see if it was previously released
    READ TABLE it_status_old INTO ls_status_old
      WITH KEY stat  = 'I0002'
               inact = ''
               objnr = ls_header-objnr
    .
    IF sy-subrc <> 0.
      "No previous I0002, newly released.
      EXIT.
    ENDIF.

  ENDIF.

* EAMagine Defect 230 - Begin of changes by Eldhose Mathew
  CALL FUNCTION 'RFC_IS_GUI_ON'
    EXPORTING
      login_check = 'X'
    IMPORTING
      on          = lv_on.

  IF lv_on EQ 'N'.
    RETURN.
  ENDIF.
* End of changes by Eldhose Mathew

**************************************
*** Validate Header
**************************************
  IF ls_header-kostl <> ls_header_old-kostl.
    CALL METHOD me->changefielderror.
    EXIT.
  ENDIF.

**************************************
*** Validate Operations
**************************************
  LOOP AT it_operation INTO ls_operation.
    "Get old operation
    READ TABLE it_operation_old_afvc INTO ls_operation_old_afvc
      WITH KEY  mandt = ls_operation-mandt
                aufpl = ls_operation-aufpl
                aplzl = ls_operation-aplzl.

    IF sy-subrc <> 0.
      "New item skip
      CONTINUE.
    ENDIF.

    READ TABLE it_operation_old_afvv INTO ls_operation_old_afvv
  WITH KEY  mandt = ls_operation-mandt
            aufpl = ls_operation-aufpl
            aplzl = ls_operation-aplzl.

    IF sy-subrc <> 0.
      "New item skip
      CONTINUE.
    ENDIF.

    READ TABLE it_operation_old_afvu INTO ls_operation_old_afvu
  WITH KEY  mandt = ls_operation-mandt
            aufpl = ls_operation-aufpl
            aplzl = ls_operation-aplzl.

    IF sy-subrc <> 0.
      "New item skip
      CONTINUE.
    ENDIF.


    IF ls_operation-steus <> 'PM02'.
      "Not PM02 line....
      CONTINUE.
    ENDIF.
**********
****AFVC
**********
    IF ls_operation-preis <> ls_operation_old_afvc-preis.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-ltxa1 <> ls_operation_old_afvc-ltxa1.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-sakto <> ls_operation_old_afvc-sakto.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-werks <> ls_operation_old_afvc-werks.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-lifnr <> ls_operation_old_afvc-lifnr.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-matkl <> ls_operation_old_afvc-matkl.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-ekgrp <> ls_operation_old_afvc-ekgrp.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-afnam <> ls_operation_old_afvc-afnam.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

**********
****AFVV
**********
    IF ls_operation-fsavd <> ls_operation_old_afvv-fsavd.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

**********
****AFVU
**********
    IF ls_operation-usr00 <> ls_operation_old_afvu-usr00.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-usr01 <> ls_operation_old_afvu-usr01.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-usr02 <> ls_operation_old_afvu-usr02.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_operation-usr03 <> ls_operation_old_afvu-usr03.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    "Get Limits
    CALL FUNCTION 'MS_READ_LIMITS'
      EXPORTING
        packno          = ls_operation-packno
      TABLES
        limit_tab       = lt_esuc
      EXCEPTIONS
        no_limits_found = 1
        OTHERS          = 2.

    "Check Limits
    LOOP AT lt_esuc INTO ls_esuc.
      SELECT SINGLE * FROM esuh
        INTO ls_esuh
        WHERE packno = ls_esuc-packno
      .

      IF ls_esuc-limit <> ls_esuh-sumlimit.
        CALL METHOD me->changefielderror.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDLOOP. "Operation





**************************************
*** Validate Components
**************************************
  LOOP AT it_component INTO ls_component.
    "Get old component.
    READ TABLE it_component_old INTO ls_component_old
      WITH KEY  mandt = ls_component-mandt
                rsnum = ls_component-rsnum
                rspos = ls_component-rspos.

    IF sy-subrc <> 0.
      "New item skip
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM eban
      INTO ls_eban
      WHERE banfn = ls_component-banfnr
        AND bnfpo = ls_component-banfpo
    .

    IF sy-subrc <> 0.
      "New item skip
      CONTINUE.
    ENDIF.

*    CALL FUNCTION 'PM_ORDER_DATA_READ'
*      EXPORTING
*        order_number    = ls_component-aufnr
*      TABLES
*        iresbd          = lt_resbd
*      EXCEPTIONS
*        order_not_found = 1
*        OTHERS          = 2.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE lt_resbd INTO ls_resbd
*      WITH KEY rsnum = ls_component-rsnum
*               rspos = ls_component-rspos
*               rsart = ls_component-rsart.
*
*    IF sy-subrc <> 0.
*      "New item skip
*      CONTINUE.
*    ENDIF.


    IF ls_component-postp <> 'N'.
      "Not a Purchased item
      CONTINUE.
    ENDIF.

***********************
****Supplied Component
***********************
    IF ls_component-gpreis <> ls_component_old-gpreis.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-bdmng <> ls_component_old-bdmng.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-meins <> ls_component_old-meins.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-werks <> ls_component_old-werks.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-lgort <> ls_component_old-lgort.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-bdter <> ls_component_old-bdter.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-bdztp <> ls_component_old-bdztp.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-ekgrp <> ls_component_old-ekgrp.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-saknr <> ls_component_old-saknr.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.


    IF ls_component-lifnr <> ls_component_old-lifnr.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

***************************
****Read from Database PR
***************************
    IF ls_component-ktpnr <> ls_eban-ktpnr.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-konnr <> ls_eban-konnr.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

    IF ls_component-afnam <> ls_eban-afnam.
      CALL METHOD me->changefielderror.
      EXIT.
    ENDIF.

***********************
****Read from screen
***********************
*    IF ls_resbd-matxt <> ls_eban-txz01.
*      CALL METHOD me->changefielderror.
*      EXIT.
*    ENDIF.




  ENDLOOP. "Component
ENDMETHOD.


method IF_EX_WORKORDER_UPDATE~CMTS_CHECK.
endmethod.


method IF_EX_WORKORDER_UPDATE~INITIALIZE.
endmethod.


method IF_EX_WORKORDER_UPDATE~IN_UPDATE.
endmethod.


method IF_EX_WORKORDER_UPDATE~NUMBER_SWITCH.
endmethod.


method IF_EX_WORKORDER_UPDATE~REORG_STATUS_ACTIVATE.
endmethod.


method IF_EX_WORKORDER_UPDATE~REORG_STATUS_ACT_CHECK.
endmethod.


method IF_EX_WORKORDER_UPDATE~REORG_STATUS_REVOKE.
endmethod.
ENDCLASS.
