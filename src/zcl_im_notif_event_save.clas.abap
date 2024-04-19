class ZCL_IM_NOTIF_EVENT_SAVE definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_NOTIF_EVENT_SAVE
*"* do not include other source files here!!!

  interfaces IF_EX_NOTIF_EVENT_SAVE .
protected section.
*"* protected components of class ZCL_IM_NOTIF_EVENT_SAVE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_NOTIF_EVENT_SAVE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_IM_NOTIF_EVENT_SAVE IMPLEMENTATION.


METHOD if_ex_notif_event_save~change_data_at_save.
*IF CS_VIQMEL-AUSZT is initial.
"Execute only via work manager / BAPI update
  IF sy-tcode IS INITIAL.
    IF ( cs_viqmel-msaus = 'X' ) AND
       ( cs_viqmel-ausvn <> 0 )  AND
       ( cs_viqmel-ausbs >= cs_viqmel-ausvn ).
*       Only if breakdown indicator is set, valid start date, end date not less than start date
      cs_viqmel-auszt = ( cs_viqmel-ausbs - cs_viqmel-ausvn ) * 24 * 60 * 60.
      cs_viqmel-auszt = cs_viqmel-auszt + ( cs_viqmel-auztb - cs_viqmel-auztv ).
    ENDIF.
*  ENDIF.
  ENDIF.
ENDMETHOD.
ENDCLASS.
