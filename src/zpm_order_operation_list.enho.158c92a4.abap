"Name: \PR:RI_ORDER_OPERATION_LIST\EX:EHP605_RI_ORDER_OPERATION_L_09\EI
ENHANCEMENT 0 ZPM_ORDER_OPERATION_LIST.
*-- Proceed with filtering only when there are values
  IF ZSCODATE[] is not INITIAL.
*-- Filter data only when the field is selected as part of output ALV list
    READ TABLE g_fieldcat_tab TRANSPORTING NO FIELDS with KEY fieldname = 'ZZPMCOMPDATE' no_out = space.
    IF sy-subrc eq 0.
*-- Filter / Delete based on the conditions provided in selection screen
      DELETE object_tab where zzpmcompdate not in ZSCODATE.
    ENDIF.
  ENDIF.

*-- Proceed with filtering only when there are values.
  IF zsbword[] is not INITIAL.
*-- Filter data only when the field is selected as part of output ALV list
    READ TABLE g_fieldcat_tab TRANSPORTING NO FIELDS with KEY fieldname = 'ZZPMBWORKORD' no_out = space.
    IF sy-subrc eq 0.
*-- Filter / Delete based on the conditions provided in selection screen
      DELETE object_tab where ZZPMBWORKORD not in zsbword.
    ENDIF.
  ENDIF.

*-- Proceed with filtering only when there are values.
  IF zsbwtyp[] is not INITIAL.
*-- Filter data only when the field is selected as part of output ALV list
    READ TABLE g_fieldcat_tab TRANSPORTING NO FIELDS with KEY fieldname = 'ZZPMBWORKTYPE' no_out = space.
    IF sy-subrc eq 0.
*-- Filter / Delete based on the conditions provided in selection screen
      DELETE object_tab where ZZPMBWORKTYPE not in zsbwtyp.
    ENDIF.
  ENDIF.

*-- Proceed with filtering only when there are values.
  IF zsbsdate[] is not INITIAL.
*-- Filter data only when the field is selected as part of output ALV list
    READ TABLE g_fieldcat_tab TRANSPORTING NO FIELDS with KEY fieldname = 'ZZPMBSCHEDDATE' no_out = space.
    IF sy-subrc eq 0.
*-- Filter / Delete based on the conditions provided in selection screen
      DELETE object_tab where ZZPMBSCHEDDATE not in zsbsdate.
    ENDIF.
  ENDIF.

*-- Proceed with filtering only when there are values.
  IF zsbstatu[] is not INITIAL.
*-- Filter data only when the field is selected as part of output ALV list
    READ TABLE g_fieldcat_tab TRANSPORTING NO FIELDS with KEY fieldname = 'ZZPMBSTATUS' no_out = space.
    IF sy-subrc eq 0.
*-- Filter / Delete based on the conditions provided in selection screen
      DELETE object_tab where ZZPMBSTATUS not in zsbstatu.
    ENDIF.
  ENDIF.

*-- Ensure that objcet_tab and lt_objects are in sync.
  lt_objects[] = object_tab[].

ENDENHANCEMENT.
