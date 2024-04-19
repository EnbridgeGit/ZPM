"Name: \PR:SAPLCOMK\EX:COMPLETE_RESB_16\EI
ENHANCEMENT 0 ZPM_DEFAULT_STORAGE_LOCATION.
*
* Default Storage Location to A001 - Requirement part of Enhancement
* EAM-PM-E-106-E- Default Storage Location
  if ( resbdi-postp EQ 'N' OR
     resbdi-postp EQ 'L' OR
     resbdi-postp EQ 'T' ) AND
     resbdi-lgort IS INITIAL.
    resbdi-lgort = 'A001'.
  endif.

ENDENHANCEMENT.
