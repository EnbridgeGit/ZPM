"Name: \PR:SAPLCOBC\FO:RESBD_INFO\SE:BEGIN\EI
ENHANCEMENT 0 ZPM_DEFAULT_STORAGE_LOCATION_S.
*
* Default Storage Location to A001 - Requirement part of Enhancement
* EAM-PM-E-106-E- Default Storage Location
  if ( cs_resbd-postp EQ 'N' OR
       cs_resbd-postp EQ 'L' OR
       cs_resbd-postp EQ 'T' ) AND
     cs_resbd-lgort IS INITIAL.
    cs_resbd-lgort = 'A001'.
  endif.

ENDENHANCEMENT.
