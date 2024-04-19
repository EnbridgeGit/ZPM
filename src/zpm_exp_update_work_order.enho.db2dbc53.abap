"Name: \PR:SAPLCOBT\EX:EHP605_CO_BT_ORDER_POST_02\EI
ENHANCEMENT 0 ZPM_EXP_UPDATE_WORK_ORDER.
IF sy-tcode = 'IW31' OR sy-tcode = 'IW32' OR sy-tcode = 'IP10'.
*This Enhancement is linked to Object EAM-PM-E-075-N_Update_operation_work_center
  Include ZUPDATE_OP_WRK_CNTR.
endif.
IF sy-tcode = 'IW31' OR sy-tcode = 'IW32' .
**This Enhancement is linked to Object EAM-PM-E-76 Load Inspection Point
 Include ZUPDATE_PRT_LINES.
endif.

 if sy-tcode = 'IP10' or sy-tcode = 'IP30'.
  Include ZUPDATE_PRT_LINES_MP." PRT lines for Maintainence Plan.
 endif.

ENDENHANCEMENT.
