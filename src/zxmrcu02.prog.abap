*&---------------------------------------------------------------------*
*&  Include           ZXMRCU02
*This routine is used to call the Zt-code for Measuring Position on
*IK01/IK02/IK03
*Object : EAM-PM-E-076-N_Load_Inspection_Points
*Author : Jyoti Sharma
*Date   : 17-06-2014
*&---------------------------------------------------------------------*
IF sy-tcode = 'IK01' OR
   sy-tcode = 'IK02' OR
   sy-tcode = 'IK03'.
  CALL TRANSACTION 'ZPMMEAPOSVIEW'.
ENDIF.
