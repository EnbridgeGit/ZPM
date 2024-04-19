*&---------------------------------------------------------------------*
*&      Form  SUB_BDC_DYNPRO
*&---------------------------------------------------------------------*
*       Populate Batch Data screen information
*----------------------------------------------------------------------*
*      -->P_PROG    Program
*      -->P_DYNNR   Screen Number
*----------------------------------------------------------------------*
FORM sub_bdc_dynpro USING p_prog p_dynnr.
  CLEAR t_bdcdata.
  t_bdcdata-program  = p_prog.
  t_bdcdata-dynpro   = p_dynnr.
  t_bdcdata-dynbegin = 'X'.
  APPEND t_bdcdata.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SUB_BDC_FIELD
*&---------------------------------------------------------------------*
*       Populate batch data field information
*----------------------------------------------------------------------*
*      -->P_FNAM   Field Name
*      -->P_FVAL   Field Value
*----------------------------------------------------------------------*
FORM sub_bdc_field  USING p_fnam p_fval.
  CLEAR t_bdcdata.
  t_bdcdata-fnam = p_fnam.
  t_bdcdata-fval = p_fval.
  APPEND t_bdcdata.
ENDFORM.                    " SUB_BDC_FIELD
