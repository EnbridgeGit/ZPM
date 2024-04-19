*&---------------------------------------------------------------------*
*&  Include           Z_X_BI_MONITOR_VIEW
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_view DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_view DEFINITION.

  PUBLIC SECTION.

    DATA:
    gr_events        TYPE REF TO cl_salv_events_table,
    gr_selections    TYPE REF TO cl_salv_selections.

    METHODS:

    set_column_text IMPORTING im_column TYPE lvc_fname
                              im_ltext  TYPE scrtext_l
                              im_mtext  TYPE scrtext_m
                              im_stext  TYPE scrtext_s,
    set_data CHANGING ch_data TYPE ANY TABLE,
    send,
    set_sort_field  IMPORTING im_column TYPE lvc_fname.

  PRIVATE SECTION.

    DATA:
    gr_data            TYPE REF TO data,
    gr_dock_cont       TYPE REF TO cl_gui_docking_container,
    gr_alv_functions   TYPE REF TO cl_salv_functions_list,
    gr_columns         TYPE REF TO cl_salv_columns,
    gr_sorts           TYPE REF TO cl_salv_sorts,
    gr_layout_settings TYPE REF TO cl_salv_layout,
    gr_display         TYPE REF TO cl_salv_display_settings,
    gr_salv            TYPE REF TO cl_salv_table,

    gv_is_called     TYPE xflag.

ENDCLASS.                    "lcl_view DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_view IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_view IMPLEMENTATION.
  METHOD set_column_text.

    DATA: lr_column        TYPE REF TO cl_salv_column.

    IF gr_columns IS INITIAL.
      gr_columns = gr_salv->get_columns( ).
    ENDIF.

    lr_column = gr_columns->get_column( im_column ).
    lr_column->set_long_text( im_ltext ).
    lr_column->set_medium_text( im_mtext ).
    lr_column->set_short_text( im_stext ).

  ENDMETHOD.                    "seT_column_text
  METHOD send.

    IF gv_is_called IS INITIAL.
      CALL METHOD gr_salv->display.
      gv_is_called = abap_true.
    ELSE.
      CALL METHOD gr_salv->refresh.
    ENDIF.

  ENDMETHOD.                    "send
  METHOD set_data.

    FIELD-SYMBOLS: <f_table> TYPE ANY TABLE.
    DATA ls_layout_key      TYPE salv_s_layout_key.

    IF gr_salv IS INITIAL.

      CREATE DATA gr_data LIKE ch_data.
      ASSIGN gr_data->* TO <f_table>.

      CREATE OBJECT gr_dock_cont
        EXPORTING
          side                        = cl_gui_docking_container=>dock_at_left
          extension                   = 99999
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5
          OTHERS                      = 6.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      TRY.
        cl_salv_table=>factory(
            EXPORTING
              r_container    = gr_dock_cont
            IMPORTING
              r_salv_table   = gr_salv
            CHANGING
              t_table        = <f_table>
                 ).
      ENDTRY.

      gr_columns = gr_salv->get_columns( ).
      gr_display = gr_salv->get_display_settings( ).
      gr_alv_functions = gr_salv->get_functions( ).
      gr_selections = gr_salv->get_selections( ).
      gr_selections->set_selection_mode( if_salv_c_selection_mode=>single ).
      gr_events = gr_salv->get_event( ).
      gr_alv_functions->set_all( abap_true ).
      gr_layout_settings = gr_salv->get_layout( ).

      ls_layout_key-report = sy-repid.
      gr_layout_settings->set_key( ls_layout_key ).
      gr_layout_settings->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
      gr_layout_settings->set_default( if_salv_c_bool_sap=>true ).

      gr_columns->set_optimize( ).
      gr_display->set_striped_pattern( abap_true ).

      TRY.
          CALL METHOD gr_alv_functions->add_function
            EXPORTING
              name     = 'DISPLOG'
              icon     = 'ICON_PROTOCOL'
              text     = 'Display Log'
              tooltip  = 'Display Log'
              position = if_salv_c_function_position=>right_of_salv_functions.

          CALL METHOD gr_alv_functions->add_function
            EXPORTING
              name     = 'DISPERRORLOG'
              icon     = 'ICON_PROTOCOL'
              text     = 'Display Error Log'
              tooltip  = 'Display Error Log Only'
              position = if_salv_c_function_position=>right_of_salv_functions.

        CATCH cx_salv_existing .
        CATCH cx_salv_wrong_call .
      ENDTRY.
    ENDIF.

    IF <f_table> IS NOT ASSIGNED.
      ASSIGN gr_data->* TO <f_table>.
    ENDIF.

    <f_table> = ch_data.

  ENDMETHOD.                    "display_alv_out
  METHOD set_sort_field.

    IF gr_sorts IS INITIAL.
      gr_sorts = gr_salv->get_sorts( ).
    ENDIF.

    CALL METHOD gr_sorts->add_sort
      EXPORTING
        columnname = im_column
        subtotal   = if_salv_c_bool_sap=>false.

  ENDMETHOD.                    "set_sort_field
ENDCLASS.                    "lcl_view IMPLEMENTATION
