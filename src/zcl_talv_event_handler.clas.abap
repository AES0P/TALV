CLASS zcl_talv_event_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_talv_event_handle_imp .

    ALIASES on_handle_changed_finished
      FOR zif_talv_event_handle_imp~on_handle_changed_finished .
    ALIASES on_handle_countdown_finished
      FOR zif_talv_event_handle_imp~on_handle_countdown_finished .
    ALIASES on_handle_data_changed
      FOR zif_talv_event_handle_imp~on_handle_data_changed .
    ALIASES on_handle_double_click
      FOR zif_talv_event_handle_imp~on_handle_double_click .
    ALIASES on_handle_grid_dispatch
      FOR zif_talv_event_handle_imp~on_handle_grid_dispatch .
    ALIASES on_handle_hotspot_click
      FOR zif_talv_event_handle_imp~on_handle_hotspot_click .
    ALIASES on_handle_line_button_click
      FOR zif_talv_event_handle_imp~on_handle_line_button_click .
    ALIASES on_handle_toolbar
      FOR zif_talv_event_handle_imp~on_handle_toolbar .
    ALIASES on_handle_top_of_page
      FOR zif_talv_event_handle_imp~on_handle_top_of_page .
    ALIASES on_handle_user_command
      FOR zif_talv_event_handle_imp~on_handle_user_command .
    ALIASES on_pai_command
      FOR zif_talv_event_handle_imp~on_pai_command .
    ALIASES on_retrieve
      FOR zif_talv_event_handle_imp~on_retrieve .
    ALIASES on_set_pf_status
      FOR zif_talv_event_handle_imp~on_set_pf_status .
    ALIASES on_set_title
      FOR zif_talv_event_handle_imp~on_set_title .

    METHODS constructor
      IMPORTING
        !talv TYPE REF TO zcl_talv_parent .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA talv TYPE REF TO zcl_talv_parent .
ENDCLASS.



CLASS ZCL_TALV_EVENT_HANDLER IMPLEMENTATION.


  METHOD constructor.

    me->talv = talv.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_exit.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    CHECK talv->grid->outtab IS BOUND.

    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_exit)
 IN PROGRAM (talv->key-program)
      USING talv
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_changed_finished.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    DATA: index_rows TYPE lvc_t_row,
          row_no     TYPE lvc_t_roid.

    talv->grid->get_selected_rows(
            IMPORTING
              et_index_rows = index_rows
              et_row_no     = row_no ).

    DATA columns TYPE lvc_t_col.
    talv->grid->get_selected_columns(
            IMPORTING
              et_index_columns = columns ).

    PERFORM (talv->key-handle_data_changed_finished)
 IN PROGRAM (talv->key-program)
      USING talv e_modified et_good_cells
   CHANGING <table>
   IF FOUND.

    talv->grid->set_selected_rows( it_index_rows = index_rows
                                   it_row_no     = row_no ).

    talv->grid->set_selected_columns( it_col_table = columns ).

    CHECK talv->key-style_table_name IS NOT INITIAL.
    FIELD-SYMBOLS <table_line> TYPE any.
    LOOP AT <table> ASSIGNING <table_line>.
      talv->set_btn_style_for_single_line( CHANGING alv_line = <table_line> ).
    ENDLOOP.
    talv->refresh( ).

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_countdown_finished.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_countdown_finished)
 IN PROGRAM (talv->key-program)
      USING talv
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_data_changed.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_data_changed)
 IN PROGRAM (talv->key-program)
      USING talv er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm
   CHANGING <table>
   IF FOUND.

    talv->key-layout-cwidth_opt = abap_true.
    talv->set_layout( talv->key-layout ).

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_double_click.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_double_click)
 IN PROGRAM (talv->key-program)
      USING talv e_row e_column
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_grid_dispatch.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_grid_dispatch)
 IN PROGRAM (talv->key-program)
      USING talv action
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_hotspot_click.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_hotspot_click)
 IN PROGRAM (talv->key-program)
      USING talv e_row_id e_column_id es_row_no
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_line_button_click.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    IF es_col_id-fieldname CS '_TBTN'.

      FIELD-SYMBOLS <alv_line> TYPE any.
      FIELD-SYMBOLS <value> TYPE any.

      READ TABLE <table> ASSIGNING <alv_line> INDEX es_row_no-row_id.
      CHECK <alv_line> IS ASSIGNED.

      DATA fieldname TYPE fieldname.
      fieldname = es_col_id-fieldname.

      REPLACE '_TBTN' IN fieldname WITH space.
      ASSIGN COMPONENT fieldname OF STRUCTURE <alv_line> TO <value>.

      talv->handle_long_text_field(
        EXPORTING
          fieldname = fieldname
        CHANGING
          value     = <value> ).

      talv->refresh( ).

    ELSE.
      PERFORM (talv->key-handle_line_btn_click)
   IN PROGRAM (talv->key-program)
        USING talv es_col_id es_row_no
     CHANGING <table>
     IF FOUND.
    ENDIF.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_toolbar.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_toolbar)
 IN PROGRAM (talv->key-program)
      USING talv e_object e_interactive
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_top_of_page.

    CHECK talv->key-header_height > 0.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_top_of_page)
 IN PROGRAM (talv->key-program)
      USING talv e_dyndoc_id table_index
   CHANGING <table>
   IF FOUND.

    talv->header_document->display_document(
       EXPORTING
         reuse_control      = abap_true
         parent             = talv->header_container
       EXCEPTIONS
         html_display_error = 1 ).
    IF sy-subrc NE 0.
      MESSAGE 'Error in displaying top-of-page' TYPE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_handle_user_command.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_user_command)
 IN PROGRAM (talv->key-program)
      USING talv e_ucomm
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_pai_command.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_pai_command)
 IN PROGRAM (talv->key-program)
      USING talv e_ucomm
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_pbo.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_on_pbo)
 IN PROGRAM (talv->key-program)
      USING talv
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_retrieve.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN talv->grid->outtab->* TO <table>.

    PERFORM (talv->key-handle_retrieve)
 IN PROGRAM (talv->key-program)
      USING talv->key-ddic_type CHANGING <table>
   IF FOUND.

    CHECK talv->is_initialized( ) = abap_false.

    IF talv->key-style_table_name IS NOT INITIAL.
      talv->init_style( ).
    ENDIF.

    IF talv->key-color_table_name IS NOT INITIAL.
      talv->init_color( 2 ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_set_pf_status.
    PERFORM (talv->key-handle_set_pf_status)
 IN PROGRAM (talv->key-program)
   IF FOUND.
  ENDMETHOD.


  METHOD zif_talv_event_handle_imp~on_set_title.
    PERFORM (talv->key-handle_set_title)
 IN PROGRAM (talv->key-program)
   IF FOUND.
  ENDMETHOD.
ENDCLASS.
