INTERFACE zif_talv_event_handle_imp
  PUBLIC .


  METHODS on_handle_toolbar
    FOR EVENT toolbar OF zcl_gui_alv_grid
    IMPORTING
      !e_object
      !e_interactive .
  METHODS on_handle_user_command
    FOR EVENT user_command OF zcl_gui_alv_grid
    IMPORTING
      !e_ucomm .
  METHODS on_handle_hotspot_click
    FOR EVENT hotspot_click OF zcl_gui_alv_grid
    IMPORTING
      !e_row_id
      !e_column_id
      !es_row_no .
  METHODS on_handle_double_click
    FOR EVENT double_click OF zcl_gui_alv_grid
    IMPORTING
      !e_row
      !e_column .
  METHODS on_handle_data_changed
    FOR EVENT data_changed OF zcl_gui_alv_grid
    IMPORTING
      !er_data_changed
      !e_onf4
      !e_onf4_before
      !e_onf4_after
      !e_ucomm .
  METHODS on_handle_changed_finished
    FOR EVENT data_changed_finished OF zcl_gui_alv_grid
    IMPORTING
      !e_modified
      !et_good_cells .
  METHODS on_handle_countdown_finished
      FOR EVENT finished OF cl_gui_timer .
  METHODS on_handle_top_of_page
    FOR EVENT top_of_page OF zcl_gui_alv_grid
    IMPORTING
      !e_dyndoc_id
      !table_index .
  METHODS on_set_pf_status
      FOR EVENT set_pf_status OF zcl_talv_parent .
  METHODS on_set_title
      FOR EVENT set_title OF zcl_talv_parent .
  METHODS on_pbo
      FOR EVENT on_pbo OF zcl_talv_parent .
  METHODS on_pai_command
    FOR EVENT pai_command OF zcl_talv_parent
    IMPORTING
      !e_ucomm .
  METHODS on_exit
      FOR EVENT on_exit OF zcl_talv_parent .
  METHODS on_retrieve
      FOR EVENT retrieve OF zcl_talv_parent .
  METHODS on_handle_grid_dispatch
    FOR EVENT grid_dispatch OF zcl_gui_alv_grid
    IMPORTING
      !action .
  METHODS on_handle_line_button_click
    FOR EVENT button_click OF zcl_gui_alv_grid
    IMPORTING
      !es_col_id
      !es_row_no .
ENDINTERFACE.
