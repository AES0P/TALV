CLASS zcl_gui_alv_grid DEFINITION
  PUBLIC
  INHERITING FROM cl_gui_alv_grid
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_talv_parent .

  PUBLIC SECTION.

    EVENTS grid_dispatch
      EXPORTING
        VALUE(action) TYPE sy-ucomm .

    METHODS constructor
      IMPORTING
        VALUE(talv)          TYPE REF TO zcl_talv_parent
        VALUE(i_shellstyle)  TYPE i DEFAULT 0
        VALUE(i_lifetime)    TYPE i OPTIONAL
        VALUE(i_parent)      TYPE REF TO cl_gui_container
        VALUE(i_appl_events) TYPE char01 DEFAULT space
        !i_parentdbg         TYPE REF TO cl_gui_container OPTIONAL
        !i_applogparent      TYPE REF TO cl_gui_container OPTIONAL
        !i_graphicsparent    TYPE REF TO cl_gui_container OPTIONAL
        VALUE(i_name)        TYPE string OPTIONAL
        !i_fcat_complete     TYPE sap_bool DEFAULT space
      EXCEPTIONS
        error_cntl_create
        error_cntl_init
        error_cntl_link
        error_dp_create .
    METHODS ready_for_it
      IMPORTING
        VALUE(i_buffer_active)      TYPE any OPTIONAL
        VALUE(i_bypassing_buffer)   TYPE char01 OPTIONAL
        VALUE(i_consistency_check)  TYPE char1 OPTIONAL
        VALUE(i_structure_name)     TYPE dd02l-tabname OPTIONAL
        VALUE(i_save)               TYPE char01 DEFAULT 'A'
        VALUE(i_default)            TYPE char01 DEFAULT 'X'
        VALUE(is_print)             TYPE lvc_s_prnt OPTIONAL
        VALUE(it_special_groups)    TYPE lvc_t_sgrp OPTIONAL
        VALUE(it_toolbar_excluding) TYPE ui_functions OPTIONAL
        VALUE(it_hyperlink)         TYPE lvc_t_hype OPTIONAL
        VALUE(it_alv_graphics)      TYPE dtc_t_tc OPTIONAL
        VALUE(it_except_qinfo)      TYPE lvc_t_qinf OPTIONAL
        !ir_salv_adapter            TYPE REF TO if_salv_adapter OPTIONAL
      CHANGING
        VALUE(ct_sort)              TYPE lvc_t_sort OPTIONAL
        VALUE(ct_filter)            TYPE lvc_t_filt OPTIONAL
      EXCEPTIONS
        invalid_parameter_combination
        program_error
        too_many_lines .

    METHODS dispatch
        REDEFINITION .
    METHODS finalize
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA outtab TYPE REF TO data .
    DATA talv TYPE REF TO zcl_talv_parent .
ENDCLASS.



CLASS ZCL_GUI_ALV_GRID IMPLEMENTATION.


  METHOD constructor.

    super->constructor( i_shellstyle            = i_shellstyle
                        i_lifetime              = i_lifetime
                        i_parent                = i_parent
                        i_appl_events           = i_appl_events
                        i_parentdbg             = i_parentdbg
                        i_applogparent          = i_applogparent
                        i_graphicsparent        = i_graphicsparent
                        i_name                  = i_name
                        i_fcat_complete         = i_fcat_complete ).

    outtab = mt_outtab.

    me->talv = talv.

  ENDMETHOD.


  METHOD dispatch.

    DATA: action TYPE sy-ucomm.
    DATA: ucomm TYPE sy-ucomm.

    get_event_parameter(
      EXPORTING
        parameter_id = 0
        queue_only   = space
      IMPORTING
        parameter    = action ).

    LOOP AT talv->key-intercept_ucomms INTO ucomm.

      IF ucomm <> action.
        CONTINUE.
      ELSE.
        RAISE EVENT grid_dispatch
          EXPORTING
            action = action.
        CLEAR action.
        EXIT.
      ENDIF.

    ENDLOOP.

    IF talv->key-intercept_ucomms IS INITIAL
      OR action IS NOT INITIAL.

      CALL METHOD super->dispatch
        EXPORTING
          cargo             = cargo
          eventid           = eventid
          is_shellevent     = is_shellevent
          is_systemdispatch = is_systemdispatch
        EXCEPTIONS
          cntl_error        = 1
          OTHERS            = 2.
      IF sy-subrc <> 0.
        " Implement suitable error handling here
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD finalize.
    CALL METHOD super->finalize.
  ENDMETHOD.


  METHOD ready_for_it.

    FIELD-SYMBOLS <outtab> TYPE STANDARD TABLE.
    ASSIGN outtab->* TO <outtab>.

    set_table_for_first_display(
       EXPORTING
         i_buffer_active               = i_buffer_active
         i_bypassing_buffer            = i_bypassing_buffer
         i_consistency_check           = i_consistency_check
         i_structure_name              = i_structure_name
         is_variant                    = talv->get_variant( )
         i_save                        = i_save
         i_default                     = i_default
         is_layout                     = talv->get_layout( )
         is_print                      = is_print
         it_special_groups             = it_special_groups
         it_toolbar_excluding          = talv->get_ui_func( )
         it_hyperlink                  = it_hyperlink
         it_alv_graphics               = it_alv_graphics
         it_except_qinfo               = it_except_qinfo
         ir_salv_adapter               = ir_salv_adapter
       CHANGING
         it_outtab                     = <outtab>
         it_fieldcatalog               = talv->key-fieldcat
         it_sort                       = ct_sort
         it_filter                     = ct_filter
       EXCEPTIONS
         invalid_parameter_combination = 1
         program_error                 = 2
         too_many_lines                = 3
         OTHERS                        = 4 ).
    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
