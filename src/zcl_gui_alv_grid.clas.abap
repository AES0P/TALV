class ZCL_GUI_ALV_GRID definition
  public
  inheriting from CL_GUI_ALV_GRID
  final
  create public

  global friends ZCL_TALV_PARENT
                 ZIF_TALV_EVENT_HANDLE_IMP .

public section.

  events GRID_DISPATCH
    exporting
      value(ACTION) type SY-UCOMM .

  methods CONSTRUCTOR
    importing
      !TALV type ref to ZCL_TALV_PARENT
      !I_SHELLSTYLE type I default 0
      !I_LIFETIME type I optional
      !I_PARENT type ref to CL_GUI_CONTAINER
      !I_APPL_EVENTS type CHAR01 default SPACE
      !I_PARENTDBG type ref to CL_GUI_CONTAINER optional
      !I_APPLOGPARENT type ref to CL_GUI_CONTAINER optional
      !I_GRAPHICSPARENT type ref to CL_GUI_CONTAINER optional
      !I_NAME type STRING optional
      !I_FCAT_COMPLETE type SAP_BOOL default SPACE
    exceptions
      ERROR_CNTL_CREATE
      ERROR_CNTL_INIT
      ERROR_CNTL_LINK
      ERROR_DP_CREATE .
  methods READY_FOR_IT
    importing
      !I_BUFFER_ACTIVE type ANY optional
      !I_BYPASSING_BUFFER type CHAR01 optional
      !I_CONSISTENCY_CHECK type CHAR1 optional
      !I_STRUCTURE_NAME type DD02L-TABNAME optional
      !I_SAVE type CHAR01 default 'A'
      !I_DEFAULT type CHAR01 default 'X'
      !IS_PRINT type LVC_S_PRNT optional
      !IT_SPECIAL_GROUPS type LVC_T_SGRP optional
      !IT_TOOLBAR_EXCLUDING type UI_FUNCTIONS optional
      !IT_HYPERLINK type LVC_T_HYPE optional
      !IT_ALV_GRAPHICS type DTC_T_TC optional
      !IT_EXCEPT_QINFO type LVC_T_QINF optional
      !IR_SALV_ADAPTER type ref to IF_SALV_ADAPTER optional
    changing
      !CT_SORT type LVC_T_SORT optional
      !CT_FILTER type LVC_T_FILT optional
    exceptions
      INVALID_PARAMETER_COMBINATION
      PROGRAM_ERROR
      TOO_MANY_LINES .
  methods READ_LAYOUT_SETTING
    importing
      !OBJ_ID type ZTALV_LAYOUT_SET-OBJ_ID default '0' .

  methods DISPATCH
    redefinition .
  methods FINALIZE
    redefinition .
  PROTECTED SECTION.
private section.

  data OUTTAB type ref to DATA .
  data TALV type ref to ZCL_TALV_PARENT .

  methods READ_FIELDS_SETTING .
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

    read_layout_setting( ).

    read_fields_setting( ).

    set_table_for_first_display(
       EXPORTING
         i_buffer_active               = i_buffer_active
         i_bypassing_buffer            = i_bypassing_buffer
         i_consistency_check           = i_consistency_check
         i_structure_name              = i_structure_name
         is_variant                    = talv->get_variant( )
         i_save                        = i_save
         i_default                     = i_default
         is_layout                     = talv->key-layout
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
    CASE sy-subrc.
      WHEN 1.
        MESSAGE a530(0k) RAISING invalid_parameter_combination.
      WHEN 2.
        MESSAGE a534(0k) RAISING program_error.
      WHEN 2.
        MESSAGE a099(0k) RAISING too_many_lines.
      WHEN 4.
        MESSAGE a203(0k) RAISING program_error.
    ENDCASE.

  ENDMETHOD.


  METHOD read_fields_setting.

    FIELD-SYMBOLS <fields_setting> LIKE LINE OF talv->fields_setting.
    FIELD-SYMBOLS <fieldcat> TYPE lvc_s_fcat.

    LOOP AT talv->fields_setting ASSIGNING <fields_setting>.

      READ TABLE talv->key-fieldcat ASSIGNING <fieldcat> WITH KEY fieldname = <fields_setting>-fieldname.
      CHECK sy-subrc = 0.

      MOVE-CORRESPONDING <fields_setting> TO <fieldcat>.

    ENDLOOP.

  ENDMETHOD.


  METHOD read_layout_setting.

    FIELD-SYMBOLS <layout_setting> LIKE LINE OF talv->layout_setting.

    READ TABLE talv->layout_setting ASSIGNING <layout_setting> WITH KEY obj_id = obj_id.
    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING <layout_setting> TO talv->key-layout.

  ENDMETHOD.
ENDCLASS.
