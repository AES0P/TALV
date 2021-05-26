CLASS ycl_talv_parent DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS ycl_gui_alv_grid .

  PUBLIC SECTION.

    INTERFACES yif_talv_event_handle_imp .

    ALIASES on_retrieve
      FOR yif_talv_event_handle_imp~on_retrieve .

    TYPES:
      BEGIN OF ty_intercept_ucomm,
        action TYPE sy-ucomm,
      END OF ty_intercept_ucomm .
    TYPES:
      tty_intercept_ucomm TYPE STANDARD TABLE OF ty_intercept_ucomm WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_fieldname,
        fieldname TYPE fieldname,
      END OF ty_fieldname .
    TYPES:
      tty_fieldname TYPE STANDARD TABLE OF ty_fieldname WITH EMPTY KEY .

    EVENTS set_pf_status .
    EVENTS set_title .
    EVENTS on_pbo .
    EVENTS pai_command
      EXPORTING
        VALUE(e_ucomm) TYPE sy-ucomm .
    EVENTS on_exit .
    EVENTS retrieve .

    METHODS constructor
      IMPORTING
        VALUE(talv_key) TYPE zstalv_key .
    METHODS get_key_info
      IMPORTING
        VALUE(fieldname)  TYPE fieldname
      RETURNING
        VALUE(fieldvalue) TYPE fieldvalue .
    METHODS set_fieldcat
      IMPORTING
        VALUE(fieldcat)       TYPE lvc_t_fcat OPTIONAL
        VALUE(structure_name) TYPE tabname OPTIONAL
          PREFERRED PARAMETER fieldcat .
    METHODS get_fieldcat
      RETURNING
        VALUE(fieldcat) TYPE lvc_t_fcat .
    METHODS remove_fields
      IMPORTING
        VALUE(fields) TYPE c .
    METHODS save_fields
      IMPORTING
        VALUE(fields) TYPE c .
    METHODS set_column_editable
      IMPORTING
        !fieldname  TYPE fieldname
      RETURNING
        VALUE(talv) TYPE REF TO ycl_talv_parent .
    METHODS set_layout
      IMPORTING
        VALUE(layout) TYPE lvc_s_layo .
    METHODS get_layout
      RETURNING
        VALUE(layout) TYPE lvc_s_layo .
    METHODS set_variant
      IMPORTING
        VALUE(variant) TYPE disvariant .
    METHODS get_variant
      RETURNING
        VALUE(variant) TYPE disvariant .
    METHODS set_ui_func
      IMPORTING
        VALUE(remove_all) TYPE abap_bool OPTIONAL
        VALUE(ui_func)    TYPE ui_functions OPTIONAL .
    METHODS get_ui_func
      RETURNING
        VALUE(ui_func) TYPE ui_functions .
    METHODS get_grid
      RETURNING
        VALUE(grid) TYPE REF TO ycl_gui_alv_grid .
    METHODS set_interval
      IMPORTING
        VALUE(interval) TYPE i .
    METHODS get_interval
      RETURNING
        VALUE(interval) TYPE i .
    METHODS get_log
      RETURNING
        VALUE(log) TYPE REF TO ycl_log .
    METHODS get_outtab
      RETURNING
        VALUE(outtab) TYPE REF TO data .
    METHODS refresh .
    METHODS add_button
      IMPORTING
        VALUE(fun_code)  TYPE ui_func
        VALUE(btn_type)  TYPE tb_btype
        VALUE(icon)      TYPE any
        VALUE(text)      TYPE text40 OPTIONAL
        VALUE(quickinfo) TYPE iconquick OPTIONAL
        VALUE(object)    TYPE REF TO cl_alv_event_toolbar_set .
    METHODS display
        ABSTRACT .
    METHODS pai
      CHANGING
        !ucomm TYPE sy-ucomm .
    METHODS pbo .
    METHODS free .
    METHODS init_style
      IMPORTING
        !edit TYPE abap_bool OPTIONAL .
    METHODS add_line_style
      IMPORTING
        !fieldname  TYPE lvc_s_styl-fieldname OPTIONAL
        !style      TYPE lvc_style
        !style2     TYPE lvc_style OPTIONAL
        !style3     TYPE lvc_style OPTIONAL
        !style4     TYPE lvc_style OPTIONAL
        !maxlen     TYPE int4 OPTIONAL
      RETURNING
        VALUE(talv) TYPE REF TO ycl_talv_parent .
    METHODS set_style_for_all_lines .
    METHODS set_style_for_single_line
      IMPORTING
        !index TYPE i .
    METHODS get_style_table
      RETURNING
        VALUE(style_table) TYPE lvc_t_styl .
    METHODS set_style_table
      IMPORTING
        !style_table TYPE lvc_t_styl .
    METHODS clear_line_style_table .
    METHODS init_color
      IMPORTING
        VALUE(color) TYPE lvc_col .
    METHODS add_line_color
      IMPORTING
        VALUE(fieldname) TYPE lvc_s_styl-fieldname OPTIONAL
        VALUE(col)       TYPE lvc_col
        VALUE(int)       TYPE lvc_int OPTIONAL
        VALUE(inv)       TYPE lvc_inv OPTIONAL
        VALUE(nokeycol)  TYPE lvc_nokeyc OPTIONAL
      RETURNING
        VALUE(talv)      TYPE REF TO ycl_talv_parent .
    METHODS set_color_for_all_lines .
    METHODS set_color_for_single_line
      IMPORTING
        !index TYPE i .
    METHODS get_color_table
      RETURNING
        VALUE(color_table) TYPE lvc_t_scol .
    METHODS set_color_table
      IMPORTING
        !color_table TYPE lvc_t_scol .
    METHODS clear_line_color_table .
    METHODS init_fieldcat .
    METHODS is_initialized
      RETURNING
        VALUE(initialized) TYPE abap_bool .
  PROTECTED SECTION.

    ALIASES on_handle_changed_finished
      FOR yif_talv_event_handle_imp~on_handle_changed_finished .
    ALIASES on_handle_countdown_finished
      FOR yif_talv_event_handle_imp~on_handle_countdown_finished .
    ALIASES on_handle_data_changed
      FOR yif_talv_event_handle_imp~on_handle_data_changed .
    ALIASES on_handle_double_click
      FOR yif_talv_event_handle_imp~on_handle_double_click .
    ALIASES on_handle_grid_dispatch
      FOR yif_talv_event_handle_imp~on_handle_grid_dispatch .
    ALIASES on_handle_hotspot_click
      FOR yif_talv_event_handle_imp~on_handle_hotspot_click .
    ALIASES on_handle_line_button_click
      FOR yif_talv_event_handle_imp~on_handle_line_button_click .
    ALIASES on_handle_toolbar
      FOR yif_talv_event_handle_imp~on_handle_toolbar .
    ALIASES on_handle_user_command
      FOR yif_talv_event_handle_imp~on_handle_user_command .
    ALIASES on_pai_command
      FOR yif_talv_event_handle_imp~on_pai_command .
    ALIASES on_set_pf_status
      FOR yif_talv_event_handle_imp~on_set_pf_status .
    ALIASES on_set_title
      FOR yif_talv_event_handle_imp~on_set_title .

    DATA key TYPE zstalv_key .
    DATA container TYPE REF TO cl_gui_container .
    DATA log TYPE REF TO ycl_log .
    DATA timer TYPE REF TO cl_gui_timer .
    DATA grid TYPE REF TO ycl_gui_alv_grid .
  PRIVATE SECTION.

    DATA initialized TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA style_table TYPE lvc_t_styl .
    DATA color_table TYPE lvc_t_scol .
    DATA intercept_ucomm TYPE tty_intercept_ucomm .

    METHODS set_btn_style_for_single_line
      CHANGING
        VALUE(alv_line) TYPE any .
    METHODS create_table .
    METHODS copy_table .
    METHODS check_key_info .
    METHODS init .
    METHODS init_event_prefix .
    METHODS set_key_info
      IMPORTING
        VALUE(fieldname)  TYPE fieldname
        VALUE(fieldvalue) TYPE fieldvalue .
    METHODS handle_long_text_field
      IMPORTING
        !fieldname TYPE fieldname
      CHANGING
        !value     TYPE c .
ENDCLASS.



CLASS YCL_TALV_PARENT IMPLEMENTATION.


  METHOD add_button.

    DATA: utoolbar TYPE stb_button.
    CLEAR utoolbar.

    utoolbar-function  = fun_code.  "功能代码
    utoolbar-butn_type = btn_type.  "工具栏按钮类型
    utoolbar-icon      = icon.      "图标名称
    utoolbar-text      = text.      "文本
    utoolbar-quickinfo = quickinfo. "鼠标停留时的提示信息

    "utoolbar-disabled = 'X'.        " X表示灰色，不可用

    APPEND utoolbar TO object->mt_toolbar.

  ENDMETHOD.


  METHOD add_line_color.

    "1：海蓝；2：浅清；3：黄色；4：浅蓝；5：青色；6：红色；7：橙色
    " 列的颜色设置:LVC_S_FCAT-EMPHASIZE,关键（KEY）列，则设置的颜色就不会起作用了.

    DATA cell_color TYPE lvc_s_scol.

    cell_color-fname     = fieldname.
    cell_color-color-col = col."主颜色
    cell_color-color-int = int."辅助颜色
    "末位为0时，表示首位数字表为表格的底色
    "末位为1时，则表示以1为底色，首位数字则表为表格字体的颜色
    "末位为其它颜色时，则表示底色为alv的默认颜色
    "其中c200与系统标准alv底色比较相似；c410与系统标准关键字颜色比较相似
    cell_color-color-inv = int.
    cell_color-nokeycol  = nokeycol.

    INSERT cell_color INTO TABLE color_table.

    talv = me.

  ENDMETHOD.


  METHOD add_line_style.

    DATA style_line TYPE lvc_s_styl.

    style_line-fieldname = fieldname.
    style_line-style     = style.
    style_line-style2    = style2.
    style_line-style3    = style3.
    style_line-style4    = style4.
    style_line-maxlen    = maxlen.

    "排序表不要直接append
    INSERT style_line INTO TABLE style_table.

    talv = me.

  ENDMETHOD.


  METHOD check_key_info.

    key-program = sy-cprog.

  ENDMETHOD.


  METHOD clear_line_color_table.
    CLEAR: color_table.
  ENDMETHOD.


  METHOD clear_line_style_table.
    CLEAR: style_table.
  ENDMETHOD.


  METHOD constructor.

    key = talv_key.

    check_key_info( ).

    init_event_prefix( ).

  ENDMETHOD.


  METHOD copy_table.

    DATA dynamictor TYPE REF TO ycl_dynamic_tool.
    dynamictor ?= ycl_dynamic_tool=>get_instance( ).

    DATA components TYPE cl_abap_structdescr=>component_table.

    FIELD-SYMBOLS <ref_table> TYPE STANDARD TABLE.
    DATA table_position TYPE char100.

    IF key-ref_table_name IS NOT INITIAL.
      table_position = '(' && key-program && ')' && key-ref_table_name.
    ELSEIF key-ref_data_name IS NOT INITIAL.
      table_position = '(' && key-program && ')' && key-ref_data_name && '->*'.
    ENDIF.

    ASSIGN (table_position) TO <ref_table>.

    IF key-ref_table_name IS NOT INITIAL."内表是参考全局DDIC创建的，可以自动生成fieldcat
      components    = dynamictor->get_table_components_by_data( <ref_table> ).
      key-fieldcat  = dynamictor->convert_components_to_fieldcat( components ).
    ELSEIF key-ref_data_name IS NOT INITIAL.
      IF key-fieldcat IS INITIAL."数据引用则必须自己传入fieldcat
        MESSAGE '参考数据引用创建TALV必须同时传入Fieldcat！' TYPE 'E'.
      ENDIF.
    ENDIF.

    set_fieldcat( key-fieldcat ).

    create_table( ).

    FIELD-SYMBOLS <alv_table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <alv_table>.

    MOVE-CORRESPONDING <ref_table> TO <alv_table>.

  ENDMETHOD.


  METHOD create_table.

    CHECK grid->outtab IS NOT BOUND.
    IF key-style_table_name IS INITIAL AND key-checkbox_name IS INITIAL.
      CREATE DATA grid->outtab TYPE STANDARD TABLE OF (key-ddic_type).
    ELSE.

      grid->outtab = ycl_dynamic_tool=>create_dynamic_table( talv_key = key ).

      DATA fieldcat TYPE lvc_s_fcat.
      IF key-checkbox_name IS NOT INITIAL.
        CLEAR: fieldcat.
        fieldcat-key       = abap_true.
        fieldcat-checkbox  = abap_true.
        fieldcat-edit      = abap_true.
        fieldcat-fieldname = key-checkbox_name.
        fieldcat-coltext   = key-checkbox_name.
        APPEND fieldcat TO key-fieldcat.
      ENDIF.

      IF key-light_name IS NOT INITIAL.
        CLEAR: fieldcat.
        fieldcat-key       = abap_true.
        fieldcat-icon      = abap_true.
        fieldcat-fieldname = key-light_name.
        fieldcat-coltext   = key-light_name.
        APPEND fieldcat TO key-fieldcat.
      ENDIF.

      set_fieldcat( key-fieldcat ).

    ENDIF.

  ENDMETHOD.


  METHOD free.

    RAISE EVENT on_exit.

    IF timer IS BOUND.
      timer->free( ).
    ENDIF.

    IF log IS BOUND.
      log->free( ).
    ENDIF.

    IF grid IS BOUND.
      grid->free( ).
    ENDIF.

    IF container IS BOUND.
      container->free( ).
    ENDIF.

    CLEAR: color_table,
           style_table.

    FREE: timer,
          log,
          grid,
          container.

    DATA dynnr(4) TYPE n VALUE ''.
    CASE key-type.
      WHEN 'TALV'.

        "HANDLE FULL ALV SCREEN NO
        dynnr = '8999'.
        IMPORT dynnr = dynnr FROM MEMORY ID 'TALV_FULL_DYNNR'.
        IF dynnr <> '8999'.
          dynnr = dynnr - 1.
          EXPORT dynnr = dynnr TO MEMORY ID 'TALV_FULL_DYNNR'.
        ELSE.
          FREE MEMORY ID 'TALV_FULL_DYNNR'.
        ENDIF.

      WHEN 'TALV_POPUP'.

        "HANDLE POPUP ALV SCREEN NO
        dynnr = '9899'.
        IMPORT dynnr = dynnr FROM MEMORY ID 'TALV_POP_DYNNR'.
        IF dynnr <> '9899'.
          dynnr = dynnr - 1.
          EXPORT dynnr = dynnr TO MEMORY ID 'TALV_POP_DYNNR'.
        ELSE.
          FREE MEMORY ID 'TALV_POP_DYNNR'.
        ENDIF.

    ENDCASE.

    CLEAR: key.

    CLEAR initialized.

  ENDMETHOD.


  METHOD get_color_table.
    color_table = me->color_table.
  ENDMETHOD.


  METHOD get_fieldcat.
    grid->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = fieldcat ).
  ENDMETHOD.


  METHOD get_grid.
    grid = me->grid.
  ENDMETHOD.


  METHOD get_interval.
    interval = key-interval.
  ENDMETHOD.


  METHOD get_key_info.

    FIELD-SYMBOLS <field> TYPE any.

    "此方法只用于获取 字符 数字 类型的字段值，内表和结构字段不允许通过此方法获取
    CHECK NOT fieldname CS 'FIELDCAT'.
    CHECK NOT fieldname CS 'LAYOUT'.
    CHECK NOT fieldname CS 'VARIANT'.
    CHECK NOT fieldname CS 'UI_FUNC'.
    CHECK NOT fieldname CS 'INTERCEPT_UCOMMS'.

    ASSIGN COMPONENT fieldname OF STRUCTURE key TO <field>.
    IF <field> IS ASSIGNED.
      fieldvalue = <field>.
    ENDIF.

  ENDMETHOD.


  METHOD get_layout.
    grid->get_frontend_layout( IMPORTING es_layout = layout  ).
  ENDMETHOD.


  METHOD get_log.
    log =  me->log.
  ENDMETHOD.


  METHOD get_outtab.
    outtab = grid->outtab.
  ENDMETHOD.


  METHOD get_style_table.
    style_table = me->style_table.
  ENDMETHOD.


  METHOD get_ui_func.
    ui_func = key-ui_func.
  ENDMETHOD.


  METHOD get_variant.
    grid->get_variant( IMPORTING es_variant = variant ).
  ENDMETHOD.


  METHOD handle_long_text_field.

    DATA fieldcat TYPE lvc_s_fcat.

    READ TABLE key-fieldcat INTO fieldcat WITH KEY fieldname = fieldname.
    CHECK fieldcat IS NOT INITIAL.

    DATA text TYPE string.
    text = value.

    DATA title TYPE sy-title.
    IF fieldcat-coltext IS NOT INITIAL.
      title = fieldcat-coltext.
    ELSE.
      title = fieldcat-scrtext_l.
    ENDIF.

    CALL FUNCTION 'ZFM_EDIT_LONG_TEXT_FIELD'
      EXPORTING
        i_title             = title
        i_editable          = fieldcat-edit
        i_field_length      = fieldcat-intlen
        i_wordwrap_position = 132
      CHANGING
        c_text              = text.
    IF sy-subrc = 0.
      value = text.
    ENDIF.

  ENDMETHOD.


  METHOD init.

    CHECK is_initialized( ) = abap_false.

    IF key-container_name IS NOT INITIAL.
      CREATE OBJECT container TYPE cl_gui_custom_container
        EXPORTING
          container_name = key-container_name.
    ELSE.
      CREATE OBJECT container TYPE cl_gui_docking_container
        EXPORTING
          repid     = key-program
          dynnr     = key-dynnr
          extension = 3000. "alv宽度
    ENDIF.

    CREATE OBJECT grid
      EXPORTING
        talv     = me
        i_parent = container.

    "不设置时也默认为1
    grid->set_ready_for_input( 1 ).

    "check data changed
    grid->check_changed_data( ).

    "单元格更改触发
    grid->register_edit_event( grid->mc_evt_modified ).

    SET HANDLER on_handle_toolbar           FOR grid.
    SET HANDLER on_handle_user_command      FOR grid.
    SET HANDLER on_handle_hotspot_click     FOR grid.
    SET HANDLER on_handle_double_click      FOR grid.
    SET HANDLER on_handle_data_changed      FOR grid.
    SET HANDLER on_handle_changed_finished  FOR grid.
    SET HANDLER on_handle_grid_dispatch     FOR grid.
    SET HANDLER on_handle_line_button_click FOR grid.

    SET HANDLER yif_talv_event_handle_imp~on_pbo  FOR me.
    SET HANDLER on_pai_command   FOR me.
    SET HANDLER yif_talv_event_handle_imp~on_exit FOR me.
    SET HANDLER on_set_pf_status FOR me.
    SET HANDLER on_set_title     FOR me.

    SET HANDLER on_retrieve      FOR me.

    IF key-log_object IS NOT INITIAL AND key-log_subobject IS NOT INITIAL.

      DATA: keyinfo TYPE balnrext.
      keyinfo = key-program && key-dynnr && key-container_name.

      log = ycl_log=>get_instance( object    = key-log_object
                                   subobject = key-log_subobject
                                   msg_ext   = keyinfo ).
    ENDIF.

    IF key-interval > 0.

      CREATE OBJECT timer.

      SET HANDLER on_handle_countdown_finished FOR timer.

      set_interval( key-interval ).

    ENDIF.

    set_variant( key-variant ).

    set_layout( key-layout ).

    set_ui_func( remove_all = key-no_ui
                 ui_func    = key-ui_func ).

    IF key-ref_table_name IS NOT INITIAL OR key-ref_data_name IS NOT INITIAL.

      init_fieldcat( ).

      copy_table( ).

    ELSE.

      set_fieldcat( structure_name = key-ddic_type
                    fieldcat       = key-fieldcat ).

      init_fieldcat( ).

      create_table( ).

      RAISE EVENT retrieve.

    ENDIF.

    grid->ready_for_it( ).

    initialized = abap_true.

  ENDMETHOD.


  METHOD init_color.

    clear_line_color_table( ).
    add_line_color( color ).
    set_color_for_all_lines( ).

  ENDMETHOD.


  METHOD init_event_prefix.

    key-frm_prefix = 'FRM_' && key-dynnr && '_'.

    IF key-handle_toolbar IS INITIAL.
      key-handle_toolbar = key-frm_prefix && 'HANDLE_TOOLBAR'.
    ENDIF.

    IF key-handle_user_command IS INITIAL.
      key-handle_user_command = key-frm_prefix && 'HANDLE_USER_COMMAND'.
    ENDIF.

    IF key-handle_hotspot_click IS INITIAL.
      key-handle_hotspot_click = key-frm_prefix && 'HANDLE_HOTSPOT_CLICK'.
    ENDIF.

    IF key-handle_double_click IS INITIAL.
      key-handle_double_click = key-frm_prefix && 'HANDLE_DOUBLE_CLICK'.
    ENDIF.

    IF key-handle_data_changed IS INITIAL.
      key-handle_data_changed = key-frm_prefix && 'HANDLE_DATA_CHANGED'.
    ENDIF.

    IF key-handle_data_changed_finished IS INITIAL.
      key-handle_data_changed_finished = key-frm_prefix && 'HANDLE_CHANGED_OVER'.
    ENDIF.

    IF key-handle_countdown_finished IS INITIAL.
      key-handle_countdown_finished = key-frm_prefix && 'HANDLE_COUNTDOWN'.
    ENDIF.

    IF key-handle_set_pf_status IS INITIAL.
      key-handle_set_pf_status = key-frm_prefix && 'HANDLE_SET_PF_STATUS'.
    ENDIF.

    IF key-handle_set_title IS INITIAL.
      key-handle_set_title = key-frm_prefix && 'HANDLE_SET_TITLE'.
    ENDIF.

    IF key-handle_on_pbo IS INITIAL.
      key-handle_on_pbo = key-frm_prefix && 'HANDLE_ON_PBO'.
    ENDIF.

    IF key-handle_pai_command IS INITIAL.
      key-handle_pai_command = key-frm_prefix && 'HANDLE_PAI_COMMAND'.
    ENDIF.

    IF key-handle_exit IS INITIAL.
      key-handle_exit = key-frm_prefix && 'HANDLE_EXIT'.
    ENDIF.

    IF key-handle_retrieve IS INITIAL.
      key-handle_retrieve = key-frm_prefix && 'HANDLE_RETRIEVE'.
    ENDIF.

    IF key-handle_grid_dispatch IS INITIAL.
      key-handle_grid_dispatch = key-frm_prefix && 'HANDLE_GRID_DISPATCH'.
    ENDIF.

    IF key-handle_line_btn_click IS INITIAL.
      key-handle_line_btn_click = key-frm_prefix && 'HANDLE_LINE_BTN_CLICK'.
    ENDIF.

  ENDMETHOD.


  METHOD init_fieldcat.

    CHECK key-fieldcat IS NOT INITIAL.

    IF key-style_table_name IS NOT INITIAL
      AND key-show_long_text_button = abap_true.

      DATA fieldcat TYPE lvc_s_fcat.
      DATA: fieldcat_new  TYPE lvc_s_fcat,
            fieldcats_new TYPE lvc_t_fcat.

      LOOP AT key-fieldcat INTO fieldcat WHERE intlen > 128.

        CLEAR fieldcat_new.
        fieldcat_new-fieldname = fieldcat-fieldname && '_TBTN'.
        fieldcat_new-coltext   = '长文本'.
        fieldcat_new-col_pos   = fieldcat-col_pos.
        fieldcat_new-icon      = abap_true.
        APPEND fieldcat_new TO fieldcats_new.

        CLEAR: fieldcat.

      ENDLOOP.

      IF fieldcats_new IS NOT INITIAL.
        APPEND LINES OF fieldcats_new TO key-fieldcat.
      ENDIF.

      SORT key-fieldcat BY col_pos.

    ENDIF.

  ENDMETHOD.


  METHOD init_style.

    clear_line_style_table( ).

    IF edit = abap_true.
      add_line_style( ycl_gui_alv_grid=>mc_style_enabled ).
    ELSE.
      add_line_style( ycl_gui_alv_grid=>mc_style_disabled ).

      IF key-checkbox_name <> space.
        add_line_style( fieldname = key-checkbox_name
                        style     = ycl_gui_alv_grid=>mc_style_enabled ).
      ENDIF.

    ENDIF.

    set_style_for_all_lines( ).

  ENDMETHOD.


  METHOD is_initialized.

    initialized = me->initialized.

  ENDMETHOD.


  METHOD pai.

    IF ucomm = '&BACK' OR ucomm = '&EXIT' OR ucomm = '&CANCEL'.
      LEAVE TO SCREEN 0.
    ENDIF.

    IF key-handle_pai_command IS INITIAL.

      IF cl_abap_demo_services=>is_production_system( ) <> abap_true.
        MESSAGE '请自定义响应操作' TYPE 'S'.
      ENDIF.

    ELSE.

      RAISE EVENT pai_command
        EXPORTING e_ucomm = ucomm.

    ENDIF.

  ENDMETHOD.


  METHOD pbo.

    init( ).

    SET PF-STATUS 'STATUS' OF PROGRAM 'SAPLZFUNG_TALV'.
    RAISE EVENT set_pf_status.

    SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH '这' '是' '个' '标' '题'.
    RAISE EVENT set_title.

    RAISE EVENT on_pbo.

  ENDMETHOD.


  METHOD refresh.

    "刷新变量
    DATA: stable TYPE lvc_s_stbl.

    "刷新显示
    CLEAR stable.

    stable-row = 'X'."基于行刷新
    stable-col = 'X'."基于列刷新

    grid->refresh_table_display( is_stable = stable ).

    IF timer IS BOUND.
      timer->run( ).
    ENDIF.

  ENDMETHOD.


  METHOD remove_fields.

    DATA: fieldcats TYPE lvc_t_fcat.

    DATA: fieldnames TYPE tty_fieldname,
          fieldname  LIKE LINE OF fieldnames.

    CHECK fields IS NOT INITIAL.

    TRANSLATE fields TO UPPER CASE.
    SPLIT fields AT space INTO TABLE fieldnames.

    fieldcats = get_fieldcat( ).

    LOOP AT fieldnames INTO fieldname.
      DELETE fieldcats WHERE fieldname = fieldname-fieldname.
    ENDLOOP.

    set_fieldcat( fieldcats ).

  ENDMETHOD.


  METHOD save_fields.

    DATA: fieldcats_old TYPE lvc_t_fcat,
          fieldcats_new TYPE lvc_t_fcat.

    DATA: fieldcat LIKE LINE OF fieldcats_old.

    DATA: fieldnames TYPE tty_fieldname,
          fieldname  LIKE LINE OF fieldnames.

    CHECK fields IS NOT INITIAL.

    TRANSLATE fields TO UPPER CASE.
    SPLIT fields AT space INTO TABLE fieldnames.

    CLEAR: fieldcats_new.
    fieldcats_old = get_fieldcat( ).

    LOOP AT fieldnames INTO fieldname.

      READ TABLE fieldcats_old INTO fieldcat WITH KEY fieldname = fieldname-fieldname.
      IF sy-subrc = 0.
        APPEND fieldcat TO fieldcats_new.
        CLEAR: fieldcat.
      ENDIF.

    ENDLOOP.

    set_fieldcat( fieldcats_new ).

  ENDMETHOD.


  METHOD set_btn_style_for_single_line.

    CHECK key-style_table_name IS NOT INITIAL.
    CHECK alv_line IS NOT INITIAL.

    FIELD-SYMBOLS <style> TYPE lvc_t_styl.
    DATA style_line TYPE lvc_s_styl.

    FIELD-SYMBOLS <value> TYPE any.

    ASSIGN COMPONENT key-style_table_name OF STRUCTURE alv_line TO <style>.

    DATA fieldcats TYPE lvc_t_fcat.
    DATA fieldcat TYPE lvc_s_fcat.

    fieldcats = key-fieldcat.
    SORT fieldcats BY fieldname.
    LOOP AT fieldcats INTO fieldcat WHERE fieldname CS '_TBTN'.

      DELETE <style> WHERE fieldname = fieldcat-fieldname.
      CLEAR style_line.
      style_line-fieldname = fieldcat-fieldname.
      style_line-style     = ycl_gui_alv_grid=>mc_style_button.
      INSERT style_line INTO TABLE <style>.

      ASSIGN COMPONENT fieldcat-fieldname OF STRUCTURE alv_line TO <value>.
      <value> = '@3S@'.

    ENDLOOP.


  ENDMETHOD.


  METHOD set_color_for_all_lines.

    CHECK get_key_info( 'COLOR_TABLE_NAME' ) <> space.

    FIELD-SYMBOLS: <table>      TYPE STANDARD TABLE,
                   <table_line> TYPE any.

    ASSIGN grid->outtab->* TO <table>.

    LOOP AT <table> ASSIGNING <table_line>.
      set_color_for_single_line( sy-tabix ).
    ENDLOOP.

  ENDMETHOD.


  METHOD set_color_for_single_line.

    CHECK get_key_info( 'COLOR_TABLE_NAME' ) <> space.

    FIELD-SYMBOLS: <table>      TYPE STANDARD TABLE,
                   <table_line> TYPE any,
                   <color_tab>  TYPE lvc_t_scol.

    ASSIGN grid->outtab->* TO <table>.

    READ TABLE <table> ASSIGNING <table_line> INDEX index.
    CHECK <table_line> IS ASSIGNED.

    ASSIGN COMPONENT get_key_info( 'COLOR_TABLE_NAME' ) OF STRUCTURE <table_line> TO <color_tab>.
    CHECK <color_tab> IS ASSIGNED.

    <color_tab> = get_color_table( ).

  ENDMETHOD.


  METHOD set_color_table.
    me->color_table = color_table.
  ENDMETHOD.


  METHOD set_column_editable.

    DATA fieldcat TYPE lvc_s_fcat.
    fieldcat-edit = abap_true.

    MODIFY key-fieldcat FROM fieldcat TRANSPORTING edit WHERE fieldname = fieldname.
    set_fieldcat( key-fieldcat ).

    talv = me.

  ENDMETHOD.


  METHOD set_fieldcat.

    IF structure_name IS INITIAL AND fieldcat IS NOT INITIAL.

      LOOP AT fieldcat TRANSPORTING NO FIELDS WHERE datatype  = space
                                                AND ref_table = space
                                                AND ref_field = space
                                                AND rollname  = space
                                                AND fieldname <> key-checkbox_name
                                                AND fieldname <> key-light_name
                                                AND NOT fieldname CS '_TBTN'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        MESSAGE '参考字段或者数据类型至少提供一种！' TYPE 'E'.
      ENDIF.

      key-fieldcat = fieldcat.

    ELSEIF structure_name IS NOT INITIAL.

      CLEAR: key-fieldcat.
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = structure_name
        CHANGING
          ct_fieldcat            = key-fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      IF sy-subrc <> 0.
      ENDIF.

    ENDIF.

    DATA fieldcatlog TYPE lvc_s_fcat.
    fieldcatlog-col_opt = 'A'.
    MODIFY key-fieldcat FROM fieldcatlog TRANSPORTING col_opt WHERE col_opt <> 'A'.

    grid->set_frontend_fieldcatalog( key-fieldcat ).

  ENDMETHOD.


  METHOD set_interval.

    key-interval    = interval.
    timer->interval = key-interval.

    timer->run( ).

  ENDMETHOD.


  METHOD set_key_info.

    FIELD-SYMBOLS <field> TYPE any.

    ASSIGN COMPONENT fieldname OF STRUCTURE key TO <field>.
    IF <field> IS ASSIGNED.
      <field> = fieldvalue.
    ENDIF.

  ENDMETHOD.


  METHOD set_layout.

    IF layout IS NOT INITIAL.

      grid->set_frontend_layout( layout ).

    ELSE.

      CLEAR: key-layout.
      key-layout-zebra      = abap_true.  "zebra
      key-layout-no_rowmark = abap_false.  "no rowmark
      key-layout-cwidth_opt = abap_true.  "cwidth_opt

      IF key-style_table_name IS NOT INITIAL.
        key-layout-stylefname = key-style_table_name.
      ENDIF.

      IF key-checkbox_name IS NOT INITIAL.
*        key-layout-box_fname  = key-checkbox_name.
        key-layout-no_rowmark = abap_true.  "no rowmark
      ENDIF.

      IF key-color_table_name IS NOT INITIAL.
        key-layout-ctab_fname = key-color_table_name.
        key-layout-zebra      = abap_false.  "zebra
      ENDIF.

      grid->set_frontend_layout( key-layout ).

    ENDIF.

  ENDMETHOD.


  METHOD set_style_for_all_lines.

    CHECK get_key_info( 'STYLE_TABLE_NAME' ) <> space.

    FIELD-SYMBOLS: <table>      TYPE STANDARD TABLE,
                   <table_line> TYPE any.

    ASSIGN grid->outtab->* TO <table>.

    LOOP AT <table> ASSIGNING <table_line>.
      set_style_for_single_line( sy-tabix ).
    ENDLOOP.

  ENDMETHOD.


  METHOD set_style_for_single_line.

    CHECK get_key_info( 'STYLE_TABLE_NAME' ) <> space.

    FIELD-SYMBOLS: <table>      TYPE STANDARD TABLE,
                   <table_line> TYPE any,
                   <style_tab>  TYPE lvc_t_styl.

    ASSIGN grid->outtab->* TO <table>.

    READ TABLE <table> ASSIGNING <table_line> INDEX index.
    CHECK <table_line> IS ASSIGNED.

    ASSIGN COMPONENT get_key_info( 'STYLE_TABLE_NAME' ) OF STRUCTURE <table_line> TO <style_tab>.
    CHECK <style_tab> IS ASSIGNED.

    <style_tab> = get_style_table( ).
    set_btn_style_for_single_line( CHANGING alv_line = <table_line> ).

  ENDMETHOD.


  METHOD set_style_table.
    me->style_table = style_table.
  ENDMETHOD.


  METHOD set_ui_func.

    CLEAR: key-ui_func.

    IF remove_all = abap_true.

      APPEND ycl_gui_alv_grid=>mc_fc_excl_all TO key-ui_func.

    ELSEIF ui_func IS NOT INITIAL.

      key-ui_func = ui_func.

    ELSE.

      "不想使用这里的默认排除，就自己通过key传入ui_func
*      APPEND ycl_gui_alv_grid=>mc_fc_loc_append_row    TO key-ui_func.
      APPEND ycl_gui_alv_grid=>mc_fc_loc_insert_row    TO key-ui_func.
*      APPEND ycl_gui_alv_grid=>mc_fc_loc_delete_row    TO key-ui_func.
      APPEND ycl_gui_alv_grid=>mc_fc_loc_cut           TO key-ui_func.
      APPEND ycl_gui_alv_grid=>mc_fc_loc_paste         TO key-ui_func.
      APPEND ycl_gui_alv_grid=>mc_fc_loc_paste_new_row TO key-ui_func.
      APPEND ycl_gui_alv_grid=>mc_fc_loc_copy_row      TO key-ui_func.
      APPEND ycl_gui_alv_grid=>mc_fc_loc_undo          TO key-ui_func.

    ENDIF.

  ENDMETHOD.


  METHOD set_variant.

    IF variant IS NOT INITIAL.

      grid->set_variant( variant ).

    ELSE.

      CLEAR: key-variant.
      "使用 报表名 + 屏幕编号 + 容器ID 就可以给每一个ALV设定属于它自己的变式
      key-variant-report = key-program && key-dynnr && key-container_name.
      key-variant-handle   = '0001'.
      key-variant-username = sy-uname.

      grid->set_variant( key-variant ).

    ENDIF.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_exit.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    CHECK grid IS BOUND.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_exit)
 IN PROGRAM (key-program)
      USING me
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_changed_finished.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    DATA: index_rows TYPE lvc_t_row,
          row_no     TYPE lvc_t_roid.

    grid->get_selected_rows(
            IMPORTING
              et_index_rows = index_rows
              et_row_no     = row_no ).

    DATA columns TYPE lvc_t_col.
    grid->get_selected_columns(
            IMPORTING
              et_index_columns = columns ).

    PERFORM (key-handle_data_changed_finished)
 IN PROGRAM (key-program)
      USING me e_modified et_good_cells
   CHANGING <table>
   IF FOUND.

    grid->set_selected_rows( it_index_rows = index_rows
                             it_row_no     = row_no ).

    grid->set_selected_columns( it_col_table = columns ).

    CHECK key-style_table_name IS NOT INITIAL.
    FIELD-SYMBOLS <table_line> TYPE any.
    LOOP AT <table> ASSIGNING <table_line>.
      set_btn_style_for_single_line( CHANGING alv_line = <table_line> ).
    ENDLOOP.
    refresh( ).

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_countdown_finished.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_countdown_finished)
 IN PROGRAM (key-program)
      USING me
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_data_changed.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_data_changed)
 IN PROGRAM (key-program)
      USING me er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm
   CHANGING <table>
   IF FOUND.

    key-layout-cwidth_opt = abap_true.
    set_layout( key-layout ).

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_double_click.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_double_click)
 IN PROGRAM (key-program)
      USING me e_row e_column
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_grid_dispatch.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_grid_dispatch)
 IN PROGRAM (key-program)
      USING me action
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_hotspot_click.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_hotspot_click)
 IN PROGRAM (key-program)
      USING me e_row_id e_column_id es_row_no
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_line_button_click.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    IF es_col_id-fieldname CS '_TBTN'.

      FIELD-SYMBOLS <alv_line> TYPE any.
      FIELD-SYMBOLS <value> TYPE any.

      READ TABLE <table> ASSIGNING <alv_line> INDEX es_row_no-row_id.
      CHECK <alv_line> IS ASSIGNED.

      DATA fieldname TYPE fieldname.
      fieldname = es_col_id-fieldname.

      REPLACE '_TBTN' IN fieldname WITH space.
      ASSIGN COMPONENT fieldname OF STRUCTURE <alv_line> TO <value>.

      handle_long_text_field(
        EXPORTING
          fieldname = fieldname
        CHANGING
          value     = <value> ).

      refresh( ).

    ELSE.
      PERFORM (key-handle_line_btn_click)
   IN PROGRAM (key-program)
        USING me es_col_id es_row_no
     CHANGING <table>
     IF FOUND.
    ENDIF.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_toolbar.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_toolbar)
 IN PROGRAM (key-program)
      USING me e_object e_interactive
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_handle_user_command.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_user_command)
 IN PROGRAM (key-program)
      USING me e_ucomm
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_pai_command.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_pai_command)
 IN PROGRAM (key-program)
      USING me e_ucomm
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_pbo.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_on_pbo)
 IN PROGRAM (key-program)
      USING me
   CHANGING <table>
   IF FOUND.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_retrieve.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <table>.

    PERFORM (key-handle_retrieve)
 IN PROGRAM (key-program)
      USING key-ddic_type CHANGING <table>
   IF FOUND.

    CHECK is_initialized( ) = abap_false.

    IF key-style_table_name IS NOT INITIAL.
      init_style( ).
    ENDIF.

    IF key-color_table_name IS NOT INITIAL.
      init_color( 2 ).
    ENDIF.

  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_set_pf_status.
    PERFORM (key-handle_set_pf_status)
 IN PROGRAM (key-program)
   IF FOUND.
  ENDMETHOD.


  METHOD yif_talv_event_handle_imp~on_set_title.
    PERFORM (key-handle_set_title)
 IN PROGRAM (key-program)
   IF FOUND.
  ENDMETHOD.
ENDCLASS.
