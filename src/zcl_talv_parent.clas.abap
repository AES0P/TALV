class ZCL_TALV_PARENT definition
  public
  abstract
  create public

  global friends ZCL_GUI_ALV_GRID
                 ZIF_TALV_EVENT_HANDLE_IMP .

public section.

  types:
    BEGIN OF ty_intercept_ucomm,
        action TYPE sy-ucomm,
      END OF ty_intercept_ucomm .
  types:
    tty_intercept_ucomm TYPE STANDARD TABLE OF ty_intercept_ucomm WITH EMPTY KEY .
  types:
    BEGIN OF ty_fieldname,
        fieldname TYPE fieldname,
      END OF ty_fieldname .
  types:
    tty_fieldname TYPE STANDARD TABLE OF ty_fieldname WITH EMPTY KEY .
  types TY_FIELDS_SETTING type ZTALV_FIELDS_SET .
  types:
    tty_fields_setting TYPE STANDARD TABLE OF ty_fields_setting WITH EMPTY KEY .
  types TY_LAYOUT_SETTING type ZTALV_LAYOUT_SET .
  types:
    tty_layout_setting TYPE STANDARD TABLE OF ty_layout_setting WITH EMPTY KEY .

  events SET_PF_STATUS .
  events SET_TITLE .
  events ON_PBO .
  events PAI_COMMAND
    exporting
      value(E_UCOMM) type SY-UCOMM .
  events ON_EXIT .
  events RETRIEVE .
  events CHECK_LONG_TEXT
    exporting
      value(FIELDNAME) type FIELDNAME
      value(ROW) type INT4
      value(LONG_TEXT) type STRING .

  methods CONSTRUCTOR
    importing
      !TALV_KEY type ZSTALV_KEY .
  methods GET_FIELDS_SETTING
    returning
      value(FIELDS_SETTING) type TTY_FIELDS_SETTING .
  methods GET_LAYOUT_SETTING
    returning
      value(LAYOUT_SETTING) type TTY_LAYOUT_SETTING .
  methods GET_KEY_INFO
    importing
      !FIELDNAME type FIELDNAME
    returning
      value(FIELDVALUE) type FIELDVALUE .
  methods SET_FIELDCAT
    importing
      value(FIELDCAT) type LVC_T_FCAT optional
      !STRUCTURE_NAME type TABNAME optional
    preferred parameter FIELDCAT .
  methods GET_FIELDCAT
    returning
      value(FIELDCAT) type LVC_T_FCAT .
  methods REMOVE_FIELDS
    importing
      value(FIELDS) type C .
  methods SAVE_FIELDS
    importing
      value(FIELDS) type C .
  methods SET_COLUMN_EDITABLE
    importing
      !FIELDNAME type FIELDNAME
    returning
      value(TALV) type ref to ZCL_TALV_PARENT .
  methods SET_LAYOUT
    importing
      value(LAYOUT) type LVC_S_LAYO .
  methods GET_LAYOUT
    returning
      value(LAYOUT) type LVC_S_LAYO .
  methods SET_VARIANT
    importing
      value(VARIANT) type DISVARIANT .
  methods GET_VARIANT
    returning
      value(VARIANT) type DISVARIANT .
  methods SET_UI_FUNC
    importing
      value(REMOVE_ALL) type ABAP_BOOL optional
      value(UI_FUNC) type UI_FUNCTIONS optional .
  methods GET_UI_FUNC
    returning
      value(UI_FUNC) type UI_FUNCTIONS .
  methods GET_GRID
    returning
      value(GRID) type ref to ZCL_GUI_ALV_GRID .
  methods SET_INTERVAL
    importing
      value(INTERVAL) type I .
  methods GET_INTERVAL
    returning
      value(INTERVAL) type I .
  methods GET_LOG
    returning
      value(LOG) type ref to ZCL_LOG .
  methods GET_OUTTAB
    returning
      value(OUTTAB) type ref to DATA .
  methods REFRESH .
  methods ADD_BUTTON
    importing
      !FUN_CODE type UI_FUNC
      !BTN_TYPE type TB_BTYPE
      !ICON type ANY
      !TEXT type TEXT40 optional
      !QUICKINFO type ICONQUICK optional
      !OBJECT type ref to CL_ALV_EVENT_TOOLBAR_SET .
  methods SET_HEADER_DOCUMENT
    importing
      value(DOC) type ref to CL_DD_DOCUMENT .
  methods GET_HEADER_DOCUMENT
    returning
      value(DOC) type ref to CL_DD_DOCUMENT .
  methods DISPLAY
  abstract .
  methods PAI
    changing
      !UCOMM type SY-UCOMM .
  methods PBO .
  methods INIT_STYLE
    importing
      !EDIT type ABAP_BOOL optional .
  methods ADD_LINE_STYLE
    importing
      !FIELDNAME type LVC_S_STYL-FIELDNAME optional
      !STYLE type LVC_STYLE
      !STYLE2 type LVC_STYLE optional
      !STYLE3 type LVC_STYLE optional
      !STYLE4 type LVC_STYLE optional
      !MAXLEN type INT4 optional
    returning
      value(TALV) type ref to ZCL_TALV_PARENT .
  methods SET_STYLE_FOR_ALL_LINES .
  methods SET_STYLE_FOR_SINGLE_LINE
    importing
      !INDEX type I .
  methods GET_STYLE_TABLE
    returning
      value(STYLE_TABLE) type LVC_T_STYL .
  methods SET_STYLE_TABLE
    importing
      !STYLE_TABLE type LVC_T_STYL .
  methods CLEAR_LINE_STYLE_TABLE .
  methods INIT_COLOR
    importing
      !COLOR type LVC_COL .
  methods ADD_LINE_COLOR
    importing
      !FIELDNAME type LVC_S_STYL-FIELDNAME optional
      !COL type LVC_COL
      !INT type LVC_INT optional
      !INV type LVC_INV optional
      !NOKEYCOL type LVC_NOKEYC optional
    returning
      value(TALV) type ref to ZCL_TALV_PARENT .
  methods SET_COLOR_FOR_ALL_LINES .
  methods SET_COLOR_FOR_SINGLE_LINE
    importing
      !INDEX type I .
  methods GET_COLOR_TABLE
    returning
      value(COLOR_TABLE) type LVC_T_SCOL .
  methods SET_COLOR_TABLE
    importing
      !COLOR_TABLE type LVC_T_SCOL .
  methods CLEAR_LINE_COLOR_TABLE .
  methods INIT_FIELDCAT .
  methods IS_INITIALIZED
    returning
      value(INITIALIZED) type ABAP_BOOL .
  methods ON_RETRIEVE .
  methods FREE .
  PROTECTED SECTION.

    DATA key TYPE zstalv_key .
    DATA container TYPE REF TO cl_gui_container .
    DATA header_container TYPE REF TO cl_gui_container .
    DATA header_document TYPE REF TO cl_dd_document .
    DATA log TYPE REF TO zcl_log .
    DATA timer TYPE REF TO cl_gui_timer .
    DATA grid TYPE REF TO zcl_gui_alv_grid .
private section.

  data INITIALIZED type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data STYLE_TABLE type LVC_T_STYL .
  data COLOR_TABLE type LVC_T_SCOL .
  data INTERCEPT_UCOMM type TTY_INTERCEPT_UCOMM .
  data EVENT_HANDLER type ref to ZIF_TALV_EVENT_HANDLE_IMP .
  data FIELDS_SETTING type TTY_FIELDS_SETTING .
  data LAYOUT_SETTING type TTY_LAYOUT_SETTING .

  methods SET_BTN_STYLE_FOR_ALL_LINE .
  methods SET_BTN_STYLE_FOR_SINGLE_LINE
    changing
      !ALV_LINE type ANY .
  methods CREATE_TABLE .
  methods COPY_TABLE .
  methods CHECK_KEY_INFO .
  methods INIT_EVENT_PREFIX .
  methods INIT .
  methods INIT_GRID .
  methods INIT_EVENT .
  methods INIT_DATA .
  methods SET_KEY_INFO
    importing
      !FIELDNAME type FIELDNAME
      !FIELDVALUE type FIELDVALUE .
  methods HANDLE_LONG_TEXT_FIELD
    importing
      !FIELDNAME type FIELDNAME
      !ROW type INT4
    changing
      !VALUE type C .
ENDCLASS.



CLASS ZCL_TALV_PARENT IMPLEMENTATION.


  METHOD remove_fields.

    CHECK fields IS NOT INITIAL.

    DATA: fieldnames TYPE tty_fieldname,
          fieldname  LIKE LINE OF fieldnames.

    TRANSLATE fields TO UPPER CASE.
    SPLIT fields AT space INTO TABLE fieldnames.

    get_fieldcat( ).

    LOOP AT fieldnames INTO fieldname.
      DELETE key-fieldcat WHERE fieldname = fieldname-fieldname.
    ENDLOOP.

    set_fieldcat( key-fieldcat ).

  ENDMETHOD.


  METHOD save_fields.

    CHECK fields IS NOT INITIAL.

    DATA: fieldnames TYPE tty_fieldname,
          fieldname  LIKE LINE OF fieldnames.

    TRANSLATE fields TO UPPER CASE.
    SPLIT fields AT space INTO TABLE fieldnames.

    DATA: fieldcats_old TYPE lvc_t_fcat,
          fieldcat_old  LIKE LINE OF fieldcats_old.

    fieldcats_old = get_fieldcat( ).

    CLEAR key-fieldcat.

    LOOP AT fieldnames INTO fieldname.

      READ TABLE fieldcats_old INTO fieldcat_old WITH KEY fieldname = fieldname-fieldname.
      IF sy-subrc = 0.
        APPEND fieldcat_old TO key-fieldcat.
        CLEAR: fieldcat_old.
      ENDIF.

    ENDLOOP.

    set_fieldcat( key-fieldcat ).

  ENDMETHOD.


  METHOD set_btn_style_for_all_line.

    CHECK key-show_long_text_button = abap_true.

    FIELD-SYMBOLS: <table>      TYPE STANDARD TABLE,
                   <table_line> TYPE any.
    ASSIGN grid->outtab->* TO <table>.

    LOOP AT <table> ASSIGNING <table_line>.
      set_btn_style_for_single_line( CHANGING alv_line = <table_line> ).
    ENDLOOP.

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
      style_line-style     = zcl_gui_alv_grid=>mc_style_button.
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
      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE' ##FM_SUBRC_OK
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


  METHOD set_header_document.
    header_document = doc.
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

      key-layout = layout.

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

    ENDIF.

    grid->set_frontend_layout( key-layout ).

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

      APPEND zcl_gui_alv_grid=>mc_fc_excl_all TO key-ui_func.

    ELSEIF ui_func IS NOT INITIAL.

      key-ui_func = ui_func.

    ELSE.

      IF key-layout-edit <> abap_true.
        APPEND zcl_gui_alv_grid=>mc_fc_loc_append_row  TO key-ui_func.
        APPEND zcl_gui_alv_grid=>mc_fc_loc_delete_row  TO key-ui_func.
      ENDIF.
      "不想使用这里的默认排除，就自己通过key传入ui_func
      APPEND zcl_gui_alv_grid=>mc_fc_loc_insert_row    TO key-ui_func.
      APPEND zcl_gui_alv_grid=>mc_fc_loc_cut           TO key-ui_func.
      APPEND zcl_gui_alv_grid=>mc_fc_loc_paste         TO key-ui_func.
      APPEND zcl_gui_alv_grid=>mc_fc_loc_paste_new_row TO key-ui_func.
      APPEND zcl_gui_alv_grid=>mc_fc_loc_copy_row      TO key-ui_func.
      APPEND zcl_gui_alv_grid=>mc_fc_loc_undo          TO key-ui_func.

    ENDIF.

  ENDMETHOD.


  METHOD set_variant.

    IF variant IS NOT INITIAL.

      key-variant = variant.
      grid->set_variant( variant ).

    ELSE.

      CLEAR: key-variant.
      "使用 报表名 + 屏幕编号 + 容器ID 就可以给每一个ALV设定属于它自己的变式
      key-variant-report = key-program && key-dynnr && key-container_name  && key-container_position.
      key-variant-handle   = '0001'.
      key-variant-username = sy-uname.

      grid->set_variant( key-variant ).

    ENDIF.

  ENDMETHOD.


  METHOD on_retrieve.

    CHECK event_handler IS BOUND.

    event_handler->on_retrieve( ).

  ENDMETHOD.


  METHOD refresh.

    set_btn_style_for_all_line( ).

    key-layout-cwidth_opt = abap_true.
    set_layout( key-layout ).

    "刷新变量
    DATA: stable TYPE lvc_s_stbl.

    "刷新显示
    CLEAR stable.

    stable-row = 'X'."基于行刷新
    stable-col = 'X'."基于列刷新

    grid->refresh_table_display( is_stable = stable i_soft_refresh = abap_true ).


    IF timer IS BOUND.
      timer->run( ).
    ENDIF.

  ENDMETHOD.


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

    key-program = sy-cprog."报表程序里是这个，其它的不一定

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

    get_layout_setting( ).

    get_fields_setting( ).

  ENDMETHOD.


  METHOD copy_table.

    DATA dynamictor TYPE REF TO zcl_dynamic_tool.
    dynamictor ?= zcl_dynamic_tool=>get_instance( ).

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

    "数据传递
    FIELD-SYMBOLS <alv_table> TYPE STANDARD TABLE.
    ASSIGN grid->outtab->* TO <alv_table>.

    MOVE-CORRESPONDING <ref_table> TO <alv_table>.

  ENDMETHOD.


  METHOD create_table.

    CHECK grid->outtab IS NOT BOUND.

    IF key-style_table_name IS INITIAL
      AND key-ref_table_name IS INITIAL
      AND key-ref_data_name IS INITIAL
      AND key-checkbox_name IS INITIAL.
      CREATE DATA grid->outtab TYPE STANDARD TABLE OF (key-ddic_type).
    ELSE.

      grid->outtab = zcl_dynamic_tool=>create_dynamic_table_by_rttc( talv_key = key ).

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

    IF key-header_height > 0.

      header_container->free( ).
      FREE header_document.

    ENDIF.

    IF container IS BOUND.
      container->free( ).
    ENDIF.

    CLEAR: color_table,
           style_table.

    FREE: timer,
          log,
          grid,
          header_container,
          container.

    DATA dynnr(4) TYPE n VALUE IS INITIAL.
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
    key-fieldcat = fieldcat.
  ENDMETHOD.


  METHOD get_grid.
    grid = me->grid.
  ENDMETHOD.


  METHOD get_header_document.
    doc = header_document.
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
    CHECK NOT fieldname = 'CONTAINER'.

    ASSIGN COMPONENT fieldname OF STRUCTURE key TO <field>.
    IF <field> IS ASSIGNED.
      fieldvalue = <field>.
    ENDIF.

  ENDMETHOD.


  METHOD get_layout.
    grid->get_frontend_layout( IMPORTING es_layout = layout  ).
    key-layout = layout.
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

    DATA title TYPE sy-title.
    IF fieldcat-coltext IS NOT INITIAL.
      title = fieldcat-coltext.
    ELSE.
      title = fieldcat-scrtext_l.
    ENDIF.

    DATA text TYPE string.
    text = value.

    CALL FUNCTION 'ZFM_EDIT_LONG_TEXT_FIELD'
      EXPORTING
        i_title             = title
        i_editable          = fieldcat-edit
        i_field_length      = fieldcat-intlen
        i_wordwrap_position = 132
      CHANGING
        c_text              = text.

    value = text.

    RAISE EVENT check_long_text
      EXPORTING
        fieldname = fieldname
        row       = row
        long_text = text.

  ENDMETHOD.


  METHOD init.

    CHECK is_initialized( ) = abap_false.

    init_grid( ).

    init_event( ).

    set_variant( key-variant ).

    set_layout( key-layout ).

    set_ui_func( remove_all = key-no_ui
                 ui_func    = key-ui_func ).

    init_data( ).

    grid->ready_for_it( ).

    IF key-header_height > 0.
      grid->list_processing_events( i_event_name = 'TOP_OF_PAGE' i_dyndoc_id = header_document ).
    ENDIF.

    initialized = abap_true.

  ENDMETHOD.


  METHOD init_color.

    clear_line_color_table( ).
    add_line_color( color ).
    set_color_for_all_lines( ).

  ENDMETHOD.


  METHOD init_data.

    IF key-ref_table_name IS NOT INITIAL OR key-ref_data_name IS NOT INITIAL.

      "直接把程序里的表或者数据引用拿过来，先改造内表字段结构，再传递数据
      init_fieldcat( ).

      copy_table( ).

    ELSE.

      "直接创建ddic_type类型的内表，同时抛出事件，自行取值
      set_fieldcat( structure_name = key-ddic_type
                    fieldcat       = key-fieldcat ).

      init_fieldcat( ).

      create_table( ).

      RAISE EVENT retrieve.

    ENDIF.

  ENDMETHOD.


  METHOD init_event.

    "不设置时也默认为1
    grid->set_ready_for_input( 1 ).

    "check data changed
    grid->check_changed_data( ).

    "单元格更改触发
    grid->register_edit_event( grid->mc_evt_modified ).

    IF key-event_handler IS NOT BOUND.
      CREATE OBJECT event_handler TYPE zcl_talv_event_handler
        EXPORTING
          talv = me.
    ELSE.
      event_handler = key-event_handler.
    ENDIF.

    SET HANDLER event_handler->on_handle_toolbar           FOR grid.
    SET HANDLER event_handler->on_handle_user_command      FOR grid.
    SET HANDLER event_handler->on_handle_hotspot_click     FOR grid.
    SET HANDLER event_handler->on_handle_double_click      FOR grid.
    SET HANDLER event_handler->on_handle_data_changed      FOR grid.
    SET HANDLER event_handler->on_handle_changed_finished  FOR grid.
    SET HANDLER event_handler->on_handle_grid_dispatch     FOR grid.
    SET HANDLER event_handler->on_handle_line_button_click FOR grid.
    SET HANDLER event_handler->on_handle_top_of_page       FOR grid.

    SET HANDLER event_handler->on_set_pf_status   FOR me.
    SET HANDLER event_handler->on_set_title       FOR me.
    SET HANDLER event_handler->on_pbo             FOR me.
    SET HANDLER event_handler->on_pai_command     FOR me.
    SET HANDLER event_handler->on_exit            FOR me.
    SET HANDLER event_handler->on_check_long_text FOR me.

    SET HANDLER event_handler->on_retrieve      FOR me.

    IF key-log_object IS NOT INITIAL AND key-log_subobject IS NOT INITIAL.

      DATA: keyinfo TYPE balnrext.
      keyinfo = key-program && key-dynnr && key-container_name.

      log = zcl_log=>get_instance( object    = key-log_object
                                   subobject = key-log_subobject
                                   msg_ext   = keyinfo ).
    ENDIF.

    IF key-interval > 0.

      CREATE OBJECT timer.

      SET HANDLER event_handler->on_handle_countdown_finished FOR timer.

      set_interval( key-interval ).

    ENDIF.

  ENDMETHOD.


  METHOD init_event_prefix.

    IF key-container_position <> '00'.
      key-frm_prefix = 'F' && key-dynnr && '_' && key-container_position && '_'."9位
    ELSE.
      key-frm_prefix = 'FRM_' && key-dynnr && '_'."9位
    ENDIF.

    IF key-handle_toolbar IS INITIAL.
      key-handle_toolbar = key-frm_prefix                 && 'HANDLE_TOOLBAR'.
    ENDIF.

    IF key-handle_user_command IS INITIAL.
      key-handle_user_command = key-frm_prefix            && 'HANDLE_USER_COMMAND'.
    ENDIF.

    IF key-handle_hotspot_click IS INITIAL.
      key-handle_hotspot_click = key-frm_prefix           && 'HANDLE_HOTSPOT_CLICK'.
    ENDIF.

    IF key-handle_double_click IS INITIAL.
      key-handle_double_click = key-frm_prefix            && 'HANDLE_DOUBLE_CLICK'.
    ENDIF.

    IF key-handle_data_changed IS INITIAL.
      key-handle_data_changed = key-frm_prefix            && 'HANDLE_DATA_CHANGED'.
    ENDIF.

    IF key-handle_data_changed_finished IS INITIAL.
      key-handle_data_changed_finished = key-frm_prefix   && 'HANDLE_CHANGED_OVER'.
    ENDIF.

    IF key-handle_countdown_finished IS INITIAL.
      key-handle_countdown_finished = key-frm_prefix      && 'HANDLE_COUNTDOWN'.
    ENDIF.

    IF key-handle_set_pf_status IS INITIAL.
      key-handle_set_pf_status = key-frm_prefix           && 'HANDLE_SET_PF_STATUS'.
    ENDIF.

    IF key-handle_set_title IS INITIAL.
      key-handle_set_title = key-frm_prefix               && 'HANDLE_SET_TITLE'.
    ENDIF.

    IF key-handle_on_pbo IS INITIAL.
      key-handle_on_pbo = key-frm_prefix                  && 'HANDLE_ON_PBO'.
    ENDIF.

    IF key-handle_pai_command IS INITIAL.
      key-handle_pai_command = key-frm_prefix             && 'HANDLE_PAI_COMMAND'.
    ENDIF.

    IF key-handle_exit IS INITIAL.
      key-handle_exit = key-frm_prefix                    && 'HANDLE_EXIT'.
    ENDIF.

    IF key-handle_retrieve IS INITIAL.
      key-handle_retrieve = key-frm_prefix                && 'HANDLE_RETRIEVE'.
    ENDIF.

    IF key-handle_grid_dispatch IS INITIAL.
      key-handle_grid_dispatch = key-frm_prefix           && 'HANDLE_GRID_DISPATCH'.
    ENDIF.

    IF key-handle_line_btn_click IS INITIAL.
      key-handle_line_btn_click = key-frm_prefix          && 'HANDLE_LINE_BTN_CLK'.
    ENDIF.

    IF key-handle_top_of_page IS INITIAL.
      key-handle_top_of_page = key-frm_prefix             && 'HANDLE_TOP_OF_PAGE'.
    ENDIF.

    IF key-handle_check_long_text IS INITIAL.
      key-handle_check_long_text = key-frm_prefix             && 'CHECK_LONG_TEXT'.
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


  METHOD init_grid.

    IF key-container IS NOT BOUND.
      CREATE OBJECT container TYPE cl_gui_custom_container
        EXPORTING
          container_name = key-container_name.
    ELSE.
      container = key-container.
    ENDIF.

    IF key-header_height > 0.

      DATA splitter TYPE REF TO cl_gui_splitter_container.

      CREATE OBJECT splitter
        EXPORTING
          parent  = container
          rows    = 2
          columns = 1.

      splitter->set_row_height( id = 1 height = CONV i( key-header_height ) ).

      header_container = splitter->get_container( row = 1 column = 1 ).

      CREATE OBJECT grid
        EXPORTING
          talv     = me
          i_parent = splitter->get_container( row = 2 column = 1 ).

      CREATE OBJECT header_document
        EXPORTING
          style = 'ALV_GRID'.

    ELSE.
      CREATE OBJECT grid
        EXPORTING
          talv     = me
          i_parent = container.
    ENDIF.


  ENDMETHOD.


  METHOD init_style.

    clear_line_style_table( ).

    IF edit = abap_true.
      add_line_style( zcl_gui_alv_grid=>mc_style_enabled ).
    ELSE.
      add_line_style( zcl_gui_alv_grid=>mc_style_disabled ).

      IF key-checkbox_name <> space.
        add_line_style( fieldname = key-checkbox_name
                        style     = zcl_gui_alv_grid=>mc_style_enabled ).
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

    SET PF-STATUS 'STATUS' OF PROGRAM 'SAPLZFUNG_TALV'.

    SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH '这' '是' '个' '标' '题'.

    init( ).

    RAISE EVENT set_title.
    RAISE EVENT set_pf_status.
    RAISE EVENT on_pbo.

  ENDMETHOD.


  METHOD get_fields_setting.

    CHECK key-no_read_fcat_setting = abap_false.

    SELECT *
      FROM ztalv_fields_set
      INTO CORRESPONDING FIELDS OF TABLE me->fields_setting
     WHERE cprog              = key-program
       AND dynnr              = key-dynnr
*       AND ddic_type          = key-ddic_type
       AND container_position = key-container_position.

    fields_setting = me->fields_setting.

  ENDMETHOD.


  METHOD get_layout_setting.

    CHECK key-no_read_layo_setting = abap_false.

    SELECT *
      FROM ztalv_layout_set
      INTO CORRESPONDING FIELDS OF TABLE me->layout_setting
     WHERE cprog              = key-program
       AND dynnr              = key-dynnr
       AND container_position = key-container_position
       AND obj_id             = '0'.

    layout_setting = me->layout_setting.

  ENDMETHOD.
ENDCLASS.
