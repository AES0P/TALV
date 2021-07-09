*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_05
*&---------------------------------------------------------------------*
*&  cell color / style & checkbox & light
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_05.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    "定义此结构是为了方便读者理解本例中系统会构造出什么样的内表
    TYPES: BEGIN OF ty_ekko.
    TYPES: box(1)   TYPE c,
           light(4) TYPE c,
           color    TYPE lvc_t_scol,
           style    TYPE lvc_t_styl.
           INCLUDE  TYPE ekko.
    TYPES: END OF ty_ekko.

    TYPES tty_ekko TYPE STANDARD TABLE OF ty_ekko WITH DEFAULT KEY.

    CLASS-DATA: ddic         TYPE tabname VALUE 'EKKO',
                fields(1024) TYPE c VALUE 'EBELN BUKRS BSTYP BSART LOEKZ AEDAT ERNAM'.

    CLASS-METHODS main.

ENDCLASS.

*----------------------------------------------------------------------*
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_this=>main( ).

*----------------------------------------------------------------------*
* 类实施
*----------------------------------------------------------------------*
CLASS lcl_this IMPLEMENTATION.

  METHOD main.

    DATA: key TYPE zstalv_key.

    "ALV类型：全屏
    key-type             = 'TALV'.
    "数据字典类型 可动态指定
    key-ddic_type        = lcl_this=>ddic.
    "ALV内表checkbox_name列的列名
    key-checkbox_name    = 'BOX'.
    "ALV内表信号灯列的列名
    key-light_name       = 'LIGHT'.
    "ALV内表单元格颜色内表的列名
    key-color_table_name = 'COLOR'.
    "ALV内表style控制列的列名
    key-style_table_name = 'STYLE'.

    "工厂模式生成TALV并直接展示
    zcl_talv_factory=>get_talv( key )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9000_handle_on_pbo USING talv TYPE REF TO zcl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE.

  talv->save_fields( 'BOX LIGHT EBELN BUKRS BSTYP BSART LOEKZ AEDAT ERNAM' ).

  PERFORM frm_9000_set_style USING talv
                          CHANGING alv_table.

  talv->refresh( ).

ENDFORM.

FORM frm_9000_handle_retrieve USING pv_ddic_type TYPE tabname
                           CHANGING alv_table TYPE STANDARD TABLE.

  IF lcl_this=>fields IS NOT INITIAL.
    SELECT (lcl_this=>fields)
      FROM (pv_ddic_type)
      INTO CORRESPONDING FIELDS OF TABLE alv_table
      UP TO 10 ROWS.
  ELSE.
    SELECT *
      FROM (pv_ddic_type)
      INTO CORRESPONDING FIELDS OF TABLE alv_table
      UP TO 10 ROWS.
  ENDIF.

ENDFORM.

FORM frm_9000_handle_changed_over USING talv TYPE REF TO zcl_talv_parent
                                        pv_modified   TYPE char01
                                        pt_good_cells TYPE lvc_t_modi
                               CHANGING alv_table TYPE STANDARD TABLE.

  PERFORM frm_9000_set_style USING talv
                          CHANGING alv_table.

  talv->refresh( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form frm_9000_set_style
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_9000_set_style  USING talv TYPE REF TO zcl_talv_parent
                      CHANGING alv_table TYPE STANDARD TABLE.

  FIELD-SYMBOLS <table> TYPE lcl_this=>tty_ekko.
  ASSIGN alv_table TO <table>.

  LOOP AT <table> ASSIGNING FIELD-SYMBOL(<table_line>).

    DATA(index) = sy-tabix.

    "设置style color前先清空一下，防止被别的地方设置的在此错误应用
    talv->clear_line_style_table( ).
    talv->clear_line_color_table( ).

    IF <table_line>-box <> abap_true.

      <table_line>-light = icon_led_red.

      talv->add_line_style( style = zcl_gui_alv_grid=>mc_style_disabled
         )->add_line_color( 3 ).

    ELSE.

      <table_line>-light    = icon_led_green.

      talv->add_line_style( style     = zcl_gui_alv_grid=>mc_style_disabled
         )->add_line_style( fieldname = 'BUKRS'"指定列名则只对该列生效，不指定则全部列生效
                            style     = zcl_gui_alv_grid=>mc_style_enabled
                            style2    = zcl_gui_alv_grid=>mc_style_hotspot
                            style4    = zcl_gui_alv_grid=>mc_style4_link ).

      talv->add_line_color( fieldname = 'BUKRS' col = 6
         )->add_line_color( fieldname = 'BUKRS' col = 5 ).

    ENDIF.

    talv->add_line_style( fieldname = CONV fieldname( talv->get_key_info( 'CHECKBOX_NAME' ) )"不冻结选择框
                          style     = zcl_gui_alv_grid=>mc_style_enabled ).

    "针对指定行修改
    talv->set_style_for_single_line( index ).
    talv->set_color_for_single_line( index ).

  ENDLOOP.

ENDFORM.
