*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_07
*&---------------------------------------------------------------------*
*& 本程序示例如何将一个普通内表&数据引用转换成TALV并展示
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_07.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_itab,
             ebeln TYPE ekpo-ebeln,
             ebelp TYPE ekpo-ebelp,
             matnr TYPE ekpo-matnr,
             menge TYPE ekpo-menge,
             bprme TYPE ekpo-bprme,
             meins TYPE ekpo-meins,
             netpr TYPE netpr,
*             netpr(7) TYPE p DECIMALS 4,"只支持数据字典里定义过的，这种不标准的不支持
           END OF ty_itab.

    TYPES tty_itab TYPE STANDARD TABLE OF ty_itab WITH DEFAULT KEY.

    CLASS-DATA po_lines TYPE tty_itab.
    CLASS-DATA po_data  TYPE REF TO tty_itab.

    CLASS-METHODS main.
    CLASS-METHODS show_itab_data.
    CLASS-METHODS show_reftab_data.

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

    show_itab_data( ).
    show_reftab_data( ).

    CLEAR po_lines.

  ENDMETHOD.

  METHOD show_itab_data.

    SELECT *
      FROM ekpo
      INTO CORRESPONDING FIELDS OF TABLE po_lines
      UP TO 20 ROWS.

    "从普通表copy数据到TALV进行展示和处理
    ycl_talv_factory=>get_talv( VALUE #(  type             = 'TALV_POPUP'
                                          ref_table_name   = 'LCL_THIS=>PO_LINES'"参考内表
                                          checkbox_name    = 'BOX'
                                          light_name       = 'LIGHT'
                                          color_table_name = 'COLOR'
                                          style_table_name = 'STYLE' )
                              )->display( ).

  ENDMETHOD.

  METHOD show_reftab_data.

    DATA fieldcat TYPE lvc_t_fcat.

    fieldcat = VALUE #( ( fieldname = 'EBELN' ref_table = 'EKPO' ref_field = 'EBELN' )
                        ( fieldname = 'EBELP' ref_table = 'EKPO' ref_field = 'EBELP' )
                        ( fieldname = 'MATNR' ref_table = 'EKPO' ref_field = 'MATNR' )
                        ( fieldname = 'MENGE' ref_table = 'EKPO' ref_field = 'MENGE' )
                        ( fieldname = 'BPRME' ref_table = 'EKPO' ref_field = 'BPRME' )
                        ( fieldname = 'MEINS' ref_table = 'EKPO' ref_field = 'MEINS' )
                        ( fieldname = 'NETPR' ref_table = 'EKPO' ref_field = 'NETPR' ) ).

    CREATE DATA po_data.
    ASSIGN po_data->* TO FIELD-SYMBOL(<po_data>).
    MOVE-CORRESPONDING po_lines TO <po_data>.

    ""从参考对象copy数据到TALV进行展示和处理
    ycl_talv_factory=>get_talv( VALUE #(  type                         = 'TALV'
                                          ref_data_name                = 'LCL_THIS=>PO_DATA'"参考引用
                                          fieldcat                     = fieldcat"引用必须传入fieldcat
                                          handle_data_changed_finished = 'FRM_9900_HANDLE_CHANGED_OVER'
                                          checkbox_name                = 'BOX'
                                          light_name                   = 'LIGHT'
                                          color_table_name             = 'COLOR'
                                          style_table_name             = 'STYLE' )
                              )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9900_handle_set_title.
  SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH '内表转TALV'.
ENDFORM.

FORM frm_9000_handle_set_title.
  SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH '数据引用转TALV'.
ENDFORM.

FORM frm_9900_handle_changed_over USING talv TYPE REF TO ycl_talv_parent
                                        pv_modified   TYPE char01
                                        pt_good_cells TYPE lvc_t_modi
                               CHANGING alv_table TYPE STANDARD TABLE.

  READ TABLE pt_good_cells INTO DATA(cell) INDEX 1.

  LOOP AT alv_table ASSIGNING FIELD-SYMBOL(<table_line>).

    DATA(index) = sy-tabix.

    "设置style color前先清空一下，防止被别的地方设置的在此错误应用
    talv->clear_line_style_table( ).
    talv->clear_line_color_table( ).

    ASSIGN COMPONENT 'BOX'   OF STRUCTURE <table_line> TO FIELD-SYMBOL(<box>).
    ASSIGN COMPONENT 'LIGHT' OF STRUCTURE <table_line> TO FIELD-SYMBOL(<light>).

    IF <box> <> abap_true.

      <light> = icon_led_red.

      talv->add_line_style( style = ycl_gui_alv_grid=>mc_style_disabled ).
      talv->add_line_color( 3 ).

    ELSE.

      <light>    = icon_led_green.

      talv->add_line_style( style     = ycl_gui_alv_grid=>mc_style_disabled ).
      talv->add_line_style( fieldname = 'MENGE'"指定列名则只对该列生效，不指定则全部列生效
                            style     = ycl_gui_alv_grid=>mc_style_enabled
                            style2    = ycl_gui_alv_grid=>mc_style_hotspot
                            style4    = ycl_gui_alv_grid=>mc_style4_link ).

      talv->add_line_color( 6 ).
      talv->add_line_color( fieldname = 'MENGE' col = 5 ).

    ENDIF.

    talv->add_line_style( fieldname = CONV fieldname( talv->get_key_info( 'CHECKBOX_NAME' ) )"不冻结选择框
                          style     = ycl_gui_alv_grid=>mc_style_enabled ).

    "针对指定行修改
    talv->set_style_for_single_line( index ).
    talv->set_color_for_single_line( index ).

  ENDLOOP.

  "从ALV回写数据到普通表
  MOVE-CORRESPONDING alv_table TO lcl_this=>po_lines.

  talv->refresh( ).

ENDFORM.
