*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_01
*&---------------------------------------------------------------------*
*&  TALV超长字段（>128)维护示例
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_09.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-DATA: ddic TYPE tabname VALUE '/ATL/IC_ACT_ASST'.

    CLASS-METHODS main.

ENDCLASS.


START-OF-SELECTION.
  lcl_this=>main( ).

*----------------------------------------------------------------------*
* 类实施
*----------------------------------------------------------------------*
CLASS lcl_this IMPLEMENTATION.

  METHOD main.

    "工厂模式生成TALV并直接展示
    zcl_talv_factory=>get_talv( VALUE #(  type                  = 'TALV'
                                          ddic_type             = lcl_this=>ddic
                                          ui_func               = VALUE #( ( zcl_gui_alv_grid=>mc_fc_loc_undo ) )
                                          show_long_text_button = abap_true
                                          style_table_name      = 'STYLE'  ) )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9000_handle_on_pbo USING talv      TYPE REF TO zcl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE.

  DATA(fcat) = talv->get_fieldcat( ).
  "这里控制长文本是否可以编辑
  fcat[ fieldname = 'ITEM_DESCRIPTION' ]-edit = abap_true.
  talv->set_fieldcat( fcat ).

  talv->refresh( ).

ENDFORM.

FORM frm_9000_handle_retrieve USING ddic_type TYPE tabname
                           CHANGING alv_table TYPE STANDARD TABLE.

  SELECT *
    FROM (ddic_type)
    INTO CORRESPONDING FIELDS OF TABLE alv_table
    UP TO 10 ROWS.

ENDFORM.
