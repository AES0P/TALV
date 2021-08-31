*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_01
*&---------------------------------------------------------------------*
*&  simple to display
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_01.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS main.

ENDCLASS.


START-OF-SELECTION.
  lcl_this=>main( ).

*----------------------------------------------------------------------*
* 类实施
*----------------------------------------------------------------------*
CLASS lcl_this IMPLEMENTATION.

  METHOD main.

    DATA: key TYPE zstalv_key.

    "ALV类型：全屏
    key-type          = 'TALV'.
    "数据字典类型 可动态指定
    key-ddic_type     = 'RSAU_BUF_DATA'.

    "工厂模式生成TALV并直接展示
    zcl_talv_factory=>get_talv( key )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9000_handle_retrieve USING ddic_type TYPE tabname
                           CHANGING alv_table TYPE STANDARD TABLE.

  SELECT *
    FROM (ddic_type)
    INTO CORRESPONDING FIELDS OF TABLE alv_table
    UP TO 10 ROWS.

ENDFORM.
