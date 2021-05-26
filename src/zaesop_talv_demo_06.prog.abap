*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_06
*&---------------------------------------------------------------------*
*&  拦截grid对标准工具栏的功能响应
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_06.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

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

    "要拦截的标准功能
    key-intercept_ucomms = VALUE #(
                                   ( ycl_gui_alv_grid=>mc_fc_refresh )
                                   ( ycl_gui_alv_grid=>mc_fc_sort_asc )
                                   ( ycl_gui_alv_grid=>mc_fc_sort_dsc )
                                   ).

    "工厂模式生成TALV并直接展示
    ycl_talv_factory=>get_talv( key )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9000_handle_on_pbo USING talv TYPE REF TO ycl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE.

  talv->save_fields( 'BOX EBELN BUKRS BSTYP BSART LOEKZ AEDAT ERNAM' ).

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

FORM frm_9000_handle_grid_dispatch USING talv TYPE REF TO ycl_talv_parent
                                         action   TYPE sy-ucomm
                                CHANGING alv_table TYPE STANDARD TABLE.
  "重写拦截功能
  CASE action.
    WHEN ycl_gui_alv_grid=>mc_fc_refresh.
      MESSAGE 'MC_FC_REFRESH' TYPE 'S'.
      talv->on_retrieve( ).
    WHEN ycl_gui_alv_grid=>mc_fc_sort_asc.
      MESSAGE 'MC_FC_SORT_ASC' TYPE 'S'.
      SORT alv_table ASCENDING BY ('AEDAT').
    WHEN ycl_gui_alv_grid=>mc_fc_sort_dsc.
      MESSAGE 'MC_FC_SORT_DSC' TYPE 'S'.
      SORT alv_table DESCENDING BY ('AEDAT')..
  ENDCASE.

  talv->refresh( ).

ENDFORM.
