*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_02
*&---------------------------------------------------------------------*
*&  TALV框架常用功能示例
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_02.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES ty_ekko  TYPE ekko.
    TYPES tty_ekko TYPE STANDARD TABLE OF ty_ekko WITH DEFAULT KEY.

    CLASS-METHODS main.

ENDCLASS.

*----------------------------------------------------------------------*
* 选择屏幕
*----------------------------------------------------------------------*
PARAMETERS: p_ddic         TYPE tabname DEFAULT 'EKKO' OBLIGATORY,
            p_fields(1024) TYPE c DEFAULT 'EBELN BUKRS BSTYP BSART LOEKZ AEDAT ERNAM'.

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
    key-type          = 'TALV'.
    "数据字典类型 可动态指定
    key-ddic_type     = p_ddic.
    "ALV自动刷新时间间隔 单位 S（秒） 不传值则不自动刷新
    key-interval      = '10'.

    key-header_height = 10."设置高度才能显示抬头文本

    "工厂模式生成TALV并直接展示
    zcl_talv_factory=>get_talv( key )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9000_handle_set_pf_status.
  SET PF-STATUS 'STATUS' OF PROGRAM 'SAPLZFUNG_TALV'.
ENDFORM.

FORM frm_9000_handle_set_title.
  SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH '抬头' '明细'.
ENDFORM.

FORM frm_9000_handle_on_pbo USING po_talv      TYPE REF TO zcl_talv_parent
                         CHANGING ct_alv_table TYPE STANDARD TABLE.

  DATA(fieldcat) = po_talv->get_fieldcat( )."生成TALV后仍可修改其字段目录 及每个字段的属性

  IF line_exists( fieldcat[ fieldname = 'EBELN' ] ).
    fieldcat[ fieldname = 'EBELN' ]-key     = abap_true.
    fieldcat[ fieldname = 'EBELN' ]-hotspot = abap_true.
  ENDIF.

  IF line_exists( fieldcat[ fieldname = 'MBLNR' ] ).
    fieldcat[ fieldname = 'MBLNR' ]-key     = abap_true.
    fieldcat[ fieldname = 'MBLNR' ]-hotspot = abap_true.
  ENDIF.

  IF line_exists( fieldcat[ fieldname = 'BSTYP' ] ).

    fieldcat[ fieldname = 'BSTYP' ]-drdn_hndl = '1'.

    DATA: drops TYPE lvc_t_drop.
    APPEND VALUE lvc_s_drop(  handle = 1 value = 'A'  ) TO drops.
    APPEND VALUE lvc_s_drop(  handle = 1 value = 'D'  ) TO drops.
    APPEND VALUE lvc_s_drop(  handle = 1 value = 'F'  ) TO drops.

    DATA: drals TYPE lvc_t_dral.
    APPEND VALUE lvc_s_dral(  handle = 1 value = 'B' int_value = 'AAAA' ) TO drals.
    APPEND VALUE lvc_s_dral(  handle = 1 value = 'C' int_value = 'DDDD' ) TO drals.
    APPEND VALUE lvc_s_dral(  handle = 1 value = 'E' int_value = 'FFFF' ) TO drals.

    po_talv->get_grid( )->set_drop_down_table(
                         it_drop_down       = drops
                         it_drop_down_alias = drals ).

  ENDIF.


  po_talv->set_fieldcat( fieldcat )."修改字段目录 可传入字段目录也可传入DDIC名称

  po_talv->save_fields( p_fields )."最终要保留的字段 以空格间隔
  po_talv->remove_fields( 'ERNAM' )."要去除的字段

  DATA(layout) = po_talv->get_layout( )."生成TALV后仍可修改其布局设置属性
  layout-edit  = abap_true.
  po_talv->set_layout( layout )."修改布局设置属性

  po_talv->refresh( ).

ENDFORM.

FORM frm_9000_handle_pai_command USING po_talv      TYPE REF TO zcl_talv_parent
                                       pv_ucomm     TYPE sy-ucomm
                              CHANGING ct_alv_table TYPE STANDARD TABLE.

  MESSAGE pv_ucomm TYPE 'S'.

ENDFORM.

FORM frm_9000_handle_exit USING po_talv      TYPE REF TO zcl_talv_parent
                       CHANGING ct_alv_table TYPE STANDARD TABLE.

  MESSAGE 'exit' TYPE 'S'.

ENDFORM.

FORM frm_9000_handle_top_of_page USING po_talv        TYPE REF TO zcl_talv_parent
                                       po_document    TYPE REF TO cl_dd_document
                                       pv_table_index TYPE syindex
                              CHANGING ct_alv_table   TYPE STANDARD TABLE.

  po_document->add_text( text         = 'TEST HEADER'
                         sap_emphasis = cl_dd_area=>key
                         sap_color    = cl_dd_area=>list_positive ).

ENDFORM.

FORM frm_9000_handle_retrieve USING pv_ddic_type  TYPE tabname
                           CHANGING ct_alv_table  TYPE STANDARD TABLE.

  IF p_fields IS NOT INITIAL.
    SELECT (p_fields)
      FROM (pv_ddic_type)
      INTO CORRESPONDING FIELDS OF TABLE ct_alv_table
      UP TO 10 ROWS.
  ELSE.
    SELECT *
      FROM (pv_ddic_type)
      INTO CORRESPONDING FIELDS OF TABLE ct_alv_table
      UP TO 10 ROWS.
  ENDIF.

ENDFORM.
*
FORM frm_9000_handle_toolbar  USING po_talv        TYPE REF TO zcl_talv_parent
                                    po_object      TYPE REF TO cl_alv_event_toolbar_set
                                    pv_interactive TYPE char01
                           CHANGING ct_alv_table   TYPE STANDARD TABLE.

  po_talv->add_button( fun_code  = '&COUNTS'           "计数
                    btn_type  =  0                  "工具栏按钮类型
                    icon      = icon_select_all     "图标名称
                    text      = TEXT-fn1            "文本
                    quickinfo = TEXT-fn1            "文本
                    object    = po_object ).

ENDFORM.

FORM frm_9000_handle_user_command USING po_talv      TYPE REF TO zcl_talv_parent
                                        pv_ucomm     TYPE sy-ucomm
                               CHANGING ct_alv_table TYPE STANDARD TABLE.

  MESSAGE pv_ucomm TYPE 'S'.

ENDFORM.

FORM frm_9000_handle_hotspot_click USING po_talv       TYPE REF TO zcl_talv_parent
                                         ps_row        TYPE lvc_s_row
                                         ps_column     TYPE lvc_s_col
                                         ps_sub_row    TYPE lvc_s_roid
                                CHANGING ct_alv_table  TYPE STANDARD TABLE.

  MESSAGE 'hotspot click' TYPE 'S'.

ENDFORM.

FORM frm_9000_handle_double_click USING po_talv      TYPE REF TO zcl_talv_parent
                                        ps_row       TYPE lvc_s_row
                                        ps_column    TYPE lvc_s_col
                               CHANGING ct_alv_table TYPE STANDARD TABLE.

  MESSAGE 'double click' TYPE 'S'.

ENDFORM.

FORM frm_9000_handle_data_changed USING po_talv          TYPE REF TO zcl_talv_parent
                                        po_data_changed  TYPE REF TO cl_alv_changed_data_protocol
                                        pv_onf4          TYPE char01
                                        pv_onf4_before   TYPE char01
                                        pv_onf4_after    TYPE char01
                                        pv_ucomm         TYPE sy-ucomm
                               CHANGING ct_alv_table     TYPE STANDARD TABLE.

  CHECK p_ddic = 'EKKO'.

  FIELD-SYMBOLS <alv_table> TYPE lcl_this=>tty_ekko.
  ASSIGN ct_alv_table TO <alv_table>.

  LOOP AT po_data_changed->mt_mod_cells INTO DATA(cell) WHERE fieldname = 'BUKRS'.

    IF <alv_table>[ cell-row_id ]-bukrs = 'C001'.

      po_data_changed->add_protocol_entry(
                         i_msgid     = '00'
                         i_msgty     = 'E'
                         i_msgno     = '001'
                         i_msgv1     = '公司代码不能修改！ '
                         i_fieldname = cell-fieldname ).

      "主键重复则将字段内容更新
      po_data_changed->modify_cell(
                         i_row_id    = cell-row_id
                         i_fieldname = cell-fieldname
                         i_value     = <alv_table>[ cell-row_id ]-bukrs ).

    ENDIF.

  ENDLOOP.

ENDFORM.

FORM frm_9000_handle_changed_over USING po_talv       TYPE REF TO zcl_talv_parent
                                        pv_modified   TYPE char01
                                        pt_good_cells TYPE lvc_t_modi
                               CHANGING ct_alv_table  TYPE STANDARD TABLE.

  DATA(layout) = po_talv->get_layout( )."生成TALV后仍可修改其布局设置属性
  layout-edit  = abap_true.
  po_talv->set_layout( layout )."修改布局设置属性

  MESSAGE 'changed over' TYPE 'S'.
  po_talv->refresh( ).

ENDFORM.

FORM frm_9000_handle_countdown USING po_talv      TYPE REF TO zcl_talv_parent
                            CHANGING ct_alv_table TYPE STANDARD TABLE.

  MESSAGE 'countdown' TYPE 'S'.
  po_talv->on_retrieve( ).
  po_talv->refresh( ).

ENDFORM.
