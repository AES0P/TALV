*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_04
*&---------------------------------------------------------------------*
*&  同一个会话内如果有多个POPUP TALV，则对应form名里的屏幕编号递增1即可,起始9900
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_04.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES ty_ekko  TYPE ekko.
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
    key-type          = 'TALV'.
    "数据字典类型 可动态指定
    key-ddic_type     = lcl_this=>ddic.

    "工厂模式生成TALV并直接展示
    ycl_talv_factory=>get_talv( key )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9000_handle_set_pf_status.
  SET PF-STATUS 'STATUS' OF PROGRAM 'SAPLZFUNG_TALV'.
ENDFORM.

FORM frm_9000_handle_set_title.
  SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH '抬头' '明细'.
ENDFORM.

FORM frm_9900_handle_set_title.
  SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH '行项目' '明细'.
ENDFORM.

FORM frm_9000_handle_on_pbo USING talv TYPE REF TO ycl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE.

  DATA(fieldcat) = talv->get_fieldcat( )."生成TALV后仍可修改其字段目录 及每个字段的属性
  fieldcat[ fieldname = 'EBELN' ]-hotspot = abap_true.
  talv->set_fieldcat( fieldcat )."修改字段目录 可传入字段目录也可传入DDIC名称

  talv->save_fields( lcl_this=>fields )."最终要保留的字段 以空格间隔
  talv->remove_fields( 'ERNAM' )."要去除的字段

  talv->refresh( ).

ENDFORM.

FORM frm_9900_handle_on_pbo USING talv TYPE REF TO ycl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE.

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

FORM frm_9900_handle_retrieve USING pv_ddic_type TYPE tabname
                           CHANGING alv_table TYPE STANDARD TABLE.

  DATA: ebeln TYPE ebeln.
  IMPORT ebeln = ebeln FROM MEMORY ID 'EBELN'.

  SELECT *
    FROM ekpo
    INTO CORRESPONDING FIELDS OF TABLE alv_table
   WHERE ebeln = ebeln.

ENDFORM.

FORM frm_9000_handle_hotspot_click USING talv TYPE REF TO ycl_talv_parent
                                         e_row      TYPE lvc_s_row
                                         e_column   TYPE lvc_s_col
                                         es_sub_row TYPE lvc_s_roid
                                CHANGING alv_table TYPE STANDARD TABLE.

  FIELD-SYMBOLS <alv_table> TYPE lcl_this=>tty_ekko.
  ASSIGN alv_table TO <alv_table>.

  DATA: key TYPE zstalv_key.

  key-type                 = 'TALV_POPUP'.
  key-ddic_type            = 'EKPO'.
  key-handle_set_pf_status = 'FRM_9000_HANDLE_SET_PF_STATUS'."复用9000的status

  DATA(ebeln) = <alv_table>[ e_row-index ]-ebeln.
  EXPORT ebeln = ebeln TO MEMORY ID 'EBELN'.

  DATA popup TYPE REF TO ycl_talv.
  popup ?= ycl_talv_factory=>get_talv( key ).

  popup->display_popup( start_column = 10
                        start_row    = 5
                        end_column   = 145
                        end_row      = 20 ).

ENDFORM.
