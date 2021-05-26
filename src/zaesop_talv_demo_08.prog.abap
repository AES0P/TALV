*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_08
*&---------------------------------------------------------------------*
*& 本程序示例如何将一个JSON转为TALV进行数据处理，以及处理完毕如何转回JSON
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_08.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-DATA json_str     TYPE string VALUE 'type:"F", title:"*", expanded:true,columns:2, labelSpan:3.2,fields:"*"'.
    CLASS-DATA split_symbol TYPE c VALUE ','.

    CLASS-DATA json_handler TYPE REF TO yif_talv_json_handler.

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

    json_handler ?= ycl_talv_json_handler=>get_instance( ).

    json_handler->json_to_talv(
                    json      = json_str
                    talv_type = 'TALV' )->display( ).

  ENDMETHOD.

ENDCLASS.

FORM frm_9000_handle_set_title.
  SET TITLEBAR 'TITLE'  OF PROGRAM 'SAPLZFUNG_TALV' WITH 'JSON数据处理'.
ENDFORM.

FORM frm_9000_handle_on_pbo USING talv TYPE REF TO ycl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE.

  DATA(layout) = talv->get_layout( )."生成TALV后仍可修改其布局设置属性
  layout-edit  = abap_true.
  talv->set_layout( layout )."修改布局设置属性

  talv->refresh( ).

ENDFORM.

FORM frm_9000_handle_pai_command USING talv TYPE REF TO ycl_talv_parent
                                       pv_ucomm TYPE sy-ucomm
                              CHANGING alv_table TYPE STANDARD TABLE.
  cl_demo_output=>display(
    lcl_this=>json_handler->talv_to_json(
                              talv         = talv
                              split_symbol = lcl_this=>split_symbol ) ).
ENDFORM.
