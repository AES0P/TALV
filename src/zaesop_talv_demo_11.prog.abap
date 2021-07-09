*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_10
*&---------------------------------------------------------------------*
*&  display customer screen
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_11.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-DATA: talvs TYPE REF TO cl_object_collection.

    CLASS-DATA: column_row TYPE zstalv_key-container_position VALUE '34'." 3columns * 4rows

    CLASS-METHODS main.

ENDCLASS.


START-OF-SELECTION.
  lcl_this=>main( ).

*----------------------------------------------------------------------*
* 类实施
*----------------------------------------------------------------------*
CLASS lcl_this IMPLEMENTATION.

  METHOD main.

    CREATE OBJECT talvs.

    DATA(columns) = column_row+0(1).
    DATA(rows)    = column_row+1(1).

    DATA(splitter) = NEW cl_gui_splitter_container( columns = CONV i( columns )
                                                    rows    = CONV i( rows )
                                                    parent  = NEW cl_gui_custom_container( container_name = 'CON' ) ).

    DO columns TIMES.

      DATA(column) = sy-index.

      DO rows TIMES.

        DATA(row) = sy-index.

        "工厂模式生成TALV并直接添加到集合
        talvs->add( zcl_talv_factory=>get_talv( VALUE #( type                         = 'TALV_CUS'
                                                         ddic_type                    = 'EKKO'
                                                         dynnr                        = '9300'
                                                         checkbox_name                = 'BOX'
                                                         light_name                   = 'LIGHT'
                                                         color_table_name             = 'COLOR'
                                                         style_table_name             = 'STYLE'
                                                         handle_retrieve              = 'F9300_11_HANDLE_RETRIEVE'
                                                         handle_top_of_page           = 'F9300_11_HANDLE_TOP_OF_PAGE'
                                                         handle_on_pbo                = 'F9300_11_HANDLE_ON_PBO'
                                                         handle_data_changed_finished = 'F9300_11_HANDLE_CHANGED_OVER'
                                                         handle_hotspot_click         = 'F9300_11_HANDLE_HOTSPOT_CLICK'
                                                         container_position           = column && row
                                                         header_height                = 10
                                                         container                    = splitter->get_container( column = column row = row )  ) ) ).

      ENDDO.

    ENDDO.

    DATA(iterator) = talvs->get_iterator( ).

    WHILE iterator->has_next( ).
      CAST zcl_talv_parent( iterator->get_next( ) )->display( ).
    ENDWHILE.

    CALL SCREEN 9300.

  ENDMETHOD.

ENDCLASS.

FORM f9300_11_handle_retrieve USING ddic_type TYPE tabname
                           CHANGING alv_table TYPE STANDARD TABLE.

  SELECT *
    FROM (ddic_type)
    INTO CORRESPONDING FIELDS OF TABLE alv_table
    UP TO 10 ROWS.

ENDFORM.


FORM f9300_11_handle_on_pbo USING talv TYPE REF TO zcl_talv_parent
                         CHANGING alv_table TYPE STANDARD TABLE.

  DATA(fieldcat) = talv->get_fieldcat( )."生成TALV后仍可修改其字段目录 及每个字段的属性
  fieldcat[ fieldname = 'EBELN' ]-hotspot = abap_true.
  talv->set_fieldcat( fieldcat )."修改字段目录 可传入字段目录也可传入DDIC名称

  talv->remove_fields( 'ERNAM' )."要去除的字段

  talv->refresh( ).

ENDFORM.

FORM f9300_11_handle_top_of_page USING talv TYPE REF TO zcl_talv_parent
                                       document TYPE REF TO cl_dd_document
                                       table_index TYPE syindex
                              CHANGING alv_table TYPE STANDARD TABLE.

  document->add_text( text         = 'TALV:' && talv->get_key_info( 'CONTAINER_POSITION' )
                      sap_emphasis = cl_dd_area=>heading
                      sap_color    = cl_dd_area=>list_negative_inv ).


ENDFORM.

FORM f9300_11_handle_changed_over USING talv TYPE REF TO zcl_talv_parent
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

      talv->add_line_style( style = zcl_gui_alv_grid=>mc_style_disabled ).
      talv->add_line_color( 3 ).

    ELSE.

      <light>    = icon_led_green.

      talv->add_line_style( style     = zcl_gui_alv_grid=>mc_style_disabled ).
      talv->add_line_style( fieldname = 'MENGE'"指定列名则只对该列生效，不指定则全部列生效
                            style     = zcl_gui_alv_grid=>mc_style_enabled
                            style2    = zcl_gui_alv_grid=>mc_style_hotspot
                            style4    = zcl_gui_alv_grid=>mc_style4_link ).

      talv->add_line_color( 6 ).
      talv->add_line_color( fieldname = 'MENGE' col = 5 ).

    ENDIF.

    talv->add_line_style( fieldname = CONV fieldname( talv->get_key_info( 'CHECKBOX_NAME' ) )"不冻结选择框
                          style     = zcl_gui_alv_grid=>mc_style_enabled ).

    "针对指定行修改
    talv->set_style_for_single_line( index ).
    talv->set_color_for_single_line( index ).

  ENDLOOP.


  talv->refresh( ).

ENDFORM.


FORM f9300_11_handle_hotspot_click USING talv TYPE REF TO zcl_talv_parent
                                         e_row      TYPE lvc_s_row
                                         e_column   TYPE lvc_s_col
                                         es_sub_row TYPE lvc_s_roid
                                CHANGING alv_table TYPE STANDARD TABLE.


  DATA: key TYPE zstalv_key.

  key-type                 = 'TALV_POPUP'.
  key-ddic_type            = 'EKPO'.

  READ TABLE alv_table ASSIGNING FIELD-SYMBOL(<alv_line>) INDEX e_row-index.
  CHECK <alv_line> IS ASSIGNED.

  ASSIGN COMPONENT 'EBELN' OF STRUCTURE <alv_line> TO FIELD-SYMBOL(<ebeln>).
  CHECK <ebeln> IS ASSIGNED.

  EXPORT ebeln = <ebeln> TO MEMORY ID 'EBELN'.

  DATA popup TYPE REF TO zcl_talv.
  popup ?= zcl_talv_factory=>get_talv( key ).

  popup->display_popup( start_column = 10
                        start_row    = 5
                        end_column   = 145
                        end_row      = 20 ).

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

*&---------------------------------------------------------------------*
*& Module status_9300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9300 OUTPUT.
  SET PF-STATUS 'STATUS' OF PROGRAM 'SAPLZFUNG_TALV'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  user_command_9300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9300 INPUT.
  IF sy-ucomm = '&BACK' OR sy-ucomm = '&EXIT' OR sy-ucomm = '&CANCEL'.

    DATA(iterator) = lcl_this=>talvs->get_iterator( ).
    WHILE iterator->has_next( ).
      CAST zcl_talv_parent( iterator->get_next( ) )->free( ).
    ENDWHILE.

    FREE iterator.

    lcl_this=>talvs->clear( ).
    FREE lcl_this=>talvs.

    LEAVE TO SCREEN 0.

  ENDIF.
ENDMODULE.
