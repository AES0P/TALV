*&---------------------------------------------------------------------*
*& Report ZAESOP_TALV_DEMO_10
*&---------------------------------------------------------------------*
*&  display customer screen
*&---------------------------------------------------------------------*
REPORT zaesop_talv_demo_10.

*----------------------------------------------------------------------*
* 类定义
*----------------------------------------------------------------------*
CLASS lcl_this DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    CLASS-DATA po_items TYPE REF TO zcl_talv_parent.

    CLASS-METHODS main.

ENDCLASS.


START-OF-SELECTION.
  lcl_this=>main( ).

*----------------------------------------------------------------------*
* 类实施
*----------------------------------------------------------------------*
CLASS lcl_this IMPLEMENTATION.

  METHOD main.

    "工厂模式生成TALV
    zcl_talv_factory=>get_talv( VALUE #( type               = 'TALV_CUS'
                                         ddic_type          = 'EKKO'
                                         dynnr              = '9000'
                                         checkbox_name      = 'SEL'
                                         light_name         = 'LIGHT'
                                         color_table_name   = 'COLOR'
                                         style_table_name   = 'STYLE'
                                         container_position = '01'
                                         header_height      = 28
                                         container          = NEW cl_gui_custom_container( container_name = 'CON1' ) ) ).

    lcl_this=>po_items = zcl_talv_factory=>get_talv( VALUE #( type               = 'TALV_CUS'
                                                              ddic_type          = 'EKPO'
                                                              checkbox_name      = 'SEL'
                                                              dynnr              = '9000'
                                                              container_position = '02'
                                                              header_height      = 6
                                                              container          = NEW cl_gui_custom_container( container_name = 'CON2' ) ) ).

    CALL SCREEN 9000.

  ENDMETHOD.

ENDCLASS.

FORM f9000_01_handle_retrieve USING ddic_type TYPE tabname
                           CHANGING alv_table TYPE STANDARD TABLE.

  DATA max_line TYPE ekpo-ebeln VALUE '30'.

  DATA: po_header TYPE STANDARD TABLE OF ekko.

  SELECT *
    FROM ekko
   INNER JOIN ekpo
      ON ekko~ebeln = ekpo~ebeln
    INTO CORRESPONDING FIELDS OF TABLE po_header
      UP TO 15 ROWS
   WHERE ekpo~ebelp >= max_line.

  EXPORT po_header = po_header TO MEMORY ID 'PO_HEADER'.

  MOVE-CORRESPONDING po_header TO alv_table.

ENDFORM.

FORM f9000_02_handle_retrieve USING ddic_type TYPE tabname
                           CHANGING alv_table TYPE STANDARD TABLE.

  DATA: po_header TYPE STANDARD TABLE OF ekko.
  IMPORT po_header = po_header FROM MEMORY ID 'PO_HEADER'.

  CHECK po_header IS NOT INITIAL.

  SELECT *
    FROM ekpo
    INTO CORRESPONDING FIELDS OF TABLE alv_table
     FOR ALL ENTRIES IN po_header
   WHERE ebeln = po_header-ebeln..

ENDFORM.

FORM f9000_01_handle_top_of_page USING talv TYPE REF TO zcl_talv_parent
                                       document TYPE REF TO cl_dd_document
                                       table_index TYPE syindex
                              CHANGING alv_table TYPE STANDARD TABLE.

  document->initialize_document( background_color = cl_dd_area=>col_textarea ).

  document->add_picture( 'ENJOYSAP_LOGO' ).

  document->add_gap( width = 2 ).
  document->add_text( text         = 'TALV'
                      sap_style    = cl_dd_area=>heading
                      sap_fontsize = cl_dd_area=>large
                      sap_color    = cl_dd_area=>list_group ).

  document->new_line( ).
  document->add_text( text         = 'Program Name :'
                      sap_emphasis = cl_dd_area=>heading
                      sap_color    = cl_dd_area=>list_heading ).

  document->add_text( text         = CONV sdydo_text_element( sy-repid )
                      sap_emphasis = cl_dd_area=>heading ).

  document->new_line( ).
  document->add_text( text         =  'Run Date :'
                      sap_emphasis = cl_dd_area=>heading
                      sap_color    = cl_dd_area=>list_heading_int ).

  document->add_text( text         = CONV sdydo_text_element( sy-datum )
                      sap_emphasis = cl_dd_area=>heading ).

  document->new_line( ).
  document->add_text( text         =  'Run Time :'
                      sap_emphasis = cl_dd_area=>heading
                      sap_color    = cl_dd_area=>list_heading_int ).

  document->add_text( text         = CONV sdydo_text_element( sy-uzeit )
                      sap_emphasis = cl_dd_area=>heading ).
ENDFORM.

FORM f9000_02_handle_top_of_page USING talv TYPE REF TO zcl_talv_parent
                                       document TYPE REF TO cl_dd_document
                                       table_index TYPE syindex
                              CHANGING alv_table TYPE STANDARD TABLE.

  document->add_text( text         = 'ITEMS'
                      sap_emphasis = cl_dd_area=>key
                      sap_color    = cl_dd_area=>list_positive ).

ENDFORM.

FORM f9000_01_handle_changed_over USING talv TYPE REF TO zcl_talv_parent
                                        pv_modified   TYPE char01
                                        pt_good_cells TYPE lvc_t_modi
                               CHANGING alv_table TYPE STANDARD TABLE.

  DATA(po_items) = lcl_this=>po_items->get_outtab( ).
  FIELD-SYMBOLS <po_items> TYPE STANDARD TABLE.
  ASSIGN po_items->* TO <po_items>.

  READ TABLE pt_good_cells INTO DATA(cell) INDEX 1.

  LOOP AT alv_table ASSIGNING FIELD-SYMBOL(<table_line>).

    DATA(index) = sy-tabix.

    "设置style color前先清空一下，防止被别的地方设置的在此错误应用
    talv->clear_line_style_table( ).
    talv->clear_line_color_table( ).

    ASSIGN COMPONENT 'SEL'   OF STRUCTURE <table_line> TO FIELD-SYMBOL(<box>).
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'LIGHT' OF STRUCTURE <table_line> TO FIELD-SYMBOL(<light>).
    CHECK sy-subrc = 0.
    ASSIGN COMPONENT 'EBELN' OF STRUCTURE <table_line> TO FIELD-SYMBOL(<ebeln>).
    CHECK sy-subrc = 0.

    DATA(where) = 'EBELN = ''' &&  <ebeln> && ''''.
    LOOP AT <po_items> ASSIGNING FIELD-SYMBOL(<po_item>) WHERE (where).
      ASSIGN COMPONENT 'SEL'   OF STRUCTURE <po_item> TO FIELD-SYMBOL(<item_box>).
      CHECK sy-subrc = 0.
      <item_box> = <box>.
    ENDLOOP.

    IF <box> <> abap_true.

      <light> = icon_led_red.

      talv->add_line_style( style = zcl_gui_alv_grid=>mc_style_disabled ).
      talv->add_line_color( 3 ).

    ELSE.

      <light>    = icon_led_green.

      talv->add_line_style( style     = zcl_gui_alv_grid=>mc_style_disabled ).
      talv->add_line_style( fieldname = 'BUKRS'"指定列名则只对该列生效，不指定则全部列生效
                            style     = zcl_gui_alv_grid=>mc_style_enabled
                            style2    = zcl_gui_alv_grid=>mc_style_hotspot
                            style4    = zcl_gui_alv_grid=>mc_style4_link ).

      talv->add_line_color( 6 ).
      talv->add_line_color( fieldname = 'BUKRS' col = 5 ).

    ENDIF.

    talv->add_line_style( fieldname = CONV fieldname( talv->get_key_info( 'CHECKBOX_NAME' ) )"不冻结选择框
                          style     = zcl_gui_alv_grid=>mc_style_enabled ).

    "针对指定行修改
    talv->set_style_for_single_line( index ).
    talv->set_color_for_single_line( index ).

  ENDLOOP.

  lcl_this=>po_items->refresh( ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_9000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9000 OUTPUT.

  "展示
  CALL FUNCTION 'ZFM_TALV_MULTIPLE_DISPLAY'.
  SET PF-STATUS 'STATUS' OF PROGRAM 'SAPLZFUNG_TALV'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  IF sy-ucomm = '&BACK' OR sy-ucomm = '&EXIT' OR sy-ucomm = '&CANCEL'.
    CALL FUNCTION 'ZFM_TALV_MULTIPLE_FREE'.
    LEAVE TO SCREEN 0.
  ENDIF.
ENDMODULE.
