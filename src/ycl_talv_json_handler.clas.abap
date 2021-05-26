class YCL_TALV_JSON_HANDLER definition
  public
  final
  create private .

public section.

  interfaces YIF_SINGLETON .
  interfaces YIF_TALV_JSON_HANDLER .

  aliases FIELDCATS
    for YIF_TALV_JSON_HANDLER~FIELDCATS .
  aliases GET_INSTANCE
    for YIF_SINGLETON~GET_INSTANCE .
  aliases JSON_TO_TALV
    for YIF_TALV_JSON_HANDLER~JSON_TO_TALV .
  aliases TALV_TO_JSON
    for YIF_TALV_JSON_HANDLER~TALV_TO_JSON .

  class-data TABLE type ref to DATA .

  methods JSON_TO_KVTAB
    importing
      value(JSON) type STRING
      value(SPLIT_SYMBOL) type C default ','
    returning
      value(KVTAB) type STRING_TABLE .
  methods KVTAB_TO_JSON
    importing
      value(KVTAB) type STRING_TABLE
      value(SPLIT_SYMBOL) type C
    returning
      value(JSON) type STRING .
  methods KVTAB_TO_FIELDCATS
    importing
      value(KVTAB) type STRING_TABLE
    returning
      value(FIELDCATS) type YIF_TALV_JSON_HANDLER~TTY_KV .
  methods FIELDCATS_TO_KVTAB
    importing
      value(FIELDCATS) type YIF_TALV_JSON_HANDLER~TTY_KV
    returning
      value(KVTAB) type STRING_TABLE .
  methods KV_TO_FIELDCAT
    importing
      value(KV) type STRING_TABLE
    returning
      value(FIELDCAT) type YIF_TALV_JSON_HANDLER~TY_KV .
  methods FIELDCAT_TO_KV
    importing
      value(FIELDCAT) type YIF_TALV_JSON_HANDLER~TY_KV
    returning
      value(KV) type STRING .
  methods FIELDCATS_TO_TALV
    importing
      value(FIELDCATS) type YIF_TALV_JSON_HANDLER~TTY_KV
      value(TALV_TYPE) type ZDETALV_TYPE optional
    returning
      value(TALV) type ref to YCL_TALV_PARENT .
  methods TALV_TO_FIELDCATS
    importing
      !TALV type ref to YCL_TALV_PARENT
    returning
      value(FIELDCATS) type YIF_TALV_JSON_HANDLER~TTY_KV .
  PROTECTED SECTION.

private section.

  class-data HANDLER type ref to YCL_TALV_JSON_HANDLER .
ENDCLASS.



CLASS YCL_TALV_JSON_HANDLER IMPLEMENTATION.


  METHOD FIELDCATS_TO_KVTAB.

    CLEAR kvtab.

    DATA fieldcat TYPE yif_talv_json_handler~ty_kv.
    LOOP AT fieldcats INTO fieldcat.

      APPEND fieldcat_to_kv( fieldcat = fieldcat ) TO kvtab.

    ENDLOOP.

  ENDMETHOD.


  METHOD fieldcats_to_talv.

    DATA fieldcatalog TYPE lvc_t_fcat.
    MOVE-CORRESPONDING fieldcats TO fieldcatalog.
    table = ycl_dynamic_tool=>create_dynamic_table( fieldcatalog = fieldcatalog ).

    FIELD-SYMBOLS: <table>      TYPE STANDARD TABLE,
                   <table_line> TYPE any,
                   <field>      TYPE any.

    ASSIGN table->* TO <table>.
    APPEND INITIAL LINE TO <table>.
    READ TABLE <table> ASSIGNING <table_line> INDEX 1.

    DATA fieldcat TYPE yif_talv_json_handler~ty_kv.
    LOOP AT fieldcats INTO fieldcat.

      ASSIGN COMPONENT fieldcat-fieldname OF STRUCTURE <table_line> TO <field>.
      CHECK <field> IS ASSIGNED.
      <field> = fieldcat-value.

    ENDLOOP.

    IF talv_type IS INITIAL.
      talv_type = 'TALV_POPUP'.
    ENDIF.

    talv = ycl_talv_factory=>get_talv( VALUE #(  type             = talv_type
                                                 ref_data_name    = 'YCL_TALV_JSON_HANDLER=>TABLE'
                                                 fieldcat         = fieldcatalog
                                                 color_table_name = 'COLOR'
                                                 style_table_name = 'STYLE' ) ).

  ENDMETHOD.


  METHOD FIELDCAT_TO_KV.

    DATA: key   TYPE string,
          value TYPE string.

    CONCATENATE '"'
                fieldcat-coltext
                '"'
           INTO key.

    value = fieldcat-value.

    IF fieldcat-datatype = 'CHAR'
      AND fieldcat-ref_table = 'TXX_CON' AND fieldcat-ref_field = 'ACTIVE'.

      IF value = abap_true.
        value = 'true'.
      ELSE.
        value = 'false'.
      ENDIF.

    ELSEIF fieldcat-datatype = 'CHAR'.

      CONCATENATE '"'
                  value
                  '"'
             INTO value.

    ELSEIF fieldcat-datatype = 'DEC'.

      DATA num TYPE dmbtr.
      value = num = value.

      CONDENSE value NO-GAPS.

    ELSEIF fieldcat-datatype = 'NUMC'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = value
        IMPORTING
          output = value.

      CONDENSE value NO-GAPS.

    ENDIF.

    CONCATENATE key
                ':'
                value
           INTO kv.

  ENDMETHOD.


  METHOD json_to_kvtab.

    SPLIT json AT split_symbol INTO TABLE kvtab.
    IF kvtab IS INITIAL.
      MESSAGE 'JSON数据有误，请检查！' TYPE 'E'.
    ENDIF.

  ENDMETHOD.


  METHOD kvtab_to_fieldcats.

    CLEAR fieldcats.

    DATA: kvline TYPE string.

    DATA: kv TYPE string_table.

    LOOP AT kvtab INTO kvline.

      CLEAR: kv.
      SPLIT kvline AT ':' INTO TABLE kv.

      "按照冒号 ：拆出，超过2行的全部视为value被拆错的情形，拼回value行 begin
      DATA keyv TYPE string.
      FIELD-SYMBOLS <keyv> TYPE string.
      LOOP AT kv INTO keyv FROM 3.

        READ TABLE kv ASSIGNING <keyv> INDEX 2.
        <keyv> = <keyv> && keyv.

      ENDLOOP.
      "按照:拆出，超过2行的全部视为value被拆错的情形，拼回value行 end

      IF kv IS INITIAL.
        MESSAGE 'JSON数据有误，请检查！' TYPE 'E'.
      ENDIF.

      APPEND kv_to_fieldcat( kv ) TO fieldcats.

    ENDLOOP.

  ENDMETHOD.


  METHOD KVTAB_TO_JSON.

    CLEAR json.

    DATA kvline TYPE string.

    LOOP AT kvtab INTO kvline.

      IF sy-tabix = lines( kvtab ).
        CLEAR split_symbol.
      ENDIF.

      json = json && kvline && split_symbol.

    ENDLOOP.

  ENDMETHOD.


  METHOD kv_to_fieldcat.

    DATA: key   TYPE string,
          value TYPE string.

    READ TABLE: kv INTO key   INDEX 1,
                kv INTO value INDEX 2.

    REPLACE '"' IN key WITH space.
    REPLACE '"' IN key WITH space.
    CONDENSE key NO-GAPS.

    CLEAR: fieldcat.
    fieldcat-fieldname = key.
    "建议给控件属性加上描述
*      SELECT SINGLE control_name
*        FROM zui_control
*        INTO fieldcat-coltext
*       WHERE control_id = key.
*      IF fieldcat-coltext IS INITIAL.
    fieldcat-coltext = key.
*      ENDIF.

    "1.有双引号就是字符串
    "2.无双引号，转成小写是否是 true / false ，是就是布尔型，不是，判断是否为数字，不是数字就当成字符串
    IF value CS '"'.
      fieldcat-datatype = 'CHAR'.
      fieldcat-intlen   = '256'.

      REPLACE '"' IN value WITH space.
      REPLACE '"' IN value WITH space.

    ELSEIF to_lower( value ) = 'true' OR to_lower( value ) = 'false'.
      fieldcat-ref_table = 'TXX_CON'.
      fieldcat-ref_field = 'ACTIVE'.

      IF to_lower( value ) = 'true'.
        value = abap_true.
      ELSE.
        value = abap_false.
      ENDIF.

    ELSE.

      DATA: type TYPE dd01v-datatype.

      CALL FUNCTION 'NUMERIC_CHECK'
        EXPORTING
          string_in = value
        IMPORTING
          htype     = type. "带小数点会判断为char

      IF type = 'NUMC'.

        fieldcat-datatype = type.
        fieldcat-intlen   = 23.

      ELSE.

        TRY .

            DATA num TYPE dmbtr.
            num = value.

            fieldcat-datatype = 'DEC'.
            fieldcat-intlen   = 23.
            fieldcat-decimals = 2.

          CATCH cx_sy_conversion_no_number.

            fieldcat-datatype = 'CHAR'.
            fieldcat-intlen   = '256'.

        ENDTRY.

      ENDIF.

    ENDIF.

    fieldcat-value = value.

  ENDMETHOD.


  METHOD talv_to_fieldcats.

    CHECK talv IS BOUND.

    DATA data TYPE REF TO data.
    data = talv->get_outtab( ).

    FIELD-SYMBOLS: <table>      TYPE STANDARD TABLE,
                   <table_line> TYPE any,
                   <value>      TYPE any.
    ASSIGN data->* TO <table>.
    READ TABLE <table> ASSIGNING <table_line> INDEX 1.

    MOVE-CORRESPONDING talv->get_fieldcat( ) TO fieldcats.

    FIELD-SYMBOLS <fieldcat> TYPE yif_talv_json_handler~ty_kv.
    LOOP AT fieldcats ASSIGNING <fieldcat>.

      ASSIGN COMPONENT to_upper( <fieldcat>-fieldname ) OF STRUCTURE <table_line> TO <value>.
      CHECK <value> IS ASSIGNED.
      <fieldcat>-value = <value>.

    ENDLOOP.

  ENDMETHOD.


  METHOD yif_singleton~get_instance.

    IF handler IS NOT BOUND.
      CREATE OBJECT handler.
    ENDIF.

    instance = handler.

  ENDMETHOD.


  METHOD yif_talv_json_handler~json_to_talv.

    talv = fieldcats_to_talv(
             fieldcats = kvtab_to_fieldcats(
                           kvtab = json_to_kvtab(
                             json         = json
                             split_symbol = split_symbol  ) )
             talv_type = talv_type ).

  ENDMETHOD.


  METHOD yif_talv_json_handler~talv_to_json.

    json = kvtab_to_json(
             kvtab        = fieldcats_to_kvtab(
                              fieldcats = talv_to_fieldcats( talv ) )
             split_symbol = split_symbol ).

  ENDMETHOD.
ENDCLASS.
