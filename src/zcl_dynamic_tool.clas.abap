class ZCL_DYNAMIC_TOOL definition
  public
  create private .

public section.

  interfaces ZIF_SINGLETON .

  aliases GET_INSTANCE
    for ZIF_SINGLETON~GET_INSTANCE .

  types:
    BEGIN OF comp_info,
        field      TYPE fieldname,
        text       TYPE string,
        domname    TYPE domname,
        leng       TYPE ddleng,
        intlen     TYPE intlen,
        outputlen  TYPE outputlen,
        decimals   TYPE decimals,
        datatype   TYPE datatype,
        inttype    TYPE inttype,
        checktable TYPE checktable,
        reftable   TYPE reftable,
        reffield   TYPE reffield,
        convexit   TYPE convexit,
        memoryid   TYPE memoryid,
      END OF comp_info .
  types:
    tcomp_info TYPE STANDARD TABLE OF comp_info WITH KEY field .

  class-methods COPY_REF_TABLE_VALUE
    importing
      !OUTTAB type STANDARD TABLE
      !FIELDCATALOG type LVC_T_FCAT
    returning
      value(DESTAB) type ref to DATA .
  class-methods INTERNAL_RECEIVER
    importing
      !IP_TABLE type ref to DATA .
  class-methods CREATE_DYNAMIC_TABLE
    importing
      !TALV_KEY type ZSTALV_KEY optional
      !FIELDCATALOG type LVC_T_FCAT optional
      !CHECKBOX_NAME type C optional
      !STYLE_TABLE_NAME type C optional
      !LENGTH_IN_BYTE type BOOLEAN optional
    returning
      value(DATA_TABLE) type ref to DATA .
  methods TRANSFER_LVC_FCAT_TO_KKBLO
    importing
      !FIELDCAT_LVC type LVC_T_FCAT
    returning
      value(FIELDCAT_KKBLO) type KKBLO_T_FIELDCAT .
  methods CREATE_TABLE_BY_FIELDCAT
    importing
      value(FIELDCATS) type KKBLO_T_FIELDCAT
      !OBJECT type ANY
      !CLASS_NAME type ANY
      !CLASS_METHOD type C
      !CHECKBOX_NAME type C
      !LIGHT_NAME type C
      !CELL_COLOR_TABLE_NAME type C
      !STYLE_TABLE type C
      !TABNAME type KKBLO_TABNAME
      !LENGTH_IN_BYTE type BOOLEAN .
  methods GET_TABLE_COMPONENTS_BY_DATA
    importing
      value(TABLE) type STANDARD TABLE
    returning
      value(COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  methods GET_STRUC_COMPONENTS_BY_DATA
    importing
      !WA type ANY
    returning
      value(COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  methods GET_COMPONENTS_BY_GLOBAL_TYPE
    importing
      !TYPE_NAME type C
    returning
      value(COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  methods GET_COMPONENTS_BASE
    importing
      !STRUCTDESCR type ref to CL_ABAP_STRUCTDESCR
    returning
      value(COMPONENTS) type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE .
  methods GET_COMPONENTS_INFO
    importing
      !COMPONENTS type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    returning
      value(INFO) type TCOMP_INFO .
  methods GET_COMPONENT_INFO_BASE
    importing
      !COMPONENTDESCR type ABAP_COMPONENTDESCR
      !RETTYPE type CHAR1 optional
    exporting
      !FIELD_INFO type DFIES
    returning
      value(TEXT) type STRING .
  methods CONVERT_COMPONENTS_TO_FIELDCAT
    importing
      !COMPONENTS type CL_ABAP_STRUCTDESCR=>COMPONENT_TABLE
    returning
      value(TFIELDCAT) type LVC_T_FCAT .
  methods GET_TABLE_KEY_FIELDS
    importing
      value(TABNAME) type C
    returning
      value(KEY_FIELDS) type STRING_TABLE .
  methods GET_TABLE_RELATIVE_NAME
    importing
      !TABLE type TABLE
    returning
      value(TABNAME) type TROBJ_NAME .
  PROTECTED SECTION.
private section.

  class-data TOOL_INSTANCE type ref to ZCL_DYNAMIC_TOOL .
  data DEFAULT_NAME type CHAR6 value 'ORIGIN' ##NO_TEXT.
  class-data TABLE type ref to DATA .
ENDCLASS.



CLASS ZCL_DYNAMIC_TOOL IMPLEMENTATION.


  METHOD convert_components_to_fieldcat.

    CLEAR: tfieldcat.

    DATA: info TYPE tcomp_info,
          desc TYPE comp_info.

    DATA: fieldcat LIKE LINE OF tfieldcat.

    FIELD-SYMBOLS <component> LIKE LINE OF components.

    info = get_components_info( components ).
    SORT info ASCENDING BY field.

    LOOP AT components ASSIGNING <component>.

      CLEAR: fieldcat.
      fieldcat-fieldname = <component>-name.

      READ TABLE info INTO desc WITH KEY field = <component>-name
                                BINARY SEARCH.
      IF sy-subrc = 0.

        fieldcat-domname    = desc-domname.
        fieldcat-intlen     = desc-intlen.
        fieldcat-outputlen  = desc-outputlen.
        fieldcat-decimals   = desc-decimals.
        fieldcat-datatype   = desc-datatype.
        fieldcat-inttype    = desc-inttype.
        fieldcat-checktable = desc-checktable.
        fieldcat-ref_table   = desc-reftable.
        fieldcat-ref_field   = desc-reffield.
        fieldcat-convexit   = desc-convexit.
*        fieldcat-leng       = desc-leng.
*        fieldcat-memoryid   = desc-memoryid.

        fieldcat-coltext = desc-text.
        fieldcat-scrtext_l = fieldcat-coltext.
        fieldcat-scrtext_m = fieldcat-coltext.
        fieldcat-scrtext_s = fieldcat-coltext.

      ENDIF.

      APPEND fieldcat TO tfieldcat.

    ENDLOOP.

  ENDMETHOD.


  METHOD copy_ref_table_value.

    FIELD-SYMBOLS: <destinations> TYPE STANDARD TABLE,
                   <destination>  TYPE any,
                   <fieldcat>     TYPE lvc_s_fcat,
                   <original>     TYPE any,
                   <from>         TYPE any,
                   <to>           TYPE any.

    DATA: destination TYPE REF TO data.

    "用指定要copy的字段创建内表
    destab = zcl_dynamic_tool=>create_dynamic_table( fieldcatalog = fieldcatalog ).

    ASSIGN destab->* TO <destinations>.
    CREATE DATA destination LIKE LINE OF <destinations>.
    ASSIGN destination->* TO <destination>.

    LOOP AT outtab ASSIGNING <original>.
      LOOP AT fieldcatalog ASSIGNING <fieldcat>.
        ASSIGN COMPONENT <fieldcat>-fieldname OF STRUCTURE <original>    TO <from>.
        ASSIGN COMPONENT <fieldcat>-fieldname OF STRUCTURE <destination> TO <to>.
        MOVE <from> TO <to>.
      ENDLOOP.
      APPEND <destination> TO <destinations>.
    ENDLOOP.

  ENDMETHOD.


  METHOD create_dynamic_table.

    DATA this TYPE REF TO zcl_dynamic_tool.

    this ?= get_instance( ).

    DATA: fieldcats TYPE lvc_t_fcat,
          style     TYPE fieldname,
          checkbox  TYPE fieldname.

    fieldcats = talv_key-fieldcat.
    style     = talv_key-style_table_name.
    checkbox  = talv_key-checkbox_name.

    IF fieldcatalog IS SUPPLIED.
      fieldcats = fieldcatalog.
    ENDIF.

    IF style_table_name IS SUPPLIED.
      style = style_table_name.
    ENDIF.

    IF checkbox_name IS SUPPLIED.
      checkbox = checkbox_name.
    ENDIF.

    this->create_table_by_fieldcat( object                = this
                                    class_name            = 'ZCL_DYNAMIC_TOOL'
                                    class_method          = 'INTERNAL_RECEIVER'
                                    fieldcats             = this->transfer_lvc_fcat_to_kkblo( fieldcats )
                                    checkbox_name         = checkbox
                                    light_name            = talv_key-light_name
                                    cell_color_table_name = talv_key-color_table_name
                                    style_table           = style
                                    length_in_byte        = length_in_byte
                                    tabname               = '1' ).
    IF table IS BOUND.
      data_table = table.
    ELSE.
*      RAISE generate_subpool_dir_full.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD create_table_by_fieldcat.

    CONSTANTS: con_report(60)    TYPE c VALUE 'REPORT GENENERATED_SUBROUTINE_POOL.',
               con_form(60)      TYPE c VALUE 'FORM  TABLE_CREATE.',
               con_form2(60)     TYPE c VALUE 'FORM  TABLE_CREATE USING OBJECT',
               con_begin(60)     TYPE c VALUE 'DATA: BEGIN OF GENTAB OCCURS 0.',
               con_checkbox(60)  TYPE c VALUE 'DATA: CHECKBOX(1) TYPE C.',
               con_light(60)     TYPE c VALUE 'DATA: LIGHT(4) TYPE C.',
               con_cellcolor(60) TYPE c VALUE 'DATA: CELLCOLOR TYPE LVC_T_SCOL.',
               con_style(60)     TYPE c VALUE 'DATA: STYLE_TABLE TYPE LVC_T_STYL.',
               con_end(60)       TYPE c VALUE 'DATA: END OF GENTAB.',
               con_endform(60)   TYPE c VALUE 'ENDFORM.',
               con_reference(60) TYPE c VALUE 'DATA: POINTER TYPE REF TO DATA.',
               con_create(60)    TYPE c VALUE 'CREATE DATA POINTER LIKE STANDARD TABLE OF GENTAB.'.

    DATA: fieldcat TYPE kkblo_fieldcat.
    DATA: string(60) TYPE c.
    DATA: report LIKE sy-repid.
    DATA: message(240) TYPE c,
          line         TYPE i,
          word(72)     TYPE c.

    DATA: code_line TYPE string.
    DATA: source_code TYPE string_table.

    DATA: form(30) TYPE c VALUE 'TABLE_CREATE'.
    DATA: outlen TYPE lvc_outlen.                           "Y9CK020977

    CALL FUNCTION 'K_KKB_FIELDCAT_COMPLETE'
      EXPORTING
        i_tabname   = tabname
      CHANGING
        ct_fieldcat = fieldcats[]
      EXCEPTIONS
        OTHERS      = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid
            TYPE sy-msgty
          NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    code_line = con_report.
    APPEND code_line TO source_code.

    IF object IS INITIAL.
      code_line = con_form.
    ELSE.
      code_line = con_form2.
      CONCATENATE code_line
                  'TYPE REF TO'
                  class_name
                  '.'
             INTO code_line SEPARATED BY space.
    ENDIF.
    APPEND code_line TO source_code.

    code_line = con_begin.
    APPEND code_line TO source_code.

    IF checkbox_name IS NOT INITIAL.
      code_line = con_checkbox.
      "使用指定的checkbox name
      REPLACE 'CHECKBOX' IN code_line WITH checkbox_name.
      APPEND code_line TO source_code.
    ENDIF.

    IF light_name IS NOT INITIAL.
      code_line = con_light.
      "使用指定的light name
      REPLACE 'LIGHT' IN code_line WITH light_name.
      APPEND code_line TO source_code.
    ENDIF.

    IF cell_color_table_name IS NOT INITIAL.
      code_line = con_cellcolor.
      "使用指定的cell color table name
      REPLACE 'CELLCOLOR' IN code_line WITH cell_color_table_name.
      APPEND code_line TO source_code.
    ENDIF.

    IF style_table IS NOT INITIAL.
      code_line = con_style.
      "使用指定的style table name
      REPLACE 'STYLE_TABLE' IN code_line WITH style_table.
      APPEND code_line TO source_code.
    ENDIF.

*<<< YI3K081678 get invalid signs to check fieldnames
    DATA: l_space    TYPE string,
          fieldname  TYPE string,
          field_mess TYPE string,
          subrc      TYPE sy-subrc.

    l_space = cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).
    CONCATENATE l_space ':' INTO l_space.
    CONCATENATE l_space ';' INTO l_space.
    CONCATENATE l_space '.' INTO l_space.
*>>> YI3K081678

    SORT fieldcats BY col_pos ASCENDING.

    LOOP AT fieldcats INTO fieldcat.
*<<< YI3K081678 check fieldnames
      fieldname = fieldcat-fieldname.
      IF fieldname CA l_space.
        field_mess = fieldname.
        subrc = '1'.
* message raised outside the loop
      ELSE.
*>>> YI3K081678
        CLEAR code_line.
        IF NOT fieldcat-ref_fieldname IS INITIAL.
          CONCATENATE 'DATA:'
                      fieldcat-fieldname
                      'TYPE'
                      fieldcat-ref_tabname
                 INTO code_line SEPARATED BY space.

          CONCATENATE code_line
                      '-'
                      fieldcat-ref_fieldname
                      '.'
                 INTO code_line.
        ELSE.
          IF fieldcat-datatype = 'CHAR'. "Y6AK044662  " #EC CI_UTCL_OK

*  >>Y6AK037383
            IF length_in_byte EQ abap_true.
              CLASS cl_abap_char_utilities DEFINITION LOAD.
              fieldcat-intlen = fieldcat-intlen / cl_abap_char_utilities=>charsize.
            ENDIF.
*  <<Y6AK037383

            IF fieldcat-ddic_outputlen > 0.  ">>B20K8A0Q9G
              outlen = fieldcat-ddic_outputlen.             "Y6AK044662
            ENDIF.

            IF fieldcat-intlen > 0.
              IF fieldcat-ddic_outputlen > fieldcat-intlen.
                outlen = fieldcat-ddic_outputlen.
              ELSE.
                outlen = fieldcat-intlen.
              ENDIF.
            ENDIF.

            IF outlen = 0.
              outlen = fieldcat-outputlen.
            ENDIF.  "<<B20K8A0Q9G

            CONCATENATE 'DATA:'
                        fieldcat-fieldname
                   INTO code_line SEPARATED BY space.
            CONCATENATE code_line '(' outlen ').'           "B20K8A0Q9G
                   INTO code_line.

          ELSEIF fieldcat-datatype = 'NUMC'.  "Y6AK044662   " #EC CI_UTCL_OK

            IF length_in_byte EQ abap_true.
              CLASS cl_abap_char_utilities DEFINITION LOAD.
              fieldcat-intlen = fieldcat-intlen / cl_abap_char_utilities=>charsize.
            ENDIF.

            IF fieldcat-ddic_outputlen > 0.
              outlen = fieldcat-ddic_outputlen.
            ENDIF.

            IF fieldcat-intlen > 0.
              IF fieldcat-ddic_outputlen > fieldcat-intlen.
                outlen = fieldcat-ddic_outputlen.
              ELSE.
                outlen = fieldcat-intlen.
              ENDIF.
            ENDIF.

            IF outlen = 0.
              outlen = fieldcat-outputlen.
            ENDIF.  "<<B20K8A0Q9G

            CONCATENATE 'DATA:'
                        fieldcat-fieldname
                   INTO code_line SEPARATED BY space.

            CONCATENATE code_line '(' outlen ')'
                   INTO code_line.

            CONCATENATE code_line 'TYPE N.'
                   INTO code_line SEPARATED BY space.       "Y6AK044662

          ELSEIF fieldcat-datatype = 'CURR'.            "#EC CI_UTCL_OK

            CONCATENATE 'DATA:'
                        fieldcat-fieldname
                        'TYPE'
                        fieldcat-inttype
                        'DECIMALS 2.'
                   INTO code_line SEPARATED BY space.

          ELSEIF fieldcat-datatype EQ 'INT1'            "#EC CI_UTCL_OK
                OR fieldcat-inttype EQ 'B'
                OR fieldcat-inttype EQ 'b'.

            fieldcat-inttype = 'I'.

            CONCATENATE 'DATA:'
                        fieldcat-fieldname
                        'TYPE'
                        'INT1'                              "#EC NOTEXT
                        '.'
                   INTO code_line SEPARATED BY space.       "B20K8A0MOD

          ELSEIF fieldcat-datatype EQ 'INT2'                 " #EC CI_UTCL_OK
            OR fieldcat-inttype EQ 's'.
            fieldcat-inttype = 'I'.

            CONCATENATE 'DATA:'
                        fieldcat-fieldname
                        'TYPE'
                        'INT2'                              "#EC NOTEXT
                        '.'
                   INTO code_line SEPARATED BY space.       "B20K8A0MOD

          ELSEIF fieldcat-datatype EQ 'INT8'                 " #EC CI_UTCL_OK
              OR fieldcat-inttype EQ '8'.

            fieldcat-inttype = '8'.

            CONCATENATE 'DATA:'
                        fieldcat-fieldname
                        'TYPE'
                        'INT8'                              "#EC NOTEXT
                        '.'
                   INTO code_line SEPARATED BY space.      " YI3K487344

          ELSEIF fieldcat-datatype EQ 'UTCL'                 " #EC CI_UTCL_OK
              OR fieldcat-inttype EQ 'p'.
            fieldcat-inttype = 'p'.

            CONCATENATE 'DATA:'
                        fieldcat-fieldname
                        'TYPE'
                        'UTCL'                              "#EC NOTEXT
                        '.'
                   INTO code_line SEPARATED BY space.      " YI3K487344
          ELSE.
            IF NOT fieldcat-intlen IS INITIAL AND fieldcat-inttype NE 'g'. "B20K8A0MOD

              IF fieldcat-inttype = 'F' AND fieldcat-intlen NE 8.
                fieldcat-intlen = 8.
              ENDIF.

              IF fieldcat-inttype = 'D' AND fieldcat-intlen NE 8.
                fieldcat-intlen = 8.
              ENDIF.
              IF fieldcat-inttype = 'T' AND fieldcat-intlen NE 6.
                fieldcat-intlen = 6.
              ENDIF.
              IF fieldcat-inttype EQ 'P'.
                DATA: l_leng TYPE lvc_outlen.
                IF length_in_byte EQ abap_true.
                  l_leng = fieldcat-intlen.
                ELSE.
*                if fieldcat-no_sign is initial.
*                  l_leng =  ( fieldcat-intlen / 2 ) + 1 .
*                else.
                  l_leng =  ( fieldcat-intlen + 1 ) / 2.
*                endif.
                ENDIF.
                CONCATENATE fieldcat-fieldname
                            '('
                            l_leng
                            ')'
                       INTO string.
              ELSE.
                CONCATENATE fieldcat-fieldname
                            '('
                            fieldcat-intlen
                            ')'
                       INTO string.
              ENDIF.
            ELSE.
              string = fieldcat-fieldname.
            ENDIF.

            IF fieldcat-inttype EQ 'g'.                      "B20K8A0MOD     " #EC CI_UTCL_OK

              CONCATENATE 'DATA:'
                          string
                          'TYPE'
                          'String'                          "#EC NOTEXT
                          '.'
                     INTO code_line SEPARATED BY space.     "B20K8A0MOD
            ELSE.
              CONCATENATE 'DATA:'
                          string
                          'TYPE'
                          fieldcat-inttype '.'
                     INTO code_line SEPARATED BY space.
            ENDIF.

            IF fieldcat-inttype = 'P' AND NOT fieldcat-decimals IS
               INITIAL.
              REPLACE '.' WITH 'DECIMALS' INTO code_line.
              CONCATENATE code_line
                          fieldcat-decimals
                          '.'
                     INTO code_line SEPARATED BY space.
            ENDIF.
          ENDIF.
        ENDIF.
        APPEND code_line TO source_code.
      ENDIF.                                                "YI3K081678
    ENDLOOP.

*<<< YI3K081678 single (!) message incorrect fieldname
    IF subrc EQ '1'.
      MESSAGE i538(0k) WITH field_mess 'FIELDNAME'.
      CLEAR subrc.
    ENDIF.
*>>> YI3K081678

    code_line = con_end.
    APPEND code_line TO source_code.

    CLEAR code_line.
    IF class_name IS NOT INITIAL.

      code_line = con_reference.
      APPEND code_line TO source_code.

      code_line = con_create.
      APPEND code_line TO source_code.

      IF NOT object IS INITIAL.
        CONCATENATE 'CALL METHOD OBJECT->'
                    class_method
               INTO code_line.

        CONCATENATE code_line
                    'EXPORTING IP_TABLE = POINTER.'
               INTO code_line SEPARATED BY space.
      ELSE.
        CONCATENATE 'CALL METHOD ZCL_DYNAMIC_TOOL=>'
                    class_method
               INTO code_line.

        CONCATENATE code_line 'EXPORTING IP_TABLE = POINTER.'
               INTO code_line SEPARATED BY space.
      ENDIF.

    ENDIF.
    APPEND code_line TO source_code.

    code_line = con_endform.
    APPEND code_line TO source_code.

    report = sy-cprog.

    CATCH SYSTEM-EXCEPTIONS generate_subpool_dir_full = 9.
      GENERATE SUBROUTINE POOL source_code
                          NAME report
                       MESSAGE message
                          LINE line
                          WORD word.                   "#EC CI_GENERATE
    ENDCATCH.
    CASE sy-subrc.
      WHEN 0.
      WHEN 9.
        MESSAGE x000(0k) WITH 'GENERATE_SUBPOOL_DIR_FULL'.
      WHEN OTHERS.
        MESSAGE x000(0k) WITH message line word.
    ENDCASE.

*****Unicode_2003
    IF NOT message IS INITIAL.
      MESSAGE e000(0k) WITH message line word.
    ENDIF.

*****Unicode_2003
    IF object IS INITIAL.
      PERFORM (form) IN PROGRAM (report).
    ELSE.
      PERFORM (form) IN PROGRAM (report) USING object.
    ENDIF.

  ENDMETHOD.


  METHOD get_components_base.

    DATA: structdesc TYPE REF TO cl_abap_structdescr,
          comps      TYPE cl_abap_structdescr=>component_table,
          comps2     TYPE cl_abap_structdescr=>component_table,
          flag       TYPE abap_bool,
          idx        TYPE i,
          tabix      TYPE i.

    FIELD-SYMBOLS: <comp>  LIKE LINE OF comps,
                   <comp2> LIKE LINE OF comps2.

    CLEAR: comps[],
           components[],
           comps2[].

    comps = structdescr->get_components(  ).
    flag = abap_true.

    WHILE flag = abap_true.
      CLEAR flag.
      LOOP AT comps ASSIGNING <comp> FROM tabix.
        IF <comp>-as_include EQ 'X'.
          flag = abap_true.
          tabix = sy-tabix.
          idx   = sy-tabix + 1.
          structdesc ?= <comp>-type.
          comps2 = structdesc->get_components( ).
          LOOP AT comps2 ASSIGNING <comp2>.
            INSERT <comp2> INTO comps INDEX idx.
            ADD 1 TO idx.
          ENDLOOP.
          DELETE comps INDEX tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDWHILE.

    " Aligning data structure by keys

    components[] = comps[].

  ENDMETHOD.


  METHOD get_components_by_global_type.

    components = get_components_base( CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( type_name ) ) ).

  ENDMETHOD.


  METHOD get_components_info.

    CLEAR: info.
    DATA: desc TYPE comp_info.
    DATA: field_info  TYPE dfies.

    FIELD-SYMBOLS: <comp> LIKE LINE OF components.

    LOOP AT components ASSIGNING <comp>.
      desc-field = <comp>-name.
      desc-text  = get_component_info_base(
                     EXPORTING
                       componentdescr = <comp>
                     IMPORTING
                       field_info = field_info ).
      MOVE-CORRESPONDING field_info TO desc.
      IF desc-datatype = 'QUAN' OR desc-datatype = 'CURR'.
        desc-intlen = field_info-leng.
      ELSE.
        desc-intlen = field_info-outputlen.
      ENDIF.
      APPEND desc TO info.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_component_info_base.

    DATA: elemdescr TYPE REF TO cl_abap_elemdescr,
          datadescr TYPE REF TO cl_abap_datadescr,
          flddescr  TYPE dfies,
          ret_type  TYPE char1.

    datadescr ?= componentdescr-type.
    IF rettype IS NOT SUPPLIED.
      ret_type = 'M'.
    ELSE.
      ret_type = rettype.
    ENDIF.

    CASE ret_type.
      WHEN 'M' OR 'S' OR 'L' OR 'R'.
      WHEN OTHERS.
        ret_type = 'M'.
    ENDCASE.

    IF datadescr->kind NE cl_abap_datadescr=>kind_elem.
      text = componentdescr-name.
      RETURN.
    ENDIF.

    elemdescr ?= componentdescr-type.

    elemdescr->get_ddic_field(
          EXPORTING
            p_langu      = sy-langu    " Current Language
          RECEIVING
            p_flddescr   = flddescr    " Field Description
          EXCEPTIONS
            OTHERS       = 3
        ).
    IF sy-subrc NE 0
    OR flddescr-scrtext_m IS INITIAL.
      elemdescr->get_ddic_field(
            EXPORTING
              p_langu      = 'E'    " Current Language
            RECEIVING
              p_flddescr   = flddescr    " Field Description
            EXCEPTIONS
              OTHERS       = 3
          ).
      IF sy-subrc NE 0
      OR flddescr-scrtext_m IS INITIAL.
        IF default_name = 'ORIGIN'.
          flddescr-scrtext_m = componentdescr-name.
        ELSE.
          flddescr-scrtext_m = 'NULL'.
        ENDIF.
        ret_type = 'M'.
      ENDIF.
    ENDIF.

    CASE ret_type.
      WHEN 'M'.
        text = flddescr-scrtext_m.
      WHEN 'R'.
        text = flddescr-reptext.
      WHEN 'S'.
        text = flddescr-scrtext_s.
      WHEN 'L'.
        text = flddescr-scrtext_l.
    ENDCASE.

    field_info = flddescr.

  ENDMETHOD.


  METHOD get_struc_components_by_data.

    components = get_components_base( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( wa ) ) ).

  ENDMETHOD.


  METHOD get_table_components_by_data.

    FIELD-SYMBOLS <structure> TYPE any.

    IF table IS INITIAL.
      APPEND INITIAL LINE TO table.
    ENDIF.

    READ TABLE table ASSIGNING <structure> INDEX 1.
    components = get_struc_components_by_data( <structure> ).

  ENDMETHOD.


  METHOD internal_receiver.
    table = ip_table.
  ENDMETHOD.


  METHOD transfer_lvc_fcat_to_kkblo.

    CLEAR: fieldcat_kkblo.

    CALL FUNCTION 'LVC_TRANSFER_TO_KKBLO'
      EXPORTING
        it_fieldcat_lvc   = fieldcat_lvc
      IMPORTING
        et_fieldcat_kkblo = fieldcat_kkblo.

  ENDMETHOD.


  METHOD zif_singleton~get_instance.

    IF tool_instance IS NOT BOUND.
      CREATE OBJECT tool_instance.
    ENDIF.

    instance = tool_instance.

  ENDMETHOD.


  METHOD GET_TABLE_KEY_FIELDS.

    SELECT fieldname
      FROM dd03l
      INTO TABLE key_fields
     WHERE tabname = tabname
       AND keyflag = abap_true
     ORDER BY position.

  ENDMETHOD.


  METHOD get_table_relative_name.

    FIELD-SYMBOLS <table_line> TYPE any.
    READ TABLE table ASSIGNING <table_line> INDEX 1.

    ASSERT <table_line> IS ASSIGNED.

    tabname = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data( <table_line> ) )->get_relative_name( ).

  ENDMETHOD.
ENDCLASS.
