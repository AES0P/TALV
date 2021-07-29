class ZCL_TR_TABLE_ENTRY definition
  public
  final
  create private .

public section.

  interfaces ZIF_TR_TOOL .

  aliases ASSOCIATE
    for ZIF_TR_TOOL~ASSOCIATE .
  aliases COMMIT
    for ZIF_TR_TOOL~COMMIT .
  aliases ENTRY
    for ZIF_TR_TOOL~ENTRY .
  aliases ENTRYS
    for ZIF_TR_TOOL~ENTRYS .
  aliases TTY_E071
    for ZIF_TR_TOOL~TTY_E071 .
  aliases TTY_E071K
    for ZIF_TR_TOOL~TTY_E071K .
  aliases TY_E071
    for ZIF_TR_TOOL~TY_E071 .
  aliases TY_E071K
    for ZIF_TR_TOOL~TY_E071K .

  methods CONSTRUCTOR .
  class-methods GET_INSTANCE
    returning
      value(OBJECT) type ref to ZCL_TR_TABLE_ENTRY .
  methods CLEAR .
  PROTECTED SECTION.
private section.

  class-data INSTANCE type ref to ZCL_TR_TABLE_ENTRY .
  data E071 type TTY_E071 .
  data E071K type TTY_E071K .
  data ORDER type E071-TRKORR .
  data TASK type E071-TRKORR .
  data PGMID type PGMID value 'R3TR' ##NO_TEXT.
  data OBJECT type TROBJTYPE value 'TABU' ##NO_TEXT.
  data DYNAMIC_TOOL type ref to ZCL_DYNAMIC_TOOL .
ENDCLASS.



CLASS ZCL_TR_TABLE_ENTRY IMPLEMENTATION.


  METHOD clear.
    CLEAR e071.
    CLEAR e071k.
  ENDMETHOD.


  METHOD constructor.
    dynamic_tool ?= zcl_dynamic_tool=>get_instance( ).
  ENDMETHOD.


  METHOD get_instance.

    IF instance IS NOT BOUND.
      CREATE OBJECT instance.
    ENDIF.

    object = instance.

  ENDMETHOD.


  METHOD zif_tr_tool~associate.

    clear( ).

    IF task IS NOT INITIAL.
      is_ok = abap_true.
      RETURN.
    ENDIF.

    CALL FUNCTION 'TRINT_ORDER_CHOICE'
      EXPORTING
        wi_order_type          = 'W'
        wi_task_type           = 'Q'
        wi_category            = 'CUST'
      IMPORTING
        we_order               = order
        we_task                = task
      TABLES
        wt_e071                = e071
        wt_e071k               = e071k
      EXCEPTIONS
        no_correction_selected = 1
        display_mode           = 2
        object_append_error    = 3
        recursive_call         = 4
        wrong_order_type       = 5
        OTHERS                 = 6.
    IF sy-subrc = 0.
      is_ok = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_tr_tool~commit.

    CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
      EXPORTING
*       WI_SIMULATION                  = ' '
*       WI_SUPPRESS_KEY_CHECK          = ' '
        wi_trkorr                      = task
*       IT_E071K_STR                   =
*       IV_DIALOG                      = 'X'
      TABLES
        wt_e071                        = e071
        wt_e071k                       = e071k
      EXCEPTIONS
        key_char_in_non_char_field     = 1
        key_check_keysyntax_error      = 2
        key_inttab_table               = 3
        key_longer_field_but_no_generc = 4
        key_missing_key_master_fields  = 5
        key_missing_key_tablekey       = 6
        key_non_char_but_no_generic    = 7
        key_no_key_fields              = 8
        key_string_longer_char_key     = 9
        key_table_has_no_fields        = 10
        key_table_not_activ            = 11
        key_unallowed_key_function     = 12
        key_unallowed_key_object       = 13
        key_unallowed_key_objname      = 14
        key_unallowed_key_pgmid        = 15
        key_without_header             = 16
        ob_check_obj_error             = 17
        ob_devclass_no_exist           = 18
        ob_empty_key                   = 19
        ob_generic_objectname          = 20
        ob_ill_delivery_transport      = 21
        ob_ill_lock                    = 22
        ob_ill_parts_transport         = 23
        ob_ill_source_system           = 24
        ob_ill_system_object           = 25
        ob_ill_target                  = 26
        ob_inttab_table                = 27
        ob_local_object                = 28
        ob_locked_by_other             = 29
        ob_modif_only_in_modif_order   = 30
        ob_name_too_long               = 31
        ob_no_append_of_corr_entry     = 32
        ob_no_append_of_c_member       = 33
        ob_no_consolidation_transport  = 34
        ob_no_original                 = 35
        ob_no_shared_repairs           = 36
        ob_no_systemname               = 37
        ob_no_systemtype               = 38
        ob_no_tadir                    = 39
        ob_no_tadir_not_lockable       = 40
        ob_privat_object               = 41
        ob_repair_only_in_repair_order = 42
        ob_reserved_name               = 43
        ob_syntax_error                = 44
        ob_table_has_no_fields         = 45
        ob_table_not_activ             = 46
        tr_enqueue_failed              = 47
        tr_errors_in_error_table       = 48
        tr_ill_korrnum                 = 49
        tr_lockmod_failed              = 50
        tr_lock_enqueue_failed         = 51
        tr_not_owner                   = 52
        tr_no_systemname               = 53
        tr_no_systemtype               = 54
        tr_order_not_exist             = 55
        tr_order_released              = 56
        tr_order_update_error          = 57
        tr_wrong_order_type            = 58
        ob_invalid_target_system       = 59
        tr_no_authorization            = 60
        ob_wrong_tabletyp              = 61
        ob_wrong_category              = 62
        ob_system_error                = 63
        ob_unlocal_objekt_in_local_ord = 64
        tr_wrong_client                = 65
        ob_wrong_client                = 66
        key_wrong_client               = 67
        OTHERS                         = 68.
    IF sy-subrc <> 0.
      is_commited = abap_false.
    ELSE.
      clear( ).
      is_commited = abap_true.
    ENDIF.


  ENDMETHOD.


  METHOD zif_tr_tool~entry.

    IF NOT line_exists( e071[ obj_name = objname ]  ).
      INSERT VALUE #(
                       pgmid       = pgmid
                       object      = object
                       obj_name    = objname"表名
                       objfunc     = objfunc
                     ) INTO TABLE e071.
    ENDIF.


    IF NOT line_exists( e071k[ tabkey = tabkey ]  ).
      INSERT VALUE #(
                       pgmid      = pgmid
                       object     = object
                       objname    = objname"表名
                       mastertype = object
                       mastername = objname"表名
                       tabkey     = tabkey
                     ) INTO TABLE e071k.
    ENDIF.

  ENDMETHOD.


  METHOD zif_tr_tool~entrys.

    tr_tool = me.

    CHECK table IS NOT INITIAL.

    IF ddic_type IS SUPPLIED.
      DATA(tabname) = ddic_type.
    ELSE.
      tabname = dynamic_tool->get_table_relative_name( table ).
    ENDIF.
    DATA(key_fields) = dynamic_tool->get_table_key_fields( tabname ).

    DATA tabkey TYPE trobj_name.
    LOOP AT table ASSIGNING FIELD-SYMBOL(<table_line>).

      CLEAR tabkey.
      LOOP AT key_fields INTO DATA(key_field).
        ASSIGN COMPONENT key_field OF STRUCTURE <table_line> TO FIELD-SYMBOL(<field_value>).
        ASSERT sy-subrc = 0.
        tabkey = tabkey && <field_value>.
      ENDLOOP.

      entry( objname = tabname tabkey = tabkey ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
