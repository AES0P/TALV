CLASS ycl_log DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_log_detail,
        msgno     TYPE sy-msgno,
        msgty     TYPE sy-msgty,
        msg       TYPE baltmsg,
        timestamp TYPE timestamp,
        username  TYPE sy-uname,
      END OF ty_log_detail .
    TYPES:
      BEGIN OF ty_mapping_rule,
        field TYPE fieldname,
        mean  TYPE fieldname,
      END OF ty_mapping_rule .
    TYPES:
      BEGIN OF ty_return,
        type    TYPE char01,
        message TYPE char255,
      END OF ty_return .
    TYPES:
      tty_log_detail TYPE STANDARD TABLE OF ty_log_detail WITH EMPTY KEY .
    TYPES:
      tty_mapping_rule TYPE STANDARD TABLE OF ty_mapping_rule WITH EMPTY KEY .

    DATA system_log_level TYPE ballevel VALUE '9' ##NO_TEXT.
    DATA auto_commit TYPE abap_bool VALUE abap_true ##NO_TEXT.
    DATA log_table TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA log_bal TYPE abap_bool VALUE abap_false ##NO_TEXT.
    DATA msg_list TYPE REF TO if_reca_message_list .

    CLASS-METHODS get_instance
      IMPORTING
        VALUE(object)      TYPE balobj_d
        VALUE(subobject)   TYPE balsubobj
        VALUE(msg_ext)     TYPE balnrext OPTIONAL
        VALUE(auto_commit) TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(log)         TYPE REF TO ycl_log .
    METHODS get_table
      RETURNING
        VALUE(table) TYPE tty_log_detail .
    METHODS msg .
    METHODS success
      IMPORTING
        VALUE(content) TYPE c
      RETURNING
        VALUE(log)     TYPE REF TO ycl_log .
    METHODS info
      IMPORTING
        VALUE(content) TYPE c
      RETURNING
        VALUE(log)     TYPE REF TO ycl_log .
    METHODS warning
      IMPORTING
        VALUE(content) TYPE c
      RETURNING
        VALUE(log)     TYPE REF TO ycl_log .
    METHODS error
      IMPORTING
        VALUE(content) TYPE c
      RETURNING
        VALUE(log)     TYPE REF TO ycl_log .
    METHODS debug
      IMPORTING
        VALUE(uname) TYPE sy-uname
      RETURNING
        VALUE(log)   TYPE REF TO ycl_log .
    METHODS exception
      IMPORTING
        VALUE(exception) TYPE REF TO cx_root .
    METHODS return
      IMPORTING
        VALUE(return)       TYPE any
        VALUE(mapping_rule) TYPE tty_mapping_rule OPTIONAL
      RETURNING
        VALUE(log)          TYPE REF TO ycl_log .
    METHODS insert_message_to_table
      IMPORTING
        VALUE(message) TYPE recamsg .
    METHODS commit .
    METHODS add_bal_log_msg
      IMPORTING
        VALUE(message) TYPE recamsg .
    METHODS is_valid_level
      IMPORTING
        VALUE(level)          TYPE recamsg-detlevel
      RETURNING
        VALUE(is_valid_level) TYPE abap_bool .
    METHODS get_salv
      CHANGING
        !table      TYPE STANDARD TABLE
      RETURNING
        VALUE(salv) TYPE REF TO cl_salv_table .
    METHODS display_as_alv_popup
      IMPORTING
        VALUE(start_column) TYPE i DEFAULT 5
        VALUE(start_line)   TYPE i DEFAULT 5
        VALUE(end_column)   TYPE i DEFAULT 120
        VALUE(end_line)     TYPE i DEFAULT 25 .
    METHODS display_in_slg1
      IMPORTING
        VALUE(amodal) TYPE abap_bool OPTIONAL .
    METHODS display_using_dbtable
      IMPORTING
        VALUE(table_name) TYPE tablenam
        VALUE(where)      TYPE c OPTIONAL .
    METHODS table_to_string
      IMPORTING
        VALUE(table)  TYPE ANY TABLE
        VALUE(field)  TYPE fieldname
      RETURNING
        VALUE(string) TYPE string .
    METHODS free .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA log_handle TYPE balloghndl .
    CLASS-DATA logger TYPE REF TO ycl_log .
    DATA object TYPE balobj_d .
    DATA subobject TYPE balsubobj .
    DATA msg_ext TYPE balnrext .
    DATA table TYPE tty_log_detail .
    DATA dummy TYPE c .

    METHODS constructor
      IMPORTING
        VALUE(object)      TYPE balobj_d
        VALUE(subobject)   TYPE balsubobj
        VALUE(msg_ext)     TYPE balnrext
        VALUE(auto_commit) TYPE abap_bool OPTIONAL .
ENDCLASS.



CLASS YCL_LOG IMPLEMENTATION.


  METHOD add_bal_log_msg.

    DATA msg TYPE bal_s_msg.

    MOVE-CORRESPONDING message TO msg.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = log_handle
        i_s_msg          = message
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.


  ENDMETHOD.


  METHOD commit.

    "调用对象方法, 保存消息到数据库
    msg_list->store(
      EXPORTING
        if_in_update_task = abap_false
      EXCEPTIONS
        error             = 1
        OTHERS            = 2 ).
    IF sy-subrc <> 0.
    ENDIF.

    "提交到数据库
    cf_reca_storable=>commit( ).

  ENDMETHOD.


  METHOD constructor.

    DATA log TYPE bal_s_log.

    me->object    = object.
    me->subobject = subobject.
    me->msg_ext   = msg_ext.

    "创建日志对象 gi_msglist, 传入为日志类型、子类型、外部信息
    CALL METHOD cf_reca_message_list=>create
      EXPORTING
        id_object    = me->object   "(SLG0)
        id_subobject = me->subobject  " (SLG0)
        id_extnumber = me->msg_ext "external number
      RECEIVING
        ro_instance  = me->msg_list.

    log-extnumber  = msg_ext.
    log-object     = object.
    log-subobject  = subobject.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = log
      IMPORTING
        e_log_handle            = log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    me->auto_commit = auto_commit.

  ENDMETHOD.


  METHOD debug.

    DATA: msg TYPE string.

    msg = uname.
    TRANSLATE msg TO UPPER CASE.

    IF msg = sy-uname.

      BREAK-POINT sy-uname.

      msg = 'Started debug mode by - ' && sy-uname.

      MESSAGE a001(00) WITH msg INTO dummy.
      msg( ).

    ENDIF.

    log = me.

  ENDMETHOD.


  METHOD display_as_alv_popup.

    DATA: salv TYPE REF TO cl_salv_table.

    salv = get_salv( CHANGING table = table ).

    salv->set_screen_popup( start_column = start_column
                            start_line   = start_line
                            end_column   = end_column
                            end_line     = end_line ).

    salv->display( ).

  ENDMETHOD.


  METHOD display_in_slg1.

    DATA: profile TYPE bal_s_prof.

    IF sy-batch IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile  = profile
        i_t_log_handle       = VALUE bal_t_logh( ( log_handle ) )
        i_amodal             = amodal
      EXCEPTIONS
        profile_inconsistent = 1
        internal_error       = 2
        no_data_available    = 3
        no_authority         = 4
        OTHERS               = 5.

    IF sy-subrc <> 0.
    ENDIF.

  ENDMETHOD.


  METHOD display_using_dbtable.

    DATA: dbtable TYPE REF TO data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    CREATE DATA dbtable TYPE STANDARD TABLE OF (table_name).
    ASSIGN dbtable->* TO <table>.

    SELECT *
      FROM (table_name)
      INTO CORRESPONDING FIELDS OF TABLE <table>
     WHERE (where).

    get_salv( CHANGING table = <table> )->display( ).

  ENDMETHOD.


  METHOD error.
    MESSAGE e001(00) WITH content INTO dummy.
    msg( ).
    log = me.
  ENDMETHOD.


  METHOD exception.

    DATA: content TYPE string.

    content = exception->get_text( ).

    MESSAGE a001(00) WITH content INTO dummy.
    msg( ).

    RAISE EXCEPTION exception.

  ENDMETHOD.


  METHOD free.

    success( 'User: ' && sy-uname && ' leave this transaction.' ).

    CALL FUNCTION 'BAL_LOG_REFRESH'
      EXPORTING
        i_log_handle  = log_handle "句柄
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.

    IF msg_list IS BOUND.
      msg_list->free( ).
    ENDIF.

    CLEAR: system_log_level,
           object,
           subobject,
           msg_ext,
           msg_list,
           logger.


  ENDMETHOD.


  METHOD get_instance.
    IF logger IS NOT BOUND.
      CREATE OBJECT logger
        EXPORTING
          object      = object
          subobject   = subobject
          msg_ext     = msg_ext
          auto_commit = auto_commit.
    ENDIF.
    log = logger.
  ENDMETHOD.


  METHOD get_salv.

    DATA: cx_msg TYPE REF TO cx_root.

    TRY.
        cl_salv_table=>factory(
            IMPORTING
              r_salv_table   = salv
            CHANGING
              t_table        = table ).
      CATCH cx_salv_msg INTO cx_msg.
    ENDTRY.

    IF salv IS BOUND.

      salv->get_columns( )->set_optimize( abap_true ).
      salv->get_functions( )->set_all( ).

      TRY.
          salv->get_columns(:
             )->get_column( 'TIMESTAMP' )->set_visible( abap_false ),
             )->get_column( 'USERNAME' )->set_visible( abap_false ).
        CATCH cx_salv_not_found ##NO_HANDLER.
      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD get_table.
    table = me->table.
  ENDMETHOD.


  METHOD info.
    MESSAGE i001(00) WITH content INTO dummy.
    msg( ).
    log = me.
  ENDMETHOD.


  METHOD insert_message_to_table.

    DATA: line LIKE LINE OF table.
    CLEAR: line.

    line-msgno     = lines( table ) + 1.
    line-msgty     = message-msgty.

    MESSAGE ID message-msgid
          TYPE message-msgty
        NUMBER message-msgno
          WITH message-msgv1
               message-msgv2
               message-msgv3
               message-msgv4
          INTO line-msg.

    GET TIME STAMP FIELD line-timestamp.
    line-username  = sy-uname.
    APPEND line TO table.

  ENDMETHOD.


  METHOD is_valid_level.

    DATA: numc TYPE n LENGTH 1,
          type TYPE dd01v-datatype.

    is_valid_level = abap_false.

    "日志级别<=系统级别才记录日志
    IF system_log_level >= level.
      is_valid_level = abap_true.
    ENDIF.

    CALL FUNCTION 'NUMERIC_CHECK'
      EXPORTING
        string_in  = level
      IMPORTING
        string_out = numc
        htype      = type.
    IF type <> 'NUMC'.
      is_valid_level = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD msg.

    DATA: log_level TYPE recamsg-detlevel. "应用日志：细节层次
    DATA: message TYPE recamsg. "定义消息变量

    CLEAR: log_level,message.

    "当日志级别大于等于控制打印级时, 输出日志, 否则不输出, 以节约系统资源
    "日志级别 1 - 9, 1表示该日志最为概略, 9表示该日志最为细节

    TRANSLATE sy-msgty TO UPPER CASE.
    CASE sy-msgty.
      WHEN 'E'.
        log_level = '9'.
      WHEN 'A'.
        log_level = '7'.
      WHEN 'W'.
        log_level = '5'.
      WHEN 'I'.
        log_level = '3'.
      WHEN 'S'.
        log_level = '1'.
      WHEN OTHERS.
    ENDCASE.

    CHECK is_valid_level( log_level ).

    MOVE-CORRESPONDING sy TO message.

    message-msgv2 = message-msgv1.
    message-msgv1 = sy-uzeit && ': '.

    IF log_table = abap_true.
      insert_message_to_table( message )."for salv show or other
    ENDIF.

    IF log_bal = abap_true.
      add_bal_log_msg( message )."for slg1 show
    ENDIF.

    msg_list->add( is_message = message )."添加到缓存表中

    IF auto_commit = abap_true.
      commit( )."for system store
    ENDIF.

  ENDMETHOD.


  METHOD return.

    DATA: new_return TYPE ty_return.
    DATA: msg TYPE string.

    FIELD-SYMBOLS: <type>         TYPE any,
                   <message>      TYPE any,
                   <mapping_rule> TYPE any.

    IF mapping_rule IS INITIAL.

      ASSIGN COMPONENT:  'TYPE' OF STRUCTURE return TO <type>,
                      'MESSAGE' OF STRUCTURE return TO <message>.
      IF <type> IS ASSIGNED AND <message> IS ASSIGNED.

        IF NOT <type> CA 'EAISW'.
          msg = '消息类型不是E/A/I/S/W之一：TYPE： ' && <type> && ' ,MESSAGE： ' && <message>.
          MESSAGE w001(00) WITH msg INTO dummy.
        ELSE.
          MESSAGE
               ID '00'
             TYPE <type>
           NUMBER '001'
             WITH <message> INTO dummy.
        ENDIF.

        msg( ).

      ENDIF.

    ENDIF.

    IF mapping_rule IS NOT INITIAL.

      FIELD-SYMBOLS: <_type>    TYPE any,
                     <_message> TYPE any.

      READ TABLE mapping_rule ASSIGNING <mapping_rule> WITH KEY mean = 'TYPE'.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'FIELD' OF STRUCTURE <mapping_rule> TO <type>.
      ENDIF.

      READ TABLE mapping_rule ASSIGNING <mapping_rule> WITH KEY mean = 'MESSAGE'.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'FIELD' OF STRUCTURE <mapping_rule> TO <message>.
      ENDIF.

      IF <type> IS ASSIGNED AND <message> IS ASSIGNED.

        ASSIGN COMPONENT: <type>    OF STRUCTURE return TO <_type>,
                          <message> OF STRUCTURE return TO <_message>.

      ENDIF.

      IF <_type> IS ASSIGNED AND <_message> IS ASSIGNED.

        new_return-type    = <_type>.
        new_return-message = <_message>.

        return( new_return ).

      ELSE.

        warning( '日志丢失，字段映射表声明有误！' ).

      ENDIF.

    ENDIF.

    log = me.

  ENDMETHOD.


  METHOD success.
    MESSAGE s001(00) WITH content INTO dummy.
    msg( ).
    log = me.
  ENDMETHOD.


  METHOD table_to_string.

    FIELD-SYMBOLS: <line>  TYPE any.

    LOOP AT table ASSIGNING <line>.
      CHECK <line> IS NOT INITIAL.

      ASSIGN COMPONENT field OF STRUCTURE <line> TO FIELD-SYMBOL(<field>).
      IF <field> IS ASSIGNED AND <field> IS NOT INITIAL.
        CONCATENATE string <field> INTO string SEPARATED BY space.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD warning.
    MESSAGE w001(00) WITH content INTO dummy.
    msg( ).
    log = me.
  ENDMETHOD.
ENDCLASS.
