REPORT zaesop_log_demo.

*----------------------------------------------------------------------*
* 类型声明
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_return1,
         type    TYPE sy-msgty,
         message TYPE string,
       END OF ty_return1.

TYPES: BEGIN OF ty_return2,
         attribute TYPE sy-msgty,
         msg       TYPE string,
       END OF ty_return2.

*----------------------------------------------------------------------*
* 全局变量声明
*----------------------------------------------------------------------*
DATA log TYPE REF TO zcl_log.

*----------------------------------------------------------------------*
* initialization/初始化
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM frm_initialization.

*----------------------------------------------------------------------*
* start-of-selection/开始选择屏幕
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM frm_start_of_selection.

*----------------------------------------------------------------------*
* end-of-selection/结束选择屏幕（程序结束处理，输出等）
*----------------------------------------------------------------------*
END-OF-SELECTION.
  PERFORM frm_end_of_selection.

*&---------------------------------------------------------------------*
*& Form frm_initialization
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_initialization .

  "需提前在SLG0创建系统日志对象（跨客户端表）
  log = zcl_log=>get_instance( object    = 'ZMM'
                               subobject = 'ZMM/ZMM001'
                               msg_ext   = CONV balnrext( sy-uname ) ).

  "需手动开启，开启之后记录到BAL内表
  log->log_bal = abap_true.

  "需手动开启，开启之后记录到内表
  log->log_table = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_start_of_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_start_of_selection .

  PERFORM frm_normal.
  PERFORM frm_return.
  PERFORM frm_bal.
  PERFORM frm_internal_table.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_end_of_selection
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_end_of_selection.

  log->free( ).

  FREE: log.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_normal
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_normal .

  "下面的日志操作，一旦执行就会被自动提交到数据库,因为自动提交开关是默认打开的

  log->error( `error.` ).

  log->debug( `hand08` )."输入用户名触发断点

*  TRY .
*      log->exception( NEW cx_action_profile_not_defined( ) ).
*    CATCH cx_action_profile_not_defined.
*
*  ENDTRY.

  "下边的信息由于取消了自动提交，如果不显式调用 COMMIT 方法，就不会被写入数据库
  log->auto_commit = abap_false.

  log->success( `success 1.` ##NO_TEXT
    )->success( `success 2.` ).

  log->info( `info.` ).
  log->warning( `warning.` ).
  log->error( `error.` ).

  "显式调用提交
  log->commit( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_return
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_return .

  DATA: return  TYPE ty_return1,
        return2 TYPE ty_return2.

  log->auto_commit = abap_true.

  "无需mapping规则示例
  return = VALUE #( type    = 'C'
                    message = '测试return表信息记录'  ).

  log->return( return ).
  log->return( VALUE ty_return1( type    = 'A'
                                 message = '测试return表信息记录'  ) ).

  "需mapping规则示例
  "传入return表与预定义return表有些字段名不一致时，需使用声明式编程传入mapping规则
  return2 = VALUE #( attribute = 'S'
                     msg       = '测试return表通过mapping规则进行信息记录'  ).

  log->return( return       = return2
               mapping_rule = VALUE #( ( field = 'ATTRIBUTE' mean = 'TYPE'    )"传入return表的字段 ATTRIBUTE 对应 预定义return表的 TYPE 字段
                                       ( field = 'MSG'       mean = 'MESSAGE' ) )"传入return表的字段 MSG 对应 预定义return表的 MESSAGE 字段
             ).

  log->return( return       = VALUE ty_return2( attribute = 'X'
                                                msg       = '测试return表通过错误mapping规则进行信息记录' )
               mapping_rule = VALUE #(
                                      ( field = 'ATTRIBUTE1' mean = 'TYPE'    )
                                      ( field = 'MSG'        mean = 'MESSAGE' )
                                     ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_bal
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_bal .

  "把所有日志记录搜集到标准日志记录功能里，供程序运行后立刻展示，与实际提交至数据库的记录可能有差异
  log->display_in_slg1( abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_internal_table
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM frm_internal_table .

  "以SALV展示此次执行过程中生成的日志记录
  log->display_as_alv_popup( ).

ENDFORM.
