FUNCTION zfm_talv_multiple_display .
*"----------------------------------------------------------------------
*"*"本地接口：
*"----------------------------------------------------------------------

  ASSERT talv_multi IS NOT INITIAL.

  LOOP AT talv_multi INTO talv.
    IF  talv->is_initialized( ) = abap_false.
      talv->display( ).
    ELSE.
      talv->refresh( ).
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
