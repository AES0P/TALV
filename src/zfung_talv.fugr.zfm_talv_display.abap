FUNCTION zfm_talv_display.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(ALV) TYPE REF TO  ZCL_TALV_PARENT
*"     VALUE(START_ROW) TYPE  I OPTIONAL
*"     VALUE(START_COLUMN) TYPE  I OPTIONAL
*"     VALUE(END_ROW) TYPE  I OPTIONAL
*"     VALUE(END_COLUMN) TYPE  I OPTIONAL
*"----------------------------------------------------------------------

  APPEND alv TO talv_stack.
  READ TABLE talv_stack INTO talv INDEX lines( talv_stack ).

  dynnr = talv->get_key_info( 'DYNNR' ).

  IF dynnr >= 9000 AND dynnr <= 9100."全屏ALV的屏幕编号范围 9000 ~ 9100
    CALL SCREEN dynnr.
  ELSEIF dynnr >= 9900 AND dynnr <= 9999."弹窗ALV的屏幕编号范围 9900 ~ 9999
    IF start_row       IS NOT INITIAL
      AND start_column IS NOT INITIAL
      AND end_row      IS NOT INITIAL
      AND end_column   IS NOT INITIAL.
      CALL SCREEN dynnr
      STARTING AT start_column start_row
        ENDING AT end_column   end_row.
    ELSE.
      CALL SCREEN dynnr
      STARTING AT 10 5
        ENDING AT 145 20.
    ENDIF.
  ENDIF.

ENDFUNCTION.
