*----------------------------------------------------------------------*
***INCLUDE LZFUNG_TALVO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9200 OUTPUT.
  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITLE' WITH TEXT-t01 title.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_9200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_9200 OUTPUT.

  IF editor_container IS NOT BOUND.

    "创建容器
    CREATE OBJECT editor_container
      EXPORTING
        container_name              = 'CON_EDITOR'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
      "add your handling
    ENDIF.

    "绑定容器
    CREATE OBJECT editor
      EXPORTING
        parent                     = editor_container
        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_to_linebreak_mode = cl_gui_textedit=>false"不把换行处转换为换行符 ##
        wordwrap_position          = wordwrap_position
        max_number_chars           = max_number_chars
      EXCEPTIONS
        OTHERS                     = 1.

    editor->set_status_text( title ).
    editor->set_textstream( <text> ).
    editor->set_toolbar_mode( 1 ).
    editor->set_statusbar_mode( 1 ).

    CASE editable.
      WHEN abap_true.
        editor->set_readonly_mode( 0 ).
      WHEN OTHERS.
        editor->set_readonly_mode( 1 ).
    ENDCASE.

  ENDIF.

ENDMODULE.
