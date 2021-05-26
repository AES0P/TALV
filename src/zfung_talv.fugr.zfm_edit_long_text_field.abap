FUNCTION zfm_edit_long_text_field .
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(I_EDITABLE) TYPE  ABAP_BOOL DEFAULT ABAP_FALSE
*"     REFERENCE(I_FIELD_LENGTH) TYPE  LVC_S_FCAT-DD_OUTLEN DEFAULT
*"       1000
*"     REFERENCE(I_WORDWRAP_POSITION) TYPE  I DEFAULT 132
*"     REFERENCE(I_TITLE) TYPE  SY-TITLE DEFAULT 'long text maintain'
*"  CHANGING
*"     REFERENCE(C_TEXT) TYPE  STRING
*"----------------------------------------------------------------------

  title             = i_title.
  editable          = i_editable.
  max_number_chars  = i_field_length.
  wordwrap_position = i_wordwrap_position.

  ASSIGN c_text TO <text>.

  CALL SCREEN 9200.

ENDFUNCTION.
