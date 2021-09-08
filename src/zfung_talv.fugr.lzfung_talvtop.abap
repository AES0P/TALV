FUNCTION-POOL zfung_talv.                        "MESSAGE-ID ..

* INCLUDE LZTALVD01.                         " Local class definition


"talv
DATA talv_stack TYPE ztty_talv.
DATA talv TYPE REF TO zcl_talv_parent.

"同屏TALV集合
DATA talv_multi TYPE ztty_talv.

DATA ok_code TYPE sy-ucomm.
DATA dynnr TYPE sy-dynnr.

"9200
DATA editor_container TYPE REF TO cl_gui_custom_container.

DATA: editable          TYPE abap_bool,
      wordwrap_position TYPE i,
      max_number_chars  TYPE i,
      title             TYPE sy-title.

FIELD-SYMBOLS <text> TYPE any.
DATA editor TYPE REF TO cl_gui_textedit.
