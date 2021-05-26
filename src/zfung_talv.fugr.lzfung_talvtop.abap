FUNCTION-POOL zfung_talv.                        "MESSAGE-ID ..

* INCLUDE LZTALVD01.                         " Local class definition


"talv
DATA talv_stack TYPE STANDARD TABLE OF REF TO ycl_talv_parent.
DATA talv TYPE REF TO ycl_talv_parent.

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
