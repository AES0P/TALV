*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2021/09/04 at 13:33:53
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTALV_FIELDS_SET................................*
DATA:  BEGIN OF STATUS_ZTALV_FIELDS_SET              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTALV_FIELDS_SET              .
CONTROLS: TCTRL_ZTALV_FIELDS_SET
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTALV_FIELDS_SET              .
TABLES: ZTALV_FIELDS_SET               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
