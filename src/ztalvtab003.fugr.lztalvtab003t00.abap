*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2021/09/04 at 15:46:35
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTALV_LAYOUT_SET................................*
DATA:  BEGIN OF STATUS_ZTALV_LAYOUT_SET              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTALV_LAYOUT_SET              .
CONTROLS: TCTRL_ZTALV_LAYOUT_SET
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTALV_LAYOUT_SET              .
TABLES: ZTALV_LAYOUT_SET               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
