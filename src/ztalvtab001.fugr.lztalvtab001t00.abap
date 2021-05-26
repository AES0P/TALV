*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 2021.05.26 at 15:08:12
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTALV_SERVICE...................................*
DATA:  BEGIN OF STATUS_ZTALV_SERVICE                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTALV_SERVICE                 .
CONTROLS: TCTRL_ZTALV_SERVICE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTALV_SERVICE                 .
TABLES: ZTALV_SERVICE                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
