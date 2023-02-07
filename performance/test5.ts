export const test5 = `
TYPES: BEGIN OF ty,
         name TYPE string,
       END OF ty.
DATA tab1 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA tab2 TYPE SORTED TABLE OF ty WITH NON-UNIQUE KEY name.
DATA row LIKE LINE OF tab1.
DATA numc TYPE n LENGTH 5.
DATA lc_rows TYPE i VALUE 3000.

DO lc_rows TIMES.
  numc = sy-index.
  row-name = 'foo' && numc.
  INSERT row INTO TABLE tab1.
ENDDO.

DO 20 TIMES.
  tab2 = tab1.
ENDDO.

ASSERT lines( tab2 ) = lc_rows.`;