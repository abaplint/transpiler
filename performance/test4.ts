export const test4 = `
TYPES: BEGIN OF ty,
         name TYPE string,
       END OF ty.
TYPES tt TYPE SORTED TABLE OF ty WITH NON-UNIQUE KEY name.
DATA tab1 TYPE tt.
DATA tab2 TYPE tt.
DATA row LIKE LINE OF tab1.
DATA numc TYPE n LENGTH 5.
DATA lc_rows TYPE i VALUE 1000.

DO lc_rows TIMES.
  numc = sy-index.
  row-name = 'foo' && numc.
  INSERT row INTO TABLE tab1.
ENDDO.

DO 30 TIMES.
  tab2 = tab1.
ENDDO.

ASSERT lines( tab2 ) = lc_rows.`;