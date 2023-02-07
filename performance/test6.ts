export const test6 = `
TYPES: BEGIN OF ty,
         name TYPE string,
       END OF ty.
DATA tab1 TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab1.
DATA numc TYPE n LENGTH 6.

DO 50000 TIMES.
  numc = sy-index.
  row-name = 'foo' && numc.
  INSERT row INTO tab1 INDEX 1.
ENDDO.`;