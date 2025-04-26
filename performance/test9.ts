export const test9 = `
TYPES: BEGIN OF ty,
         field TYPE string,
         val   TYPE i,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY field.
DATA row LIKE LINE OF tab.
CONSTANTS lc_count TYPE i VALUE 200000.
DO lc_count TIMES.
  row-field = |foo{ sy-index }|.
  INSERT row INTO TABLE tab.
ENDDO.
ASSERT lines( tab ) = lc_count.`;