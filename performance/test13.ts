export const test13 = `
TYPES: BEGIN OF ty,
         field TYPE string,
         val   TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
CONSTANTS lc_count TYPE i VALUE 40000.
DO lc_count TIMES.
  row-field = |foo{ sy-index }|.
  INSERT row INTO TABLE tab.
ENDDO.
ASSERT lines( tab ) = lc_count.`;