export const test44 = `
TYPES: BEGIN OF ty,
         min TYPE i,
         max TYPE i,
       END OF ty.
DATA tab TYPE SORTED TABLE OF ty WITH UNIQUE KEY min.
DATA lv_count TYPE i.
lv_count = 4000.
DO lv_count TIMES.
  INSERT VALUE ty( min = sy-index
                   max = sy-index ) INTO TABLE tab.
ENDDO.

ASSERT lines( tab ) = lv_count.`;
