export const test7 = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.

DO 1000000 TIMES.
  APPEND row TO tab.
ENDDO.`;