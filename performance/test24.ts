export const test24 = `
TYPES: BEGIN OF ty,
         foo TYPE i,
         bar TYPE i,
       END OF ty.
TYPES tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
DATA source TYPE tab.
DATA target TYPE tab.
DATA row LIKE LINE OF source.

DO 1000 TIMES.
  INSERT row INTO TABLE source.
ENDDO.

DO 2000 TIMES.
  target = source.
ENDDO.`;