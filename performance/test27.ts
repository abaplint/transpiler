export const test27 = `
TYPES: BEGIN OF ty,
         field TYPE string,
         val1  TYPE i,
         val2  TYPE i,
         val3  TYPE i,
         val4  TYPE i,
         val5  TYPE i,
         val6  TYPE i,
         val7  TYPE i,
         val8  TYPE i,
         val9  TYPE i,
         val0  TYPE i,
         asdfval1  TYPE i,
         asdfval2  TYPE i,
         asdfval3  TYPE i,
         asdfval4  TYPE i,
         asdfval5  TYPE i,
         asdfval6  TYPE i,
         asdfval7  TYPE i,
         asdfval8  TYPE i,
         asdfval9  TYPE i,
         asdfval0  TYPE i,
       END OF ty.
DATA tab TYPE HASHED TABLE OF ty WITH UNIQUE KEY field.

DATA row LIKE LINE OF tab.
DO 20000 TIMES.
  READ TABLE tab INTO row WITH KEY field = 'ab'.
ENDDO.`;