export const test41 = `
DATA field1 TYPE c LENGTH 3.
DATA field2 TYPE c LENGTH 10.
field1 = 'FOO'.
field2 = 'FOO'.

DO 1000000 TIMES.
  IF field1 = field2.
  ENDIF.
ENDDO.
`;
