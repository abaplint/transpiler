export const test39 = `
DATA foo TYPE i.
DATA bar TYPE i.
foo = 1.
bar = 2.

DO 10000000 TIMES.
  IF foo = bar.
  ENDIF.
ENDDO.
`;
