export const test40 = `
DATA foo TYPE string.
DATA bar TYPE string.
foo = 'HELLO'.
bar = 'WORLD'.

DO 10000000 TIMES.
  IF foo = bar.
  ENDIF.
ENDDO.
`;
