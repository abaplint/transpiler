export const test35 = `
DATA foo TYPE i.
DATA bar TYPE i.

DO 10000000 TIMES.
  IF foo > bar.
  ENDIF.
ENDDO.
`;