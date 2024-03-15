export const test32 = `
DATA foo TYPE x LENGTH 8.
DATA val TYPE i.
DO 5000000 TIMES.
  GET BIT 18 OF foo INTO val.
ENDDO.`;