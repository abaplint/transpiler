export const test23 = `
DATA bar TYPE i.
DO 200000 TIMES.
  CASE bar.
    WHEN 1 OR 11.
    WHEN 2 OR 12.
    WHEN 3 OR 13.
    WHEN 4 OR 14.
    WHEN 5 OR 15.
    WHEN 6 OR 16.
    WHEN 7 OR 17.
  ENDCASE.
ENDDO.`;