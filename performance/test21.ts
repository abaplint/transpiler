export const test21 = `
DATA bar TYPE string.
DO 200000 TIMES.
  CASE bar.
    WHEN 'abc1' OR 'sdfsdf1'.
    WHEN 'abc2' OR 'sdfsdf2'.
    WHEN 'abc3' OR 'sdfsdf3'.
    WHEN 'abc4' OR 'sdfsdf4'.
    WHEN 'abc5' OR 'sdfsdf5'.
    WHEN 'abc6' OR 'sdfsdf6'.
    WHEN 'abc7' OR 'sdfsdf7'.
  ENDCASE.
ENDDO.`;