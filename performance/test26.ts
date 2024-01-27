export const test26 = `
DATA lv_string TYPE string.
lv_string = 'fooobarrrrrrrrrrrrrrrrrrrr'.
DO 100000 TIMES.
  REPLACE ALL OCCURRENCES OF 'a' IN lv_string WITH 'b'.
  REPLACE ALL OCCURRENCES OF 'b' IN lv_string WITH 'c'.
ENDDO.`;