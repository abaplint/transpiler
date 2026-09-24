// BIT-XOR of two 4 KB xstrings
export const test47 = `
DATA lv_left TYPE xstring.
DATA lv_right TYPE xstring.
DATA lv_result TYPE xstring.
lv_left = 'A5C3'.
lv_right = '3C5A'.
DO 11 TIMES.
  CONCATENATE lv_left lv_left INTO lv_left IN BYTE MODE.
  CONCATENATE lv_right lv_right INTO lv_right IN BYTE MODE.
ENDDO.
DO 2000 TIMES.
  lv_result = lv_left BIT-XOR lv_right.
ENDDO.`;
