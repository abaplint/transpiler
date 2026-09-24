// BIT-XOR and BIT-AND of x LENGTH 4, like the register of a CRC-32 loop
export const test46 = `
DATA lv_crc TYPE x LENGTH 4 VALUE 'FFFFFFFF'.
DATA lv_tab TYPE x LENGTH 4 VALUE 'EDB88320'.
DATA lv_low TYPE x LENGTH 4 VALUE '000000FF'.
DATA lv_idx TYPE x LENGTH 4.
DO 500000 TIMES.
  lv_idx = lv_crc BIT-AND lv_low.
  lv_crc = lv_crc BIT-XOR lv_tab.
ENDDO.`;
