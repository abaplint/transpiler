export function callTransaction() {
  // @ts-ignore
  abap.builtin.sy.get().subrc.set(4);
  // @ts-ignore
  abap.builtin.sy.get().msgid.set("00");
  // @ts-ignore
  abap.builtin.sy.get().msgty.set("E");
  // @ts-ignore
  abap.builtin.sy.get().msgno.set("000");
  // @ts-ignore
  abap.builtin.sy.get().msgv1.set("CALL TRANSACTION");
  // @ts-ignore
  abap.builtin.sy.get().msgv2.set("not supported in open-abap");
}