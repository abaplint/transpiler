import {ABAP} from "..";

declare const abap: ABAP;

export function callTransaction() {
  abap.builtin.sy.get().subrc.set(4);
  abap.builtin.sy.get().msgid.set("00");
  abap.builtin.sy.get().msgty.set("E");
  abap.builtin.sy.get().msgno.set("000");
  abap.builtin.sy.get().msgv1.set("CALL TRANSACTION");
  abap.builtin.sy.get().msgv2.set("not supported in open-abap");
}