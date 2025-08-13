import {ABAP} from "..";
import {Character, String} from "../types";

declare const abap: ABAP;

export function xsdbool(input: boolean | undefined | {val: Character | String}) {
  if (input === true) {
    return abap.builtin.abap_true;
  } else if (input === false || input === undefined) {
    return abap.builtin.abap_false;
  } else if (input.val instanceof String && input.val.get().trim() === "") {
    return abap.builtin.abap_false;
  } else if (input.val instanceof Character && input.val.get().trim() === "") {
    return abap.builtin.abap_false;
  } else {
    return abap.builtin.abap_true;
  }
}