import {ABAP} from "..";
import {isLineNotFound} from "../is_line_not_found";
import {ICharacter} from "../types/_character";

export const LINE_NOT_FOUND = "CX_SY_ITAB_LINE_NOT_FOUND";

declare const abap: ABAP;

export function line_exists(callback: () => void): ICharacter {

  try {
    callback();
  } catch (error) {
    if (isLineNotFound(error)) {
      return abap.builtin.abap_false;
    }
    throw error;
  }

  return abap.builtin.abap_true;
}