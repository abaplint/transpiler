import {ABAP} from "..";
import {ICharacter} from "../types/_character";

export const LINE_NOT_FOUND = "CX_SY_ITAB_LINE_NOT_FOUND";

declare const abap: ABAP;

export function line_exists(callback: () => void): ICharacter {

  try {
    callback();
  } catch (error) {
    sdf
    if (abap.Classes[LINE_NOT_FOUND] !== undefined
        && error instanceof abap.Classes[LINE_NOT_FOUND]) {
      return abap.builtin.abap_false;
    } else if (error.toString() === `Error: Global class ${LINE_NOT_FOUND} not found`) {
      return abap.builtin.abap_false;
    }
    throw error;
  }

  return abap.builtin.abap_true;
}