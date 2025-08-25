import {ABAP} from ".";
import {LINE_NOT_FOUND} from "./builtin";

declare const abap: ABAP;

export function isLineNotFound(error: Error) {
  if (abap.Classes[LINE_NOT_FOUND] !== undefined
      && error instanceof abap.Classes[LINE_NOT_FOUND]) {
    return true;
      } else if (error.toString() === `Error: Global class ${LINE_NOT_FOUND} not found`) {
    return true;
  }
  return false;
}