import {ABAP} from "..";
import {Integer} from "../types";
import {LINE_NOT_FOUND} from "./line_exists";

declare const abap: ABAP;

export function line_index(callback: () => void): Integer {

  try {
    callback();
  } catch (error) {
    if (abap.Classes[LINE_NOT_FOUND] !== undefined
        && error instanceof abap.Classes[LINE_NOT_FOUND]) {
      return abap.IntegerFactory.get(-1);
    } else if (error.toString() === `Error: Global class ${LINE_NOT_FOUND} not found`) {
      return abap.IntegerFactory.get(-1);
    }
    throw error;
  }

  throw new Error("runtime line_index() todo");
}