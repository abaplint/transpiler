import {ABAP} from "..";
import {isLineNotFound} from "../is_line_not_found";
import {foundIndex} from "../operators";
import {Integer} from "../types";

declare const abap: ABAP;

export async function line_index(callback: () => Promise<void>): Promise<Integer> {

  try {
    await callback();
  } catch (error) {
    if (isLineNotFound(error)) {
      return abap.IntegerFactory.get(0);
    }
    throw error;
  }

  return abap.IntegerFactory.get(foundIndex);
}