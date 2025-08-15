import {ABAP} from "..";
import {Integer} from "../types";

declare const abap: ABAP;

export function line_index(callback: () => void): Integer {

  try {
    callback();
  } catch (error) {
    throw new Error("runtime line_index() todo");
  }

  throw new Error("runtime line_index() todo");
}