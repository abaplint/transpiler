import {INumeric} from "../types/_numeric";

let prev: number | undefined = undefined;

export function getRunTime(value: INumeric) {

  if (prev === undefined) {
    value.set(0);
    prev = new Date().getTime();
  } else {
    const now = new Date().getTime();
    value.set(now - prev);
    prev = now;
  }

}