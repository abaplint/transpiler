import {ABAPObject} from "../types/index.js";

export function instance_of(val: ABAPObject, cname: any): boolean {
  return val.get() instanceof cname;
}