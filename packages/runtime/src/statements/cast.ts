import {ABAPObject} from "../types";

// todo, field symbols as input?
export async function cast(target: ABAPObject, source: ABAPObject) {
// cx_sy_move_cast_error
// check with javascript instanceof?
// handling interfaces?
  target.set(source);
}