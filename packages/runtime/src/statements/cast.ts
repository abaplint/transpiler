import {initial} from "../compare";
import {ABAPObject} from "../types";

// todo, field symbols as input?
export async function cast(target: ABAPObject, source: ABAPObject) {
  if (initial(source)) {
    target.clear();
    return;
  }

// throw cx_sy_move_cast_error if incompatible
// check with javascript instanceof?
// handling interfaces?
  target.set(source);
}