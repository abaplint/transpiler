import {initial} from "../compare";
import {ABAPObject} from "../types";

// todo, field symbols as input?
// todo, local classes?
export async function cast(target: ABAPObject, source: ABAPObject) {
  if (initial(source)) {
    target.clear();
    return;
  }

  const targetName = target.getQualifiedName()?.toUpperCase();
  if (targetName?.startsWith("IF_") === false
      && targetName?.startsWith("ZIF_") === false) { // todo, interfaces are also classes but not inherited
    // using "instanceof" is probably wrong in some cases,
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
    // @ts-ignore
    if (abap.Classes[targetName] && source instanceof abap.Classes[targetName] === false) {
      // @ts-ignore
      if (abap.Classes["CX_SY_MOVE_CAST_ERROR"] !== undefined) {
        // @ts-ignore
        throw new abap.Classes["CX_SY_MOVE_CAST_ERROR"]();
      } else {
        throw "Global class CX_SY_MOVE_CAST_ERROR not found";
      }
    }
  }

// throw cx_sy_move_cast_error if incompatible
// check with javascript instanceof?
// handling interfaces?
  target.set(source);
}