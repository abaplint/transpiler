import {initial} from "../compare";
import {ABAPObject} from "../types";

// todo, field symbols as input?
// todo, local classes?
// check with javascript instanceof?
// handling interfaces?
export async function cast(target: ABAPObject, source: ABAPObject) {
  if (initial(source)) {
    target.clear();
    return;
  }

  // eslint-disable-next-line prefer-const
  let castEnabled = true;
  if (castEnabled === true) {
    const targetName = target.getQualifiedName()?.toUpperCase();
    /*
    if (targetName?.startsWith("IF_") === false
        && targetName?.startsWith("ZIF_") === false) { // todo, interfaces are also classes but not inherited
*/
    // @ts-ignore
    const found = abap.Classes[targetName];
    if (found?.INTERNAL_TYPE === "CLAS") {
      // using "instanceof" is probably wrong in some cases,
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
      if (source.get() instanceof found === false) {
        // @ts-ignore
        if (abap.Classes["CX_SY_MOVE_CAST_ERROR"] !== undefined) {
          // @ts-ignore
          throw new abap.Classes["CX_SY_MOVE_CAST_ERROR"]();
        } else {
          throw "Global class CX_SY_MOVE_CAST_ERROR not found";
        }
      }
    } else if (found?.INTERNAL_TYPE === "INTF") {
      console.dir("intf, todo");
      // @ts-ignore
      console.dir(abap.Classes["ZCL_FOO"]);
      console.dir(source);
      console.dir(source.get());
      console.dir(source.get().IMPLEMENTED_INTERFACES);
    }
//    }
    target.set(source);
  } else {
    target.set(source);
  }
}