import {initial} from "../compare";
import {ABAPObject} from "../types";

function throwError() {
  // @ts-ignore
  if (abap.Classes["CX_SY_MOVE_CAST_ERROR"] !== undefined) {
    // @ts-ignore
    throw new abap.Classes["CX_SY_MOVE_CAST_ERROR"]();
  } else {
    throw "Global class CX_SY_MOVE_CAST_ERROR not found";
  }
}

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
    const targetClass = abap.Classes[targetName];

    if (targetClass?.INTERNAL_TYPE === "CLAS") {
      // using "instanceof" is probably wrong in some cases,
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
      if (source.get() instanceof targetClass === false) {
        throwError();
      }
    } else if (targetClass?.INTERNAL_TYPE === "INTF") {
      const list: string[] = [...source.get().IMPLEMENTED_INTERFACES];
      let sup = source.get().super;
      while (sup !== undefined) {
        list.push(...sup.get().IMPLEMENTED_INTERFACES);
        sup = sup.get().super;
      }
      // todo: ammend list with interfaces implemented by interfaces
      const isImplemented = list.some(i => i === targetName);
      if (isImplemented === false) {
        throwError();
      }
    }
//    }
    target.set(source);
  } else {
    target.set(source);
  }
}