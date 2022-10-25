import {initial} from "../compare";
import {throwError} from "../throw_error";
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
  let checkIntf = true;

  const targetName = target.getQualifiedName()?.toUpperCase();

  // @ts-ignore
  const targetClass = abap.Classes[targetName];

    /*
    if (targetName?.startsWith("IF_") === false
        && targetName?.startsWith("ZIF_") === false) { // todo, interfaces are also classes but not inherited
*/

  if (targetClass?.INTERNAL_TYPE === "CLAS") {
    // using "instanceof" is probably wrong in some cases,
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
    if (source.get() instanceof targetClass === false) {
      throwError("CX_SY_MOVE_CAST_ERROR");
    }
  } else if (checkIntf === true && targetClass?.INTERNAL_TYPE === "INTF") {
    const list: string[] = [...source.get().constructor.IMPLEMENTED_INTERFACES];

    // interfaces implemented in super classes
    let sup = source.get().super;
    while (sup !== undefined) {
      list.push(...sup.get().constructor.IMPLEMENTED_INTERFACES);
      sup = sup.get().super;
    }

    // interfaces implemented by interfaces
    const visit = new Set<string>(list);
    while (visit.size > 0) {
      const intfName = visit.values().next().value;
      // @ts-ignore
      const intf = abap.Classes[intfName];
      for (const i of intf?.IMPLEMENTED_INTERFACES || []) {
        if (visit.has(i) === false) {
          visit.add(i);
          list.push(i);
        }
      }
      visit.delete(intfName);
    }

    const isImplemented = list.some(i => i === targetName);
    if (isImplemented === false) {
      throwError("CX_SY_MOVE_CAST_ERROR");
    }
  }
  target.set(source);

}