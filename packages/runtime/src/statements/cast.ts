import {throwError} from "../throw_error";
import {ABAPObject, FieldSymbol} from "../types";

// todo, field symbols as input?
// todo, local classes?
// check with javascript instanceof?
// handling interfaces?
export async function cast(target: ABAPObject | FieldSymbol, source: ABAPObject) {
  if (source instanceof ABAPObject && source.get() === undefined) {
    target.clear();
    return;
  }

  // eslint-disable-next-line prefer-const
  let checkIntf = true;

  if (source instanceof FieldSymbol) {
    if (source.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    await cast(target, source.getPointer());
    return;
  } else if (target instanceof FieldSymbol && target.getPointer() === undefined) {
    throw new Error("GETWA_NOT_ASSIGNED");
  }

  let targetName: string | undefined = undefined;
  if (target.getQualifiedName) {
    targetName = target.getQualifiedName()?.toUpperCase();
  }

  // @ts-ignore
  let targetClass = abap.Classes[targetName];
  if (targetClass === undefined) {
    // todo, for unit testing,
    // @ts-ignore
    targetClass = abap.Classes["PROG-ZFOOBAR-" + targetName];
  }

  if (targetClass?.INTERNAL_TYPE === "CLAS") {
    // using "instanceof" is probably wrong in some cases,
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/instanceof
    if (source.get() instanceof targetClass === false) {
      throwError("CX_SY_MOVE_CAST_ERROR");
    }
  } else if (checkIntf === true && targetClass?.INTERNAL_TYPE === "INTF") {
    const list: string[] = source.get().constructor.IMPLEMENTED_INTERFACES;
    const isImplemented = list.some(i => i === targetName);
    if (isImplemented === false) {
      throwError("CX_SY_MOVE_CAST_ERROR");
    }
  }
  target.set(source);

}