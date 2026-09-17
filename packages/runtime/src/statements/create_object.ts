import {throwError} from "../throw_error";

/**
 * CREATE OBJECT with a DYNAMIC class name, into a reference that has a static
 * type: the class the name resolves to has to be compatible with that type.
 *
 * A system raises CX_SY_CREATE_OBJECT_ERROR when it is not, and it raises it
 * BEFORE the constructor runs - so this is checked on the class, not on an
 * instance. Without the check an unrelated class was simply assigned to the
 * reference and the mismatch surfaced later as a raw javascript TypeError on
 * the first member access, which no CATCH takes: a caller that guards its
 * dynamic CREATE OBJECT the way the ABAP documentation asks for was not
 * guarded at all.
 *
 * `targetType` is the class or interface object of the reference's static
 * type and `targetName` its upper-case name; a generic `REF TO object` target
 * passes both as undefined, and then there is nothing to check.
 */
export function checkCreateObjectType(clas: any, targetType: any, targetName?: string) {
  if (clas === undefined || targetType === undefined) {
    return;
  }

  if (targetType.INTERNAL_TYPE === "CLAS") {
    // the class itself or anything below it
    if (clas === targetType || clas.prototype instanceof targetType) {
      return;
    }
  } else if (targetType.INTERNAL_TYPE === "INTF") {
    // the same list the cast checks, and it carries the interfaces a
    // superclass or a composed interface brings along
    const list: string[] = clas.IMPLEMENTED_INTERFACES;
    if (list?.some(i => i === targetName) === true) {
      return;
    }
  } else {
    // not something this can decide - leave it as it was
    return;
  }

  throwError("CX_SY_CREATE_OBJECT_ERROR");
}
