import {ABAPObject} from "../types";
import {ABAP} from "..";

declare const abap: ABAP;

/** true when the class or interface is the target or a subtype of it:
 *  superclasses, implemented interfaces and interfaces they include */
function isSubtype(start: any, cname: any): boolean {
  const seen = new Set<any>();
  const matches = (source: any): boolean => {
    if (source == null || seen.has(source)) {
      return false;
    } else if (source === cname) {
      return true;
    }
    seen.add(source);
    const internalName: string = source.INTERNAL_NAME || "";
    const prefix = internalName.substring(0, internalName.lastIndexOf("-") + 1);
    const interfaces: string[] = source.IMPLEMENTED_INTERFACES || [];
    return interfaces.some(i => matches(abap.Classes[prefix + i] || abap.Classes[i]))
      || matches(Object.getPrototypeOf(source));
  };
  return matches(start);
}

export function instance_of(val: ABAPObject, cname: any): boolean {
  const obj = val.get();
  if (obj === undefined) {
    const name = val.getQualifiedName()?.toUpperCase();
    if (name === undefined || name === "OBJECT") {
      return cname === "OBJECT";
    } else if (cname === "OBJECT") {
      return true;
    }
    // Local qualified names are short; RTTI carries the declaring program or class pool.
    const scope = val.getRTTIName()?.toUpperCase().match(/^\\(PROGRAM|CLASS-POOL)=([^\\]+)/);
    const prefix = scope ? (scope[1] === "PROGRAM" ? "PROG" : "CLAS") + "-" + scope[2] + "-" : "";
    return isSubtype(abap.Classes[prefix + name] || abap.Classes[name], cname);
  } else if (cname === "OBJECT") {
    return true;
  }
  // an interface is not on the prototype chain of the classes implementing it
  return obj instanceof cname || isSubtype(obj.constructor, cname);
}
