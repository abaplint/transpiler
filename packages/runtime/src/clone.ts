import {ABAPObject, DataReference, HexUInt8} from "./types";

export function clone<T>(obj: T): T {
  if (obj instanceof ABAPObject) {
    const n = new ABAPObject();
    n.set(obj.get());
    // @ts-ignore
    return n;
  } else if (obj instanceof DataReference) {
    const n = new DataReference(obj.getType());
    n.assign(obj.getPointer());
    // @ts-ignore
    return n;
  } else if (obj instanceof HexUInt8) {
    const n = new HexUInt8({length: obj.getLength(), qualifiedName: obj.getQualifiedName()});
    n.set(obj.get());
    // @ts-ignore
    return n;
  }

  // @ts-ignore
  const copy = new obj.constructor();
  for (const attr in obj) {
    // @ts-ignore
    // eslint-disable-next-line no-prototype-builtins
    if (obj.hasOwnProperty(attr)) {
      if (typeof obj[attr] !== "object") {
        copy[attr] = obj[attr];
      } else if (obj[attr] === null) {
        copy[attr] = null;
      } else {
        copy[attr] = clone(obj[attr]);
      }
    }
  }
  if (copy["constant"]) {
    copy["constant"] = false;
  }
  return copy;
}