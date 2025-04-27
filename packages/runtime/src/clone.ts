import {ABAPObject, DataReference, HexUInt8} from "./types";

export function clone<T>(obj: T): T {
  // @ts-ignore
  if (obj.clone) {
    // @ts-ignore
    return obj.clone() as T;
  } else if (obj instanceof ABAPObject) {
    const n = new ABAPObject();
    n.set(obj.get());
    return n as T;
  } else if (obj instanceof DataReference) {
    const n = new DataReference(obj.getType());
    n.assign(obj.getPointer());
    return n as T;
  } else if (obj instanceof HexUInt8) {
    const n = new HexUInt8({length: obj.getLength(), qualifiedName: obj.getQualifiedName()});
    n.set(obj.get());
    return n as T;
  }

  // @ts-ignore
  const copy = new obj.constructor();
  for (const attr in obj) {
    // @ts-ignore
    // eslint-disable-next-line no-prototype-builtins
    if (obj.hasOwnProperty(attr)) {
      if ("object" !== typeof obj[attr] || obj[attr] === null) {
        copy[attr] = obj[attr];
      } else {
        copy[attr] = clone(obj[attr]);
      }
    }
  }

  return copy;
}