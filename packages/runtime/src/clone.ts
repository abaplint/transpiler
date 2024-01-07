import {ABAPObject, DataReference} from "./types";

export function clone<T>(obj: T): T {
  if (null == obj || "object" != typeof obj) {
    return obj;
  }

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
  }

  // @ts-ignore
  const copy = new obj.constructor();
  for (const attr in obj) {
    // @ts-ignore
    // eslint-disable-next-line no-prototype-builtins
    if (obj.hasOwnProperty(attr)) {
      /*
      if (null === obj[attr]) {
        console.dir("null");
        copy[attr] = null;
      } else
      */
      if ("object" !== typeof obj[attr]) {
        copy[attr] = obj[attr];
      } else {
        copy[attr] = clone(obj[attr]);
      }
    }
  }
  return copy;
}