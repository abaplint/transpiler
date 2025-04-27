export function clone<T>(obj: T): T {
  // @ts-ignore
  if (obj.clone) {
    // @ts-ignore
    return obj.clone() as T;
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