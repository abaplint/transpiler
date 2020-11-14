export function clone(obj: any) {
  if (null == obj || "object" != typeof obj) {
    return obj;
  }
  const copy = new obj.constructor();
  for (const attr in obj) {
    // eslint-disable-next-line no-prototype-builtins
    if (obj.hasOwnProperty(attr)) {
      if (null == obj[attr] || "object" != typeof obj[attr]) {
        copy[attr] = obj[attr];
      } else {
        copy[attr] = clone(obj[attr]);
      }
    }
  }
  return copy;
}