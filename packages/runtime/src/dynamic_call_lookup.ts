import {throwError} from "./throw_error";
import {ICharacter} from "./types/_character";

function findInterfaceMethod(obj: any, name: string): any {
  const suffix = "$" + name;
  const found: any[] = [];
  const seen = new Set<string>();

  let current = obj;
  while (current !== undefined && current !== null) {
    for (const key of Object.getOwnPropertyNames(current)) {
      if (seen.has(key)) {
        continue;
      }
      seen.add(key);
      if (key.endsWith(suffix) && typeof obj[key] === "function") {
        found.push(obj[key]);
      }
    }
    current = Object.getPrototypeOf(current);
  }

  return found.length === 1 ? found[0] : undefined;
}

function findFriendInterfaceMethod(obj: any, name: string): any {
  const suffix = "$" + name;
  const found = Object.keys(obj.FRIENDS_ACCESS_INSTANCE || {})
    .filter(key => key.endsWith(suffix))
    .map(key => obj.FRIENDS_ACCESS_INSTANCE[key]);

  return found.length === 1 ? found[0] : undefined;
}

export function dynamicCallLookup(obj: any, methodName: string | ICharacter): any {
  let name = typeof methodName === "string" ? methodName.toLowerCase().trimEnd() : methodName.get().toLowerCase().trimEnd();
  name = name.replaceAll("~", "$").replaceAll("/", "$");

  let ret = obj[name];

  if (ret !== undefined) {
    ret = ret.bind(obj);
  } else if (obj.FRIENDS_ACCESS_INSTANCE !== undefined) {
    // it might be private, note this currently does not respect encapsulation properly
    // FRIENDS_ACCESS_INSTANCE is already .bind()'ed
    ret = obj.FRIENDS_ACCESS_INSTANCE[name];
  }
  if (ret === undefined && name.includes("$") === false) {
    ret = findInterfaceMethod(obj, name);
    if (ret !== undefined) {
      ret = ret.bind(obj);
    } else if (obj.FRIENDS_ACCESS_INSTANCE !== undefined) {
      ret = findFriendInterfaceMethod(obj, name);
    }
  }

  if (ret === undefined) {
    throwError("CX_SY_DYN_CALL_ILLEGAL_METHOD");
  }

  return ret;
}
