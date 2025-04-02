import {ICharacter} from "./types/_character";

export function dynamicCallLookup(obj: any, methodName: string | ICharacter): any {
  // todo: escape interface and namespace characters in name?
  const name = typeof methodName === "string" ? methodName : methodName.get().toLowerCase().trimEnd();

  let ret = obj[name];

  if (ret === undefined && obj.FRIENDS_ACCESS_INSTANCE !== undefined) {
    // it might be private, note this currently does not respect encapsulation properly
    ret = obj.FRIENDS_ACCESS_INSTANCE[name];
  }

  if (ret === undefined) {
    // @ts-ignore
    if (abap.Classes['CX_SY_DYN_CALL_ILLEGAL_METHOD'] === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_METHOD not found"; }
    // @ts-ignore
    throw new abap.Classes['CX_SY_DYN_CALL_ILLEGAL_METHOD']();
  }

  return ret;
}