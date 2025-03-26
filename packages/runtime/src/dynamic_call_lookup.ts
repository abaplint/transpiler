import {ICharacter} from "./types/_character";

export function dynamicCallLookup(obj: any, methodName: ICharacter): any {
  const name = methodName.get().toLowerCase().trimEnd();
  // todo: escape interface and namespace characters in name?
  let ret = obj[name];

  if (ret === undefined) {
    // it might be local
    ret = obj["#" + name];
  }

  if (ret === undefined) {
    // @ts-ignore
    if (abap.Classes['CX_SY_DYN_CALL_ILLEGAL_METHOD'] === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_METHOD not found"; }
    // @ts-ignore
    throw new abap.Classes['CX_SY_DYN_CALL_ILLEGAL_METHOD'.trimEnd()]();
  }

  return ret;
}