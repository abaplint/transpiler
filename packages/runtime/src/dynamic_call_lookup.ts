import {ABAP} from ".";
import {ICharacter} from "./types/_character";

declare const abap: ABAP;

export function dynamicCallLookup(obj: any, methodName: string | ICharacter): any {
  let name = typeof methodName === "string" ? methodName : methodName.get().toLowerCase().trimEnd();
  name = name.replaceAll("~", "$").replaceAll("/", "$");

  let ret = obj[name];

  if (ret !== undefined) {
    ret = ret.bind(obj);
  } else if (obj.FRIENDS_ACCESS_INSTANCE !== undefined) {
    // it might be private, note this currently does not respect encapsulation properly
    // FRIENDS_ACCESS_INSTANCE is already .bind()'ed
    ret = obj.FRIENDS_ACCESS_INSTANCE[name];
  }

  if (ret === undefined) {
    if (abap.Classes['CX_SY_DYN_CALL_ILLEGAL_METHOD'] === undefined) { throw "CX_SY_DYN_CALL_ILLEGAL_METHOD not found"; }
    throw new abap.Classes['CX_SY_DYN_CALL_ILLEGAL_METHOD']();
  }

  return ret;
}