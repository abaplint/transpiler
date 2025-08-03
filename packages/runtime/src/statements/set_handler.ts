import {ABAP} from "..";
import {ABAPObject, FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";

declare const abap: ABAP;

export function setHandler(methods: any[], forObject: ABAPObject | FieldSymbol, activation?: ICharacter) {

  if (forObject instanceof FieldSymbol) {
    const pointer = forObject.getPointer();
    if (pointer === undefined) {
      throw new Error("CX_SY_ SOMETHING TODO");
    }
    return setHandler(methods, pointer, activation);
  } else if (forObject instanceof ABAPObject && forObject.get() === undefined) {
    throw new Error("SET_HANDLER_FOR_NULL");
  }

  const act = activation === undefined ? true : activation.get() === "X";

  // todo
  abap.eventing.setHandler({EVENT_NAME: "FOO", EVENT_CLASS: "PROG-ZFOOBAR-LCL"}, methods, forObject, act);
}