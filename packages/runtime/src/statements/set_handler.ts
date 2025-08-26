import {ABAP} from "..";
import {ABAPEventReference} from "../abap_eventing";
import {ABAPObject, FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";

declare const abap: ABAP;

export function setHandler(eventReference: ABAPEventReference,
                           methods: any[],
                           forObject: ABAPObject | FieldSymbol,
                           activation?: ICharacter) {

  if (forObject instanceof FieldSymbol) {
    const pointer = forObject.getPointer();
    if (pointer === undefined) {
      throw new Error("CX_SY SOMETHING TODO");
    }
    return setHandler(eventReference, methods, pointer, activation);
  } else if (forObject instanceof ABAPObject && forObject.get() === undefined) {
    throw new Error("SET_HANDLER_FOR_NULL");
  }

  const act = activation === undefined ? true : activation.get() === "X";

  abap.eventing.setHandler(eventReference, methods, forObject, act);
}