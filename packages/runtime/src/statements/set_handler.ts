import {ABAPObject, FieldSymbol} from "../types";
import {ICharacter} from "../types/_character";

export function setHandler(_methods: any[], forObject: ABAPObject | FieldSymbol, _activation?: ICharacter) {

  if (forObject instanceof FieldSymbol) {
    // Handle FieldSymbol case
    if (forObject.getPointer() === undefined) {
      throw new Error("SET HANDLER: Field symbol is not assigned");
    }
  }

  if (forObject instanceof ABAPObject && forObject.get() === undefined) {
    throw new Error("SET_HANDLER_FOR_NULL");
  }

  // todo
  return;
}