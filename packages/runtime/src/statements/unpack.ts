import {throwError} from "../throw_error";
import {Character, Date, FieldSymbol, Packed} from "../types";
import {ICharacter} from "../types/_character";

export async function unpack(source: ICharacter | FieldSymbol | Packed,
                             target: ICharacter | FieldSymbol | Date) {

  if (source instanceof FieldSymbol) {
    const pointer = source.getPointer();
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return unpack(pointer, target);
  }

  if (target instanceof FieldSymbol) {
    const pointer = target.getPointer();
    if (pointer === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return unpack(source, pointer);
  }

  if (source instanceof Character && target instanceof Character) {
    const sourceValue = source.getTrimEnd().trimStart();

    if (sourceValue.length > 0 && /^\d+$/.test(sourceValue) === false) {
      throwError("CX_SY_CONVERSION_NO_NUMBER");
    }

    target.set(sourceValue.padStart(target.getLength(), "0"));
  } else if (source instanceof Packed && target instanceof Date) {
    target.set(source.get() + "");
  } else {
    throw new Error("unpack, transpiler runtime todo, types");
  }

}