import {FieldSymbol, Structure} from "../types";

export function moveCorresponding(source: Structure | FieldSymbol, target: Structure | FieldSymbol): void {
  if (source instanceof FieldSymbol) {
    if (source.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    moveCorresponding(source.getPointer(), target);
    return;
  }
  if (target instanceof FieldSymbol) {
    if (target.isAssigned() === false) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    moveCorresponding(source, target.getPointer());
    return;
  }

  for (const n in source.get()) {
    if (target.get()[n] instanceof Structure) {
      moveCorresponding(source.get()[n], target.get()[n]);
    } else {
      target.get()[n]?.set(source.get()[n]);
    }
  }
}