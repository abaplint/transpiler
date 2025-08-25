import {DataReference, FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {ABAP} from "..";

declare const abap: ABAP;

/**
 * note target and source is switched vs the ABAP statement for acommodating expression REF
 */
export function getReference(
  target: FieldSymbol | DataReference,
  source: INumeric | ICharacter | Table | Structure | DataReference | FieldSymbol,
) {
//  console.dir(input);

  if (source instanceof FieldSymbol) {
    source = source.getPointer() as any;
    if (source === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
  }

  if (target instanceof FieldSymbol) {
    target = target.getPointer() as any;
    if (target === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
  }

  target = target as DataReference;
  target.assign(source as any);

  return target;
}