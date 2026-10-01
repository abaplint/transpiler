import {eq} from "../compare";
import {primaryKeyValues} from "../primary_key";
import {DecFloat34, Float, HashedTable, Integer, Integer8, Packed, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {insertInternal} from "./insert_internal";
import {ABAP} from "..";

declare const abap: ABAP;

function sumNumeric(found: any, source: any, keyValues: Set<any>): void {
  if (found instanceof Structure && source instanceof Structure) {
    for (const name of Object.keys(source.get())) {
      sumNumeric(found.get()[name], source.get()[name], keyValues);
    }
  } else if (keyValues.has(source)) {
    return;
  } else if (found instanceof Integer && source instanceof Integer) {
    found.set(found.get() + source.get());
  } else if (found instanceof Integer8 && source instanceof Integer8) {
    found.set(found.get() + source.get());
  } else if (found instanceof Packed && source instanceof Packed) {
    const decimals = found.getDecimals();
    const factor = 10n ** BigInt(decimals);
    const left = BigInt(found.toFixed(decimals).replace(".", ""));
    const right = BigInt(source.toFixed(decimals).replace(".", ""));
    const total = left + right;
    const magnitude = total < 0n ? -total : total;
    const fraction = decimals === 0 ? "" : "." + (magnitude % factor).toString().padStart(decimals, "0");
    found.set((total < 0n ? "-" : "") + (magnitude / factor).toString() + fraction);
  } else if (found instanceof Float && source instanceof Float) {
    found.set(found.getRaw() + source.getRaw());
  } else if (found instanceof DecFloat34 && source instanceof DecFloat34) {
    found.set(found.getRaw() + source.getRaw());
  }
}

export function collect(source: ICharacter | Structure | Table, target?: Table | HashedTable) {
  if (target === undefined && source instanceof Table) {
    target = source;
    source = source.getHeader() as ICharacter | Structure;
  }
  if (target === undefined) {
    throw new Error("COLLECT, no target specified");
  }

  const sourceKeys = primaryKeyValues(target, source);
  const matches = (row: any) => {
    const rowKeys = primaryKeyValues(target, row);
    return rowKeys.length === sourceKeys.length && rowKeys.every((key, index) => eq(key, sourceKeys[index]));
  };
  const rows = target.array();
  let index = rows.findIndex(matches);
  if (index >= 0) {
    sumNumeric(rows[index], source, new Set(sourceKeys));
    abap.builtin.sy.get().subrc.set(0);
  } else {
    insertInternal({table: target, data: source});
    index = target.array().findIndex(matches);
  }
  abap.builtin.sy.get().tabix.set(target instanceof HashedTable ? 0 : index + 1);
}
