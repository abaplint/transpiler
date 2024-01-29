/* eslint-disable max-len */
import {ABAPObject, Character, Date, FieldSymbol, Float, HashedTable, String, Hex, Integer, Numc, Structure, Table, DataReference, toInteger, XString, Integer8, Packed} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

function compareTables(left: Table | HashedTable, right: Table | HashedTable): boolean {
  const leftArray = left.array();
  const rightArray = right.array();

  if (leftArray.length !== rightArray.length) {
    return false;
  }

  for (let i = 0; i < leftArray.length; i++) {
    const rowCompare = eq(leftArray[i], rightArray[i]);
    if (rowCompare === false) {
      return false;
    }
  }

  return true;
}

export function eq(
  left: number | string | ICharacter | INumeric | Float | String | ABAPObject | Structure | Hex | HashedTable | Table | FieldSymbol,
  right: number | string | ICharacter | INumeric | Float | String | ABAPObject | Structure | Hex | HashedTable | Table | FieldSymbol): boolean {
/*
  console.dir(left);
  console.dir(right);
*/
  if (right instanceof FieldSymbol) {
    return eq(left, right.getPointer()!);
  } else if (left instanceof FieldSymbol) {
    return eq(left.getPointer()!, right);
  }

// for performance, do the typicaly/easy cases first
  if (right instanceof Character) {
    if (left instanceof Character) {
      if (right.getLength() === left.getLength()) {
        return right.get() === left.get();
      } else {
        return right.getTrimEnd() === left.getTrimEnd();
      }
    } else if (left instanceof Integer) {
      return toInteger(right.get(), false) === left.get();
    } else if (left instanceof String
        || left instanceof XString
        || left instanceof Numc
        || left instanceof Hex) {
      return right.getTrimEnd() === left.get();
    }
  } else if (right instanceof String) {
    if (left instanceof Character) {
      return right.get() === left.getTrimEnd();
    } else if (left instanceof String) {
      return right.get() === left.get();
    }
  } else if (right instanceof Numc) {
    if (left instanceof Numc && right.getLength() === left.getLength()) {
      return right.get() === left.get();
    } else if (left instanceof Integer) {
      return left.get() === parseInt(right.get(), 10);
    }
  } else if (right instanceof Integer) {
    if (left instanceof Integer || left instanceof Integer8) {
      return right.get() === left.get();
    } else if (left instanceof Character) {
      return (parseInt(left.get(), 10) || 0) === right.get();
    } else if (left instanceof Numc) {
      return right.get() === parseInt(left.get(), 10);
    }
  } else if (right instanceof DataReference && left instanceof DataReference) {
    return right.getPointer() === left.getPointer();
  } else if (right instanceof Table
      || right instanceof HashedTable) {
    if (left instanceof Table
        || left instanceof HashedTable) {
      return compareTables(left, right);
    } else {
// this happens in dynamic/ANY typed scenarios?
      return false;
    }
  } else if (right instanceof Hex) {
    if (left instanceof Hex || left instanceof XString) {
      return right.get() === left.get();
    }
  } else if (right instanceof XString) {
    if (left instanceof Hex || left instanceof XString) {
      return right.get() === left.get();
    }
  } else if (right instanceof ABAPObject) {
    if (left instanceof ABAPObject) {
      return right.get() === left.get();
    }
  } else if (right instanceof Date) {
    if (left instanceof Date) {
      return right.get() === left.get();
    }
  } else if (right instanceof Packed) {
    if (left instanceof Packed) {
      return right.get() === left.get();
    }
  } else if (right instanceof Integer8) {
    if (left instanceof Integer
        || left instanceof Integer8
        || left instanceof Packed) {
      return right.get() === left.get();
    }
  } else if (right instanceof Float) {
    if (left instanceof Float) {
      return right.getRaw() === left.getRaw();
    } else if (left instanceof Integer) {
      return right.getRaw() === left.get();
    }
  }

  if (left instanceof Structure || right instanceof Structure) {
    if (!(right instanceof Structure)) {
      return eq((left as Structure).getCharacter(), right);
    } else if (!(left instanceof Structure)) {
      return eq(left, (right as Structure).getCharacter());
    }
    const l = left.get();
    const r = right.get();
    const leftKeys = Object.keys(l);
    const rightKeys = Object.keys(r);
    if (leftKeys.length !== rightKeys.length) {
      return false;
    }
    for (const k of leftKeys) {
      const e = eq(l[k], r[k]);
      if (e === false) {
        return false;
      }
    }
    return true;
  }

  let l: number | string | undefined = undefined;
  if (left instanceof Character) {
    l = left.getTrimEnd();
  } else if (left instanceof Date) {
    l = left.get().trimEnd();
  } else if (left instanceof Table || left instanceof HashedTable) {
    return false;
  } else if (typeof left === "object") {
    l = left.get();
  } else {
    l = left;
  }

  let r: number | string | undefined = undefined;
  if (right instanceof Character) {
    r = right.getTrimEnd();
  } else if (right instanceof Date) {
    r = right.get().trimEnd();
  } else if (typeof right === "object") {
    r = right.get();
  } else {
    r = right;
  }

  if (right instanceof Hex && typeof l === "number") {
    r = parseInt(right.get(), 16);
  } else if (left instanceof Hex && typeof r === "number") {
    l = parseInt(left.get(), 16);
  }

  if (right instanceof Float && left instanceof Float) {
    r = right.getRaw();
    l = left.getRaw();
  } else if (right instanceof Float && typeof l === "number") {
    r = right.getRaw();
  } else if (left instanceof Float) {
    if (typeof r === "number") {
      l = left.getRaw();
    } else if (typeof r === "string") {
      l = left.getRaw();
      r = Number(r);
    }
  }

  if (right instanceof Numc && left instanceof Integer) {
    l = left.get();
    r = parseInt(right.get(), 10);
  } else if (right instanceof Integer && left instanceof Numc) {
    r = right.get();
    l = parseInt(left.get(), 10);
  }

  // assumption: typically no casts are required, so start checking if the types doesnt match
  if (typeof l !== typeof r) {
    if (typeof l === "string" && typeof r === "number") {
      r = r.toString();
    } else if (typeof l === "number" && typeof r === "string") {
      if (r === "") {
        r = 0;
      } else if (r.includes(".")) {
        r = parseFloat(r);
      } else {
        r = parseInt(r, 10);
      }
    }
  }
/*
  console.dir(l);
  console.dir(r);
*/
  return l === r;
}