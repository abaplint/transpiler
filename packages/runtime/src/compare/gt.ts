import {parse} from "../operators/_parse";
import {ABAPObject, Character, DataReference, DecFloat34, FieldSymbol, Float, HashedTable, Hex, HexUInt8, Integer8, Structure, Table, XString} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {Integer} from "../types/integer";
import {compareDecFloat34} from "../operators/_decfloat";

// references have no value to order by, and a sorted key over a REF TO component is
// not allowed; a hashed secondary key over one is, and the runtime keeps a hashed
// secondary key as a sorted index too, so the sort and the binary search need one
// order they agree on: every target gets a number on first use, which stands for
// the rest of its life
const referenceIds = new WeakMap<object, number>();
let nextReferenceId = 1;

/** the position of a reference target in the identity order; an unbound reference is first */
function referenceId(target: object | undefined): number {
  if (target === undefined || target === null) {
    return 0;
  }
  let id = referenceIds.get(target);
  if (id === undefined) {
    id = nextReferenceId++;
    referenceIds.set(target, id);
  }
  return id;
}

export function gt(
  left: number | string | ICharacter | INumeric | ABAPObject | Structure | Table | HashedTable | Integer8,
  right: number | string | ICharacter | INumeric | ABAPObject | Structure | Table | HashedTable | Integer8): boolean {

  if (left instanceof DecFloat34 || right instanceof DecFloat34) {
    // exact, getRaw() would compare the nearest doubles
    const compared = compareDecFloat34(left, right);
    if (compared !== undefined) {
      return compared > 0;
    }
  }

  if (right instanceof Integer) {
    if (left instanceof Integer) {
      return left.get() > right.get();
    } else if (left instanceof Float) {
      return left.getRaw() > right.get();
    } else if (left instanceof Character) {
      return parse(left) > right.get();
    } else if (left instanceof Integer8) {
      const l = left.get();
      const r = BigInt(right.get());
      return l > r;
    }
  } else if (right instanceof Float) {
    if (left instanceof Float) {
      return left.getRaw() > right.getRaw();
    } else if (left instanceof Integer) {
      return left.get() > right.getRaw();
    } else if (left instanceof Integer8) {
      const l = left.get();
      const r = BigInt(right.getRaw());
      return l > r;
    }
  } else if (right instanceof Integer8) {
    if (left instanceof Table || left instanceof HashedTable) {
      throw new Error("runtime_todo, gt TABLE");
    }
    const l = left instanceof Integer8 ? left.get() : BigInt(parse(left as any));
    const r = right instanceof Integer8 ? right.get() : BigInt(parse(right));
    return l > r;
  }

  if (left instanceof FieldSymbol) {
    if (left.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return gt(left.getPointer(), right);
  } else if (right instanceof FieldSymbol) {
    if (right.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return gt(left, right.getPointer());
  }

  if (left instanceof DataReference && right instanceof DataReference) {
    return referenceId(left.getPointer()) > referenceId(right.getPointer());
  } else if (left instanceof ABAPObject && right instanceof ABAPObject) {
    return referenceId(left.get()) > referenceId(right.get());
  }

  if (left instanceof Table || right instanceof Table || left instanceof HashedTable || right instanceof HashedTable) {
    throw new Error("runtime_todo, gt TABLE");
  } else if (left instanceof Hex || right instanceof Hex || left instanceof HexUInt8 || right instanceof HexUInt8) {
    return gt_with_hex(left, right);
  }

  if (left instanceof Integer8) {
    const l = left.get();
    const r = BigInt(parse(right as any));
    return l > r;
  }

  if (left instanceof Structure || right instanceof Structure) {
    if (!(right instanceof Structure)) {
      return gt((left as Structure).getCharacter(), right);
    } else if (!(left instanceof Structure)) {
      return gt(left, (right as Structure).getCharacter());
    }
  }

  let l: number | string | undefined = undefined;
  if (typeof left === "number" || typeof left === "string") {
    l = left;
  } else if (left instanceof Float || left instanceof DecFloat34) {
    l = left.getRaw();
  } else {
    l = left.get();
  }

  let r: number | string | undefined = undefined;
  if (typeof right === "number" || typeof right === "string") {
    r = right;
  } else if (right instanceof Float || right instanceof DecFloat34) {
    r = right.getRaw();
  } else {
    r = right.get();
  }

  // a character operand is converted to the type of the numeric one it is
  // compared with: '0.5' against a float is 0.5, not 0. parse() knows the
  // rules, and an empty string is 0 there too
  if (typeof l === "string" && typeof r === "number") {
    l = parse(l);
  } else if (typeof l === "number" && typeof r === "string") {
    r = parse(r);
  }

  if (l === undefined) {
    return true; // todo, not sure this is correct
  } else if (r === undefined) {
    return true; // todo, not sure this is correct
  }

  return l > r;
}

///////////////////////////////////////////////////////////////////

function gt_with_hex(
  left: number | string | ICharacter | INumeric | ABAPObject | Structure | Table | Integer8,
  right: number | string | ICharacter | INumeric | ABAPObject | Structure | Table | Integer8): boolean {

  let left_hex = get_hex_from_parameter(left);
  let right_hex = get_hex_from_parameter(right);
  if ((left instanceof Hex || left instanceof HexUInt8) && (right instanceof Hex || right instanceof HexUInt8)) {
    // two x fields of different lengths: the shorter one is padded with 00 on the right
    const length = Math.max(left_hex.length, right_hex.length);
    left_hex = left_hex.padEnd(length, "0");
    right_hex = right_hex.padEnd(length, "0");
  }
  return left_hex > right_hex;
}

function get_hex_from_parameter(
  comparison_part: number | string | ICharacter | INumeric | ABAPObject | Structure | Table | Integer8): string {
// todo: optimize?
  let hex_from_parameter = "";

  switch (typeof comparison_part) {
    case "number":
      hex_from_parameter = comparison_part.toString(16);
      break;
    case "string":
      hex_from_parameter = comparison_part.split("")
        .map(c => c.charCodeAt(0).toString(16).padStart(2, "0"))
        .join("");
      break;
    case "object":
      if (comparison_part instanceof Hex
          || comparison_part instanceof XString
          || comparison_part instanceof HexUInt8
          || comparison_part instanceof Character) {
        hex_from_parameter = comparison_part.get();
      } else if (comparison_part instanceof Integer) {
        hex_from_parameter = comparison_part.get().toString(16).toUpperCase();
        if (hex_from_parameter.length % 2 === 1) {
          hex_from_parameter = "0" + hex_from_parameter;
        }
      } else {
        throw new Error("runtime_todo, gt hex1");
      }

      break;
    default:
      throw new Error("runtime_todo, gt hex2");

  }

  return hex_from_parameter;
}