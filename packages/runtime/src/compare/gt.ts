import {ABAPObject, Float, Hex, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {Integer} from "../types/integer";


export function gt(
  left: number | string | ICharacter | INumeric | ABAPObject | Structure | Table,
  right: number | string | ICharacter | INumeric | ABAPObject | Structure | Table): boolean {

  if (left instanceof Table || right instanceof Table) {
    throw "runtime_todo, gt TABLE";
  }
  if (left instanceof Hex || right instanceof Hex) {
    return gt_with_hex(left, right);
  }


  let l: number | string | undefined = undefined;
  if (typeof left === "number" || typeof left === "string") {
    l = left;
  } else if (left instanceof Float) {
    l = left.getRaw();
  } else {
    l = left.get();
  }

  let r: number | string | undefined = undefined;
  if (typeof right === "number" || typeof right === "string") {
    r = right;
  } else if (right instanceof Float) {
    r = right.getRaw();
  } else {
    r = right.get();
  }

  if (typeof l === "string" && typeof r === "number") {
    if (l === "") {
      l = 0;
    } else {
      l = parseInt(l, 10);
    }
  } else if (typeof l === "number" && typeof r === "string") {
    if (r === "") {
      r = 0;
    } else {
      r = parseInt(r, 10);
    }
  }

  if (l === undefined) {
    return true; // todo, not sure this is correct
  }
  if (r === undefined) {
    return true; // todo, not sure this is correct
  }

  return l > r;
}

function gt_with_hex(
  left: number | string | ICharacter | INumeric | ABAPObject | Structure | Table,
  right: number | string | ICharacter | INumeric | ABAPObject | Structure | Table): boolean {

  const left_hex = get_hex_from_parameter(left);
  const right_hex = get_hex_from_parameter(right);
  return left_hex > right_hex;
}

function get_hex_from_parameter(comparison_part: number | string | ICharacter | INumeric | ABAPObject | Structure | Table): string {
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
      if (comparison_part instanceof Hex) {
        hex_from_parameter = comparison_part.get();
      } else if (comparison_part instanceof Integer) {
        hex_from_parameter = comparison_part.get().toString(16).toUpperCase();
        if (hex_from_parameter.length % 2 === 1) {
          hex_from_parameter = "0" + hex_from_parameter;
        }
      } else {
        throw "runtime_todo, gt hex";
      }

      break;
    default:
      throw "runtime_todo, gt hex";

  }

  return hex_from_parameter;
}