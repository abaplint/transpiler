import {Character, Numc, String} from "../types";
import {compareDecimal, Decimal, parseDecimal} from "../types/_decimal";

function toDecimal(value: Numc | Character | String | string): Decimal {
  const text = typeof value === "string" ? value : value.get();
  const decimal = parseDecimal(text);
  if (decimal === undefined) {
    throw new Error("CONVT_NO_NUMBER");
  }
  return decimal;
}

function isText(value: unknown): value is Character | String | string {
  return typeof value === "string" || value instanceof Character || value instanceof String;
}

/** a key value of READ TABLE or a table expression is converted to the type of its
 * component first: a text for an n component becomes an n, its letters dropped, no
 * CONVT_NO_NUMBER. Other values are returned as they are. */
export function keyAsComponentType(component: unknown, value: unknown): unknown {
  if (component instanceof Numc && isText(value)) {
    return component.clone().set(value);
  }
  return value;
}

/** n compared with c, string or an n of another length: both operands are
 * numbers (calculation type p), a text that is no number is CONVT_NO_NUMBER.
 * Returns undefined when the rule does not apply. */
export function compareNumc(left: unknown, right: unknown): number | undefined {
  if (left instanceof Numc) {
    if (right instanceof Numc) {
      if (right.getLength() === left.getLength()) {
        return undefined;
      }
    } else if (isText(right) === false) {
      return undefined;
    }
  } else if (right instanceof Numc) {
    if (isText(left) === false) {
      return undefined;
    }
  } else {
    return undefined;
  }
  return compareDecimal(toDecimal(left as Numc), toDecimal(right as Numc));
}
