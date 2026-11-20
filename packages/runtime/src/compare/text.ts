import {Character, FieldSymbol, String, Structure} from "../types";
import {eq} from "./eq";
import {lt} from "./lt";

let collator: Intl.Collator | undefined;

/** AS TEXT: character-like values are compared by the text environment instead of by code.
 *  Intl.Collator("en") gives the order of a system with logon language EN, also for umlauts,
 *  accents, case and punctuation. Structures are compared component by component, other types
 *  as usual. Returns a negative number, zero or a positive number */
export function compareText(a: any, b: any): number {
  if (a instanceof FieldSymbol) {
    if (a.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return compareText(a.getPointer(), b);
  } else if (b instanceof FieldSymbol) {
    if (b.getPointer() === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    return compareText(a, b.getPointer());
  }

  if (a instanceof Structure && b instanceof Structure) {
    const valb = b.get();
    for (const [name, value] of Object.entries(a.get())) {
      const result = compareText(value, valb[name]);
      if (result !== 0) {
        return result;
      }
    }
    return 0;
  }

  const texta = textValue(a);
  const textb = textValue(b);
  if (texta !== undefined && textb !== undefined) {
    if (collator === undefined) {
      collator = new Intl.Collator("en");
    }
    const result = collator.compare(texta, textb);
    if (result !== 0) {
      return result;
    }
    // values the collator does not tell apart are ordered by code
    return texta < textb ? -1 : texta > textb ? 1 : 0;
  }

  return eq(a, b) ? 0 : lt(a, b) ? -1 : 1;
}

function textValue(value: any): string | undefined {
  if (value instanceof Character) {
    return value.getTrimEnd();
  } else if (value instanceof String) {
    return value.get();
  } else if (typeof value === "string") {
    return value;
  }
  return undefined;
}
