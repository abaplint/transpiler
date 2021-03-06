import {Structure} from "../types";
import {FieldSymbol} from "../types/field_symbol";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export function write(source: INumeric | ICharacter | FieldSymbol | string | number, options?: { newLine?: boolean, skipLine?: boolean }) {
  if (options?.skipLine === true) {
    this.console.add("\n");
  } else {
    if (options?.newLine === true && this.console.get().length > 0) {
      this.console.add("\n");
    }
    if (typeof source === "string" || typeof source === "number") {
      this.console.add(source.toString());
    } else {
      if (source instanceof Structure) {
        const obj = source.get();
        for (const f in obj) {
          // @ts-ignore
          abap.statements.write(obj[f]);
        }
      } else {
        this.console.add(source.get().toString());
      }
    }
  }

}