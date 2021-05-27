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
/*       } else if (source instanceof Integer) {
        //get the padded integer
        const lv_integer_value = source.get();
        let lv_integer_as_string = lv_integer_value.toString();
        lv_integer_as_string = lv_integer_as_string.padStart(10, " ");
        //add sign suffix
        const lv_sign_suffix = (lv_integer_value > 0) ? " " : "-";
        lv_integer_as_string = lv_integer_as_string + lv_sign_suffix;

        this.console.add(lv_integer_as_string); */
      } else {
        this.console.add(source.get().toString());
      }
    }
  }

}