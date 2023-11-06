import {Context} from "../context";
import {Float, Packed, Structure} from "../types";
import {FieldSymbol} from "../types/field_symbol";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IWriteOptions {
  newLine?: boolean,
  skipLine?: boolean,
  target?: ICharacter,
  exponent?: ICharacter | INumeric,
  // suppresses the thousands separators
  noGrouping?: boolean,
  noSign?: boolean,
}

export class WriteStatement {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public write(source: INumeric | ICharacter | FieldSymbol | string | number, options?: IWriteOptions) {
    let right = false;

    if (options?.skipLine === true) {
      this.context.console.add("\n");
    } else {
      if (options?.newLine === true && this.context.console.isEmpty() === false) {
        this.context.console.add("\n");
      }

      let result = "";
      if (typeof source === "string" || typeof source === "number") {
        result = source.toString();
      } else if (source instanceof Structure) {
        const obj = source.getCharacter();
        this.write(obj, {...options});
      } else if (source instanceof Float) {
        if (options?.exponent?.get() === 0) {
          const tens = source.getRaw().toFixed(0).length - 1;
          if (options.noSign === true  && source.getRaw() < 0) {
            result = source.getRaw().toFixed(17 - tens).replace(".", ",");
            result = result.replace("-", "");
          } else {
            result = source.getRaw().toFixed(16 - tens).replace(".", ",");
          }
        } else {
          result = source.get().toString();
        }
      } else if (source instanceof Packed) {
        const num = source.get();
        result = num.toFixed(source.getDecimals()).replace(".", ",");
        right = true;
      } else {
        result = source.get().toString();
      }

      if (options?.noSign === true) {
        result = result.replace("-", "");
      }

      if (options?.target) {
        if (right === true) {
          const len = options.target.get().length;
          options.target.set(" ".repeat(len - result.length) + result);
        } else {
          options.target.set(result);
        }
      } else {
        this.context.console.add(result);
      }

    }
  }
}