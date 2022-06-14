import {Context} from "../context";
import {Float, Structure} from "../types";
import {FieldSymbol} from "../types/field_symbol";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IWriteOptions {
  newLine?: boolean,
  skipLine?: boolean,
  target?: ICharacter,
  exponent?: ICharacter | INumeric,
  noGrouping?: boolean,
  noSign?: boolean,
}

export class WriteStatement {
  private readonly context: Context;

  public constructor(context: Context) {
    this.context = context;
  }

  public write(source: INumeric | ICharacter | FieldSymbol | string | number, options?: IWriteOptions) {
    if (options?.skipLine === true) {
      this.context.console.add("\n");
    } else {
      if (options?.newLine === true && this.context.console.get().length > 0) {
        this.context.console.add("\n");
      }

      let result = "";
      if (typeof source === "string" || typeof source === "number") {
        result = source.toString();
      } else if (source instanceof Structure) {
        const obj = source.get();
        for (const f in obj) {
          this.write(obj[f], {...options});
        }
      } else if (source instanceof Float) {
        if (options?.exponent?.get() === 0) {
          result = source.getRaw().toFixed(16).replace(".", ",");
        } else {
          result = source.get().toString();
        }
      } else {
        result = source.get().toString();
      }

      if (options?.target) {
        options.target.set(result);
      } else {
        this.context.console.add(result);
      }

    }
  }
}