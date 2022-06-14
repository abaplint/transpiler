import {Context} from "../context";
import {Structure} from "../types";
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
      if (typeof source === "string" || typeof source === "number") {
        if (options?.target) {
          options.target.set(source.toString());
        } else {
          this.context.console.add(source.toString());
        }
      } else {
        if (source instanceof Structure) {
          const obj = source.get();
          for (const f in obj) {
            // @ts-ignore
            abap.statements.write(obj[f]);
          }
        } else {
          if (options?.target) {
            options.target.set(source.get().toString());
          } else {
            this.context.console.add(source.get().toString());
          }
        }
      }
    }
  }
}