import {clone} from "../clone";
import {FieldSymbol} from "./field_symbol";
import {Table} from "./table";
import {ICharacter} from "./_character";
import {INumeric} from "./_numeric";

export class Structure {
  private readonly value: any;

  public constructor(fields: any) {
    this.value = fields;
  }

  public clear() {
    for (const f in this.value) {
      // @ts-ignore
      this.value[f].clear();
    }
    return this;
  }

  public set(input: Structure | string | INumeric | Table | ICharacter | FieldSymbol | undefined) {
    if (input === undefined) {
      return;
    }

    if (input instanceof FieldSymbol) {
      this.set(input.getPointer());
    } else if (input instanceof Table) {
      throw "Structure, input is a table";
    } else if (input instanceof Structure) {
      const obj = input.get();
      for (const f in obj) {
        // @ts-ignore
        this.value[f].set(clone(obj[f]));
      }
    } else {
      this.setCharacter(input);
    }

    return this;
  }

  private setCharacter(input: string | ICharacter | INumeric) {
    this.clear();

    let val = input;
    if (typeof val !== "string") {
      val = val.get() + "";
    }

    for (const key of Object.keys(this.value)) {
      const targetLength = this.value[key].getLength();
      this.value[key].set(val.substr(0, targetLength));
      val = val.substr(targetLength);
    }
  }

  public get() {
    return this.value;
  }
}