import {FieldSymbol, Structure} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IAssignInput {
  source: INumeric | ICharacter | Structure,
  target: FieldSymbol,
  component?: string | ICharacter,
}

export function assign(input: IAssignInput) {

  if (input.component) {
    if (!(input.source instanceof Structure)) {
      throw "ASSIGN, not a structure"; // todo, this should be a runtime error?
    }

    let component = input.component;
    if (typeof component !== "string") {
      component = component.get();
    }

    const result = input.source.get()[component.toLowerCase()];
    if (result === undefined) {
      // not a field in the structure
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
    } else {
      input.target.assign(result);
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(0);
    }

  } else {
    if (input.source instanceof FieldSymbol) {
      input.target.assign(input.source.getPointer());
    } else {
      input.target.assign(input.source);
    }
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
  }

}