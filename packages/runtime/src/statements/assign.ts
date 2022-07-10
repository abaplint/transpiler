import {DataReference, FieldSymbol, Structure} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IAssignInput {
  source?: INumeric | ICharacter | Structure | DataReference,
  target: FieldSymbol,
  dynamicText?: string | ICharacter,
  dynamicSource?: ICharacter,
  casting?: boolean,
  component?: string | ICharacter,
}

export function assign(input: IAssignInput) {
  console.dir(input);
  if (input.dynamicText) {
    // todo, check for "->"'s in input text
    if (input.dynamicSource) {
      input.target.assign(input.dynamicSource);
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(0);
    } else {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
    }
  } else if (input.component) {
    if (input.source instanceof FieldSymbol || input.source instanceof DataReference) {
      input.source = input.source.getPointer() as any;
      assign(input);
      return;
    } else if (!(input.source instanceof Structure)) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
      return;
    }

    let component = input.component;
    if (typeof component !== "string") {
      component = component.get();
    }
    let result: any = undefined;
    if (typeof component === "number") {
      const structure_as_object = input.source.get();
      const keys = Object.keys(structure_as_object);
      const component_name = keys[(component - 1)];
      result = structure_as_object[component_name];
    } else {
      result = input.source.get()[component.toLowerCase()];

    }

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
      if (input.casting) {
        input.target.setCasting();
      }
      input.target.assign(input.source);
    }
    // @ts-ignore
    abap.builtin.sy.get().subrc.set(0);
  }

}