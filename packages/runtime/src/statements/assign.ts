import {ABAPObject, DataReference, FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

export interface IAssignInput {
  source?: INumeric | ICharacter | Table | Structure | DataReference,
  target: FieldSymbol,
  dynamicName?: string,
  dynamicSource?: ICharacter, // first part only
  casting?: boolean,
  component?: string | ICharacter,
}

export function assign(input: IAssignInput) {
//  console.dir(input);
  if (input.dynamicName) {
    if (input.dynamicSource instanceof FieldSymbol) {
      input.dynamicSource = input.dynamicSource.getPointer();
    }

    input.dynamicName = input.dynamicName.trimEnd();

    if (input.dynamicName.includes("->")) {
      if (input.dynamicSource instanceof ABAPObject || input.dynamicSource instanceof DataReference) {
        const split = input.dynamicName.split(/->|-/);
        split.shift();
        for (const s of split) {
          if (s === "*") {
            // @ts-ignore
            input.dynamicSource = input.dynamicSource.dereference();
          } else {
            // @ts-ignore
            const source = input.dynamicSource.get();
            if (source === undefined) {
              // @ts-ignore
              abap.builtin.sy.get().subrc.set(4);
              return;
            }
            input.dynamicSource = source[s.toLowerCase().replace(/[~\\/]/g, "$") as any];
          }
        }
      } else {
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(4);
        return;
      }
    } else if (input.dynamicName.includes("=>")) {
      const split = input.dynamicName.split("=>");

      // @ts-ignore
      const clas = abap.Classes[split[0].toUpperCase()];
      if (clas === undefined) {
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(4);
        return;
      }

      if (clas[split[1].toLowerCase()] !== undefined) {
        input.target.assign(clas[split[1].toLowerCase()]);
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(0);
        return;
      } else if(clas[split[0].toLowerCase() + "$" + split[1].toLowerCase()] !== undefined) {
        input.target.assign(clas[split[0].toLowerCase() + "$" + split[1].toLowerCase()]);
        // @ts-ignore
        abap.builtin.sy.get().subrc.set(0);
        return;
      }
    }

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
    } else if (!(input.source instanceof Structure)
        && !(input.source instanceof Table)) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
      return;
    }

    let component = input.component;
    if (typeof component !== "string") {
      component = component.get();
    }
    if (input.source instanceof Table) {
      if (input.source.getOptions()?.withHeader === true) {
        input.source = input.source.getHeader();
      } else {
        // result is the table itself, no change of input.source
      }
    }

    let result: any = undefined;
    if (typeof component === "number") {
      if (component === 0) {
        result = input.source;
      } else if (input.source instanceof Structure) {
        const structure_as_object = input.source.get();
        const keys = Object.keys(structure_as_object);
        const component_name = keys[component - 1];
        result = structure_as_object[component_name];
      }
    } else if (!(input.source instanceof Table)){
      const split = component.toLowerCase().trimEnd().split("-");
      result = input.source;
      for (const s of split) {
        result = result.get()[s];
      }
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
//    console.dir(input);
    if (input.source instanceof FieldSymbol) {
      const pnt = input.source.getPointer();
      if (pnt === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      input.target.assign(pnt);
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(0);
    } else if (input.source === undefined) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
    } else {
      if (input.source instanceof Table && input.source.getOptions()?.withHeader === true) {
        input.target.assign(input.source.getHeader());
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

}