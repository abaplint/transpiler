import {ABAPObject, DataReference, Date, DecFloat34, FieldSymbol, Float, HashedTable, Integer, Integer8, String, Structure, Table, TableAccessType, Time, XString} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";
import {ABAP} from "..";

declare const abap: ABAP;

export interface IAssignInput {
  source?: INumeric | ICharacter | Table | Structure | DataReference | FieldSymbol,
  target: FieldSymbol,
  dynamicName?: string,
  dynamicSource?: ICharacter, // first part only
  casting?: boolean,
  component?: string | ICharacter,
  // a statically specified data object, ASSIGN does not set sy-subrc then
  keepSubrc?: boolean,
}

/** the kind of a table a field symbol is declared with, or a table holds: undefined
 *  when the runtime does not know it */
function tableKind(t: any): TableAccessType | undefined {
  if (t instanceof HashedTable) {
    return TableAccessType.hashed;
  } else if (t instanceof Table) {
    return t.getOptions()?.primaryKey?.type;
  }
  return undefined;
}

// elementary types no generic type (any, data, clike, csequence, simple, numeric, c, n, x, p)
// is transpiled to, so a field symbol of one of them is declared with that type
const EXPLICIT_ELEMENTARY: readonly unknown[] = [Integer, Integer8, String, XString, Float, DecFloat34, Date, Time];

/** a system refuses an ASSIGN whose target field symbol is declared with a type the source
 *  does not have: a table of another kind is ASSIGN_TYPE_CONFLICT, an elementary type of
 *  another kind is ASSIGN_CAST_ILLEGAL_CAST unless CASTING is given; both are uncatchable */
function checkTypeConflict(target: FieldSymbol, source: any, casting: boolean | undefined): void {
  const targetType = target.getType();
  if (targetType === undefined || source === null || typeof source !== "object") {
    return;
  }

  const targetKind = tableKind(targetType);
  if (targetKind !== undefined) {
    const sourceKind = tableKind(source);
    if (sourceKind === undefined
        || sourceKind === TableAccessType.index
        || sourceKind === TableAccessType.any) {
      // a source table the runtime cannot place is accepted, as before
      return;
    }
    const conflict = targetKind === TableAccessType.index ? sourceKind === TableAccessType.hashed
      : targetKind === TableAccessType.any ? false
      : sourceKind !== targetKind;
    if (conflict) {
      throw new Error("ASSIGN_TYPE_CONFLICT");
    }
    return;
  }

  if (casting === true
      || EXPLICIT_ELEMENTARY.includes(targetType.constructor) === false
      || source.constructor === targetType.constructor) {
    return;
  }
  if (source instanceof Table || source instanceof HashedTable || source instanceof Structure
      || source instanceof DataReference || source instanceof ABAPObject || source instanceof FieldSymbol) {
    // abaplint types a FIELD-SYMBOLS TYPE STANDARD TABLE OF i by its row type: only an
    // elementary source is compared
    return;
  }
  throw new Error("ASSIGN_CAST_ILLEGAL_CAST");
}

export function assign(input: IAssignInput) {
//  console.dir(input);

/*
  if (input.source instanceof FieldSymbol) {
    return assign({...input, source: input.source.getPointer()});
  }
*/

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
          const upperS = s.toUpperCase().trimEnd();
          if (upperS === "*") {
            if (!(input.dynamicSource instanceof DataReference)
                && !(input.dynamicSource instanceof FieldSymbol)) {
              // an earlier segment resolved to nothing, or to something that has
              // no address, so the name cannot be followed: sy-subrc 4
              abap.builtin.sy.get().subrc.set(4);
              return;
            }
            // @ts-ignore
            input.dynamicSource = input.dynamicSource.dereference();
          } else if (upperS === "TABLE_LINE" && input.dynamicSource instanceof DataReference) {
            // @ts-ignore
            input.dynamicSource = input.dynamicSource.getPointer();
            if (input.dynamicSource instanceof Table || input.dynamicSource instanceof HashedTable) {
              const options = input.dynamicSource.getOptions();
              if (options?.withHeader === true) {
                // @ts-ignore
                input.dynamicSource = input.dynamicSource.getHeader();
              } else {
                input.dynamicSource = undefined;
              }
            }
          } else {
            let sourceContainer: any = input.dynamicSource;
            if (sourceContainer instanceof DataReference) {
              sourceContainer = sourceContainer.getPointer();
            }
            if (sourceContainer instanceof Table || sourceContainer instanceof HashedTable) {
              if (sourceContainer.getOptions()?.withHeader === true) {
                sourceContainer = sourceContainer.getHeader();
              } else {
                abap.builtin.sy.get().subrc.set(4);
                return;
              }
            }
            const source = sourceContainer?.get();
            if (source === undefined) {
              abap.builtin.sy.get().subrc.set(4);
              return;
            }
            const componentName = sourceContainer instanceof Structure
              ? s.toLowerCase()
              : s.toLowerCase().replace(/[~\\/]/g, "$");
            let next = source[componentName as any];
            if (next === undefined && source.FRIENDS_ACCESS_INSTANCE !== undefined) {
              // it might be private, note this currently does not respect encapsulation properly
              next = source.FRIENDS_ACCESS_INSTANCE[componentName];
            }
            // @ts-ignore
            input.dynamicSource = next;
          }
        }
      } else {
        abap.builtin.sy.get().subrc.set(4);
        return;
      }
    } else if (input.dynamicName.includes("=>")) {
      const split = input.dynamicName.split("=>");

      const clas = abap.Classes[split[0].toUpperCase()];
      if (clas === undefined) {
        abap.builtin.sy.get().subrc.set(4);
        return;
      }

      if (clas[split[1].toLowerCase()] !== undefined) {
        input.target.assign(clas[split[1].toLowerCase()]);
        abap.builtin.sy.get().subrc.set(0);
        return;
      } else if(clas[split[0].toLowerCase() + "$" + split[1].toLowerCase()] !== undefined) {
        input.target.assign(clas[split[0].toLowerCase() + "$" + split[1].toLowerCase()]);
        abap.builtin.sy.get().subrc.set(0);
        return;
      }
    }

    if (input.dynamicSource) {
      input.target.assign(input.dynamicSource);
      abap.builtin.sy.get().subrc.set(0);
    } else {
      abap.builtin.sy.get().subrc.set(4);
    }
  } else if (input.component) {
    if (input.source instanceof FieldSymbol || input.source instanceof DataReference) {
      input.source = input.source.getPointer() as any;
      assign(input);
      return;
    } else if (!(input.source instanceof Structure)
        && !(input.source instanceof Table)) {
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
      // the whole name first: a structure built at runtime may carry a
      // component whose NAME has a dash (RAP's %element-<field>); then the
      // dash as a path, where a missing step is sy-subrc 4
      const name = component.toLowerCase().trimEnd();
      const source: any = input.source;
      result = source.get()[name];
      if (result === undefined && name.includes("-")) {
        result = source;
        for (const s of name.split("-")) {
          result = result?.get()[s];
        }
      }
    }

    if (result === undefined) {
      // not a field in the structure
      abap.builtin.sy.get().subrc.set(4);
    } else {
      input.target.assign(result);
      abap.builtin.sy.get().subrc.set(0);
    }

  } else {
//    console.dir(input);
    if (input.source instanceof FieldSymbol) {
      const pnt = input.source.getPointer();
      if (pnt === undefined) {
        throw new Error("GETWA_NOT_ASSIGNED");
      }
      checkTypeConflict(input.target, pnt, input.casting);
      input.target.assign(pnt);
      abap.builtin.sy.get().subrc.set(0);
    } else if (input.source === undefined) {
      abap.builtin.sy.get().subrc.set(4);
    } else {
      if (input.source instanceof Table && input.source.getOptions()?.withHeader === true) {
        input.target.assign(input.source.getHeader());
      } else if ((input.target.getType() instanceof Table || input.target.getType() instanceof HashedTable)
          && !(input.source instanceof Table)
          && !(input.source instanceof HashedTable)) {
        throw new Error("ASSIGN_TYPE_CONFLICT");
      } else {
        checkTypeConflict(input.target, input.source, input.casting);
        if (input.casting) {
          input.target.setCasting();
        }
        input.target.assign(input.source);
      }
      if (input.keepSubrc !== true) {
        abap.builtin.sy.get().subrc.set(0);
      }
    }

  }

}