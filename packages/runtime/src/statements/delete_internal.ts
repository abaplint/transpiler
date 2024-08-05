import {FieldSymbol, HashedTable, Structure, Table} from "../types";
import {eq} from "../compare";
import {INumeric} from "../types/_numeric";
import {loop} from "./loop";

export interface IDeleteInternalOptions {
  where?: (i: any) => Promise<boolean>,
  index?: INumeric,
  adjacent?: boolean,
  comparing?: string[],
  fromValue?: any,
  from?: any,
  to?: any,
}

export async function deleteInternal(target: Table | HashedTable | FieldSymbol, options?: IDeleteInternalOptions): Promise<void> {
  let index = 0;

  if (target instanceof FieldSymbol) {
    target = target.getPointer() as Table;
    if (target === undefined) {
      throw new Error("deleteInternal, FS not assigned");
    }
  }

  if (options?.index
      && options?.where === undefined
      && options?.adjacent === undefined
      && options?.fromValue === undefined
      && options?.from === undefined
      && options?.to === undefined) {
    if (target.array()[options.index.get() - 1] === undefined) {
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(4);
      return;
    } else {
      target.deleteIndex(options.index.get() - 1);
      // @ts-ignore
      abap.builtin.sy.get().subrc.set(0);
      return;
    }
  }

  if (options?.to) {
    if (options?.from !== undefined || options?.where !== undefined) {
      throw "DeleteInternalTodo";
    }
    for (let i = 0; i < options.to.get(); i++) {
      target.deleteIndex(0);
    }
    return;
  }

  if (options?.adjacent === true) {
    if (target instanceof HashedTable) {
      throw new Error("delete adjacent, hashed table");
    }

    const array = target.array();

    for (let index = array.length - 1; index > 0; index--) {

      const prev = array[ index - 1];
      const i = array[ index ];

      if (options?.comparing) {
        let match = false;
        for (const compareField of options.comparing) {
          match = eq(prev.get()[compareField], i.get()[compareField]);
          if (!match) {
            break;
          }
        }
        if (match) {
          target.deleteIndex(index);
        }
      } else if (eq(prev, i) === true) {
        target.deleteIndex(index);
      }
    }
    return;
  }

  if (target instanceof HashedTable && options?.fromValue) {
    target.deleteFrom(options.fromValue);
    return;
  }

  // short form, "DELETE tab"
  if (options === undefined) {
    target.deleteIndex((target as Table).getCurrentLoopIndex());
    return;
  }

  for await (const i of loop(target)) {
    // @ts-ignore
    index = abap.builtin.sy.get().tabix.get() - 1;

    if (options?.where) {
      const row = i instanceof Structure ? i.get() : {table_line: i};
      if (await options.where(row) === true) {
        if (target instanceof HashedTable) {
          target.deleteFrom(i);
        } else {
          target.deleteIndex(index);
        }
      }
    } else if (options?.index && options.index.get() === index) {
      target.deleteIndex(options.index.get() - 1);
      break;
    } else if (options?.fromValue && eq(options.fromValue, i)) {
      target.deleteIndex(index);
    } else if (options?.from && options.from.get() <= index + 1) {
      target.deleteIndex(index);
    }
  }
}