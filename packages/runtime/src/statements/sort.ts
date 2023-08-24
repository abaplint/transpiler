import {FieldSymbol, HashedTable, Table, TableAccessType, TableRowType} from "../types";
import {eq, lt, gt} from "../compare";

export interface ISortOptions {
  descending?: boolean,
  skipSortedCheck?: boolean,
  by?: {component: string, descending?: boolean}[],
}

function compare(a: any, b: any, input: {component: string, descending?: boolean}) {
  const componentName = input.component;
  const descending = input.descending;

  let vala: any = undefined;
  let valb: any = undefined;

  if (componentName === "table_line") {
    vala = a.get();
    valb = b.get();
  } else if (componentName.includes("-")) {
    const sub = componentName.split("-");
    vala = a;
    valb = b;
    for (const s of sub) {
      vala = vala.get()[s];
      valb = valb.get()[s];
    }
  } else {
    vala = a.get()[componentName];
    valb = b.get()[componentName];
  }
  if (vala === undefined || valb === undefined) {
    throw new Error("sort compare, wrong component name, " + componentName);
  }

  if (descending && gt(vala, valb))  {
    return -1;
  } else if (!descending && lt(vala, valb)) {
    return -1;
  } else if (eq(vala, valb)) {
    return 0;
  } else {
    return 1;
  }
}

export function sort(input: Table | FieldSymbol | any[], options?: ISortOptions) {
//  console.dir(input);

  if (input instanceof FieldSymbol) {
    const pnt = input.getPointer();
    if (pnt === undefined) {
      throw new Error("GETWA_NOT_ASSIGNED");
    }
    sort(pnt, options);
    return;
  } else if (options?.skipSortedCheck !== true
      && input instanceof Table
      && input.getOptions()?.primaryKey?.type === TableAccessType.sorted) {
    throw new Error("SORT called on sorted table");
  }

  if (input instanceof HashedTable) {
    throw new Error("Sort hashed table, ugh?");
  }

  if (options?.by) {
    if (options.by.length === 0) {
      throw "SortByLengthZero";
    }

    input.sort((a, b) => {
      for (const c of options.by || []) {
        const res = compare(a, b, c);
        if (res !== 0) {
          return res;
        }
      }
      return 0;
    });

  } else {
    const descending = options?.descending === true;
    input.sort((a: TableRowType, b: TableRowType) => {
      if (eq(a,b)) {
        return 0;
      } else if (descending && gt(a,b))  {
        return -1;
      } else if (!descending && lt(a ,b)) {
        return -1;
      } else {
        return 1;
      }
    });
  }

}
