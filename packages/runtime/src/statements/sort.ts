import {Table} from "../types";
import {eq, lt, gt} from "../compare";

export interface ISortOptions {
  descending?: boolean,
  by?: {component: string, descending?: boolean}[],
}

function compare(a: any, b: any, input: {component: string, descending?: boolean}) {
  const componentName = input.component;
  const descending = input.descending;

  const vala = a.get()[componentName];
  const valb = b.get()[componentName];

  if (eq(vala,valb)) {
    return 0;
  } else if (descending && gt(vala, valb))  {
    return -1;
  } else if (!descending && lt(vala, valb)) {
    return -1;
  } else {
    return 1;
  }
}

export function sort(input: Table, options?: ISortOptions) {
  const items = input.array();

  if (options?.by) {
    if (options.by.length === 0) {
      throw "SortByLengthZero";
    }

    items.sort((a, b) => {
      for (const c of options.by || []) {
        const res = compare(a, b, c);
        if (res !== 0) {
          return res;
        }
      }
      return 0;
    });

  } else {
    const descending = options?.descending === true ? true : false;
    items.sort((a, b) => {
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

  input.clear();

  for (const i of items) {
    input.append(i);
  }
}
