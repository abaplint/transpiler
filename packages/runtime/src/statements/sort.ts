import {Table} from "../types";
import {eq, lt, gt} from "../compare";

export function sort(input: Table, options?: {descending?: boolean}) {
  const items = input.array();
  const descending = options?.descending === true ? true : false;
  items.sort((a, b) => {
    if (eq(a,b)) {
      return 0;
    } else if (descending && (gt(a,b)))  {
      return -1;
    } else if (!descending && lt(a ,b)) {
      return -1;
    } else {
      return 1;
    }
  });

  input.clear();

  for (const i of items) {
    input.append(i);
  }
}