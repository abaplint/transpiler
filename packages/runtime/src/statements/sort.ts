import {Table} from "../types";
import {eq, lt} from "../compare";

export function sort(input: Table) {
  const items = input.array();

  items.sort((a, b) => {
    if (eq(a,b)) {
      return 0;
    } else if (lt(a ,b)) {
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