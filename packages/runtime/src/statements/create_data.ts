import {clone} from "../clone";
import {DataReference} from "../types";


export function createData(target: DataReference) {
  target.assign(clone(target.getType()));
}