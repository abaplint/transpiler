import {clone} from "../clone";
import {DataReference} from "../types";

export interface ICreateDataOptions {
  table?: boolean,
  name?: string,
}

export function createData(target: DataReference, _options?: ICreateDataOptions) {
  target.assign(clone(target.getType()));
}