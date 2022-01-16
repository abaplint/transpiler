import {clone} from "../clone";
import {DataReference} from "../types";

export interface ICreateDataOptions {
  table?: boolean,
  name?: string,
}

export function createData(target: DataReference, options?: ICreateDataOptions) {

  if (options?.name && options?.table) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      // todo, throw exception
      return;
    }
    // @ts-ignore
    target.assign(new abap.types.Table(abap.DDIC[options.name].type));
  } else if (options?.name) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      // todo, throw exception
      return;
    }
    // @ts-ignore
    target.assign(clone(abap.DDIC[options.name].type));
  } else {
    target.assign(clone(target.getType()));
  }
}