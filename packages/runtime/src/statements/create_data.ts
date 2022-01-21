import {clone} from "../clone";
import {ABAPObject, DataReference, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure;

export interface ICreateDataOptions {
  table?: boolean,
  name?: string,
  type?: PointerType,
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
  } else if (options?.type) {
    target.assign(clone(options.type));
  } else {
    target.assign(clone(target.getType()));
  }
}