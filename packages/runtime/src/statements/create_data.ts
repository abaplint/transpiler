import {clone} from "../clone";
import {ABAPObject, DataReference, FieldSymbol, Structure, Table} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure | FieldSymbol;

export interface ICreateDataOptions {
  table?: boolean,
  name?: string,
  type?: PointerType,
  length?: INumeric,
  likeLineOf?: FieldSymbol | Table,
  like?: any,
}

function throwGlobalException(name: string) {
  // @ts-ignore
  if (abap.Classes[name] !== undefined) {
    // @ts-ignore
    throw new abap.Classes[name]();
  } else {
    throw `Global class ${name} not found`;
  }
}

export function createData(target: DataReference, options?: ICreateDataOptions) {
//  console.dir(options);

  if (options?.name && options?.table) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      throwGlobalException("CX_SY_CREATE_DATA_ERROR");
    }
    // @ts-ignore
    target.assign(new abap.types.Table(abap.DDIC[options.name].type));
  } else if (options?.name) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      throwGlobalException("CX_SY_CREATE_DATA_ERROR");
    }
    // @ts-ignore
    target.assign(clone(abap.DDIC[options.name].type));
  } else if (options?.type) {
    target.assign(clone(options.type));
  } else if (options?.likeLineOf) {
    if (options.likeLineOf instanceof FieldSymbol) {
      options.likeLineOf = options.likeLineOf.getPointer() as Table;
    }
    target.assign(clone(options.likeLineOf.getRowType()));
  } else if (options?.like) {
    if (options.like instanceof FieldSymbol) {
      options.like = options.like.getPointer();
    }
    target.assign(clone(options.like));
  } else {
    target.assign(clone(target.getType()));
  }
}