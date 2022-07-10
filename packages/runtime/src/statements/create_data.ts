import {clone} from "../clone";
import {ABAPObject, DataReference, Date, FieldSymbol, Float, Integer, Structure, Table, Time} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure | FieldSymbol;

export interface ICreateDataOptions {
  table?: boolean,
  name?: string,
  type?: PointerType,
  typeHandle?: ABAPObject,
  likeLineOf?: FieldSymbol | Table,
}

export function createData(target: DataReference, options?: ICreateDataOptions) {
//  console.dir(options);

  if (options?.name && options?.table) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      // todo, throw exception CX_SY_CREATE_DATA_ERROR
      return;
    }
    // @ts-ignore
    target.assign(new abap.types.Table(abap.DDIC[options.name].type));
  } else if (options?.typeHandle) {
    switch (options.typeHandle.get().type_kind.get()) {
      case "F":
        target.assign(new Float());
        break;
      case "I":
        target.assign(new Integer());
        break;
      case "D":
        target.assign(new Date());
        break;
      case "T":
        target.assign(new Time());
        break;
      default:
        throw "CREATE DATA, unknown handle type";
    }
  } else if (options?.name) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      // @ts-ignore
      if (abap.Classes["CX_SY_CREATE_DATA_ERROR"] !== undefined) {
        // @ts-ignore
        throw new abap.Classes["CX_SY_CREATE_DATA_ERROR"]();
      } else {
        throw "Global class CX_SY_CREATE_DATA_ERROR not found";
      }
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
  } else {
    target.assign(clone(target.getType()));
  }
}