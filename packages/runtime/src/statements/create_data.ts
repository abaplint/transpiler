import {clone} from "../clone";
import {throwError} from "../throw_error";
import {ABAPObject, Character, DataReference, Date, String, FieldSymbol, Float, Integer, Structure, Table, Time, XString, Hex, Packed} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure | FieldSymbol;

export interface ICreateDataOptions {
  table?: boolean,
  name?: string,
  type?: PointerType,
  typeName?: string,
  length?: INumeric,
  likeLineOf?: FieldSymbol | Table,
  like?: any,
}

export function createData(target: DataReference, options?: ICreateDataOptions) {
//  console.dir(options);

  if (options?.name && options?.table) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      throwError("CX_SY_CREATE_DATA_ERROR");
    }
    // @ts-ignore
    target.assign(new abap.types.Table(abap.DDIC[options.name].type));
  } else if (options?.name) {
    // @ts-ignore
    if (abap.DDIC[options.name] === undefined) {
      throwError("CX_SY_CREATE_DATA_ERROR");
    }
    // @ts-ignore
    target.assign(clone(abap.DDIC[options.name].type));
  } else if (options?.typeName) {
    switch (options.typeName) {
      case "C":
        {
          let length = 1;
          if (options.length) {
            length = options.length.get();
          }
          target.assign(new Character({length: length}));
        }
        break;
      case "X":
        {
          let length = 1;
          if (options.length) {
            length = options.length.get();
          }
          target.assign(new Hex({length: length}));
        }
        break;
      case "P":
        {
          let length = 1;
          if (options.length) {
            length = options.length.get();
          }
          target.assign(new Packed({length: length}));
        }
        break;
      case "F":
        target.assign(new Float());
        break;
      case "D":
        target.assign(new Date());
        break;
      case "T":
        target.assign(new Time());
        break;
      case "I":
        target.assign(new Integer());
        break;
      case "STRING":
        target.assign(new String());
        break;
      case "XSTRING":
        target.assign(new XString());
        break;
      default:
        if (options.typeName.includes("=>")) {
          const [className, typeName] = options.typeName.split("=>");

          // @ts-ignore
          if (abap.Classes[className] === undefined) {
            throwError("CX_SY_CREATE_DATA_ERROR");
          }
          // @ts-ignore
          if (abap.Classes[className][typeName.toLowerCase()] === undefined) {
            throwError("CX_SY_CREATE_DATA_ERROR");
          }

          // @ts-ignore
          target.assign(clone(abap.Classes[className][typeName.toLowerCase()]));
        } else {
          throw "CREATE DATA, unknown type " + options.typeName;
        }
    }
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