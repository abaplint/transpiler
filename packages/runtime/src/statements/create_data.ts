import {throwError} from "../throw_error";
import {ABAPObject, Character, DataReference, Date, String, FieldSymbol, Float, Integer, Structure, Table, Time, XString, Hex, Packed, Numc, Integer8, UTCLong} from "../types";
import {ICharacter} from "../types/_character";
import {INumeric} from "../types/_numeric";

type PointerType = INumeric | Table | ICharacter | ABAPObject | undefined | Structure | FieldSymbol;

export interface ICreateDataOptions {
  table?: boolean,
  name?: string,
  type?: PointerType,
  typeName?: string,
  length?: INumeric,
  decimals?: INumeric,
  likeLineOf?: FieldSymbol | Table,
  typeLineOf?: boolean,
  refTo?: boolean,
  like?: any,
}

export function createData(target: DataReference | FieldSymbol, options?: ICreateDataOptions) {
  if (target instanceof FieldSymbol) {
    createData(target.getPointer(), options);
    return;
  } else if (!(target instanceof DataReference)) {
    throw new Error("CREATE_DATA_REFERENCE_EXPECTED");
  }
  if (options?.name && options?.table) {
    // @ts-ignore
    if (abap.DDIC[options.name.trimEnd()] === undefined) {
      throwError("CX_SY_CREATE_DATA_ERROR");
    }
    // @ts-ignore
    target.assign(new abap.types.Table(abap.DDIC[options.name.trimEnd()].type));
  } else if (options?.name) {
    // @ts-ignore
    if (abap.DDIC[options.name.trimEnd()]) {
      // @ts-ignore
      target.assign(abap.DDIC[options.name.trimEnd()].type.clone());
    } else if (options.name.includes("=>")) {
      const [className, typeName] = options.name.trimEnd().toUpperCase().split("=>");

      // @ts-ignore
      if (abap.Classes[className] === undefined) {
        throwError("CX_SY_CREATE_DATA_ERROR");
      }
      // @ts-ignore
      if (abap.Classes[className][typeName.toLowerCase()] === undefined) {
        throwError("CX_SY_CREATE_DATA_ERROR");
      }

      // @ts-ignore
      target.assign(abap.Classes[className][typeName.toLowerCase()].clone());
    } else if (options.name.startsWith("\\TYPE=%")) {
      // currently, only the runtime knows the references to the anonymous types
      // @ts-ignore
      const clas = abap.Classes["KERNEL_CREATE_DATA_HANDLE"];
      if (clas === undefined) {
        throw new Error("CreateData, kernel class missing");
      }
      clas.anonymous({name: options.name.trimEnd(), dref: target});
    } else if (options.name.trimEnd() === "ABAP_BOOL") {
// ABAP_BOOL is special, its not part of the type pool, its built-into abaplint
      target.assign(new Character(1, {qualifiedName: "ABAP_BOOL", ddicName: "ABAP_BOOL"}));
    } else if (options.name.trimEnd() === "STRING") {
      target.assign(new String());
    } else if (options.name.trimEnd() === "XSTRING") {
      target.assign(new XString());
    } else if (options.name.trimEnd() === "UTCLONG") {
      target.assign(new UTCLong());
    } else if (options.name.trimEnd() === "I") {
      target.assign(new Integer());
    } else if (options.name.trimEnd() === "T") {
      target.assign(new Time());
    } else if (options.name.trimEnd() === "D") {
      target.assign(new Date());
    } else if (options.name.trimEnd() === "F") {
      target.assign(new Float());
    } else if (options.name.trimEnd() === "INT8") {
      target.assign(new Integer8());
    } else if (options.refTo === true) {
      // @ts-ignore
      if (abap.Classes[options.name.toUpperCase()] === undefined) {
        throwError("CX_SY_CREATE_DATA_ERROR");
      }

      // @ts-ignore
      target.assign(new abap.types.ABAPObject({qualifiedName: options.name, RTTIName: options.name}));
    } else {
      throwError("CX_SY_CREATE_DATA_ERROR");
    }
    if (options.typeLineOf === true) {
      // @ts-ignore
      target.assign(target.getPointer().getRowType().clone());
    }
  } else if (options?.typeName) {
    switch (options.typeName) {
      case "C":
        {
          let length = 1;
          if (options.length) {
            length = options.length.get();
          }
          target.assign(new Character(length));
        }
        break;
      case "N":
        {
          let length = 1;
          if (options.length) {
            length = options.length.get();
          }
          target.assign(new Numc({length: length}));
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
          let decimals = 0;
          if (options.decimals) {
            decimals = options.decimals.get();
          }
          target.assign(new Packed({length: length, decimals: decimals}));
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
      case "INT8":
        target.assign(new Integer8());
        break;
      case "XSTRING":
        target.assign(new XString());
        break;
      case "UTCLONG":
        target.assign(new UTCLong());
        break;
      default:
          // @ts-ignore
        if (abap.DDIC[options.typeName.trimEnd()]) {
          // @ts-ignore
          target.assign(abap.DDIC[options.typeName.trimEnd()].type.clone());
        }
        else if (options.typeName.includes("=>")) {
          const [className, typeName] = options.typeName.toUpperCase().split("=>");

          // @ts-ignore
          if (abap.Classes[className] === undefined) {
            throwError("CX_SY_CREATE_DATA_ERROR");
          }
          // @ts-ignore
          if (abap.Classes[className][typeName.toLowerCase().trimEnd()] === undefined) {
            throwError("CX_SY_CREATE_DATA_ERROR");
          }

          // @ts-ignore
          target.assign(abap.Classes[className][typeName.toLowerCase().trimEnd()].clone());
        } else {
          throw "CREATE DATA, unknown type " + options.typeName;
        }
    }
  } else if (options?.type) {
    target.assign(options.type.clone() as any);
  } else if (options?.likeLineOf) {
    if (options.likeLineOf instanceof FieldSymbol) {
      options.likeLineOf = options.likeLineOf.getPointer() as Table;
    }
    target.assign(options.likeLineOf.getRowType().clone());
  } else if (options?.like) {
    if (options.like instanceof FieldSymbol) {
      options.like = options.like.getPointer();
    }
    target.assign(options.like.clone());
  } else {
    target.assign(target.getType()?.clone());
  }
}