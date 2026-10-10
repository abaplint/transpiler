import * as abaplint from "@abaplint/core";
import {Traversal} from "./traversal";
import {TypeFactoryRegistry} from "./type_factory_registry";

const featureHexUInt8 = false;

export class TranspileTypes {

  public static declare(t: abaplint.TypedIdentifier, registry?: TypeFactoryRegistry): string {
    const type = t.getType();
    return "let " + Traversal.prefixVariable(t.getName().toLowerCase()) + " = " + this.toType(type, undefined, registry) + ";";
  }

  public static declareStaticSkipVoid(pre: string, t: abaplint.TypedIdentifier, registry?: TypeFactoryRegistry): string {
    const type = t.getType();
    // todo, this should look at the configuration, for runtime vs compile time errors
    if (this.hasUnsupportedType(type)) {
      return "";
    }
    const code = this.toType(type, undefined, registry);
    return pre + t.getName().toLowerCase() + " = " + code + ";\n";
  }

  /** this returns a function, so it doesnt throw when loading the code, only when running */
  public static toTypeFunction(type: abaplint.AbstractType, registry?: TypeFactoryRegistry): string {
    if (type instanceof abaplint.BasicTypes.UnknownType) {
      return `() => { throw new Error("Unknown type: ${type.getError()}") }`;
    } else if (type instanceof abaplint.BasicTypes.VoidType) {
      return `() => { throw new Error("Void type: ${type.getVoided()}") }`;
    }
    // return singleton,
    return "(() => { let _t; return () => (_t ??= " + this.toType(type, undefined, registry) + "); })()";
  }

  public static toType(type: abaplint.AbstractType, options?: {packedDecimals?: number}, registry?: TypeFactoryRegistry): string {
    if (registry === undefined) {
      return this.toTypeInline(type, options);
    }
    // Elementary types cannot produce helpers or recursive constructor graphs.
    // Keep them off the registry's WeakMap and unsupported-graph walk; composite
    // parents still intern them as part of their exact rendered expression.
    if (!this.isComposite(type)) {
      return this.toTypeInline(type, options, registry);
    }
    const optionsKey = JSON.stringify(options ?? {});
    return registry.resolveType(type, optionsKey, () => {
      // Keep unsupported graphs on the legacy inline error path. Registering
      // their outer composite would hide the unsupported node behind a helper.
      const supportedRegistry = this.hasUnsupportedType(type) ? undefined : registry;
      return this.toTypeInline(type, options, supportedRegistry);
    });
  }

  private static toTypeInline(type: abaplint.AbstractType, options?: {packedDecimals?: number}, registry?: TypeFactoryRegistry): string {
    let resolved = "";
    let extra = "";

    if (type instanceof abaplint.BasicTypes.ObjectReferenceType
        || type instanceof abaplint.BasicTypes.GenericObjectReferenceType) {
      resolved = "ABAPObject";
      const qualifiedName = type.getQualifiedName()
        ?? (type instanceof abaplint.BasicTypes.ObjectReferenceType ? type.getIdentifierName() : undefined);
      let RTTIName = type.getRTTIName();
      if (type instanceof abaplint.BasicTypes.ObjectReferenceType && RTTIName === undefined) {
        const id = type.getIdentifier();
        // NEW and CAST can omit metadata; recover it from the referenced definition.
        const [name, kind] = id.getFilename().split("/").pop()!.replace(/#/g, "/").split(".");
        const local = (id instanceof abaplint.Types.ClassDefinition || id instanceof abaplint.Types.InterfaceDefinition)
          && id.isGlobal() === false;
        const prefix = local && kind === "prog" ? "\\PROGRAM=" + name
          : local && kind === "clas" ? "\\CLASS-POOL=" + name : "";
        const category = id instanceof abaplint.Types.InterfaceDefinition || kind === "intf" ? "INTERFACE" : "CLASS";
        RTTIName = prefix + "\\" + category + "=" + id.getName();
      }
      extra = "{qualifiedName: " + JSON.stringify(qualifiedName?.toUpperCase()) +
        ", RTTIName: " + JSON.stringify(RTTIName?.toUpperCase()) + "}";
    } else if (type instanceof abaplint.BasicTypes.TableType) {
      resolved = "Table";
      extra = this.toType(type.getRowType(), undefined, registry);
      extra += ", " + JSON.stringify(type.getOptions());
      if (type.getQualifiedName() !== undefined) {
        extra += ", \"" + type.getQualifiedName() + "\"";
      }
      const expression = "abap.types.TableFactory.construct(" + extra + ")";
      return registry !== undefined && this.isComposite(type.getRowType())
        ? registry.register(expression) : expression;
    } else if (type instanceof abaplint.BasicTypes.IntegerType) {
      resolved = "Integer";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.Integer8Type) {
      resolved = "Integer8";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.StringType) {
      resolved = "String";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.UTCLongType) {
      resolved = "UTCLong";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.DateType) {
      resolved = "Date";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.TimeType) {
      resolved = "Time";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.DataReference) {
      resolved = "DataReference";
      extra = this.toType(type.getType(), undefined, registry);
    } else if (type instanceof abaplint.BasicTypes.StructureType) {
      resolved = "Structure";
      const list: string[] = [];
      const suffix: { [key: string]: string } = {};
      const asInclude: { [key: string]: boolean } = {};

      for (const c of type.getComponents()) {
        const lower = c.name.toLowerCase();
        list.push(`"` + lower + `": ` + this.toType(c.type, undefined, registry));
        if (c.suffix) {
          suffix[lower] = c.suffix;
        }
        if (c.asInclude) {
          asInclude[lower] = true;
        }
      }
      extra = "{\n" + list.join(",\n") + "}";
      if (type.getQualifiedName() !== undefined) {
        extra += ", \"" + type.getQualifiedName() + "\"";
      } else {
        extra += ", undefined";
      }
      if (type.getDDICName() !== undefined) {
        extra += ", \"" + type.getQualifiedName() + "\"";
      } else {
        extra += ", undefined";
      }
      extra += ", " + JSON.stringify(suffix);
      extra += ", " + JSON.stringify(asInclude);
    } else if (type instanceof abaplint.BasicTypes.CLikeType
        || type instanceof abaplint.BasicTypes.CGenericType
        || type instanceof abaplint.BasicTypes.CSequenceType) {
      // if not supplied its a Character(1)
      resolved = "Character";
    } else if (type instanceof abaplint.BasicTypes.AnyType
        || type instanceof abaplint.BasicTypes.DataType) {
      // if not supplied its a Character(4)
      resolved = "Character";
      extra = "4";
    } else if (type instanceof abaplint.BasicTypes.SimpleType) {
      // if not supplied its a Character(1)
      resolved = "Character";
    } else if (type instanceof abaplint.BasicTypes.CharacterType) {
      resolved = "Character";
      extra = type.getLength() + ", " + JSON.stringify(type.getAbstractTypeData());
    } else if (type instanceof abaplint.BasicTypes.NumericType) {
      resolved = "Numc";
      if (type.getQualifiedName() && type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + ", qualifiedName: \"" + type.getQualifiedName() + "\"}";
      } else if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + "}";
      } else if (type.getQualifiedName()) {
        extra = "{qualifiedName: \"" + type.getQualifiedName() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.PackedType) {
      resolved = "Packed";
      const decimals = options?.packedDecimals ?? type.getDecimals();
      if (type.getQualifiedName()) {
        extra = "{length: " + type.getLength() + ", decimals: " + decimals + ", qualifiedName: \"" + type.getQualifiedName() + "\"}";
      } else {
        extra = "{length: " + type.getLength() + ", decimals: " + decimals + "}";
      }
    } else if (type instanceof abaplint.BasicTypes.NumericGenericType) {
      resolved = "Packed";
      extra = "{length: 8, decimals: 2}";
    } else if (type instanceof abaplint.BasicTypes.PGenericType) {
      // if not supplied its a P LENGTH 8 DECIMALS 0
      resolved = "Packed";
      extra = "{length: 8, decimals: 0}";
    } else if (type instanceof abaplint.BasicTypes.XStringType) {
      resolved = "XString";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.XSequenceType
        || type instanceof abaplint.BasicTypes.XGenericType) {
      // if not supplied itsa a Hex(1)
      resolved = "Hex";
    } else if (type instanceof abaplint.BasicTypes.HexType) {
      resolved = featureHexUInt8 ? "HexUInt8" : "Hex";
      if (type.getLength() !== 1 && type.getQualifiedName() !== undefined) {
        extra = "{length: " + type.getLength() + ", qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      } else if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + "}";
      } else if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.FloatType) {
      resolved = "Float";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.FloatingPointType) {
      resolved = "Float";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.DecFloat34Type) {
      resolved = "DecFloat34";
    } else if (type instanceof abaplint.BasicTypes.EnumType) {
      resolved = "String";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.UnknownType) {
      return `(() => { throw new Error("Unknown type: ${type.getError()}") })()`;
    } else if (type instanceof abaplint.BasicTypes.VoidType) {
      return `(() => { throw new Error("Void type: ${type.getVoided()}") })()`;
    } else {
      resolved = "typeTodo" + type.constructor.name;
    }

    const expression = "new abap.types." + resolved + "(" + extra + ")";
    if (registry !== undefined && (type instanceof abaplint.BasicTypes.StructureType
        || (type instanceof abaplint.BasicTypes.DataReference && this.isComposite(type.getType())))) {
      return registry.register(expression);
    }
    return expression;
  }

  private static isComposite(type: abaplint.AbstractType): boolean {
    if (type instanceof abaplint.BasicTypes.StructureType || type instanceof abaplint.BasicTypes.TableType) {
      return true;
    } else if (type instanceof abaplint.BasicTypes.DataReference) {
      return this.isComposite(type.getType());
    }
    return false;
  }

  private static hasUnsupportedType(type: abaplint.AbstractType, seen?: Set<abaplint.AbstractType>): boolean {
    if (type instanceof abaplint.BasicTypes.UnknownType || type instanceof abaplint.BasicTypes.VoidType) {
      return true;
    } else if (type instanceof abaplint.BasicTypes.StructureType) {
      if (seen?.has(type)) {
        return false;
      }
      seen ??= new Set<abaplint.AbstractType>();
      seen.add(type);
      return type.getComponents().some(component => this.hasUnsupportedType(component.type, seen));
    } else if (type instanceof abaplint.BasicTypes.TableType) {
      if (seen?.has(type)) {
        return false;
      }
      seen ??= new Set<abaplint.AbstractType>();
      seen.add(type);
      return this.hasUnsupportedType(type.getRowType(), seen);
    } else if (type instanceof abaplint.BasicTypes.DataReference) {
      if (seen?.has(type)) {
        return false;
      }
      seen ??= new Set<abaplint.AbstractType>();
      seen.add(type);
      return this.hasUnsupportedType(type.getType(), seen);
    } else if (type instanceof abaplint.BasicTypes.IntegerType
        || type instanceof abaplint.BasicTypes.CharacterType
        || type instanceof abaplint.BasicTypes.NumericType
        || type instanceof abaplint.BasicTypes.PackedType) {
      // Common elementary types make up most fields in ordinary structures.
      // Keep them on a short path, and avoid allocating the cycle-detection set
      // for leaves.
      return false;
    } else if (type instanceof abaplint.BasicTypes.ObjectReferenceType
        || type instanceof abaplint.BasicTypes.GenericObjectReferenceType
        || type instanceof abaplint.BasicTypes.Integer8Type
        || type instanceof abaplint.BasicTypes.StringType
        || type instanceof abaplint.BasicTypes.UTCLongType
        || type instanceof abaplint.BasicTypes.DateType
        || type instanceof abaplint.BasicTypes.TimeType
        || type instanceof abaplint.BasicTypes.CLikeType
        || type instanceof abaplint.BasicTypes.CGenericType
        || type instanceof abaplint.BasicTypes.CSequenceType
        || type instanceof abaplint.BasicTypes.AnyType
        || type instanceof abaplint.BasicTypes.DataType
        || type instanceof abaplint.BasicTypes.SimpleType
        || type instanceof abaplint.BasicTypes.NumericGenericType
        || type instanceof abaplint.BasicTypes.PGenericType
        || type instanceof abaplint.BasicTypes.XStringType
        || type instanceof abaplint.BasicTypes.XSequenceType
        || type instanceof abaplint.BasicTypes.XGenericType
        || type instanceof abaplint.BasicTypes.HexType
        || type instanceof abaplint.BasicTypes.FloatType
        || type instanceof abaplint.BasicTypes.FloatingPointType
        || type instanceof abaplint.BasicTypes.DecFloat34Type
        || type instanceof abaplint.BasicTypes.EnumType) {
      return false;
    }
    return true;
  }

}
