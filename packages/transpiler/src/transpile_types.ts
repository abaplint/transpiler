import * as abaplint from "@abaplint/core";

const featureHexUInt8 = true;

// todo: change this class to static for performance?
export class TranspileTypes {

  public declare(t: abaplint.TypedIdentifier): string {
    const type = t.getType();
    return "let " + t.getName().toLowerCase() + " = " + this.toType(type) + ";";
  }

  public declareStaticSkipVoid(pre: string, t: abaplint.TypedIdentifier): string {
    const type = t.getType();
    const code = this.toType(type);
    // todo, this should look at the configuration, for runtime vs compile time errors
    if (code.includes("Void type") || code.includes("abap.types.typeTodo")) {
      return "";
    }
    return pre + t.getName().toLowerCase() + " = " + code + ";\n";
  }

  public toType(type: abaplint.AbstractType): string {
    let resolved = "";
    let extra = "";

    if (type instanceof abaplint.BasicTypes.ObjectReferenceType
        || type instanceof abaplint.BasicTypes.GenericObjectReferenceType) {
      resolved = "ABAPObject";
      extra = "{qualifiedName: " + JSON.stringify(type.getQualifiedName()?.toUpperCase()) +
        ", RTTIName: " + JSON.stringify(type.getRTTIName()?.toUpperCase()) + "}";
    } else if (type instanceof abaplint.BasicTypes.TableType) {
      resolved = "Table";
      extra = this.toType(type.getRowType());
      extra += ", " + JSON.stringify(type.getOptions());
      if (type.getQualifiedName() !== undefined) {
        extra += ", \"" + type.getQualifiedName() + "\"";
      }
      return "abap.types.TableFactory.construct(" + extra + ")";
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
      extra = this.toType(type.getType());
    } else if (type instanceof abaplint.BasicTypes.StructureType) {
      resolved = "Structure";
      const list: string[] = [];
      const suffix: { [key: string]: string } = {};
      const asInclude: { [key: string]: boolean } = {};
      for (const c of type.getComponents()) {
        list.push(`"` + c.name.toLowerCase() + `": ` + this.toType(c.type));
        if (c.suffix) {
          suffix[c.name.toLowerCase()] = c.suffix;
        }
        if (c.asInclude) {
          asInclude[c.name.toLowerCase()] = true;
        }
      }
      extra = "{" + list.join(", ") + "}";
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
      if (type.getQualifiedName()) {
        extra = "{length: " + type.getLength() + ", decimals: " + type.getDecimals() + ", qualifiedName: \"" + type.getQualifiedName() + "\"}";
      } else {
        extra = "{length: " + type.getLength() + ", decimals: " + type.getDecimals() + "}";
      }
    } else if (type instanceof abaplint.BasicTypes.NumericGenericType) {
      resolved = "Packed";
      extra = "{length: 8, decimals: 2}";
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
      if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + "}";
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
    } else if (type instanceof abaplint.BasicTypes.UnknownType) {
      return `(() => { throw "Unknown type: ${type.getError()}" })()`;
    } else if (type instanceof abaplint.BasicTypes.VoidType) {
      return `(() => { throw "Void type: ${type.getVoided()}" })()`;
    } else {
      resolved = "typeTodo" + type.constructor.name;
    }

    return "new abap.types." + resolved + "(" + extra + ")";
  }

}