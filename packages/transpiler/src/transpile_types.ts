import * as abaplint from "@abaplint/core";

export class TranspileTypes {

  public declare(t: abaplint.TypedIdentifier): string {
    const type = t.getType();
    return "let " + t.getName().toLowerCase() + " = " + this.toType(type) + ";";
  }

  public toType(type: abaplint.AbstractType): string {
    let resolved = "";
    let extra = "";

    if (type instanceof abaplint.BasicTypes.ObjectReferenceType
        || type instanceof abaplint.BasicTypes.GenericObjectReferenceType) {
      resolved = "ABAPObject";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.TableType) {
      resolved = "Table";
      extra = this.toType(type.getRowType());
      extra += ", " + JSON.stringify(type.getOptions());
      if (type.getQualifiedName() !== undefined) {
        extra += ", \"" + type.getQualifiedName() + "\"";
      }
    } else if (type instanceof abaplint.BasicTypes.IntegerType) {
      resolved = "Integer";
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
      for (const c of type.getComponents()) {
        list.push(c.name.toLowerCase() + ": " + this.toType(c.type));
      }
      extra = "{" + list.join(", ") + "}";
      if (type.getQualifiedName() !== undefined) {
        extra += ", \"" + type.getQualifiedName() + "\"";
      }
    } else if (type instanceof abaplint.BasicTypes.CLikeType
        || type instanceof abaplint.BasicTypes.CSequenceType) {
      // if not supplied its a Character(1)
      resolved = "Character";
    } else if (type instanceof abaplint.BasicTypes.AnyType) {
      // if not supplied its a Character(4)
      resolved = "Character";
      extra = "{length: 4}";
    } else if (type instanceof abaplint.BasicTypes.SimpleType) {
      // if not supplied its a Character(1)
      resolved = "Character";
    } else if (type instanceof abaplint.BasicTypes.CharacterType) {
      resolved = "Character";
      if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + ", qualifiedName: \"" + type.getQualifiedName() + "\"}";
      } else if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.NumericType) {
      resolved = "Numc";
      if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + "}";
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
      extra = "{length: 8, decimals: 0}";
    } else if (type instanceof abaplint.BasicTypes.XStringType) {
      resolved = "XString";
      if (type.getQualifiedName() !== undefined) {
        extra = "{qualifiedName: \"" + type.getQualifiedName()?.toUpperCase() + "\"}";
      }
    } else if (type instanceof abaplint.BasicTypes.XSequenceType) {
      // if not supplied itsa a Hex(1)
      resolved = "Hex";
    } else if (type instanceof abaplint.BasicTypes.HexType) {
      resolved = "Hex";
      if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + "}";
      }
    } else if (type instanceof abaplint.BasicTypes.FloatType) {
      resolved = "Float";
    } else if (type instanceof abaplint.BasicTypes.FloatingPointType) {
      resolved = "Float";
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