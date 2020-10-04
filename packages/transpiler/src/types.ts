import * as abaplint from "@abaplint/core";

export class TranspileTypes {

  public declare(t: abaplint.TypedIdentifier): string {
    const type = t.getType();
    return "let " + t.getName() + " = " + this.toType(type) + ";";
  }

  public toType(type: abaplint.AbstractType): string {
    let resolved = "";
    let extra = "";

    if (type instanceof abaplint.BasicTypes.ObjectReferenceType) {
      resolved = "ABAPObject";
    } else if (type instanceof abaplint.BasicTypes.TableType) {
      resolved = "Table";
    } else if (type instanceof abaplint.BasicTypes.IntegerType) {
      resolved = "Integer";
    } else if (type instanceof abaplint.BasicTypes.StringType
        || type instanceof abaplint.BasicTypes.CLikeType) {
      resolved = "String";
    } else if (type instanceof abaplint.BasicTypes.StructureType) {
      resolved = "Structure";
      const list: string[] = [];
      for (const c of type.getComponents()) {
        list.push(c.name + ": " + this.toType(c.type));
      }
      extra = "{" + list.join(", ") + "}";
    } else if (type instanceof abaplint.BasicTypes.CharacterType) {
      resolved = "Character";
      if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + "}";
      }
    } else if (type instanceof abaplint.BasicTypes.PackedType) {
      resolved = "Packed";
    } else if (type instanceof abaplint.BasicTypes.XStringType
        || type instanceof abaplint.BasicTypes.XSequenceType) {
      resolved = "XString";
    } else if (type instanceof abaplint.BasicTypes.HexType) {
      resolved = "Hex";
      if (type.getLength() !== 1) {
        extra = "{length: " + type.getLength() + "}";
      }
    } else {
      resolved = "typeTodo";
    }

    return "new abap.types." + resolved + "(" + extra + ")";
  }

}