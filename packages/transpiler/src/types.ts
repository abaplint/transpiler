import * as abaplint from "abaplint";

export class TranspileTypes {

  public declare(t: abaplint.TypedIdentifier): string {
    const type = t.getType();

    return "let " + t.getName() + " = " + this.toType(type) + ";";

  }

  private toType(type: abaplint.AbstractType): string {
    let resolved = "typeTodo";
    let extra = "";

    if (type instanceof abaplint.BasicTypes.ObjectReferenceType) {
      resolved = "ABAPObject";
    } else if (type instanceof abaplint.BasicTypes.TableType) {
      resolved = "Table";
    } else if (type instanceof abaplint.BasicTypes.IntegerType) {
      resolved = "Integer";
    } else if (type instanceof abaplint.BasicTypes.StringType) {
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
    }

/* todo
    case "P":
      return "abap.types.Packed";
*/

    return "new abap.types." + resolved + "(" + extra + ")";
  }

}