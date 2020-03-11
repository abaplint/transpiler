import * as abaplint from "abaplint";

export class TranspileTypes {

  public declare(t: abaplint.TypedIdentifier): string {
    const type = t.getType();
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
    return "let " + t.getName() + " = new abap.types." + resolved + "(" + extra + ");";

  }

}