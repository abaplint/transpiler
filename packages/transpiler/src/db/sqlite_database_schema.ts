import * as abaplint from "@abaplint/core";

export class SQLiteDatabaseSchema {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  public run(): string {
    let ret = "";
    // CREATE TABLEs
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.Table && obj.getTableCategory() === abaplint.Objects.TableCategory.Transparent) {
        ret += this.transparentTable(obj);
      }
    }
    return ret.trim();
  }

//////////////////

  private transparentTable(tabl: abaplint.Objects.Table): string {
    const type = tabl.parseType(this.reg);
    if (!(type instanceof abaplint.BasicTypes.StructureType)) {
      return "";
    }

    const fields: string[] = [];
    for (const field of type.getComponents()) {
      fields.push(field.name.toLowerCase() + " " + this.toType(field.type, field.name, tabl.getName()));
    }

    // assumption: all transparent tables have primary keys
    const key = ", PRIMARY KEY(" + tabl.listKeys().map(e => e.toLowerCase()).join(",") + ")";

    return `CREATE TABLE ${tabl.getName().toLowerCase()} (${fields.join(", ")}${key});\n`;
  }

  private toType(type: abaplint.AbstractType, fieldname: string, table: string): string {
    if (type instanceof abaplint.BasicTypes.CharacterType) {
      return `NCHAR(${type.getLength()})`;
    } else if (type instanceof abaplint.BasicTypes.TimeType) {
      return `NCHAR(6)`;
    } else if (type instanceof abaplint.BasicTypes.DateType) {
      return `NCHAR(8)`;
    } else if (type instanceof abaplint.BasicTypes.NumericType) {
      // it will be fine, the runtime representation of numc is also text
      return `NCHAR(${type.getLength()})`;
    } else if (type instanceof abaplint.BasicTypes.StringType) {
      return `TEXT`;
    } else if (type instanceof abaplint.BasicTypes.XStringType) {
      // it will be fine, the runtime representation of xstring is also text
      return `TEXT`;
    } else if (type instanceof abaplint.BasicTypes.HexType) {
      return `NCHAR(${type.getLength() * 2})`;
    } else if (type instanceof abaplint.BasicTypes.IntegerType) {
      return `INT`;
    } else if (type instanceof abaplint.BasicTypes.PackedType){
      return `DECIMAL(${type.getLength()},${type.getDecimals()})`;
    } else if (type instanceof abaplint.BasicTypes.VoidType) {
      throw `Type of ${table}-${fieldname} is VoidType(${type.getVoided()}), make sure the type is known, enable strict syntax checking`;
    } else {
      throw "database_setup: " + table + "-" + fieldname + ", todo toType handle: " + type.constructor.name;
    }
  }

}
