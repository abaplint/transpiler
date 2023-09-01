import * as abaplint from "@abaplint/core";
import {DatabaseSchemaGenerator} from "./database_schema_generator";

export class SnowflakeDatabaseSchema implements DatabaseSchemaGenerator {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  public buildVIEW(view: abaplint.Objects.View): string {
    const fields = view.getFields();
    let firstTabname = "";
    const columns = fields?.map((f) => {
      firstTabname = "'" + f.TABNAME.toLowerCase() + "'";
      return firstTabname + "." + f.FIELDNAME.toLowerCase() + " AS " + f.VIEWFIELD.toLowerCase();
    }).join(", ");

    let from = "";
    let previous = "";
    for (const j of view.getJoin() || []) {
      if (previous === "") {
        from += "'" + j.LTAB.toLowerCase() + "' INNER JOIN '" + j.RTAB.toLowerCase() + "' ON '" + j.LTAB.toLowerCase() + "'." + j.LFIELD.toLowerCase() + " = '" + j.RTAB.toLowerCase() + "'." + j.RFIELD.toLowerCase();
      } else if (previous === j.LTAB + "," + j.RTAB) {
        from += " AND '" + j.LTAB.toLowerCase() + "'." + j.LFIELD.toLowerCase() + " = '" + j.RTAB.toLowerCase() + "'." + j.RFIELD.toLowerCase();
      } else {
        from += " INNER JOIN '" + j.RTAB.toLowerCase() + "' ON '" + j.LTAB.toLowerCase() + "'." + j.LFIELD.toLowerCase() + " = '" + j.RTAB.toLowerCase() + "'." + j.RFIELD.toLowerCase();
      }
      previous = j.LTAB + "," + j.RTAB;
    }
    from = from.trim();
    if (from === "") {
      from = firstTabname;
    }

    return `CREATE VIEW '${view.getName().toLowerCase()}' AS SELECT ${columns} FROM ${from};\n`;
  }

  public buildTABL(tabl: abaplint.Objects.Table): string {
    const type = tabl.parseType(this.reg);
    if (!(type instanceof abaplint.BasicTypes.StructureType)) {
      return "";
    }

    const fields: string[] = [];
    const fieldsRaw: string[] = [];
    for (const field of type.getComponents()) {
      if (field.type instanceof abaplint.BasicTypes.StructureType) {
        // is a GROUP NAME
        continue;
      }
      fieldsRaw.push(field.name.toLowerCase());
      fields.push("\"" + field.name.toLowerCase() + "\" " + this.toType(field.type, field.name, tabl.getName()));
    }

    // assumption: all transparent tables have primary keys
    // add single quotes to field names to allow for keywords as field names
    const key = ", PRIMARY KEY(" + tabl.listKeys(this.reg)
      .filter(e => fieldsRaw.includes(e.toLowerCase()))
      .map(e => "\"" + e.toLowerCase() + "\"").join(",") + ")";

    return `CREATE TABLE "${tabl.getName().toLowerCase()}" (${fields.join(", ")}${key});\n`;
  }

  // https://docs.snowflake.com/en/sql-reference/collation
  private toType(type: abaplint.AbstractType, fieldname: string, errorInfo: string): string {
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
    } else if (type instanceof abaplint.BasicTypes.FloatType
        || type instanceof abaplint.BasicTypes.FloatingPointType) {
      return `REAL`;
    } else if (type instanceof abaplint.BasicTypes.PackedType){
      return `DECIMAL(${type.getLength()},${type.getDecimals()})`;
    } else if (type instanceof abaplint.BasicTypes.VoidType) {
      throw `Type of ${errorInfo}-${fieldname} is VoidType(${type.getVoided()
      }), make sure the type is known, enable strict syntax checking`;
    } else {
      throw "database_setup: " + errorInfo + "-" + fieldname + ", todo toType handle: " + type.constructor.name;
    }
  }

}
