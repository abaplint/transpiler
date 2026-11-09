import * as abaplint from "@abaplint/core";
import {ExternalView, TableColumn} from "./external_view";
import {DatabaseSchemaGenerator, packedTypeToDatabase} from "./database_schema_generator";

export class SnowflakeDatabaseSchema implements DatabaseSchemaGenerator {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  public buildVIEW(view: abaplint.Objects.View): string {
    if (ExternalView.isExternal(view)) {
      const {columns, keys} = ExternalView.columns(view, this.reg);
      return this.createTable(view.getName(), columns, keys);
    }
    return "Error: buildView, todo snowflake";
  }

  public buildTABL(tabl: abaplint.Objects.Table): string {
    const type = tabl.parseType(this.reg);
    if (!(type instanceof abaplint.BasicTypes.StructureType)) {
      return "";
    }

    const columns: TableColumn[] = [];
    for (const field of type.getComponents()) {
      if (field.type instanceof abaplint.BasicTypes.StructureType) {
        // is a GROUP NAME
        continue;
      }
      columns.push(field);
    }

    // assumption: all transparent tables have primary keys
    return this.createTable(tabl.getName(), columns, tabl.listKeys(this.reg));
  }

  private createTable(name: string, columns: TableColumn[], keys: string[]): string {
    const fieldsRaw = columns.map(c => c.name.toLowerCase());
    // quote field names to allow for keywords as field names
    const fields = columns.map(c => "\"" + c.name.toLowerCase() + "\" " + this.toType(c.type, c.name, name));
    const key = ", PRIMARY KEY(" + keys
      .filter(e => fieldsRaw.includes(e.toLowerCase()))
      .map(e => "\"" + e.toLowerCase() + "\"").join(",") + ")";

    return `CREATE TABLE "${name.toLowerCase()}" (${fields.join(", ")}${key});\n`;
  }

  // https://docs.snowflake.com/en/sql-reference/collation
  private toType(type: abaplint.AbstractType, fieldname: string, errorInfo: string): string {
    if (type instanceof abaplint.BasicTypes.CharacterType) {
      return `NCHAR(${type.getLength()}) COLLATE 'rtrim'`;
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
    } else if (type instanceof abaplint.BasicTypes.Integer8Type) {
      return `NUMBER(19)`;
    } else if (type instanceof abaplint.BasicTypes.FloatType
        || type instanceof abaplint.BasicTypes.FloatingPointType) {
      return `REAL`;
    } else if (type instanceof abaplint.BasicTypes.PackedType){
      return packedTypeToDatabase(type);
    } else if (type instanceof abaplint.BasicTypes.VoidType) {
      throw `Type of ${errorInfo}-${fieldname} is VoidType(${type.getVoided()
      }), make sure the type is known, enable strict syntax checking`;
    } else {
      throw "database_setup: " + errorInfo + "-" + fieldname + ", todo toType handle: " + type.constructor.name;
    }
  }

}
