import * as abaplint from "@abaplint/core";
import {ITranspilerOptions} from "../types";
import {DatabaseSetupResult} from "./database_setup_result";
import {SQLiteDatabaseSchema} from "./schema_generation/sqlite_database_schema";
import {PGDatabaseSchema} from "./schema_generation/pg_database_schema";
import {DatabaseSchemaGenerator} from "./schema_generation/database_schema_generator";
import {SnowflakeDatabaseSchema} from "./schema_generation/snowflake_database_schema";

/////////////////////////
// NOTES
/////////////////////////
// Postgres is case sensitive, so all column names should be lower case
// Sqlite escapes field names with single qoute, postgres with double

export class DatabaseSetup {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  public run(options?: ITranspilerOptions | undefined): DatabaseSetupResult {
    return {
      schemas: {
        sqlite: this.driver(new SQLiteDatabaseSchema(this.reg)),
        hdb: ["todo"],
        pg: this.driver(new PGDatabaseSchema(this.reg)),
        snowflake: this.driver(new SnowflakeDatabaseSchema(this.reg)),
      },
      insert: this.buildInsert(options),
    };
  }

////////////////////

  private driver(schemaGenerator: DatabaseSchemaGenerator): string[] {
    const statements: string[] = [];
    // CREATE TABLEs
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.Table
          && obj.getTableCategory() === abaplint.Objects.TableCategory.Transparent) {
        statements.push(schemaGenerator.buildTABL(obj).trim());
      }
    }
    // CREATE VIEWs after TABLEs
    // todo: what if the view is based on another view?
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.View) {
        statements.push(schemaGenerator.buildVIEW(obj).trim());
      }
    }
    return statements;
  }

  private buildInsert(options?: ITranspilerOptions | undefined): string[] {
    // note: avoid hitting maximum statement size by splitting into multiple statements
    const insert: string[] = [];
    // INSERT data
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.MessageClass) {
        insert.push(...this.insertT100(obj));
      } else if (obj instanceof abaplint.Objects.Class
          || obj instanceof abaplint.Objects.Interface) {
        if (options?.skipReposrc !== true) {
          insert.push(this.insertREPOSRC(obj));
        }
      }
    }
    insert.push(this.insertT000());
    return insert;
  }

  private insertREPOSRC(obj: abaplint.ABAPObject): string {
    if (this.reg.getObject("TABL", "REPOSRC") === undefined) {
      return "";
    }

    const name = obj.getName().toUpperCase();
    const raw = obj.getMainABAPFile()?.getRaw();
    if (raw === undefined) {
      return "";
    }

    return `INSERT INTO reposrc ('PROGNAME', 'DATA') VALUES ('${name.padEnd(40, " ")}', '${this.escape(raw)}');`;
  }

  private insertT000(): string {
    const tabl = this.reg.getObject("TABL", "T000") as abaplint.Objects.Table | undefined;
    if (tabl === undefined) {
      return "";
    }

    const type = tabl.parseType(this.reg);
    if (type instanceof abaplint.BasicTypes.StructureType && type.getComponents().length >= 3) {
      // todo, this should take the client number from the settings
      return `INSERT INTO t000 ('mandt', 'cccategory', 'ccnocliind') VALUES ('123', '', '');`;
    } else {
      return "";
    }
  }

  private insertT100(msag: abaplint.Objects.MessageClass): string[] {
    // ignore if T100 is unknown
    const obj = this.reg.getObject("TABL", "T100") as abaplint.Objects.Table | undefined;
    if (obj === undefined) {
      return [];
    }
    const ret = [];
    for (const m of msag.getMessages()) {
      ret.push(`INSERT INTO "t100" ("sprsl", "arbgb", "msgnr", "text") VALUES ('E', '${msag.getName().padEnd(20, " ")}', '${m.getNumber()}', '${this.escape(m.getMessage().padEnd(73, " "))}');`);
    }
    return ret;
  }

  private escape(value: string): string {
    let ret = value.replace(/\'/g, "''");
    // statements are inside a javascript string stemplate
    ret = ret.replace(/\\/g, "\\\\");
    ret = ret.replace(/`/g, "\\`");
    ret = ret.replace(/\${/g, "\\${");
    return ret;
  }

}