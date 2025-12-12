import * as abaplint from "@abaplint/core";
import {ITranspilerOptions} from "../types";
import {DatabaseSetupResult} from "./database_setup_result";
import {SQLiteDatabaseSchema} from "./schema_generation/sqlite_database_schema";
import {PGDatabaseSchema} from "./schema_generation/pg_database_schema";
import {DatabaseSchemaGenerator} from "./schema_generation/database_schema_generator";
import {SnowflakeDatabaseSchema} from "./schema_generation/snowflake_database_schema";
import {PopulateTables} from "./populate_tables";

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
    const populateTables = new PopulateTables(this.reg);
    // INSERT data
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.MessageClass) {
        insert.push(...populateTables.insertT100(obj));
      } else if (options?.populateTables?.wwwparams !== false
          && obj instanceof abaplint.Objects.WebMIME) {
        insert.push(...populateTables.insertWWWPARAMS(obj));
      } else if (obj instanceof abaplint.Objects.Class
          || obj instanceof abaplint.Objects.Interface) {
        if (options?.populateTables?.reposrc !== false) {
          insert.push(populateTables.insertREPOSRC(obj));
        }
        if (options?.populateTables?.seosubco !== false) {
          insert.push(...populateTables.insertSEOSUBCO(obj));
        }
        if (options?.populateTables?.seosubcodf !== false) {
          insert.push(...populateTables.insertSEOSUBCODF(obj));
        }
        if (options?.populateTables?.seosubcotx !== false) {
          insert.push(...populateTables.insertSEOSUBCOTX(obj));
        }
      }
    }
    insert.push(populateTables.insertT000());
    return insert;
  }

}