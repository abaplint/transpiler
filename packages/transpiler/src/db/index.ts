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
        // HANA takes the PG schema unchanged -- measured against HANA Express
        // on a tree of 77 tables. The one thing it needs is that identifiers
        // reach it quoted in UPPER case, and that is not a dialect of DDL: the
        // same rule applies to every statement, so @abaplint/database-hdb
        // folds them on the way out rather than a second generator doing it
        // here. (Unquoted would nearly work and then meet a column called
        // `cross`, which is one of HANA's reserved words.)
        hdb: this.driver(new PGDatabaseSchema(this.reg)),
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
    const add = (statement: string): void => {
      if (statement.length > 0) {
        insert.push(statement);
      }
    };
    const addAll = (statements: readonly string[]): void => {
      for (const statement of statements) {
        add(statement);
      }
    };

    // INSERT data
    for (const obj of this.reg.getObjects()) {
      if (options?.populateTables?.tadir !== false) {
        add(populateTables.insertTADIR(obj));
      }

      if (obj instanceof abaplint.Objects.MessageClass) {
        addAll(populateTables.insertT100(obj));
      } else if (options?.populateTables?.wwwparams !== false
          && obj instanceof abaplint.Objects.WebMIME) {
        addAll(populateTables.insertWWWPARAMS(obj));
      } else if (obj instanceof abaplint.Objects.Class
          || obj instanceof abaplint.Objects.Interface
          || obj instanceof abaplint.Objects.Program) {
        if (options?.populateTables?.reposrc !== false) {
          add(populateTables.insertREPOSRC(obj));
        }
        if ((obj instanceof abaplint.Objects.Class || obj instanceof abaplint.Objects.Interface)
            && options?.populateTables?.seosubco !== false) {
          addAll(populateTables.insertSEOSUBCO(obj));
        }
        if ((obj instanceof abaplint.Objects.Class || obj instanceof abaplint.Objects.Interface)
            && options?.populateTables?.seosubcodf !== false) {
          addAll(populateTables.insertSEOSUBCODF(obj));
        }
        if ((obj instanceof abaplint.Objects.Class || obj instanceof abaplint.Objects.Interface)
            && options?.populateTables?.seosubcotx !== false) {
          addAll(populateTables.insertSEOSUBCOTX(obj));
        }
      }
    }
    add(populateTables.insertT000());
    return insert;
  }

}
