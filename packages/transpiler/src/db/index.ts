import * as abaplint from "@abaplint/core";
import {ITranspilerOptions} from "../types";
import {DatabaseSetupInsert, DatabaseSetupResult, DatabaseSetupTable} from "./database_setup_result";
import {SQLiteDatabaseSchema} from "./schema_generation/sqlite_database_schema";
import {PGDatabaseSchema} from "./schema_generation/pg_database_schema";
import {DatabaseSchemaGenerator} from "./schema_generation/database_schema_generator";
import {SnowflakeDatabaseSchema} from "./schema_generation/snowflake_database_schema";
import {PopulateTables} from "./populate_tables";
import {CDSDatabaseView} from "./cds_database_view";

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
    const ifNotExists = options?.databaseSetup?.ifNotExists === true;
    // CDS views are dialect independent SQL, added after the tables and views they select from
    const cdsViews = this.buildCDSViews();
    const tables = this.buildTables({
      sqlite: new SQLiteDatabaseSchema(this.reg, ifNotExists),
      pg: new PGDatabaseSchema(this.reg, ifNotExists),
      // HANA takes the PG schema unchanged -- measured against HANA Express
      // on a tree of 77 tables. The one thing it needs is that identifiers
      // reach it quoted in UPPER case, and that is not a dialect of DDL: the
      // same rule applies to every statement, so @abaplint/database-hdb
      // folds them on the way out rather than a second generator doing it
      // here. (Unquoted would nearly work and then meet a column called
      // `cross`, which is one of HANA's reserved words.)
      // HANA has no IF NOT EXISTS on CREATE TABLE, so the option does not reach it.
      hdb: new PGDatabaseSchema(this.reg, false),
      snowflake: new SnowflakeDatabaseSchema(this.reg, ifNotExists),
    });
    const inserts = this.buildInsert(options);
    return {
      schemas: {
        sqlite: [...tables.map(t => t.sqlite), ...cdsViews],
        hdb: tables.map(t => t.hdb),
        pg: [...tables.map(t => t.pg), ...cdsViews],
        snowflake: [...tables.map(t => t.snowflake), ...cdsViews],
      },
      tables,
      insert: inserts.map(i => i.sql),
      inserts,
    };
  }

////////////////////

  private buildCDSViews(): string[] {
    const views: string[] = [];
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.DataDefinition) {
        const view = new CDSDatabaseView(this.reg).build(obj);
        if (view !== undefined) {
          views.push(view);
        }
      }
    }
    return views;
  }

  private buildTables(generators: {[dialect in "sqlite" | "pg" | "hdb" | "snowflake"]: DatabaseSchemaGenerator}): DatabaseSetupTable[] {
    const tables: DatabaseSetupTable[] = [];
    const build = (name: string, type: "TABL" | "VIEW", statement: (g: DatabaseSchemaGenerator) => string) => {
      tables.push({
        name: name.toUpperCase(),
        type,
        sqlite: statement(generators.sqlite).trim(),
        pg: statement(generators.pg).trim(),
        hdb: statement(generators.hdb).trim(),
        snowflake: statement(generators.snowflake).trim(),
      });
    };

    // CREATE TABLEs
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.Table
          && obj.getTableCategory() === abaplint.Objects.TableCategory.Transparent) {
        build(obj.getName(), "TABL", g => g.buildTABL(obj));
      }
    }

    // CREATE VIEWs after TABLEs
    // todo: what if the view is based on another view?
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.View) {
        build(obj.getName(), "VIEW", g => g.buildVIEW(obj));
      }
    }

    return tables;
  }

  private buildInsert(options?: ITranspilerOptions | undefined): DatabaseSetupInsert[] {
    // note: avoid hitting maximum statement size by splitting into multiple statements
    const inserts: DatabaseSetupInsert[] = [];
    const populateTables = new PopulateTables(this.reg);
    const add = (table: string, sql: string): void => {
      if (sql.length > 0) {
        inserts.push({table, sql});
      }
    };
    const addAll = (table: string, statements: readonly string[]): void => {
      for (const sql of statements) {
        add(table, sql);
      }
    };

    // INSERT data
    for (const obj of this.reg.getObjects()) {
      if (options?.populateTables?.tadir !== false) {
        add("TADIR", populateTables.insertTADIR(obj));
      }

      if (obj instanceof abaplint.Objects.MessageClass) {
        addAll("T100", populateTables.insertT100(obj));
      } else if (options?.populateTables?.wwwparams !== false
          && obj instanceof abaplint.Objects.WebMIME) {
        addAll("WWWPARAMS", populateTables.insertWWWPARAMS(obj));
      } else if (obj instanceof abaplint.Objects.Class
          || obj instanceof abaplint.Objects.Interface
          || obj instanceof abaplint.Objects.Program) {
        if (options?.populateTables?.reposrc !== false) {
          add("REPOSRC", populateTables.insertREPOSRC(obj));
        }
        if ((obj instanceof abaplint.Objects.Class || obj instanceof abaplint.Objects.Interface)
            && options?.populateTables?.seosubco !== false) {
          addAll("SEOSUBCO", populateTables.insertSEOSUBCO(obj));
        }
        if ((obj instanceof abaplint.Objects.Class || obj instanceof abaplint.Objects.Interface)
            && options?.populateTables?.seosubcodf !== false) {
          addAll("SEOSUBCODF", populateTables.insertSEOSUBCODF(obj));
        }
        if ((obj instanceof abaplint.Objects.Class || obj instanceof abaplint.Objects.Interface)
            && options?.populateTables?.seosubcotx !== false) {
          addAll("SEOSUBCOTX", populateTables.insertSEOSUBCOTX(obj));
        }
      }
    }
    add("T000", populateTables.insertT000());
    return inserts;
  }

}
