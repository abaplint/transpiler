/* eslint-disable max-len */
import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult} from "./database_setup_result";
import {SQLiteDatabaseSchema} from "./sqlite_database_schema";

export class DatabaseSetup {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  public run(): DatabaseSetupResult {
    return {
      schemas: {
        sqlite: new SQLiteDatabaseSchema(this.reg).run(),
        hdb: "todo",
        pg: "todo",
      },
      insert: this.buildInsert(),
    };
  }

////////////////////

  private buildInsert(): string {
    let insert = "";
    // INSERT data
    for (const obj of this.reg.getObjects()) {
      if (obj instanceof abaplint.Objects.MessageClass) {
        insert += this.insertT100(obj);
      } else if (obj instanceof abaplint.Objects.Class
          || obj instanceof abaplint.Objects.Interface) {
        insert += this.insertREPOSRC(obj);
      }
    }
    insert += this.insertT000();
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

    return `INSERT INTO reposrc ('PROGNAME', 'DATA') VALUES ('${name}', '${this.escape(raw)}');\n`;
  }

  private insertT000(): string {
    const tabl = this.reg.getObject("TABL", "T000") as abaplint.Objects.Table | undefined;
    if (tabl === undefined) {
      return "";
    }

    const type = tabl.parseType(this.reg);
    if (type instanceof abaplint.BasicTypes.StructureType && type.getComponents().length >= 3) {
      // todo, this should take the client number from the settings
      return `INSERT INTO t000 ('MANDT', 'CCCATEGORY', 'CCNOCLIIND') VALUES ('123', '', '');\n`;
    } else {
      return "";
    }
  }

  private insertT100(msag: abaplint.Objects.MessageClass): string {
    // ignore if T100 is unknown
    if (this.reg.getObject("TABL", "T100") === undefined) {
      return "";
    }
    let ret = "";
    for (const m of msag.getMessages()) {
      ret += `INSERT INTO t100 ('SPRSL', 'ARBGB', 'MSGNR', 'TEXT') VALUES ('E', '${msag.getName()}', '${m.getNumber()}', '${this.escape(m.getMessage())}');\n`;
    }
    return ret;
  }

  private escape(value: string): string {
    let ret = value.replace(/\'/g, "''");
    // statements are inside a javascript string stemplate
    ret = ret.replace(/\\/g, "\\\\");
    ret = ret.replace(/`/g, "\\`");
    return ret;
  }

}