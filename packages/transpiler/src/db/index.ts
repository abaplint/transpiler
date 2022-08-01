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
        insert += this.messageClass(obj);
      }
    }
    insert += this.t000Insert();
    return insert;
  }

  private t000Insert(): string {
    const obj = this.reg.getObject("TABL", "T000") as abaplint.Objects.Table | undefined;
    if (obj === undefined) {
      return "";
    }

    const type = obj.parseType(this.reg);
    if (type instanceof abaplint.BasicTypes.StructureType && type.getComponents().length === 3) {
      // todo, this should take the client number from the settings
      return `INSERT INTO t000 VALUES ('123', '', '');\n`;
    } else {
      return "";
    }
  }

  private messageClass(msag: abaplint.Objects.MessageClass): string {
    // ignore if T100 is unknown
    if (this.reg.getObject("TABL", "T100") === undefined) {
      return "";
    }
    let ret = "";
    for (const m of msag.getMessages()) {
      ret += `INSERT INTO t100 VALUES ('E', '${msag.getName()}', '${m.getNumber()}', '${m.getMessage().replace(/\'/g, "''")}');\n`;
    }
    return ret;
  }

}