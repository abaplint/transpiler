import * as abaplint from "@abaplint/core";

export class PopulateTables {
  private readonly reg: abaplint.IRegistry;

  public constructor(reg: abaplint.IRegistry) {
    this.reg = reg;
  }

  public insertREPOSRC(obj: abaplint.Objects.Class | abaplint.Objects.Interface): string {
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

  public insertT100(msag: abaplint.Objects.MessageClass): string[] {
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

  public insertSEOSUBCO(_obj: abaplint.Objects.Class | abaplint.Objects.Interface): string[] {
    // todo
    return [];
  }

  public insertSEOSUBCODF(_obj: abaplint.Objects.Class | abaplint.Objects.Interface): string[] {
    // todo
    return [];
  }

  public insertSEOSUBCOTX(_obj: abaplint.Objects.Class | abaplint.Objects.Interface): string[] {
    // todo
    return [];
  }

  public insertT000(): string {
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

  private escape(value: string): string {
    let ret = value.replace(/\'/g, "''");
    // statements are inside a javascript string stemplate
    ret = ret.replace(/\\/g, "\\\\");
    ret = ret.replace(/`/g, "\\`");
    ret = ret.replace(/\${/g, "\\${");
    return ret;
  }
}