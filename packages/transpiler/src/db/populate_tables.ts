import * as abaplint from "@abaplint/core";

export class PopulateTables {
  private readonly hasREPOSRC: boolean;
  private readonly hasSEOSUBCO: boolean;
  private readonly hasSEOSUBCODF: boolean;
  private readonly hasSEOSUBCOTX: boolean;
  private readonly hasT000: boolean;
  private readonly hasT100: boolean;

  public constructor(reg: abaplint.IRegistry) {
    this.hasREPOSRC = reg.getObject("TABL", "REPOSRC") !== undefined;
    this.hasSEOSUBCO = reg.getObject("TABL", "SEOSUBCO") !== undefined;
    this.hasSEOSUBCODF = reg.getObject("TABL", "SEOSUBCODF") !== undefined;
    this.hasSEOSUBCOTX = reg.getObject("TABL", "SEOSUBCOTX") !== undefined;
    this.hasT000 = reg.getObject("TABL", "T000") !== undefined;
    this.hasT100 = reg.getObject("TABL", "T100") !== undefined;
  }

  public insertREPOSRC(obj: abaplint.Objects.Class | abaplint.Objects.Interface): string {
    if (!this.hasREPOSRC) {
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
    if (!this.hasT100) {
      return [];
    }
    const ret = [];
    for (const m of msag.getMessages()) {
      ret.push(`INSERT INTO "t100" ("sprsl", "arbgb", "msgnr", "text") VALUES ('E', '${
        msag.getName().padEnd(20, " ")}', '${m.getNumber()}', '${this.escape(m.getMessage().padEnd(73, " "))}');`);
    }
    return ret;
  }

  public insertSEOSUBCO(obj: abaplint.Objects.Class | abaplint.Objects.Interface): string[] {
    const def = obj.getDefinition();
    const ret = [];

    if (def === undefined || !this.hasSEOSUBCO) {
      return [];
    }

    for (const method of def.getMethodDefinitions().getAll()) {
      for (const parameter of method.getParameters().getAll()) {
        ret.push(`INSERT INTO "seosubco" ("clsname", "cmpname", "sconame") VALUES ('${
          obj.getName()}', '${method.getName()}', '${parameter.getName()}');`);
      }
    }

    return ret;
  }

  public insertSEOSUBCODF(obj: abaplint.Objects.Class | abaplint.Objects.Interface): string[] {
    const def = obj.getDefinition();
    const ret = [];

    if (def === undefined || !this.hasSEOSUBCODF) {
      return [];
    }

    for (const method of def.getMethodDefinitions().getAll()) {
      for (const parameter of method.getParameters().getAll()) {
        ret.push(`INSERT INTO "seosubcodf" ("clsname", "cmpname", "sconame", "type") VALUES ('${
          obj.getName()}', '${method.getName()}', '${parameter.getName()}', '${parameter.getType().getQualifiedName()}');`);
      }
    }

    return ret;
  }

  public insertSEOSUBCOTX(obj: abaplint.Objects.Class | abaplint.Objects.Interface): string[] {
    const def = obj.getDefinition();
    const ret = [];

    if (def === undefined || !this.hasSEOSUBCOTX) {
      return [];
    }

    for (const method of def.getMethodDefinitions().getAll()) {
      for (const parameter of method.getParameters().getAll()) {
        ret.push(`INSERT INTO "SEOSUBCOTX" ("clsname", "cmpname", "langu", "descript") VALUES ('${
          obj.getName()}', '${method.getName()}', '${parameter.getName()}', 'E', 'todo');`);
      }
    }

    return ret;
  }

  public insertT000(): string {
    if (!this.hasT000) {
      return "";
    }

    // todo, this should take the client number from the settings
    return `INSERT INTO t000 ('mandt', 'cccategory', 'ccnocliind') VALUES ('123', '', '');`;
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