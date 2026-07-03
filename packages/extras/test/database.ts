import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult, IOutputFile, ITranspilerOptions, ITranspilerPlugin, Transpiler} from "@abaplint/transpiler";

class AmendDatabase implements ITranspilerPlugin {
  public objectTypes(): string[] {
    return [];
  }

  public handleObject(_obj: abaplint.IObject, _reg: abaplint.IRegistry, _options: ITranspilerOptions): IOutputFile[] | undefined {
    return undefined;
  }

  public amendDatabaseSetup(dbSetup: DatabaseSetupResult, _reg: abaplint.IRegistry, _options: ITranspilerOptions): void {
    dbSetup.schemas.sqlite.push("CREATE TABLE zplugin (foo NCHAR(1));");
    dbSetup.insert.push("INSERT INTO zplugin VALUES ('A');");
  }
}

describe("amend database setup", () => {

  it("plugin schema and insert statements are added", async () => {
    const reg = new abaplint.Registry().parse();

    const res = await new Transpiler({}, new AmendDatabase()).run(reg);

    expect(res.databaseSetup.schemas.sqlite.join("\n")).to.include("CREATE TABLE zplugin");
    expect(res.databaseSetup.insert.join("\n")).to.include("INSERT INTO zplugin");
    expect(res.initializationScript).to.include("CREATE TABLE zplugin");
  });

});
