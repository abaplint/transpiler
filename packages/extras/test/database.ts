import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {DatabaseSetupResult, IOutputFile, ITranspilerOptions, ITranspilerPlugin, Transpiler} from "@abaplint/transpiler";
import {plugin} from "../src";

import {t000, joinedView, viewFiles} from "./_cds";

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
    const reg = new abaplint.Registry();

    const res = await new Transpiler({}, new AmendDatabase()).run(reg);

    expect(res.databaseSetup.schemas.sqlite.join("\n")).to.include("CREATE TABLE zplugin");
    expect(res.databaseSetup.insert.join("\n")).to.include("INSERT INTO zplugin");
    expect(res.initializationScript).to.include("CREATE TABLE zplugin");
  });

});

describe("DDLS database setup", () => {

  it("creates a database view on top of a table", async () => {
    const ddls = `define view entity ZDDLS as select from t000 {
  key mandt,
      cccategory
}`;
    const reg = new abaplint.Registry()
      .addFile(new abaplint.MemoryFile("t000.tabl.xml", t000))
      .addFile(new abaplint.MemoryFile("zddls.ddls.asddls", ddls));

    const res = await new Transpiler({}, plugin).run(reg);
    const sqlite = res.databaseSetup.schemas.sqlite.join("\n");

    expect(sqlite).to.include(
      'CREATE VIEW "zddls" AS SELECT "t000"."mandt" AS "mandt", ' +
      '"t000"."cccategory" AS "cccategory" FROM "t000";');
  });

  it("preserves source fields, aliases, joins and filters in every schema and initialization", async () => {
    const reg = new abaplint.Registry().addFiles(viewFiles().map(f => new abaplint.MemoryFile(f.filename, f.contents)));
    const res = await new Transpiler({}, plugin).run(reg);
    const expected = 'CREATE VIEW "zddls" AS SELECT "item"."mandt" AS "purchasingdocument", ' +
      '"header"."cccategory" AS "headercategory", "text"."cccategory" AS "language", ' +
      '"change"."cccategory" AS "changestatus" FROM "t000" AS "item" ' +
      'INNER JOIN "t001" AS "header" ON "item"."mandt" = "header"."mandt" ' +
      'LEFT OUTER JOIN "t002" AS "text" ON "header"."mandt" = "text"."mandt" ' +
      'AND ( "text"."cccategory" = \'E\' OR "text"."cccategory" = \'F\' ) ' +
      'LEFT OUTER JOIN "t003" AS "change" ON "item"."mandt" = "change"."mandt" WHERE "item"."mandt" <> \'999\';';
    for (const dialect of ["sqlite", "pg", "snowflake"] as const) {
      expect(res.databaseSetup.schemas[dialect]).to.include(expected);
    }
    expect(res.initializationScript).to.include(expected);
    const direct: DatabaseSetupResult = {schemas: {sqlite: [], pg: [], snowflake: [], hdb: []}, insert: []};
    plugin.amendDatabaseSetup!(direct, reg, {});
    expect(direct.schemas.sqlite).to.deep.equal([expected]);
  });

  it("keeps the source column when a single-source projection is renamed", async () => {
    const reg = new abaplint.Registry().addFiles(viewFiles(
      "define view entity ZDDLS as select from t000 as client { key client.mandt as id }")
      .map(f => new abaplint.MemoryFile(f.filename, f.contents)));
    const res = await new Transpiler({}, plugin).run(reg);
    expect(res.databaseSetup.schemas.sqlite).to.include(
      'CREATE VIEW "zddls" AS SELECT "client"."mandt" AS "id" FROM "t000" AS "client";');
  });

  for (const [description, ddls, message] of [
    ["unresolved first source", joinedView.replace("t000 as item", "missing as item"), "source missing"],
    ["unresolved joined source", joinedView.replace("t003 as change", "missing as change"), "source missing"],
    ["computed projection", joinedView.replace("change.cccategory as ChangeStatus", "coalesce(change.cccategory, 'N') as ChangeStatus"),
      "coalesce"],
    ["union", joinedView + " union select from t000 { mandt, cccategory, cccategory, cccategory }", "unsupported"],
    ["grouping", joinedView + " group by item.mandt, header.cccategory, text.cccategory, change.cccategory", "group"],
    ["parameters", joinedView.replace("as select", "with parameters p : abap.char(3) as select"), "parameters"],
  ]) {
    it("fails explicitly for " + description, async () => {
      const reg = new abaplint.Registry().addFiles(viewFiles(ddls).map(f => new abaplint.MemoryFile(f.filename, f.contents)));
      reg.parse();
      const setup: DatabaseSetupResult = {schemas: {sqlite: [], pg: [], snowflake: [], hdb: []}, insert: []};
      expect(() => plugin.amendDatabaseSetup!(setup, reg, {})).to.throw("CDS view zddls")
        .with.property("message").that.includes(message);
      expect(setup.schemas.sqlite).to.deep.equal([]);
    });
  }

});
