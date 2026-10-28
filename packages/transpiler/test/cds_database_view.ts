import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "../src";
import {DatabaseSetup} from "../src/db";

import {t000, joinedView, viewFiles} from "./_cds";

describe("DDLS database setup", () => {

  it("creates a database view on top of a table", async () => {
    const ddls = `define view entity ZDDLS as select from t000 {
  key mandt,
      cccategory
}`;
    const reg = new abaplint.Registry()
      .addFile(new abaplint.MemoryFile("t000.tabl.xml", t000))
      .addFile(new abaplint.MemoryFile("zddls.ddls.asddls", ddls));

    const res = await new Transpiler().run(reg);
    const sqlite = res.databaseSetup.schemas.sqlite.join("\n");

    expect(sqlite).to.include(
      'CREATE VIEW "zddls" AS SELECT "t000"."mandt" AS "mandt", ' +
      '"t000"."cccategory" AS "cccategory" FROM "t000";');
  });

  it("preserves source fields, aliases, joins and filters in every schema and initialization", async () => {
    const reg = new abaplint.Registry().addFiles(viewFiles().map(f => new abaplint.MemoryFile(f.filename, f.contents)));
    const res = await new Transpiler().run(reg);
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
    // after the CREATE TABLEs it selects from
    expect(res.databaseSetup.schemas.sqlite[res.databaseSetup.schemas.sqlite.length - 1]).to.equal(expected);
  });

  it("hardcodes session system language in join conditions", async () => {
    const ddls = joinedView.replace("text.cccategory = 'E' or text.cccategory = 'F'",
      "text.cccategory = $session.system_language or text.cccategory = 'F'");
    const reg = new abaplint.Registry().addFiles(viewFiles(ddls).map(f => new abaplint.MemoryFile(f.filename, f.contents)));
    const res = await new Transpiler().run(reg);
    expect(res.databaseSetup.schemas.sqlite.join("\n")).to.include(
      '"text"."cccategory" = \'E\' OR "text"."cccategory" = \'F\'');
  });

  it("keeps the source column when a single-source projection is renamed", async () => {
    const reg = new abaplint.Registry().addFiles(viewFiles(
      "define view entity ZDDLS as select from t000 as client { key client.mandt as id }")
      .map(f => new abaplint.MemoryFile(f.filename, f.contents)));
    const res = await new Transpiler().run(reg);
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
      expect(() => new DatabaseSetup(reg).run()).to.throw("CDS view zddls")
        .with.property("message").that.includes(message);
    });
  }

});
