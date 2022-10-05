/* eslint-disable max-len */
import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {msag_zag_unit_test, tabl_t100xml, zag_unit_test_v} from "./_data";
import {SQLiteDatabaseSchema} from "../src/db/sqlite_database_schema";
import {DatabaseSetup} from "../src/db";

describe("transpiler, database setup", () => {

  it("t100 create table", async () => {
    const tabl = new abaplint.MemoryFile("t100.tabl.xml", tabl_t100xml);
    const reg = new abaplint.Registry().addFile(tabl).parse();
    const result = new SQLiteDatabaseSchema(reg).run();
    expect(result[0]).to.equal(`CREATE TABLE 't100' ('sprsl' NCHAR(1), 'arbgb' NCHAR(20), 'msgnr' NCHAR(3), 'text' NCHAR(73), PRIMARY KEY('sprsl','arbgb','msgnr'));`);
  });

  it("t100 create table, insert MSAG entries", async () => {
    const tabl = new abaplint.MemoryFile("t100.tabl.xml", tabl_t100xml);
    const msag = new abaplint.MemoryFile("zag_unit_test.msag.xml", msag_zag_unit_test);
    const reg = new abaplint.Registry().addFiles([tabl, msag]).parse();
    const result = new DatabaseSetup(reg).run().insert;
    expect(result[0]).to.include(`VALUES ('E', 'ZAG_UNIT_TEST', '000', 'hello world');`);
  });

  it("VIEW", async () => {
    const view = new abaplint.MemoryFile("zag_unit_test_v.view.xml", zag_unit_test_v);
    const reg = new abaplint.Registry().addFiles([view]).parse();
    const result = new SQLiteDatabaseSchema(reg).run();
    expect(result[0]).to.equal(`CREATE VIEW 'zag_unit_test_v' AS SELECT zag_unit_test_t1.mandt AS mandt, zag_unit_test_t1.key_field AS key_field, zag_unit_test_t1.data_field AS data_field, zag_unit_test_t2.data AS data FROM zag_unit_test_t1 INNER JOIN zag_unit_test_t2 ON zag_unit_test_t1.data_field = zag_unit_test_t2.key_field;`);
  });

});
