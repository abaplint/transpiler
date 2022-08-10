/* eslint-disable max-len */
import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {msag_zag_unit_test, tabl_t100xml} from "./_data";
import {SQLiteDatabaseSchema} from "../src/db/sqlite_database_schema";
import {DatabaseSetup} from "../src/db";

describe("transpiler, database setup", () => {

  it("t100 create table", async () => {
    const tabl = new abaplint.MemoryFile("t100.tabl.xml", tabl_t100xml);
    const reg = new abaplint.Registry().addFile(tabl).parse();
    const result = new SQLiteDatabaseSchema(reg).run();
    expect(result[0]).to.equal(`CREATE TABLE t100 ('sprsl' NCHAR(1), 'arbgb' NCHAR(20), 'msgnr' NCHAR(3), 'text' NCHAR(73), PRIMARY KEY('sprsl','arbgb','msgnr'));`);
  });

  it("t100 create table, insert MSAG entries", async () => {
    const tabl = new abaplint.MemoryFile("t100.tabl.xml", tabl_t100xml);
    const msag = new abaplint.MemoryFile("zag_unit_test.msag.xml", msag_zag_unit_test);
    const reg = new abaplint.Registry().addFiles([tabl, msag]).parse();
    const result = new DatabaseSetup(reg).run().insert;
    expect(result[0]).to.include(`VALUES ('E', 'ZAG_UNIT_TEST', '000', 'hello world');`);
  });

});
