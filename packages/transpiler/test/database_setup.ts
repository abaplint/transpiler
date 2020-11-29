import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {msag_zag_unit_test, tabl_t100xml} from "./_data";
import {DatabaseSetup} from "../src/database_setup";

describe("transpiler, database setup", () => {

  it("t100 create table", async () => {
    const tabl = new abaplint.MemoryFile("t100.tabl.xml", tabl_t100xml);
    const reg = new abaplint.Registry().addFile(tabl).parse();
    const result = new DatabaseSetup(reg).run();
    expect(result).to.equal(`CREATE TABLE t100 (sprsl NCHAR(1), arbgb NCHAR(20), msgnr NCHAR(3), text NCHAR(73));`);
  });

  it("t100 create table, insert MSAG entries", async () => {
    const tabl = new abaplint.MemoryFile("t100.tabl.xml", tabl_t100xml);
    const msag = new abaplint.MemoryFile("zag_unit_test.msag.xml", msag_zag_unit_test);
    const reg = new abaplint.Registry().addFiles([tabl, msag]).parse();
    const result = new DatabaseSetup(reg).run();
    expect(result).to.include(`INSERT INTO t100 VALUES ('E', 'ZAG_UNIT_TEST', '000', 'hello world');`);
  });

});
