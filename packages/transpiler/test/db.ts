/* eslint-disable max-len */
import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {tabl_t100xml, zag_unit_test_v, zsingletablview} from "./_data.js";
import {SQLiteDatabaseSchema} from "../src/db/sqlite_database_schema.js";

describe("transpiler, database setup", () => {

  it("t100 create table", async () => {
    const tabl = new abaplint.MemoryFile("t100.tabl.xml", tabl_t100xml);
    const reg = new abaplint.Registry().addFile(tabl).parse();
    const result = new SQLiteDatabaseSchema(reg).run();
    expect(result[0]).to.equal(`CREATE TABLE 't100' ('sprsl' NCHAR(1) COLLATE RTRIM, 'arbgb' NCHAR(20) COLLATE RTRIM, 'msgnr' NCHAR(3) COLLATE RTRIM, 'text' NCHAR(73) COLLATE RTRIM, PRIMARY KEY('sprsl','arbgb','msgnr'));`);
  });

  it("VIEW", async () => {
    const view = new abaplint.MemoryFile("zag_unit_test_v.view.xml", zag_unit_test_v);
    const reg = new abaplint.Registry().addFiles([view]).parse();
    const result = new SQLiteDatabaseSchema(reg).run();
    expect(result[0]).to.equal(`CREATE VIEW 'zag_unit_test_v' AS SELECT 'zag_unit_test_t1'.mandt AS mandt, 'zag_unit_test_t1'.key_field AS key_field, 'zag_unit_test_t1'.data_field AS data_field, 'zag_unit_test_t2'.data AS data FROM 'zag_unit_test_t1' INNER JOIN 'zag_unit_test_t2' ON 'zag_unit_test_t1'.data_field = 'zag_unit_test_t2'.key_field;`);
  });

  it("VIEW, single TABL no join", async () => {
    const view = new abaplint.MemoryFile("zsingletablview.view.xml", zsingletablview);
    const reg = new abaplint.Registry().addFiles([view]).parse();
    const result = new SQLiteDatabaseSchema(reg).run();
    expect(result[0]).to.equal(`CREATE VIEW 'zsingletablview' AS SELECT 'sflight'.mandt AS mandt, 'sflight'.carrid AS carrid FROM 'sflight';`);
  });

});
