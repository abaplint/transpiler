import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {DatabaseSetup} from "../src/db";
import {zhead, zinfo, ztext, zvExtPrio, zvHeadText} from "./_view";

function setup(view: string) {
  const reg = new abaplint.Registry().addFiles([
    new abaplint.MemoryFile("zhead.tabl.xml", zhead),
    new abaplint.MemoryFile("ztext.tabl.xml", ztext),
    new abaplint.MemoryFile("zinfo.tabl.xml", zinfo),
    new abaplint.MemoryFile("zv_head_text.view.xml", view),
  ]).parse();
  return new DatabaseSetup(reg).run();
}

describe("VIEW database setup", () => {

  it("joins a constant condition and a third table joined to the second", async () => {
    const res = setup(zvHeadText());
    expect(res.schemas.pg).to.include('CREATE VIEW "zv_head_text" AS SELECT ' +
      '"zhead".mandt AS mandt, "zhead".id AS id, "ztext".descr AS descr, "zinfo".info AS info FROM "zhead" ' +
      'INNER JOIN "ztext" ON "ztext".mandt = "zhead".mandt AND "ztext".id = "zhead".id AND "ztext".langu = \'EN\' ' +
      'INNER JOIN "zinfo" ON "zinfo".mandt = "ztext".mandt AND "zinfo".id = "ztext".id;');
  });

  it("escapes a constant for the javascript template string", async () => {
    const res = setup(zvHeadText("&apos;`${x}\\&apos;"));
    expect(res.schemas.sqlite[res.schemas.sqlite.length - 1]).to.include("'ztext'.langu = '\\`\\${x}\\\\'");
  });

  it("external view, VIEWCLASS X, becomes a table with the EXVDATATYPE columns", async () => {
    const reg = new abaplint.Registry().addFiles([
      new abaplint.MemoryFile("zvext_prio.view.xml", zvExtPrio),
    ]).parse();
    const res = new DatabaseSetup(reg).run();
    expect(res.schemas.sqlite).to.deep.equal(["CREATE TABLE 'zvext_prio' (" +
      "'item' NCHAR(18) COLLATE RTRIM, 'ean' NCHAR(14) COLLATE RTRIM, 'flag_a' NCHAR(1) COLLATE RTRIM, " +
      "'flag_b' NCHAR(1) COLLATE RTRIM, 'check' NCHAR(2) COLLATE RTRIM, 'test' INT, 'unit' NCHAR(3) COLLATE RTRIM, " +
      "'request' NCHAR(12) COLLATE RTRIM, 'status' NCHAR(2) COLLATE RTRIM, " +
      "PRIMARY KEY('item','ean','flag_a','flag_b','check','test','unit','request','status'));"]);
    expect(res.schemas.pg).to.have.length(1);
    expect(res.schemas.pg[0]).to.match(/^CREATE TABLE "zvext_prio" \("item" /);
    expect(res.schemas.snowflake[0]).to.match(/^CREATE TABLE "zvext_prio" \("item" /);
  });

  it("a view without tables gives no database view and does not stop the setup", async () => {
    const reg = new abaplint.Registry().addFiles([
      new abaplint.MemoryFile("zv_empty.view.xml", `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_VIEW" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD25V>
    <VIEWNAME>ZV_EMPTY</VIEWNAME>
    <VIEWCLASS>C</VIEWCLASS>
   </DD25V>
  </asx:values>
 </asx:abap>
</abapGit>`),
    ]).parse();
    const res = new DatabaseSetup(reg).run();
    expect(res.schemas.sqlite.join("")).to.not.include("zv_empty");
  });

});
