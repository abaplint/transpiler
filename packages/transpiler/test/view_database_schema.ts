import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {DatabaseSetup} from "../src/db";
import {zhead, zinfo, ztext, zvHeadText} from "./_view";

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

});
