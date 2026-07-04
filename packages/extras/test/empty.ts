import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "@abaplint/transpiler";
import {plugin} from "../src";

const abapgitXML = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
</abapGit>`;

const testFiles: {[filename: string]: string} = {
  "zexample.ddlx.asddlxs": `@Metadata.layer: #CORE
annotate view ZDDLS with
{
}`,
  "zddls.ddls.asddls": `define view entity ZDDLS as select from t000 { key mandt }`,
  "zbdef.bdef.asbdef": `managed implementation in class zbp_example unique;
define behavior for ZDDLS
{
}`,
  "zsrvd.srvd.srvdsrv": `define service ZSRVD {
  expose ZDDLS;
}`,
  "zaplo.aplo.xml": abapgitXML,
  "zsajc.sajc.xml": abapgitXML,
  "zsajt.sajt.xml": abapgitXML,
  "zsco2.sco2.xml": abapgitXML,
  "zsia6.sia6.xml": abapgitXML,
  "zsrvb.srvb.xml": abapgitXML,
};

describe("empty handlers", () => {

  for (const [filename, contents] of Object.entries(testFiles)) {
    it(filename + " is allowed and produces no output", async () => {
      const file = new abaplint.MemoryFile(filename, contents);
      const reg = new abaplint.Registry().addFile(file).parse();

      const res = await new Transpiler({}, plugin).run(reg);

      expect(res.objects.length).to.equal(0);
      expect(res.initializationScript).to.not.include(filename.split(".")[1]);
    });
  }

  it("without the plugin, DDLX fails validation", async () => {
    const file = new abaplint.MemoryFile("zexample.ddlx.asddlxs", testFiles["zexample.ddlx.asddlxs"]);
    const reg = new abaplint.Registry().addFile(file).parse();

    try {
      await new Transpiler().run(reg);
      expect.fail("expected validation error");
    } catch (error) {
      expect(error.message).to.include("allowed_object_types");
    }
  });

});
