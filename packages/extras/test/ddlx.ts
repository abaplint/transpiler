import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {Transpiler} from "@abaplint/transpiler";
import {plugin} from "../src";

const ddlxSource = `@Metadata.layer: #CORE
annotate view ZEXAMPLE with
{
}`;

describe("handle DDLX", () => {

  it("DDLX is allowed and produces no output", async () => {
    const file = new abaplint.MemoryFile("zexample.ddlx.asddlxs", ddlxSource);
    const reg = new abaplint.Registry().addFile(file).parse();

    const res = await new Transpiler({}, plugin).run(reg);

    expect(res.objects.length).to.equal(0);
    expect(res.initializationScript).to.not.include("ddlx");
  });

  it("without the plugin, DDLX fails validation", async () => {
    const file = new abaplint.MemoryFile("zexample.ddlx.asddlxs", ddlxSource);
    const reg = new abaplint.Registry().addFile(file).parse();

    try {
      await new Transpiler().run(reg);
      expect.fail("expected validation error");
    } catch (error) {
      expect(error.message).to.include("allowed_object_types");
    }
  });

});
