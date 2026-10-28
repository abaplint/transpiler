import {expect} from "chai";
import {runSingle} from "./_utils";
import {Transpiler} from "../src";
import * as abaplint from "@abaplint/core";

describe("Validation", () => {
  it("parses an unparsed registry only once", async () => {
    const reg = new abaplint.Registry().addFile(
      new abaplint.MemoryFile("zfoobar.prog.abap", "DATA foo TYPE i."));
    const originalParse = reg.parse.bind(reg);
    let parseCalls = 0;
    (reg as any).parse = () => {
      parseCalls++;
      return originalParse();
    };

    await new Transpiler().run(reg);

    expect(parseCalls).to.equal(1);
  });

  it("Unknown variable, throws error", async () => {
    const abap = `WRITE foowrite.`;

    try {
      await runSingle(abap);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("not found");
    }
  });

  it("Unknown type, throws error", async () => {
    const abap = `DATA foo TYPE sdfsd.`;

    try {
      await runSingle(abap);
    } catch (e) {
      expect(e.message).to.contain("foo");
    }
  });

  it("SELECT SINGLE FOR UPDATE, throws error", async () => {
    const abap = `SELECT SINGLE FOR UPDATE * FROM t100 INTO ls_result.`;

    try {
      await runSingle(abap, {ignoreSyntaxCheck: true});
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("FOR UPDATE");
    }
  });

  it("Forbidden identifier", async () => {
    const abap = `DATA unique2 TYPE string.`;

    try {
      await runSingle(abap);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("allowed");
    }
  });

  it("leaves the registry's configuration as it found it", async () => {
    const custom = new abaplint.Config(JSON.stringify({
      global: {files: "/**/*.*"},
      syntax: {version: abaplint.Version.v702, errorNamespace: "^(Z|Y)"},
      rules: {line_length: {length: 10}},
    }));
    const original = JSON.parse(JSON.stringify(custom.get()));
    const reg = new abaplint.Registry(custom).addFile(
      new abaplint.MemoryFile("zfoobar.prog.abap", "DATA foo TYPE i.\nWRITE foo."));

    const output = await new Transpiler().run(reg);

    expect(output.objects.length).to.equal(1);
    expect(reg.getConfig()).to.equal(custom);
    expect(reg.getConfig().get()).to.deep.equal(original);
    expect(reg.getConfig().getVersion()).to.equal(abaplint.Version.v702);
    // the restored configuration is live again: line_length is not a transpiler rule
    const keys = reg.findIssues().map(i => i.getKey());
    expect(keys).to.include("line_length");
    expect(keys).to.not.include("forbidden_identifier");
  });

  it("restores the configuration also when validation throws", async () => {
    const custom = new abaplint.Config(JSON.stringify({
      global: {files: "/**/*.*"},
      syntax: {version: abaplint.Version.v702, errorNamespace: "^(Z|Y)"},
      rules: {},
    }));
    const reg = new abaplint.Registry(custom).addFile(
      new abaplint.MemoryFile("zfoobar.prog.abap", "WRITE foowrite."));

    try {
      await new Transpiler().run(reg);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("not found");
    }
    expect(reg.getConfig()).to.equal(custom);
    expect(reg.getConfig().getVersion()).to.equal(abaplint.Version.v702);
  });
});
