import {expect} from "chai";
import {runSingle} from "./_utils";
import {Transpiler, TranspilerValidationError} from "../src";
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

  it("Forbidden identifier", async () => {
    const abap = `DATA unique2 TYPE string.`;

    try {
      await runSingle(abap);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("allowed");
    }
  });

  it("throws TranspilerValidationError carrying the issues", async () => {
    const reg = new abaplint.Registry().addFile(
      new abaplint.MemoryFile("zfoobar.prog.abap", "WRITE foowrite."));

    try {
      await new Transpiler().run(reg);
      expect.fail();
    } catch (e) {
      expect(e).to.be.instanceof(TranspilerValidationError);
      expect(e).to.be.instanceof(Error);
      expect(e.issues.length).to.equal(1);
      expect(e.issues[0].getFilename()).to.equal("zfoobar.prog.abap");
      expect(e.issues[0].getStart().getRow()).to.equal(1);
      expect(e.message).to.equal(e.issues[0].getKey() + ", " + e.issues[0].getMessage() + ", zfoobar.prog.abap:1");
      expect(e.message).to.match(/^[a-z_]+, .+, zfoobar\.prog\.abap:1$/);
    }
  });

  it("joins several issues with a newline", async () => {
    const reg = new abaplint.Registry().addFile(
      new abaplint.MemoryFile("zfoobar.prog.abap", "WRITE foowrite.\nWRITE barwrite."));

    try {
      await new Transpiler().run(reg);
      expect.fail();
    } catch (e) {
      expect(e).to.be.instanceof(TranspilerValidationError);
      expect(e.issues.length).to.equal(2);
      expect(e.message.split("\n")).to.have.lengthOf(2);
    }
  });
});
