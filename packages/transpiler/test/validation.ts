import {inspect} from "util";
import {expect} from "chai";
import {runSingle} from "./_utils";
import {Transpiler, TranspilerValidationError} from "../src";
import {Validation} from "../src/validation";
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
      // console.log(e) in the CLI shows the message and the stack, not the issues
      expect(Object.keys(e)).to.deep.equal([]);
      expect(inspect(e)).to.not.contain("issues");
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

  describe("@KERNEL allow list", () => {
    const kernelClass = (name: string) => new abaplint.MemoryFile(name.toLowerCase() + ".clas.abap", `
CLASS ${name} DEFINITION PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.
CLASS ${name} IMPLEMENTATION.
  METHOD run.
    WRITE '@KERNEL console.log("hello");'.
  ENDMETHOD.
ENDCLASS.`);

    it("no option, everything allowed", async () => {
      const reg = new abaplint.Registry().addFile(kernelClass("ZOTHER"));
      const issues = new Validation().run(reg);
      expect(issues.length).to.equal(0);
    });

    it("allowed object has no issue", async () => {
      const reg = new abaplint.Registry().addFile(kernelClass("ZALLOWED_CLASS"));
      const issues = new Validation({kernel: {allow: ["^ZALLOWED"]}}).run(reg);
      expect(issues.length).to.equal(0);
    });

    it("other object gets forbidden_kernel_write with file and row", async () => {
      const reg = new abaplint.Registry().addFile(kernelClass("ZOTHER")).addFile(kernelClass("ZALLOWED_CLASS"));
      const issues = new Validation({kernel: {allow: ["^ZALLOWED"]}}).run(reg);
      expect(issues.length).to.equal(1);
      expect(issues[0].getKey()).to.equal("forbidden_kernel_write");
      expect(issues[0].getFilename()).to.equal("zother.clas.abap");
      expect(issues[0].getStart().getRow()).to.equal(8);
      expect(issues[0].getMessage()).to.contain("ZOTHER");
    });

    it("empty allow list forbids every object", async () => {
      const reg = new abaplint.Registry().addFile(kernelClass("ZALLOWED_CLASS"));
      const issues = new Validation({kernel: {allow: []}}).run(reg);
      expect(issues.length).to.equal(1);
      expect(issues[0].getKey()).to.equal("forbidden_kernel_write");
    });

    it("patterns are matched case insensitively against the object name", async () => {
      const reg = new abaplint.Registry().addFile(kernelClass("ZALLOWED_CLASS"));
      const issues = new Validation({kernel: {allow: ["zallowed_class"]}}).run(reg);
      expect(issues.length).to.equal(0);
    });

    it("the issue is thrown by run()", async () => {
      const reg = new abaplint.Registry().addFile(kernelClass("ZOTHER"));
      try {
        await new Transpiler({kernel: {allow: ["^ZALLOWED"]}}).run(reg);
        expect.fail();
      } catch (e) {
        expect(e.message).to.contain("forbidden_kernel_write");
        expect(e.message).to.contain("zother.clas.abap:8");
      }
    });
  });
});
