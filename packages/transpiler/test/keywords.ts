import {MemoryFile, Registry} from "@abaplint/core";
import {expect} from "chai";
import {Keywords} from "../src/keywords";

describe("Keywords", () => {

  it("form name", async () => {
    const file = new MemoryFile("zfoo1.prog.abap", "form int_to_string.\nendform.");
    const reg = new Registry().addFile(file);

    new Keywords().handle(reg);

    const after = reg.getFirstObject()?.getFiles()[0].getRaw();
    expect(after).to.equal("form int_to_string.\nendform.");
  });

  it("data name", async () => {
    const file = new MemoryFile("zfoo1.prog.abap", "data int_to_string type c.");
    const reg = new Registry().addFile(file);

    new Keywords().handle(reg);

    const after = reg.getFirstObject()?.getFiles()[0].getRaw();
    expect(after).to.equal("data int_to_string type c.");
  });

  it("abstract, keyword, expect renaming", async () => {
    const file = new MemoryFile("zfoo1.prog.abap", "data abstract type i.");
    const reg = new Registry().addFile(file);

    new Keywords().handle(reg);

    const after = reg.getFirstObject()?.getFiles()[0].getRaw();
    expect(after).to.equal("data abstract_ type i.");
  });

  it("abstract, exclamation", async () => {
    const file = new MemoryFile("zfoo1.prog.abap", `INTERFACE lif.
  METHODS bar EXPORTING !return TYPE i.
ENDINTERFACE.`);
    const reg = new Registry().addFile(file);

    new Keywords().handle(reg);

    const after = reg.getFirstObject()?.getFiles()[0].getRaw();
    expect(after).to.equal(`INTERFACE lif.
  METHODS bar EXPORTING !return_ TYPE i.
ENDINTERFACE.`);
  });

  it("macro contents", async () => {
    const file = new MemoryFile("zfoo1.prog.abap", `
data: begin of sdfsdf,
        interface type i,
      end of sdfsdf.

define _foo.
  clear sdfsdf-&1.
end-of-definition.

_foo interface.`);
    const reg = new Registry().addFile(file);

    new Keywords().handle(reg);

    const after = reg.getFirstObject()?.getFiles()[0].getRaw();
    expect(after).to.equal(`
data: begin of sdfsdf,
        interface_ type i,
      end of sdfsdf.

define _foo.
  clear sdfsdf-&1.
end-of-definition.

_foo interface_.`);
  });

});