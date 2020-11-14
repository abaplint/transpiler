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

});