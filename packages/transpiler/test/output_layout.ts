import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import {OutputLayout, importPath} from "../src/output_layout";
import {Transpiler} from "../src";

describe("Output layout", () => {
  it("uses relative, URL-escaped specifiers without escaping directories", () => {
    expect(importPath("project/zapp.prog.mjs", "lib%#/x#foo.mjs")).to.equal("../lib%25%23/x%23foo.mjs");
    expect(importPath("lib/a.mjs", "lib/b.mjs")).to.equal("./b.mjs");
    expect(importPath("project/a.mjs", "_init.mjs")).to.equal("../_init.mjs");
  });

  it("keeps flat output for existing callers", async () => {
    const result = await new Transpiler().runRaw([{filename: "zapp.prog.abap", contents: "WRITE 'hello'."}]);
    expect(result.objects[0].filename).to.equal("zapp.prog.mjs");
  });

  it("rejects mixed object ownership and maps merged class locals", () => {
    const reg = new abaplint.Registry().addFiles([
      new abaplint.MemoryFile("zcl_app.clas.abap", ""),
      new abaplint.MemoryFile("zcl_app.clas.locals_def.abap", ""),
    ]);
    const folders = new Map([["zcl_app.clas.abap", "project"], ["zcl_app.clas.locals_def.abap", "lib"]]);
    expect(() => new OutputLayout(reg, folders)).to.throw("Ambiguous output ownership");
    folders.set("zcl_app.clas.locals_def.abap", "project");
    expect(new OutputLayout(reg, folders).sourceModule("zcl_app.clas.locals.abap")).to.equal("project/zcl_app.clas.locals.mjs");
  });

  it("resolves dependency-to-project and same-dependency inheritance", async () => {
    const files = [
      {filename: "acl.clas.abap", contents: "CLASS acl DEFINITION PUBLIC. ENDCLASS. CLASS acl IMPLEMENTATION. ENDCLASS."},
      {filename: "bcl.clas.abap", contents:
        "CLASS bcl DEFINITION PUBLIC INHERITING FROM acl. ENDCLASS. CLASS bcl IMPLEMENTATION. ENDCLASS."},
      {filename: "ccl.clas.abap", contents:
        "CLASS ccl DEFINITION PUBLIC INHERITING FROM bcl. ENDCLASS. CLASS ccl IMPLEMENTATION. ENDCLASS."},
    ];
    const reg = new abaplint.Registry().addFiles(files.map(file => new abaplint.MemoryFile(file.filename, file.contents)));
    const output = await new Transpiler({addCommonJS: true}).run(reg, undefined,
      new Map([["acl.clas.abap", "project"], ["bcl.clas.abap", "lib"], ["ccl.clas.abap", "lib"]]));
    expect(output.objects.find(file => file.filename === "lib/bcl.clas.mjs")?.chunk.getCode())
      .to.contain('import("../project/acl.clas.mjs")');
    expect(output.objects.find(file => file.filename === "lib/ccl.clas.mjs")?.chunk.getCode()).to.contain('import("./bcl.clas.mjs")');
  });

  it("keeps initialization order independent of folder names", async () => {
    const reg = new abaplint.Registry().addFiles([
      new abaplint.MemoryFile("acl.clas.abap", "CLASS acl DEFINITION PUBLIC. ENDCLASS. CLASS acl IMPLEMENTATION. ENDCLASS."),
      new abaplint.MemoryFile("zcl.clas.abap", "CLASS zcl DEFINITION PUBLIC. ENDCLASS. CLASS zcl IMPLEMENTATION. ENDCLASS."),
    ]);
    const output = await new Transpiler().run(reg, undefined,
      new Map([["acl.clas.abap", "zfolder"], ["zcl.clas.abap", "afolder"]]));
    expect(output.initializationScript.indexOf("zfolder/acl")).to.be.lessThan(output.initializationScript.indexOf("afolder/zcl"));
  });

  it("rejects output paths escaping the owning folder", () => {
    const reg = new abaplint.Registry().addFile(new abaplint.MemoryFile("zapp.prog.abap", ""));
    const layout = new OutputLayout(reg, new Map([["zapp.prog.abap", "project"]]));
    expect(() => layout.file({type: "PROG", name: "ZAPP"}, "../init.mjs")).to.throw("Invalid output filename");
  });
});
