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
  it("imports objects of external folders from their module specifier", () => {
    const reg = new abaplint.Registry().addFiles([
      new abaplint.MemoryFile("zapp.prog.abap", ""),
      new abaplint.MemoryFile("cx_root.clas.abap", ""),
    ]);
    const layout = new OutputLayout(reg, new Map([["zapp.prog.abap", "project"], ["cx_root.clas.abap", "core"]]),
      new Map([["core", "@scope/pkg/output/core/"]]));
    expect(layout.isExternal({type: "CLAS", name: "CX_ROOT"})).to.equal(true);
    expect(layout.isExternal({type: "PROG", name: "ZAPP"})).to.equal(false);
    expect(layout.specifier("project/zapp.prog.mjs", "core/cx_root.clas.mjs")).to.equal("@scope/pkg/output/core/cx_root.clas.mjs");
    expect(layout.specifier("project/zapp.prog.mjs", "core/%23cl#x.clas.mjs")).to.equal("@scope/pkg/output/core/%2523cl%23x.clas.mjs");
    expect(layout.specifier("project/a.mjs", "project/b.mjs")).to.equal("./b.mjs");
    expect(() => new OutputLayout(reg, undefined, new Map([["core", " "]]))).to.throw("Empty import specifier");
  });

  it("does not emit external objects and imports them by specifier", async () => {
    const reg = new abaplint.Registry().addFiles([
      new abaplint.MemoryFile("zcl_app.clas.abap",
        "CLASS zcl_app DEFINITION PUBLIC INHERITING FROM lcl_base. ENDCLASS. CLASS zcl_app IMPLEMENTATION. ENDCLASS."),
    ]);
    reg.addDependency(new abaplint.MemoryFile("lcl_base.clas.abap",
      "CLASS lcl_base DEFINITION PUBLIC. ENDCLASS. CLASS lcl_base IMPLEMENTATION. ENDCLASS."));
    const output = await new Transpiler({addCommonJS: true}).run(reg, undefined,
      new Map([["zcl_app.clas.abap", "project"], ["lcl_base.clas.abap", "core"]]), new Map([["core", "pkg/core"]]));
    expect(output.objects.map(file => file.filename)).to.deep.equal(["project/zcl_app.clas.mjs"]);
    expect(output.objects[0].chunk.getCode()).to.contain('import("pkg/core/lcl_base.clas.mjs")');
    expect(output.initializationScript).to.contain('"pkg/core/lcl_base.clas.mjs"');
    expect(output.initializationScript).to.contain('"./project/zcl_app.clas.mjs"');
  });
});
