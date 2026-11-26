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

  it("merges the DDIC definitions of each folder into one module", async () => {
    const dtel = (name: string) => `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V><ROLLNAME>${name}</ROLLNAME><DDTEXT>text</DDTEXT><DATATYPE>CHAR</DATATYPE><LENG>000010</LENG></DD04V>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const tabl = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_TABL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD02V><TABNAME>ZTAB</TABNAME><TABCLASS>INTTAB</TABCLASS></DD02V>
   <DD03P_TABLE>
    <DD03P><FIELDNAME>FIELD</FIELDNAME><ROLLNAME>ZPROJECT</ROLLNAME><COMPTYPE>E</COMPTYPE></DD03P>
    <DD03P><FIELDNAME>OTHER</FIELDNAME><ROLLNAME>ZLIB</ROLLNAME><COMPTYPE>E</COMPTYPE></DD03P>
   </DD03P_TABLE>
  </asx:values>
 </asx:abap>
</abapGit>`;
    const files = [
      {filename: "zproject.dtel.xml", contents: dtel("ZPROJECT")},
      {filename: "ztab.tabl.xml", contents: tabl},
      {filename: "zlib.dtel.xml", contents: dtel("ZLIB")},
      {filename: "acl.clas.abap", contents: "CLASS acl DEFINITION PUBLIC. ENDCLASS. CLASS acl IMPLEMENTATION. ENDCLASS."},
    ];
    const reg = new abaplint.Registry().addFiles(files.map(file => new abaplint.MemoryFile(file.filename, file.contents)));
    const output = await new Transpiler().run(reg, undefined, new Map([
      ["zproject.dtel.xml", "project"], ["ztab.tabl.xml", "project"], ["zlib.dtel.xml", "lib"], ["acl.clas.abap", "project"]]));

    expect(output.objects.map(file => file.filename).sort())
      .to.deep.equal(["lib/_ddic.mjs", "project/_ddic.mjs", "project/acl.clas.mjs"]);

    const DDIC: {[name: string]: {objectType: string}} = {};
    for (const file of output.objects.filter(file => file.object.type === "DDIC")) {
      new Function("abap", file.chunk.getCode())({DDIC});
    }
    expect(Object.keys(DDIC).sort()).to.deep.equal(["ZLIB", "ZPROJECT", "ZTAB"]);
    expect(DDIC["ZTAB"].objectType).to.equal("TABL");

    for (const script of [output.initializationScript, output.initializationScript2]) {
      expect(script).to.not.contain(".dtel.mjs");
      expect(script).to.not.contain(".tabl.mjs");
      const lib = script.indexOf("./lib/_ddic.mjs");
      const project = script.indexOf("./project/_ddic.mjs");
      expect(lib).to.be.greaterThan(-1);
      expect(project).to.be.greaterThan(lib);
      expect(script.indexOf("./project/acl.clas.mjs")).to.be.greaterThan(project);
    }
  });

  it("keeps one module per DDIC object in flat output", async () => {
    const result = await new Transpiler().runRaw([{filename: "zint8.dtel.xml", contents: `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_DTEL" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <DD04V><ROLLNAME>ZINT8</ROLLNAME><DATATYPE>CHAR</DATATYPE><LENG>000010</LENG></DD04V>
  </asx:values>
 </asx:abap>
</abapGit>`}]);
    expect(result.objects.map(file => file.filename)).to.deep.equal(["zint8.dtel.mjs"]);
    expect(result.initializationScript).to.contain("./zint8.dtel.mjs");
  });

  it("rejects output paths escaping the owning folder", () => {
    const reg = new abaplint.Registry().addFile(new abaplint.MemoryFile("zapp.prog.abap", ""));
    const layout = new OutputLayout(reg, new Map([["zapp.prog.abap", "project"]]));
    expect(() => layout.file({type: "PROG", name: "ZAPP"}, "../init.mjs")).to.throw("Invalid output filename");
  });
});
