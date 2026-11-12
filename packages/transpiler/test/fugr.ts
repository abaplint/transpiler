import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import * as sourceMap from "source-map";
import {Transpiler} from "../src";
import {UniqueIdentifier} from "../src/unique_identifier";
import {IFile, ITranspilerOptions} from "../src/types";

// function groups are compiled file by file into one chunk, so they exercise
// the accumulation logic in HandleFUGR: indentation must run once over the
// final chunk, not once per appended file
describe("FUGR", () => {

  const xml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0" serializer="LCL_OBJECT_FUGR" serializer_version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <AREAT>test</AREAT>
   <INCLUDES>
    <SOBJ_NAME>LZFGROUPTOP</SOBJ_NAME>
    <SOBJ_NAME>SAPLZFGROUP</SOBJ_NAME>
   </INCLUDES>
   <FUNCTIONS>
    <item>
     <FUNCNAME>ZFMODULE1</FUNCNAME>
     <SHORT_TEXT>one</SHORT_TEXT>
    </item>
    <item>
     <FUNCNAME>ZFMODULE2</FUNCNAME>
     <SHORT_TEXT>two</SHORT_TEXT>
    </item>
   </FUNCTIONS>
  </asx:values>
 </asx:abap>
</abapGit>`;

  const topxml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>LZFGROUPTOP</NAME>
    <DBAPL>S</DBAPL>
    <DBNA>D$</DBNA>
    <SUBC>I</SUBC>
    <APPL>S</APPL>
    <FIXPT>X</FIXPT>
    <LDBNAME>D$S</LDBNAME>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;

  const saplxml = `<?xml version="1.0" encoding="utf-8"?>
<abapGit version="v1.0.0">
 <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
  <asx:values>
   <PROGDIR>
    <NAME>SAPLZFGROUP</NAME>
    <DBAPL>S</DBAPL>
    <DBNA>D$</DBNA>
    <SUBC>F</SUBC>
    <APPL>S</APPL>
    <RLOAD>E</RLOAD>
    <FIXPT>X</FIXPT>
    <LDBNAME>D$S</LDBNAME>
    <UCCHECK>X</UCCHECK>
   </PROGDIR>
  </asx:values>
 </asx:abap>
</abapGit>`;

  // both function modules contain nested braces (IF inside FUNCTION); with
  // per-file indentation runs, the second file would be indented once more
  // than the first
  const fm1 = `FUNCTION zfmodule1.
  IF 1 = 1.
    WRITE 'one'.
  ENDIF.
ENDFUNCTION.`;

  const fm2 = `FUNCTION zfmodule2.
  IF 2 = 2.
    WRITE 'two'.
  ENDIF.
ENDFUNCTION.`;

  const files: IFile[] = [
    {filename: "zfgroup.fugr.zfmodule1.abap", contents: fm1},
    {filename: "zfgroup.fugr.zfmodule2.abap", contents: fm2},
    {filename: "zfgroup.fugr.lzfgrouptop.abap", contents: "FUNCTION-POOL zfgroup."},
    {filename: "zfgroup.fugr.lzfgrouptop.xml", contents: topxml},
    {filename: "zfgroup.fugr.saplzfgroup.abap", contents: "INCLUDE lzfgrouptop.\nINCLUDE lzfgroupuxx."},
    {filename: "zfgroup.fugr.saplzfgroup.xml", contents: saplxml},
    {filename: "zfgroup.fugr.xml", contents: xml},
  ];

  async function runFugr(inputFiles = files, options?: ITranspilerOptions) {
    UniqueIdentifier.reset();
    const memory = inputFiles.map(f => new abaplint.MemoryFile(f.filename, f.contents));
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory);
    const res = await new Transpiler(options).run(reg);
    const fugr = res.objects.find(o => o.object.type === "FUGR");
    expect(fugr).to.not.equal(undefined);
    return fugr!;
  }

  it("multi-file function group: indentation is consistent across files", async () => {
    const fugr = await runFugr();
    const js = fugr.chunk.getCode();
    const lines = js.split("\n");

    // both function modules expand to the same shape, so equivalent lines
    // must sit at identical indentation regardless of file order
    const indentOf = (needle: string): number => {
      const hits = lines.filter(l => l.includes(needle));
      expect(hits.length, `expected exactly one line containing "${needle}" in:\n${js}`).to.equal(1);
      return hits[0].length - hits[0].trimStart().length;
    };

    expect(indentOf("'one'")).to.equal(indentOf("'two'"));
    expect(indentOf("async function zfmodule1")).to.equal(indentOf("async function zfmodule2"));

    // indentation must follow brace depth everywhere: recompute the expected
    // indent per line with the same rules as runIndentationLogic
    let depth = 0;
    for (const l of lines) {
      const trimmed = l.trimStart();
      if (trimmed.startsWith("}")) {
        depth--;
      }
      const expected = depth > 0 ? depth * 2 : 0;
      expect(l.length - trimmed.length, `indent of line "${l}" in:\n${js}`).to.equal(expected);
      if (trimmed.endsWith(" {")) {
        depth++;
      }
    }
  });

  it("multi-file function group: mappings point into the correct file", async () => {
    const fugr = await runFugr();
    const js = fugr.chunk.getCode();
    const map = fugr.chunk.getMap(fugr.filename);
    const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(map));

    const lines = js.split("\n");
    for (const needle of [{code: "'one'", source: "zfgroup.fugr.zfmodule1.abap", line: 3},
                          {code: "'two'", source: "zfgroup.fugr.zfmodule2.abap", line: 3}]) {
      const index = lines.findIndex(l => l.includes(needle.code));
      expect(index, `generated line containing ${needle.code}`).to.be.greaterThan(-1);
      const generatedLine = lines[index];
      const orig = consumer.originalPositionFor({line: index + 1, column: generatedLine.length - generatedLine.trimStart().length});
      expect(orig.source).to.equal(needle.source);
      expect(orig.line).to.equal(needle.line);
    }
  });

  it("shares constructors across source files in the final function-group module", async () => {
    const repeated = (name: string) => `FUNCTION ${name}.
TYPES: BEGIN OF ty_shared,
  value TYPE i,
END OF ty_shared.
DATA item TYPE ty_shared.
ENDFUNCTION.`;
    const input = files.map(file => file.filename === "zfgroup.fugr.zfmodule1.abap"
      ? {...file, contents: repeated("zfmodule1")}
      : file.filename === "zfgroup.fugr.zfmodule2.abap"
        ? {...file, contents: repeated("zfmodule2")}
        : file);
    const fugr = await runFugr(input, {sharedTypeFactories: true});
    const code = fugr.chunk.getCode();
    expect(code.match(/function \$t_/g)?.length).to.equal(1);
    expect(code.match(/\$t_[\w$]+\(\)/g)?.length).to.be.greaterThan(2);
  });

  it("declares inline data in function module scope", async () => {
    const inlineFiles = files.map(file => file.filename === "zfgroup.fugr.zfmodule1.abap" ? {
      ...file,
      contents: `FUNCTION zfmodule1.
  DATA(ls_result) = 42.
  WRITE ls_result.
ENDFUNCTION.`,
    } : file);

    const fugr = await runFugr(inlineFiles);
    const js = fugr.chunk.getCode();
    const declaration = js.indexOf("let ls_result = new abap.types.Integer(");
    const assignment = js.indexOf("ls_result.set(");

    expect(declaration, js).to.be.greaterThan(-1);
    expect(assignment, js).to.be.greaterThan(declaration);
  });

  it("an include that is not there does not stop the function group", async () => {
    const input = files.map(file => file.filename === "zfgroup.fugr.saplzfgroup.abap"
      ? {...file, contents: "INCLUDE lzfgrouptop.\nINCLUDE lzfgroupf99.\nINCLUDE lzfgroupuxx."}
      : file);
    const fugr = await runFugr(input, {ignoreSyntaxCheck: true});
    expect(fugr.chunk.getCode()).to.include("INCLUDE lzfgroupf99");
  });

  describe("local class split over TOP and F01 includes", () => {
    const includeXml = (name: string) => topxml.replace("LZFGROUPTOP", name);
    const splitXml = xml.replace("<SOBJ_NAME>SAPLZFGROUP</SOBJ_NAME>",
                                 "<SOBJ_NAME>LZFGROUPF01</SOBJ_NAME>\n    <SOBJ_NAME>SAPLZFGROUP</SOBJ_NAME>");

    function buildFiles(top: string, f01: string): IFile[] {
      return [
        ...files.filter(f => f.filename !== "zfgroup.fugr.saplzfgroup.abap" && f.filename !== "zfgroup.fugr.xml"
          && f.filename !== "zfgroup.fugr.lzfgrouptop.abap"),
        {filename: "zfgroup.fugr.xml", contents: splitXml},
        {filename: "zfgroup.fugr.saplzfgroup.abap", contents: "INCLUDE lzfgrouptop.\nINCLUDE lzfgroupuxx.\nINCLUDE lzfgroupf01."},
        {filename: "zfgroup.fugr.lzfgrouptop.abap", contents: top},
        {filename: "zfgroup.fugr.lzfgroupf01.abap", contents: f01},
        {filename: "zfgroup.fugr.lzfgroupf01.xml", contents: includeXml("LZFGROUPF01")},
      ];
    }

    function expectParses(js: string) {
      // the module body is plain statements, a function body gives the same duplicate declaration checks
      expect(() => new Function(`return async () => {\n${js}\n};`), js).to.not.throw();
    }

    it("DEFINITION in TOP, IMPLEMENTATION in F01: one class declaration", async () => {
      const top = `FUNCTION-POOL zfgroup.
CLASS lcl_example DEFINITION.
  PUBLIC SECTION.
    METHODS run RETURNING VALUE(rv) TYPE i.
ENDCLASS.`;
      const f01 = `CLASS lcl_example IMPLEMENTATION.
  METHOD run.
    rv = 2.
  ENDMETHOD.
ENDCLASS.`;
      const js = (await runFugr(buildFiles(top, f01))).chunk.getCode();
      expect(js.match(/^\s*class lcl_example\b/gm)?.length, js).to.equal(1);
      expect(js, js).to.include("async run(");
      expectParses(js);
    });

    it("declaration-only class in TOP still gets a class", async () => {
      const top = `FUNCTION-POOL zfgroup.
CLASS lcl_abstract DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS run ABSTRACT.
ENDCLASS.`;
      const js = (await runFugr(buildFiles(top, "* nothing"))).chunk.getCode();
      expect(js.match(/^\s*class lcl_abstract\b/gm)?.length, js).to.equal(1);
      expectParses(js);
    });
  });

});
