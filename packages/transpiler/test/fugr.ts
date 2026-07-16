import {expect} from "chai";
import * as abaplint from "@abaplint/core";
import * as sourceMap from "source-map";
import {Transpiler} from "../src";
import {UniqueIdentifier} from "../src/unique_identifier";
import {IFile} from "../src/types";

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

  async function runFugr() {
    UniqueIdentifier.reset();
    const memory = files.map(f => new abaplint.MemoryFile(f.filename, f.contents));
    const reg: abaplint.IRegistry = new abaplint.Registry().addFiles(memory).parse();
    const res = await new Transpiler().run(reg);
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

});
