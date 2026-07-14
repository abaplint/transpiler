import {expect} from "chai";
import * as sourceMap from "source-map";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";
import {execFileSync} from "child_process";
import {Position} from "@abaplint/core";
import {ITranspilerOptions} from "../src/types";
import {Chunk} from "../src/chunk";
import {runSingleMapped, validateSourceMap} from "./_utils";

const OPTIONS: ITranspilerOptions = {
  ignoreSyntaxCheck: true,
  addCommonJS: true,
};

async function countMappingsPerGeneratedLine(map: string | undefined) {
  if (map === undefined) {
    return {};
  }
  const ret: { [index: number]: number } = {};
  const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(map));
  consumer.computeColumnSpans();
  consumer.eachMapping((m) => {
    if (ret[m.generatedLine] === undefined) {
      ret[m.generatedLine] = 0;
    }
    ret[m.generatedLine]++;
  });
  return ret;
}

describe("Source Map", () => {

  it("IF, count mappings", async () => {
    const abap = `IF foo CP bar.
ENDIF.`;

    const js =
`if (abap.compare.cp(foo, bar)) {
}`;

    const result = await runSingleMapped(abap, OPTIONS);
    expect(result?.js).to.equal(js);

    const perLine = await countMappingsPerGeneratedLine(result?.map);
    expect(perLine[1]).to.equal(5);
    expect(perLine[2]).to.equal(1);
  });

  it("APPEND", async () => {
    const abap = "APPEND <ls_branch> TO rt_branches.";
    const js = `abap.statements.append({source: fs_ls_branch_, target: rt_branches});`;
    const result = await runSingleMapped(abap, OPTIONS);
    expect(result?.js).to.equal(js);

    const perLine = await countMappingsPerGeneratedLine(result?.map);
    expect(perLine[1]).to.equal(4);
  });

  it("LOOP head and target-assignment line both map", async () => {
    const abap = `DATA foo TYPE i.
DATA tab TYPE STANDARD TABLE OF i.
LOOP AT tab INTO foo.
  WRITE foo.
ENDLOOP.`;

    const result = await runSingleMapped(abap, OPTIONS);
    const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(result!.map!));
    const jsLines = result!.js.split("\n");

    const headIdx = jsLines.findIndex(l => l.includes("abap.statements.loop("));
    const targetIdx = jsLines.findIndex(l => l.includes("foo.set(") && l.includes("unique"));
    expect(headIdx).to.be.greaterThan(-1);
    expect(targetIdx).to.be.greaterThan(-1);

    // both the "for await(...)" head and the "foo.set(unique)" line map to the LOOP (abap line 3);
    // use each line's first non-whitespace column since the body line is indented
    const codeCol = (l: string) => l.length - l.trimStart().length;
    expect(consumer.originalPositionFor({line: headIdx + 1, column: codeCol(jsLines[headIdx])}).line).to.equal(3);
    expect(consumer.originalPositionFor({line: targetIdx + 1, column: codeCol(jsLines[targetIdx])}).line).to.equal(3);
  });

  it("CLEAR, target and trailing syntax mapped", async () => {
    const abap = `DATA foo TYPE i.
CLEAR foo.`;

    const result = await runSingleMapped(abap, OPTIONS);
    // the CLEAR statement is generated as "foo.clear();"
    expect(result?.js).to.include("foo.clear();");

    const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(result!.map!));
    const clearLine = result!.js.split("\n").findIndex(l => l.includes("foo.clear();")) + 1;

    // the target "foo" (generated column 0) maps back to CLEAR's operand on abap line 2
    const target = consumer.originalPositionFor({line: clearLine, column: 0});
    expect(target.line).to.equal(2);

    // there must also be a mapping for the ".clear();" trailing syntax on the same line
    let mappingsOnClearLine = 0;
    consumer.eachMapping(m => {
      if (m.generatedLine === clearLine) {
        mappingsOnClearLine++;
        // every original position must sit within the abap source (line >= 1)
        expect(m.originalLine).to.be.greaterThan(0);
      }
    });
    expect(mappingsOnClearLine).to.be.greaterThan(1);
  });

  it("baseline mapping for an otherwise unmapped statement (DATA)", async () => {
    // DATA builds a plain string chunk with no per-token mappings; the traversal
    // fallback must still give it a start mapping so it resolves to its source line
    const abap = `WRITE 'x'.
DATA foo TYPE i.`;

    const result = await runSingleMapped(abap, OPTIONS);
    const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(result!.map!));

    // find the generated line declaring foo, confirm it maps back to abap line 2
    const dataLine = result!.js.split("\n").findIndex(l => l.includes("foo")) + 1;
    expect(dataLine).to.be.greaterThan(0);
    const orig = consumer.originalPositionFor({line: dataLine, column: 0});
    expect(orig.line).to.equal(2);
  });

  it("global CLAS", async () => {
    const abap = `CLASS zcl_maptest DEFINITION PUBLIC CREATE PUBLIC.
PUBLIC SECTION.
  METHODS bar.
ENDCLASS.
CLASS zcl_maptest IMPLEMENTATION.
  METHOD bar.
    WRITE 'moo'.
  ENDMETHOD.
ENDCLASS.`;

    const js =
`class zcl_maptest {
  static STATIC_SUPER = undefined;
  static INTERNAL_TYPE = 'CLAS';
  static INTERNAL_NAME = 'ZCL_MAPTEST';
  static IMPLEMENTED_INTERFACES = [];
  static ATTRIBUTES = {};
  static METHODS = {"BAR": {"visibility": "U", "parameters": {}}};
  constructor() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    this.INTERNAL_ID = abap.internalIdCounter++;
    this.FRIENDS_ACCESS_INSTANCE = {
      "bar": this.bar.bind(this),
    };
  }
  async constructor_(INPUT) {
    if (super.constructor_) { await super.constructor_(INPUT); }
    return this;
  }
  async bar() {
    abap.statements.write(abap.CharacterFactory.get(3, 'moo'));
  }
}
abap.Classes['ZCL_MAPTEST'] = zcl_maptest;
export {zcl_maptest};`;

    const result = await runSingleMapped(abap, OPTIONS, "zcl_maptest.clas.abap");
    expect(result?.js).to.equal(js);

    const perLine = await countMappingsPerGeneratedLine(result?.map);
    expect(perLine[21]).to.equal(3); // the WRITE statement
  });

  it("method body: each statement kind resolves back to its abap line", async () => {
    const abap = `CLASS zcl_cov DEFINITION PUBLIC CREATE PUBLIC.
PUBLIC SECTION.
  METHODS run.
ENDCLASS.
CLASS zcl_cov IMPLEMENTATION.
  METHOD run.
    DATA lv_i TYPE i.
    DATA lt_tab TYPE STANDARD TABLE OF i.
    lv_i = 2.
    CLEAR lv_i.
    APPEND lv_i TO lt_tab.
    LOOP AT lt_tab INTO lv_i.
      WRITE lv_i.
    ENDLOOP.
    DO 3 TIMES.
      lv_i = lv_i + 1.
    ENDDO.
  ENDMETHOD.
ENDCLASS.`;

    const result = await runSingleMapped(abap, OPTIONS, "zcl_cov.clas.abap");
    // validateSourceMap throws if any mapping points outside the abap source
    await validateSourceMap(abap, result!.js, result!.map!);

    // each statement I wrote in the body must resolve back to its abap line;
    // collect the set of original lines the map covers and check the key ones
    const consumer = await new sourceMap.SourceMapConsumer(JSON.parse(result!.map!));
    const coveredOriginalLines = new Set<number>();
    consumer.eachMapping(m => {
      if (m.originalLine !== null) {
        coveredOriginalLines.add(m.originalLine);
      }
    });

    // 7=DATA 9=assign 10=CLEAR 11=APPEND 12=LOOP 13=WRITE 15=DO 16=assign
    for (const abapLine of [7, 9, 10, 11, 12, 13, 15, 16]) {
      expect(coveredOriginalLines.has(abapLine), `abap line ${abapLine} should be mapped`).to.equal(true);
    }
  });

  it("node --enable-source-maps rewrites the stack to the source file", async () => {
    // end-to-end: the map we emit must be consumable by V8's built-in source-map
    // support (the same machinery VS Code uses). Build a tiny executable file
    // through the real Chunk/getMap pipeline, run it, and inspect the stack.
    const trav = {getFilename: () => "zsrc.abap"};
    const c = new Chunk();
    c.append("function boom() {", new Position(1, 1), trav);
    c.appendString("\n");
    c.append("throw new Error('boom from abap');", new Position(2, 3), trav); // abap 2:3
    c.appendString("\n}\n");
    c.append("boom();", new Position(4, 1), trav); // abap 4:1
    c.appendString("\n");
    c.runIndentationLogic();

    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "smtest-"));
    try {
      const js = c.getCode() + "\n//# sourceMappingURL=prog.mjs.map";
      fs.writeFileSync(path.join(dir, "prog.mjs"), js);
      fs.writeFileSync(path.join(dir, "prog.mjs.map"), c.getMap("prog.mjs"));

      let stack = "";
      try {
        execFileSync(process.execPath, ["--enable-source-maps", path.join(dir, "prog.mjs")], {stdio: "pipe"});
      } catch (e: any) {
        stack = (e.stderr?.toString() || "") + (e.stdout?.toString() || "");
      }

      // V8 must map the throw back to zsrc.abap line 2 (column 3) and the call to line 4
      expect(stack).to.match(/zsrc\.abap:2:3/);
      expect(stack).to.match(/zsrc\.abap:4:1/);
    } finally {
      fs.rmSync(dir, {recursive: true, force: true});
    }
  });

});