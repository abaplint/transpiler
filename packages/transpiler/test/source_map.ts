import {expect} from "chai";
import * as sourceMap from "source-map";
import {ITranspilerOptions} from "../src/types";
import {runSingleMapped} from "./_utils";

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

});