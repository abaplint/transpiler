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
    abap.statements.write(new abap.types.Character(3).set('moo'));
  }
}
abap.Classes['ZCL_MAPTEST'] = zcl_maptest;
export {zcl_maptest};`;

    const result = await runSingleMapped(abap, OPTIONS, "zcl_maptest.clas.abap");
    expect(result?.js).to.equal(js);

    const perLine = await countMappingsPerGeneratedLine(result?.map);
    expect(perLine[20]).to.equal(3); // the WRITE statement
  });

});