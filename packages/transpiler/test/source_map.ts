import {expect} from "chai";
import * as sourceMap from "source-map";
import {ITranspilerOptions} from "../src";
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

  it("TRY", async () => {
    const abap = `TRY.
ENDTRY.`;

    const js =
`try {
}`;

    const map =
`{
  "version": 3,
  "sources": [
    "zfoobar.prog.abap"
  ],
  "names": [],
  "mappings": "AAAA;AACA",
  "file": "zfoobar.prog.mjs",
  "sourceRoot": ""
}`;

    const result = await runSingleMapped(abap, OPTIONS);
    expect(result?.js).to.equal(js);
    expect(result?.map).to.equal(map);
  });

  it("IF, count mappings", async () => {
    const abap = `IF foo CP bar.
ENDIF.`;

    const js =
`if (abap.compare.cp(foo, bar)) {
}`;

    const result = await runSingleMapped(abap, OPTIONS);
    expect(result?.js).to.equal(js);

    const perLine = await countMappingsPerGeneratedLine(result?.map);
    expect(perLine[1]).to.equal(3);
    expect(perLine[2]).to.equal(1);
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
  async constructor_() {
    this.me = new abap.types.ABAPObject();
    this.me.set(this);
    return this;
  }
  async bar() {
    abap.statements.write(new abap.types.Character({length: 3}).set('moo'));
  }
}
abap.Classes['ZCL_MAPTEST'] = zcl_maptest;
export {zcl_maptest};`;

    const result = await runSingleMapped(abap, OPTIONS, "zcl_maptest.clas.abap");
    expect(result?.js).to.equal(js);

    const perLine = await countMappingsPerGeneratedLine(result?.map);
    expect(perLine[8]).to.equal(2); // the WRITE statement
  });

});