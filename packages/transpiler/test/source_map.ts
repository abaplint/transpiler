import {expect} from "chai";
import * as sourceMap from "source-map";
import {runSingleMapped} from "./_utils";

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

    const result = await runSingleMapped(abap, {ignoreSyntaxCheck: true});
    expect(result?.js).to.equal(js);
    expect(result?.map).to.equal(map);
  });

  it("IF", async () => {
    const abap = `IF foo CP bar.
ENDIF.`;

    const js =
`if (abap.compare.cp(foo, bar)) {
}`;

    const result = await runSingleMapped(abap, {ignoreSyntaxCheck: true});
    expect(result?.js).to.equal(js);

    const perLine = await countMappingsPerGeneratedLine(result?.map);
    expect(perLine[1]).to.equal(3);
    expect(perLine[2]).to.equal(1);
  });

});