import {expect} from "chai";
import {runSingleMapped} from "./_utils";

describe("Source Map", () => {

  it("TRY", async () => {
    const abap = `
TRY.
ENDTRY.`;

    const js =
`try {
}`;

    const map =
`{
  "version": 3,
  "sources": [],
  "names": [],
  "mappings": "",
  "file": "zfoobar.prog.mjs",
  "sourceRoot": ""
}`;

    const result = await runSingleMapped(abap, {ignoreSyntaxCheck: true});
    expect(result?.js).to.equal(js);
    expect(result?.map).to.equal(map);
  });

});