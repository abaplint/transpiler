import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - LOOP AT SCREEN", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Basic", async () => {
    const code = `
LOOP AT SCREEN.
ENDLOOP.`;
    const js = await run(code);
    expect(js).to.include("{");
    expect(js).to.include("}");
  });

});