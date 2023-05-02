import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - ENHANCEMENT SECTION", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("section", async () => {
    const code = `
ENHANCEMENT-SECTION foobar SPOTS boofoo.
  WRITE 'foo'.
end-enhancement-section.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

});