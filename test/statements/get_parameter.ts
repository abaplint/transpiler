import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET PARAMETER", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("simple", async () => {
    const code = `
    DATA lv_foo TYPE c LENGTH 10.
    GET PARAMETER ID 'ABC' FIELD lv_foo.
    WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

});