// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET RUN TIME", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("simple", async () => {
    const code = `
    DATA lv_start TYPE i.
    GET RUN TIME FIELD lv_start.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});