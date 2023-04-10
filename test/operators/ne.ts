// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - NE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("char vs string", async () => {
    const code = `
ASSERT 'FOO' <> |FOO |.
ASSERT |FOO | <> 'FOO'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});