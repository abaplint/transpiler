// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - concat", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("char ends with newline", async () => {
    const code = `
    DATA char1 TYPE c LENGTH 3.
    DATA char2 TYPE c LENGTH 3.
    DATA res TYPE string.
    char1 = |ab\\n|.
    char2 = |cde|.
    res = char1 && char2.
    ASSERT res = |ab\\ncde|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("simple ampersand concat", async () => {
    const code = `
    DATA lv_value TYPE string.
    lv_value = 'foo' & 'bar'.
    ASSERT lv_value = 'foobar'.`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});