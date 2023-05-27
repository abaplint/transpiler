import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - concat", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("simple ampersand concat", async () => {
    const code = `
    DATA lv_value TYPE string.
    lv_value = 'foo' & 'bar' & 'moo'.
    ASSERT lv_value = 'foobarmoo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("simple ampersand concat", async () => {
    const code = `
    DATA lv_str1 TYPE string.
    DATA lv_str2 TYPE string.
    lv_str1 = |foo  |.
    lv_str2 = lv_str1 && '-'.
    WRITE lv_str2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo  -");
  });

});