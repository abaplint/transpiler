import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - UNPACK", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("char to char", async () => {
    const code = `DATA lv_char4 TYPE c LENGTH 4.
DATA lv_char6 TYPE c LENGTH 6.
lv_char4 = '300'.
UNPACK lv_char4 TO lv_char6.
ASSERT lv_char6 = '000300'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("char to char, start space", async () => {
    const code = `DATA lv_char4 TYPE c LENGTH 4.
DATA lv_char6 TYPE c LENGTH 6.
lv_char4 = ' 300'.
UNPACK lv_char4 TO lv_char6.
ASSERT lv_char6 = '000300'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("throw CX_SY_CONVERSION_NO_NUMBER", async () => {
    const code = `
DATA lv_char4 TYPE c LENGTH 4.
DATA lv_char6 TYPE c LENGTH 6.
lv_char4 = 'A'.
UNPACK lv_char4 TO lv_char6.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e.toString()).to.contain("CX_SY_CONVERSION_NO_NUMBER");
    }
  });

});