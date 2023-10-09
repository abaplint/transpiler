import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfloat.prog.abap", contents}]);
}

describe("Running Examples - Numeric type", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Numeric, initial", async () => {
    const code = `
DATA num TYPE n LENGTH 3.
ASSERT num IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Numeric, not initial", async () => {
    const code = `
DATA num TYPE n LENGTH 3.
num = 1.
ASSERT num IS NOT INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("numeric, offset", async () => {
    const code = `
DATA: lv_str TYPE string,
      lv_tmp TYPE n LENGTH 10.
lv_tmp = '0123456789'.
CONCATENATE lv_tmp+1 lv_str INTO lv_str.
WRITE lv_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123456789");
  });

  it("numeric, offset and length", async () => {
    const code = `
DATA: lv_str TYPE string,
      lv_tmp TYPE n LENGTH 10.
lv_tmp = '0123456789'.
CONCATENATE lv_tmp+1(2) lv_str INTO lv_str.
WRITE lv_str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12");
  });

  it("numeric, from float", async () => {
    const code = `
DATA lv_offset TYPE n LENGTH 5.
DATA lv_float TYPE f.
lv_float = 100.
lv_offset = lv_float.
WRITE lv_offset.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00100");
  });

});
