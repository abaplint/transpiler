import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - round", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test, half down", async () => {
    const code = `
    CONSTANTS half_down TYPE i VALUE 4.
    DATA lv_f TYPE f.
    DATA lv_num TYPE i.
    lv_f = '2.1'.
    lv_num = round( val = lv_f dec = 0 mode = half_down ).
    WRITE / lv_num.
    lv_f = '2.5'.
    lv_num = round( val = lv_f dec = 0 mode = half_down ).
    WRITE / lv_num.
    lv_f = '2.7'.
    lv_num = round( val = lv_f dec = 0 mode = half_down ).
    WRITE / lv_num.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `2\n2\n3`);
  });

  it("test, floor", async () => {
    const code = `
    CONSTANTS floor TYPE i VALUE 6.
    DATA lv_f TYPE f.
    DATA lv_num TYPE i.
    lv_f = '2.1'.
    lv_num = round( val = lv_f dec = 0 mode = floor ).
    WRITE / lv_num.
    lv_f = '2.5'.
    lv_num = round( val = lv_f dec = 0 mode = floor ).
    WRITE / lv_num.
    lv_f = '2.7'.
    lv_num = round( val = lv_f dec = 0 mode = floor ).
    WRITE / lv_num.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `2\n2\n2`);
  });

  it("half should round up", async () => {
    const code = `ASSERT round( val = 1 / 2 dec = 0 mode = 1 ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("2 div 5", async () => {
    const code = `ASSERT round( val = 2 / 5 dec = 0 mode = 1 ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("more rounding", async () => {
    const code = `
    DATA p TYPE p LENGTH 10 DECIMALS 2.
    p = round( val = '7.1' dec = 0 ).
    WRITE / p.
    p = round( val = '7.6' dec = 0 ).
    WRITE / p.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `7,00\n8,00`);
  });

});
