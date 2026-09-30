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

  it("dec, rounds as a decimal number", async () => {
    // scaling the nearest binary float gives 2.67 here, ABAP rounds the decimal 2.675
    const code = `
    CONSTANTS half_even TYPE i VALUE 3.
    DATA p TYPE p LENGTH 10 DECIMALS 3 VALUE '2.675'.
    WRITE / |{ round( val = p dec = 2 ) }|.
    p = '-2.675'.
    WRITE / |{ round( val = p dec = 2 ) }|.
    WRITE / |{ round( val = '1.005' dec = 2 ) }|.
    WRITE / |{ round( val = '0.125' dec = 2 mode = half_even ) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`2.68\n-2.68\n1.01\n0.12`);
  });

  it("dec, a binary float keeps its binary expansion", async () => {
    // f 2.675 is 2.67499999999999982236431605997495353221893310546875
    const code = `
    DATA f TYPE f VALUE '2.675'.
    WRITE / |{ round( val = f dec = 2 ) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`2.67`);
  });

  it("dec, negative rounds before the decimal point", async () => {
    const code = `
    CONSTANTS half_down TYPE i VALUE 4.
    DATA i TYPE i.
    i = round( val = 1234 dec = -2 ).
    WRITE / |{ i }|.
    i = round( val = 1250 dec = -2 ).
    WRITE / |{ i }|.
    i = round( val = -1250 dec = -2 mode = half_down ).
    WRITE / |{ i }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`1200\n1300\n-1200`);
  });

  it("dec, all rounding modes", async () => {
    const code = `
    DATA p TYPE p LENGTH 10 DECIMALS 2.
    DATA mode TYPE i.
    DATA line TYPE string.
    DO 7 TIMES.
      mode = sy-index - 1.
      p = '2.25'.
      line = |{ mode }: { round( val = p dec = 1 mode = mode ) }|.
      p = '-2.25'.
      line = |{ line } { round( val = p dec = 1 mode = mode ) }|.
      WRITE / line.
    ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    // ceiling, up, half up, half even, half down, down, floor
    expect(abap.console.get()).to.equal([
      "0: 2.3 -2.2",
      "1: 2.3 -2.3",
      "2: 2.3 -2.3",
      "3: 2.2 -2.2",
      "4: 2.2 -2.2",
      "5: 2.2 -2.2",
      "6: 2.2 -2.3"].join("\n"));
  });

  it("prec, significant digits", async () => {
    const code = `
    WRITE / |{ round( val = '1234.5678' prec = 6 ) }|.
    WRITE / |{ round( val = '0.00012345' prec = 3 ) }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`1234.57\n0.000123`);
  });

});
