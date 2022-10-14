import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - frac", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Builtin numerical: frac", async () => {
    const code = `
  DATA chars TYPE c LENGTH 10.
  DATA int TYPE i.
  DATA packed TYPE p.

  chars = '12.34'.
  WRITE / frac( chars ).

  packed = chars.
  WRITE / frac( packed ).

  int = 12.
  WRITE / frac( int ).

  WRITE / frac( '-43.21' ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0.34\n0\n0\n-0.21");
  });

  it("frac, decfloat34", async () => {
    const code = `
    DATA lv_result TYPE decfloat34.
    lv_result = 7 / 2.
    WRITE frac( lv_result ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0,5");
  });

  it("frac, float", async () => {
    const code = `
    DATA lv_result TYPE f.
    lv_result = 7 / 2.
    WRITE frac( lv_result ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5,0000000000000000E-01");
  });

});
