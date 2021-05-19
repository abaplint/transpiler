import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples - Hex type", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Hex, initial value", async () => {
    const code = `
  DATA lv_hex TYPE x LENGTH 1.
  WRITE lv_hex.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("00");
  });

  it("Hex Calculation, MOD has precedence", async () => {
    const code = `
    DATA lv_hex    TYPE x LENGTH 1.
    DATA lv_type   TYPE i.
    DATA lv_length TYPE i.
    lv_type = 2.
    lv_length = 123.
    lv_hex = lv_hex + lv_type + lv_length MOD 16.
    WRITE lv_hex.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0D"); // 13 in decimal
  });

});