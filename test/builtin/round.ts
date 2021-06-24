import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - round", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("test", async () => {
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
    WRITE / lv_num.
      `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal( `2\n2\n3`);
  });

});
