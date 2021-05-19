import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - count", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic count()", async () => {
    const code = `
      DATA lv_count TYPE i.
      lv_count = count( val = 'password' sub = 's' ).
      WRITE lv_count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});
