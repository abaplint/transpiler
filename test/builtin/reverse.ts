import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - reverse", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("reverse", async () => {
    const code = `
      DATA str TYPE string.
      str = reverse( 'abc' ).
      WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("cba");
  });

});
