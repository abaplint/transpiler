import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - match", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("match 01", async () => {
    const code = `
    DATA result TYPE string.
    result = match( val = 'foo=bar' regex = '=' ).
    WRITE / result.

    result = match( val = 'abc' regex = 'd' ).
    ASSERT result IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("=");
  });

});
