import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - replace", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("replace 01", async () => {
    const code = `
    DATA result TYPE string.
    result = replace( val = 'hello' sub = 'l' with = 'o' ).
    WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("heolo");
  });

  it("replace 02", async () => {
    const code = `
    DATA result TYPE string.
    result = replace( val = 'hello' sub = 'l' with = 'o' occ = 0).
    WRITE result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("heooo");
  });

});
