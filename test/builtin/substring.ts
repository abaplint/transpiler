// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - substring", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("substring 01", async () => {
    const code = `
    DATA path TYPE string VALUE '/'.
    DATA result TYPE string.
    result = substring( val = path off = strlen( path ) - 1 ).
    ASSERT result = '/'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic substring", async () => {
    const code = `ASSERT substring( val = |abc| off = 1 len = 1 ) = |b|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("substring, only val and len supplied", async () => {
    const code = `ASSERT substring( val = |abc| len = 1 ) = |a|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
