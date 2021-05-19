// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

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

});
