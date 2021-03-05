// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - substring_after", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("substring_after 01", async () => {
    const code = `
    DATA result TYPE string.
    result = substring_after( val = 'foo=bar' sub = '=' ).
    ASSERT result = 'bar'.
    result = substring_after( val = 'abc' sub = '=' ).
    ASSERT result = ''.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
