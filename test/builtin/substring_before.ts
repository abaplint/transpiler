// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - substring_before", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("substring_before 01", async () => {
    const code = `
    DATA result TYPE string.
    result = substring_before( val = 'abc=CP' regex = '=*CP$' ).
    ASSERT result = 'abc'.
    result = substring_before( val = 'abc' regex = '=*CP$' ).
    ASSERT result = ''.
    result = substring_before( val = 'sdf===CP' regex = '(=+CP)?$' ).
    ASSERT result = 'sdf'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
