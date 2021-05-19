// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - shift_left", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic shift_left()", async () => {
    const code = "ASSERT shift_left( val = 'aabbcc' sub = `a` ) = 'bbcc'.";
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
