// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - sqrt", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("basic sqrt()", async () => {
    const code = "ASSERT sqrt( 4 ) = 2.";
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
