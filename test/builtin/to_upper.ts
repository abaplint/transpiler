// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - to_upper", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("to_upper()", async () => {
    const code = `
      DATA bar TYPE string VALUE 'BAR'.
      ASSERT to_upper( |bar| ) = |BAR|.
      ASSERT to_upper( |bar| ) = bar.
      ASSERT to_upper( bar ) = bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
