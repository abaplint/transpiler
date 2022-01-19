import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CALL", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it.skip("Dynamic, class not found", async () => {
    const code = `
TRY.
    CALL METHOD ('ZSDFSD')=>foo.
    WRITE 'fail'.
  CATCH cx_sy_dyn_call_illegal_class.
    WRITE 'expected'.
ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("expected");
  });

});