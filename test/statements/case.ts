// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CASE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("CASE, only OTHERS", async () => {
    const code = `DATA lv_code TYPE i.
    CASE lv_code.
      WHEN OTHERS.
    ENDCASE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});