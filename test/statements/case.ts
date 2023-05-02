// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

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

  it("CASE, only OTHERS inside loop", async () => {
    const code = `DATA lv_code TYPE i.
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.
    LOOP AT tab INTO row.
      CASE lv_code.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});