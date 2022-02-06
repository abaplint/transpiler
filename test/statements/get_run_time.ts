// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET RUN TIME", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("simple", async () => {
    const code = `
    DATA lv_start TYPE i.
    DATA lv_end TYPE i.
    DATA calc TYPE i.
    GET RUN TIME FIELD lv_start.
    ASSERT lv_start = 0.
    DO 1000 TIMES.
      calc = 2 * 2 * 2.
      WRITE calc.
    ENDDO.
    GET RUN TIME FIELD lv_end.
    ASSERT lv_end <> 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});