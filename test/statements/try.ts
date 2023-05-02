import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - TRY", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("TRY without CATCH", async () => {
    const code = `
      TRY.
        WRITE 'hello'.
      ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});