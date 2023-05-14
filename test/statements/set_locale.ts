// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SET LOCALE", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("SET LOCALE", async () => {
    const code = `
DATA gv_login_language TYPE c LENGTH 1.
gv_login_language = 'E'.
SET LOCALE LANGUAGE gv_login_language.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});