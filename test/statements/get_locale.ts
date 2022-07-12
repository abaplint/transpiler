// import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - GET LOCALE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("GET LOCALE", async () => {
    const code = `
DATA gv_login_language TYPE c LENGTH 1.
DATA lv_dummy TYPE string.
GET LOCALE LANGUAGE gv_login_language COUNTRY lv_dummy MODIFIER lv_dummy.
ASSERT gv_login_language = 'E'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});