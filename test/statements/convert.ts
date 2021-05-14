import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONVERT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it.only("test 01", async () => {
    const code = `
    DATA lv_date TYPE d.
    DATA lv_time TYPE t.
    DATA lv_timestamp TYPE p LENGTH 8.
    DATA lv_utc TYPE string VALUE 'UTC'.
    lv_date = '19500505'.
    lv_time = '185024'.
    CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE lv_utc.
    ASSERT lv_timestamp = '19500505185024'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});