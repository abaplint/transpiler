import {ABAP} from "../../packages/runtime/src";
import {expect} from "chai";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONVERT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("test 01", async () => {
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

  it("test 02", async () => {
    const code = `
DATA iv_val TYPE p LENGTH 8.
DATA lv_utc TYPE string VALUE 'UTC'.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
iv_val = '20210505120000'.
CONVERT TIME STAMP iv_val TIME ZONE lv_utc
  INTO DATE lv_date TIME lv_time.
ASSERT lv_date = '20210505'.
WRITE / lv_time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("120000");
  });

  it("initial date and time", async () => {
    const code = `
DATA lv_timestamp TYPE p LENGTH 8.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE 'UTC'.
ASSERT lv_timestamp IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("initial ts to date and time", async () => {
    const code = `
CONSTANTS lc_utc TYPE c LENGTH 6 VALUE 'UTC'.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA iv_ts TYPE p LENGTH 8.
CONVERT TIME STAMP iv_ts TIME ZONE lc_utc INTO DATE lv_date TIME lv_time.
ASSERT lv_date IS INITIAL.
ASSERT lv_time IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});