import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {expect} from "chai";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONVERT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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
    WRITE lv_timestamp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("19500505185024");
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

  it("Convert UTC to CET", async () => {
    const code = `
DATA lv_timestamp TYPE p LENGTH 8.
CONVERT DATE '20220905' TIME '175055' INTO TIME STAMP lv_timestamp TIME ZONE 'CET'.
WRITE / |{ lv_timestamp TIMESTAMP = ISO }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2022-09-05T15:50:55");
  });

  it("More convert", async () => {
    const code = `
DATA iv_ts TYPE p LENGTH 8.

CONSTANTS lc_utc TYPE c LENGTH 6 VALUE 'UTC'.

DATA lv_date TYPE d.
DATA lv_time TYPE t.

iv_ts = '20220401200103'.

CONVERT TIME STAMP iv_ts TIME ZONE lc_utc
  INTO DATE lv_date TIME lv_time.
WRITE lv_time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("200103");
  });

  it("Empty time zone", async () => {
    const code = `
DATA lv_timestamp TYPE p LENGTH 8.
CONVERT DATE '20220831' TIME '000000'
  INTO TIME STAMP lv_timestamp TIME ZONE ''.
WRITE lv_timestamp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20220831000000");
  });

  it("CONVERT TIME STAMP", async () => {
    const code = `
DATA rv_tracktstmp TYPE p LENGTH 8.
DATA lv_time_zone TYPE c LENGTH 3.
DATA lv_date TYPE d.
DATA lv_time TYPE t.
rv_tracktstmp = '20211212133030'.
lv_time_zone = 'CET'.
CONVERT TIME STAMP rv_tracktstmp TIME ZONE lv_time_zone INTO DATE lv_date TIME lv_time.
WRITE / |{ lv_date }|.
WRITE / lv_time.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("20211212\n143030");
  });

  it("date = space", async () => {
    const code = `
DATA lv_date TYPE d.
DATA lv_timestamp TYPE p LENGTH 8.
lv_date = space.
CONVERT DATE lv_date TIME '000000' INTO TIME STAMP lv_timestamp TIME ZONE 'UTC'.
WRITE lv_timestamp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("more empty fields", async () => {
    const code = `
DATA lv_date TYPE d.
DATA lv_time TYPE t.
DATA lv_timestamp TYPE p LENGTH 8.
lv_date = ''.
lv_time = ''.
CONVERT DATE lv_date TIME lv_time INTO TIME STAMP lv_timestamp TIME ZONE 'CET'.
WRITE / lv_timestamp.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});