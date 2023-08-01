// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - EQ", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("EQ char vs structure", async () => {
    const code = `
CONSTANTS:
  BEGIN OF c_package_file,
    obj_name  TYPE c LENGTH 7 VALUE 'package',
    sep1      TYPE c LENGTH 1 VALUE '.',
    obj_type  TYPE c LENGTH 4 VALUE 'devc',
    sep2      TYPE c LENGTH 1 VALUE '.',
    extension TYPE c LENGTH 3 VALUE 'xml',
  END OF c_package_file.

DATA str TYPE string.
str = |package.devc.xml|.

ASSERT 'package.devc.xml' = c_package_file.
ASSERT c_package_file = 'package.devc.xml'.
ASSERT str = c_package_file.
ASSERT c_package_file = str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("dates", async () => {
    const code = `
    DATA lv_date1 TYPE d.
    DATA lv_date2 TYPE d.
    lv_date1 = sy-datum.
    lv_date2 = sy-datum.
    ASSERT lv_date1 = lv_date2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("string and char", async () => {
    const code = `
    ASSERT |hello| = 'hello '.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("char and char, different lengths", async () => {
    const code = `
    ASSERT 'a' = 'a '.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("yea, empty char is zero, yea", async () => {
    const code = `
DATA lv_val TYPE c LENGTH 10.
ASSERT lv_val = 0.
ASSERT 0 = lv_val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("yea, one", async () => {
    const code = `
DATA lv_val TYPE c LENGTH 10.
lv_val = 1.
ASSERT lv_val = 1.
ASSERT 1 = lv_val.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});