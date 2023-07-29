import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - NE", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("char vs string", async () => {
    const code = `
ASSERT 'FOO' <> |FOO |.
ASSERT |FOO | <> 'FOO'.`;
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
    IF lv_date1 <> lv_date2.
      WRITE 'error'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("sub", async () => {
    const code = `
TYPES: BEGIN OF ty,
         BEGIN OF sub,
           foo TYPE c LENGTH 1,
         END OF sub,
       END OF ty.
DATA data1 TYPE ty.
DATA data2 TYPE ty.

data1-sub-foo = 'A'.
data2-sub-foo = 'B'.
ASSERT data1 <> data2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("sub2", async () => {
    const code = `
TYPES: BEGIN OF ty,
         bar TYPE c LENGTH 1,
         BEGIN OF sub,
           foo TYPE c LENGTH 1,
         END OF sub,
       END OF ty.
DATA data1 TYPE ty.
DATA data2 TYPE ty.

data1-sub-foo = 'A'.
data2-sub-foo = 'B'.
ASSERT data1 <> data2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});