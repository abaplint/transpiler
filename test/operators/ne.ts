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

});