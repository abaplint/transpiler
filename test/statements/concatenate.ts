import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CONCATENATE", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Basic CONCATENATE", async () => {
    const code = `
      DATA target TYPE string.
      CONCATENATE 'foo' 'bar' INTO target.
      ASSERT target = 'foobar'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CONCATENATE SEPARATED BY space", async () => {
    const code = `
      DATA lv_string TYPE string.
      DATA lv_char10 TYPE c LENGTH 10.
      DATA iv_type TYPE c LENGTH 6 VALUE 'commit'.
      lv_char10 = 6.
      CONDENSE lv_char10.
      CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
      ASSERT lv_string = 'commit 6'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CONCATENATE LINES OF", async () => {
    const code = `
      DATA rv_html TYPE string.
      DATA lt_temp TYPE STANDARD TABLE OF string.
      APPEND 'fo' TO lt_temp.
      APPEND 'bar' TO lt_temp.
      CONCATENATE LINES OF lt_temp INTO rv_html SEPARATED BY 'o'.
      ASSERT rv_html = 'foobar'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});