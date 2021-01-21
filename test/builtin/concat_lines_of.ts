import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - concat_lines_of", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("concat_lines_of 1", async () => {
    const code = `
      DATA rv_text TYPE string.
      DATA lt_rows TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      APPEND 'a' TO lt_rows.
      APPEND 'c' TO lt_rows.
      rv_text = concat_lines_of( table = lt_rows
                                 sep   = |b| ).
      WRITE rv_text.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("abc");
  });

  it("concat_lines_of, without sep", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    APPEND 'foo' TO tab.
    APPEND 'bar' TO tab.
    WRITE concat_lines_of( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foobar");
  });

});
