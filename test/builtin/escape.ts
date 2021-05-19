import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions - escape", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("escape()", async () => {
    // todo, split this unit test into 3 separate unit tests
    const code = `
      CONSTANTS e_html_text TYPE i VALUE 4.
      CONSTANTS e_html_attr TYPE i VALUE 5.
      CONSTANTS e_url TYPE i VALUE 12.
      DATA lv_result TYPE string.
      lv_result = escape( val = |abc123&<>"'| format = e_html_attr ).
      WRITE / lv_result.
      lv_result = escape( val = |abc123&<>"'| format = e_html_text ).
      WRITE / lv_result.
      lv_result = escape( val = |abc123&<>"'| format = e_url ).
      WRITE / lv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(
      `abc123&amp;&lt;&gt;&quot;&#39;\n` +
      `abc123&amp;&lt;&gt;"'\n` +
      `abc123&%3C%3E%22'`);
  });

});
