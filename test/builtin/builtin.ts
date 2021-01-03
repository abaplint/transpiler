import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Builtin functions", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("boolc test", async () => {
    const code = `
      DATA rv_yes TYPE abap_bool.
      DATA iv_path TYPE string.
      iv_path = '/'.
      rv_yes = boolc( iv_path = '/' ).
      WRITE rv_yes.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("X");
  });

  it("concat_lines_of", async () => {
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

  it("condense", async () => {
    const code = `
      DATA foo TYPE string.
      foo = '12  3 '.
      foo = condense( foo ).
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12 3");
  });

  it("condense integer", async () => {
    const code = `
      DATA foo TYPE string.
      data bar type i.
      bar = 2.
      foo = bar.
      foo = condense( foo ).
      WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("basic count()", async () => {
    const code = `
      DATA lv_count TYPE i.
      lv_count = count( val = 'password' sub = 's' ).
      WRITE lv_count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("escape()", async () => {
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

  it("basic repeat()", async () => {
    const code = `ASSERT repeat( val = 'a' occ = 2 ) = 'aa'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("reverse", async () => {
    const code = `
      DATA str TYPE string.
      str = reverse( 'abc' ).
      WRITE str.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("cba");
  });

  it("basic shift_left()", async () => {
    const code = "ASSERT shift_left( val = 'aabbcc' sub = `a` ) = 'bbcc'.";
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("nested calls to builtins", async () => {
    const code = `
      DATA lv_line TYPE string.
      lv_line = to_upper( shift_left( val = 'aabb' sub = 'a' ) ).
      ASSERT lv_line = 'BB'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic strlen", async () => {
    const code = `
      DATA foo TYPE string.
      foo = '123'.
      WRITE strlen( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("basic substring", async () => {
    const code = `ASSERT substring( val = |abc| off = 1 len = 1 ) = |b|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("to_lower()", async () => {
    const code = `ASSERT to_lower( 'ABC' ) = 'abc'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("to_upper()", async () => {
    const code = `
      DATA bar TYPE string VALUE 'BAR'.
      ASSERT to_upper( |bar| ) = |BAR|.
      ASSERT to_upper( |bar| ) = bar.
      ASSERT to_upper( bar ) = bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic xstrlen", async () => {
    const code = `
      DATA foo TYPE xstring.
      foo = 'AA'.
      WRITE xstrlen( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

});
