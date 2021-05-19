import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - FIND", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("FIND FIRST OCCURRENCE, found", async () => {
    const code = `
      DATA lv_offset.
      FIND FIRST OCCURRENCE OF |bar| IN |foobar| MATCH OFFSET lv_offset.
      WRITE / sy-subrc.
      WRITE / lv_offset.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n3");
  });

  it("FIND FIRST OCCURRENCE, not found", async () => {
    const code = `
      DATA lv_offset.
      FIND FIRST OCCURRENCE OF |bar| IN |foo| MATCH OFFSET lv_offset.
      WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("FIND FIRST OCCURRENCE", async () => {
    const code = `
      FIND FIRST OCCURRENCE OF 'bar' IN 'foobar'.
      ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND REGEX, count and length", async () => {
    const code = `
      DATA lv_cnt TYPE i.
      DATA lv_len TYPE i.
      FIND FIRST OCCURRENCE OF REGEX 'b+c' IN 'abcdbc' MATCH COUNT lv_cnt MATCH LENGTH lv_len.
      WRITE / lv_cnt.
      WRITE / lv_len.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("FIND REGEX, not found", async () => {
    const code = `
      DATA lv_host TYPE string.
      FIND REGEX 'a' IN '1122' SUBMATCHES lv_host.
      WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("FIND REGEX SUBMATCHES, found", async () => {
    const code = `
      DATA lv_host TYPE string.
      FIND REGEX '11(\\w+)22' IN '11abc22' SUBMATCHES lv_host.
      ASSERT sy-subrc = 0.
      ASSERT lv_host = 'abc'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND REGEX slashes", async () => {
    const code = `
      FIND REGEX '//' IN '11//22'.
      ASSERT sy-subrc = 0.
      FIND REGEX '/' IN '11/22'.
      ASSERT sy-subrc = 0.
      FIND REGEX '/' IN '1122'.
      ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND ALL, MATCH COUNT", async () => {
    const code = `
      DATA lv_count TYPE i.
      FIND ALL OCCURRENCES OF 'a' IN 'aaa' MATCH COUNT lv_count.
      ASSERT lv_count = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND ALL, more submatches", async () => {
    const code = `
      DATA lv_val1 TYPE string.
      DATA lv_val2 TYPE string.
      DATA lv_val3 TYPE string.
      DATA lv_val4 TYPE string.
      FIND REGEX '(\\d+)-(\\d+) (\\w): (\\w+)' IN '5-9 g: ggccggmgn' SUBMATCHES lv_val1 lv_val2 lv_val3 lv_val4.
      WRITE / lv_val1.
      WRITE / lv_val2.
      WRITE / lv_val3.
      WRITE / lv_val4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5\n9\ng\nggccggmgn");
  });

  it("FIND ALL, should clear", async () => {
    const code = `
      DATA lv_count TYPE i.
      lv_count = 1.
      FIND ALL OCCURRENCES OF 'a' IN '123' MATCH COUNT lv_count.
      WRITE lv_count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("FIND FIRST occurrence, empty input with MATCH LENGTH", async () => {
    const code = `
      DATA iv_fullpath TYPE string.
      DATA lv_cnt TYPE i.
      DATA lv_len TYPE i.
      FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath MATCH COUNT lv_cnt MATCH LENGTH lv_len.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND RESULTS, 1", async () => {
    const code = `
      TYPES: BEGIN OF ty_submatch,
              offset TYPE i,
              length TYPE i,
            END OF ty_submatch.

      TYPES: BEGIN OF ty_match,
              line       TYPE i,
              offset     TYPE i,
              length     TYPE i,
              submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
            END OF ty_match.

      DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
      DATA ls_match LIKE LINE OF lt_matches.

      FIND REGEX |bar| IN |hello bar world| RESULTS lt_matches.
      ASSERT lines( lt_matches ) = 1.
      READ TABLE lt_matches INDEX 1 INTO ls_match.
      ASSERT ls_match-line = 0.
      ASSERT ls_match-offset = 6.
      ASSERT ls_match-length = 3.
      ASSERT lines( ls_match-submatches ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND RESULTS, 2", async () => {
    const code = `
      TYPES: BEGIN OF ty_submatch,
          offset TYPE i,
          length TYPE i,
        END OF ty_submatch.

      TYPES: BEGIN OF ty_match,
          line       TYPE i,
          offset     TYPE i,
          length     TYPE i,
          submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
        END OF ty_match.

      DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
      DATA ls_match LIKE LINE OF lt_matches.
      DATA ls_submatch LIKE LINE OF ls_match-submatches.

      FIND REGEX |(bar)| IN |hello bar bar world| RESULTS lt_matches.
      ASSERT lines( lt_matches ) = 1.

      FIND ALL OCCURRENCES OF REGEX |(bar)| IN |hello bar bar world| RESULTS lt_matches.
      ASSERT lines( lt_matches ) = 2.
      READ TABLE lt_matches INDEX 1 INTO ls_match.
      ASSERT lines( ls_match-submatches ) = 1.
      READ TABLE ls_match-submatches INDEX 1 INTO ls_submatch.
      ASSERT ls_submatch-offset = 6.
      ASSERT ls_submatch-length = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND RESULTS, 3", async () => {
    const code = `
      TYPES: BEGIN OF ty_submatch,
          offset TYPE i,
          length TYPE i,
        END OF ty_submatch.

      TYPES: BEGIN OF ty_match,
          line       TYPE i,
          offset     TYPE i,
          length     TYPE i,
          submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
        END OF ty_match.

      DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
      DATA find TYPE string.
      find = 'aa'.
      DATA in TYPE string.
      in = 'fooaabar'.
      FIND ALL OCCURRENCES OF REGEX find IN in RESULTS lt_matches.
      ASSERT lines( lt_matches ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND RESULTS, 4", async () => {
    const code = `
      TYPES: BEGIN OF ty_submatch,
              offset TYPE i,
              length TYPE i,
            END OF ty_submatch.

      TYPES: BEGIN OF ty_match,
              line       TYPE i,
              offset     TYPE i,
              length     TYPE i,
              submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
            END OF ty_match.

      DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.

      FIND ALL OCCURRENCES OF REGEX '\\b[-_a-z0-9]+\\b' IN 'REPORT zfoo.' RESULTS lt_matches IGNORING CASE.
      ASSERT lines( lt_matches ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND RESULTS, 5", async () => {
    const code = `
      TYPES: BEGIN OF ty_submatch,
              offset TYPE i,
              length TYPE i,
            END OF ty_submatch.
      TYPES: BEGIN OF ty_match,
              line       TYPE i,
              offset     TYPE i,
              length     TYPE i,
              submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
            END OF ty_match.
      DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
      DATA ls_match LIKE LINE OF lt_matches.
      DATA ls_submatch LIKE LINE OF ls_match-submatches.
      FIND ALL OCCURRENCES OF
        REGEX '(?:"[^"]*")|(?:''[^'']*'')|([<>])'
        IN '<tag attribute="value"/>'
        RESULTS lt_matches IGNORING CASE.
      LOOP AT lt_matches INTO ls_match.
        LOOP AT ls_match-submatches INTO ls_submatch.
          WRITE / ls_submatch-offset.
          WRITE / ls_submatch-length.
        ENDLOOP.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0\n1\n-1\n0\n23\n1");
  });

  it("FIND, bad types", async () => {
    const code = `
      DATA lv_offset TYPE string.
      FIND REGEX 'aa' IN 'bbaabb' MATCH OFFSET lv_offset.
      ASSERT lv_offset = '2'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND, SECTION + BYTE MODE + MATCH OFFSET", async () => {
    const code = `
  DATA lv_cursor TYPE i.
  DATA lv_match TYPE i.
  CONSTANTS lc_null TYPE x VALUE '00'.
  DATA iv_data TYPE xstring.
  iv_data = '1122003344'.
  FIND FIRST OCCURRENCE OF lc_null IN SECTION OFFSET lv_cursor OF iv_data IN BYTE MODE MATCH OFFSET lv_match.
  WRITE lv_match.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});