import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}


describe("Running statements - FIND", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("FIND FIRST OCCURRENCE, found 2", async () => {
    const code = `
      DATA lv_offset.
      FIND FIRST OCCURRENCE OF |bar| IN |foobarr| MATCH OFFSET lv_offset.
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

  it("FIND, more BYTE MODE", async () => {
    const code = `
    DATA lv_cursor TYPE i.
    DATA lv_match TYPE i.
    CONSTANTS lc_null TYPE x VALUE '00'.
    DATA iv_data TYPE xstring.
    iv_data = '1122003344'.
    DO 5 TIMES.
      FIND FIRST OCCURRENCE OF lc_null IN SECTION OFFSET lv_cursor OF iv_data IN BYTE MODE MATCH OFFSET lv_match.
      WRITE: / sy-subrc, lv_match.
      lv_cursor = lv_cursor + 1.
    ENDDO.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("02\n02\n02\n42\n42");
  });

  it("FIND, with REGEX :space:", async () => {
    const code = `
    DATA lv_string TYPE string.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.
    lv_string = 'foo MOO  BAR'.
    FIND REGEX |MOO[[:space:]]+| IN lv_string MATCH OFFSET lv_offset MATCH LENGTH lv_length.
    WRITE / lv_offset.
    WRITE / lv_length.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n5");
  });

  it("FIND, with REGEX :space:, check for newline", async () => {
    const code = `
  DATA lv_string TYPE string.
  DATA lv_offset TYPE i.
  DATA lv_length TYPE i.
  lv_string = |foo MOO\\nBAR|.
  FIND REGEX |MOO[[:space:]]+| IN lv_string MATCH OFFSET lv_offset MATCH LENGTH lv_length.
  WRITE / lv_offset.
  WRITE / lv_length.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n4");
  });

  it("FIND, with REGEX", async () => {
    const code = `
    DATA str TYPE string.
    DATA lv_name TYPE string.
    DATA lv_email TYPE string.
    str = |Foo Bar <foo@bar.com> 1526718052 +0000|.
    FIND FIRST OCCURRENCE OF REGEX \`(.*)<(.*)>\` IN str SUBMATCHES lv_name lv_email.
    WRITE / lv_name.
    WRITE / lv_email.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("Foo Bar \nfoo@bar.com");
  });

  it("FIND FIRST with REGEX + RESULTS", async () => {
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

DATA ls_match TYPE ty_match.
DATA ls_submatch LIKE LINE OF ls_match-submatches.
DATA iv_data TYPE string.

iv_data = 'done symref=HEAD:refs/heads/main filter object'.
FIND FIRST OCCURRENCE OF REGEX '\\ssymref=HEAD:([^\\s]+)' IN iv_data RESULTS ls_match.
READ TABLE ls_match-submatches INTO ls_submatch INDEX 1.
WRITE / ls_match-offset.
WRITE / ls_submatch-offset.
WRITE / ls_submatch-length.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n17\n15");
  });

  it("FIND FIRST, not a regex!", async () => {
    const code = `
    DATA lv_offset TYPE i.
    FIND FIRST OCCURRENCE OF '?>' IN '?>' MATCH OFFSET lv_offset.
    ASSERT lv_offset = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND FIRST REGEX RESULTS", async () => {
    const code = `
TYPES: BEGIN OF ty_submatch,
         offset TYPE i,
         length TYPE i,
       END OF ty_submatch.

DATA: BEGIN OF ls_match,
        line       TYPE i,
        offset     TYPE i,
        length     TYPE i,
        submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
      END OF ls_match.

FIND FIRST OCCURRENCE OF REGEX 'b(ar)' IN 'fobar' RESULTS ls_match.
ASSERT ls_match-offset = 2.
ASSERT ls_match-length = 3.
ASSERT lines( ls_match-submatches ) = 1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND FIRST REGEX RESULTS, 2", async () => {
    const code = `
TYPES: BEGIN OF ty_submatch,
         offset TYPE i,
         length TYPE i,
       END OF ty_submatch.

DATA: BEGIN OF ls_match,
        line       TYPE i,
        offset     TYPE i,
        length     TYPE i,
        submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
      END OF ls_match.

FIND FIRST OCCURRENCE OF
  REGEX '<\\/?(\\w+)( \\w+="[\\w\\.]+")*>'
  IN '<abapGit version="v1.0.0">'
  RESULTS ls_match.
ASSERT ls_match-offset = 0.
ASSERT ls_match-length = 26.
ASSERT lines( ls_match-submatches ) = 2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND more REGEX", async () => {
    const code = `
  DATA text TYPE string.
  DATA ev_author TYPE string.
  DATA ev_email TYPE string.
  DATA ev_time TYPE string.
  CONSTANTS c_author_regex TYPE string VALUE '^(.+) <(.*)> (\\d{10})\\s?.\\d{4}$' ##NO_TEXT.
  text = 'Lastname, Firstname <mail@mail.com> 1532611350 +0000'.
  FIND REGEX c_author_regex IN text
    SUBMATCHES
    ev_author
    ev_email
    ev_time.
  ASSERT sy-subrc = 0.
  ASSERT ev_author = 'Lastname, Firstname'.
  ASSERT ev_email = 'mail@mail.com'.
  ASSERT ev_time = '1532611350'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND more REGEX, constant defined in INTF", async () => {
    const code = `
INTERFACE bar.
  CONSTANTS regex TYPE string VALUE '\\d+'.
ENDINTERFACE.

START-OF-SELECTION.
  FIND REGEX bar=>regex IN '123'.
  ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND more REGEX, submatches should not clear", async () => {
    const code = `
  DATA text TYPE string.
  DATA ev_author TYPE string.
  DATA ev_email TYPE string.
  DATA ev_time TYPE string.
  ev_author = '1'.
  ev_email = '1'.
  ev_time = '1'.
  CONSTANTS c_author_regex TYPE string VALUE '^(.+) <(.*)> (\\d{10})\\s?.\\d{4}$' ##NO_TEXT.
  text = 'no match'.
  FIND REGEX c_author_regex IN text
    SUBMATCHES
    ev_author
    ev_email
    ev_time.
  ASSERT sy-subrc <> 0.
  ASSERT ev_author = '1'.
  ASSERT ev_email = '1'.
  ASSERT ev_time = '1'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND more REGEX, hosts", async () => {
    const code = `
  DATA rv_prefix TYPE string.
  rv_prefix = 'sdf'.
  FIND REGEX '\\w(\\/[\\w\\d\\.\\-\\/]+)' IN 'https://api.foobar.com' SUBMATCHES rv_prefix.
  ASSERT rv_prefix = 'sdf'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND more REGEX, clearing of submatches", async () => {
    const code = `
    DATA foo TYPE string.
    DATA duplicates TYPE string.
    DATA str TYPE string.
    str = 'Z'.
    FIND REGEX '([\\w|\\s])(.*)(.*)' IN str SUBMATCHES foo duplicates str.
    ASSERT foo = 'Z'.
    ASSERT str = ''.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND REGEX in TABLE", async () => {
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
DATA lt_words TYPE TABLE OF string.
APPEND 'hello' TO lt_words.
APPEND 'bar' TO lt_words.
APPEND 'world' TO lt_words.
FIND REGEX |bar| IN TABLE lt_words RESULTS lt_matches.
ASSERT lines( lt_matches ) = 1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("FIND REGEX in TABLE, check LINE", async () => {
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
DATA ls_submatch TYPE ty_submatch.
DATA ls_match TYPE ty_match.
DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
DATA lt_words TYPE TABLE OF string.
APPEND 'hello' TO lt_words.
APPEND '__bar__' TO lt_words.
APPEND 'world' TO lt_words.
FIND REGEX |(bar)| IN TABLE lt_words RESULTS lt_matches.
LOOP AT lt_matches INTO ls_match.
  WRITE ls_match-line.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("FIND, empty, simple", async () => {
    const code = `
    DATA letter TYPE string.
    DATA letters TYPE string.
    letter = ''.
    letters = 'abcde'.
    FIND letter IN letters.
    ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    await f(abap);
  });

  it("find printable, zero", async () => {
    const code = `
DATA lv_count TYPE i.
DATA lv_string TYPE string.
lv_string = |hello|.
FIND ALL OCCURRENCES OF REGEX '[^[:print:]]' IN lv_string MATCH COUNT lv_count.
WRITE lv_count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("find printable, one", async () => {
    const code = `
DATA lv_count TYPE i.
DATA lv_string TYPE string.
lv_string = |\\n|.
FIND ALL OCCURRENCES OF REGEX '[^[:print:]]' IN lv_string MATCH COUNT lv_count.
WRITE lv_count.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("FIND RESULTS, 6", async () => {
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
DATA ls_submatch TYPE ty_submatch.
DATA ls_match LIKE LINE OF lt_matches.

FIND REGEX 'U\\+(....)' IN |U+0000| RESULTS lt_matches.
ASSERT lines( lt_matches ) = 1.
READ TABLE lt_matches INDEX 1 INTO ls_match.
ASSERT ls_match-offset = 0.
ASSERT ls_match-length = 6.

ASSERT lines( ls_match-submatches ) = 1.
READ TABLE ls_match-submatches INDEX 1 INTO ls_submatch.
ASSERT ls_submatch-offset = 2.
ASSERT ls_submatch-length = 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("find pcre", async () => {
    const code = `
    DATA offset TYPE i.
    FIND FIRST OCCURRENCE OF PCRE 'l+' IN 'hello' MATCH OFFSET offset.
    ASSERT offset = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND some REGEX", async () => {
    const code = `
DATA iv_path TYPE string.
DATA foo TYPE string.
iv_path = |s[?(@.name=='METH1')].d|.
FIND FIRST OCCURRENCE OF REGEX '(\\[[^\\]]*\\])' IN iv_path SUBMATCHES foo.
ASSERT foo = |[?(@.name=='METH1')]|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("FIND, REGEX slash C", async () => {
    const code = `
DATA iv_line TYPE string.
iv_line = 'a'.
FIND FIRST OCCURRENCE OF REGEX '\\C' IN iv_line.
WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("FIND, another REGEX", async () => {
    const code = `
DATA iv_line TYPE string.
DATA str1 TYPE string.
DATA str2 TYPE string.
DATA str3 TYPE string.

iv_line = '#FOOO,30: Greetings'.

FIND FIRST OCCURRENCE OF REGEX '^#(\\C{4})(?:,(\\d+))?(?::(.*))'
  IN iv_line SUBMATCHES str1 str2 str3.

WRITE / str1.
WRITE / str2.
WRITE / str3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("FOOO\n30\n Greetings");
  });

  it("FIND, inline DATA", async () => {
    const code = `
FORM foo.
  DATA str TYPE string.
  FIND ALL OCCURRENCES OF REGEX 'foo' IN str RESULTS DATA(result_table).
  WRITE / lines( result_table ).
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});
