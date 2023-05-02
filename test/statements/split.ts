import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SPLIT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Character field semantics", async () => {
    const code = `
      DATA lv_str TYPE string.
      DATA lt_table TYPE STANDARD TABLE OF string.
      lv_str = 'foo bar'.
      SPLIT lv_str AT | | INTO TABLE lt_table.
      ASSERT lines( lt_table ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("split 1", async () => {
    const code = `
      DATA: lv_major   TYPE c LENGTH 4,
            lv_minor   TYPE c LENGTH 4,
            lv_release TYPE c LENGTH 4.
      SPLIT |blah| AT '.' INTO lv_major lv_minor lv_release.
      WRITE / lv_major.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("blah");
  });

  it("split 2", async () => {
    const code = `
      DATA: lv_major   TYPE c LENGTH 4,
      lv_minor   TYPE c LENGTH 4,
      lv_release TYPE c LENGTH 4.
      SPLIT |blah.boo| AT '.' INTO lv_major lv_minor lv_release.
      WRITE / lv_major.
      WRITE / lv_minor.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("blah\nboo ");
  });

  it("split 3", async () => {
    const code = `
      DATA: lv_major   TYPE c LENGTH 4,
      lv_minor   TYPE c LENGTH 4,
      lv_release TYPE c LENGTH 4.
      SPLIT |1.2.3| AT '.' INTO lv_major lv_minor lv_release.
      WRITE / lv_major.
      WRITE / lv_minor.
      WRITE / lv_release.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1   \n2   \n3   ");
  });

  it("SPLIT empty string, should give empty table", async () => {
    const code = `
      DATA strs TYPE STANDARD TABLE OF string.
      SPLIT || AT |a| INTO TABLE strs.
      ASSERT lines( strs ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SPLIT non-empty string, should give non-empty table", async () => {
    const code = `
      DATA lt_tab TYPE STANDARD TABLE OF string.
      SPLIT |sdfds| AT |AA| INTO TABLE lt_tab.
      ASSERT lines( lt_tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("split at newline", async () => {
    const code = `
      DATA moo TYPE string.
      DATA foo TYPE string.
      DATA bar TYPE string.
      moo = |foo\\nbar|.
      SPLIT moo AT |\\n| INTO foo bar.
      WRITE / foo.
      WRITE / bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo\nbar");
  });

  it("split at A", async () => {
    const code = `
DATA lv_str TYPE string.
DATA lt_str TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
SPLIT |AbarAA| AT |A| INTO TABLE lt_str.
ASSERT lines( lt_str ) = 3.
READ TABLE lt_str INDEX 1 INTO lv_str.
ASSERT lv_str = ''.
READ TABLE lt_str INDEX 2 INTO lv_str.
ASSERT lv_str = 'bar'.
READ TABLE lt_str INDEX 3 INTO lv_str.
ASSERT lv_str = ''.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("another split", async () => {
    const code = `
  DATA lv_str TYPE string.
  DATA lt_str TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  SPLIT |fooAAbar| AT |A| INTO TABLE lt_str.
  ASSERT lines( lt_str ) = 3.
  READ TABLE lt_str INDEX 1 INTO lv_str.
  ASSERT lv_str = 'foo'.
  READ TABLE lt_str INDEX 2 INTO lv_str.
  ASSERT lv_str = ''.
  READ TABLE lt_str INDEX 3 INTO lv_str.
  ASSERT lv_str = 'bar'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("SPLIT, too short", async () => {
    const code = `
DATA lv_children TYPE string.
DATA lv_order TYPE string.
DATA lv_last TYPE string.
DATA lv_int TYPE i.

SPLIT '2|3' AT '|' INTO
  lv_children
  lv_order
  lv_last.

lv_int = lv_children.
WRITE / lv_int.
lv_int = lv_last.
WRITE / lv_int.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n0");
  });

  it("split 3", async () => {
    const code = `
    DATA lv_first TYPE string.
    DATA lv_rest TYPE string.
    SPLIT |hello world moo| AT space INTO lv_first lv_rest.
    WRITE / lv_first.
    WRITE / lv_rest.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\nworld moo");
  });

  it("split 4", async () => {
    const code = `
  DATA lv_str TYPE string.
  DATA lv_order TYPE string.
  SPLIT 'hello|' AT '|' INTO lv_str lv_order.
  WRITE / lv_str.
  WRITE / lv_order.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello\n");
  });

  it("split 5", async () => {
    const code = `
    DATA lv_str1 TYPE string.
    DATA lv_str2 TYPE string.
    DATA lv_str3 TYPE string.
    DATA lv_str4 TYPE string.
    DATA lv_str5 TYPE string.
    DATA lv_str6 TYPE string.
    DATA lv_str7 TYPE string.

    SPLIT '/a/b/c/ |d     |object |     ||0' AT '|' INTO
    lv_str1
    lv_str2
    lv_str3
    lv_str4
    lv_str5
    lv_str6
    lv_str7.

    WRITE: / '1', lv_str1.
    WRITE: / '2', lv_str2.
    WRITE: / '3', lv_str3.
    WRITE: / '4', lv_str4.
    WRITE: / '5', lv_str5.
    WRITE: / '6', lv_str6.
    WRITE: / '7', lv_str7.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`1/a/b/c/\n2d\n3object\n4\n5\n60\n7`);
  });

  it("split 6", async () => {
    const code = `
    DATA lv_str1 TYPE string.
    DATA lv_str2 TYPE string.
    SPLIT ' a|d' AT '|' INTO
    lv_str1
    lv_str2.
    WRITE: / '1', lv_str1.
    WRITE: / '2', lv_str2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`1 a\n2d`);
  });

  it("split 7", async () => {
    const code = `
    DATA lv_path TYPE string.
    DATA lv_new TYPE string.
    lv_path = 'foo/bar/'.
    SPLIT lv_path AT '/' INTO lv_new lv_path.
    ASSERT lv_path = 'bar/'.
    ASSERT lv_new = 'foo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("split 8", async () => {
    const code = `
    DATA lv_path TYPE string.
    DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    lv_path = 'foo/bar/'.
    SPLIT lv_path AT '/' INTO TABLE tab.
    WRITE lines( tab ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`2`);
  });

  it("split 9", async () => {
    const code = `
DATA val1 TYPE string.
DATA val2 TYPE string.
DATA rv_escaped TYPE string.
rv_escaped = | \\|a"\\\\\\t|.
SPLIT rv_escaped AT '|' INTO val1 val2.
ASSERT val2 = |a"\\\\\\t|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("split 10", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA str TYPE string.
DATA val TYPE c LENGTH 20.
val = 'foo#bar'.
SPLIT val AT '#' INTO TABLE tab.
LOOP AT tab INTO str.
  WRITE / strlen( str ).
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal(`3\n3`);
  });

  it("split 11", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
DATA str TYPE string.
DATA val TYPE c LENGTH 20.
val = 'foo#bar'.
SPLIT val AT '# ' INTO TABLE tab.
LOOP AT tab INTO str.
  WRITE / strlen( str ).
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal(`7`);
  });

});