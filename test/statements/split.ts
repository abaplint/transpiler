import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

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
    expect(abap.console.get()).to.equal("blah\nboo");
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
    expect(abap.console.get()).to.equal("1\n2\n3");
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

});