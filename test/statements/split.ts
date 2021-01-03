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

});