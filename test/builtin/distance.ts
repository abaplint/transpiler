import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function expectValue(code: string, expected: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  expect(abap.console.get()).to.equal(expected);
}

describe("Builtin functions - distance", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("kitten and sitting", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'kitten' val2 = 'sitting' ).
    WRITE lv.`, "3");
  });

  it("the same in both directions", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'sitting' val2 = 'kitten' ).
    WRITE lv.`, "3");
  });

  it("equal texts", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'abc' val2 = 'abc' ).
    WRITE lv.`, "0");
  });

  it("both empty", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = \`\` val2 = \`\` ).
    WRITE lv.`, "0");
  });

  it("one empty, the length of the other", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = \`\` val2 = 'abc' ).
    WRITE lv.`, "3");
  });

  it("one inserted character", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'abc' val2 = 'abxc' ).
    WRITE lv.`, "1");
  });

  it("one deleted character", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'abcd' val2 = 'acd' ).
    WRITE lv.`, "1");
  });

  it("one replaced character", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'abc' val2 = 'abd' ).
    WRITE lv.`, "1");
  });

  it("swapped neighbours are two operations", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'ab' val2 = 'ba' ).
    WRITE lv.`, "2");
  });

  it("case sensitive", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'ABC' val2 = 'abc' ).
    WRITE lv.`, "3");
  });

  it("trailing blanks of a text field are ignored", async () => {
    await expectValue(`
    DATA lv TYPE i.
    DATA lv_text TYPE c LENGTH 10 VALUE 'abc'.
    lv = distance( val1 = lv_text val2 = \`abc\` ).
    WRITE lv.`, "0");
  });

  it("trailing blanks of a text literal are ignored", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = 'abc ' val2 = \`abc\` ).
    WRITE lv.`, "0");
  });

  it("trailing blanks of a string count", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = \`abc \` val2 = \`abc\` ).
    WRITE lv.`, "1");
  });

  it("leading blanks count", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = ' abc' val2 = 'abc' ).
    WRITE lv.`, "1");
  });

  it("field symbol pointing to a text field", async () => {
    await expectValue(`
    DATA lv TYPE i.
    DATA lv_text TYPE c LENGTH 10 VALUE 'abc'.
    FIELD-SYMBOLS <lv_text> TYPE c.
    ASSIGN lv_text TO <lv_text>.
    lv = distance( val1 = <lv_text> val2 = 'abd' ).
    WRITE lv.`, "1");
  });

  it("non-ASCII characters", async () => {
    await expectValue(`
    DATA lv TYPE i.
    lv = distance( val1 = \`äöü\` val2 = \`aöu\` ).
    WRITE lv.`, "2");
  });

  it("in a logical expression", async () => {
    await expectValue(`
    IF distance( val1 = 'material' val2 = 'materail' ) <= 2.
      WRITE 'close'.
    ENDIF.`, "close");
  });

});
