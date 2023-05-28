import {expect} from "chai";
import {ABAP, MemoryConsole} from "../packages/runtime/src/";
import {AsyncFunction, runFiles} from "./_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Value conversions", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("hex value conversion", async () => {
    const code = `
  DATA hex TYPE x.
  DATA integer TYPE i.
  hex = 'AA'.
  integer = hex.
  WRITE integer.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("170");
  });

  it("hex value conversion, longer", async () => {
    const code = `
    DATA hex TYPE x LENGTH 4.
    DATA integer TYPE i.
    hex = 'AAAAAAAA'.
    integer = hex.
    WRITE integer.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("-1431655766");
  });

  it("character to string value conversion", async () => {
    const code = `
    DATA lv_char TYPE c LENGTH 5.
    DATA lv_str TYPE string.
    lv_char = ' '.
    lv_str = lv_char.
    ASSERT lv_str = ||.
    lv_char = 'a b  '.
    lv_str = lv_char.
    ASSERT lv_str = |a b|.
    lv_char = ' b'.
    lv_str = lv_char.
    ASSERT lv_str = | b|.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("character to string value conversion 2", async () => {
    const code = `
  DATA lv_str TYPE string.
  DATA lt_tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  APPEND ' ' TO lt_tab.
  READ TABLE lt_tab INDEX 1 INTO lv_str.
  ASSERT lv_str = ||.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("character to string value conversion 3", async () => {
    const code = `
    DATA lv_str TYPE string.
    lv_str = ' '.
    ASSERT lv_str = ||.
    lv_str = 'a b  '.
    ASSERT lv_str = |a b|.
    lv_str = ' b'.
    ASSERT lv_str = | b|.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("empty string to int", async () => {
    const code = `
  DATA str TYPE string.
  DATA int TYPE i.
  int = str.
  ASSERT int = 0.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("empty char to int", async () => {
    const code = `
  DATA str TYPE c.
  DATA int TYPE i.
  int = str.
  ASSERT int = 0.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("field symbol to structure", async () => {
    const code = `
TYPES: BEGIN OF bar,
         foo TYPE i,
       END OF bar.
DATA data1 TYPE bar.
DATA data2 TYPE bar.
FIELD-SYMBOLS <fs> TYPE bar.

ASSIGN data1 TO <fs>.
data1-foo = 2.

data2 = <fs>.
WRITE data2-foo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("field symbol to i", async () => {
    const code = `
  DATA data1 TYPE i.
  DATA data2 TYPE i.
  FIELD-SYMBOLS <fs> TYPE i.

  ASSIGN data1 TO <fs>.
  data1 = 2.

  data2 = <fs>.
  WRITE data2.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("field symbol to table", async () => {
    const code = `
  TYPES tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA data1 TYPE tab.
  DATA data2 TYPE tab.
  FIELD-SYMBOLS <fs> TYPE tab.

  ASSIGN data1 TO <fs>.
  data2 = <fs>.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});