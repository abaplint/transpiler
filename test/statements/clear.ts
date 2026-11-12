import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CLEAR", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("Clear structure", async () => {
    const code = `
      TYPES: BEGIN OF ty_bar,
          moo TYPE i,
        END OF ty_bar.
      DATA: data1 TYPE ty_bar.
      data1-moo = 2.
      CLEAR data1.
      ASSERT data1-moo = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic CLEAR", async () => {
    const code = `
      DATA da TYPE i.
      da = 2.
      CLEAR da.
      ASSERT da = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("clear object reference", async () => {
    const code = `
      interface lif_bar.
      endinterface.
      DATA bar TYPE REF TO lif_bar.
      CLEAR bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("clear structure", async () => {
    const code = `
DATA: BEGIN OF hex,
        01 TYPE x LENGTH 1,
        11 TYPE x LENGTH 1,
      END   OF hex.
CLEAR hex-01.
CLEAR hex-11.
WRITE hex-01.
WRITE hex-11.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("clear hex", async () => {
    const code = `
    DATA lane TYPE x LENGTH 8.
    CLEAR lane.
    lane+4 = '11223344'.
    WRITE lane.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000000011223344");
  });

  it("CLEAR of a table with a header line clears the header line, FREE the rows", async () => {
    const code = `
TYPES ty_c3 TYPE c LENGTH 3.
DATA tab TYPE STANDARD TABLE OF ty_c3 WITH HEADER LINE.
DATA count TYPE i.
DATA row TYPE ty_c3.
tab = 'A'.
APPEND tab.
tab = 'B'.
APPEND tab.
CLEAR tab.
count = lines( tab[] ).
WRITE / count.
APPEND tab.
READ TABLE tab INDEX 3 INTO row.
WRITE / |{ row }.|.
tab = 'C'.
FREE tab.
count = lines( tab[] ).
WRITE / count.
APPEND tab.
READ TABLE tab INDEX 1 INTO row.
WRITE / |{ row }.|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2\n.\n0\nC.");
  });

});