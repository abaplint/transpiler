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

});