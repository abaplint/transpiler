import {ABAP} from "../../packages/runtime/src";
import {runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CLEAR", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
    const f = new Function("abap", js);
    f(abap);
  });

  it("basic CLEAR", async () => {
    const code = `
      DATA da TYPE i.
      da = 2.
      CLEAR da.
      ASSERT da = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("clear object reference", async () => {
    const code = `
      interface lif_bar.
      endinterface.
      DATA bar TYPE REF TO lif_bar.
      CLEAR bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

});