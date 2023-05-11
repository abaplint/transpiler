import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - MODIFY internal", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("MODIFY internal table INDEX FROM", async () => {
    const code = `
    DATA result TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA dat LIKE LINE OF result.
    APPEND 2 TO result.
    MODIFY result INDEX 1 FROM 4.
    ASSERT sy-subrc = 0.
    READ TABLE result INDEX 1 INTO dat.
    WRITE dat.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("MODIFY, testing references", async () => {
    const code = `
  DATA integers TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA int TYPE i.
  APPEND 1 TO integers.
  APPEND 2 TO integers.
  int = 3.
  MODIFY integers INDEX 1 FROM int.
  int = 4.
  MODIFY integers INDEX 2 FROM int.
  LOOP AT integers INTO int.
    WRITE / int.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n4");
  });

  it("MODIFY, table key", async () => {
    const code = `
TYPES: BEGIN OF ty_css_var,
         name  TYPE string,
         value TYPE string,
       END OF ty_css_var.
TYPES ty_css_vars TYPE SORTED TABLE OF ty_css_var WITH UNIQUE KEY name.
DATA lt_tab TYPE ty_css_vars.
DATA row LIKE LINE OF lt_tab.
row-name = 'foo'.
row-value = 'bar'.
MODIFY TABLE lt_tab FROM row.
ASSERT lines( lt_tab ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});