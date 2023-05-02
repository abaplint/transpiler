import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src/index.js";
import {AsyncFunction, runFiles} from "../_utils.js";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - SORT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Basic sort table", async () => {
    const code = `
      DATA: table   TYPE STANDARD TABLE OF i,
            integer TYPE i.
      APPEND 2 TO table.
      APPEND 1 TO table.
      SORT table.
      LOOP AT table INTO integer.
        WRITE / integer.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("Basic sort table, descending", async() => {
    const code = `
      DATA: table   TYPE STANDARD TABLE OF i,
            integer TYPE i.
      APPEND 2 TO table.
      APPEND 3 TO table.
      APPEND 1 TO table.
      SORT table DESCENDING.
      LOOP AT table INTO integer.
        WRITE / integer.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3\n2\n1");
  });

  it("SORT structure", async () => {
    const code = `
      TYPES: BEGIN OF ty_structure,
              field TYPE i,
            END OF ty_structure.
      DATA tab TYPE STANDARD TABLE OF ty_structure WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      row-field = 2.
      APPEND row TO tab.
      row-field = 1.
      APPEND row TO tab.
      SORT tab BY field.
      LOOP AT tab INTO row.
        WRITE / row-field.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("SORT BY table_line", async () => {
    const code = `
      DATA lt_keywords TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      APPEND 'foo' TO lt_keywords.
      APPEND 'bar' TO lt_keywords.
      SORT lt_keywords BY table_line ASCENDING.
      DATA keyword TYPE string.
      LOOP AT lt_keywords INTO keyword.
        WRITE / keyword.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar\nfoo");
  });

});