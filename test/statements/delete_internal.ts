import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DELETE internal", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Basic delete internal", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE table WHERE table_line = 1.
      ASSERT lines( table ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE from table INDEX", async () => {
    const code = `
      DATA foo TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      APPEND 2 TO foo.
      APPEND 3 TO foo.
      DELETE foo INDEX 2.
      ASSERT lines( foo ) = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, no deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, one deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DELETE table FROM index", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA row LIKE LINE OF tab.
      DO 4 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      DELETE tab FROM 2.
      LOOP AT tab INTO row.
        WRITE / row.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("DELETE INITIAL where IS INITIAL", async () => {
    const code = `
      DATA lt_keywords TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      APPEND '' TO lt_keywords.
      ASSERT lines( lt_keywords ) = 1.
      DELETE lt_keywords WHERE table_line IS INITIAL.
      ASSERT lines( lt_keywords ) = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("delete internal tab with object references", async () => {
    const code = `
      CLASS lcl_foo DEFINITION.
      ENDCLASS.
      CLASS lcl_foo IMPLEMENTATION.
      ENDCLASS.

      DATA tab TYPE STANDARD TABLE OF REF TO lcl_foo.
      DATA ref1 TYPE REF TO lcl_foo.
      DATA ref2 TYPE REF TO lcl_foo.
      CREATE OBJECT ref1.
      APPEND ref1 TO tab.
      CREATE OBJECT ref2.
      APPEND ref2 TO tab.

      DELETE tab INDEX 2.
      ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});