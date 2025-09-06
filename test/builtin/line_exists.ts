import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_line_exists.prog.abap", contents}]);
}

describe("Builtin functions - line_exists", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("positive", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
INSERT VALUE #( ) INTO TABLE tab.
IF line_exists( tab[ 1 ] ).
  WRITE / 'yes'.
ELSE.
  WRITE / 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("negative", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
IF line_exists( tab[ 1 ] ).
  WRITE / 'yes'.
ELSE.
  WRITE / 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`no`);
  });

  it("negated negative", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
IF NOT line_exists( tab[ 1 ] ).
  WRITE / 'yes'.
ELSE.
  WRITE / 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it.skip("call method", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
    CLASS-METHODS foo RETURNING VALUE(int) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF ty,
             field TYPE i,
           END OF ty.
    DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
    INSERT VALUE #( field = 2 ) INTO TABLE tab.
    IF line_exists( tab[ field = lcl=>foo( ) ] ).
      WRITE / 'yes'.
    ENDIF.
  ENDMETHOD.

  METHOD foo.
    int = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

});
