import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DO", () => {

  beforeEach(async () => {
    abap = new ABAP(new MemoryConsole());
  });

  it("DO method() TIMES", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
    CLASS-METHODS int RETURNING VALUE(int) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DO int( ) TIMES.
    ENDDO.
  ENDMETHOD.
  METHOD int.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DO should set sy-index", async () => {
    const code = `
DO 3 TIMES.
  WRITE / sy-index.
ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("DO forever should set sy-index", async () => {
    const code = `
DO.
  WRITE / sy-index.
  IF sy-index >= 3.
    EXIT.
  ENDIF.
ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("DO, should reset to prev value after loop", async () => {
    const code = `
sy-index = 10.
DO 2 TIMES.
  WRITE / sy-index.
ENDDO.
WRITE / sy-index.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n10");
  });

  it("DO, early RETURN, should also reset sy-tabix", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS method1.
    CLASS-METHODS method2.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD method1.
    DO 2 TIMES.
      WRITE / sy-index.
      method2( ).
      WRITE / sy-index.
    ENDDO.
  ENDMETHOD.

  METHOD method2.
    DO 2 TIMES.
      WRITE / sy-index.
      RETURN.
    ENDDO.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>method1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n1\n2\n1\n2");
  });

});