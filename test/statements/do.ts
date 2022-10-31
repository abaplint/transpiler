import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - DO", () => {

  beforeEach(async () => {
    abap = new ABAP();
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

});