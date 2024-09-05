import {expect} from "chai";
import {ABAP, MemoryConsole} from "../packages/runtime/src/";
import {AsyncFunction, runFiles} from "./_utils";
import {ITranspilerOptions} from "packages/transpiler/src";

let abap: ABAP;

async function run(contents: string) {
  const options: ITranspilerOptions = {
    keywords: [],
  };
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}], options);
}

describe("Keywords", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("class", async () => {
    const code = `
DATA class TYPE x.
class = 'AA'.
WRITE class.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AA");
  });

  it("class, case", async () => {
    const code = `
DATA CLASS TYPE x.
class = 'AA'.
WRITE class.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AA");
  });

  it("class, INITIAL", async () => {
    const code = `
DATA CLASS TYPE x.
IF class IS INITIAL.
ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("method parameter", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING class TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE / class.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( 2 ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("method parameter, returning", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo RETURNING VALUE(class) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    class = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("form parameter", async () => {
    const code = `
FORM foo USING class TYPE i.
  WRITE class.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo USING 2.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("class attribute", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
  PRIVATE SECTION.
    DATA class TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    WRITE class.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

});