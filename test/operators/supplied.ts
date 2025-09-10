import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_supplied.prog.abap", contents}]);
}

describe("Running operators - SUPPLIED", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("IS SUPPLIED", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo
      IMPORTING opt TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD moo.
    IF opt IS SUPPLIED.
      WRITE / 'yes'.
    ELSE.
      WRITE / 'no'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM run.
  lcl_bar=>moo( ).
  lcl_bar=>moo( 1 ).
  lcl_bar=>moo( opt = 1 ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no\nyes\nyes");
  });

  it("IS SUPPLIED with boolc()", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo
      IMPORTING opt TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD moo.
    IF boolc( opt IS SUPPLIED ) = abap_true.
      WRITE / 'yes'.
    ELSE.
      WRITE / 'no'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM run.
  lcl_bar=>moo( ).
  lcl_bar=>moo( 1 ).
  lcl_bar=>moo( opt = 1 ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no\nyes\nyes");
  });

  it("IS NOT SUPPLIED", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING bar TYPE string OPTIONAL.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    ASSERT bar IS NOT SUPPLIED.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NOT IS SUPPLIED", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo IMPORTING bar TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    IF NOT bar IS SUPPLIED.
      WRITE / 'moo'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("moo");
  });

  it("IS NOT SUPPLIED, completely empty", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run IMPORTING bar TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    IF bar IS NOT SUPPLIED.
      WRITE / 'moo'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("moo");
  });

});