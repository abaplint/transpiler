import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function output(code: string): Promise<string> {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
  return abap.console.get();
}

describe("Running code structure - generic parameters passed by value", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("generic c, a change in the method does not reach the caller", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(text) TYPE c.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    text = 'Z'.
    WRITE / text.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_text TYPE c LENGTH 4 VALUE 'ABCD'.
  lcl=>run( lv_text ).
  WRITE / lv_text.`;
    expect(await output(code)).to.equal("Z   \nABCD");
  });

  it("generic c keeps the length of the actual parameter", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(text) TYPE c.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lv_length TYPE i.
    DESCRIBE FIELD text LENGTH lv_length IN CHARACTER MODE.
    WRITE / lv_length.
    WRITE / text.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_text TYPE c LENGTH 4 VALUE 'ABCD'.
  lcl=>run( lv_text ).`;
    expect(await output(code)).to.equal("4\nABCD");
  });

  it("any, structure", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_row,
             field TYPE i,
           END OF ty_row.
    CLASS-METHODS run IMPORTING VALUE(data) TYPE any.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    FIELD-SYMBOLS <ls_row> TYPE ty_row.
    ASSIGN data TO <ls_row>.
    <ls_row>-field = 2.
    WRITE / <ls_row>-field.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ls_row TYPE lcl=>ty_row.
  ls_row-field = 1.
  lcl=>run( ls_row ).
  WRITE / ls_row-field.`;
    expect(await output(code)).to.equal("2\n1");
  });

  it("any table", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(rows) TYPE STANDARD TABLE.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    CLEAR rows.
    WRITE / lines( rows ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lt_rows TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  APPEND 1 TO lt_rows.
  APPEND 2 TO lt_rows.
  lcl=>run( lt_rows ).
  WRITE / lines( lt_rows ).`;
    expect(await output(code)).to.equal("0\n2");
  });

  it("csequence, string", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(text) TYPE csequence.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    text = 'changed'.
    WRITE / text.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_text TYPE string.
  lv_text = 'original'.
  lcl=>run( lv_text ).
  WRITE / lv_text.`;
    expect(await output(code)).to.equal("changed\noriginal");
  });

  it("actual parameter is a field symbol", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(text) TYPE c.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    text = 'Z'.
    WRITE / text.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_text TYPE c LENGTH 4 VALUE 'ABCD'.
  FIELD-SYMBOLS <lv_text> TYPE c.
  ASSIGN lv_text TO <lv_text>.
  lcl=>run( <lv_text> ).
  WRITE / lv_text.`;
    expect(await output(code)).to.equal("Z   \nABCD");
  });

  it("optional, not supplied", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(text) TYPE c OPTIONAL.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    IF text IS INITIAL.
      WRITE / 'initial'.
    ENDIF.
    text = 'Z'.
    WRITE / text.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    expect(await output(code)).to.equal("initial\nZ");
  });

  it("with DEFAULT, supplied and not supplied", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING VALUE(text) TYPE c DEFAULT 'D'.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    WRITE / text.
    text = 'Z'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_text TYPE c LENGTH 4 VALUE 'ABCD'.
  lcl=>run( ).
  lcl=>run( lv_text ).
  WRITE / lv_text.`;
    expect(await output(code)).to.equal("D\nABCD\nABCD");
  });

  it("passed by reference, the method still reads the caller's value", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run IMPORTING text TYPE c.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD run.
    WRITE / text.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lv_text TYPE c LENGTH 4 VALUE 'ABCD'.
  lcl=>run( lv_text ).`;
    expect(await output(code)).to.equal("ABCD");
  });

});
