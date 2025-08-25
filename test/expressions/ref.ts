import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_ref.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - REF", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
DATA foo TYPE i.
DATA ref TYPE REF TO i.
ref = REF #( foo ).
WRITE ref->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("generic data", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar IMPORTING data TYPE data.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD bar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA int TYPE i.
  lcl=>bar( REF #( int ) ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.only("methods & field symbols & references", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS ref_of_x_into_y
      CHANGING
        x TYPE any
        y TYPE any.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD ref_of_x_into_y.
    DATA lr_any TYPE REF TO data.
    lr_any = REF #( x ).
    y ?= lr_any.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TYPES: BEGIN OF dty,
           field1 TYPE string,
           field2 TYPE i,
         END OF dty.
  DATA data TYPE REF TO dty.

  TYPES: BEGIN OF ty,
           value TYPE REF TO data,
         END OF ty.
  DATA tab TYPE STANDARD TABLE OF ty WITH DEFAULT KEY.
  DATA row TYPE ty.
  DATA lv_type TYPE c LENGTH 1.
  FIELD-SYMBOLS <val> TYPE any.
  FIELD-SYMBOLS <field> TYPE any.

  CREATE DATA data.

  ASSIGN data->field1 TO <field>.
  lcl=>ref_of_x_into_y( CHANGING
    x = <field>
    y = row-value ).
  APPEND row TO tab.

  ASSIGN data->field2 TO <field>.
  lcl=>ref_of_x_into_y( CHANGING
    x = <field>
    y = row-value ).
  APPEND row TO tab.

  LOOP AT tab INTO row.
    ASSIGN row-value->* TO <val>.
    DESCRIBE FIELD <val> TYPE lv_type.
    WRITE / lv_type.
  ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("g\nI");
  });

});