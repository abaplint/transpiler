import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_switch.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - SWITCH", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
FORM foo.
  DATA(lv_name) = SWITCH string(
    'sdf' WHEN 'sdf' THEN 'foo' WHEN 'bar' THEN 'bar' ).
  WRITE / lv_name.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("fallthrough", async () => {
    const code = `
FORM foo.
  DATA(lv_name) = SWITCH string(
    '1' WHEN 'sdf' THEN 'foo' WHEN 'bar' THEN 'bar' ).
  WRITE / lv_name.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("OR", async () => {
    const code = `
FORM foo.
  DATA(lv_name) = SWITCH string(
    '1' WHEN 'sdf' OR '1' THEN 'foo' WHEN 'bar' THEN 'bar' ).
  WRITE / lv_name.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("ELSE", async () => {
    const code = `
FORM foo.
  DATA(lv_name) = SWITCH string(
    '1' WHEN 'sdf' THEN 'foo' WHEN 'bar' THEN 'bar' ELSE 'baz' ).
  WRITE / lv_name.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("baz");
  });

  it("ELSE, last WHEN matches", async () => {
    const code = `
FORM foo.
  DATA(lv_name) = SWITCH string(
    'bar' WHEN 'sdf' THEN 'foo' WHEN 'bar' THEN 'bar' ELSE 'baz' ).
  WRITE / lv_name.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("lower case keywords, inferred type, integer WHEN", async () => {
    const code = `
TYPES: BEGIN OF ty_struc,
         flag TYPE i,
       END OF ty_struc.
DATA ls_struc TYPE ty_struc.
DATA lv_result TYPE c LENGTH 1.

ls_struc-flag = 1.
lv_result = SWITCH #( ls_struc-flag WHEN 1 then 'S' ELSE 'N').
WRITE / lv_result.
ls_struc-flag = 2.
lv_result = SWITCH #( ls_struc-flag WHEN 1 then 'S' ELSE 'N').
WRITE / lv_result.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("S\nN");
  });

  it("lower case keywords, named type, attribute", async () => {
    const code = `
TYPES ty_name TYPE c LENGTH 10.

CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA mv_value TYPE ty_name.
    METHODS run RETURNING VALUE(rv_name) TYPE ty_name.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD run.
    rv_name = SWITCH ty_name( me->mv_value WHEN '' then 'DEFAULT' else me->mv_value ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_bar TYPE REF TO lcl_bar.
  CREATE OBJECT lo_bar.
  WRITE / lo_bar->run( ).
  lo_bar->mv_value = 'HELLO'.
  WRITE / lo_bar->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("DEFAULT   \nHELLO     ");
  });


  it("THROW", async () => {
    const code = `
CLASS cx_root DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS lcx DEFINITION INHERITING FROM cx_root.
  PUBLIC SECTION.
    DATA value TYPE i.
    METHODS constructor IMPORTING value TYPE i OPTIONAL.
ENDCLASS.
CLASS lcx IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->value = value.
  ENDMETHOD.
ENDCLASS.

FORM run USING iv TYPE i.
  DATA lv TYPE i.
  DATA lx TYPE REF TO lcx.
  TRY.
      lv = SWITCH #( iv WHEN 1 THEN 10 WHEN 2 OR 3 THEN THROW lcx( value = iv ) ELSE THROW lcx( ) ).
      WRITE / lv.
    CATCH lcx INTO lx.
      WRITE / lx->value.
  ENDTRY.
ENDFORM.

START-OF-SELECTION.
  PERFORM run USING 1.
  PERFORM run USING 3.
  PERFORM run USING 4.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10\n3\n0");
  });

});