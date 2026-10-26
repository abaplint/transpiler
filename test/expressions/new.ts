import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_new.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - NEW", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo RETURNING VALUE(val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    val = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  ref = NEW #( ).
  WRITE / ref->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("with constructor parameter", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING bar TYPE i.
    METHODS foo RETURNING VALUE(val) TYPE i.
  PRIVATE SECTION.
    DATA mv TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    mv = bar.
  ENDMETHOD.

  METHOD foo.
    val = mv.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  ref = NEW #( bar = 2 ).
  WRITE / ref->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("with constructor parameter unnamed", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING bar TYPE i.
    METHODS foo RETURNING VALUE(val) TYPE i.
  PRIVATE SECTION.
    DATA mv TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    mv = bar.
  ENDMETHOD.

  METHOD foo.
    val = mv.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  ref = NEW #( 2 ).
  WRITE / ref->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("named new", async () => {
    const code = `
CLASS lcl DEFINITION.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  ref = NEW lcl( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("inline ref to data", async () => {
    const code = `
FORM foo.
  TYPES ty TYPE i.
  DATA(lv_msgv1) = NEW ty( 2 ).
  write lv_msgv1->*.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("inline ref to local structure type", async () => {
    const code = `
FORM foo.
  TYPES: BEGIN OF ty,
           bar TYPE i,
         END OF ty.
  DATA(sdf) = NEW ty( ).
  WRITE / sdf->bar.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("data, source is a method call", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS seven RETURNING VALUE(val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD seven.
    val = 7.
  ENDMETHOD.
ENDCLASS.

FORM foo.
  DATA(ref) = NEW i( lcl=>seven( ) ).
  WRITE ref->*.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("7");
  });

  it("data, source is a table expression", async () => {
    const code = `
FORM foo.
  DATA numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  APPEND 3 TO numbers.
  DATA(ref) = NEW i( numbers[ 1 ] ).
  WRITE ref->*.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("data, a new data object, not a reference to the source variable", async () => {
    const code = `
FORM foo.
  DATA(value) = 1.
  DATA(ref) = NEW i( value ).
  value = 2.
  WRITE ref->*.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("data, a new data object, not a reference to the source row", async () => {
    const code = `
FORM foo.
  DATA numbers TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  APPEND 3 TO numbers.
  DATA(ref) = NEW i( numbers[ 1 ] ).
  numbers[ 1 ] = 4.
  WRITE ref->*.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("data, literal source, changed through the reference", async () => {
    const code = `
FORM foo.
  DATA(ref) = NEW i( 2 ).
  ref->* = 5.
  DATA(ref2) = NEW i( 2 ).
  DATA(two) = 2.
  WRITE |{ ref->* } { ref2->* } { two }|.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5 2 2");
  });

  it("data, source converted to the type", async () => {
    const code = `
FORM foo.
  DATA(ref) = NEW i( '42' ).
  DATA(result) = ref->* + 1.
  WRITE result.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("43");
  });

  it("data, structure components", async () => {
    const code = `
FORM foo.
  TYPES: BEGIN OF ty,
           a TYPE i,
           b TYPE i,
         END OF ty.
  DATA(ref) = NEW ty( a = 3 b = 4 ).
  WRITE |{ ref->a } { ref->b }|.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3 4");
  });

  it("data, table lines", async () => {
    const code = `
FORM foo.
  TYPES ty_tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  DATA(ref) = NEW ty_tab( ( 1 ) ( 2 ) ).
  LOOP AT ref->* INTO DATA(line).
    WRITE / line.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("data, table lines with components", async () => {
    const code = `
FORM foo.
  TYPES: BEGIN OF ty,
           a TYPE i,
           b TYPE i,
         END OF ty.
  TYPES ty_tab TYPE STANDARD TABLE OF ty WITH EMPTY KEY.
  DATA(ref) = NEW ty_tab( ( a = 1 b = 2 ) ( a = 3 b = 4 ) ).
  LOOP AT ref->* INTO DATA(line).
    WRITE / |{ line-a } { line-b }|.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1 2\n3 4");
  });

  it("data, table from a table", async () => {
    const code = `
FORM foo.
  TYPES ty_tab TYPE STANDARD TABLE OF i WITH EMPTY KEY.
  DATA(numbers) = VALUE ty_tab( ( 1 ) ( 2 ) ).
  DATA(ref) = NEW ty_tab( numbers ).
  CLEAR numbers.
  WRITE lines( ref->* ).
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});
