import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

async function execute(code: string) {
  const js = await run(code);
  const f = new AsyncFunction("abap", js);
  await f(abap);
}

// the expected values are checked on a real system, the tests fail via ASSERT
describe("Running code structure - Inheritance", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("static attribute is shared by super and sub class", async () => {
    await execute(`
CLASS lcl_super DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA counter TYPE i.
ENDCLASS.
CLASS lcl_super IMPLEMENTATION.
ENDCLASS.
CLASS lcl_sub DEFINITION INHERITING FROM lcl_super.
ENDCLASS.
CLASS lcl_sub IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_sub.
  lcl_sub=>counter = 5.
  lcl_super=>counter = lcl_super=>counter + 1.
  obj = NEW #( ).
  obj->counter = obj->counter + 10.
  ASSERT lcl_sub=>counter = 16.
  ASSERT lcl_super=>counter = 16.`);
  });

  it("inherited static method changes the static attribute seen by both classes", async () => {
    await execute(`
CLASS lcl_super DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA log TYPE string.
    CLASS-METHODS add IMPORTING iv TYPE string.
ENDCLASS.
CLASS lcl_super IMPLEMENTATION.
  METHOD add.
    log = log && iv.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_sub DEFINITION INHERITING FROM lcl_super.
  PUBLIC SECTION.
    METHODS run.
ENDCLASS.
CLASS lcl_sub IMPLEMENTATION.
  METHOD run.
    add( 'a' ).
    log = log && 'b'.
    lcl_sub=>add( 'c' ).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  NEW lcl_sub( )->run( ).
  lcl_super=>add( 'd' ).
  lcl_sub=>add( 'e' ).
  ASSERT lcl_sub=>log = 'abcde'.
  ASSERT lcl_super=>log = 'abcde'.`);
  });

  it("order of class constructors and instance constructors", async () => {
    await execute(`
CLASS lcl_super DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA trace TYPE string.
    CLASS-METHODS class_constructor.
    METHODS constructor.
ENDCLASS.
CLASS lcl_super IMPLEMENTATION.
  METHOD class_constructor.
    trace = trace && 'Ps'.
  ENDMETHOD.
  METHOD constructor.
    trace = trace && 'Pi'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_sub DEFINITION INHERITING FROM lcl_super.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
    METHODS constructor.
ENDCLASS.
CLASS lcl_sub IMPLEMENTATION.
  METHOD class_constructor.
    trace = trace && 'Cs'.
  ENDMETHOD.
  METHOD constructor.
    trace = trace && 'C1'.
    super->constructor( ).
    trace = trace && 'C2'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_sub.
  obj = NEW #( ).
  obj = NEW #( ).
  ASSERT lcl_super=>trace = 'PsCsC1PiC2C1PiC2'.`);
  });

  it("super-> skips a middle class without redefinition", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    METHODS m RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD m.
    r = 'A'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
ENDCLASS.
CLASS lcl_c DEFINITION INHERITING FROM lcl_b.
  PUBLIC SECTION.
    METHODS m REDEFINITION.
ENDCLASS.
CLASS lcl_c IMPLEMENTATION.
  METHOD m.
    r = super->m( ) && 'C'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_a.
  obj = NEW lcl_c( ).
  ASSERT obj->m( ) = 'AC'.`);
  });

  it("super-> chain over three levels", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    METHODS m RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD m.
    r = 'A'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS m REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD m.
    r = super->m( ) && 'B'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_c DEFINITION INHERITING FROM lcl_b.
  PUBLIC SECTION.
    METHODS m REDEFINITION.
ENDCLASS.
CLASS lcl_c IMPLEMENTATION.
  METHOD m.
    r = super->m( ) && 'C'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_a.
  obj = NEW lcl_c( ).
  ASSERT obj->m( ) = 'ABC'.
  obj = NEW lcl_b( ).
  ASSERT obj->m( ) = 'AB'.`);
  });

  it("abstract method called from the super class runs the redefinition", async () => {
    await execute(`
CLASS lcl_a DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS run RETURNING VALUE(r) TYPE string.
  PROTECTED SECTION.
    METHODS hook ABSTRACT RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD run.
    r = |[{ hook( ) }]|.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PROTECTED SECTION.
    METHODS hook REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD hook.
    r = 'sub'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_a.
  obj = NEW lcl_b( ).
  ASSERT obj->run( ) = '[sub]'.`);
  });

  it("each constructor of three levels calls the implementation of its own class", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    DATA log TYPE string.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS hook RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD constructor.
    log = log && hook( ).
  ENDMETHOD.
  METHOD hook.
    r = 'A'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS hook REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    log = log && hook( ).
  ENDMETHOD.
  METHOD hook.
    r = 'B'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_c DEFINITION INHERITING FROM lcl_b.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS hook REDEFINITION.
ENDCLASS.
CLASS lcl_c IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    log = log && hook( ).
  ENDMETHOD.
  METHOD hook.
    r = 'C'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_a.
  obj = NEW lcl_c( ).
  ASSERT obj->log = 'ABC'.`);
  });

  it("redefinition calls super-> of the super class implementation, after the super constructor", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
    DATA log TYPE string.
  PROTECTED SECTION.
    DATA mode TYPE string VALUE 'a'.
    METHODS hook.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD constructor.
    hook( ).
  ENDMETHOD.
  METHOD hook.
    log = log && mode.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS constructor.
  PROTECTED SECTION.
    METHODS hook REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mode = 'b'.
    hook( ).
  ENDMETHOD.
  METHOD hook.
    super->hook( ).
    log = log && 'H' && mode.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  ASSERT NEW lcl_b( )->log = 'abHb'.`);
  });

  it("interface method implemented in the super class, redefined in the sub class", async () => {
    await execute(`
INTERFACE lif.
  METHODS name RETURNING VALUE(r) TYPE string.
ENDINTERFACE.
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    ALIASES name FOR lif~name.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD lif~name.
    r = 'A'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS lif~name REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD lif~name.
    r = super->lif~name( ) && 'B'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA if_ref TYPE REF TO lif.
  DATA obj TYPE REF TO lcl_a.
  DATA sub TYPE REF TO lcl_b.
  obj = NEW lcl_b( ).
  if_ref ?= obj.
  sub ?= obj.
  ASSERT if_ref->name( ) = 'AB'.
  ASSERT obj->lif~name( ) = 'AB'.
  ASSERT sub->name( ) = 'AB'.`);
  });

  it("redefinition gets the default value of the inherited parameter", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    METHODS m IMPORTING iv TYPE string DEFAULT 'd' RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD m.
    r = 'A' && iv.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS m REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD m.
    r = 'B' && iv.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_a.
  obj = NEW lcl_b( ).
  ASSERT obj->m( ) = 'Bd'.
  ASSERT obj->m( 'x' ) = 'Bx'.`);
  });

  it("sub class without constructor takes the constructor of the nearest super class", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    DATA name TYPE string.
    METHODS constructor IMPORTING iv_a TYPE string iv_b TYPE string DEFAULT 'def'.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD constructor.
    name = iv_a && iv_b.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS constructor IMPORTING iv TYPE string.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_a = iv ).
  ENDMETHOD.
ENDCLASS.
CLASS lcl_c DEFINITION INHERITING FROM lcl_b.
ENDCLASS.
CLASS lcl_c IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA obj1 TYPE REF TO lcl_a.
  DATA obj2 TYPE REF TO lcl_c.
  obj1 = NEW lcl_c( iv = 'x' ).
  CREATE OBJECT obj2 EXPORTING iv = 'y'.
  ASSERT obj1->name = 'xdef'.
  ASSERT obj2->name = 'ydef'.`);
  });

  it("start values of attributes of both classes are set before the constructors", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    DATA a TYPE string VALUE 'a'.
    METHODS constructor.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD constructor.
    a = to_upper( a ).
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    DATA b TYPE string VALUE 'b'.
    DATA r TYPE string.
    METHODS constructor.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    r = a && b.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  ASSERT NEW lcl_b( )->r = 'Ab'.`);
  });

  it("constants and types of the super class in the sub class", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c TYPE string VALUE 'k'.
    TYPES ty TYPE c LENGTH 3.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS m RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD m.
    DATA x TYPE ty.
    DATA long TYPE string VALUE 'abcdef'.
    x = long.
    r = c && x.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA x TYPE lcl_b=>ty.
  DATA long TYPE string VALUE 'xyzw'.
  x = long.
  ASSERT NEW lcl_b( )->m( ) = 'kabc'.
  ASSERT lcl_b=>c = 'k'.
  ASSERT x = 'xyz'.`);
  });

  it("dynamic calls of inherited and redefined methods", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    METHODS m RETURNING VALUE(r) TYPE string.
    METHODS n RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD m.
    r = 'A'.
  ENDMETHOD.
  METHOD n.
    r = 'n'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS m REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD m.
    r = 'B'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_a.
  DATA r1 TYPE string.
  DATA r2 TYPE string.
  obj = NEW lcl_b( ).
  CALL METHOD obj->('M') RECEIVING r = r1.
  CALL METHOD obj->('N') RECEIVING r = r2.
  ASSERT r1 = 'B'.
  ASSERT r2 = 'n'.
  CLEAR obj.
  CREATE OBJECT obj TYPE ('LCL_B').
  ASSERT obj->m( ) = 'B'.`);
  });

  it("redefined event handler", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    EVENTS evt.
    METHODS fire.
    METHODS on_evt FOR EVENT evt OF lcl_a.
    DATA log TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD fire.
    RAISE EVENT evt.
  ENDMETHOD.
  METHOD on_evt.
    log = log && 'A'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS on_evt REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD on_evt.
    log = log && 'B'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO lcl_a.
  obj = NEW lcl_b( ).
  SET HANDLER obj->on_evt FOR obj.
  obj->fire( ).
  ASSERT obj->log = 'B'.`);
  });

  it("me stored by the super constructor dispatches to the redefinition later", async () => {
    await execute(`
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA registry TYPE STANDARD TABLE OF REF TO lcl_a WITH EMPTY KEY.
    METHODS constructor.
    METHODS name RETURNING VALUE(r) TYPE string.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
  METHOD constructor.
    APPEND me TO registry.
  ENDMETHOD.
  METHOD name.
    r = 'A'.
  ENDMETHOD.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
  PUBLIC SECTION.
    METHODS name REDEFINITION.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
  METHOD name.
    r = 'B'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA res TYPE string.
  DATA ref TYPE REF TO lcl_a.
  NEW lcl_a( ).
  NEW lcl_b( ).
  LOOP AT lcl_a=>registry INTO ref.
    res = res && ref->name( ).
  ENDLOOP.
  ASSERT res = 'AB'.`);
  });

});
