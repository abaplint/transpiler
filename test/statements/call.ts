import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CALL", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("return value", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo.
    CLASS-METHODS bar RETURNING VALUE(field) TYPE string.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    DATA field TYPE string.
    CALL METHOD bar RECEIVING field = field.
    WRITE field.
  ENDMETHOD.

  METHOD bar.
    field = 'helloworld'.
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("helloworld");
  });

  it("dynamic method call", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo.
    CLASS-METHODS run.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE 'hello'.
  ENDMETHOD.

  METHOD run.
    DATA lv_bar TYPE string.
    lv_bar = 'FOO'.
    CALL METHOD lcl=>(lv_bar).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("dynamic method call, method in this", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS foo.
    METHODS bar.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    DATA lv_str TYPE string.
    lv_str = 'BAR'.
    CALL METHOD (lv_str).
  ENDMETHOD.

  METHOD bar.
    WRITE 'hello world'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello world");
  });

  it("throw CX_SY_DYN_CALL_ILLEGAL_METHOD", async () => {
    const code = `
CLASS lcl DEFINITION.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
* workaround for avoid testing a global class,
  CALL METHOD ('PROG-ZFOOBAR-LCL')=>nononono_limit.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch (e) {
      expect(e.toString()).to.contain("CX_SY_DYN_CALL_ILLEGAL_METHOD");
    }
  });

  it("call method, spaces, var", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS m1.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD m1.
    WRITE 'foo'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  DATA lv_name TYPE c LENGTH 10.
  lv_name = 'M1'.
  CREATE OBJECT lo.
  CALL METHOD lo->(lv_name).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("call method, escape interface method name", async () => {
    const code = `
INTERFACE lif.
  CLASS-METHODS method.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif~method.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CALL METHOD ('LCL')=>lif~method.`;
    const js = await run(code);
    expect(js).to.include("].lif$method");
  });

  it("call method, simple parameter table", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING val TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE val.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TYPES: BEGIN OF abap_parmbind,
           name  TYPE c LENGTH 30,
           kind  TYPE c LENGTH 1,
           value TYPE REF TO data,
         END OF abap_parmbind.
  TYPES abap_parmbind_tab TYPE HASHED TABLE OF abap_parmbind WITH UNIQUE KEY name.

  DATA tab TYPE abap_parmbind_tab.
  DATA row LIKE LINE OF tab.
  row-name = 'VAL'.
  row-kind = 'E'.
  GET REFERENCE OF 1 INTO row-value.
  INSERT row INTO TABLE tab.
  CALL METHOD lcl=>('FOO') PARAMETER-TABLE tab.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it.skip("call method, simple parameter table, with exceptions tabl", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING val TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    WRITE val.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  TYPES: BEGIN OF abap_parmbind,
           name  TYPE c LENGTH 30,
           kind  TYPE c LENGTH 1,
           value TYPE REF TO data,
         END OF abap_parmbind.
  TYPES abap_parmbind_tab TYPE HASHED TABLE OF abap_parmbind WITH UNIQUE KEY name.

  TYPES: BEGIN OF abap_excpbind,
           name  TYPE c LENGTH 30,
           value TYPE i,
         END OF abap_excpbind.
  TYPES abap_excpbind_tab TYPE HASHED TABLE OF abap_excpbind WITH UNIQUE KEY name.

  DATA tab TYPE abap_parmbind_tab.
  DATA etab TYPE abap_excpbind_tab.
  DATA row LIKE LINE OF tab.
  row-name = 'VAL'.
  row-kind = 'E'.
  GET REFERENCE OF 1 INTO row-value.
  INSERT row INTO TABLE tab.
  CALL METHOD lcl=>('FOO') PARAMETER-TABLE tab EXCEPTION-TABLE etab.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it.only("dynamic method call, returning parameter", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS run.
    METHODS get RETURNING VALUE(int) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    TYPES: BEGIN OF abap_parmbind,
             name  TYPE c LENGTH 30,
             kind  TYPE c LENGTH 1,
             value TYPE REF TO data,
           END OF abap_parmbind.
    TYPES abap_parmbind_tab TYPE HASHED TABLE OF abap_parmbind WITH UNIQUE KEY name.
    DATA lt_parameters_getter TYPE abap_parmbind_tab.
    DATA result TYPE REF TO i.

    CREATE DATA result.
    INSERT VALUE #( name = 'INT' value = result kind = 'R' ) INTO TABLE lt_parameters_getter.
    CALL METHOD me->('GET')
      PARAMETER-TABLE lt_parameters_getter.
    WRITE / result->*.
  ENDMETHOD.

  METHOD get.
    int = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->run( ).`;
    const js = await run(code);
    console.dir(js);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("static call, interfaced aliased method", async () => {
    const code = `
INTERFACE lif1.
  METHODS foo.
ENDINTERFACE.

INTERFACE lif2.
  INTERFACES lif1.

  ALIASES foo FOR lif1~foo.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif2.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif1~foo.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  CALL METHOD ref->lif2~foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("dynamic call, interfaced aliased method", async () => {
    const code = `
INTERFACE lif1.
  METHODS foo.
ENDINTERFACE.

INTERFACE lif2.
  INTERFACES lif1.

  ALIASES foo FOR lif1~foo.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif2.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif1~foo.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  CALL METHOD ref->('LIF2~FOO').`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("dynamic call, string", async () => {
    const code = `
INTERFACE lif.
  METHODS bar.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD lif~bar.
    WRITE / 'foo'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  CALL METHOD lo->(\`LIF~BAR\`).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("dynamic call, private method", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
  PRIVATE SECTION.
    METHODS priv.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA name TYPE string.
    DATA lo_obj TYPE REF TO lcl.
    CREATE OBJECT lo_obj.
    name = 'PRIV'.
    CALL METHOD lo_obj->(name).
  ENDMETHOD.

  METHOD priv.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("dynamic call, public method fixed name accessing instance attribute", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    METHODS name RETURNING VALUE(val) TYPE i.
  PRIVATE SECTION.
    DATA foo TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD name.
    val = foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  CALL METHOD lo->('NAME').`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("dynamic call, interfaced method", async () => {
    const code = `
INTERFACE lif.
  METHODS foo.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif~foo.
    WRITE / 'works'.
  ENDMETHOD.

  METHOD run.
    DATA lv_method TYPE string.
    lv_method = 'LIF~FOO'.
    CALL METHOD me->(lv_method).
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  ref->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("works");
  });

});