import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, compileFiles, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Interface", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("call interfaced method", async () => {
    const code = `
INTERFACE lif_foo.
  METHODS bar.
ENDINTERFACE.

CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_foo.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD lif_foo~bar.
    WRITE 'helloabc'.
  ENDMETHOD.
ENDCLASS.

FORM bar.
  DATA ref TYPE REF TO lif_foo.
  CREATE OBJECT ref TYPE lcl_foo.
  ref->bar( ).
ENDFORM.

START-OF-SELECTION.
  PERFORM bar.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("helloabc");
  });

  it("class implementing interface with attribute", async () => {
    const code = `
INTERFACE lif_bar.
  DATA moo TYPE i.
ENDINTERFACE.

CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_bar.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    lif_bar~moo = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_foo.
  CREATE OBJECT bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("class, interface with constant", async () => {
    const code = `
INTERFACE lif_bar.
  CONSTANTS moo TYPE i VALUE 2.
ENDINTERFACE.

CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_bar.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    WRITE lif_bar~moo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_foo.
  CREATE OBJECT bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("class, interfaced method", async () => {
    const code = `
INTERFACE zif_test.
  METHODS moo.
ENDINTERFACE.

CLASS zcl_super DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_test.
ENDCLASS.
CLASS zcl_super IMPLEMENTATION.
  METHOD zif_test~moo.
    WRITE 2.
  ENDMETHOD.
ENDCLASS.

FORM foo.
  DATA bar TYPE REF TO zif_test.
  CREATE OBJECT bar TYPE zcl_super.
  bar->moo( ).
ENDFORM.

PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("write constant from interface", async () => {
    const code = `
    INTERFACE lif_foo.
      CONSTANTS bar TYPE i VALUE 2.
    ENDINTERFACE.

    FORM moo.
      WRITE lif_foo=>bar.
    ENDFORM.

    START-OF-SELECTION.
      PERFORM moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("Value from constant structure in interface", async () => {
    const code = `
INTERFACE lif.
  CONSTANTS: BEGIN OF bar,
               foo TYPE c VALUE 'A',
             END OF bar.
ENDINTERFACE.

FORM moo.
  WRITE lif=>bar-foo.
ENDFORM.

START-OF-SELECTION.
  PERFORM moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A");
  });

  it("interfaced target variable", async () => {
    const code = `
INTERFACE lintf.
  DATA bar TYPE i.
ENDINTERFACE.

CLASS lclas DEFINITION.
  PUBLIC SECTION.
    INTERFACES lintf.
    METHODS bar.
ENDCLASS.

CLASS lclas IMPLEMENTATION.
  METHOD bar.
    me->lintf~bar = 2.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Value from interface output", async () => {
    const code = `
INTERFACE lintf.
  DATA bar TYPE i.
ENDINTERFACE.

CLASS lclas DEFINITION.
  PUBLIC SECTION.
    INTERFACES lintf.
ENDCLASS.

CLASS lclas IMPLEMENTATION.
ENDCLASS.

FORM bar.
  DATA li TYPE REF TO lintf.
  CREATE OBJECT li TYPE lclas.
  li->bar = 2.
  WRITE li->bar.
ENDFORM.

START-OF-SELECTION.
  PERFORM bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("global interface and classes", async () => {
    const zcl_client = `
    CLASS zcl_client DEFINITION PUBLIC.
      PUBLIC SECTION.
        INTERFACES if_client.
    ENDCLASS.
    CLASS zcl_client IMPLEMENTATION.
    ENDCLASS.`;

    const tests = `
    CLASS lcl_test DEFINITION.
      PUBLIC SECTION.
        METHODS bar.
    ENDCLASS.
    CLASS lcl_test IMPLEMENTATION.
      METHOD bar.
        DATA li_client TYPE REF TO if_client.
        CREATE OBJECT li_client TYPE zcl_client.
        CLEAR li_client->request.
      ENDMETHOD.
    ENDCLASS.`;

    const if_client = `
    INTERFACE if_client PUBLIC.
      DATA request TYPE REF TO if_request.
    ENDINTERFACE.`;

    const if_request = `
    INTERFACE if_request PUBLIC.
    ENDINTERFACE.`;

    const result = await compileFiles([
      {filename: "zcl_client.clas.abap", contents: zcl_client},
      {filename: "zcl_client.clas.testclasses.abap", contents: tests},
      {filename: "if_client.intf.abap", contents: if_client},
      {filename: "if_request.intf.abap", contents: if_request},
    ]);

    const js = result.objects[1].chunk.getCode();
    expect(js).to.contain(".if_client$request");
  });

  it("WRITE constant from interface", async () => {
    const code = `
INTERFACE lif.
  CONSTANTS bar TYPE c VALUE 'a'.
ENDINTERFACE.

FORM moo.
  WRITE lif=>bar.
ENDFORM.

START-OF-SELECTION.
  PERFORM moo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("a");
  });

  it("call interfaced method in super, check it compiles to valid JS", async () => {
    const code = `
INTERFACE lcl_if.
  METHODS get_text RETURNING VALUE(result) TYPE string.
ENDINTERFACE.

CLASS sup DEFINITION.
  PUBLIC SECTION.
    INTERFACES lcl_if.
ENDCLASS.

CLASS sup IMPLEMENTATION.
  METHOD lcl_if~get_text.
  ENDMETHOD.
ENDCLASS.

CLASS sub DEFINITION INHERITING FROM sup.
  PUBLIC SECTION.
    METHODS lcl_if~get_text REDEFINITION.
ENDCLASS.

CLASS sub IMPLEMENTATION.
  METHOD lcl_if~get_text.
    CALL METHOD super->lcl_if~get_text
      RECEIVING
        result = result.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("data from interface implementing interface", async () => {
    const code = `
INTERFACE zif_top.
  DATA bar TYPE i.
ENDINTERFACE.

INTERFACE zif_client.
  INTERFACES zif_top.
ENDINTERFACE.

CLASS zcl_client DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_client.
    METHODS constructor.
ENDCLASS.


CLASS zcl_client IMPLEMENTATION.
  METHOD constructor.
    zif_top~bar = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA obj TYPE REF TO zcl_client.
  CREATE OBJECT obj.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("calls static method", async () => {
    const code = `
INTERFACE lif.
  CLASS-METHODS m1.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif~m1.
    WRITE 'works'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>lif~m1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("works");
  });

  it("constants initial value", async () => {
    const code = `
INTERFACE bar.
  TYPES enum_formatting_option TYPE i.
  CONSTANTS: BEGIN OF formatting_option,
               no_formatting TYPE enum_formatting_option VALUE IS INITIAL,
               camel_case    TYPE enum_formatting_option VALUE 1,
             END OF formatting_option.
ENDINTERFACE.

START-OF-SELECTION.
  WRITE bar=>formatting_option-no_formatting.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("local, namespaced, me", async () => {
    const code = `
INTERFACE /foo/lif.
  DATA go TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES /foo/lif.
    METHODS foo.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    ASSERT /foo/lif~go IS INITIAL.
    ASSERT me->/foo/lif~go IS INITIAL.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("default = SPACE in interface", async () => {
    const code = `
INTERFACE /foo/bar.
  METHODS bar IMPORTING foo TYPE i DEFAULT SPACE.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES /foo/bar.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD /foo/bar~bar.
    WRITE foo.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("keyword new", async () => {
    const code = `
INTERFACE lif.
  METHODS new.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif~new.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("default uname", async () => {
    const code = `
INTERFACE lif.
  METHODS foo IMPORTING u LIKE sy-uname DEFAULT sy-uname.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD lif~foo.
  ENDMETHOD.
ENDCLASS.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("call method should prefix intf name", async () => {
    const code = `
INTERFACE lif.
  METHODS foo.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD lif~foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lif.
  CREATE OBJECT lo TYPE lcl.
  CALL METHOD lo->foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("call method should prefix intf name, static", async () => {
    const code = `
INTERFACE lif.
  CLASS-METHODS foo.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD lif~foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  CALL METHOD lcl=>lif~foo.
  lcl=>lif~foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("interface var set with prefix", async () => {
    const code = `
INTERFACE /foo/lif.
  DATA foo TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES /foo/lif.
    METHODS constructor.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD constructor.
    me->/foo/lif~foo = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("interface constant referenced via instance", async () => {
    const code = `
INTERFACE lif.
  CONSTANTS foo TYPE i VALUE 6.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl.
  CREATE OBJECT ref.
  WRITE ref->lif~foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6");
  });

  it("interface static constant accessed via instance", async () => {
    const code = `
INTERFACE lif.
  CLASS-DATA field TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lif.
  CREATE OBJECT lo TYPE lcl.
  WRITE lo->field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get().trimEnd()).to.equal("0");
  });

  it("interface, nested constants values", async () => {
    const code = `
INTERFACE lif.
  CONSTANTS: BEGIN OF  http_content_type,
               BEGIN OF application,
                 json TYPE c LENGTH 20 VALUE 'application/json',
                 xml  TYPE c LENGTH 20 VALUE 'application/xml',
               END OF  application,
             END OF    http_content_type.
ENDINTERFACE.`;
    const js = await run(code);
    expect(js).to.include("application/json");
  });

  it("interface, upper case nested constant", async () => {
    const code = `
INTERFACE lif.
  CONSTANTS:
    BEGIN OF gc_type,
      val1 TYPE string VALUE 'val1',
      VAL2 TYPE string VALUE 'val2',
    END OF gc_type.
ENDINTERFACE.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("interface, aliases from other interface constant", async () => {
    const code = `
INTERFACE if1.
  CONSTANTS world TYPE string VALUE 'moo'.
ENDINTERFACE.

INTERFACE if2.
  INTERFACES if1.
  ALIASES hello FOR if1~world.
ENDINTERFACE.

START-OF-SELECTION.
  WRITE / if2=>hello.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("moo");
  });


  // INTERFACES ... PARTIALLY IMPLEMENTED: a system generates the methods the class
  // does not implement as empty methods, the transpiler emitted nothing for them
  it("PARTIALLY IMPLEMENTED, calling an unimplemented method raises CX_SY_DYN_CALL_ILLEGAL_METHOD", async () => {
    // ABAP keyword documentation, INTERFACES - PARTIALLY IMPLEMENTED: "If an interface
    // method that is not implemented is called during a test, an exception from the
    // class CX_SY_DYN_CALL_ILLEGAL_METHOD is raised." Before, it was a TypeError
    const code = `
CLASS cx_root DEFINITION ABSTRACT PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.

CLASS cx_sy_dyn_call_illegal_method DEFINITION PUBLIC INHERITING FROM cx_root.
  PUBLIC SECTION.
    DATA classname TYPE string.
    DATA methodname TYPE string.
    METHODS constructor IMPORTING classname TYPE string OPTIONAL methodname TYPE string OPTIONAL.
ENDCLASS.
CLASS cx_sy_dyn_call_illegal_method IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->classname = classname.
    me->methodname = methodname.
  ENDMETHOD.
ENDCLASS.

INTERFACE lif.
  METHODS a.
  METHODS b RETURNING VALUE(r) TYPE i.
ENDINTERFACE.

CLASS ltd DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd IMPLEMENTATION.
  METHOD lif~a.
    WRITE 'a'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lif.
  DATA lx TYPE REF TO cx_sy_dyn_call_illegal_method.
  DATA v TYPE i.
  CREATE OBJECT lo TYPE ltd.
  lo->a( ).
  TRY.
      v = lo->b( ).
      WRITE 'no exception'.
    CATCH cx_sy_dyn_call_illegal_method INTO lx.
      WRITE lx->classname.
      WRITE lx->methodname.
  ENDTRY.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("aLTDLIF~B");
  });

  it("PARTIALLY IMPLEMENTED, without the exception class the error names the method", async () => {
    const code = `
INTERFACE lif.
  METHODS b RETURNING VALUE(r) TYPE i.
ENDINTERFACE.

CLASS ltd DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lif.
  DATA v TYPE i.
  CREATE OBJECT lo TYPE ltd.
  v = lo->b( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    let message = "";
    try {
      await f(abap);
    } catch (e) {
      message = e.message;
    }
    expect(message).to.contain("CX_SY_DYN_CALL_ILLEGAL_METHOD");
    expect(message).to.contain("LTD->LIF~B");
  });

  it("PARTIALLY IMPLEMENTED, static method, included interface, and the implemented methods keep working", async () => {
    const code = `
CLASS cx_root DEFINITION ABSTRACT PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.

CLASS cx_sy_dyn_call_illegal_method DEFINITION PUBLIC INHERITING FROM cx_root.
  PUBLIC SECTION.
    DATA classname TYPE string.
    DATA methodname TYPE string.
    METHODS constructor IMPORTING classname TYPE string OPTIONAL methodname TYPE string OPTIONAL.
ENDCLASS.
CLASS cx_sy_dyn_call_illegal_method IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->classname = classname.
    me->methodname = methodname.
  ENDMETHOD.
ENDCLASS.

INTERFACE lif_inner.
  METHODS inner RETURNING VALUE(r) TYPE string.
ENDINTERFACE.

INTERFACE lif.
  INTERFACES lif_inner.
  METHODS done RETURNING VALUE(r) TYPE i.
  CLASS-METHODS stat RETURNING VALUE(r) TYPE i.
ENDINTERFACE.

CLASS ltd DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd IMPLEMENTATION.
  METHOD lif~done.
    r = 1.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO ltd.
  DATA lx TYPE REF TO cx_sy_dyn_call_illegal_method.
  CREATE OBJECT lo.
  WRITE lo->lif~done( ).
  TRY.
      lo->lif_inner~inner( ).
    CATCH cx_sy_dyn_call_illegal_method INTO lx.
      WRITE lx->methodname.
  ENDTRY.
  TRY.
      ltd=>lif~stat( ).
    CATCH cx_sy_dyn_call_illegal_method INTO lx.
      WRITE lx->methodname.
  ENDTRY.
  TRY.
      lo->lif~stat( ).
    CATCH cx_sy_dyn_call_illegal_method INTO lx.
      WRITE lx->methodname.
  ENDTRY.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1LIF_INNER~INNERLIF~STATLIF~STAT");
  });

  it("PARTIALLY IMPLEMENTED, interface inherited from the superclass keeps its implementation", async () => {
    const code = `
CLASS cx_root DEFINITION ABSTRACT PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.

CLASS cx_sy_dyn_call_illegal_method DEFINITION PUBLIC INHERITING FROM cx_root.
  PUBLIC SECTION.
    DATA classname TYPE string.
    DATA methodname TYPE string.
    METHODS constructor IMPORTING classname TYPE string OPTIONAL methodname TYPE string OPTIONAL.
ENDCLASS.
CLASS cx_sy_dyn_call_illegal_method IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->classname = classname.
    me->methodname = methodname.
  ENDMETHOD.
ENDCLASS.

INTERFACE lif_inner.
  METHODS inner RETURNING VALUE(r) TYPE i.
ENDINTERFACE.

INTERFACE lif.
  INTERFACES lif_inner.
  METHODS b RETURNING VALUE(r) TYPE i.
ENDINTERFACE.

CLASS lcl_base DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_inner.
ENDCLASS.

CLASS lcl_base IMPLEMENTATION.
  METHOD lif_inner~inner.
    r = 7.
  ENDMETHOD.
ENDCLASS.

CLASS ltd DEFINITION INHERITING FROM lcl_base FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif PARTIALLY IMPLEMENTED.
ENDCLASS.

CLASS ltd IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lif.
  DATA lx TYPE REF TO cx_sy_dyn_call_illegal_method.
  CREATE OBJECT lo TYPE ltd.
  WRITE lo->lif_inner~inner( ).
  TRY.
      lo->b( ).
    CATCH cx_sy_dyn_call_illegal_method INTO lx.
      WRITE lx->methodname.
  ENDTRY.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("7LIF~B");
  });

  it("without PARTIALLY IMPLEMENTED the missing method is still reported", async () => {
    const code = `
INTERFACE lif.
  METHODS a.
  METHODS b RETURNING VALUE(r) TYPE i.
ENDINTERFACE.

CLASS ltd DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES lif.
ENDCLASS.

CLASS ltd IMPLEMENTATION.
  METHOD lif~a.
  ENDMETHOD.
ENDCLASS.`;

    let message = "";
    try {
      await run(code);
    } catch (e: any) {
      message = e.message;
    }
    expect(message).to.contain("implement_methods");
  });

});
