import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, compileFiles, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running code structure - Interface", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
        WRITE li_client->request.
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

  it.only("call interfaced method in super, check it compiles to valid JS", async () => {
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

});