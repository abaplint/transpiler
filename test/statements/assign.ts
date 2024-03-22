import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - ASSIGN", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic ASSIGN COMPONENT", async () => {
    const code = `
      TYPES: BEGIN OF ty_stru,
              bar TYPE string,
            END OF ty_stru.
      DATA: ls_stru TYPE ty_stru.
      FIELD-SYMBOLS <lv_val> TYPE any.
      ls_stru-bar = 'foo'.
      ASSIGN COMPONENT 'BAR' OF STRUCTURE ls_stru TO <lv_val>.
      WRITE <lv_val>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("basic ASSIGN COMPONENT, space", async () => {
    const code = `
      TYPES: BEGIN OF ty_stru,
              bar TYPE string,
            END OF ty_stru.
      DATA: ls_stru TYPE ty_stru.
      FIELD-SYMBOLS <lv_val> TYPE any.
      ls_stru-bar = 'foo'.
      ASSIGN COMPONENT 'BAR ' OF STRUCTURE ls_stru TO <lv_val>.
      WRITE <lv_val>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("ASSIGN fs TO fs", async () => {
    const code = `
      FIELD-SYMBOLS <fs1> TYPE i.
      FIELD-SYMBOLS <fs2> TYPE i.
      DATA data TYPE i.
      data = 42.
      ASSIGN data TO <fs1>.
      ASSIGN <fs1> TO <fs2>.
      ASSERT <fs2> IS ASSIGNED.
      WRITE <fs2>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("42");
  });

  it("ASSIGN fs TO fs, 2", async () => {
    const code = `
      DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      FIELD-SYMBOLS <fs1> TYPE i.
      FIELD-SYMBOLS <fs2> TYPE i.
      DO 3 TIMES.
        APPEND sy-index TO tab.
      ENDDO.
      LOOP AT tab ASSIGNING <fs1>.
        IF <fs2> IS NOT ASSIGNED.
          ASSIGN <fs1> TO <fs2>.
        ENDIF.
      ENDLOOP.
      ASSERT <fs1> = 3.
      ASSERT <fs2> = 1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("more ASSIGNing", async () => {
    const code = `
      TYPES: BEGIN OF ty_structure,
              field1 TYPE i,
              field2 TYPE i,
            END OF ty_structure.
      DATA tab TYPE STANDARD TABLE OF ty_structure WITH DEFAULT KEY.
      FIELD-SYMBOLS <fs1> TYPE ty_structure.
      FIELD-SYMBOLS <fs2> TYPE ty_structure.
      DO 2 TIMES.
        APPEND INITIAL LINE TO tab ASSIGNING <fs1>.
        <fs1>-field1 = sy-tabix.
      ENDDO.
      LOOP AT tab ASSIGNING <fs1>.
        IF <fs2> IS ASSIGNED.
          <fs2>-field2 = sy-tabix.
        ENDIF.
        ASSIGN <fs1> TO <fs2>.
      ENDLOOP.
      LOOP AT tab ASSIGNING <fs1>.
        WRITE / <fs1>-field1.
        WRITE / <fs1>-field2.
      ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2\n2\n0");
  });

  it("assign with field symbol source", async () => {
    const code = `
TYPES: BEGIN OF ts_test,
         a TYPE c LENGTH 4,
       END OF ts_test.
DATA lt_test TYPE STANDARD TABLE OF ts_test WITH DEFAULT KEY.
DATA ls_test LIKE LINE OF lt_test.
FIELD-SYMBOLS <ls> LIKE LINE OF lt_test.
FIELD-SYMBOLS <lv> TYPE any.

ls_test-a = 'asd'.
APPEND ls_test TO lt_test.
LOOP AT lt_test ASSIGNING <ls> .
  ASSIGN COMPONENT 'A' OF STRUCTURE <ls> TO <lv>.
  WRITE <lv>.
ENDLOOP.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("asd ");
  });

  it("ASSIGN by number", async () => {
    const code = `
      TYPES:
        BEGIN OF ts_test,
          a TYPE c LENGTH 4,
        END OF ts_test.
      DATA ls_test type ts_test.
      ls_test-a = 'ABCD'.
      FIELD-SYMBOLS <lv> TYPE any.
      ASSIGN COMPONENT 1 OF STRUCTURE ls_test TO <lv>.
      WRITE <lv>.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABCD");
  });

  it("ASSIGN by with field symbol", async () => {
    const code = `
      DATA lv_test TYPE string VALUE 'ABCD'.
      DATA lv_test_ref TYPE REF TO data.
      GET REFERENCE OF lv_test INTO lv_test_ref.
      FIELD-SYMBOLS <lv_test> TYPE string.
      ASSIGN lv_test_ref->* TO <lv_test>.
      WRITE: / <lv_test>.
    `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABCD");
  });

  it("ASSIGN CASTING", async () => {
    const code = `
  DATA lv_x TYPE x LENGTH 2 VALUE '0000'.
  FIELD-SYMBOLS <lv_y> TYPE c.
  ASSIGN lv_x TO <lv_y> CASTING.
  WRITE strlen( <lv_y> ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("ASSIGN CASTING, 2", async () => {
    const code = `
TYPES:
  tv_char_4 TYPE c LENGTH 5,
  BEGIN OF ts_test,
    a TYPE tv_char_4,
  END OF ts_test.
DATA ls_test TYPE ts_test.
FIELD-SYMBOLS <lv> TYPE tv_char_4.
ls_test-a = '1234'.
ASSIGN COMPONENT 'A' OF STRUCTURE ls_test TO <lv> CASTING.
WRITE <lv>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1234 ");
  });

  it("ASSIGN CASTING, 3", async () => {
// note, this ABAP code doesnt actually test the right thing?
    const code = `
DATA lv_c   TYPE c LENGTH 1.
DATA lv_out TYPE x LENGTH 2.
DATA lv_x   TYPE x LENGTH 4 VALUE '00000000'.
FIELD-SYMBOLS <lv_y> TYPE c.
ASSIGN lv_x TO <lv_y> CASTING.
lv_c = <lv_y>.
lv_out = lv_c.
WRITE lv_out.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000");
  });

  it("ASSIGN CASTING, 4", async () => {
    const code = `
    DATA lv_c TYPE c LENGTH 1.
    FIELD-SYMBOLS <lv_x> TYPE x.
    lv_c = 'A'.
    ASSIGN lv_c TO <lv_x> CASTING.
    ASSERT <lv_x> = '4100'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ASSIGN COMPONENT of non STRUCTURE, should set subrc", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA bar TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

FORM run.
  DATA ref TYPE REF TO lcl.
  FIELD-SYMBOLS <fs> TYPE any.
  CREATE OBJECT ref.
  ASSIGN COMPONENT 'BAR' OF STRUCTURE ref TO <fs>.
  ASSERT sy-subrc = 4.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ASSIGN dynamic, class attribute", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA bar TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

FORM run.
  DATA ref TYPE REF TO lcl.
  DATA attribute_name TYPE string.
  FIELD-SYMBOLS <fs> TYPE any.
  CREATE OBJECT ref.
  CONCATENATE 'ref->' 'bar' INTO attribute_name.
  ASSIGN (attribute_name) TO <fs>.
  ASSERT sy-subrc = 0.
  WRITE <fs>.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("ASSIGN dynamic, ok", async () => {
    const code = `
DATA foo TYPE i.
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN ('FOO') TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ASSIGN dynamic, ok, text based", async () => {
    const code = `
DATA foo TYPE i.
DATA text TYPE string.
FIELD-SYMBOLS <fs> TYPE any.
text = 'FOO'.
ASSIGN (text) TO <fs>.
ASSERT sy-subrc = 0.
ASSERT <fs> IS ASSIGNED.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ASSIGN dynamic, set subrc", async () => {
    const code = `
FIELD-SYMBOLS <fs> TYPE any.
ASSIGN ('SDFDSFS') TO <fs>.
ASSERT sy-subrc = 4.
ASSERT <fs> IS NOT ASSIGNED.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ASSIGN another test", async () => {
    const code = `
DATA: BEGIN OF stru,
        bar TYPE i,
      END OF stru.
DATA lv TYPE c LENGTH 1.
FIELD-SYMBOLS <stru> TYPE any.
FIELD-SYMBOLS <field> TYPE simple.
ASSIGN stru TO <stru>.
ASSIGN COMPONENT 'BAR' OF STRUCTURE <stru> TO <field>.
ASSERT sy-subrc = 0.
DESCRIBE FIELD <field> TYPE lv.
WRITE lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("I");
  });

  it("ASSIGN float CASTING TYPE x", async () => {
// javascript / open-abap is little endian
    const code = `
    DATA lv_f TYPE f.
    FIELD-SYMBOLS <lv_hex> TYPE x.
    lv_f = 2.
    ASSIGN lv_f TO <lv_hex> CASTING TYPE x.
    WRITE <lv_hex>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000000000000040");
  });

  it("ASSIGN float CASTING TYPE x, assign", async () => {
// javascript / open-abap is little endian
    const code = `
    DATA lv_f TYPE f.
    FIELD-SYMBOLS <lv_hex> TYPE x.
    ASSIGN lv_f TO <lv_hex> CASTING TYPE x.
    <lv_hex> = '0000000000000040'.
    ASSERT lv_f = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("recursion and field symbols", async () => {
    const code = `
CLASS lcl_font DEFINITION.
  PUBLIC SECTION.
    DATA bold TYPE c LENGTH 1.
ENDCLASS.

CLASS lcl_font IMPLEMENTATION.
ENDCLASS.

CLASS lcl_style DEFINITION.
  PUBLIC SECTION.
    DATA font TYPE REF TO lcl_font.
    METHODS constructor.
    CLASS-METHODS run1.
    CLASS-METHODS run2 IMPORTING foo TYPE any.
ENDCLASS.

CLASS lcl_style IMPLEMENTATION.
  METHOD constructor.
    CREATE OBJECT font.
  ENDMETHOD.

  METHOD run2.
    FIELD-SYMBOLS <attribute> TYPE any.
    ASSIGN ('foo->bold') TO <attribute>.
    ASSERT sy-subrc = 0.
    <attribute> = abap_true.
  ENDMETHOD.

  METHOD run1.
    DATA style TYPE REF TO lcl_style.
    FIELD-SYMBOLS <attribute> TYPE any.
    CREATE OBJECT style.
    ASSIGN ('style->font') TO <attribute>.
    run2( <attribute> ).
    WRITE style->font->bold.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_style=>run1( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("X");
  });

  it("ASSIGN tab CASTING TYPE x", async () => {
    const code = `
  TYPES hex TYPE x LENGTH 10.
  DATA tab TYPE STANDARD TABLE OF hex WITH HEADER LINE.
  DATA len TYPE i.
  FIELD-SYMBOLS <binary> TYPE x.
  ASSIGN COMPONENT 1 OF STRUCTURE tab TO <binary> TYPE 'X'.
  ASSERT sy-subrc = 4.
  ASSIGN COMPONENT 0 OF STRUCTURE tab TO <binary> TYPE 'X'.
  ASSERT sy-subrc = 0.
  DESCRIBE FIELD <binary> LENGTH len IN BYTE MODE.
  WRITE len.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("ASSIGN tab", async () => {
    const code = `
    TYPES char TYPE c LENGTH 10.
    DATA tab TYPE STANDARD TABLE OF char WITH DEFAULT KEY.
    FIELD-SYMBOLS <any> TYPE any.

    ASSIGN COMPONENT 1 OF STRUCTURE tab TO <any>.
    WRITE / sy-subrc.
    ASSIGN COMPONENT 0 OF STRUCTURE tab TO <any>.
    WRITE / sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4\n0");
  });

  it("ASSIGN dynamic", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA foo TYPE REF TO ty.
FIELD-SYMBOLS <field> TYPE any.
CREATE DATA foo.
ASSIGN foo->('FIELD') TO <field>.
<field> = 2.
WRITE foo->field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("ASSIGN dynamic 2", async () => {
    const code = `
TYPES: BEGIN OF ty,
         field TYPE i,
       END OF ty.
DATA foo TYPE REF TO ty.
DATA: BEGIN OF symb,
        name TYPE string,
      END OF symb.
FIELD-SYMBOLS <field> TYPE any.
CREATE DATA foo.
symb-name = 'FIELD'.
ASSIGN foo->(symb-name) TO <field>.
<field> = 2.
WRITE foo->field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("ASSIGN unassigned deref data reference", async () => {
    const code = `
DATA ref TYPE REF TO data.
FIELD-SYMBOLS <table> TYPE ANY TABLE.

ASSIGN ref->* TO <table>.
WRITE sy-subrc.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("unassigned fs", async () => {
    const code = `
    FIELD-SYMBOLS <table1> TYPE ANY TABLE.
    FIELD-SYMBOLS <table2> TYPE ANY TABLE.
    ASSIGN <table1> TO <table2>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    try {
      await f(abap);
      expect.fail();
    } catch(e) {
      expect(e.toString()).to.contain("GETWA_NOT_ASSIGNED");
    }
  });

  it("value from namespaced intf", async () => {
    const code = `
INTERFACE /foo/if.
  DATA int TYPE i.
ENDINTERFACE.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    INTERFACES /foo/if.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lv_str TYPE string.
    FIELD-SYMBOLS <fs> TYPE i.
    lv_str = '/FOO/IF~INT'.
    ASSIGN (lv_str) TO <fs>.
    WRITE sy-subrc.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("spaces in dynname", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA vart TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  FIELD-SYMBOLS <fs> TYPE i.
  CREATE OBJECT lo.
  ASSIGN ('LO->VART   ') TO <fs>.
  ASSERT sy-subrc = 0.
  <fs> = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("spaces in dynname, another example", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA vari TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  FIELD-SYMBOLS <fs> TYPE i.
  CREATE OBJECT lo.
  ASSIGN lo->('VARI   ') TO <fs>.
  ASSERT sy-subrc = 0.
  <fs> = 2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("dynamic assign ME->", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    FIELD-SYMBOLS <fs1> TYPE any.
    FIELD-SYMBOLS <fs2> TYPE any.
    ASSIGN ('me->FOO') TO <fs1>.
    WRITE / sy-subrc.
    <fs1> = 2.
    ASSIGN ('ME->FOO') TO <fs2>.
    WRITE / sy-subrc.
    WRITE / <fs2>.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("0\n0\n2");
  });

  it("dynamic assign ME->, another case", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lv_name TYPE c LENGTH 10.
    FIELD-SYMBOLS <fs1> TYPE any.
    lv_name = 'FOO'.
    ASSIGN me->(lv_name) TO <fs1>.
    WRITE / sy-subrc.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("0");
  });

  it("dynamic assign ME->, another case, upper", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
    METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    DATA lv_name TYPE c LENGTH 10.
    FIELD-SYMBOLS <fs1> TYPE any.
    lv_name = 'FOO'.
    ASSIGN ME->(lv_name) TO <fs1>.
    WRITE / sy-subrc.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  CREATE OBJECT lo.
  lo->run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("0");
  });

  it("dynamic assign ref deep", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA: BEGIN OF foo,
            field TYPE i,
          END OF foo.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_ref TYPE REF TO lcl.
  FIELD-SYMBOLS <any> TYPE any.
  CREATE OBJECT lo_ref.
  lo_ref->foo-field = 2.
  ASSIGN ('LO_REF->FOO-FIELD') TO <any>.
  WRITE / sy-subrc.
  WRITE / <any>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("0\n2");
  });

  it("component deep", async () => {
    const code = `
FIELD-SYMBOLS <test> TYPE data.
DATA: BEGIN OF foo,
        BEGIN OF bar,
          baz TYPE string,
        END OF bar,
      END OF foo.

ASSIGN COMPONENT 'BAR-BAZ' OF STRUCTURE foo TO <test>.
IF sy-subrc <> 0.
  WRITE / 'BUG'.
ELSE.
  WRITE / 'OK'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("OK");
  });

  it("fs", async () => {
    const code = `
CLASS lcl_attribute DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
ENDCLASS.
CLASS lcl_attribute IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl_attribute.
  FIELD-SYMBOLS <any> TYPE REF TO lcl_attribute.
  FIELD-SYMBOLS <feld> TYPE any.
  CREATE OBJECT ref.
  ASSIGN ref TO <any>.
  ASSIGN <any>->('FOO') TO <feld>.
  WRITE <feld>.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("0");
  });

  it("interfaced field name", async () => {
    const code = `
INTERFACE lif_intf.
  DATA field TYPE i.
ENDINTERFACE.

CLASS lcl_impl DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_intf.
ENDCLASS.
CLASS lcl_impl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_foo TYPE REF TO lcl_impl.
  DATA lv_name TYPE string.
  FIELD-SYMBOLS <any> TYPE any.

  CREATE OBJECT lo_foo.
  lv_name = 'LIF_INTF~FIELD'.

  ASSIGN lo_foo->('LIF_INTF~FIELD') TO <any>.
  ASSERT sy-subrc = 0.
  ASSIGN lo_foo->(lv_name) TO <any>.
  ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("dynamic dereference", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE REF TO i.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    CREATE DATA foo.
    foo->* = 5.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lo_foo TYPE REF TO lcl_foo.
  FIELD-SYMBOLS <int> TYPE i.
  CREATE OBJECT lo_foo.
  ASSIGN lo_foo->('FOO->*') TO <int>.
  ASSERT sy-subrc = 0.
  WRITE / <int>.
  <int> = 2.
  WRITE / lo_foo->foo->*.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.getTrimmed()).to.equal("5\n2");
  });

});