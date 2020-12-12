import {expect} from "chai";
import {ABAP} from "../packages/runtime/src/";
import {runFiles} from "./_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running Examples", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("Fibonacci", async () => {
    const code = `
    DATA: lv_old     TYPE i VALUE 1,
          lv_current TYPE i VALUE 2,
          lv_next    TYPE i.

    DO 8 TIMES.
      lv_next = lv_old + lv_current.
      lv_old = lv_current.
      lv_current = lv_next.
    ENDDO.`;

    const js = await run(code) + "\nreturn lv_current.get();";

    const f = new Function("abap", js);
    const res = f(abap);

    expect(res).to.equal(89);
  });

  it("Simple IF", async () => {
    const code = `
    DATA: foo TYPE i VALUE 1,
          bar TYPE i VALUE 1.

    IF foo = bar.
      foo = 2.
    ENDIF.`;

    const js = await run(code) + "\nreturn foo.get();";
    const f = new Function("abap", js);
    expect(f(abap)).to.equal(2);
  });

  it("Character field semantics", async () => {
    const code = `
    DATA: foo TYPE c.
    foo = 'abc'.
    ASSERT foo = 'a'.
    foo = 2 + 1.
    ASSERT foo = '3'.
    ASSERT foo = 3.
    foo = 2 + '1'.
    ASSERT foo = '3'.
    ASSERT foo = 3.
    foo = 0.
    ASSERT foo = '0'.
    ASSERT foo = 0.
    foo = '0'.
    ASSERT foo = '0'.
    ASSERT foo = 0.
    foo = |0|.
    ASSERT foo = '0'.
    ASSERT foo = 0.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Character field semantics", async () => {
    const code = `
    DATA lv_str TYPE string.
    DATA lt_table TYPE STANDARD TABLE OF string.
    lv_str = 'foo bar'.
    SPLIT lv_str AT | | INTO TABLE lt_table.
    ASSERT lines( lt_table ) = 2.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("ASSERTs, left hand and right hand, none should fail", async () => {
    const code = `
      ASSERT 1 = 1.
      ASSERT 1 = '1'.
      ASSERT 1 = |1|.
      ASSERT 1 = \`1\`.
      ASSERT '1' = 1.
      ASSERT |1| = 1.
      ASSERT \`1\` = 1.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Console tracks output", async () => {
    const code = `WRITE 'foo'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("Offset +1", async () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar+1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("bc");
  });

  it("Length (1)", async () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar(1).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("a");
  });

  it("Basic delete internal", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE table WHERE table_line = 1.
      ASSERT lines( table ) = 1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, no deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Basic delete ADJACENT DUPLICATES, one deleted", async () => {
    const code = `
      DATA table TYPE STANDARD TABLE OF i.
      APPEND 1 TO table.
      APPEND 2 TO table.
      APPEND 2 TO table.
      DELETE ADJACENT DUPLICATES FROM table.
      ASSERT lines( table ) = 2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("String compare", async () => {
    const code = `
    ASSERT 'a' < 'b'.
    ASSERT 'A' < 'b'.
    ASSERT 'A' < 'B'.
    ASSERT 'b' >= 'B'.
    ASSERT 'a' < 'ba'.
    ASSERT 1 < '2'.
    ASSERT 1 <= '1'.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Basic sort table", async () => {
    const code = `
    DATA: table   TYPE STANDARD TABLE OF i,
          integer TYPE i.
    APPEND 2 TO table.
    APPEND 1 TO table.
    SORT table.
    LOOP AT table INTO integer.
      WRITE / integer.
    ENDLOOP.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("Basic sort table, descending", async() => {
    const code = `
    DATA: table   TYPE STANDARD TABLE OF i,
          integer TYPE i.
    APPEND 2 TO table.
    APPEND 3 TO table.
    APPEND 1 TO table.
    SORT table DESCENDING.
    LOOP AT table INTO integer.
      WRITE / integer.
    ENDLOOP.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("3\n2\n1");

  });

  it("Should throw an error if invalid code is requested to be transpiled", async () => {
    const code = `THIS IS NOT ABAP.`;

    try {
      await run(code);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("Statement does not exist");
    }
  });

  it("Should throw an error if language features are not supported yet", async () => {
    const code = `
    DATA: table TYPE STANDARD TABLE OF i.
    DELETE ADJACENT DUPLICATES FROM table COMPARING FIELDS table_line`;

    try {
      await run(code);
      expect.fail();
    } catch (e) {
      expect(e.message).to.contain("Statement does not exist");
    }
  });

  it("Locally defined structure", async () => {
    const code = `
    TYPES: BEGIN OF ty_http,
             body TYPE string,
           END OF ty_http.
    DATA ls_request TYPE ty_http.
    ASSERT ls_request-body = ''.
    ls_request-body = 'foo'.
    ASSERT ls_request-body = 'foo'.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("LOOP AT pass by value", async () => {
    const code = `
  data tab type table of i.
  data val type i.
  append 2 to tab.
  loop at tab into val.
  write / val.
  val = 1.
  endloop.
  loop at tab into val.
  write / val.
  endloop.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2\n2");
  });

  it("APPEND string", async () => {
    const code = `
  data tab type standard table of string.
  data val type string.
  append |foo| to tab.
  loop at tab into val.
  write val.
  endloop.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("Class, simple method call", async () => {
    const code = `
    CLASS zcl_words DEFINITION.
      PUBLIC SECTION.
        METHODS
          run.
    ENDCLASS.

    CLASS zcl_words IMPLEMENTATION.
      METHOD run.
        WRITE 'foo'.
      ENDMETHOD.
    ENDCLASS.

    DATA foo TYPE REF TO zcl_words.
    CREATE OBJECT foo.
    foo->run( ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("Class, call method in same class", async () => {
    const code = `
    CLASS zcl_words DEFINITION.
      PUBLIC SECTION.
        METHODS:
          run,
          bar.
    ENDCLASS.

    CLASS zcl_words IMPLEMENTATION.
      METHOD run.
        bar( ).
      ENDMETHOD.
      METHOD bar.
        WRITE 'foo'.
      ENDMETHOD.
    ENDCLASS.

    DATA foo TYPE REF TO zcl_words.
    CREATE OBJECT foo.
    foo->run( ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("Class, attribute", async () => {
    const code = `
    CLASS zcl_words DEFINITION.
      PUBLIC SECTION.
        DATA bar TYPE i.
        METHODS: run.
    ENDCLASS.

    CLASS zcl_words IMPLEMENTATION.
      METHOD run.
        WRITE bar.
      ENDMETHOD.
    ENDCLASS.

    DATA foo TYPE REF TO zcl_words.
    CREATE OBJECT foo.
    foo->run( ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("Class, constructor", async () => {
    const code = `
    CLASS zcl_words DEFINITION.
      PUBLIC SECTION.
        DATA bar TYPE i.
        METHODS: constructor.
    ENDCLASS.

    CLASS zcl_words IMPLEMENTATION.
      METHOD constructor.
        bar = 2.
        WRITE bar.
      ENDMETHOD.
    ENDCLASS.

    DATA foo TYPE REF TO zcl_words.
    CREATE OBJECT foo.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("Basic CONCATENATE", async () => {
    const code = `
      DATA target TYPE string.
      CONCATENATE 'foo' 'bar' INTO target.
      ASSERT target = 'foobar'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Set structure", async () => {
    const code = `
TYPES: BEGIN OF ty_bar,
    moo TYPE i,
  END OF ty_bar.
DATA: data1 TYPE ty_bar,
      data2 TYPE ty_bar.
data1-moo = 2.
data2 = data1.
ASSERT data2-moo = 2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Clear structure", async () => {
    const code = `
TYPES: BEGIN OF ty_bar,
    moo TYPE i,
  END OF ty_bar.
DATA: data1 TYPE ty_bar.
data1-moo = 2.
CLEAR data1.
ASSERT data1-moo = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("sy-index", async () => {
    const code = `DO 1 TIMES.
    ASSERT sy-index = 1.
    ENDDO.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CLASS-DATA", async () => {
    const code = `
    CLASS zcl_words DEFINITION.
      PUBLIC SECTION.
        CLASS-DATA bar TYPE i.
        METHODS: run.
    ENDCLASS.

    CLASS zcl_words IMPLEMENTATION.
      METHOD run.
        bar = 2.
      ENDMETHOD.
    ENDCLASS.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CLASS-DATA, type ref", async () => {
    const code = `
    CLASS zcl_words DEFINITION.
      PUBLIC SECTION.
        CLASS-DATA bar TYPE REF TO zcl_words.
        METHODS: run.
    ENDCLASS.

    CLASS zcl_words IMPLEMENTATION.
      METHOD run.
        CREATE OBJECT bar.
      ENDMETHOD.
    ENDCLASS.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("basic FIELD-SYMBOLS", async () => {
    const code = `
  DATA da TYPE i.
  FIELD-SYMBOLS <fs> TYPE i.
  ASSIGN da TO <fs>.
  da = 1.
  ASSERT da = 1.
  ASSERT <fs> = 1.
  <fs> = 2.
  ASSERT da = 2.
  ASSERT <fs> = 2.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("basic CLEAR", async () => {
    const code = `
  DATA da TYPE i.
  da = 2.
  CLEAR da.
  ASSERT da = 0.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("static variable in class", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA foo TYPE i.
    CLASS-METHODS name.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD name.
    CLEAR foo.
  ENDMETHOD.
ENDCLASS.

lcl_bar=>name( ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("basic xstring", async () => {
    const code = `
    DATA foo TYPE xstring.
    foo = 'AA'.
    WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("AA");
  });

  it("basic strlen", async () => {
    const code = `
    DATA foo TYPE string.
    foo = '123'.
    WRITE strlen( foo ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("basic xstrlen", async () => {
    const code = `
    DATA foo TYPE xstring.
    foo = 'AA'.
    WRITE xstrlen( foo ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("xstring constant", async () => {
    const code = `
    CONSTANTS lc_raw TYPE xstring VALUE '48656C6C6F20576F726C64210D0A'.
    WRITE lc_raw.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("48656C6C6F20576F726C64210D0A");
  });

  it("IS INITIAL, yes", async () => {
    const code = `
    DATA raw TYPE xstring.
    IF raw IS INITIAL.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("IS INITIAL, no", async () => {
    const code = `
    CONSTANTS lc_raw TYPE xstring VALUE 'AA'.
    IF lc_raw IS INITIAL.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("GET BIT", async () => {
    const code = `
    DATA lv_bit TYPE i.
    DATA lv_c TYPE c LENGTH 1.
    DATA result TYPE string.
    DATA lv_x TYPE xstring.
    lv_x = 'AB'.
    DO 8 TIMES.
      GET BIT sy-index OF lv_x INTO lv_c.
      CONCATENATE result lv_c INTO result.
    ENDDO.
    WRITE / result.
    result = ''.
    lv_x = '01'.
    DO 8 TIMES.
      GET BIT sy-index OF lv_x INTO lv_c.
      CONCATENATE result lv_c INTO result.
    ENDDO.
    WRITE / result.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("10101011\n00000001");
  });

  it("early RETURN in method", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS bar RETURNING VALUE(ret) TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD bar.
    ret = 1.
    RETURN.
  ENDMETHOD.
ENDCLASS.

  WRITE lcl_bar=>bar( ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("basic minus", async () => {
    const code = `
  DATA foo TYPE i.
  foo = 5 - 2.
  WRITE foo.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("3");
  });

  it("hex type", async () => {
    const code = `
  DATA foo TYPE x.
  foo = 1.
  WRITE / foo.
  foo = 20.
  WRITE / foo.
  foo = 'AA'.
  WRITE / foo.
  foo = '1234'.
  WRITE / foo.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("01\n14\nAA\n12");
  });

  it("convert type1", async () => {
    const code = `
  DATA i TYPE i.
  i = '1'.
  WRITE i.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("convert type2", async () => {
    const code = `
  DATA i TYPE i.
  DATA c TYPE c.
  i = 2.
  c = '3'.
  i = i + c.
  WRITE i.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("convert type3", async () => {
    const code = `
  DATA foo TYPE c LENGTH 1.
  foo = 'AB'.
  WRITE foo.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("A");
  });

  it("hex offset and length", async () => {
    const code = `
  DATA x TYPE xstring.
  x = '123456'.
  WRITE / x+1.
  WRITE / x(1).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("3456\n12");
  });

  it("first bit of x", async () => {
    const code = `
  DATA x TYPE x.
  DATA c TYPE c.
  x = '01'.
  GET BIT 1 OF x INTO c.
  WRITE c.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("class constant from static method", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c TYPE i VALUE 10.
    CLASS-METHODS foo.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD foo.
    WRITE c.
  ENDMETHOD.
ENDCLASS.

lcl_bar=>foo( ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("class constant from instance method", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CONSTANTS c TYPE i VALUE 10.
    METHODS foo.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD foo.
    WRITE c.
  ENDMETHOD.
ENDCLASS.

  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  bar->foo( ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("source field lengths and offsets", async () => {
    const code = `
  DATA bar TYPE string VALUE '12345'.
  DATA len TYPE i.
  len = 2.
  WRITE / bar+len.
  WRITE / bar(len).
  WRITE / bar+2.
  WRITE / bar(2).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("345\n12\n345\n12");
  });

  it("ASSERT sy-subrc = 0.", async () => {
    const code = `ASSERT sy-subrc = 0.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("constant_0 should not change", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i.
  FIELD-SYMBOLS <lv_i> LIKE LINE OF tab.
  APPEND 0 TO tab.
  READ TABLE tab INDEX 1 ASSIGNING <lv_i>.
  <lv_i> = 123.
  WRITE 0.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("ASSERT obj ref is initial", async () => {
    const code = `
    INTERFACE lif_bar.
    ENDINTERFACE.
    DATA ref TYPE REF TO lif_bar.
    ASSERT ref IS INITIAL.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("DO with calculation", async () => {
    const code = `
  CONSTANTS lc_bar TYPE i VALUE 2.
  DO lc_bar - 1 TIMES.
    WRITE 'bar'.
  ENDDO.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("EXPORTING value", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS bar EXPORTING val TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD bar.
    val = 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  DATA res TYPE i.
  bar->bar( IMPORTING val = res ).
  WRITE res.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("clike", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS foo IMPORTING moo TYPE clike.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD foo.
    WRITE moo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  bar->foo( moo = 'hello' ).`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("NOT INITIAL", async () => {
    const code = `
    DATA str TYPE string.
    str = 'abc'.
    ASSERT NOT str IS INITIAL.
    str = ''.
    ASSERT str IS INITIAL.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("REPLACE ALL", async () => {
    const code = `
  DATA str TYPE string.
  str = 'aabbccbb'.
  REPLACE ALL OCCURRENCES OF |bb| IN str WITH |dd|.
  ASSERT str = 'aaddccdd'.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CONDENSE", async () => {
    const code = `
  DATA str TYPE string.
  str = | aa |.
  ASSERT str = | aa |.
  CONDENSE str.
  ASSERT str = |aa|.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CP", async () => {
    const code = `
  DATA bar TYPE string.
  bar = 'abc'.
  ASSERT bar CP 'a*'.
  ASSERT bar CP 'A*'.
  ASSERT bar CP '*b*'.
  ASSERT bar CP '*c'.
  ASSERT bar CP 'abc'.
  ASSERT bar CP '*abc*'.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("structured constant", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: BEGIN OF bar,
                 field TYPE c VALUE 'A',
               END OF bar.
ENDCLASS.
CLASS lcl_foo IMPLEMENTATION.
ENDCLASS.

WRITE lcl_foo=>bar-field.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("A");
  });

  it("DELETE from table INDEX", async () => {
    const code = `
  DATA foo TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  APPEND 2 TO foo.
  APPEND 3 TO foo.
  DELETE foo INDEX 2.
  ASSERT lines( foo ) = 1.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("simple PERFORM", async () => {
    const code = `
FORM bar.
  WRITE 'hello'.
ENDFORM.

START-OF-SELECTION.
  PERFORM bar.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("hello");
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
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("helloabc");
  });

  it("LOOP at assigning", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    FIELD-SYMBOLS <row> TYPE i.
    APPEND 1 TO tab.
    LOOP AT tab ASSIGNING <row>.
      <row> = 2.
    ENDLOOP.
    LOOP AT tab ASSIGNING <row>.
      WRITE <row>.
    ENDLOOP.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("FIND FIRST OCCURRENCE, found", async () => {
    const code = `
  DATA lv_offset.
  FIND FIRST OCCURRENCE OF |bar| IN |foobar| MATCH OFFSET lv_offset.
  WRITE / sy-subrc.
  WRITE / lv_offset.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0\n3");
  });

  it("FIND FIRST OCCURRENCE, not found", async () => {
    const code = `
  DATA lv_offset.
  FIND FIRST OCCURRENCE OF |bar| IN |foo| MATCH OFFSET lv_offset.
  WRITE / sy-subrc.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("concat_lines_of", async () => {
    const code = `
  DATA rv_text TYPE string.
  DATA lt_rows TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
  APPEND 'a' TO lt_rows.
  APPEND 'c' TO lt_rows.
  rv_text = concat_lines_of( table = lt_rows
                             sep   = |b| ).
  WRITE rv_text.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("abc");
  });

  it("hex value conversion", async () => {
    const code = `
  DATA hex TYPE x.
  DATA integer TYPE i.
  hex = 'AA'.
  integer = hex.
  ASSERT integer = 170.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("append structure to table", async () => {
    const code = `
TYPES: BEGIN OF ty_bar,
         field TYPE i,
       END OF ty_bar.
TYPES ty_tab TYPE STANDARD TABLE OF ty_bar WITH DEFAULT KEY.
DATA bar TYPE ty_bar.
DATA tab TYPE ty_tab.

bar-field = 1.
APPEND bar TO tab.
bar-field = 2.
APPEND bar TO tab.

LOOP AT tab INTO bar.
  WRITE / bar-field.
ENDLOOP.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("read index 1 of structured table", async () => {
    const code = `
TYPES: BEGIN OF ty_bar,
         field TYPE i,
       END OF ty_bar.
TYPES ty_tab TYPE STANDARD TABLE OF ty_bar WITH DEFAULT KEY.
DATA bar TYPE ty_bar.
DATA tab TYPE ty_tab.

READ TABLE tab INDEX 1 INTO bar.
WRITE sy-subrc.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("javascript keyword uses as identifier", async () => {
    const code = `
data if type i.
if = 1.
write if.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("javascript keyword in string template", async () => {
    const code = `write |foo if bar|.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo if bar");
  });

  it("integer DIV", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 DIV 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("power", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 ** 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("25");
  });

  it("integer MOD", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 MOD 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("condense", async () => {
    const code = `
    DATA foo TYPE string.
    foo = '123 '.
    foo = condense( foo ).
    WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("condense integer", async () => {
    const code = `
    DATA foo TYPE string.
    data bar type i.
    bar = 2.
    foo = bar.
    foo = condense( foo ).
    WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("MODIFY internal table INDEX FROM", async () => {
    const code = `
    DATA result TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA dat LIKE LINE OF result.
    APPEND 2 TO result.
    MODIFY result INDEX 1 FROM 4.
    ASSERT sy-subrc = 0.
    READ TABLE result INDEX 1 INTO dat.
    WRITE dat.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("MODIFY, testing references", async () => {
    const code = `
  DATA integers TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA int TYPE i.
  APPEND 1 TO integers.
  APPEND 2 TO integers.
  int = 3.
  MODIFY integers INDEX 1 FROM int.
  int = 4.
  MODIFY integers INDEX 2 FROM int.
  LOOP AT integers INTO int.
    WRITE / int.
  ENDLOOP.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("3\n4");
  });

  it("int to hex", async () => {
    const code = `
  DATA lv_x TYPE x LENGTH 4.
  lv_x = 12345.
  WRITE lv_x.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("00003039");
  });

  it("hex to int", async () => {
    const code = `
  DATA lv_x TYPE x LENGTH 4.
  data int type i.
  lv_x = '00003039'.
  int = lv_x.
  WRITE int.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("12345");
  });

  it("xstring to int", async () => {
    const code = `
  DATA lv_x TYPE xstring.
  data int type i.
  lv_x = '00003039'.
  int = lv_x.
  WRITE int.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("12345");
  });

  it("simple concat via &&", async () => {
    const code = `WRITE 'foo' && 'bar'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foobar");
  });

  it("concat class constant", async () => {
    const code = `
    CLASS moo DEFINITION.
      PUBLIC SECTION.
        CONSTANTS bar TYPE c VALUE '_'.
    ENDCLASS.
    CLASS moo IMPLEMENTATION.
    ENDCLASS.

    DATA str TYPE string.
    str = 'ABC' && moo=>bar && '123'.
    WRITE str.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("ABC_123");
  });

  it("FIND FIRST OCCURRENCE", async () => {
    const code = `
    FIND FIRST OCCURRENCE OF 'bar' IN 'foobar'.
    ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("DESCRIBE FIELD table", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA type TYPE c LENGTH 1.
  DESCRIBE FIELD tab TYPE type.
  WRITE type.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("h");
  });

  it("SHIFT LEFT", async () => {
    const code = `
  DATA foo TYPE c LENGTH 10.
  foo = '11223355'.
  SHIFT foo LEFT DELETING LEADING '12'.
  WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("3355");
  });

  it("target length and offsets", async () => {
    const code = `
  DATA foo TYPE c LENGTH 10.
  foo = '11223355'.
  foo+5(1) = 'A'.
  WRITE / foo.
  foo(1) = 'B'.
  WRITE / foo.
  foo+3 = 'C'.
  WRITE / foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("11223A55\nB1223A55\nB12C");
  });

  it("GET all da BITs", async () => {
    const code = `
  DATA rv_bitbyte TYPE c LENGTH 8 .
  DATA iv_x TYPE x VALUE '40'.
  GET BIT 1 OF iv_x INTO rv_bitbyte+0(1).
  GET BIT 2 OF iv_x INTO rv_bitbyte+1(1).
  GET BIT 3 OF iv_x INTO rv_bitbyte+2(1).
  GET BIT 4 OF iv_x INTO rv_bitbyte+3(1).
  GET BIT 5 OF iv_x INTO rv_bitbyte+4(1).
  GET BIT 6 OF iv_x INTO rv_bitbyte+5(1).
  GET BIT 7 OF iv_x INTO rv_bitbyte+6(1).
  GET BIT 8 OF iv_x INTO rv_bitbyte+7(1).
  WRITE rv_bitbyte.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("01000000");
  });

  it("GET all da BITs", async () => {
    const code = `
  DATA rv_bitbyte TYPE c LENGTH 8.
  DATA iv_x TYPE x VALUE '40'.
  GET BIT 1 OF iv_x INTO rv_bitbyte+0(1).
  GET BIT 2 OF iv_x INTO rv_bitbyte+1(1).
  GET BIT 3 OF iv_x INTO rv_bitbyte+2(1).
  GET BIT 4 OF iv_x INTO rv_bitbyte+3(1).
  GET BIT 5 OF iv_x INTO rv_bitbyte+4(1).
  GET BIT 6 OF iv_x INTO rv_bitbyte+5(1).
  GET BIT 7 OF iv_x INTO rv_bitbyte+6(1).
  GET BIT 8 OF iv_x INTO rv_bitbyte+7(1).
  WRITE rv_bitbyte.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("01000000");
  });

  it("integer into xstring", async () => {
    const code = `
    DATA bar TYPE xstring.
    bar = 64.
    WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("40");
  });

  it("integer into hex", async () => {
    const code = `
  DATA bar TYPE x LENGTH 1.
  bar = 64.
  WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("40");
  });

  it("xstring, zero", async () => {
    const code = `
  DATA bar TYPE xstring.
  bar = 0.
  WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("00");
  });

  it("xstring, one", async () => {
    const code = `
  DATA bar TYPE xstring.
  bar = 1.
  WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("01");
  });

  it("translate to upper case", async () => {
    const code = `
  DATA foo TYPE string.
  foo = 'abc'.
  TRANSLATE foo TO UPPER CASE.
  WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("ABC");
  });

  it("APPEND INITIAL LINE", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i.
  FIELD-SYMBOLS <fs> LIKE LINE OF tab.
  APPEND INITIAL LINE TO tab ASSIGNING <fs>.
  WRITE <fs>.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("DESCRIBE FIELD", async () => {
    const code = `
  DATA f TYPE c LENGTH 4.
  DATA l TYPE i.
  DESCRIBE FIELD f LENGTH l IN CHARACTER MODE.
  WRITE l.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("APPEND field symbol", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i.
  DATA row LIKE LINE OF tab.
  FIELD-SYMBOLS <fs> LIKE LINE OF tab.
  APPEND INITIAL LINE TO tab ASSIGNING <fs>.
  <fs> = 2.
  READ TABLE tab INDEX 1 INTO row.
  WRITE row.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("back slash", async () => {
    const code = `WRITE '\\'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("\\");
  });

  it("boolc test", async () => {
    const code = `
  DATA rv_yes TYPE abap_bool.
  DATA iv_path TYPE string.
  iv_path = '/'.
  rv_yes = boolc( iv_path = '/' ).
  WRITE rv_yes.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("X");
  });

  it("reverse", async () => {
    const code = `
  DATA str TYPE string.
  str = reverse( 'abc' ).
  WRITE str.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("cba");
  });

  it("FIND REGEX, count and length", async () => {
    const code = `
  DATA lv_cnt TYPE i.
  DATA lv_len TYPE i.
  FIND FIRST OCCURRENCE OF REGEX 'b+c' IN 'abcdbc' MATCH COUNT lv_cnt MATCH LENGTH lv_len.
  WRITE / lv_cnt.
  WRITE / lv_len.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("abap_true", async () => {
    const code = `WRITE abap_true.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("X");
  });

  it("ASSERT strlen gt", async () => {
    const code = `
  DATA lv_len TYPE i.
  lv_len = 4.
  ASSERT strlen( '/dir/subdir' ) > lv_len.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("shift 1 places left", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abc'.
  SHIFT lv_temp BY 1 PLACES LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("bc");
  });

  it("shift up to left, found", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abc/bar'.
  SHIFT lv_temp UP TO '/' LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("/bar");
  });

  it("shift up to left, not found", async () => {
    const code = `
  DATA lv_temp TYPE string.
  lv_temp = 'abcbar'.
  SHIFT lv_temp UP TO '/' LEFT.
  WRITE lv_temp.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("abcbar");
  });

  it("concat vars", async () => {
    const code = `
  DATA lv_temp1 TYPE string.
  lv_temp1 = 'foo'.
  DATA lv_temp2 TYPE string.
  lv_temp2 = 'bar'.
  DATA rv_path TYPE string.
  rv_path = lv_temp1 && lv_temp2.
  WRITE rv_path.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foobar");
  });

  it("concat vars", async () => {
    const code = `
  DATA bar TYPE abap_bool.
  bar = boolc( 'foo' CA 'a' ).
  ASSERT bar = abap_false.
  bar = boolc( 'foo' CA 'abc' ).
  ASSERT bar = abap_false.
  bar = boolc( 'foo' CA 'fo' ).
  ASSERT bar = abap_true.
  bar = boolc( 'foo' CA 'o' ).
  ASSERT bar = abap_true.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("NS comparator", async () => {
    const code = `ASSERT 'foo' NS 'bar'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("BETWEEN comparator", async () => {
    const code = `
  ASSERT 2 BETWEEN 1 AND 5.
  ASSERT 1 BETWEEN 1 AND 5.
  ASSERT 5 BETWEEN 1 AND 5.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("NOT BETWEEN comparator", async () => {
    const code = `
  ASSERT 8 NOT BETWEEN 1 AND 5.
  ASSERT NOT 3 NOT BETWEEN 2 AND 5.
  ASSERT NOT 8 BETWEEN 1 AND 5.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("structured constant", async () => {
    const code = `
CONSTANTS: BEGIN OF lc_msg,
             field1 TYPE c VALUE '1',
             field2 TYPE c VALUE '2',
           END OF lc_msg.
WRITE / lc_msg-field1.
WRITE / lc_msg-field2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("structured constant, is initial", async () => {
    const code = `
CONSTANTS: BEGIN OF lc_msg,
             field1 TYPE c VALUE IS INITIAL,
           END OF lc_msg.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
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
    const f = new Function("abap", js);
    f(abap);
  });

  it("class, testing me->", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    DATA moo TYPE i.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    me->moo = 2.
    WRITE me->moo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_foo.
  CREATE OBJECT bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
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
    const f = new Function("abap", js);
    f(abap);
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
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("call method in super class, no constructor", async () => {
    const code = `
CLASS zcl_super DEFINITION.
  PUBLIC SECTION.
    METHODS method.
ENDCLASS.

CLASS zcl_super IMPLEMENTATION.
  METHOD method.
    WRITE 4.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
ENDCLASS.

CLASS zcl_sub IMPLEMENTATION.
ENDCLASS.

FORM run.
  DATA sub TYPE REF TO zcl_sub.
  CREATE OBJECT sub.
  sub->method( ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("WRITE space", async () => {
    const code = `WRITE space.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("call super in redefined method", async () => {
    const code = `
CLASS zcl_super DEFINITION.
  PUBLIC SECTION.
    METHODS method.
ENDCLASS.

CLASS zcl_super IMPLEMENTATION.
  METHOD method.
    WRITE / 'b'.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
  PUBLIC SECTION.
    METHODS:
      method REDEFINITION.
ENDCLASS.

CLASS zcl_sub IMPLEMENTATION.
  METHOD method.
    WRITE / 'a'.
    super->method( ).
  ENDMETHOD.
ENDCLASS.

FORM run.
  DATA sub TYPE REF TO zcl_sub.
  CREATE OBJECT sub.
  sub->method( ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("a\nb");
  });

  it("check structure is initial", async () => {
    const code = `
TYPES: BEGIN OF bar,
         field TYPE i,
       END OF bar.
DATA bar TYPE bar.
ASSERT bar IS INITIAL.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("check internal table is initial", async () => {
    const code = `
    DATA bar TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    ASSERT bar IS INITIAL.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("write constant from interface", async () => {
    const code = `
    INTERFACE lif_foo.
      CONSTANTS bar TYPE i VALUE 2.
    ENDINTERFACE.

    WRITE lif_foo=>bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("testing initialization of variables in constructor", async () => {
    const code = `
CLASS zcl_super DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
    METHODS constructor.
ENDCLASS.

CLASS zcl_super IMPLEMENTATION.
  METHOD constructor.
    foo = 1.
  ENDMETHOD.
ENDCLASS.

CLASS zcl_sub DEFINITION INHERITING FROM zcl_super.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS zcl_sub IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    WRITE foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA moo TYPE REF TO zcl_sub.
  CREATE OBJECT moo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("clear object reference", async () => {
    const code = `
  interface lif_bar.
  endinterface.
  DATA bar TYPE REF TO lif_bar.
  CLEAR bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CONDENSE converted type", async () => {
    const code = `
  DATA lv_len TYPE i.
  DATA lv_char10 TYPE c LENGTH 10.
  lv_len = 5.
  lv_char10 = lv_len.
  CONDENSE lv_char10.
  WRITE lv_char10.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("CONDENSE int'ed string", async () => {
    const code = `
  DATA lv_char10 TYPE c LENGTH 10.
  lv_char10 = 5.
  CONDENSE lv_char10.
  WRITE lv_char10.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("Value from constant structure in interface", async () => {
    const code = `
INTERFACE lif.
  CONSTANTS: BEGIN OF bar,
               foo TYPE c VALUE 'A',
             END OF bar.
ENDINTERFACE.

WRITE lif=>bar-foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("A");
  });

  it("CONCATENATE SEPARATED BY space", async () => {
    const code = `
  DATA lv_string TYPE string.
  DATA lv_char10 TYPE c LENGTH 10.
  DATA iv_type TYPE c LENGTH 6 VALUE 'commit'.
  lv_char10 = 6.
  CONDENSE lv_char10.
  CONCATENATE iv_type lv_char10 INTO lv_string SEPARATED BY space.
  ASSERT lv_string = 'commit 6'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("xstring offset and length", async () => {
    const code = `
  DATA lv_a TYPE i VALUE 2.
  DATA lv_x TYPE xstring VALUE '0F0F0F'.
  lv_a = lv_a + lv_x+1(1).
  WRITE lv_a.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("17");
  });

  it("FIND REGEX, not found", async () => {
    const code = `
  DATA lv_host TYPE string.
  FIND REGEX 'a' IN '1122' SUBMATCHES lv_host.
  WRITE sy-subrc.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("FIND REGEX SUBMATCHES, found", async () => {
    const code = `
DATA lv_host TYPE string.
FIND REGEX '11(\\w+)22' IN '11abc22' SUBMATCHES lv_host.
ASSERT sy-subrc = 0.
ASSERT lv_host = 'abc'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("FIND REGEX slashes", async () => {
    const code = `
  FIND REGEX '//' IN '11//22'.
  ASSERT sy-subrc = 0.
  FIND REGEX '/' IN '11/22'.
  ASSERT sy-subrc = 0.
  FIND REGEX '/' IN '1122'.
  ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("determine default parameter name", async () => {
    const code = `
CLASS cl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS name
      IMPORTING
        iv_url      TYPE string
        iv_validate TYPE abap_bool DEFAULT abap_false.
ENDCLASS.
CLASS cl IMPLEMENTATION.
  METHOD name.
    WRITE iv_url.
  ENDMETHOD.
ENDCLASS.

FORM bar.
  cl=>name( 'bar' ).
ENDFORM.

PERFORM bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("negative number", async () => {
    const code = `WRITE -1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("-1");
  });

  it("add string values", async () => {
    const code = `
  DATA lv_string1 TYPE string.
  DATA lv_string2 TYPE string.
  lv_string1 = 1.
  lv_string2 = 2.
  ASSERT lv_string1 + lv_string2 = 3.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("charcter type plus integer type", async () => {
    const code = `
  DATA lv_val1 TYPE c LENGTH 1.
  DATA lv_val2 TYPE i.
  lv_val1 = 1.
  lv_val2 = 2.
  ASSERT lv_val1 + lv_val2 = 3.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("write numc field, initial", async () => {
    const code = `
  DATA foo TYPE n LENGTH 4.
  WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0000");
  });

  it("write sy-mandt", async () => {
    const code = `WRITE sy-mandt.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("split 1", async () => {
    const code = `
DATA: lv_major   TYPE c LENGTH 4,
      lv_minor   TYPE c LENGTH 4,
      lv_release TYPE c LENGTH 4.
SPLIT |blah| AT '.' INTO lv_major lv_minor lv_release.
WRITE / lv_major.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("blah");
  });

  it("split 2", async () => {
    const code = `
DATA: lv_major   TYPE c LENGTH 4,
lv_minor   TYPE c LENGTH 4,
lv_release TYPE c LENGTH 4.
SPLIT |blah.boo| AT '.' INTO lv_major lv_minor lv_release.
WRITE / lv_major.
WRITE / lv_minor.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("blah\nboo");
  });

  it("split 3", async () => {
    const code = `
DATA: lv_major   TYPE c LENGTH 4,
lv_minor   TYPE c LENGTH 4,
lv_release TYPE c LENGTH 4.
SPLIT |1.2.3| AT '.' INTO lv_major lv_minor lv_release.
WRITE / lv_major.
WRITE / lv_minor.
WRITE / lv_release.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("numc text value, int", async () => {
    const code = `
  DATA bar TYPE n LENGTH 10.
  bar = '1'.
  WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0000000001");
  });

  it("FIND ALL, MATCH COUNT", async () => {
    const code = `
    DATA lv_count TYPE i.
    FIND ALL OCCURRENCES OF 'a' IN 'aaa' MATCH COUNT lv_count.
    ASSERT lv_count = 3.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("FIND ALL, more submatches", async () => {
    const code = `
    DATA lv_val1 TYPE string.
    DATA lv_val2 TYPE string.
    DATA lv_val3 TYPE string.
    DATA lv_val4 TYPE string.
    FIND REGEX '(\\d+)-(\\d+) (\\w): (\\w+)' IN '5-9 g: ggccggmgn' SUBMATCHES lv_val1 lv_val2 lv_val3 lv_val4.
    WRITE / lv_val1.
    WRITE / lv_val2.
    WRITE / lv_val3.
    WRITE / lv_val4.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("5\n9\ng\nggccggmgn");
  });

  it("FIND ALL, should clear", async () => {
    const code = `
  DATA lv_count TYPE i.
  lv_count = 1.
  FIND ALL OCCURRENCES OF 'a' IN '123' MATCH COUNT lv_count.
  WRITE lv_count.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("EQ, initial string and integer", async () => {
    const code = `DATA low TYPE string.
    DATA int TYPE i.
    ASSERT int EQ low.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("LT, string and integer", async () => {
    const code = `DATA low TYPE string.
    DATA int TYPE i.
    low = |1|.
    ASSERT int LT low.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("BETWEEN 1", async () => {
    const code = `DATA low TYPE string.
    DATA high TYPE string.
    DATA int TYPE i.
    ASSERT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("BETWEEN 2", async () => {
    const code = `DATA low TYPE string.
    DATA high TYPE string.
    DATA int TYPE i.
    low = |1|.
    high = |1|.
    int = 1.
    ASSERT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("BETWEEN 3", async () => {
    const code = `DATA low TYPE string.
    DATA high TYPE string.
    DATA int TYPE i.
    low = |1|.
    high = |1|.
    int = 2.
    ASSERT NOT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("BETWEEN 4", async () => {
    const code = `DATA low TYPE string.
    DATA high TYPE string.
    DATA int TYPE i.
    low = |10|.
    high = |20|.
    int = 2.
    ASSERT NOT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("basic count()", async () => {
    const code = `
  DATA lv_count TYPE i.
  lv_count = count( val = 'password' sub = 's' ).
  WRITE lv_count.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("FIND FIRST occurrence, empty input with MATCH LENGTH", async () => {
    const code = `
  DATA iv_fullpath TYPE string.
  DATA lv_cnt TYPE i.
  DATA lv_len TYPE i.
  FIND FIRST OCCURRENCE OF REGEX '^/(.*/)?' IN iv_fullpath MATCH COUNT lv_cnt MATCH LENGTH lv_len.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("concat via &", async () => {
    const code = `
  DATA mv_input TYPE string.
  mv_input = |hello| & |world|.
  WRITE mv_input.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("helloworld");
  });

  it("WRITE sy-tabix.", async () => {
    const code = `WRITE sy-tabix.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("LOOP, should set sy-tabix", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DO 3 TIMES.
    APPEND 'a' TO tab.
  ENDDO.
  DATA sdf TYPE i.
  LOOP AT tab INTO sdf.
    WRITE / sy-tabix.
  ENDLOOP.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

  it("READ TABLE TRANSPORTING NO FIELDS", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    READ TABLE tab INDEX 1 TRANSPORTING NO FIELDS.
    WRITE sy-subrc.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4");
  });

  it("integers div", async () => {
    const code = `
  DATA lv_int TYPE i.
  lv_int = 5 / 2.
  ASSERT lv_int = 3.
  lv_int = 100 / 99.
  ASSERT lv_int = 1.
  lv_int = 5 / 3.
  ASSERT lv_int = 2.
  lv_int = 5 / 4.
  ASSERT lv_int = 1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("SPLIT empty string, should give empty table", async () => {
    const code = `
  DATA strs TYPE STANDARD TABLE OF string.
  SPLIT || AT |a| INTO TABLE strs.
  ASSERT lines( strs ) = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("SPLIT non-empty string, should give non-empty table", async () => {
    const code = `
    DATA lt_tab TYPE STANDARD TABLE OF string.
    SPLIT |sdfds| AT |AA| INTO TABLE lt_tab.
    ASSERT lines( lt_tab ) = 1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("deep structure, move", async () => {
    const code = `
TYPES: BEGIN OF ty_deep,
         field TYPE string,
         tab   TYPE STANDARD TABLE OF i WITH DEFAULT KEY,
       END OF ty_deep.

DATA data1 TYPE ty_deep.
DATA data2 TYPE ty_deep.
data1 = data2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("basic substring", async () => {
    const code = `ASSERT substring( val = |abc| off = 1 len = 1 ) = |b|.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("READ TABLE WITH KEY", async () => {
    const code = `
TYPES: BEGIN OF ty_structure,
         field TYPE i,
       END OF ty_structure.

DATA tab TYPE STANDARD TABLE OF ty_structure WITH DEFAULT KEY.
DATA line LIKE LINE OF tab.

line-field = 2.
APPEND line TO tab.
line-field = 5.
APPEND line TO tab.
ASSERT lines( tab ) = 2.

CLEAR line.

READ TABLE tab INTO line WITH KEY field = 2.
ASSERT sy-subrc = 0.
ASSERT line-field = 2.

READ TABLE tab INTO line WITH KEY field = 123.
ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("to_upper()", async () => {
    const code = `
  DATA bar TYPE string VALUE 'BAR'.
  ASSERT to_upper( |bar| ) = |BAR|.
  ASSERT to_upper( |bar| ) = bar.
  ASSERT to_upper( bar ) = bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("LOOP AT WHERE", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA line LIKE LINE OF tab.
  DO 5 TIMES.
    APPEND sy-index TO tab.
  ENDDO.
  LOOP AT tab INTO line WHERE table_line <> 3.
    WRITE / line.
  ENDLOOP.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2\n4\n5");
  });

  it("LOOP AT nothing with CONTINUE and EXIT", async () => {
    const code = `
    DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA row LIKE LINE OF tab.
    LOOP AT tab INTO row.
      CONTINUE.
    ENDLOOP.
    LOOP AT tab INTO row.
      EXIT.
    ENDLOOP.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Field symbols mess", async () => {
    const code = `
  DATA act TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA exp TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA index TYPE i.
  FIELD-SYMBOLS <tab1> TYPE INDEX TABLE.
  FIELD-SYMBOLS <row1> TYPE any.
  FIELD-SYMBOLS <tab2> TYPE INDEX TABLE.
  FIELD-SYMBOLS <row2> TYPE any.
  APPEND 1 TO act.
  APPEND 1 TO exp.
  ASSIGN act TO <tab1>.
  ASSIGN exp TO <tab2>.
  DO lines( act ) TIMES.
    index = sy-index.
    READ TABLE <tab1> INDEX index ASSIGNING <row1>.
    ASSERT sy-subrc = 0.
    READ TABLE <tab2> INDEX index ASSIGNING <row2>.
    ASSERT sy-subrc = 0.
    ASSERT <row1> = <row2>.
  ENDDO.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("READ TABLE table_line", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i.
  APPEND 2 TO tab.
  READ TABLE tab WITH KEY table_line = 2 TRANSPORTING NO FIELDS.
  ASSERT sy-subrc = 0.
  READ TABLE tab WITH KEY table_line = 123 TRANSPORTING NO FIELDS.
  ASSERT sy-subrc = 4.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("escape()", async () => {
    const code = `
  CONSTANTS e_html_text TYPE i VALUE 4.
  CONSTANTS e_html_attr TYPE i VALUE 5.
  CONSTANTS e_url TYPE i VALUE 12.
  DATA lv_result TYPE string.
  lv_result = escape( val = |abc123&<>"'| format = e_html_attr ).
  WRITE / lv_result.
  lv_result = escape( val = |abc123&<>"'| format = e_html_text ).
  WRITE / lv_result.
  lv_result = escape( val = |abc123&<>"'| format = e_url ).
  WRITE / lv_result.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal(
      `abc123&amp;&lt;&gt;&quot;&#39;\n` +
      `abc123&amp;&lt;&gt;"'\n` +
      `abc123&%3C%3E%22'`);
  });

  it("FIND RESULTS, 1", async () => {
    const code = `
TYPES: BEGIN OF ty_submatch,
         offset TYPE i,
         length TYPE i,
       END OF ty_submatch.

TYPES: BEGIN OF ty_match,
         line       TYPE i,
         offset     TYPE i,
         length     TYPE i,
         submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
       END OF ty_match.

DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
DATA ls_match LIKE LINE OF lt_matches.

FIND REGEX |bar| IN |hello bar world| RESULTS lt_matches.
ASSERT lines( lt_matches ) = 1.
READ TABLE lt_matches INDEX 1 INTO ls_match.
ASSERT ls_match-line = 0.
ASSERT ls_match-offset = 6.
ASSERT ls_match-length = 3.
ASSERT lines( ls_match-submatches ) = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("FIND RESULTS, 2", async () => {
    const code = `
    TYPES: BEGIN OF ty_submatch,
    offset TYPE i,
    length TYPE i,
  END OF ty_submatch.

TYPES: BEGIN OF ty_match,
    line       TYPE i,
    offset     TYPE i,
    length     TYPE i,
    submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
  END OF ty_match.

DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
DATA ls_match LIKE LINE OF lt_matches.
DATA ls_submatch LIKE LINE OF ls_match-submatches.

FIND REGEX |(bar)| IN |hello bar bar world| RESULTS lt_matches.
ASSERT lines( lt_matches ) = 1.

FIND ALL OCCURRENCES OF REGEX |(bar)| IN |hello bar bar world| RESULTS lt_matches.
ASSERT lines( lt_matches ) = 2.
READ TABLE lt_matches INDEX 1 INTO ls_match.
ASSERT lines( ls_match-submatches ) = 1.
READ TABLE ls_match-submatches INDEX 1 INTO ls_submatch.
ASSERT ls_submatch-offset = 6.
ASSERT ls_submatch-length = 3.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("FIND RESULTS, 3", async () => {
    const code = `
    TYPES: BEGIN OF ty_submatch,
    offset TYPE i,
    length TYPE i,
  END OF ty_submatch.

TYPES: BEGIN OF ty_match,
    line       TYPE i,
    offset     TYPE i,
    length     TYPE i,
    submatches TYPE STANDARD TABLE OF ty_submatch WITH DEFAULT KEY,
  END OF ty_match.

DATA lt_matches TYPE STANDARD TABLE OF ty_match WITH DEFAULT KEY.
DATA find TYPE string.
find = 'aa'.
DATA in TYPE string.
in = 'fooaabar'.
FIND ALL OCCURRENCES OF REGEX find IN in RESULTS lt_matches.
ASSERT lines( lt_matches ) = 1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Field offsets and lengths with structures, source", async () => {
    const code = `
  TYPES:
    BEGIN OF ty_struct,
      num TYPE i,
    END OF ty_struct.

  DATA number TYPE i.
  DATA struct TYPE ty_struct.
  DATA test_string TYPE string.

  test_string = '0123456789'.
  number = 3.
  struct-num = 4.

  WRITE / test_string+1(2).
  WRITE / test_string+2(number).
  WRITE / test_string+3(struct-num).

  WRITE / test_string+number(2).
  WRITE / test_string+number(number).
  WRITE / test_string+number(struct-num).

  WRITE / test_string+struct-num(2).
  WRITE / test_string+struct-num(number).
  WRITE / test_string+struct-num(struct-num).

  sy-index = 8. " This should probably not be allowed... :)
  WRITE / test_string+sy-index(1).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("12\n234\n3456\n34\n345\n3456\n45\n456\n4567\n8");
  });

  it("Field offsets and lengths with structures, target", async () => {
    const code = `
  TYPES:
    BEGIN OF ty_struct,
      num TYPE i,
    END OF ty_struct.

  DATA number TYPE i.
  DATA struct TYPE ty_struct.
  DATA test_string TYPE c LENGTH 100.

  test_string = '0123456789012'.
  number = 3.
  struct-num = 4.

  test_string+1(2) = '##########'.
  test_string+4(number) = '##########'.
  test_string+8(struct-num) = '##########'.
  WRITE / test_string.

  test_string = '0123456789012'.
  test_string+number(struct-num) = '!!!!!!!!!!'.
  WRITE / test_string.
  test_string+number(number) = '$$$$$$$$$$'.
  WRITE / test_string.
  test_string+number(2) = '££££££££££'.
  WRITE / test_string.

  test_string = '0123456789012'.
  test_string+struct-num(struct-num) = 'PPPPPPPPPP'.
  WRITE / test_string.
  test_string+struct-num(number) = 'AAAAAAAAAA'.
  WRITE / test_string.
  test_string+struct-num(2) = 'ABABABABAB'.
  WRITE / test_string.

  sy-index = 4. " This should probably not be allowed... :)
  WRITE / test_string+sy-index(sy-index).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("0##3###7####2\n012!!!!789012\n012$$$!789012\n012££$!789012\n0123PPPP89012\n0123AAAP89012\n0123ABAP89012\nABAP");
  });

  it("getOffset for field-symbols", async () => {
    const code = `
    DATA lv_row TYPE string.
    FIELD-SYMBOLS <lv_row> TYPE string.
    lv_row = |foobar|.
    ASSIGN lv_row TO <lv_row>.
    <lv_row> = <lv_row>(3).
    WRITE <lv_row>.
    <lv_row> = |foobar|.
    <lv_row> = <lv_row>+3(3).
    WRITE / <lv_row>.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo\nbar");
  });

  it("compare using CO operator", async () => {
    const code = `
    DATA lv_val TYPE abap_bool.
    lv_val = boolc( 'hello' CO 'ab' ).
    ASSERT lv_val = abap_false.
    lv_val = boolc( 'aabbaa' CO 'ab' ).
    ASSERT lv_val = abap_true.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("IS BOUND", async () => {
    const code = `
  CLASS lcl_bar DEFINITION.
  ENDCLASS.
  CLASS lcl_bar IMPLEMENTATION.
  ENDCLASS.
  DATA foo TYPE REF TO lcl_bar.
  ASSERT NOT foo IS BOUND.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
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
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo");
  });

  it("test refs are identical", async () => {
    const code = `
  CLASS lcl_foo DEFINITION.
  ENDCLASS.
  CLASS lcl_foo IMPLEMENTATION.
  ENDCLASS.
  DATA ref1 TYPE REF TO lcl_foo.
  DATA ref2 TYPE REF TO lcl_foo.
  CREATE OBJECT ref1.
  ref2 = ref1.
  ASSERT ref1 = ref2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("delete internal tab with object references", async () => {
    const code = `
  CLASS lcl_foo DEFINITION.
  ENDCLASS.
  CLASS lcl_foo IMPLEMENTATION.
  ENDCLASS.

  DATA tab TYPE STANDARD TABLE OF REF TO lcl_foo.
  DATA ref1 TYPE REF TO lcl_foo.
  DATA ref2 TYPE REF TO lcl_foo.
  CREATE OBJECT ref1.
  APPEND ref1 TO tab.
  CREATE OBJECT ref2.
  APPEND ref2 TO tab.

  DELETE tab INDEX 2.
  ASSERT sy-subrc = 0.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("calculation inside string template", async () => {
    const code = `
  DATA int_1 TYPE i VALUE 4.
  DATA int_2 TYPE i VALUE 8.
  WRITE |{ int_1 } * { int_2 } = { int_1 * int_2 }|.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("4 * 8 = 32");
  });

  it("IS ASSIGNED", async () => {
    const code = `
  FIELD-SYMBOLS <bar> TYPE i.
  ASSERT <bar> IS NOT ASSIGNED.
  ASSERT NOT <bar> IS ASSIGNED.
  ASSIGN 2 TO <bar>.
  ASSERT <bar> IS ASSIGNED.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

});
