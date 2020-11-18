import {expect} from "chai";
import {Transpiler} from "../packages/transpiler/src/";
import * as abap from "../packages/runtime/src/";

async function run(contents: string) {
  const res = await new Transpiler().run([{filename: "zfoobar.prog.abap", contents}]);
  abap.Console.clear();
  return "global.abap = abap;\n" + res.objects[0].js.contents;
}

describe("Running Examples", () => {

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

  it.skip("Character field semantics", async () => {
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
    expect(abap.Console.get()).to.equal("foo");
  });

  it("Offset +1", async () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar+1.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("bc");
  });

  it("Length (1)", async () => {
    const code = `
      DATA: bar TYPE string.
      bar = 'abc'.
      WRITE bar(1).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("a");
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
    expect(abap.Console.get()).to.equal("1\n2");
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
    expect(abap.Console.get()).to.equal("2\n2");
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
    expect(abap.Console.get()).to.equal("foo");
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
    expect(abap.Console.get()).to.equal("foo");
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
    expect(abap.Console.get()).to.equal("foo");
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
    expect(abap.Console.get()).to.equal("0");
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
    expect(abap.Console.get()).to.equal("2");
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
    expect(abap.Console.get()).to.equal("AA");
  });

  it("basic strlen", async () => {
    const code = `
    DATA foo TYPE string.
    foo = '123'.
    WRITE strlen( foo ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("3");
  });

  it("basic xstrlen", async () => {
    const code = `
    DATA foo TYPE xstring.
    foo = 'AA'.
    WRITE xstrlen( foo ).`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("1");
  });

  it("xstring constant", async () => {
    const code = `
    CONSTANTS lc_raw TYPE xstring VALUE '48656C6C6F20576F726C64210D0A'.
    WRITE lc_raw.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("48656C6C6F20576F726C64210D0A");
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
    expect(abap.Console.get()).to.equal("yes");
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
    expect(abap.Console.get()).to.equal("no");
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
    expect(abap.Console.get()).to.equal("10101011\n00000001");
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
    expect(abap.Console.get()).to.equal("1");
  });

  it("basic minus", async () => {
    const code = `
  DATA foo TYPE i.
  foo = 5 - 2.
  WRITE foo.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("3");
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
    expect(abap.Console.get()).to.equal("01\n14\nAA\n12");
  });

  it("convert type1", async () => {
    const code = `
  DATA i TYPE i.
  i = '1'.
  WRITE i.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("1");
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
    expect(abap.Console.get()).to.equal("5");
  });

  it("convert type3", async () => {
    const code = `
  DATA foo TYPE c LENGTH 1.
  foo = 'AB'.
  WRITE foo.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("A");
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
    expect(abap.Console.get()).to.equal("3456\n12");
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
    expect(abap.Console.get()).to.equal("0");
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
    expect(abap.Console.get()).to.equal("10");
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
    expect(abap.Console.get()).to.equal("10");
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
    expect(abap.Console.get()).to.equal("345\n12\n345\n12");
  });

  it("ASSERT sy-subrc = 0.", async () => {
    const code = `ASSERT sy-subrc = 0.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("");
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
    expect(abap.Console.get()).to.equal("0");
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
    expect(abap.Console.get()).to.equal("");
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
    expect(abap.Console.get()).to.equal("bar");
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
    expect(abap.Console.get()).to.equal("2");
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
    expect(abap.Console.get()).to.equal("hello");
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
    expect(abap.Console.get()).to.equal("A");
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
    expect(abap.Console.get()).to.equal("hello");
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
    expect(abap.Console.get()).to.equal("helloabc");
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
    expect(abap.Console.get()).to.equal("2");
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
    expect(abap.Console.get()).to.equal("0\n3");
  });

  it("FIND FIRST OCCURRENCE, not found", async () => {
    const code = `
  DATA lv_offset.
  FIND FIRST OCCURRENCE OF |bar| IN |foo| MATCH OFFSET lv_offset.
  WRITE / sy-subrc.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("4");
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
    expect(abap.Console.get()).to.equal("abc");
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
    expect(abap.Console.get()).to.equal("1\n2");
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
    expect(abap.Console.get()).to.equal("4");
  });

  it("javascript keyword uses as identifier", async () => {
    const code = `
data if type i.
if = 1.
write if.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("1");
  });

  it("javascript keyword in string template", async () => {
    const code = `write |foo if bar|.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("foo if bar");
  });

  it("integer DIV", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 DIV 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("2");
  });

  it("power", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 ** 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("25");
  });

  it("integer MOD", async () => {
    const code = `
      DATA foo TYPE i.
      foo = 5 MOD 2.
      WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("1");
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
    expect(abap.Console.get()).to.equal("123");
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
    expect(abap.Console.get()).to.equal("2");
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
    expect(abap.Console.get()).to.equal("4");
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
    expect(abap.Console.get()).to.equal("3\n4");
  });

  it("int to hex", async () => {
    const code = `
  DATA lv_x TYPE x LENGTH 4.
  lv_x = 12345.
  WRITE lv_x.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("00003039");
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
    expect(abap.Console.get()).to.equal("12345");
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
    expect(abap.Console.get()).to.equal("12345");
  });

  it("simple concat via &&", async () => {
    const code = `WRITE 'foo' && 'bar'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("foobar");
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
    expect(abap.Console.get()).to.equal("ABC_123");
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
    expect(abap.Console.get()).to.equal("h");
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
    expect(abap.Console.get()).to.equal("3355");
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
    expect(abap.Console.get()).to.equal("11223A55\nB1223A55\nB12C");
  });

  it.skip("GET all da BITs", async () => {
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
    console.dir(js);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("01000000");
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
    expect(abap.Console.get()).to.equal("01000000");
  });

  it("integer into xstring", async () => {
    const code = `
    DATA bar TYPE xstring.
    bar = 64.
    WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("40");
  });

  it("integer into hex", async () => {
    const code = `
  DATA bar TYPE x LENGTH 1.
  bar = 64.
  WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("40");
  });

  it("xstring, zero", async () => {
    const code = `
  DATA bar TYPE xstring.
  bar = 0.
  WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("00");
  });

  it("xstring, one", async () => {
    const code = `
  DATA bar TYPE xstring.
  bar = 1.
  WRITE bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("01");
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
    expect(abap.Console.get()).to.equal("ABC");
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
    expect(abap.Console.get()).to.equal("0");
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
    expect(abap.Console.get()).to.equal("4");
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
    expect(abap.Console.get()).to.equal("2");
  });

  it("back slash", async () => {
    const code = `WRITE '\\'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("\\");
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
    expect(abap.Console.get()).to.equal("X");
  });

  it("reverse", async () => {
    const code = `
  DATA str TYPE string.
  str = reverse( 'abc' ).
  WRITE str.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("cba");
  });

  it("FIND REGEX", async () => {
    const code = `
  DATA lv_cnt TYPE i.
  DATA lv_len TYPE i.
  FIND FIRST OCCURRENCE OF REGEX 'b+c' IN 'abcdbc' MATCH COUNT lv_cnt MATCH LENGTH lv_len.
  WRITE / lv_cnt.
  WRITE / lv_len.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("1\n2");
  });

  it("abap_true", async () => {
    const code = `WRITE abap_true.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.Console.get()).to.equal("X");
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
    expect(abap.Console.get()).to.equal("bc");
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
    expect(abap.Console.get()).to.equal("/bar");
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
    expect(abap.Console.get()).to.equal("abcbar");
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
    expect(abap.Console.get()).to.equal("foobar");
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
    expect(abap.Console.get()).to.equal("1\n2");
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
    expect(abap.Console.get()).to.equal("2");
  });

});