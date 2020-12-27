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

  it("Console tracks output", async () => {
    const code = `WRITE 'foo'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("foo");
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

  it("SET BIT", async () => {
    const code = `
    DATA hex TYPE x LENGTH 1.
    DO 8 TIMES.
      IF sy-index > 4.
        CLEAR hex.
      ENDIF.
      SET BIT sy-index OF hex.
      WRITE / hex.
    ENDDO.

    DATA xstr TYPE xstring.
    xstr = 'F2420FA000'.
    SET BIT 30 OF xstr.
    SET BIT 25 OF xstr TO 0.
    SET BIT 35 OF xstr TO 1.
    WRITE / xstr.

    xstr = '03FF'.
    SET BIT 9 OF xstr TO 0.
    WRITE / xstr.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("80\nC0\nE0\nF0\n08\n04\n02\n01\nF2420F2420\n037F");
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
  ENDDO.

  DATA lv_foo TYPE i VALUE 1.
  DO lc_bar + lv_foo TIMES. " 2+1=3
    WRITE / 'foo'.
    lv_foo = 7.
  ENDDO.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("bar\nfoo\nfoo\nfoo");
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
  str = |   fo  o b   ar |.
  ASSERT str = |   fo  o b   ar |.
  CONDENSE str.
  ASSERT str = |fo o b ar|.
  CONDENSE str NO-GAPS.
  ASSERT str = |foobar|.`;

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

  it("condense", async () => {
    const code = `
    DATA foo TYPE string.
    foo = '12  3 '.
    foo = condense( foo ).
    WRITE foo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("12 3");
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

  it("xstring, one + C", async () => {
    const code = `
  DATA bar TYPE xstring.
  bar = 1.
  WRITE bar.
  bar = 'C'.
  WRITE / bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("01\nC0");
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

  it("to_lower()", async () => {
    const code = `ASSERT to_lower( 'ABC' ) = 'abc'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CREATE OBJECT with dashes/structure", async () => {
    const code = `
  CLASS lcl_bar DEFINITION.
  ENDCLASS.
  CLASS lcl_bar IMPLEMENTATION.
  ENDCLASS.
  TYPES: BEGIN OF ty_structure,
           field TYPE REF TO lcl_bar,
         END OF ty_structure.
  FORM moo.
    DATA ls_structure TYPE ty_structure.
    CREATE OBJECT ls_structure-field.
  ENDFORM.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("escaping constant strings, 1", async () => {
    const code = `WRITE ''''.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("'");
  });

  it("escaping constant strings, 2", async () => {
    const code = `WRITE 'bar''moo''boo'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("bar'moo'boo");
  });

  it("class constructor", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD class_constructor.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

FORM bar.
  DATA lo_bar TYPE REF TO lcl_bar.
  CREATE OBJECT lo_bar.
ENDFORM.

START-OF-SELECTION.
  PERFORM bar.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("is initial, with clike input", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS: initial IMPORTING iv_text TYPE clike.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD initial.
    ASSERT NOT iv_text IS INITIAL.
  ENDMETHOD.
ENDCLASS.

FORM moo.
  DATA lo_bar TYPE REF TO lcl_bar.
  CREATE OBJECT lo_bar.
  lo_bar->initial( 'moo' ).
ENDFORM.

START-OF-SELECTION.
  PERFORM moo.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("Structure equals", async () => {
    const code = `
TYPES: BEGIN OF ty_type,
         field TYPE i,
       END OF ty_type.
DATA data1 TYPE ty_type.
DATA data2 TYPE ty_type.
ASSERT data1 = data2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it.skip("internal table equals", async () => {
    const code = `
  DATA data1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA data2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  ASSERT data1 = data2.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("add character field", async () => {
    const code = `
    DATA int TYPE i.
    int = '5' + 3.
    ASSERT int = 8.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("DESCRIBE with field symbol", async () => {
    const code = `
    DATA lv_length TYPE i.
    DATA bar TYPE c LENGTH 10.
    FIELD-SYMBOLS <line> TYPE any.
    ASSIGN bar TO <line>.
    DESCRIBE FIELD <line> LENGTH lv_length IN CHARACTER MODE.
    WRITE lv_length.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("MESSAGE INTO", async () => {
    const code = `
    DATA lv_text TYPE string.
    MESSAGE e001(00) WITH 'foo' 'bar' INTO lv_text.
    WRITE / sy-msgid.
    WRITE / sy-msgno.
    WRITE / lv_text.`;

    await abap.initDB(`
    CREATE TABLE t100 (sprsl NCHAR(1), arbgb NCHAR(20), msgnr NCHAR(3), text NCHAR(73), PRIMARY KEY(sprsl,arbgb,msgnr));
    INSERT INTO t100 VALUES ('E', '00', '001', '&1&2&3&4');`);

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("00\n001\nfoobar");
  });

  it("MESSAGE fallback, no database initialized", async () => {
    const code = `
    DATA lv_text TYPE string.
    MESSAGE e123(abc) WITH 'foo' 'bar' INTO lv_text.
    WRITE / sy-msgid.
    WRITE / sy-msgno.
    WRITE / lv_text.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("ABC\n123\nABC:123 foo bar");
  });

  it("Method with IMPORTING default value", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: moo IMPORTING bar LIKE sy-msgid DEFAULT sy-msgid.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD moo.
    WRITE bar.
  ENDMETHOD.
ENDCLASS.
FORM form.
  sy-msgid = '123'.
  lcl_bar=>moo( ).
ENDFORM.
START-OF-SELECTION.
  PERFORM form.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("GET TIME", async () => {
    const code = `
    GET TIME.
    WRITE / sy-datlo.
    WRITE / sy-datum.
    WRITE / sy-timlo.
    WRITE / sy-uzeit.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    const result = abap.console.get();
    expect(result).to.not.equal("");
  });

  it("Date and time initial values", async () => {
    const code = `
  DATA date TYPE d.
  DATA time TYPE t.
  WRITE / date.
  WRITE / time.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("00000000\n000000");
  });

  it("basic repeat()", async () => {
    const code = `ASSERT repeat( val = 'a' occ = 2 ) = 'aa'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("basic shift_left()", async () => {
    const code = "ASSERT shift_left( val = 'aabbcc' sub = `a` ) = 'bbcc'.";
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("nested calls to builtins", async () => {
    const code = `
    DATA lv_line TYPE string.
    lv_line = to_upper( shift_left( val = 'aabb' sub = 'a' ) ).
    ASSERT lv_line = 'BB'.`;
    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("field symbol eq", async () => {
    const code = `
    DATA tab1 TYPE STANDARD TABLE OF c.
    DATA tab2 TYPE STANDARD TABLE OF c.
    FIELD-SYMBOLS <tab1> LIKE LINE OF tab1.
    FIELD-SYMBOLS <tab2> LIKE LINE OF tab2.
    APPEND 'a' TO tab1.
    APPEND 'a' TO tab2.
    READ TABLE tab1 INDEX 1 ASSIGNING <tab1>.
    READ TABLE tab2 INDEX 1 ASSIGNING <tab2>.
    ASSERT <tab1> = <tab2>.
    ASSERT <tab1> = 'a'.
    ASSERT 'a' = <tab2>.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("field symbol eq, structure", async () => {
    const code = `
TYPES: BEGIN OF ty_stru,
         foo TYPE string,
       END OF ty_stru.
DATA tab1 TYPE STANDARD TABLE OF ty_stru.
DATA tab2 TYPE STANDARD TABLE OF ty_stru.
DATA stru TYPE ty_stru.
FIELD-SYMBOLS <tab1> LIKE LINE OF tab1.
FIELD-SYMBOLS <tab2> LIKE LINE OF tab2.
stru-foo = 'abc'.
APPEND stru TO tab1.
APPEND stru TO tab2.
READ TABLE tab1 INDEX 1 ASSIGNING <tab1>.
READ TABLE tab2 INDEX 1 ASSIGNING <tab2>.
ASSERT <tab1> = <tab2>.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("more DESCRIBE", async () => {
    const code = `
    DATA lv_type TYPE c LENGTH 1.
    DESCRIBE FIELD lv_type TYPE lv_type.
    WRITE / lv_type.

    DATA lv_string TYPE string.
    DESCRIBE FIELD lv_string TYPE lv_type.
    WRITE / lv_type.

    DATA lt_tab TYPE STANDARD TABLE OF string.
    DESCRIBE FIELD lt_tab TYPE lv_type.
    WRITE / lv_type.

    DATA ref TYPE REF TO object.
    DESCRIBE FIELD ref TYPE lv_type.
    WRITE / lv_type.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("C\ng\nh\nr");
  });

  it("DESCRIBE, direct character string", async () => {
    const code = `
  DATA lv_type TYPE c LENGTH 1.
  DESCRIBE FIELD 'moo' TYPE lv_type.
  ASSERT lv_type = 'C'.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("READ TABLE INDEX not found", async () => {
    const code = `
  DATA lt_segments TYPE STANDARD TABLE OF string.
  DATA lv_segment TYPE string.
  lv_segment = 'abc'.
  READ TABLE lt_segments INTO lv_segment INDEX 2.
  ASSERT sy-subrc = 4.
  ASSERT lv_segment = 'abc'.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
  });

  it("CALL METHOD", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS name.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD name.
    WRITE / 'hello'.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  PERFORM bar.
FORM bar.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  CALL METHOD bar->name( ).
  CALL METHOD bar->name.
ENDFORM.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("hello\nhello");
  });

  it.skip("CALL METHOD with EXPORTING", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS name IMPORTING foo TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD name.
    WRITE / foo.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  PERFORM bar.
FORM bar.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  CALL METHOD bar->name( 1 ).
  CALL METHOD bar->name( foo = 2 ).
  CALL METHOD bar->name EXPORTING foo = 3.
ENDFORM.`;

    const js = await run(code);
    const f = new Function("abap", js);
    f(abap);
    expect(abap.console.get()).to.equal("1\n2\n3");
  });

});