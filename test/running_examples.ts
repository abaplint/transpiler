import {expect} from "chai";
import {ABAP} from "../packages/runtime/src/";
import {AsyncFunction, runFiles} from "./_utils";

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

    const f = new AsyncFunction("abap", js);
    const res = await f(abap);

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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Console tracks output", async () => {
    const code = `WRITE 'foo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo");
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("sy-index", async () => {
    const code = `DO 1 TIMES.
    ASSERT sy-index = 1.
    ENDDO.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("basic xstring", async () => {
    const code = `
    DATA foo TYPE xstring.
    foo = 'AA'.
    WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("AA");
  });

  it("xstring constant", async () => {
    const code = `
    CONSTANTS lc_raw TYPE xstring VALUE '48656C6C6F20576F726C64210D0A'.
    WRITE lc_raw.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("48656C6C6F20576F726C64210D0A");
  });

  it("basic minus", async () => {
    const code = `
  DATA foo TYPE i.
  foo = 5 - 2.
  WRITE foo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("01\n14\nAA\n12");
  });

  it("convert type1", async () => {
    const code = `
  DATA i TYPE i.
  i = '1'.
  WRITE i.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("convert type3", async () => {
    const code = `
  DATA foo TYPE c LENGTH 1.
  foo = 'AB'.
  WRITE foo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("A");
  });

  it("ASSERT sy-subrc = 0.", async () => {
    const code = `ASSERT sy-subrc = 0.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar\nfoo\nfoo\nfoo");
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hello");
  });

  it("javascript keyword uses as identifier", async () => {
    const code = `
data if type i.
if = 1.
write if.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1");
  });

  it("javascript keyword in string template", async () => {
    const code = `write |foo if bar|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo if bar");
  });

  it("int to hex", async () => {
    const code = `
  DATA lv_x TYPE x LENGTH 4.
  lv_x = 12345.
  WRITE lv_x.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12345");
  });

  it("simple concat via &&", async () => {
    const code = `WRITE 'foo' && 'bar'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foobar");
  });

  it("simple concat via &&, whitespace end", async () => {
    const code = `DATA lv_html TYPE string.
    lv_html = 'foo  ' && 'bar'.
    WRITE lv_html.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("ABC_123");
  });

  it("integer into xstring", async () => {
    const code = `
    DATA bar TYPE xstring.
    bar = 64.
    WRITE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("40");
  });

  it("integer into hex", async () => {
    const code = `
  DATA bar TYPE x LENGTH 1.
  bar = 64.
  WRITE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("40");
  });

  it("xstring, zero", async () => {
    const code = `
  DATA bar TYPE xstring.
  bar = 0.
  WRITE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("01\nC0");
  });

  it("back slash", async () => {
    const code = `WRITE '\\'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("\\");
  });

  it("abap_true", async () => {
    const code = `WRITE abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("X");
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foobar");
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("structured constant, is initial", async () => {
    const code = `
CONSTANTS: BEGIN OF lc_msg,
             field1 TYPE c VALUE IS INITIAL,
           END OF lc_msg.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("WRITE space", async () => {
    const code = `WRITE space.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("negative number", async () => {
    const code = `WRITE -1.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("charcter type plus integer type", async () => {
    const code = `
  DATA lv_val1 TYPE c LENGTH 1.
  DATA lv_val2 TYPE i.
  lv_val1 = 1.
  lv_val2 = 2.
  ASSERT lv_val1 + lv_val2 = 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("write numc field, initial", async () => {
    const code = `
  DATA foo TYPE n LENGTH 4.
  WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000");
  });

  it("write sy-mandt", async () => {
    const code = `WRITE sy-mandt.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("123");
  });

  it("numc text value, int", async () => {
    const code = `
  DATA bar TYPE n LENGTH 10.
  bar = '1'.
  WRITE bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0000000001");
  });

  it("concat via &", async () => {
    const code = `
  DATA mv_input TYPE string.
  mv_input = |hello| & |world|.
  WRITE mv_input.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("helloworld");
  });

  it("WRITE sy-tabix.", async () => {
    const code = `WRITE sy-tabix.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("calculation inside string template", async () => {
    const code = `
  DATA int_1 TYPE i VALUE 4.
  DATA int_2 TYPE i VALUE 8.
  WRITE |{ int_1 } * { int_2 } = { int_1 * int_2 }|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("4 * 8 = 32");
  });

  it("escaping constant strings, 1", async () => {
    const code = `WRITE ''''.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("'");
  });

  it("escaping constant strings, 2", async () => {
    const code = `WRITE 'bar''moo''boo'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar'moo'boo");
  });

  it("add character field", async () => {
    const code = `
    DATA int TYPE i.
    int = '5' + 3.
    ASSERT int = 8.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("escape constants", async () => {
    const code = `
  CONSTANTS const TYPE string VALUE '\\'.
  DATA bar TYPE string.
  bar = '\\'.
  ASSERT const = bar.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("set structure with basic varialbe", async () => {
    const code = `
DATA: BEGIN OF ls_msg,
        a1 TYPE c LENGTH 1,
        a2 TYPE c LENGTH 2,
      END OF ls_msg.
ls_msg = 'abc'.
WRITE ls_msg.
ls_msg = ''.
ls_msg = 'a'.
ls_msg = 'abcfdsfdsfds'.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("method and attribute chaining", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    DATA absolute_name TYPE string.
ENDCLASS.
CLASS lcl_foo IMPLEMENTATION.
ENDCLASS.

CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS get_referenced_type RETURNING VALUE(ret) TYPE REF TO lcl_foo.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD get_referenced_type.
    CREATE OBJECT ret.
    ret->absolute_name = 'foobar'.
  ENDMETHOD.
ENDCLASS.

FORM run.
  DATA lo_bar TYPE REF TO lcl_bar.
  CREATE OBJECT lo_bar.
  WRITE lo_bar->get_referenced_type( )->absolute_name.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foobar");
  });

  it("basic data reference", async () => {
    const code = `
    DATA ref TYPE REF TO i.
    ASSERT ref IS INITIAL.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("data reference, append initial", async () => {
    const code = `
  DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA row LIKE LINE OF tab.
  DATA ref TYPE REF TO i.
  APPEND INITIAL LINE TO tab REFERENCE INTO ref.
  ref->* = 2.
  LOOP AT tab INTO row.
    WRITE row.
  ENDLOOP.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("data reference, READ TABLE REFERENCE INTO", async () => {
    const code = `
TYPES: BEGIN OF ty_struc,
         name TYPE string,
       END OF ty_struc.
DATA tab TYPE STANDARD TABLE OF ty_struc WITH DEFAULT KEY.
DATA row LIKE LINE OF tab.
DATA ref LIKE REF TO row.
row-name = 'bar'.
APPEND row TO tab.
READ TABLE tab REFERENCE INTO ref WITH KEY name = 'bar'.
WRITE ref->name.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("set and write numc", async () => {
    const code = `
  DATA foo TYPE n LENGTH 2.
  DO 2 TIMES.
    foo = sy-index.
    WRITE / foo.
  ENDDO.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("01\n02");
  });

  it("numc, exeeed length with number", async () => {
    const code = `
  DATA foo TYPE n LENGTH 2.
  foo = 123.
  WRITE foo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("23");
  });

  it("numc, exceed length with string", async () => {
    const code = `
  DATA foo TYPE n LENGTH 2.
  foo = '123'.
  WRITE foo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("23");
  });

  it("something with references, 1", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA num TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
ENDCLASS.

FORM bar.
  DATA tab TYPE STANDARD TABLE OF ref to lcl_bar WITH DEFAULT KEY.
  DATA row LIKE LINE OF tab.
  DO 2 TIMES.
    CLEAR row.
    CREATE OBJECT row.
    row->num = sy-index.
    APPEND row TO tab.
  ENDDO.
  LOOP AT tab INTO row.
    WRITE / row->num.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM bar.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("something with references, 2", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA num TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
ENDCLASS.

FORM bar.
  TYPES: BEGIN OF ty_structure,
           field TYPE REF TO lcl_bar,
         END OF ty_structure.
  DATA tab TYPE STANDARD TABLE OF ty_structure WITH DEFAULT KEY.
  DATA row LIKE LINE OF tab.
  DO 2 TIMES.
    CLEAR row.
    CREATE OBJECT row-field.
    row-field->num = sy-index.
    APPEND row TO tab.
  ENDDO.
  LOOP AT tab INTO row.
    WRITE / row-field->num.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM bar.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("method call, chained structure field", async () => {
    const code = `
CLASS lcl_chaining DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_structure,
             val TYPE i,
           END OF ty_structure.
    CLASS-METHODS run
      RETURNING VALUE(struc) TYPE ty_structure.
ENDCLASS.

CLASS lcl_chaining IMPLEMENTATION.
  METHOD run.
    struc-val = 12.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  WRITE lcl_chaining=>run( )-val.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12");
  });

  it("method call, chained structure field, double", async () => {
    const code = `
CLASS lcl_chaining DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_structure,
             BEGIN OF bar,
               val TYPE i,
             END OF bar,
           END OF ty_structure.
    CLASS-METHODS run
      RETURNING VALUE(struc) TYPE ty_structure.
ENDCLASS.

CLASS lcl_chaining IMPLEMENTATION.
  METHOD run.
    struc-bar-val = 12.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  WRITE lcl_chaining=>run( )-bar-val.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("12");
  });

  it("access constant via instance arrow", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CONSTANTS field TYPE string VALUE 'foo'.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA ref TYPE REF TO lcl_bar.
  CREATE OBJECT ref.
  WRITE / ref->field.
  WRITE / lcl_bar=>field.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("foo\nfoo");
  });

  it("set static attribute in other class", async () => {
    const code = `
CLASS lcl_container DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA bar TYPE i.
ENDCLASS.

CLASS lcl_container IMPLEMENTATION.
ENDCLASS.

CLASS lcl_logic DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS m1.
ENDCLASS.

CLASS lcl_logic IMPLEMENTATION.
  METHOD m1.
    lcl_container=>bar = 2.
    WRITE lcl_container=>bar.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl_logic=>m1( ).`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("constructor parameter name UPPER case", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    METHODS constructor IMPORTING moo TYPE i.
    DATA foo TYPE i.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD constructor.
    foo = moo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar EXPORTING MOO = 2.
  WRITE bar->foo.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

});