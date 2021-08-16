import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Comparison", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no");
  });

  it("ASSERT obj ref is initial", async () => {
    const code = `
      INTERFACE lif_bar.
      ENDINTERFACE.
      DATA ref TYPE REF TO lif_bar.
      ASSERT ref IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("NOT INITIAL", async () => {
    const code = `
      DATA str TYPE string.
      str = 'abc'.
      ASSERT NOT str IS INITIAL.
      str = ''.
      ASSERT str IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP 2", async () => {
    const code = `ASSERT |comment\\n| CP 'comment+'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("ASSERT strlen gt", async () => {
    const code = `
      DATA lv_len TYPE i.
      lv_len = 4.
      ASSERT strlen( '/dir/subdir' ) > lv_len.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NS comparator", async () => {
    const code = `
      ASSERT 'foo' NS 'bar'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("BETWEEN comparator", async () => {
    const code = `
      ASSERT 2 BETWEEN 1 AND 5.
      ASSERT 1 BETWEEN 1 AND 5.
      ASSERT 5 BETWEEN 1 AND 5.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NOT BETWEEN comparator", async () => {
    const code = `
      ASSERT 8 NOT BETWEEN 1 AND 5.
      ASSERT NOT 3 NOT BETWEEN 2 AND 5.
      ASSERT NOT 8 BETWEEN 1 AND 5.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("check structure is initial", async () => {
    const code = `
      TYPES: BEGIN OF bar,
              field TYPE i,
            END OF bar.
      DATA bar TYPE bar.
      ASSERT bar IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("check internal table is initial", async () => {
    const code = `
      DATA bar TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      ASSERT bar IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("EQ, initial string and integer", async () => {
    const code = `
      DATA low TYPE string.
      DATA int TYPE i.
      ASSERT int EQ low.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("LT, string and integer", async () => {
    const code = `
      DATA low TYPE string.
      DATA int TYPE i.
      low = |1|.
      ASSERT int LT low.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("BETWEEN 1", async () => {
    const code = `
      DATA low TYPE string.
      DATA high TYPE string.
      DATA int TYPE i.
      ASSERT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("BETWEEN 2", async () => {
    const code = `
      DATA low TYPE string.
      DATA high TYPE string.
      DATA int TYPE i.
      low = |1|.
      high = |1|.
      int = 1.
      ASSERT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("BETWEEN 3", async () => {
    const code = `
      DATA low TYPE string.
      DATA high TYPE string.
      DATA int TYPE i.
      low = |1|.
      high = |1|.
      int = 2.
      ASSERT NOT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("BETWEEN 4", async () => {
    const code = `
      DATA low TYPE string.
      DATA high TYPE string.
      DATA int TYPE i.
      low = |10|.
      high = |20|.
      int = 2.
      ASSERT NOT int BETWEEN low AND high.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("compare using CO operator", async () => {
    const code = `
      DATA lv_val TYPE abap_bool.
      lv_val = boolc( 'hello' CO 'ab' ).
      ASSERT lv_val = abap_false.
      lv_val = boolc( 'aabbaa' CO 'ab' ).
      ASSERT lv_val = abap_true.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("IS ASSIGNED", async () => {
    const code = `
      FIELD-SYMBOLS <bar> TYPE i.
      ASSERT <bar> IS NOT ASSIGNED.
      ASSERT NOT <bar> IS ASSIGNED.
      ASSIGN 2 TO <bar>.
      ASSERT <bar> IS ASSIGNED.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it.skip("internal table equals", async () => {
    const code = `
      DATA data1 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      DATA data2 TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
      ASSERT data1 = data2.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CA", async () => {
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
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CP, whitespace", async () => {
    const code = `ASSERT |hello\\nfoobar\\nmoo| CP '*oo*'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CA, empty left -> space", async () => {
    const code = `
      ASSERT NOT '' CA 'AB'.
      ASSERT '' CA 'A B'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("object reference IS INITIAL", async () => {
    const code = `
  CLASS lcl_bar DEFINITION.
  ENDCLASS.
  CLASS lcl_bar IMPLEMENTATION.
  ENDCLASS.
  FORM foo.
    DATA foo TYPE REF TO lcl_bar.
    ASSERT foo IS INITIAL.
  ENDFORM.
  START-OF-SELECTION.
    PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("IS SUPPLIED", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo
      IMPORTING opt TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD moo.
    IF opt IS SUPPLIED.
      WRITE / 'yes'.
    ELSE.
      WRITE / 'no'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM run.
  lcl_bar=>moo( ).
  lcl_bar=>moo( 1 ).
  lcl_bar=>moo( opt = 1 ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no\nyes\nyes");
  });

  it("two x strings", async () => {
    const code = `
      DATA lv_x_len_2  TYPE x LENGTH 2 value '0106'.
      DATA lv_x_len_1 TYPE x LENGTH 1 value '07'.
      IF lv_x_len_1 > lv_x_len_2.
        WRITE: '07 > 0106'.
      ELSE.
        WRITE: '0106 > 07'.
      ENDIF.
      `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`07 > 0106`);
  });

  it("xstring and int - part 1", async () => {
    const code = `
      DATA lv_int TYPE i VALUE 8.
      DATA lv_x_len_1 TYPE x LENGTH 1 VALUE '07'.

      IF lv_int > lv_x_len_1.
        WRITE: '8i > 07'.
      ELSE.
        WRITE: '07 > 8i '.
      ENDIF.
      `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`8i > 07`);
  });

  it("xstring and int - part 2", async () => {
    const code = `
      DATA lv_int TYPE i VALUE 8.
      DATA lv_x_len_2  TYPE x LENGTH 2 value '1D06'.

      IF lv_int > lv_x_len_2.
        WRITE: '8i > 1D06'.
      ELSE.
        WRITE: '1D06 > 8i'.
      ENDIF.
      `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`1D06 > 8i`);
  });

  it("xstring and int - part 3", async () => {
    const code = `
      DATA lv_hex TYPE x.
      lv_hex = '1B'.
      DATA lv_int TYPE i VALUE 4.

      IF lv_hex > lv_int.
        WRITE: '1B > 4i'.
      ELSE.
        WRITE: '4i > 1B'.
      ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`1B > 4i`);
  });

  it("IN empty", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  ASSERT 5 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NOT IN", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  FIELD-SYMBOLS <moo> LIKE LINE OF bar.
  APPEND INITIAL LINE TO bar ASSIGNING <moo>.
  <moo>-sign = 'I'.
  <moo>-option = 'EQ'.
  <moo>-low = 2.
  ASSERT 5 NOT IN bar.
  ASSERT NOT 5 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("2 IN 2", async () => {
    const code = `
  DATA bar TYPE RANGE OF i.
  FIELD-SYMBOLS <moo> LIKE LINE OF bar.
  APPEND INITIAL LINE TO bar ASSIGNING <moo>.
  <moo>-sign = 'I'.
  <moo>-option = 'EQ'.
  <moo>-low = 2.
  ASSERT 2 IN bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Compare string with integer, gt", async () => {
    const code = `
  DATA lv_data TYPE string.
  lv_data = 190.
  IF lv_data > 76.
    WRITE / 'expected1'.
  ENDIF.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`expected1`);
  });

  it("Compare string with integer, lt", async () => {
    const code = `
  DATA lv_data TYPE string.
  lv_data = 190.
  IF lv_data < 200.
    WRITE / 'expected2'.
  ENDIF.
  `;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`expected2`);
  });

  it("more CP", async () => {
    const code = `
    ASSERT '<?xml' CP '<?xml'.
    ASSERT '<?xml sdf' CP '<?xml *'.
    ASSERT '()' CP '()'.
    ASSERT '[]' CP '[]'.
    ASSERT '.' CP '.'.
    ASSERT '|' CP '|'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});