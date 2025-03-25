import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - Comparison", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("internal table equals", async () => {
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

  it("IS REQUESTED", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo
      EXPORTING
        opt TYPE i
        sdfsdf TYPE i.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD moo.
    IF opt IS REQUESTED.
      WRITE / 'yes'.
    ELSE.
      WRITE / 'no'.
    ENDIF.
    IF sdfsdf IS REQUESTED.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM run.
  DATA foo TYPE i.
  lcl_bar=>moo( ).
  lcl_bar=>moo( IMPORTING opt = foo ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no\nyes");
  });

  it("IS REQUESTED", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo
      EXPORTING opt TYPE i.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD moo.
    IF opt IS REQUESTED.
      WRITE / 'yes'.
    ELSE.
      WRITE / 'no'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

FORM run.
  DATA lv_val TYPE i.
  lcl_bar=>moo( ).
  lcl_bar=>moo( IMPORTING opt = lv_val ).
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("no\nyes");
  });

  it("IS SUPPLIED with boolc()", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS moo
      IMPORTING opt TYPE i OPTIONAL.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD moo.
    IF boolc( opt IS SUPPLIED ) = abap_true.
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

  it("CN", async () => {
    const code = `
    ASSERT 'foboar' CN 'moo'.
    ASSERT 'foboar' CN 'a'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NA", async () => {
    const code = `
    ASSERT 'foboar' NA 'c'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NP", async () => {
    const code = `
    ASSERT 'sdf' NP '*a'.
    ASSERT 'sdf' NP '*b*'.
    ASSERT 'sdf' NP 'c*'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("Character compare and spaces", async () => {
    const code = `
    ASSERT '' = ' '.
    ASSERT ' ' = ''.
    ASSERT '  ' = ''.
    ASSERT 'A' = 'A '.
    ASSERT 'A ' = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("equals, floats", async () => {
    const code = `DATA radius TYPE f.
DATA y TYPE f.
radius = 10.
y = 10.
ASSERT y = radius.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("lt, floats", async () => {
    const code = `DATA radius TYPE f.
DATA y TYPE f.
radius = 10.
y = 9.
ASSERT y < radius.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CO, structure", async () => {
    const code = `
CONSTANTS:
  BEGIN OF texts,
    chara  TYPE c LENGTH 1 VALUE 'a',
    numc   TYPE c LENGTH 1 VALUE 'b',
    string TYPE c LENGTH 1 VALUE 'g',
  END OF texts.

IF 'g' CO texts.
  WRITE 'yes'.
ELSE.
  WRITE 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("IS NOT SUPPLIED", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo IMPORTING bar TYPE string OPTIONAL.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD foo.
    ASSERT bar IS NOT SUPPLIED.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>foo( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("char1 is initial", async () => {
    const code = `
DATA foo TYPE c LENGTH 1.
foo = boolc( 1 = 2 ).
ASSERT foo IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("char1 is initial, def", async () => {
    const code = `
DATA foo TYPE c LENGTH 1.
ASSERT foo IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NP example", async () => {
    const code = `
    ASSERT 'aa 01.01.0001 foo' NP |++.++.++++ *|.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("NE integer and numc", async () => {
    const code = `
DATA int TYPE i.
DATA num TYPE n LENGTH 6.
int = 3.
num = 3.
IF num <> int.
  WRITE 'ne'.
ELSE.
  WRITE 'eq'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`eq`);
  });

  it("Table initial via field symbol", async () => {
    const code = `
DATA tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
FIELD-SYMBOLS <fs> TYPE ANY TABLE.
ASSIGN tab TO <fs>.
ASSERT <fs> IS INITIAL.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("xstr longer", async () => {
    const code = `
DATA foo TYPE xstring.
DATA bar TYPE x LENGTH 1.
foo = '0101'.
bar = '01'.
IF foo > bar.
  WRITE 'yes'.
ELSE.
  WRITE 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("xstr eq", async () => {
    const code = `
DATA foo TYPE xstring.
DATA bar TYPE x LENGTH 1.
foo = '01'.
bar = '01'.
IF foo = bar.
  WRITE 'yes'.
ELSE.
  WRITE 'no'.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("xstr gt, yes", async () => {
    const code = `
    DATA foo TYPE xstring.
    DATA bar TYPE x LENGTH 1.
    foo = '02'.
    bar = '01'.
    IF foo > bar.
      WRITE 'yes'.
    ELSE.
      WRITE 'no'.
    ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`yes`);
  });

  it("set fdpos, CO", async () => {
    const code = `
IF 'AB' CO sy-abcde.
  WRITE / sy-fdpos.
ENDIF.
IF NOT 'ABC1' CO sy-abcde.
  WRITE / sy-fdpos.
ENDIF.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal(`2\n3`);
  });

});