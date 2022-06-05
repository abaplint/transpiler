import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - ASSIGN", () => {

  beforeEach(async () => {
    abap = new ABAP();
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
    expect(abap.console.get()).to.equal("asd");
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
    expect(abap.console.get()).to.equal("1234");
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

});