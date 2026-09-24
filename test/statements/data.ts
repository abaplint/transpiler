import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_data.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - DATA", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("DATA, with BEGIN OF", async () => {
    const code = `
DATA: BEGIN OF ls_msg,
        a1 TYPE c LENGTH 1,
        a2 TYPE c LENGTH 1,
        a3 TYPE c LENGTH 1,
        a4 TYPE c LENGTH 1,
      END OF ls_msg.
ls_msg-a3 = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DATA, upper case component name", async () => {
    const code = `
DATA: BEGIN OF ls_msg,
        a3 TYPE c LENGTH 1,
      END OF ls_msg.
ls_msg-a3 = 'A'.
ls_msg-A3 = 'A'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DATA, component name starting with percent", async () => {
    const code = `
TYPES: BEGIN OF bar,
         %field TYPE i,
       END OF bar.
DATA moo TYPE bar.
moo-%field = 2.
WRITE moo-%field.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("2");
  });

  it("DATA, VALUE", async () => {
    const code = `
DATA foo TYPE i VALUE 10.
WRITE foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("DATA, VALUE, structured", async () => {
    const code = `
DATA: BEGIN OF ls_struc,
        c TYPE i VALUE 10,
      END OF ls_struc.
WRITE ls_struc-c.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10");
  });

  it("DATA, namespaced", async () => {
    const code = `
DATA /foo/bar TYPE i.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("DATA: GROUPNAME / INCLUDE TYPE AS", async () => {
    const code = `
TYPES: BEGIN OF ty,
         foo TYPE i,
       END OF ty.
TYPES: BEGIN OF groupname.
         INCLUDE TYPE ty AS gg.
       TYPES END OF groupname.
DATA data TYPE groupname.

data-foo = 1.
WRITE / data-foo.
WRITE / data-gg-foo.

data-gg-foo = 2.
WRITE / data-foo.
WRITE / data-gg-foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n1\n2\n2");
  });

  it("DATA: GROUPNAME / INCLUDE TYPE AS with suffix", async () => {
    const code = `
TYPES: BEGIN OF ty_struc,
         a TYPE c LENGTH 2,
       END OF ty_struc.

TYPES BEGIN OF ty_named_include.
INCLUDE TYPE ty_struc AS named_with_suffix RENAMING WITH SUFFIX _suf.
TYPES el TYPE string.
TYPES END OF ty_named_include.

DATA ls_data   TYPE ty_named_include.

ls_data-named_with_suffix-a = 'hi'.
WRITE / ls_data-named_with_suffix-a.
WRITE / ls_data-a_suf.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("hi\nhi");
  });

  // todo: does the runtime need to do something special for EMPTY KEY?
  it("EMPTY KEY", async () => {
    const code = `
DATA foo TYPE STANDARD TABLE OF i WITH EMPTY KEY.
WRITE / lines( foo ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("DATA declared inside IF is visible after ENDIF", async () => {
    const code = `
FORM run.
  DATA lv_flag TYPE abap_bool VALUE abap_true.
  IF lv_flag = abap_true.
    DATA lv_inner TYPE string.
    lv_inner = 'set inside IF'.
  ENDIF.
  WRITE lv_inner.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("set inside IF");
  });

  it("DATA declared inside an IF branch that does not run is still there", async () => {
    const code = `
FORM run.
  DATA lv_flag TYPE abap_bool.
  IF lv_flag = abap_true.
    DATA lv_inner TYPE i VALUE 5.
  ENDIF.
  WRITE lv_inner.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("5");
  });

  it("DATA declared inside TRY is visible in CATCH, in a method", async () => {
    const code = `
CLASS cx_root DEFINITION ABSTRACT PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.

CLASS lcx DEFINITION INHERITING FROM cx_root.
ENDCLASS.
CLASS lcx IMPLEMENTATION.
ENDCLASS.

CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD run.
    TRY.
        DATA lx_error TYPE REF TO lcx.
        RAISE EXCEPTION TYPE lcx.
      CATCH lcx INTO lx_error.
        IF lx_error IS BOUND.
          WRITE 'caught'.
        ENDIF.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  lcl=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("caught");
  });

  it("FIELD-SYMBOLS declared inside CASE are usable after ENDCASE", async () => {
    const code = `
FORM run.
  DATA lv_target TYPE i VALUE 42.
  DATA lv_kind TYPE c LENGTH 1 VALUE 'A'.
  CASE lv_kind.
    WHEN 'A'.
      FIELD-SYMBOLS <lv_any> TYPE any.
      ASSIGN lv_target TO <lv_any>.
  ENDCASE.
  WRITE <lv_any>.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("42");
  });

  it("DATA declared inside a loop with no iterations is still there", async () => {
    const code = `
FORM run.
  DATA lt_empty TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
  DATA lv_row TYPE i.
  LOOP AT lt_empty INTO lv_row.
    DATA lv_count TYPE i VALUE 7.
  ENDLOOP.
  WRITE lv_count.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("7");
  });

  it("DATA declared inside a loop keeps its value across iterations", async () => {
    const code = `
FORM run.
  DO 3 TIMES.
    IF sy-index > 0.
      DATA lv_sum TYPE i.
      lv_sum = lv_sum + sy-index.
    ENDIF.
  ENDDO.
  WRITE lv_sum.
ENDFORM.

START-OF-SELECTION.
  PERFORM run.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("6");
  });

  it("DATA inside IF on program level, value from a constant declared before", async () => {
    const code = `
CONSTANTS lc_start TYPE i VALUE 3.
DATA lv_flag TYPE abap_bool.
IF lv_flag = abap_true.
  DATA lv_value TYPE i VALUE lc_start.
ENDIF.
WRITE lv_value.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("3");
  });

});
