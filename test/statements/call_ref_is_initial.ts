import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

// a method call through an initial object reference raises CX_SY_REF_IS_INITIAL,
// see https://github.com/open-abap/open-abap-core/issues/926
const exceptionClasses = `
CLASS cx_root DEFINITION ABSTRACT PUBLIC.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
ENDCLASS.

CLASS cx_dynamic_check DEFINITION ABSTRACT PUBLIC INHERITING FROM cx_root.
ENDCLASS.
CLASS cx_dynamic_check IMPLEMENTATION.
ENDCLASS.

CLASS cx_sy_ref_is_initial DEFINITION PUBLIC INHERITING FROM cx_dynamic_check.
ENDCLASS.
CLASS cx_sy_ref_is_initial IMPLEMENTATION.
ENDCLASS.`;

const lcl = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    DATA mo_next TYPE REF TO lcl.
    METHODS noop.
    METHODS value RETURNING VALUE(rv) TYPE i.
    METHODS next RETURNING VALUE(ro) TYPE REF TO lcl.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD noop.
    WRITE / 'noop'.
  ENDMETHOD.
  METHOD value.
    rv = 2.
  ENDMETHOD.
  METHOD next.
    ro = mo_next.
  ENDMETHOD.
ENDCLASS.`;

describe("Running statements - CALL, initial reference", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("method call, CATCH cx_sy_ref_is_initial", async () => {
    const code = exceptionClasses + lcl + `
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  TRY.
      lo->noop( ).
      WRITE / 'no exception'.
    CATCH cx_sy_ref_is_initial.
      WRITE / 'caught'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("caught");
  });

  it("method call, CATCH cx_root INTO", async () => {
    const code = exceptionClasses + lcl + `
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  DATA lx TYPE REF TO cx_root.
  TRY.
      lo->noop( ).
      WRITE / 'no exception'.
    CATCH cx_root INTO lx.
      IF lx IS INSTANCE OF cx_sy_ref_is_initial.
        WRITE / 'cx_sy_ref_is_initial'.
      ENDIF.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("cx_sy_ref_is_initial");
  });

  it("CALL METHOD, static and dynamic method name", async () => {
    const code = exceptionClasses + lcl + `
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  TRY.
      CALL METHOD lo->noop.
    CATCH cx_sy_ref_is_initial.
      WRITE / 'static'.
  ENDTRY.
  TRY.
      CALL METHOD lo->('NOOP').
    CATCH cx_sy_ref_is_initial.
      WRITE / 'dynamic'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("static\ndynamic");
  });

  it("functional call in an expression", async () => {
    const code = exceptionClasses + lcl + `
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  DATA lv TYPE i.
  TRY.
      lv = lo->value( ) + 1.
    CATCH cx_sy_ref_is_initial.
      WRITE / 'arithmetic'.
  ENDTRY.
  TRY.
      IF lo->value( ) = 2.
        WRITE / 'no exception'.
      ENDIF.
    CATCH cx_sy_ref_is_initial.
      WRITE / 'condition'.
  ENDTRY.
  TRY.
      WRITE / |{ lo->value( ) }|.
    CATCH cx_sy_ref_is_initial.
      WRITE / 'template'.
  ENDTRY.
  WRITE / lv.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("arithmetic\ncondition\ntemplate\n0");
  });

  it("chained call, the middle call returns an initial reference", async () => {
    const code = exceptionClasses + lcl + `
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  DATA lv TYPE i.
  lo = NEW #( ).
  TRY.
      lo->next( )->noop( ).
      WRITE / 'no exception'.
    CATCH cx_sy_ref_is_initial.
      WRITE / 'call'.
  ENDTRY.
  TRY.
      lv = lo->next( )->value( ).
    CATCH cx_sy_ref_is_initial.
      WRITE / 'functional'.
  ENDTRY.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("call\nfunctional");
  });

  it("bound references still work", async () => {
    const code = exceptionClasses + lcl + `
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  FIELD-SYMBOLS <lo> TYPE REF TO lcl.
  lo = NEW #( ).
  lo->mo_next = NEW #( ).
  lo->noop( ).
  CALL METHOD lo->noop.
  lo->next( )->noop( ).
  ASSIGN lo TO <lo>.
  <lo>->noop( ).
  NEW lcl( )->noop( ).
  WRITE / lo->next( )->value( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("noop\nnoop\nnoop\nnoop\nnoop\n2");
  });

  it("without the exception class the error names it", async () => {
    const code = lcl + `
START-OF-SELECTION.
  DATA lo TYPE REF TO lcl.
  lo->noop( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    let message = "";
    try {
      await f(abap);
    } catch (e) {
      message = e.message;
    }
    expect(message).to.contain("CX_SY_REF_IS_INITIAL");
  });

});
