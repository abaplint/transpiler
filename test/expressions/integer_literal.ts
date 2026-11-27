import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

// the transpiled output is loaded as ES modules, which are always strict mode code,
// where octal literals like 007 are a SyntaxError
async function runStrict(contents: string) {
  const js = await runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
  const f = new AsyncFunction("abap", "\"use strict\";\n" + js);
  await f(abap);
}

describe("Running expressions - Integer literals with leading zeros", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("assignment and arithmetic", async () => {
    const code = `
    DATA lv_int TYPE i.
    lv_int = 007.
    WRITE / lv_int.
    lv_int = 010 + 0.
    WRITE / lv_int.
    lv_int = 000.
    WRITE / lv_int.`;
    await runStrict(code);
    expect(abap.console.get()).to.equal("7\n10\n0");
  });

  it("signed", async () => {
    const code = `
    DATA lv_int TYPE i.
    lv_int = -007.
    WRITE / lv_int.
    lv_int = - 010.
    WRITE / lv_int.
    lv_int = 5 - 010.
    WRITE / lv_int.
    lv_int = -000.
    WRITE / lv_int.`;
    await runStrict(code);
    expect(abap.console.get()).to.equal("-7\n-10\n-5\n0");
  });

  it("DATA VALUE and CONSTANTS", async () => {
    const code = `
    DATA lv_int TYPE i VALUE 007.
    DATA lv_neg TYPE i VALUE -010.
    DATA lv_numc TYPE n LENGTH 3 VALUE 000.
    CONSTANTS lc_int TYPE i VALUE 010.
    CONSTANTS: BEGIN OF lc_struc,
                 field TYPE i VALUE 009,
               END OF lc_struc.
    WRITE / lv_int.
    WRITE / lv_neg.
    WRITE / lv_numc.
    WRITE / lc_int.
    WRITE / lc_struc-field.`;
    await runStrict(code);
    expect(abap.console.get()).to.equal("7\n-10\n000\n10\n9");
  });

  it("NUMC target", async () => {
    const code = `
    DATA lv_numc TYPE n LENGTH 3.
    lv_numc = 007.
    WRITE / lv_numc.
    lv_numc = 000.
    WRITE / lv_numc.`;
    await runStrict(code);
    expect(abap.console.get()).to.equal("007\n000");
  });

  it("method call arguments, parameter defaults, VALUE #( ) and comparisons", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    TYPES ty_tab TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    CLASS-METHODS double
      IMPORTING iv_val TYPE i DEFAULT 008
      RETURNING VALUE(rv_val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD double.
    rv_val = iv_val * 2.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA lt_tab TYPE lcl=>ty_tab.
  DATA lv_int TYPE i.
  WRITE / lcl=>double( 007 ).
  WRITE / lcl=>double( ).
  lt_tab = VALUE #( ( 010 ) ( -007 ) ).
  LOOP AT lt_tab INTO lv_int.
    WRITE / lv_int.
  ENDLOOP.
  lv_int = 10.
  IF lv_int = 010.
    WRITE / 'eq'.
  ENDIF.
  IF lv_int > 009 AND lv_int BETWEEN 008 AND 011.
    WRITE / 'gt'.
  ENDIF.
  CASE lv_int.
    WHEN 010.
      WRITE / 'case'.
  ENDCASE.`;
    await runStrict(code);
    expect(abap.console.get()).to.equal("14\n16\n10\n-7\neq\ngt\ncase");
  });

});
