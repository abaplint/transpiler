import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_if.prog.abap", contents}], {skipVersionCheck});
}

describe("Running statements - IF", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("IF, lower case", async () => {
    const code = `
    DATA lv_text TYPE string.
    if lv_text is initial.
    endif.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("OR, lower case", async () => {
    const code = `
    data i_amount_format type c length 2.
    if not ( i_amount_format is initial or i_amount_format+1(1) is initial ).
    endif.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("lower case not", async () => {
    const code = `
DATA: lv_equal_offset     TYPE i,
      lv_semicolon_offset TYPE i.
lv_equal_offset = 2.
lv_semicolon_offset = 0.
IF lv_equal_offset NE 0 AND not ( lv_semicolon_offset NE 0 AND lv_semicolon_offset LT lv_equal_offset ).
  WRITE 'yup'.
ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yup");
  });

  it("method conditional", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS conditional RETURNING VALUE(val) TYPE abap_bool.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD conditional.
    val = abap_true.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  IF lcl=>conditional( ).
    WRITE 'yes'.
  ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("method conditional, integer", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS conditional RETURNING VALUE(val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD conditional.
    val = 1.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  IF lcl=>conditional( ).
    WRITE 'yes'.
  ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("method conditional, negated", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS conditional RETURNING VALUE(val) TYPE abap_bool.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD conditional.
    val = abap_false.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  IF NOT lcl=>conditional( ).
    WRITE 'yes'.
  ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("method conditional, negated integer", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS conditional RETURNING VALUE(val) TYPE i.
ENDCLASS.

CLASS lcl IMPLEMENTATION.
  METHOD conditional.
    val = 0.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  IF NOT lcl=>conditional( ).
    WRITE 'yes'.
  ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("builtin matches predicate", async () => {
    const code = `
IF matches( val = 'hello' regex = 'hello' ).
  WRITE 'yes'.
ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

  it("builtin contains predicate negative", async () => {
    const code = `
IF contains( val = 'ab' regex = 'aaaa' ).
ELSE.
  WRITE 'works'.
ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("works");
  });

  it("not custom predicate", async () => {
    const code = `
CLASS lcl DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS foo RETURNING VALUE(val) TYPE i.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
  METHOD foo.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  IF NOT lcl=>foo( ).
    WRITE / 'yes'.
  ENDIF.`;

    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("yes");
  });

});