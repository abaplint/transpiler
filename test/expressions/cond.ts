import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string, skipVersionCheck = false) {
  return runFiles(abap, [{filename: "zfoobar_cond.prog.abap", contents}], {skipVersionCheck});
}

describe("Running expressions - COND", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("basic", async () => {
    const code = `
FORM foo.
  DATA(url) = COND string( WHEN 1 = 2 THEN 'foo'
                           WHEN 1 = 1 THEN 'bar' ).
  WRITE / url.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("else", async () => {
    const code = `
FORM foo.
  DATA(url) = COND string( WHEN 1 = 2 THEN 'foo'
                           ELSE 'bar' ).
  WRITE / url.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("bar");
  });

  it("no true condition", async () => {
    const code = `
FORM foo.
  DATA(url) = COND string( WHEN 1 = 2 THEN 'foo'
                           WHEN 1 = 2 THEN 'bar' ).
  WRITE / url.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("");
  });

  it("lower case ELSE", async () => {
    const code = `
FORM foo.
  DATA include_tax TYPE abap_bool.
  DATA lv_net TYPE i.
  DATA lv_tax_rate TYPE i.
  DATA(lv_tax) = COND decfloat34(
    WHEN include_tax = abap_true
    THEN lv_net * lv_tax_rate
    else 0 ).
  WRITE / lv_tax.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("0");
  });

  it("LET in table comprehension", async () => {
    const code = `
FORM foo.
  TYPES string_table TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  DATA(input) = VALUE string_table( ( \`hello\` ) ).

  DATA(output) = VALUE string_table(
  FOR text IN input
  ( COND string(
      LET upper = to_upper( text )
      IN
      WHEN text IS NOT INITIAL THEN upper
      ELSE text ) ) ).

  ASSERT output = VALUE string_table( ( \`HELLO\` ) ).
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });


  it("THROW", async () => {
    const code = `
CLASS cx_root DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.
CLASS cx_root IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.
ENDCLASS.

CLASS lcx DEFINITION INHERITING FROM cx_root.
  PUBLIC SECTION.
    DATA value TYPE i.
    METHODS constructor IMPORTING value TYPE i OPTIONAL.
ENDCLASS.
CLASS lcx IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->value = value.
  ENDMETHOD.
ENDCLASS.

FORM run USING iv TYPE i.
  DATA lv TYPE i.
  DATA lx TYPE REF TO lcx.
  TRY.
      lv = COND #( WHEN iv = 1 THEN 10 WHEN iv = 2 THEN THROW lcx( value = 2 ) ELSE THROW lcx( iv ) ).
      WRITE / lv.
    CATCH lcx INTO lx.
      WRITE / lx->value.
  ENDTRY.
ENDFORM.

START-OF-SELECTION.
  PERFORM run USING 1.
  PERFORM run USING 2.
  PERFORM run USING 3.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("10\n2\n3");
  });

});
