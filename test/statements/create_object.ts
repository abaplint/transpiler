import {expect} from "chai";
import {ABAP} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CREATE OBJECT", () => {

  beforeEach(async () => {
    abap = new ABAP();
  });

  it("CREATE OBJECT, should create new references", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA value TYPE i.
    METHODS constructor IMPORTING val TYPE i.
ENDCLASS.
CLASS lcl_bar IMPLEMENTATION.
  METHOD constructor.
    value = val.
  ENDMETHOD.
ENDCLASS.

FORM foo.
  DATA ref TYPE REF TO lcl_bar.
  DATA tab TYPE STANDARD TABLE OF REF TO lcl_bar.
  CREATE OBJECT ref EXPORTING val = 1.
  APPEND ref TO tab.
  CREATE OBJECT ref EXPORTING val = 2.
  APPEND ref TO tab.
  LOOP AT tab INTO ref.
    WRITE / ref->value.
  ENDLOOP.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("1\n2");
  });

  it("CREATE OBJECT, exporting ref", async () => {
    const code = `
CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS instantiate EXPORTING ref TYPE REF TO lcl_bar.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD instantiate.
    CREATE OBJECT ref.
  ENDMETHOD.
ENDCLASS.

FORM foo.
  DATA ref TYPE REF TO lcl_bar.
  lcl_bar=>instantiate( IMPORTING ref = ref ).
  ASSERT NOT ref IS INITIAL.
ENDFORM.

START-OF-SELECTION.
  PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE OBJECT, equals", async () => {
    const code = `
  CLASS lcl_bar DEFINITION.
  ENDCLASS.

  CLASS lcl_bar IMPLEMENTATION.
  ENDCLASS.

  FORM foo.
    DATA ref1 TYPE REF TO lcl_bar.
    DATA ref2 TYPE REF TO lcl_bar.
    CREATE OBJECT ref1.
    ref2 = ref1.
    ASSERT ref1 = ref2.
  ENDFORM.

  START-OF-SELECTION.
    PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("CREATE OBJECT, equals after append in table", async () => {
    const code = `
    CLASS lcl_bar DEFINITION.
    ENDCLASS.

    CLASS lcl_bar IMPLEMENTATION.
    ENDCLASS.

    FORM foo.
      DATA tab TYPE STANDARD TABLE OF REF TO lcl_bar.
      DATA ref1 TYPE REF TO lcl_bar.
      DATA ref2 TYPE REF TO lcl_bar.
      CREATE OBJECT ref1.
      APPEND ref1 TO tab.
      APPEND ref1 TO tab.

      READ TABLE tab INDEX 1 INTO ref1.
      READ TABLE tab INDEX 2 INTO ref2.
      ASSERT ref1 = ref2.
    ENDFORM.

    START-OF-SELECTION.
      PERFORM foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});