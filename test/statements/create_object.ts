import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running statements - CREATE OBJECT", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
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

  it("CREATE OBJECT, determine correct class", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    WRITE 'world'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE REF TO lcl_foo.
    METHODS constructor.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD constructor.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  CREATE OBJECT bar->foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("helloworld");
  });

  it("CREATE OBJECT, determine correct class, interfaced", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    WRITE 'world'.
  ENDMETHOD.
ENDCLASS.

INTERFACE lif_bar.
  DATA foo TYPE REF TO lcl_foo.
ENDINTERFACE.

CLASS lcl_bar DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_bar.
    METHODS constructor.
ENDCLASS.

CLASS lcl_bar IMPLEMENTATION.
  METHOD constructor.
    WRITE 'hello'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA bar TYPE REF TO lcl_bar.
  CREATE OBJECT bar.
  CREATE OBJECT bar->lif_bar~foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("helloworld");
  });

  it("CREATE OBJECT, dynamic local scoped class name, variable", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    WRITE 'world1'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO object.
  DATA lv_name TYPE string.
  lv_name = 'LCL_FOO'.
  CREATE OBJECT foo TYPE (lv_name).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("world1");
  });

  it("CREATE OBJECT, dynamic local scoped class name, fixed", async () => {
    const code = `
CLASS lcl_foo DEFINITION.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_foo IMPLEMENTATION.
  METHOD constructor.
    WRITE 'world2'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA foo TYPE REF TO object.
  CREATE OBJECT foo TYPE ('LCL_FOO').`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("world2");
  });

  it("CREATE OBJECT, in data object", async () => {
    const code = `
CLASS lcl DEFINITION.
ENDCLASS.
CLASS lcl IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  TYPES: BEGIN OF ty,
           instance TYPE REF TO lcl,
         END OF ty.
  DATA ld_instance TYPE REF TO ty.
  CREATE DATA ld_instance.
  CREATE OBJECT ld_instance->instance.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});