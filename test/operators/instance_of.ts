// import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar.prog.abap", contents}]);
}

describe("Running operators - INSTANCE OF", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("test", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lcl1 TYPE REF TO foo.
  CREATE OBJECT lcl1.
  ASSERT lcl1 IS INSTANCE OF foo.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

  it("negative", async () => {
    const code = `
CLASS foo DEFINITION.
  PUBLIC SECTION.
    DATA foo TYPE i.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.

CLASS bar DEFINITION INHERITING FROM foo.
  PUBLIC SECTION.
    DATA bar TYPE i.
ENDCLASS.
CLASS bar IMPLEMENTATION.
ENDCLASS.

START-OF-SELECTION.
  DATA lcl1 TYPE REF TO foo.
  CREATE OBJECT lcl1.
  ASSERT NOT lcl1 IS INSTANCE OF bar.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });


  it("initial class reference uses its static type", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA ref TYPE REF TO foo.
  ASSERT ref IS INSTANCE OF foo.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  for (const [form, declaration] of [
    ["NEW", "DATA(ref) = NEW foo( )."],
    ["CAST", "DATA(obj) = NEW foo( ). DATA(ref) = CAST foo( obj )."],
    ["RETURNING", "DATA(ref) = foo=>get_ref( )."],
    ["FIELD-SYMBOL", "DATA(ref) = NEW foo( ). ASSIGN ref TO FIELD-SYMBOL(<fs>)."],
  ]) {
    it(`initial inline ${form} reference inside a method uses its static type`, async () => {
      const code = `
CLASS foo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_ref RETURNING VALUE(ref) TYPE REF TO foo.
    CLASS-METHODS test.
ENDCLASS.
CLASS foo IMPLEMENTATION.
  METHOD get_ref.
    ref = NEW foo( ).
  ENDMETHOD.
  METHOD test.
    ${declaration}
    CLEAR ref.
    ASSERT ref IS INSTANCE OF foo.
    ${form === "FIELD-SYMBOL" ? "ASSERT <fs> IS INSTANCE OF foo." : ""}
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  foo=>test( ).`;
      await new AsyncFunction("abap", await run(code))(abap);
    });
  }

  it("initial interface reference uses its static type", async () => {
    const code = `
INTERFACE lif.
ENDINTERFACE.
START-OF-SELECTION.
  DATA ref TYPE REF TO lif.
  ASSERT ref IS INSTANCE OF lif.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("initial base reference is not an instance of a subclass", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
CLASS bar DEFINITION INHERITING FROM foo.
ENDCLASS.
CLASS bar IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA base TYPE REF TO foo.
  DATA sub TYPE REF TO bar.
  ASSERT NOT base IS INSTANCE OF bar.
  ASSERT sub IS INSTANCE OF foo.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("initial object reference is not an instance of a concrete class", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA ref TYPE REF TO object.
  ASSERT NOT ref IS INSTANCE OF foo.
  CREATE OBJECT ref TYPE foo.
  ASSERT ref IS INSTANCE OF object.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("initial reference matches implemented and included interfaces only", async () => {
    const code = `
INTERFACE lif.
ENDINTERFACE.
INTERFACE included.
  INTERFACES lif.
ENDINTERFACE.
INTERFACE unrelated.
ENDINTERFACE.
CLASS foo DEFINITION.
  PUBLIC SECTION.
    INTERFACES included.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
CLASS bar DEFINITION INHERITING FROM foo.
ENDCLASS.
CLASS bar IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA ref TYPE REF TO bar.
  DATA intf TYPE REF TO included.
  ASSERT NOT ref IS INSTANCE OF unrelated.
  ASSERT ref IS INSTANCE OF included.
  ASSERT ref IS INSTANCE OF lif.
  ASSERT intf IS INSTANCE OF lif.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("bound object reference is an instance of object", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA ref TYPE REF TO foo.
  CREATE OBJECT ref.
  ASSERT ref IS INSTANCE OF object.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("NOT and IS NOT negate the initial static type result", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA ref TYPE REF TO foo.
  ASSERT NOT ( NOT ref IS INSTANCE OF foo ).
  ASSERT NOT ( ref IS NOT INSTANCE OF foo ).
  ASSERT NOT ref IS NOT INSTANCE OF foo.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  // Inferred from the static subtype rule; not measured on ABAP 7.58.
  it("initial class and interface references are instances of object", async () => {
    const code = `
INTERFACE lif.
ENDINTERFACE.
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA ref TYPE REF TO foo.
  DATA intf TYPE REF TO lif.
  ASSERT ref IS INSTANCE OF object.
  ASSERT intf IS INSTANCE OF object.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });


  it("initial attributes and method results use their static type", async () => {
    const code = `
CLASS foo DEFINITION.
  PUBLIC SECTION.
    DATA ref TYPE REF TO foo.
    CLASS-METHODS get_ref RETURNING VALUE(ref) TYPE REF TO foo.
ENDCLASS.
CLASS foo IMPLEMENTATION.
  METHOD get_ref.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  DATA obj TYPE REF TO foo.
  CREATE OBJECT obj.
  ASSERT obj->ref IS INSTANCE OF foo.
  ASSERT foo=>get_ref( ) IS INSTANCE OF foo.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("initial method call attributes use the attribute static type", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
CLASS holder DEFINITION.
  PUBLIC SECTION.
    DATA ref TYPE REF TO foo.
    CLASS-METHODS get_holder RETURNING VALUE(holder) TYPE REF TO holder.
ENDCLASS.
CLASS holder IMPLEMENTATION.
  METHOD get_holder.
    CREATE OBJECT holder.
  ENDMETHOD.
ENDCLASS.
START-OF-SELECTION.
  ASSERT holder=>get_holder( )->ref IS INSTANCE OF foo.
  ASSERT NOT holder=>get_holder( )->ref IS INSTANCE OF holder.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("initial structure and field symbol components use their static type", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  TYPES: BEGIN OF structure,
           ref TYPE REF TO foo,
         END OF structure.
  DATA str TYPE structure.
  FIELD-SYMBOLS <str> TYPE structure.
  ASSIGN str TO <str>.
  ASSERT str-ref IS INSTANCE OF foo.
  ASSERT <str>-ref IS INSTANCE OF foo.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("initial table rows and dereferences use their static type", async () => {
    const code = `
CLASS foo DEFINITION.
ENDCLASS.
CLASS foo IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  TYPES: foo_ref TYPE REF TO foo,
         refs TYPE STANDARD TABLE OF foo_ref WITH DEFAULT KEY.
  DATA refs TYPE refs.
  DATA ref TYPE REF TO foo.
  APPEND INITIAL LINE TO refs.
  DATA dref TYPE REF TO foo_ref.
  GET REFERENCE OF ref INTO dref.
  ASSERT refs[ 1 ] IS INSTANCE OF foo.
  ASSERT dref->* IS INSTANCE OF foo.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

  it("bound reference is an instance of an interface its class implements", async () => {
    const code = `
INTERFACE lif_base.
ENDINTERFACE.
INTERFACE lif_x.
  INTERFACES lif_base.
ENDINTERFACE.
INTERFACE lif_other.
ENDINTERFACE.
CLASS lcl_a DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_x.
ENDCLASS.
CLASS lcl_a IMPLEMENTATION.
ENDCLASS.
CLASS lcl_b DEFINITION INHERITING FROM lcl_a.
ENDCLASS.
CLASS lcl_b IMPLEMENTATION.
ENDCLASS.
START-OF-SELECTION.
  DATA o TYPE REF TO object.
  CREATE OBJECT o TYPE lcl_a.
  ASSERT o IS INSTANCE OF lif_x.
  ASSERT o IS INSTANCE OF lif_base.
  ASSERT o IS NOT INSTANCE OF lif_other.
  CREATE OBJECT o TYPE lcl_b.
  ASSERT o IS INSTANCE OF lif_x.
  ASSERT o IS INSTANCE OF lif_base.
  ASSERT o IS NOT INSTANCE OF lif_other.`;
    await new AsyncFunction("abap", await run(code))(abap);
  });

});
