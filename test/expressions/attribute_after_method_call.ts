import {expect} from "chai";
import {ABAP, MemoryConsole} from "../../packages/runtime/src";
import {AsyncFunction, runFiles} from "../_utils";

let abap: ABAP;

async function run(contents: string) {
  return runFiles(abap, [{filename: "zfoobar_attr.prog.abap", contents}]);
}

const setup = `
INTERFACE lif_lang.
  DATA value TYPE c LENGTH 1 READ-ONLY.
ENDINTERFACE.

INTERFACE lif_sy.
  METHODS language RETURNING VALUE(ro_language) TYPE REF TO lif_lang.
ENDINTERFACE.

CLASS lcl_lang DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_lang.
    DATA plain TYPE c LENGTH 1.
    METHODS constructor IMPORTING iv_value TYPE c.
ENDCLASS.
CLASS lcl_lang IMPLEMENTATION.
  METHOD constructor.
    lif_lang~value = iv_value.
    plain = 'P'.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_sy DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_sy.
    METHODS concrete RETURNING VALUE(ro_lang) TYPE REF TO lcl_lang.
ENDCLASS.
CLASS lcl_sy IMPLEMENTATION.
  METHOD lif_sy~language.
    ro_language = NEW lcl_lang( 'E' ).
  ENDMETHOD.
  METHOD concrete.
    ro_lang = NEW lcl_lang( 'D' ).
  ENDMETHOD.
ENDCLASS.

CLASS lcl_cp DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA sy TYPE REF TO lif_sy.
    CLASS-METHODS class_constructor.
ENDCLASS.
CLASS lcl_cp IMPLEMENTATION.
  METHOD class_constructor.
    sy = NEW lcl_sy( ).
  ENDMETHOD.
ENDCLASS.
`;

describe("Running expressions - interface attribute after a method call", () => {

  beforeEach(async () => {
    abap = new ABAP({console: new MemoryConsole()});
  });

  it("via interface reference, without alias", async () => {
    const code = setup + `
START-OF-SELECTION.
  DATA sy_ref TYPE REF TO lif_sy.
  sy_ref = NEW lcl_sy( ).
  WRITE sy_ref->language( )->value.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("E");
  });

  it("via class reference, with intf~ alias", async () => {
    const code = setup + `
START-OF-SELECTION.
  DATA sy_ref TYPE REF TO lcl_sy.
  sy_ref = NEW lcl_sy( ).
  WRITE sy_ref->concrete( )->lif_lang~value.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("D");
  });

  it("plain class attribute after a method call is not prefixed", async () => {
    const code = setup + `
START-OF-SELECTION.
  DATA sy_ref TYPE REF TO lcl_sy.
  sy_ref = NEW lcl_sy( ).
  WRITE sy_ref->concrete( )->plain.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("P");
  });

  it("via static attribute of a class, xco_cp=>sy->language( )->value shape", async () => {
    const code = setup + `
START-OF-SELECTION.
  DATA lv_langu TYPE c LENGTH 1.
  lv_langu = lcl_cp=>sy->language( )->value.
  WRITE lv_langu.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("E");
  });

  it("via static attribute, inside a method", async () => {
    const code = setup + `
CLASS lcl_user DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS run RETURNING VALUE(rv_langu) TYPE string.
ENDCLASS.
CLASS lcl_user IMPLEMENTATION.
  METHOD run.
    rv_langu = lcl_cp=>sy->language( )->value.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  WRITE lcl_user=>run( ).`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
    expect(abap.console.get()).to.equal("E");
  });

  it("assigned to a variable", async () => {
    const code = setup + `
START-OF-SELECTION.
  DATA sy_ref TYPE REF TO lif_sy.
  DATA lv_langu TYPE c LENGTH 1.
  sy_ref = NEW lcl_sy( ).
  lv_langu = sy_ref->language( )->value.
  ASSERT lv_langu = 'E'.`;
    const js = await run(code);
    const f = new AsyncFunction("abap", js);
    await f(abap);
  });

});
